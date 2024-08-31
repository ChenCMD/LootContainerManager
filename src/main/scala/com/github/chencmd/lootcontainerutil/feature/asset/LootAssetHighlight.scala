package com.github.chencmd.lootcontainermanager.feature.asset

import com.github.chencmd.lootcontainermanager.Prefix
import com.github.chencmd.lootcontainermanager.exceptions.SystemException
import com.github.chencmd.lootcontainermanager.feature.asset.persistence.LootAssetContainer
import com.github.chencmd.lootcontainermanager.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainermanager.generic.TupleExtra.*
import com.github.chencmd.lootcontainermanager.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainermanager.minecraft.bukkit.BlockLocation
import com.github.chencmd.lootcontainermanager.minecraft.bukkit.CraftBlock
import com.github.chencmd.lootcontainermanager.minecraft.bukkit.Position
import com.github.chencmd.lootcontainermanager.minecraft.nms.NMSIBlockData

import cats.Align
import cats.data.Ior
import cats.effect.kernel.Async
import cats.effect.kernel.Ref
import cats.effect.kernel.Sync
import cats.implicits.*
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Random
import scala.util.chaining.*

import java.lang as jl
import java.util as ju
import java.util.UUID

import org.bukkit.Bukkit
import org.bukkit.block.Block
import org.bukkit.block.BlockFace
import org.bukkit.block.data.`type`.Chest as ChestData
import org.bukkit.entity.EntityType
import org.bukkit.entity.Player

import com.comphenix.protocol.PacketType
import com.comphenix.protocol.ProtocolLibrary
import com.comphenix.protocol.events.PacketContainer
import com.comphenix.protocol.wrappers.WrappedDataValue
import com.comphenix.protocol.wrappers.WrappedDataWatcher.Registry
import org.joml.Vector3f

object LootAssetHighlight {
  def task[F[_]: Async, G[_]: Sync](using
    logger: Logger[F],
    lootAssetCache: LootAssetPersistenceCacheInstr[F],
    mcThread: OnMinecraftThread[F, G]
  ): F[Unit] = for {
    entityIds <- Ref.of[F, Map[UUID, Map[Position, Int]]](Map.empty)
    _         <- Async[F].delay(logger.info("Starting highlight task"))
    _         <- (highlight[F, G](entityIds) >> Async[F].sleep(3.seconds)).foreverM
  } yield ()

  val CHESTS = Set(
    "minecraft:chest",
    "minecraft:trapped_chest"
  )

  def highlight[F[_]: Async, G[_]: Sync](entityIds: Ref[F, Map[UUID, Map[Position, Int]]])(using
    lootAssetCache: LootAssetPersistenceCacheInstr[F],
    mcThread: OnMinecraftThread[F, G]
  ): F[Unit] = for {
    players <- Async[F].delay(Bukkit.getOnlinePlayers.asScala.toList)
    players <- Either
      .catchNonFatal(players.asInstanceOf[List[Player]])
      .fold(_ => SystemException.raise[F]("Failed to get online players"), _.pure[F])

    assets          <- players.traverse { player =>
      for {
        loc <- Async[F].delay(BlockLocation.of(player.getLocation))
        world = player.getWorld
        assets <- lootAssetCache.askLootAssetLocationsNear(loc)
      } yield for {
        assets <- assets.flatTraverse { asset =>
          def toData(container: LootAssetContainer) = {
            val location = container.location
            val block    = world.getBlockAt(location.toBukkit(world))
            location.toPosition -> (container, block)
          }
          Sync[G].delay {
            if (asset.containers.size == 1) {
              asset.containers.map(toData)
            } else {
              val containers        = asset.containers.map(toData)
              val midDummyContainer = {
                val mid  = asset.containers.head.location.midPointAt(asset.containers.last.location)
                val head = containers.head
                mid -> head._2.copy(_1 = head._2._1.copy(chestType = Some(ChestData.Type.SINGLE)))
                mid -> head._2.modify(0)(_.copy(chestType = Some(ChestData.Type.LEFT)))
              }
              containers :+ midDummyContainer
            }
          }
        }
      } yield player -> Map.from(assets)
    }
    assetsForPlayer <- mcThread.run(assets.sequence)

    entityIdsForAllPlayers <- entityIds.get
    newEntityIds           <- assetsForPlayer.traverse { (p, containers) =>
      val pUuid              = p.getUniqueId
      val entityIdsForPlayer = entityIdsForAllPlayers.get(pUuid).orEmpty
      val program            = Align[Map[Position, _]].align(entityIdsForPlayer, containers).toList.flatTraverse {
        case (pos, Ior.Left((id)))                => sendDeleteImaginaryHighlightPacket(p)(id).as(List.empty)
        case (pos, Ior.Right((container, block))) => for {
            _  <- checkBlock(p)(block, container.location)
            id <- sendAddImaginaryHighlightPacket[F, G](p)(pos, container, block)
          } yield List(pos -> id)
        case (pos, Ior.Both(id, (_, b)))          => checkBlock(p)(b, pos.toBlockLocation).as(List(pos -> id))
      }
      program.map(pUuid -> _.toMap)
    }
    _                      <- entityIds.set(newEntityIds.toMap)
  } yield ()

  def checkBlock[F[_]: Async](player: Player)(block: Block, location: BlockLocation): F[Unit] = {
    Async[F].whenA(block.isEmpty())(Async[F].delay {
      player.sendMessage(s"${Prefix.ERROR}Asset として登録されている座標にコンテナ系ブロックが存在しません。")
      player.sendMessage(s"${Prefix.ERROR}座標: ${location.toXYZString}")
    })
  }

  def sendAddImaginaryHighlightPacket[F[_]: Async, G[_]: Sync](player: Player)(
    position: Position,
    container: LootAssetContainer,
    block: Block
  )(using
    mcThread: OnMinecraftThread[F, G]
  ): F[Int] = for {
    entityID <- Async[F].delay(Random.nextInt(Int.MaxValue))

    craftBlock   <- CraftBlock.cast[F](block)
    nmsBlockData <- craftBlock.getNMS[F]

    facing            = container.facing.filter(_ => CHESTS.contains(container.blockId))
    spawnEntityPacket = createSpawnEntityPacket(entityID, position, facing, container.chestType)
    setMetadataPacket = createMetadataPacket(entityID, nmsBlockData)

    pManager = ProtocolLibrary.getProtocolManager
    _ <- Async[F].delay(pManager.sendServerPacket(player, spawnEntityPacket))
    _ <- Async[F].delay(pManager.sendServerPacket(player, setMetadataPacket))
  } yield entityID

  def sendDeleteImaginaryHighlightPacket[F[_]: Async](player: Player)(entityID: Int): F[Unit] = {
    val pManager = ProtocolLibrary.getProtocolManager
    Async[F].delay(pManager.sendServerPacket(player, createDestroyEntityPacket(entityID)))
  }

  def createSpawnEntityPacket(
    entityID: Int,
    position: Position,
    facing: Option[BlockFace],
    chestType: Option[ChestData.Type] = None
  ): PacketContainer = {
    PacketContainer(PacketType.Play.Server.SPAWN_ENTITY).tap { p =>
      // Entity ID   | VarInt
      p.getIntegers().write(0, entityID)
      // Entity UUID | UUID
      p.getUUIDs().write(0, UUID.randomUUID())
      // Type        | VarInt
      p.getEntityTypeModifier().write(0, EntityType.BLOCK_DISPLAY)
      // X           | Double
      p.getDoubles().write(0, position.x + 0.5)
      // Y           | Double
      p.getDoubles().write(1, position.y)
      // Z           | Double
      p.getDoubles().write(2, position.z + 0.5)
      // Yaw
      // チェストは block_display のバグで blockState から正常に反映されないので、ここで設定する
      if (facing.isDefined) {
        val yaw    = facing match {
          case Some(BlockFace.SOUTH) => 0f
          case Some(BlockFace.WEST)  => 90f
          case Some(BlockFace.NORTH) => 180f
          case Some(BlockFace.EAST)  => 270f
          case _                     => 0f
        }
        val offset = chestType match {
          case Some(ChestData.Type.LEFT)  => 90
          case Some(ChestData.Type.RIGHT) => -90
          case _                          => 0
        }
        p.getBytes().write(1, ((yaw + offset) * (256d / 360d)).toByte)
      }
    }
  }

  def createMetadataPacket(entityID: Int, blockData: NMSIBlockData): PacketContainer = {
    PacketContainer(PacketType.Play.Server.ENTITY_METADATA).tap { p =>
      // Entity ID   | VarInt
      p.getIntegers().write(0, entityID)
      // Metadata    | WrappedDataValue
      val data = List(
        // 0x40 = has glowing effect
        WrappedDataValue(0, Registry.get(classOf[jl.Byte]), byte2Byte(0x40)),
        // Translation
        WrappedDataValue(11, Registry.get(classOf[Vector3f]), Vector3f(-0.499, 0.001, -0.499)),
        // Scale
        WrappedDataValue(12, Registry.get(classOf[Vector3f]), Vector3f(0.998f)),
        // Brightness override
        WrappedDataValue(16, Registry.get(classOf[jl.Integer]), int2Integer((15 << 4) | (15 << 20))),
        // View range
        WrappedDataValue(17, Registry.get(classOf[jl.Float]), float2Float(5.0f)),
        // Shadow strength
        WrappedDataValue(19, Registry.get(classOf[jl.Float]), float2Float(0f)),
        // Grow color override
        WrappedDataValue(22, Registry.get(classOf[jl.Integer]), int2Integer(255 << 16 | (128 + 32) << 8 | 32)),
        // Displayed block state
        WrappedDataValue(23, Registry.getBlockDataSerializer(false), blockData)
      )
      p.getDataValueCollectionModifier().write(0, data.asJava)
    }
  }

  def createDestroyEntityPacket(entityID: Int): PacketContainer = {
    PacketContainer(PacketType.Play.Server.ENTITY_DESTROY).tap { p =>
      val destroyEntities = List(entityID)
      // Entity IDs | VarInt[]
      p.getIntLists().write(0, destroyEntities.map(int2Integer).asJava)
    }
  }
}
