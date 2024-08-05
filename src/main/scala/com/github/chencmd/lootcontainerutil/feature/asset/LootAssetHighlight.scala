package com.github.chencmd.lootcontainerutil.feature.asset

import com.github.chencmd.lootcontainerutil.Prefix
import com.github.chencmd.lootcontainerutil.exceptions.SystemException
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.CraftBlock
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.NMSIBlockData
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.Position

import cats.Align
import cats.data.Ior
import cats.effect.SyncIO
import cats.effect.kernel.Async
import cats.effect.kernel.Ref
import cats.implicits.*

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Random
import scala.util.chaining.*

import com.comphenix.protocol.PacketType
import com.comphenix.protocol.ProtocolLibrary
import com.comphenix.protocol.events.PacketContainer
import com.comphenix.protocol.wrappers.WrappedDataValue
import com.comphenix.protocol.wrappers.WrappedDataWatcher.Registry
import java.lang as jl
import java.util as ju
import java.util.UUID
import org.bukkit.Bukkit
import org.bukkit.World
import org.bukkit.block.Block
import org.bukkit.block.BlockFace
import org.bukkit.entity.EntityType
import org.bukkit.entity.Player
import org.joml.Vector3f

object LootAssetHighlight {
  def task[F[_]: Async](using
    lootAssetCache: LootAssetPersistenceCacheInstr[F],
    mcThread: OnMinecraftThread[F]
  ): F[Unit] = for {
    entityIds <- Ref.of[F, Map[UUID, Map[BlockLocation, Int]]](Map.empty)
    _         <- Async[F].delay(Bukkit.getConsoleSender.sendMessage("Starting highlight task"))
    _         <- (highlight(entityIds) >> Async[F].sleep(3.seconds)).foreverM
  } yield ()

  def highlight[F[_]: Async](entityIds: Ref[F, Map[UUID, Map[BlockLocation, Int]]])(using
    lootAssetCache: LootAssetPersistenceCacheInstr[F],
    mcThread: OnMinecraftThread[F]
  ): F[Unit] = for {
    players <- Async[F].delay(Bukkit.getOnlinePlayers.asScala.toList)
    players <- Either
      .catchNonFatal(players.asInstanceOf[List[Player]])
      .fold(_ => SystemException.raise("Failed to get online players"), _.pure[F])

    assets          <- players.traverse { player =>
      for {
        loc <- Async[F].delay(BlockLocation.of(player.getLocation))
        world = player.getWorld
        assets <- lootAssetCache.askLootAssetLocationsNear(loc)
      } yield for {
        assets <- assets.traverse { asset =>
          val assetLocation = asset.location.toBukkit(world)
          SyncIO(world.getBlockAt(assetLocation)).map { block =>
            BlockLocation.of(assetLocation) -> (asset -> block)
          }
        }
      } yield player -> Map.from(assets)
    }
    assetsForPlayer <- mcThread.run(assets.sequence)

    entityIdsForAllPlayers <- entityIds.get
    newEntityIds           <- assetsForPlayer.traverse { (p, assets) =>
      val pUuid              = p.getUniqueId
      val entityIdsForPlayer = entityIdsForAllPlayers.get(pUuid).orEmpty
      val program            = Align[Map[BlockLocation, _]].align(entityIdsForPlayer, assets).toList.traverse {
        case (loc, Ior.Left((id)))       => sendDeleteImaginaryHighlightPacket(p)(id).as(None)
        case (loc, Ior.Right((a, b)))    => sendAddImaginaryHighlightPacket(p)(a, b).map(id => Some(loc -> id))
        case (loc, Ior.Both(id, (_, b))) => checkBlock(p)(b, loc).map(_ => Some(loc -> id))
      }
      program.map(pUuid -> _.flatten.toMap)
    }
    _                      <- entityIds.set(newEntityIds.toMap)
  } yield ()

  val NON_FULL_BLOCK_CONTAINER = Set(
    "minecraft:brewing_stand",
    "minecraft:chest",
    "minecraft:trapped_chest",
    "minecraft:hopper"
  )

  def checkBlock[F[_]: Async](player: Player)(block: Block, location: BlockLocation): F[Unit] = {
    Async[F].whenA(block.isEmpty())(Async[F].delay {
      player.sendMessage(s"${Prefix.ERROR}Asset として登録されている座標にコンテナ系ブロックが存在しません。")
      player.sendMessage(s"${Prefix.ERROR}座標: ${location.toXYZString}")
    })
  }

  def sendAddImaginaryHighlightPacket[F[_]: Async](player: Player)(asset: LootAsset, block: Block)(using
    mcThread: OnMinecraftThread[F]
  ): F[Int] = for {
    entityID <- Async[F].delay(Random.nextInt(Int.MaxValue))

    craftBlock   <- CraftBlock.cast(block)
    nmsBlockData <- craftBlock.getNMS

    _ <- Async[F].whenA(block.isEmpty())(Async[F].delay {
      player.sendMessage(s"${Prefix.ERROR}Asset として登録されている座標にブロックが存在しません。")
      player.sendMessage(s"${Prefix.ERROR}座標: ${asset.location.toXYZString}")
    })

    facing            = asset.facing.filter(_ => NON_FULL_BLOCK_CONTAINER.contains(asset.blockId))
    spawnEntityPacket = createSpawnEntityPacket(entityID, asset.location.toPosition, facing)
    setMetadataPacket = createMetadataPacket(entityID, nmsBlockData)

    pManager = ProtocolLibrary.getProtocolManager
    _ <- Async[F].delay(pManager.sendServerPacket(player, spawnEntityPacket))
    _ <- Async[F].delay(pManager.sendServerPacket(player, setMetadataPacket))
  } yield entityID

  def sendDeleteImaginaryHighlightPacket[F[_]: Async](player: Player)(entityID: Int): F[Unit] = {
    val pManager = ProtocolLibrary.getProtocolManager
    Async[F].delay(pManager.sendServerPacket(player, createDestroyEntityPacket(entityID)))
  }

  def createSpawnEntityPacket(entityID: Int, position: Position, facing: Option[BlockFace]): PacketContainer = {
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
      facing match {
        case Some(BlockFace.SOUTH) => p.getBytes().write(1, (0 * (256d / 360d)).toByte)
        case Some(BlockFace.WEST)  => p.getBytes().write(1, (90 * (256d / 360d)).toByte)
        case Some(BlockFace.NORTH) => p.getBytes().write(1, (180 * (256d / 360d)).toByte)
        case Some(BlockFace.EAST)  => p.getBytes().write(1, (270 * (256d / 360d)).toByte)
        case _                     => p.getBytes().write(1, 0.toByte)
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
