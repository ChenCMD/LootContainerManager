name := "LootContainerUtil"

version := "0.1"

scalaVersion := "3.4.2"

resolvers ++= Seq(
  "spigot-repo" at "https://hub.spigotmc.org/nexus/content/repositories/snapshots",
  "maven.elmakers.com" at "https://maven.elmakers.com/repository/",
  Resolver.jcenterRepo,
  Resolver.bintrayIvyRepo("com.eed3si9n", "sbt-plugins")
)

lazy val doobieVersion = "1.0.0-RC5"
libraryDependencies ++= Seq(
  "org.spigotmc"            % "spigot-api"               % "1.20.4-R0.1-SNAPSHOT",
  "dev.array21"             % "bukkit-reflection-util"   % "1.3.1",
  "org.typelevel"          %% "cats-effect"              % "3.4.8",
  "org.tpolecat"           %% "doobie-core"              % doobieVersion,
  "org.tpolecat"           %% "doobie-specs2"            % doobieVersion,
  "org.xerial"              % "sqlite-jdbc"              % "3.45.1.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
  "com.github.tarao"       %% "record4s"                 % "0.13.0",
  "org.scalatest"          %% "scalatest"                % "3.2.12" % Test
)

unmanagedBase := baseDirectory.value / "localDependencies"

excludeDependencies := Seq(
  ExclusionRule(organization = "org.bukkit", name = "bukkit")
)

semanticdbEnabled := true
semanticdbVersion := scalafixSemanticdb.revision

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ykind-projector:underscores",
  "-no-indent",
  "-Wunused:all",
  "-source:future"
)

assembly / assemblyExcludedJars  := {
  (assembly / fullClasspath).value.filter(
    _.data.absolutePath.startsWith((baseDirectory.value / "localDependencies").absolutePath)
  )
}

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", "services", "java.sql.Driver") => MergeStrategy.concat
  case PathList("META-INF", xs @ _*)                       => MergeStrategy.discard
  case x                                                   => (ThisBuild / assemblyMergeStrategy).value(x)
}

assembly / assemblyOutputPath    := new File(baseDirectory.value, "dist/LootContainerUtil.jar")
