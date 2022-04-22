name := "LootContainerUtil"

version := "0.1"

scalaVersion := "3.1.2"

resolvers ++= Seq(
  "spigot-repo" at "https://hub.spigotmc.org/nexus/content/repositories/snapshots",
  "maven.elmakers.com" at "https://maven.elmakers.com/repository/",
  Resolver.jcenterRepo,
  Resolver.bintrayIvyRepo("com.eed3si9n", "sbt-plugins")
)

libraryDependencies ++= Seq(
  "org.spigotmc" % "spigot-api" % "1.18.2-R0.1-SNAPSHOT",
  "org.typelevel" %% "cats-effect" % "3.3.11"
)

unmanagedBase := baseDirectory.value / "localDependencies"

excludeDependencies := Seq(
  ExclusionRule(organization = "org.bukkit", name = "bukkit")
)

scalacOptions ++= Seq(
  "-language:strictEquality"
)

assembly / assemblyExcludedJars := {
  (assembly / fullClasspath).value.filter(_.data.absolutePath.startsWith((baseDirectory.value / "localDependencies").absolutePath))
}