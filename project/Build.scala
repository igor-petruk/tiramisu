import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "com.github.igor-petruk.tiramisu"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization
  )
}

object TiramisuBuild extends Build {
    lazy val root = Project(id = "tiramisu-parent",
							base = file("."),
							settings = BuildSettings.buildSettings) aggregate(tiramisuCore)

    lazy val tiramisuCore = Project(id = "tiramisu-core",
                           base = file("core"))
}
