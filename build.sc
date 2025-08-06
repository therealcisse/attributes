import mill._, scalalib._, publish._

object attributes extends ScalaModule with PublishModule {
  def scalaVersion = "3.3.1"

  def ivyDeps = Agg(
    ivy"dev.zio::zio-schema:1.7.4",
    ivy"dev.zio::zio-schema-derivation:1.7.4",
    ivy"dev.zio::zio-schema-json:1.7.4",
    ivy"dev.zio::zio-schema-protobuf:1.7.4"
  )

  def publishVersion = "0.1.0"

  def pomSettings = PomSettings(
    description = "Attribute system with ZIO Schema serialization",
    organization = "com.github.therealcisse",
    url = "https://github.com/therealcisse/attributes",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("therealcisse", "attributes"),
    developers = Seq.empty
  )
}
