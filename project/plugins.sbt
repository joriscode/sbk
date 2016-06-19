// repository for Typesafe plugins
resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.0.0") // try new version 1.1.1

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")
