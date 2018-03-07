addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
scalacOptions += "-feature"
libraryDependencies ++= Seq(
  "com.sksamuel.avro4s" %% "avro4s-core"  % "1.8.0",
  "io.circe"            %% "circe-core"   % "0.9.1",
  "io.circe"            %% "circe-parser" % "0.9.1"
)
