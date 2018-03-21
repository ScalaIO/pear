lazy val monocleVersion = "1.4.0"

libraryDependencies ++= Seq(
  "com.github.julien-truffaut" %% "monocle-core"    % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-macro"   % monocleVersion
)
