name := "adsb using scodec"

version := "0.1"


crossScalaVersions := Seq("2.11.4", "2.10.4")

//scalaVersion := "2.10.5"

//resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"
resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq("org.scodec" %% "scodec-core" % "1.7.0",
   //"org.scodec" %% "scodec-bits" % "1.0.5",
  // "org.scodec" %% "scodec-stream" % "0.7.0",
   "com.novocode" % "junit-interface" % "0.11" % "test")

EclipseKeys.eclipseOutput := Some("classes")

assemblyJarName in assembly := "adsb.jar"


