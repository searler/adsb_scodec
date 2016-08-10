name := "adsb using scodec"

organization := "org.rsearle"

version := "0.3-SNAPSHOT"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq("org.scodec" %% "scodec-core" % "1.10.2",
   "com.novocode" % "junit-interface" % "0.11" % "test")

assemblyJarName in assembly := "adsb.jar"


