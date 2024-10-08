lazy val rdtapp = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := "3.5.0",
    libraryDependencies ++= Seq(
      // the next three can be published locally, but to save you from that, the jars are included in the repo in ./lib
//      "de.tu-darmstadt.stg"                   %%% "reactives"             % "0.36.0+32-60fca31b",
//      "de.tu-darmstadt.stg"                   %%% "rdts"                  % "0.36.0+32-60fca31b",
//      "replication"                           %%% "replication"           % "0.36.0+32-60fca31b",
      "de.rmgk.slips"                         %%% "delay"                 % "0.9.0",
      "org.scala-js"                          %%% "scalajs-dom"           % "2.8.0",
      "com.lihaoyi"                           %%% "scalatags"             % "0.13.1",
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"   % "2.30.9",
      "com.github.plokhotnyuk.jsoniter-scala"  %% "jsoniter-scala-macros" % "2.30.9",
      "com.lihaoyi"                           %%% "pprint"                % "0.9.0",
      "org.scalameta"                         %%% "munit"                 % "1.0.1" % Test,
    ),
  )
