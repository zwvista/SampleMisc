name := """play2bars-java-spring"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
	  javaJdbc,
	  cache,
	  javaWs,
      "org.easytesting" % "fest-assert-core" % "2.0M10",
      "com.h2database" % "h2" % "1.3.168",
      "org.springframework" % "spring-context" % "4.1.6.RELEASE",
      "org.springframework" % "spring-orm" % "4.1.6.RELEASE",
      "org.springframework" % "spring-jdbc" % "4.1.6.RELEASE",
      "org.springframework" % "spring-tx" % "4.1.6.RELEASE",
      "org.springframework" % "spring-aop" % "4.1.6.RELEASE",
      "org.springframework" % "spring-expression" % "4.1.6.RELEASE",
      "org.springframework" % "spring-test" % "4.1.6.RELEASE" % "test",
      "org.hibernate" % "hibernate-entitymanager" % "4.1.9.Final",
      "cglib" % "cglib" % "2.2.2"
)

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
routesGenerator := InjectedRoutesGenerator

fork in run := true