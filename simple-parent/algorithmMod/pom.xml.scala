<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
  <modelVersion>4.0.0</modelVersion>
  <groupId>learningScala</groupId>
  <artifactId>scala118.simple</artifactId>
  <version>0.0.1-SNAPSHOT</version>
  <name>${project.artifactId}</name>
  <description>My wonderfull scala app</description>
  <inceptionYear>2010</inceptionYear>
  <licenses>
    <license>
      <name>My License</name>
      <url>http://....</url>
      <distribution>repo</distribution>
    </license>
  </licenses>

  <properties>
    <maven.compiler.source>1.8</maven.compiler.source>
    <maven.compiler.target>1.8</maven.compiler.target>
    <encoding>UTF-8</encoding>
    <scala.tools.version>2.10</scala.tools.version>
    <scala.version>2.11.8</scala.version>
  </properties>

  <dependencies>
<!-- https://mvnrepository.com/artifact/com.typesafe.akka/akka-actor_2.11 -->
<dependency>
    <groupId>com.typesafe.akka</groupId>
    <artifactId>akka-actor_2.11</artifactId>
    <version>2.3.8</version>
</dependency>

  <!-- https://mvnrepository.com/artifact/org.scala-lang/scala-library -->
<dependency>
    <groupId>org.scala-lang</groupId>
    <artifactId>scala-library</artifactId>
    <version>2.11.8</version>
</dependency>
  <!-- 
    <dependency>
      <groupId>org.scala-lang</groupId>
      <artifactId>scala-library</artifactId>
      <version>${scala.version}</version>
    </dependency>
-->
    <!-- Test -->
    <dependency>
      <groupId>junit</groupId>
      <artifactId>junit</artifactId>
      <version>4.11</version>
      <scope>test</scope>
    </dependency>
    <!-- https://mvnrepository.com/artifact/org.specs2/specs2_2.11 -->
<dependency>
    <groupId>org.specs2</groupId>
    <artifactId>specs2_2.11</artifactId>
    <version>3.7</version>
    <type>pom</type>
</dependency>
<!-- 
    <dependency>
      <groupId>org.specs2</groupId>
      <artifactId>specs2_${scala.tools.version}</artifactId>
      <version>1.13</version>
      <scope>test</scope>
    </dependency>
    -->
    <!-- https://mvnrepository.com/artifact/org.scalatest/scalatest_2.11 -->
<dependency>
    <groupId>org.scalatest</groupId>
    <artifactId>scalatest_2.11</artifactId>
    <version>2.2.2</version>
    <scope>test</scope>
</dependency>
<!-- https://mvnrepository.com/artifact/com.typesafe.akka/akka-testkit_2.11 -->
<dependency>
    <groupId>com.typesafe.akka</groupId>
    <artifactId>akka-testkit_2.11</artifactId>
    <version>2.4.2-RC2</version>
</dependency>

 <!-- 
    <dependency>
      <groupId>org.scalatest</groupId>
      <artifactId>scalatest_${scala.tools.version}</artifactId>
      <version>2.0.M6-SNAP8</version>
      <scope>test</scope>
    </dependency>
    -->
  </dependencies>

  <build>
    <sourceDirectory>src/main/scala</sourceDirectory>
    <testSourceDirectory>src/test/scala</testSourceDirectory>
    <plugins>
      <plugin>
        <!-- see http://davidb.github.com/scala-maven-plugin -->
        <groupId>net.alchim31.maven</groupId>
        <artifactId>scala-maven-plugin</artifactId>
        <version>3.1.3</version>
        <executions>
          <execution>
            <goals>
              <goal>compile</goal>
              <goal>testCompile</goal>
            </goals>
            <configuration>
              <args>
              <!--
                <arg>-make:transitive</arg>
                -->
                <arg>-dependencyfile</arg>
                <arg>${project.build.directory}/.scala_dependencies</arg>
              </args>
            </configuration>
          </execution>
        </executions>
      </plugin>
      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-surefire-plugin</artifactId>
        <version>2.13</version>
        <configuration>
          <useFile>false</useFile>
          <disableXmlReport>true</disableXmlReport>
          <!-- If you have classpath issue like NoDefClassError,... -->
          <!-- useManifestOnlyJar>false</useManifestOnlyJar -->
          <includes>
            <include>**/*Test.*</include>
            <include>**/*Suite.*</include>
          </includes>
        </configuration>
      </plugin>
    </plugins>
  </build>
</project>
