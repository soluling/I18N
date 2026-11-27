plugins {
  id("com.android.library")
  id("com.vanniktech.maven.publish") version "0.37.0"
}

// The Maven coordinates: implementation("com.soluling:ota:<version>")
val otaGroup = "com.soluling"
val otaArtifact = "ota"
val otaVersion = "1.0.0"

android {
  namespace = "com.soluling.ota"
  compileSdk = 37

  defaultConfig {
    minSdk = 24
    testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
  }

  compileOptions {
    sourceCompatibility = JavaVersion.VERSION_17
    targetCompatibility = JavaVersion.VERSION_17
  }
}

dependencies {
  androidTestImplementation("androidx.test:runner:1.7.0")
  androidTestImplementation("androidx.test.ext:junit:1.3.0")
  androidTestImplementation("androidx.test:core:1.7.0")
  androidTestImplementation("androidx.appcompat:appcompat:1.7.1")
}

// Publishing to Maven Central: gradlew publishAndReleaseToMavenCentral. The Central Portal token
// (mavenCentralUsername, mavenCentralPassword) and the signing key (signingInMemoryKey,
// signingInMemoryKeyId, signingInMemoryKeyPassword) are in %USERPROFILE%\.gradle\gradle.properties,
// never in this repository. Without the key, publishToMavenLocal works unsigned.
mavenPublishing {
  publishToMavenCentral()

  if (project.hasProperty("signingInMemoryKey")) {
    signAllPublications()
  }

  coordinates(otaGroup, otaArtifact, otaVersion)

  pom {
    name.set("Soluling OTA")
    description.set("Over-the-air translation updates from Soluling Cloud for Android applications")
    inceptionYear.set("2026")
    url.set("https://www.soluling.com/Guides/Android#ota")

    licenses {
      license {
        name.set("MIT License")
        url.set("https://opensource.org/licenses/MIT")
        distribution.set("repo")
      }
    }

    developers {
      developer {
        id.set("soluling")
        name.set("Soluling")
        organization.set("Soluling Oy")
        organizationUrl.set("https://www.soluling.com")
      }
    }

    scm {
      url.set("https://github.com/soluling/I18N")
      connection.set("scm:git:https://github.com/soluling/I18N.git")
      developerConnection.set("scm:git:ssh://git@github.com/soluling/I18N.git")
    }
  }
}
