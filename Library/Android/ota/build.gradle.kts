plugins {
  id("com.android.library")
  id("maven-publish")
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

  publishing {
    singleVariant("release") {
      withSourcesJar()
    }
  }
}

dependencies {
  androidTestImplementation("androidx.test:runner:1.7.0")
  androidTestImplementation("androidx.test.ext:junit:1.3.0")
  androidTestImplementation("androidx.test:core:1.7.0")
  androidTestImplementation("androidx.appcompat:appcompat:1.7.1")
}

publishing {
  publications {
    register<MavenPublication>("release") {
      groupId = otaGroup
      artifactId = otaArtifact
      version = otaVersion

      afterEvaluate {
        from(components["release"])
      }

      pom {
        name.set("Soluling OTA")
        description.set("Over-the-air translation updates from Soluling Cloud for Android applications")
        url.set("https://www.soluling.com/Guides/Android#ota")

        licenses {
          license {
            name.set("MIT License")
            url.set("https://opensource.org/licenses/MIT")
          }
        }

        developers {
          developer {
            organization.set("Soluling Oy")
            organizationUrl.set("https://www.soluling.com")
          }
        }

        scm {
          url.set("https://github.com/soluling/I18N")
        }
      }
    }
  }
}
