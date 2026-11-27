# Soluling's Android Library

Over-the-air translation updates from Soluling Cloud (`com.soluling:ota`). See the [Android guide](https://www.soluling.com/Guides/Android#ota).

```kotlin
class App : Application() {
  override fun onCreate() {
    super.onCreate()
    SolulingOta.initialize(this, "project-guid")
  }
}

class MainActivity : AppCompatActivity() {
  override fun attachBaseContext(base: Context) {
    super.attachBaseContext(SolulingOta.wrap(base))
  }
}
```

## Build and test

Needs Android Studio (its JDK and the Android SDK). `local.properties` points to the SDK: `sdk.dir=C:\\Users\\<user>\\AppData\\Local\\Android\\Sdk`.

```
set JAVA_HOME=C:\Program Files\Android\Android Studio\jbr
gradlew :ota:assembleRelease                 the AAR: ota\build\outputs\aar\ota-release.aar
gradlew :ota:connectedDebugAndroidTest       the tests, on a running emulator or a device
gradlew :ota:publishToMavenLocal             com.soluling:ota into %USERPROFILE%\.m2
```

The tests (`ota\src\androidTest`) run a stand-in for Soluling's CDN and API inside the test, so they need no network. `OtaActivityTest` runs a real AppCompat activity.

The version is `otaVersion` in `ota\build.gradle.kts`.
