# Soluling's iOS Library

Over-the-air translation updates from Soluling Cloud for iOS and macOS applications: the Swift package `SolulingOta`. Its `Package.swift` is at the root of this repository, because Swift Package Manager reads it only there.

In Xcode, choose *File | Add Package Dependencies*, enter `https://github.com/soluling/I18N`, and add the `SolulingOta` library to the application target. Then initialize it when the application starts, with the project's GUID from its Manage page:

```swift
import SolulingOta

@main
struct DrivingApp: App {
  init() {
    SolulingOta.initialize(projectGuid: "project-guid")
  }

  var body: some Scene {
    WindowGroup { ContentView() }
  }
}
```

The library takes over the string lookup of the main bundle, so `NSLocalizedString`, `String(localized:)`, `Bundle.main.localizedString`, and `String.localizedStringWithFormat` return the downloaded strings. It downloads the published `.strings` and `.stringsdict` files of the user's language, or converts a published String Catalog (`.xcstrings`), once a day.

## Build and test

Needs a Mac with Xcode, or the GitHub Actions workflow `.github/workflows/ios-ota.yml`, which runs the tests on macOS and on an iOS Simulator for every push that changes the package.

```
swift test
xcodebuild test -scheme SolulingOta -destination "platform=iOS Simulator,name=iPhone 17"
```

The tests stub the network (`StubProtocol`), so they need no connection.
