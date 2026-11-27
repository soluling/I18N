// swift-tools-version:5.9
// Soluling's Swift package: over-the-air translation updates for iOS and macOS (Library/iOS).
// Add https://github.com/soluling/I18N in Xcode, and the SolulingOta library to the application.
import PackageDescription

let package = Package(
  name: "Soluling",
  defaultLocalization: "en",
  platforms: [
    .iOS(.v13),
    .macOS(.v11)
  ],
  products: [
    .library(name: "SolulingOta", targets: ["SolulingOta"])
  ],
  targets: [
    .target(
      name: "SolulingOta",
      path: "Library/iOS/Sources/SolulingOta"),
    .testTarget(
      name: "SolulingOtaTests",
      dependencies: ["SolulingOta"],
      path: "Library/iOS/Tests/SolulingOtaTests",
      resources: [.process("Resources")])
  ]
)
