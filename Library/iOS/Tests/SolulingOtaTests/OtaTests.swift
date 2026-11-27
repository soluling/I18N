import ObjectiveC
import XCTest
@testable import SolulingOta

final class OtaTests: XCTestCase {
  private static let guid = "11111111-2222-3333-4444-555555555555"
  private static let cdnManifest = "/artifacts/\(guid)/manifest.json"
  private static let apiManifest = "/api/artifacts/\(guid)/manifest"
  private static let fiStrings = "/artifacts/12/h1/fi.lproj/Localizable.strings"
  private static let fiStrings2 = "/artifacts/12/h2/fi.lproj/Localizable.strings"
  private static let fiPlurals = "/artifacts/12/h3/fi.lproj/Localizable.stringsdict"
  private static let fiMain = "/artifacts/12/h4/fi.lproj/Main.strings"
  private static let deStrings = "/artifacts/12/h5/de.lproj/Localizable.strings"
  private static let catalog = "/artifacts/12/h6/Localizable.xcstrings"

  private static let finnish = """
    /* Published Finnish strings */
    "hello" = "Moi maailma";
    "time" = "Ajoaika on %1$@ ja %2$@";
    "greeting" = "Hei %2$@";
    """

  private static let finnishPlurals = """
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
      <key>files</key>
      <dict>
        <key>NSStringLocalizedFormatKey</key>
        <string>%#@files@</string>
        <key>files</key>
        <dict>
          <key>NSStringFormatSpecTypeKey</key>
          <string>NSStringPluralRuleType</string>
          <key>NSStringFormatValueTypeKey</key>
          <string>d</string>
          <key>one</key>
          <string>%d tiedosto</string>
          <key>other</key>
          <string>%d tiedostoa</string>
        </dict>
      </dict>
    </dict>
    </plist>
    """

  // A String Catalog with every kind of entry: a string, a plural, a string with a plural
  // argument, a string that varies by device, and an untranslated one
  private static let stringCatalog = """
    {
      "sourceLanguage" : "en",
      "strings" : {
        "hello" : {
          "localizations" : {
            "en" : { "stringUnit" : { "state" : "translated", "value" : "Hello World" } },
            "fi" : { "stringUnit" : { "state" : "translated", "value" : "Hei maailma" } }
          }
        },
        "items" : {
          "localizations" : {
            "fi" : {
              "variations" : {
                "plural" : {
                  "one" : { "stringUnit" : { "state" : "translated", "value" : "%lld kohde" } },
                  "other" : { "stringUnit" : { "state" : "translated", "value" : "%lld kohdetta" } }
                }
              }
            }
          }
        },
        "found" : {
          "localizations" : {
            "fi" : {
              "stringUnit" : { "state" : "translated", "value" : "Löytyi %1$#@count@" },
              "substitutions" : {
                "count" : {
                  "argNum" : 1,
                  "formatSpecifier" : "lld",
                  "variations" : {
                    "plural" : {
                      "one" : { "stringUnit" : { "state" : "translated", "value" : "%arg tiedosto" } },
                      "other" : { "stringUnit" : { "state" : "translated", "value" : "%arg tiedostoa" } }
                    }
                  }
                }
              }
            }
          }
        },
        "untouched" : {
          "localizations" : {
            "fi" : {
              "variations" : {
                "device" : {
                  "iphone" : { "stringUnit" : { "state" : "translated", "value" : "iPhone" } },
                  "other" : { "stringUnit" : { "state" : "translated", "value" : "Muu" } }
                }
              }
            }
          }
        },
        "greeting" : { }
      },
      "version" : "1.0"
    }
    """

  private var folder: URL!

  override func setUp() {
    super.setUp()
    SolulingOta.reset()
    StubProtocol.reset()

    folder = FileManager.default.temporaryDirectory.appendingPathComponent("SolulingOtaTests-" + UUID().uuidString, isDirectory: true)

    StubProtocol.put(Self.cdnManifest, 200, manifest())
    StubProtocol.put(Self.fiStrings, 200, Self.finnish)
    StubProtocol.put(Self.fiPlurals, 200, Self.finnishPlurals)
    StubProtocol.put(Self.fiMain, 200, "\"r8V-Nr-Axy.text\" = \"Esimerkki\";")
    StubProtocol.put(Self.deStrings, 200, "\"hello\" = \"Hallo Welt\";")
    StubProtocol.put(Self.catalog, 200, Self.stringCatalog)
  }

  override func tearDown() {
    SolulingOta.reset()
    try? FileManager.default.removeItem(at: folder)
    super.tearDown()
  }

  private func item(_ fileName: String, _ language: String?, _ hash: String, _ path: String) -> String {
    let languageJson = language.map { "\"\($0)\"" } ?? "null"
    return "{\"fileName\":\"\(fileName)\",\"language\":\(languageJson),\"hash\":\"\(hash)\",\"size\":100,\"url\":\"\(StubProtocol.baseUrl)\(path)\"}"
  }

  private func manifest(stringsHash: String = "h1", stringsPath: String = OtaTests.fiStrings) -> String {
    return "[" + [
      item("de.lproj/Localizable.strings", "de", "h5", Self.deStrings),
      item("fi.lproj/Localizable.strings", "fi", stringsHash, stringsPath),
      item("fi.lproj/Localizable.stringsdict", "fi", "h3", Self.fiPlurals),
      item("fi.lproj/Main.strings", "fi", "h4", Self.fiMain),
      item("README.md", nil, "h7", "/artifacts/12/h7/README.md")
    ].joined(separator: ",") + "]"
  }

  private func options(language: String = "fi") -> SolulingOta.Options {
    var result = SolulingOta.Options(projectGuid: Self.guid)
    result.cdnUrl = URL(string: StubProtocol.baseUrl + "/artifacts")
    result.apiUrl = URL(string: StubProtocol.baseUrl + "/api")
    result.language = language
    result.folder = folder
    result.session = StubProtocol.session()
    result.bundle = Bundle.module
    return result
  }

  // Initializes, and waits for the check that initialize starts
  @discardableResult
  private func initialize(_ options: SolulingOta.Options) -> SolulingOta.Result {
    SolulingOta.initialize(options)
    update(force: false)
    return SolulingOta.lastResult
  }

  @discardableResult
  private func update(force: Bool) -> SolulingOta.Result {
    let done = expectation(description: "check")
    var result = SolulingOta.Result.notChecked

    SolulingOta.update(force: force) { value in
      result = value
      done.fulfill()
    }

    wait(for: [done], timeout: 20)
    return result
  }

  private func string(_ key: String, table: String? = nil) -> String {
    return NSLocalizedString(key, tableName: table, bundle: Bundle.module, comment: "")
  }

  func testNotInitialized_UsesTheBuiltInStrings() {
    XCTAssertEqual(string("hello"), "Hello World")
    XCTAssertEqual(SolulingOta.lastResult, .notChecked)
    XCTAssertNil(SolulingOta.language)
  }

  func testDownloadedStrings_ReplaceTheBuiltInOnes() {
    XCTAssertEqual(initialize(options()), .updated)
    XCTAssertEqual(SolulingOta.language, "fi")

    XCTAssertEqual(string("hello"), "Moi maailma")
    XCTAssertEqual(Bundle.module.localizedString(forKey: "hello", value: nil, table: nil), "Moi maailma")
    XCTAssertEqual(String(format: string("time"), "2 tuntia", "30 minuuttia"), "Ajoaika on 2 tuntia ja 30 minuuttia")
    XCTAssertEqual(string("r8V-Nr-Axy.text", table: "Main"), "Esimerkki")

    // Plurals from the .stringsdict
    XCTAssertEqual(String.localizedStringWithFormat(string("files"), 1), "1 tiedosto")
    XCTAssertEqual(String.localizedStringWithFormat(string("files"), 5), "5 tiedostoa")

    // Not in the files
    XCTAssertEqual(string("untouched"), "Untouched")

    // Rejected: other placeholders than the built-in string's
    XCTAssertEqual(String(format: string("greeting"), "Maija"), "Hello Maija")

    // Only the Finnish files: not the German file or the README
    XCTAssertEqual(StubProtocol.count(Self.fiStrings), 1)
    XCTAssertEqual(StubProtocol.count(Self.deStrings), 0)
    XCTAssertEqual(StubProtocol.count("/artifacts/12/h7/README.md"), 0)
  }

  func testStringLocalized_GetsTheDownloadedStrings() throws {
    guard #available(iOS 15, macOS 12, *) else {
      throw XCTSkip("String(localized:) needs iOS 15")
    }

    initialize(options())
    XCTAssertEqual(String(localized: "hello", bundle: Bundle.module), "Moi maailma")
  }

  // Not a check: lists the localization methods of NSBundle in the log, for when an API doesn't
  // get the downloaded strings
  func testListBundleLookups() {
    var count: UInt32 = 0

    if let methods = class_copyMethodList(Bundle.self, &count) {
      for i in 0..<Int(count) {
        let name = NSStringFromSelector(method_getName(methods[i]))

        if name.lowercased().contains("localized") {
          print("NSBundle method: \(name)")
        }
      }

      free(methods)
    }
  }

  func testOtherLanguages_UseTheBuiltInStrings() {
    XCTAssertEqual(initialize(options(language: "sv")), .upToDate)
    XCTAssertNil(SolulingOta.language)
    XCTAssertEqual(string("hello"), "Hello World")
  }

  func testCountry_UsesTheLanguagesFiles() {
    XCTAssertEqual(initialize(options(language: "fi-FI")), .updated)
    XCTAssertEqual(string("hello"), "Moi maailma")
  }

  func testStringCatalog_IsConverted() {
    StubProtocol.put(Self.cdnManifest, 200, "[" + item("Localizable.xcstrings", nil, "h6", Self.catalog) + "]")

    XCTAssertEqual(initialize(options()), .updated)
    XCTAssertEqual(SolulingOta.language, "fi")
    XCTAssertEqual(string("hello"), "Hei maailma")
    XCTAssertEqual(String.localizedStringWithFormat(string("items"), 1), "1 kohde")
    XCTAssertEqual(String.localizedStringWithFormat(string("items"), 3), "3 kohdetta")
    XCTAssertEqual(String.localizedStringWithFormat(string("found"), 2), "Löytyi 2 tiedostoa")

    // A device variation and an untranslated string keep the built-in strings
    XCTAssertEqual(string("untouched"), "Untouched")
    XCTAssertEqual(String(format: string("greeting"), "Maija"), "Hello Maija")
  }

  func testCheckInterval_ChecksOnceADay() {
    initialize(options())

    XCTAssertEqual(update(force: false), .notChecked)
    XCTAssertEqual(StubProtocol.count(Self.cdnManifest), 1)

    // Forced: checks, but a file with the same hash is not downloaded again
    XCTAssertEqual(update(force: true), .upToDate)
    XCTAssertEqual(StubProtocol.count(Self.cdnManifest), 2)
    XCTAssertEqual(StubProtocol.count(Self.fiStrings), 1)

    // Interval 0: every start and every update checks - here initialize's and the helper's
    SolulingOta.reset()
    var everyTime = options()
    everyTime.checkInterval = 0
    XCTAssertEqual(initialize(everyTime), .upToDate)
    XCTAssertEqual(StubProtocol.count(Self.cdnManifest), 4)
  }

  func testNoManifestOnTheCdn_ReadsTheApi() {
    StubProtocol.remove(Self.cdnManifest)
    StubProtocol.put(Self.apiManifest, 200, manifest())

    XCTAssertEqual(initialize(options()), .updated)
    XCTAssertEqual(StubProtocol.count(Self.cdnManifest), 1)
    XCTAssertEqual(StubProtocol.count(Self.apiManifest), 1)
    XCTAssertEqual(string("hello"), "Moi maailma")
  }

  func testDownloadedStrings_AreKeptForTheNextStart() {
    initialize(options())

    SolulingOta.reset()
    StubProtocol.setFailAll(true)

    // Checked a moment ago: no check, and the saved strings are used
    XCTAssertEqual(initialize(options()), .notChecked)
    XCTAssertEqual(string("hello"), "Moi maailma")
    XCTAssertEqual(String.localizedStringWithFormat(string("files"), 5), "5 tiedostoa")
  }

  func testFailedCheck_KeepsTheDownloadedStrings() {
    initialize(options())
    StubProtocol.setFailAll(true)

    XCTAssertEqual(update(force: true), .failed)
    XCTAssertEqual(SolulingOta.lastResult, .failed)
    XCTAssertEqual(string("hello"), "Moi maailma")
  }

  func testNoConnection_UsesTheBuiltInStrings() {
    StubProtocol.setFailAll(true)

    XCTAssertEqual(initialize(options()), .failed)
    XCTAssertEqual(string("hello"), "Hello World")
  }

  func testNewPublish_IsDownloaded() {
    let updated = expectation(description: "onUpdated")
    updated.expectedFulfillmentCount = 2
    var language: String?
    var withListener = options()

    withListener.onUpdated = { value in
      language = value
      updated.fulfill()
    }

    initialize(withListener)

    StubProtocol.put(Self.cdnManifest, 200, manifest(stringsHash: "h2", stringsPath: Self.fiStrings2))
    StubProtocol.put(Self.fiStrings2, 200, Self.finnish.replacingOccurrences(of: "Moi maailma", with: "Hei maailma"))

    XCTAssertEqual(update(force: true), .updated)
    wait(for: [updated], timeout: 5)
    XCTAssertEqual(language, "fi")
    XCTAssertEqual(string("hello"), "Hei maailma")

    // The old file is deleted
    XCTAssertFalse(FileManager.default.fileExists(atPath: folder.appendingPathComponent("files/h1").path))
    XCTAssertTrue(FileManager.default.fileExists(atPath: folder.appendingPathComponent("files/h2").path))
  }

  func testUnpublishedLanguage_UsesTheBuiltInStrings() {
    initialize(options())
    StubProtocol.put(Self.cdnManifest, 200, "[]")

    XCTAssertEqual(update(force: true), .updated)
    XCTAssertNil(SolulingOta.language)
    XCTAssertEqual(string("hello"), "Hello World")
  }

  func testInvalidFile_IsNotUsed() {
    StubProtocol.put(Self.fiStrings, 200, "\"hello\" = \"Broken")

    XCTAssertEqual(initialize(options()), .failed)
    XCTAssertEqual(string("hello"), "Hello World")
  }

  func testClear_DeletesTheDownloadedStrings() {
    initialize(options())
    SolulingOta.clear()

    // The clear runs on the same queue as the checks. The next check downloads the files again,
    // so this one fails.
    StubProtocol.setFailAll(true)
    XCTAssertEqual(update(force: false), .failed)
    XCTAssertNil(SolulingOta.language)
    XCTAssertEqual(string("hello"), "Hello World")
  }

  func testPlaceholders_ArePositions() {
    XCTAssertEqual(Placeholders.of("%1$@ and %2$d"), Placeholders.of("%2$ld ja %1$@"))
    XCTAssertEqual(Placeholders.of("%@ and %d"), Placeholders.of("%1$@ ja %2$i"))
    XCTAssertEqual(Placeholders.of("%#@files@"), Placeholders.of("%1$#@files@"))
    XCTAssertNotEqual(Placeholders.of("%1$@"), Placeholders.of("%2$@"))
    XCTAssertTrue(Placeholders.of("100%%").isEmpty)
  }

  func testCandidates_GoFromTheLocaleToTheLanguage() {
    XCTAssertEqual(OtaStore.candidates("zh-Hant-TW"), ["zh-hant-tw", "zh-hant", "zh-tw", "zh"])
    XCTAssertEqual(OtaStore.candidates("fi_FI"), ["fi-fi", "fi"])
    XCTAssertEqual(OtaStore.candidates("fi"), ["fi"])
  }

  func testStringCatalogConversion() throws {
    let data = Data(Self.stringCatalog.utf8)

    XCTAssertEqual(try StringCatalog.languages(data), ["en", "fi"])

    let finnish = try StringCatalog.convert(data, language: "FI")
    XCTAssertEqual(finnish.strings["hello"], "Hei maailma")
    XCTAssertNil(finnish.strings["untouched"])
    XCTAssertNil(finnish.strings["greeting"])

    let items = finnish.plurals["items"] as? [String: Any]
    XCTAssertEqual(items?["NSStringLocalizedFormatKey"] as? String, "%#@value@")
    XCTAssertEqual((items?["value"] as? [String: Any])?["NSStringFormatValueTypeKey"] as? String, "lld")

    let found = finnish.plurals["found"] as? [String: Any]
    XCTAssertEqual(found?["NSStringLocalizedFormatKey"] as? String, "Löytyi %1$#@count@")
    XCTAssertEqual((found?["count"] as? [String: Any])?["one"] as? String, "%lld tiedosto")
  }
}
