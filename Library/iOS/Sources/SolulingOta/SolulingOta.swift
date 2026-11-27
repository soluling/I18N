import Foundation
import ObjectiveC

/// Over-the-air translation updates from Soluling Cloud.
///
/// When you translate in Soluling Cloud and publish the project's files, the application gets the
/// new translations without a new release. Once a day, the library reads the project's manifest
/// from Soluling's CDN, downloads the changed strings files of the user's language
/// (`fi.lproj/Localizable.strings`, `.stringsdict`, or a String Catalog), and keeps them in the
/// application's Application Support folder. The downloaded strings are used instead of the ones
/// built into the application; a string that the files don't have, or whose placeholders differ
/// from the built-in one, comes from the application as before. Without a connection, or before
/// the first download, the built-in strings are used.
///
/// Initialize once, as early as possible:
///
///     @main
///     struct DrivingApp: App {
///       init() {
///         SolulingOta.initialize(projectGuid: "project-guid")
///       }
///       ...
///     }
///
/// The library takes over the string lookup of the main bundle, so `NSLocalizedString`,
/// `Bundle.main.localizedString`, and `String.localizedStringWithFormat` return the downloaded
/// strings. Strings of other bundles (frameworks, Swift packages) keep the built-in ones. Strings
/// already on the screen don't change; use `Options.onUpdated` to reload the user interface.
public enum SolulingOta {
  /// The published files on Soluling's CDN, where the manifest is read first
  public static let defaultCdnUrl = URL(string: "https://www.soluling.com/artifacts")!

  /// Soluling's API, for the manifest when the CDN doesn't have it
  public static let defaultApiUrl = URL(string: "https://www.soluling.com/api")!

  /// Seconds between the checks for newer translations: a day
  public static let defaultCheckInterval: TimeInterval = 24 * 60 * 60

  /// The result of a check for newer translations
  public enum Result {
    /// Not checked: the last check is more recent than the check interval, or not initialized
    case notChecked
    /// Checked, and the downloaded translations are the latest
    case upToDate
    /// Checked, and new translations were downloaded, or the published ones were removed
    case updated
    /// Soluling's servers could not be reached, or a file could not be downloaded
    case failed
  }

  /// The options of `initialize`
  public struct Options {
    /// The GUID of the Soluling Cloud project, from its Manage page
    public var projectGuid: String

    /// The published files on Soluling's CDN. Nil reads the manifest from the API only.
    public var cdnUrl: URL? = SolulingOta.defaultCdnUrl

    /// Soluling's API, for the manifest when the CDN doesn't have it
    public var apiUrl: URL? = SolulingOta.defaultApiUrl

    /// Seconds between the checks. Default a day; 0 checks every time the application starts.
    public var checkInterval: TimeInterval = SolulingOta.defaultCheckInterval

    /// Seconds to wait for Soluling's servers
    public var timeout: TimeInterval = 10

    /// The language to download and use, such as "fi". Default nil: the user's first preferred
    /// language, which follows the device and the per-app language. Set it if the application
    /// chooses its language itself.
    public var language: String?

    /// Which published files to download. Default: .strings, .stringsdict, and .xcstrings files.
    public var fileFilter: (String) -> Bool = OtaStore.isStringsFile

    /// Called on the main thread when new translations were downloaded, with their language, or
    /// nil if the published translations of the language were removed
    public var onUpdated: ((String?) -> Void)?

    // For the tests: the folder of the downloads, the network, and the bundle to take over
    var folder: URL?
    var session: URLSession?
    var bundle: Bundle?

    public init(projectGuid: String) {
      self.projectGuid = projectGuid.trimmingCharacters(in: .whitespacesAndNewlines)
    }
  }

  private static let lock = NSLock()
  private static var store: OtaStore?

  /// Starts using the downloaded translations, and checks for newer ones in the background if the
  /// last check is older than a day. Call once, when the application starts.
  public static func initialize(projectGuid: String) {
    initialize(Options(projectGuid: projectGuid))
  }

  /// Starts using the downloaded translations with the given options. Calls after the first are ignored.
  public static func initialize(_ options: Options) {
    precondition(!options.projectGuid.isEmpty, "The project GUID is missing")

    lock.lock()

    if store != nil {
      lock.unlock()
      return
    }

    let newStore = OtaStore(options: options)
    newStore.load()
    store = newStore
    lock.unlock()

    OtaBundle.install(on: options.bundle ?? Bundle.main)
    newStore.checkAsync(force: false, completion: nil)
  }

  /// Checks for newer translations in the background, and calls completion on the main thread
  /// when done.
  /// - Parameter force: true checks even if the last check is more recent than the check interval
  public static func update(force: Bool = false, completion: ((Result) -> Void)? = nil) {
    if let current = currentStore() {
      current.checkAsync(force: force, completion: completion)
    } else if let completion = completion {
      DispatchQueue.main.async { completion(.notChecked) }
    }
  }

  /// Deletes the downloaded translations, so the built-in ones are used until the next check
  public static func clear() {
    currentStore()?.clear()
  }

  /// The result of the last check
  public static var lastResult: Result {
    return currentStore()?.lastResult ?? .notChecked
  }

  /// The language of the downloaded translations, such as "fi", or nil if there are none
  public static var language: String? {
    return currentStore()?.language
  }

  static func currentStore() -> OtaStore? {
    lock.lock()
    defer { lock.unlock() }
    return store
  }

  // For the tests: forget the initialization, so initialize can be called again
  static func reset() {
    lock.lock()
    store = nil
    lock.unlock()
  }
}

/// The class that the main bundle gets: its string lookup returns the downloaded strings first
final class OtaBundle: Bundle {
  static func install(on bundle: Bundle) {
    if !(bundle is OtaBundle) {
      object_setClass(bundle, OtaBundle.self)
    }
  }

  override func localizedString(forKey key: String, value: String?, table tableName: String?) -> String {
    if let store = SolulingOta.currentStore(), let found = store.find(key: key, table: tableName) {
      if let valid = found.valid {
        if valid {
          return found.value
        }
      } else {
        // The first time: valid if its placeholders are the built-in string's
        let original = super.localizedString(forKey: key, value: value, table: tableName)
        let valid = Placeholders.of(found.value) == Placeholders.of(original)

        store.setValid(valid, for: found.cacheKey, value: found.value)
        return valid ? found.value : original
      }
    }

    return super.localizedString(forKey: key, value: value, table: tableName)
  }
}
