import Foundation

/// The downloaded translations: checks the project's manifest, downloads the changed files of one
/// language, and keeps them in Application Support/SolulingOta:
///
/// - files: the downloaded files, named by their hash, so a file is never downloaded twice
/// - bundles/<id>: the strings files in use, as a bundle (String Catalogs converted). A new set of
///   files gets a new folder, because Bundle caches a bundle and its tables by path.
/// - state.json: the files in use, the language, and the time of the last check
final class OtaStore {
  struct State: Codable {
    var lastCheck: Double = 0   // Seconds since 1970, 0 if never
    var locale: String?         // The language that was checked
    var language: String?       // The published language of the files, nil if none
    var folder: String?         // The bundle folder, nil if none
    var files: [FileEntry] = []
  }

  struct FileEntry: Codable, Equatable {
    var fileName: String
    var hash: String
  }

  // One published file of the manifest
  struct ManifestItem {
    var fileName: String
    var language: String?
    var hash: String
    var url: URL
  }

  /// A downloaded string: valid is nil until its placeholders have been checked
  struct Found {
    var value: String
    var cacheKey: String
    var valid: Bool?
  }

  enum OtaError: Error {
    case noManifest
    case http(Int, URL)
    case invalidFile(String)
  }

  private let options: SolulingOta.Options
  private let root: URL
  private let session: URLSession
  private let queue = DispatchQueue(label: "com.soluling.ota")
  private let lock = NSLock()

  private var state = State()
  private var bundle: Bundle?
  private var verified: [String: Bool] = [:]
  private var result = SolulingOta.Result.notChecked

  // Something no translation is, to see that the downloaded bundle doesn't have a string
  private static let missing = "\u{1}SolulingOta.missing\u{1}"

  init(options: SolulingOta.Options) {
    self.options = options

    if let folder = options.folder {
      root = folder
    } else {
      let support = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask).first ?? FileManager.default.temporaryDirectory
      root = support.appendingPathComponent("SolulingOta", isDirectory: true)
    }

    if let session = options.session {
      self.session = session
    } else {
      let configuration = URLSessionConfiguration.default
      configuration.timeoutIntervalForRequest = options.timeout
      configuration.requestCachePolicy = .reloadIgnoringLocalCacheData
      self.session = URLSession(configuration: configuration)
    }
  }

  /// The default file filter: strings files, plurals, and String Catalogs
  static func isStringsFile(_ fileName: String) -> Bool {
    let ext = (fileName as NSString).pathExtension.lowercased()
    return ext == "strings" || ext == "stringsdict" || ext == "xcstrings"
  }

  var lastResult: SolulingOta.Result {
    lock.lock()
    defer { lock.unlock() }
    return result
  }

  var language: String? {
    lock.lock()
    defer { lock.unlock() }
    return state.language
  }

  /// Reads the state of the earlier downloads
  func load() {
    guard let data = try? Data(contentsOf: root.appendingPathComponent("state.json")),
          let loaded = try? JSONDecoder().decode(State.self, from: data) else {
      return
    }

    lock.lock()
    state = loaded
    bundle = loaded.folder.flatMap { Bundle(path: bundlesFolder.appendingPathComponent($0).path) }
    verified = [:]
    lock.unlock()
  }

  // The language whose downloaded strings are used: the option or the user's first one
  private var currentLanguage: String {
    return options.language ?? Locale.preferredLanguages.first ?? "en"
  }

  /// The downloaded string of a key, or nil if there is none for the current language
  func find(key: String, table: String?) -> Found? {
    lock.lock()
    let current = bundle
    let language = state.language
    lock.unlock()

    guard let current = current, let language = language,
          OtaStore.candidates(currentLanguage).contains(OtaStore.normalize(language)) else {
      return nil
    }

    let value = current.localizedString(forKey: key, value: OtaStore.missing, table: table)

    if value == OtaStore.missing {
      return nil
    }

    let cacheKey = (table ?? "Localizable") + "\u{0}" + key

    lock.lock()
    let valid = verified[cacheKey]
    lock.unlock()

    return Found(value: value, cacheKey: cacheKey, valid: valid)
  }

  func setValid(_ valid: Bool, for cacheKey: String, value: String) {
    lock.lock()
    verified[cacheKey] = valid
    lock.unlock()
  }

  func checkAsync(force: Bool, completion: ((SolulingOta.Result) -> Void)?) {
    queue.async {
      let value = self.check(force: force)

      if let completion = completion {
        DispatchQueue.main.async { completion(value) }
      }
    }
  }

  // Runs on the queue
  private func check(force: Bool) -> SolulingOta.Result {
    let locale = currentLanguage
    let now = Date().timeIntervalSince1970

    lock.lock()
    let current = state
    lock.unlock()

    if !force && current.lastCheck > 0 && now >= current.lastCheck && now - current.lastCheck < options.checkInterval
      && locale.caseInsensitiveCompare(current.locale ?? "") == .orderedSame {
      return .notChecked
    }

    var value: SolulingOta.Result

    do {
      value = try download(locale: locale, now: now) ? .updated : .upToDate
    } catch {
      NSLog("SolulingOta: cannot check for new translations: %@", String(describing: error))
      value = .failed
    }

    lock.lock()
    result = value
    let language = state.language
    lock.unlock()

    if value == .updated, let onUpdated = options.onUpdated {
      DispatchQueue.main.async { onUpdated(language) }
    }

    return value
  }

  private var filesFolder: URL {
    return root.appendingPathComponent("files", isDirectory: true)
  }

  private var bundlesFolder: URL {
    return root.appendingPathComponent("bundles", isDirectory: true)
  }

  // Downloads the changed files of the best published language. True if the strings in use changed.
  private func download(locale: String, now: Double) throws -> Bool {
    let manifest = try readManifest()
    let candidates = OtaStore.candidates(locale)
    var language: String?
    var chosen: [ManifestItem] = []

    // Strings files of the language or its parent
    for candidate in candidates {
      chosen = manifest.filter {
        options.fileFilter($0.fileName) && !OtaStore.isCatalog($0.fileName) && $0.language.map(OtaStore.normalize) == candidate
      }

      if !chosen.isEmpty {
        language = chosen[0].language
        break
      }
    }

    // String Catalogs hold every language: one with no language, or one of the candidates
    let catalogs = manifest.filter {
      options.fileFilter($0.fileName) && OtaStore.isCatalog($0.fileName) && ($0.language.map { candidates.contains(OtaStore.normalize($0)) } ?? true)
    }

    try FileManager.default.createDirectory(at: filesFolder, withIntermediateDirectories: true)

    for item in chosen + catalogs {
      try downloadFile(item)
    }

    // The language of each catalog: the strings files' language, or the best one it has
    var catalogLanguages: [(item: ManifestItem, language: String)] = []

    for item in catalogs {
      let available = try StringCatalog.languages(Data(contentsOf: file(item.hash)))
      let wanted = language.map { [OtaStore.normalize($0)] } ?? candidates

      if let pick = wanted.lazy.compactMap({ candidate in available.first { OtaStore.normalize($0) == candidate } }).first {
        language = language ?? pick
        catalogLanguages.append((item, pick))
      }
    }

    let files = (chosen + catalogLanguages.map { $0.item }).map { FileEntry(fileName: $0.fileName, hash: $0.hash) }

    lock.lock()
    let current = state
    lock.unlock()

    var updated = State(lastCheck: now, locale: locale, language: language, folder: current.folder, files: files)
    let changed = (files != current.files) || (language != current.language)

    if changed {
      if files.isEmpty {
        updated.folder = nil
      } else {
        updated.folder = try buildBundle(strings: chosen, catalogs: catalogLanguages)
      }
    }

    try saveState(updated)

    lock.lock()
    state = updated

    if changed {
      bundle = updated.folder.flatMap { Bundle(path: bundlesFolder.appendingPathComponent($0).path) }
      verified = [:]
    }

    lock.unlock()

    deleteUnused(updated)
    return changed
  }

  // Writes the strings files into a new bundle folder; returns its name
  private func buildBundle(strings: [ManifestItem], catalogs: [(item: ManifestItem, language: String)]) throws -> String {
    let name = UUID().uuidString
    let folder = bundlesFolder.appendingPathComponent(name, isDirectory: true)

    try FileManager.default.createDirectory(at: folder, withIntermediateDirectories: true)

    for item in strings {
      let target = folder.appendingPathComponent((item.fileName as NSString).lastPathComponent)
      try? FileManager.default.removeItem(at: target)
      try FileManager.default.copyItem(at: file(item.hash), to: target)
    }

    for (item, language) in catalogs {
      let table = ((item.fileName as NSString).lastPathComponent as NSString).deletingPathExtension
      let converted = try StringCatalog.convert(Data(contentsOf: file(item.hash)), language: language)

      try write(converted.strings, to: folder.appendingPathComponent(table + ".strings"))
      try write(converted.plurals, to: folder.appendingPathComponent(table + ".stringsdict"))
    }

    return name
  }

  // A strings file can be a property list: Bundle reads both
  private func write(_ dictionary: [String: Any], to url: URL) throws {
    if dictionary.isEmpty {
      return
    }

    try PropertyListSerialization.data(fromPropertyList: dictionary, format: .xml, options: 0).write(to: url)
  }

  // The static manifest on the CDN first - it costs nothing on Soluling's servers however many
  // applications check it - and the API's if the CDN doesn't have it yet
  private func readManifest() throws -> [ManifestItem] {
    var data: Data?

    if let cdnUrl = options.cdnUrl {
      data = try? fetch(cdnUrl.appendingPathComponent(options.projectGuid).appendingPathComponent("manifest.json"))
    }

    if data == nil, let apiUrl = options.apiUrl {
      data = try fetch(apiUrl.appendingPathComponent("artifacts").appendingPathComponent(options.projectGuid).appendingPathComponent("manifest"))
    }

    guard let json = data, let items = try JSONSerialization.jsonObject(with: json) as? [[String: Any]] else {
      throw OtaError.noManifest
    }

    return items.compactMap { item in
      guard let hash = item["hash"] as? String, !hash.isEmpty,
            let address = item["url"] as? String, let url = URL(string: address) else {
        return nil
      }

      return ManifestItem(fileName: (item["fileName"] as? String) ?? "", language: item["language"] as? String, hash: hash, url: url)
    }
  }

  // The body of a 200 response, nil for 404, and an error for others
  private func fetch(_ url: URL) throws -> Data? {
    var request = URLRequest(url: url)
    request.timeoutInterval = options.timeout

    let semaphore = DispatchSemaphore(value: 0)
    var body: Data?
    var status = 0
    var failure: Error?

    session.dataTask(with: request) { data, response, error in
      body = data
      status = (response as? HTTPURLResponse)?.statusCode ?? 0
      failure = error
      semaphore.signal()
    }.resume()

    semaphore.wait()

    if let failure = failure {
      throw failure
    }

    if status == 404 {
      return nil
    }

    if status != 200 {
      throw OtaError.http(status, url)
    }

    return body ?? Data()
  }

  // Downloads a file unless it already is, and checks that it can be read
  private func downloadFile(_ item: ManifestItem) throws {
    let target = file(item.hash)

    if FileManager.default.fileExists(atPath: target.path) {
      return
    }

    guard let data = try fetch(item.url) else {
      throw OtaError.http(404, item.url)
    }

    if OtaStore.isCatalog(item.fileName) {
      _ = try StringCatalog.languages(data)
    } else if !((try? PropertyListSerialization.propertyList(from: data, format: nil)) is [String: Any]) {
      throw OtaError.invalidFile(item.fileName)
    }

    try data.write(to: target, options: .atomic)
  }

  private func saveState(_ value: State) throws {
    try FileManager.default.createDirectory(at: root, withIntermediateDirectories: true)
    try JSONEncoder().encode(value).write(to: root.appendingPathComponent("state.json"), options: .atomic)
  }

  // Deletes the files and bundle folders that the state doesn't use
  private func deleteUnused(_ value: State) {
    let manager = FileManager.default
    let usedFiles = Set(value.files.map { file($0.hash).lastPathComponent })

    for url in (try? manager.contentsOfDirectory(at: filesFolder, includingPropertiesForKeys: nil)) ?? [] where !usedFiles.contains(url.lastPathComponent) {
      try? manager.removeItem(at: url)
    }

    for url in (try? manager.contentsOfDirectory(at: bundlesFolder, includingPropertiesForKeys: nil)) ?? [] where url.lastPathComponent != value.folder {
      try? manager.removeItem(at: url)
    }
  }

  /// Deletes the downloaded files, so the built-in strings are used until the next check
  func clear() {
    queue.async {
      self.lock.lock()
      self.state = State()
      self.bundle = nil
      self.verified = [:]
      self.lock.unlock()

      try? FileManager.default.removeItem(at: self.root)
    }
  }

  // The file of a hash. A hash may have characters that a file name can't.
  private func file(_ hash: String) -> URL {
    let name = String(hash.unicodeScalars.map { CharacterSet.alphanumerics.contains($0) || $0 == "-" || $0 == "_" ? Character($0) : "_" })
    return filesFolder.appendingPathComponent(name)
  }

  static func isCatalog(_ fileName: String) -> Bool {
    return (fileName as NSString).pathExtension.lowercased() == "xcstrings"
  }

  static func normalize(_ language: String) -> String {
    return language.replacingOccurrences(of: "_", with: "-").lowercased()
  }

  /// The language and its parents, most specific first: zh-Hant-TW, zh-Hant, zh-TW, zh
  static func candidates(_ languageTag: String) -> [String] {
    let parts = normalize(languageTag).split(separator: "-").map(String.init)
    var result: [String] = []

    func add(_ value: String) {
      if !value.isEmpty && !result.contains(value) {
        result.append(value)
      }
    }

    guard let language = parts.first else {
      return result
    }

    let script = parts.dropFirst().first { $0.count == 4 }
    let region = parts.dropFirst().first { $0.count == 2 || ($0.count == 3 && Int($0) != nil) }

    add(parts.joined(separator: "-"))

    if let script = script {
      add(language + "-" + script)
    }

    if let region = region {
      add(language + "-" + region)
    }

    add(language)
    return result
  }
}
