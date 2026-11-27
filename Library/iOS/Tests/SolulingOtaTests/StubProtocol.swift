import Foundation

/// A stand-in for Soluling's CDN and API: serves the responses set with put, and counts the requests
final class StubProtocol: URLProtocol {
  private static let lock = NSLock()
  private static var responses: [String: (status: Int, body: Data)] = [:]
  private static var counts: [String: Int] = [:]
  private static var failAll = false

  static let baseUrl = "https://ota.test"

  static func session() -> URLSession {
    let configuration = URLSessionConfiguration.ephemeral
    configuration.protocolClasses = [StubProtocol.self]
    return URLSession(configuration: configuration)
  }

  static func reset() {
    lock.lock()
    responses = [:]
    counts = [:]
    failAll = false
    lock.unlock()
  }

  static func put(_ path: String, _ status: Int, _ body: String) {
    lock.lock()
    responses[path] = (status, Data(body.utf8))
    lock.unlock()
  }

  static func remove(_ path: String) {
    lock.lock()
    responses[path] = nil
    lock.unlock()
  }

  // Every request gets 500
  static func setFailAll(_ value: Bool) {
    lock.lock()
    failAll = value
    lock.unlock()
  }

  static func count(_ path: String) -> Int {
    lock.lock()
    defer { lock.unlock() }
    return counts[path] ?? 0
  }

  override class func canInit(with request: URLRequest) -> Bool {
    return request.url?.host == "ota.test"
  }

  override class func canonicalRequest(for request: URLRequest) -> URLRequest {
    return request
  }

  override func startLoading() {
    let url = request.url!
    let path = url.path

    StubProtocol.lock.lock()
    StubProtocol.counts[path, default: 0] += 1
    let response = StubProtocol.failAll ? (500, Data("Error".utf8)) : (StubProtocol.responses[path] ?? (404, Data("Not found".utf8)))
    StubProtocol.lock.unlock()

    client?.urlProtocol(self, didReceive: HTTPURLResponse(url: url, statusCode: response.0, httpVersion: "HTTP/1.1", headerFields: nil)!, cacheStoragePolicy: .notAllowed)
    client?.urlProtocol(self, didLoad: response.1)
    client?.urlProtocolDidFinishLoading(self)
  }

  override func stopLoading() {
  }
}
