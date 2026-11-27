import Foundation

/// The arguments of an Apple format string as position:conversion, such as 1:@ and 2:d, and
/// 1:#@files@ for the plural variable of a .stringsdict
enum Placeholders {
  // %@, %d, %1$@, %lld, %.2f, %-10s, %#@name@. %% is not an argument.
  private static let pattern = try! NSRegularExpression(
    pattern: "%(?:(\\d+)\\$)?(?:#@([A-Za-z0-9_]+)@|[-#+ 0']*\\d*(?:\\.\\d+)?(?:hh|h|ll|l|q|L|z|t|j)?([@dDiuUxXoOfFeEgGaAcCsSp%]))")

  static func of(_ value: String) -> Set<String> {
    var result = Set<String>()
    var position = 0
    let text = value as NSString

    for match in pattern.matches(in: value, range: NSRange(location: 0, length: text.length)) {
      let conversion = group(match, 3, text)

      if conversion == "%" {
        continue
      }

      let index: Int

      if let number = group(match, 1, text).flatMap({ Int($0) }) {
        index = number
      } else {
        position += 1
        index = position
      }

      if let variable = group(match, 2, text) {
        result.insert("\(index):#@\(variable)@")
      } else if let conversion = conversion {
        result.insert("\(index):\(normalize(conversion))")
      }
    }

    return result
  }

  // The integer conversions are the same argument: %d, %i, %u, %D
  private static func normalize(_ conversion: String) -> String {
    switch conversion {
    case "d", "D", "i", "u", "U":
      return "d"
    default:
      return conversion.lowercased()
    }
  }

  private static func group(_ match: NSTextCheckingResult, _ index: Int, _ text: NSString) -> String? {
    let range = match.range(at: index)
    return (range.location == NSNotFound) ? nil : text.substring(with: range)
  }
}
