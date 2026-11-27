import Foundation

/// Converts a String Catalog (.xcstrings) into the strings and the .stringsdict plurals of one
/// language, as Xcode does when it builds the application: a device reads only those.
///
/// - stringUnit: a plain string.
/// - variations.plural: a plural, as a .stringsdict entry with the variable "value".
/// - stringUnit and substitutions: a string with plural arguments ("Found %1$#@count@"); each
///   substitution is a variable of the .stringsdict entry, and %arg in its forms is its argument.
/// - variations.device: left out, so the built-in string is used.
enum StringCatalog {
  struct Language {
    var strings: [String: String] = [:]
    var plurals: [String: Any] = [:]
  }

  enum CatalogError: Error {
    case invalid
  }

  /// The languages that the catalog has translations for
  static func languages(_ data: Data) throws -> [String] {
    var result = Set<String>()

    for entry in try strings(data).values {
      if let localizations = (entry as? [String: Any])?["localizations"] as? [String: Any] {
        result.formUnion(localizations.keys)
      }
    }

    return result.sorted()
  }

  static func convert(_ data: Data, language: String) throws -> Language {
    var result = Language()

    for (key, entry) in try strings(data) {
      guard let entry = entry as? [String: Any],
            (entry["shouldTranslate"] as? Bool) != false,
            let localizations = entry["localizations"] as? [String: Any],
            let localization = find(localizations, language) as? [String: Any] else {
        continue
      }

      if let substitutions = localization["substitutions"] as? [String: Any], let format = unit(localization) {
        var plural: [String: Any] = ["NSStringLocalizedFormatKey": format]
        var valid = true

        for (name, substitution) in substitutions {
          guard let substitution = substitution as? [String: Any], let forms = pluralForms(substitution["variations"]) else {
            valid = false
            break
          }

          let specifier = (substitution["formatSpecifier"] as? String) ?? "d"
          plural[name] = rule(forms.mapValues { $0.replacingOccurrences(of: "%arg", with: "%" + specifier) }, specifier)
        }

        if valid {
          result.plurals[key] = plural
        }
      } else if let forms = pluralForms(localization["variations"]) {
        result.plurals[key] = [
          "NSStringLocalizedFormatKey": "%#@value@",
          "value": rule(forms, valueType(forms["other"] ?? ""))
        ]
      } else if let value = unit(localization) {
        result.strings[key] = value
      }
    }

    return result
  }

  private static func strings(_ data: Data) throws -> [String: Any] {
    guard let root = try JSONSerialization.jsonObject(with: data) as? [String: Any],
          let strings = root["strings"] as? [String: Any] else {
      throw CatalogError.invalid
    }

    return strings
  }

  // The localization of the language, ignoring the case: zh-Hans and zh-hans
  private static func find(_ localizations: [String: Any], _ language: String) -> Any? {
    if let value = localizations[language] {
      return value
    }

    return localizations.first { $0.key.caseInsensitiveCompare(language) == .orderedSame }?.value
  }

  private static func unit(_ value: [String: Any]) -> String? {
    guard let text = (value["stringUnit"] as? [String: Any])?["value"] as? String, !text.isEmpty else {
      return nil
    }

    return text
  }

  // The plural forms, or nil if there are none or the other form is missing
  private static func pluralForms(_ variations: Any?) -> [String: String]? {
    guard let plural = (variations as? [String: Any])?["plural"] as? [String: Any] else {
      return nil
    }

    let forms = plural.compactMapValues { unit(($0 as? [String: Any]) ?? [:]) }
    return (forms["other"] != nil) ? forms : nil
  }

  private static func rule(_ forms: [String: String], _ valueType: String) -> [String: Any] {
    var result: [String: Any] = [
      "NSStringFormatSpecTypeKey": "NSStringPluralRuleType",
      "NSStringFormatValueTypeKey": valueType
    ]

    for (form, text) in forms {
      result[form] = text
    }

    return result
  }

  private static let specifier = try! NSRegularExpression(pattern: "%(?:\\d+\\$)?[-#+ 0']*\\d*(?:\\.\\d+)?((?:hh|h|ll|l|q|L|z|t|j)?[dDiuUxXoOfFeEgGaAcC])")

  // The format of the count in a plural form, such as lld in "%lld items"
  private static func valueType(_ text: String) -> String {
    let value = text as NSString

    guard let match = specifier.firstMatch(in: text, range: NSRange(location: 0, length: value.length)) else {
      return "d"
    }

    return value.substring(with: match.range(at: 1))
  }
}
