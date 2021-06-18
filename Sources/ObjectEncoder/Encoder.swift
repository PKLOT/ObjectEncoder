//
//  Encoder.swift
//  ObjectEncoder
//
//  Created by Norio Nomura on 10/21/17.
//  Copyright (c) 2017 ObjectEncoder. All rights reserved.
//

import Foundation

public struct ObjectEncoder {
    public init() {}
    public func encode<T>(_ value: T, userInfo: [CodingUserInfoKey: Any] = [:]) throws -> Any where T: Swift.Encodable {
        do {
            let encoder = ObjectEncoder.Encoder(options, userInfo)
            var container = encoder.singleValueContainer()
            try container.encode(value)
            return encoder.object
        } catch let error as EncodingError {
            throw error
        } catch {
            let description = "Unable to encode the given top-level value to Object."
            let context = EncodingError.Context(codingPath: [],
                                                debugDescription: description,
                                                underlyingError: error)
            throw EncodingError.invalidValue(value, context)
        }
    }

    public struct EncodingStrategy<T: Encodable> {
        public typealias Closure = (T, Encoder) throws -> Void
        public init(closure: @escaping Closure) { self.closure = closure }
        fileprivate let closure: Closure
    }

    public struct EncodingStrategies {
        var strategies = [ObjectIdentifier: Any]()
        public subscript<T>(type: T.Type) -> EncodingStrategy<T>? {
            get { return strategies[ObjectIdentifier(type)] as? EncodingStrategy<T> }
            set { strategies[ObjectIdentifier(type)] = newValue }
        }
    }

    /// The strategies to use for encoding values.
    public var encodingStrategies: EncodingStrategies {
        get { return options.encodingStrategies }
        set { options.encodingStrategies = newValue }
    }

    /// The strategies to use for encoding keys.
    public var keyEncodingStrategy: KeyEncodingStrategy {
        get { return options.keyEncodingStrategy }
        set { options.keyEncodingStrategy = newValue }
    }

    // MARK: -

    fileprivate struct Options {
        fileprivate var encodingStrategies = EncodingStrategies()
        fileprivate var keyEncodingStrategy: KeyEncodingStrategy = .useDefaultKeys
    }

    fileprivate var options = Options()
}

extension ObjectEncoder {
    public class Encoder: Swift.Encoder {
        public final var object: Any = [:]

        fileprivate typealias Options = ObjectEncoder.Options
        fileprivate let options: Options

        fileprivate init(_ options: Options, _ userInfo: [CodingUserInfoKey: Any], _ codingPath: [CodingKey] = []) {
            self.options = options
            self.userInfo = userInfo
            self.codingPath = codingPath
        }

        // MARK: - Swift.Encoder properties

        public final let codingPath: [CodingKey]
        public final let userInfo: [CodingUserInfoKey: Any]
    }
}

extension ObjectEncoder.Encoder {
    // MARK: - Swift.Encoder methods

    public final func container<Key>(keyedBy type: Key.Type) -> KeyedEncodingContainer<Key> {
        if canEncodeNewValue {
            object = [:]
        } else {
            precondition(
                object is [String: Any],
                "Attempt to push new keyed encoding container when already previously encoded at this path."
            )
        }
        return .init(_KeyedEncodingContainer<Key>(referencing: self))
    }

    public final func unkeyedContainer() -> UnkeyedEncodingContainer {
        if canEncodeNewValue {
            object = []
        } else {
            precondition(
                object is [Any],
                "Attempt to push new keyed encoding container when already previously encoded at this path."
            )
        }
        return _UnkeyedEncodingContainer(referencing: self)
    }

    public final func singleValueContainer() -> SingleValueEncodingContainer { return self }

    // MARK: -

    fileprivate var dictionary: [String: Any] {
        get { return object as? [String: Any] ?? [:] }
        set { object = newValue }
    }

    fileprivate var array: [Any] {
        get { return object as? [Any] ?? [] }
        set { object = newValue }
    }

    fileprivate func encoder(for key: CodingKey) -> _KeyReferencingEncoder {
        return .init(referencing: self, key: key)
    }

    fileprivate func encoder(at index: Int) -> _IndexReferencingEncoder {
        return .init(referencing: self, at: index)
    }

    private var canEncodeNewValue: Bool {
        if let dictionary = object as? [String: Any], dictionary.isEmpty {
            return true
        }
        return false
    }
}

extension ObjectEncoder {
    /// The strategy to use for automatically changing the value of keys before encoding.
    public enum KeyEncodingStrategy {
        /// Use the keys specified by each type. This is the default strategy.
        case useDefaultKeys

        /// Convert from "camelCaseKeys" to "snake_case_keys" before writing a key to JSON payload.
        ///
        /// Capital characters are determined by testing membership in `CharacterSet.uppercaseLetters` and `CharacterSet.lowercaseLetters` (Unicode General Categories Lu and Lt).
        /// The conversion to lower case uses `Locale.system`, also known as the ICU "root" locale. This means the result is consistent regardless of the current user's locale and language preferences.
        ///
        /// Converting from camel case to snake case:
        /// 1. Splits words at the boundary of lower-case to upper-case
        /// 2. Inserts `_` between words
        /// 3. Lowercases the entire string
        /// 4. Preserves starting and ending `_`.
        ///
        /// For example, `oneTwoThree` becomes `one_two_three`. `_oneTwoThree_` becomes `_one_two_three_`.
        ///
        /// - Note: Using a key encoding strategy has a nominal performance cost, as each string key has to be converted.
        case convertToSnakeCase

        /// Provide a custom conversion to the key in the encoded JSON from the keys specified by the encoded types.
        /// The full path to the current encoding position is provided for context (in case you need to locate this key within the payload). The returned key is used in place of the last component in the coding path before encoding.
        /// If the result of the conversion is a duplicate key, then only one value will be present in the result.
        case custom((_ codingPath: [CodingKey]) -> CodingKey)

        fileprivate static func _convertToSnakeCase(_ stringKey: String) -> String {
            guard !stringKey.isEmpty else { return stringKey }

            var words : [Range<String.Index>] = []
            // The general idea of this algorithm is to split words on transition from lower to upper case, then on transition of >1 upper case characters to lowercase
            //
            // myProperty -> my_property
            // myURLProperty -> my_url_property
            //
            // We assume, per Swift naming conventions, that the first character of the key is lowercase.
            var wordStart = stringKey.startIndex
            var searchRange = stringKey.index(after: wordStart)..<stringKey.endIndex

            // Find next uppercase character
            while let upperCaseRange = stringKey.rangeOfCharacter(from: CharacterSet.uppercaseLetters, options: [], range: searchRange) {
                let untilUpperCase = wordStart..<upperCaseRange.lowerBound
                words.append(untilUpperCase)

                // Find next lowercase character
                searchRange = upperCaseRange.lowerBound..<searchRange.upperBound
                guard let lowerCaseRange = stringKey.rangeOfCharacter(from: CharacterSet.lowercaseLetters, options: [], range: searchRange) else {
                    // There are no more lower case letters. Just end here.
                    wordStart = searchRange.lowerBound
                    break
                }

                // Is the next lowercase letter more than 1 after the uppercase? If so, we encountered a group of uppercase letters that we should treat as its own word
                let nextCharacterAfterCapital = stringKey.index(after: upperCaseRange.lowerBound)
                if lowerCaseRange.lowerBound == nextCharacterAfterCapital {
                    // The next character after capital is a lower case character and therefore not a word boundary.
                    // Continue searching for the next upper case for the boundary.
                    wordStart = upperCaseRange.lowerBound
                } else {
                    // There was a range of >1 capital letters. Turn those into a word, stopping at the capital before the lower case character.
                    let beforeLowerIndex = stringKey.index(before: lowerCaseRange.lowerBound)
                    words.append(upperCaseRange.lowerBound..<beforeLowerIndex)

                    // Next word starts at the capital before the lowercase we just found
                    wordStart = beforeLowerIndex
                }
                searchRange = lowerCaseRange.upperBound..<searchRange.upperBound
            }
            words.append(wordStart..<searchRange.upperBound)
            let result = words.map({ (range) in
                return stringKey[range].lowercased()
            }).joined(separator: "_")
            return result
        }
    }
}

private class _KeyReferencingEncoder: ObjectEncoder.Encoder {
    let encoder: ObjectEncoder.Encoder
    let key: String

    fileprivate init(referencing encoder: ObjectEncoder.Encoder, key: CodingKey) {
        self.encoder = encoder
        self.key = key.stringValue
        super.init(encoder.options, encoder.userInfo, encoder.codingPath + [key])
    }

    deinit {
        encoder.dictionary[key] = object
    }
}

private class _IndexReferencingEncoder: ObjectEncoder.Encoder {
    let encoder: ObjectEncoder.Encoder
    let index: Int

    fileprivate init(referencing encoder: ObjectEncoder.Encoder, at index: Int) {
        self.encoder = encoder
        self.index = index
        super.init(encoder.options, encoder.userInfo, encoder.codingPath + [_ObjectCodingKey(index: index)])
    }

    deinit {
        encoder.array[index] = object
    }
}

private struct _KeyedEncodingContainer<Key: CodingKey>: KeyedEncodingContainerProtocol {
    private let encoder: ObjectEncoder.Encoder

    private func encoder(for key: CodingKey) -> _KeyReferencingEncoder { return encoder.encoder(for: key) }

    init(referencing encoder: ObjectEncoder.Encoder) {
        self.encoder = encoder
    }

    // MARK: - Coding Path Operations

    private func _converted(_ key: CodingKey) -> CodingKey {
        switch encoder.options.keyEncodingStrategy {
        case .useDefaultKeys:
            return key
        case .convertToSnakeCase:
            let newKeyString = ObjectEncoder.KeyEncodingStrategy._convertToSnakeCase(key.stringValue)
            return _ObjectCodingKey(stringValue: newKeyString)!
        case .custom(let converter):
            return converter(codingPath + [key])
        }
    }

    // MARK: - Swift.KeyedEncodingContainerProtocol Methods

    var codingPath: [CodingKey] { return encoder.codingPath }
    func encodeNil(forKey key: Key)             throws { try encoder(for: _converted(key)).encodeNil() }
    func encode<T>(_ value: T, forKey key: Key) throws where T: Primitive { try encoder(for: _converted(key)).encode(value) }
    func encode<T>(_ value: T, forKey key: Key) throws where T: Encodable { try encoder(for: _converted(key)).encode(value) }

    func nestedContainer<NestedKey>(keyedBy type: NestedKey.Type,
                                    forKey key: Key) -> KeyedEncodingContainer<NestedKey> {
        return encoder(for: _converted(key)).container(keyedBy: type)
    }

    func nestedUnkeyedContainer(forKey key: Key) -> UnkeyedEncodingContainer {
        return encoder(for: _converted(key)).unkeyedContainer()
    }

    func superEncoder() -> Encoder { return encoder(for: _converted(_ObjectCodingKey.super)) }
    func superEncoder(forKey key: Key) -> Encoder { return encoder(for: _converted(key)) }
}

private struct _UnkeyedEncodingContainer: UnkeyedEncodingContainer {
    private let encoder: ObjectEncoder.Encoder

    private var currentEncoder: _IndexReferencingEncoder {
        defer { encoder.array.append("") }
        return encoder.encoder(at: count)
    }

    init(referencing encoder: ObjectEncoder.Encoder) {
        self.encoder = encoder
    }

    // MARK: - Swift.UnkeyedEncodingContainer Methods

    var codingPath: [CodingKey] { return encoder.codingPath }
    var count: Int { return encoder.array.count }
    func encodeNil()           throws { try currentEncoder.encodeNil() }
    func encode<T>(_ value: T) throws where T: Primitive { try currentEncoder.encode(value) }
    func encode<T>(_ value: T) throws where T: Encodable { try currentEncoder.encode(value) }

    func nestedContainer<NestedKey>(keyedBy keyType: NestedKey.Type) -> KeyedEncodingContainer<NestedKey> {
        return currentEncoder.container(keyedBy: keyType)
    }

    func nestedUnkeyedContainer() -> UnkeyedEncodingContainer { return currentEncoder.unkeyedContainer() }
    func superEncoder() -> Encoder { return currentEncoder }
}

extension ObjectEncoder.Encoder: SingleValueEncodingContainer {

    // MARK: - Swift.SingleValueEncodingContainer Methods

    public final func encodeNil()           throws { assertCanEncodeNewValue(); object = NSNull() }
    public final func encode<T>(_ value: T) throws where T: Primitive { try box(value) }
    public final func encode<T>(_ value: T) throws where T: Encodable {
        assertCanEncodeNewValue()
        if try !applyStrategy(value) {
            try value.encode(to: self)
        }
    }

    // MARK: -

    private func applyStrategy<T: Encodable>(_ value: T) throws -> Bool {
        if let strategy = options.encodingStrategies[T.self] {
            try strategy.closure(value, self)
            return true
        }
        return false
    }

    private func box<T: Encodable>(_ value: T) throws {
        assertCanEncodeNewValue()
        if try !applyStrategy(value) {
            object = value
        }
    }

    /// Asserts that a single value can be encoded at the current coding path
    /// (i.e. that one has not already been encoded through this container).
    /// `preconditionFailure()`s if one cannot be encoded.
    private func assertCanEncodeNewValue() {
        precondition(
            canEncodeNewValue,
            "Attempt to encode value through single value container when previously value already encoded."
        )
    }
}

public protocol Primitive: Encodable {}
extension Bool: Primitive {}
extension Int: Primitive {}
extension Int8: Primitive {}
extension Int16: Primitive {}
extension Int32: Primitive {}
extension Int64: Primitive {}
extension UInt: Primitive {}
extension UInt8: Primitive {}
extension UInt16: Primitive {}
extension UInt32: Primitive {}
extension UInt64: Primitive {}
extension Float: Primitive {}
extension Double: Primitive {}
extension String: Primitive {}

// MARK: - CodingKey for `_UnkeyedEncodingContainer`, `_UnkeyedDecodingContainer`, `superEncoders` or `superDecoders`

struct _ObjectCodingKey: CodingKey { // swiftlint:disable:this type_name
    var stringValue: String
    var intValue: Int?

    init?(stringValue: String) {
        self.stringValue = stringValue
        self.intValue = nil
    }

    init?(intValue: Int) {
        self.stringValue = "\(intValue)"
        self.intValue = intValue
    }

    init(index: Int) {
        self.stringValue = "Index \(index)"
        self.intValue = index
    }

    static let `super` = _ObjectCodingKey(stringValue: "super")!
}

// MARK: - EncodingError helpers

private func _invalidFloatingPointValue<T: FloatingPoint>(_ value: T, at codingPath: [CodingKey]) -> EncodingError {
    let valueDescription: String
    if value == T.infinity {
        valueDescription = "\(T.self).infinity"
    } else if value == -T.infinity {
        valueDescription = "-\(T.self).infinity"
    } else {
        valueDescription = "\(T.self).nan"
    }

    let debugDescription = """
    Unable to encode \(valueDescription) directly in JSONObjectEncoder. \
    Use JSONObjectEncoder.NonConformingFloatEncodingStrategy.convertToString to specify how the value should be encoded.
    """
    return .invalidValue(value, .init(codingPath: codingPath, debugDescription: debugDescription))
}

// MARK: - ObjectEncoder.EncodingStrategy

extension ObjectEncoder {
    /// The strategy to use for encoding `Data` values.
    public typealias DataEncodingStrategy = EncodingStrategy<Data>
    /// The strategy to use for encoding `Date` values.
    public typealias DateEncodingStrategy = EncodingStrategy<Date>

    /// The strategy to use for encoding `Double` values.
    public typealias DoubleEncodingStrategy = EncodingStrategy<Double>
    /// The strategy to use for encoding `Float` values.
    public typealias FloatEncodingStrategy = EncodingStrategy<Float>
}

extension ObjectEncoder.EncodingStrategy {
    /// Encode the `T` as a custom value encoded by the given closure.
    ///
    /// If the closure fails to encode a value into the given encoder,
    /// the encoder will encode an empty automatic container in its place.
    public static func custom(_ closure: @escaping Closure) -> ObjectEncoder.EncodingStrategy<T> {
        return .init(closure: closure)
    }
}

extension ObjectEncoder.EncodingStrategy where T == Data {
    /// Defer to `Data` for choosing an encoding.
    public static let deferredToData: ObjectEncoder.DataEncodingStrategy? = nil

    /// Encoded the `Data` as a Base64-encoded string. This is the default strategy.
    public static let base64 = ObjectEncoder.DataEncodingStrategy.custom {
        try $0.base64EncodedString().encode(to: $1)
    }
}

@available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
var iso8601Formatter: ISO8601DateFormatter = {
    let formatter = ISO8601DateFormatter()
    formatter.formatOptions = .withInternetDateTime
    return formatter
}()

extension ObjectEncoder.EncodingStrategy where T == Date {
    /// Defer to `Date` for choosing an encoding. This is the default strategy.
    public static let deferredToDate: ObjectEncoder.DateEncodingStrategy? = nil

    /// Encode the `Date` as a UNIX timestamp (as a `Double`).
    public static let secondsSince1970 = ObjectEncoder.DateEncodingStrategy.custom { date, encoder in
        var container = encoder.singleValueContainer()
        try container.encode(date.timeIntervalSince1970)
    }

    /// Encode the `Date` as UNIX millisecond timestamp (as a `Double`).
    public static let millisecondsSince1970 = ObjectEncoder.DateEncodingStrategy.custom { date, encoder in
        var container = encoder.singleValueContainer()
        try container.encode(1000.0 * date.timeIntervalSince1970)
    }

    /// Encode the `Date` as an ISO-8601-formatted string (in RFC 3339 format).
    @available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
    public static let iso8601 = ObjectEncoder.DateEncodingStrategy.custom { date, encoder in
        var container = encoder.singleValueContainer()
        try container.encode(iso8601Formatter.string(from: date))
    }

    /// Encode the `Date` as a string formatted by the given formatter.
    public static func formatted(_ formatter: DateFormatter) -> ObjectEncoder.DateEncodingStrategy {
        return .custom { date, encoder in
            var container = encoder.singleValueContainer()
            try container.encode(formatter.string(from: date))
        }
    }
}

extension ObjectEncoder.EncodingStrategy where T == Decimal {
    public static let compatibleWithJSONEncoder = ObjectEncoder.EncodingStrategy<Decimal>.custom { decimal, encoder in
        encoder.object = NSDecimalNumber(decimal: decimal)
    }
}

extension ObjectEncoder.EncodingStrategy where T == Double {
    public static let throwOnNonConformingFloat = ObjectEncoder.DoubleEncodingStrategy.custom { double, encoder in
        guard !double.isInfinite && !double.isNaN else {
            throw _invalidFloatingPointValue(double, at: encoder.codingPath)
        }
        encoder.object = NSNumber(value: double)
    }

    public static func convertNonConformingFloatToString(_ positiveInfinity: String,
                                                         _ negativeInfinity: String,
                                                         _ nan: String) -> ObjectEncoder.DoubleEncodingStrategy {
        return .custom { double, encoder in
            if double == .infinity {
                encoder.object = positiveInfinity
            } else if double == -.infinity {
                encoder.object = negativeInfinity
            } else if double.isNaN {
                encoder.object = nan
            } else {
                encoder.object = NSNumber(value: double)
            }
        }
    }
}

extension ObjectEncoder.EncodingStrategy where T == Float {
    public static let throwOnNonConformingFloat = ObjectEncoder.FloatEncodingStrategy.custom { float, encoder in
        guard !float.isInfinite && !float.isNaN else {
            throw _invalidFloatingPointValue(float, at: encoder.codingPath)
        }
        encoder.object = NSNumber(value: float)
    }

    public static func convertNonConformingFloatToString(_ positiveInfinity: String,
                                                         _ negativeInfinity: String,
                                                         _ nan: String) -> ObjectEncoder.FloatEncodingStrategy {
        return .custom { float, encoder in
            if float == .infinity {
                encoder.object = positiveInfinity
            } else if float == -.infinity {
                encoder.object = negativeInfinity
            } else if float.isNaN {
                encoder.object = nan
            } else {
                encoder.object = NSNumber(value: float)
            }
        }
    }
}

extension ObjectEncoder.EncodingStrategy where T == URL {
    public static let compatibleWithJSONEncoder = ObjectEncoder.EncodingStrategy<URL>.custom { url, encoder in
        var container = encoder.singleValueContainer()
        try container.encode(url.absoluteString)
    }
}

// swiftlint:disable:this file_length
