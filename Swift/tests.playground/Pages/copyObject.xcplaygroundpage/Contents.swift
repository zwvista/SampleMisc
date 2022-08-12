import Foundation

// https://stackoverflow.com/questions/780897/how-do-i-find-all-the-property-keys-of-a-kvc-compliant-objective-c-object
extension Encodable {
     public func toDictionary() -> [String: AnyObject]? {
        let encoder = JSONEncoder()
        encoder.outputFormatting = .prettyPrinted
        guard let data =  try? encoder.encode(self),
              let json = try? JSONSerialization.jsonObject(with: data, options: .init(rawValue: 0)), let jsonDict = json as? [String: AnyObject] else {
            return nil
        }
        return jsonDict
    }
}

class ExampleObj: NSObject, Encodable {
    @objc var prop1: String = ""
    @objc var prop2: String = ""
}

let a = ExampleObj()
a.prop1 = "a"
a.prop2 = "b"
let b = ExampleObj()
let keys = a.toDictionary()!.keys
for k in keys {
    var v = String(describing: a.value(forKey: k) ?? "")
    b.setValue(v, forKey: k)
    print("\(k) = \(v)")
    v = String(describing: b.value(forKey: k) ?? "")
    print("\(k) = \(v)")
}
