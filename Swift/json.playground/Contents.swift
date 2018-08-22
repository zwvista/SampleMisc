import Cocoa

struct Persons : Codable {
  let persons: [Person]
}

struct Person : Codable {
  let name: String
  let age: Int
}

let jsonString = """
{
  "persons":
  [
    {
      "name": "Joe",
      "age":  12,
    },
  ]
}
"""
let jsonData = jsonString.data(using: .utf8)!
let decoder = JSONDecoder()
let o = try! decoder.decode(Persons.self, from: jsonData)
print(o) // Persons(persons: [__lldb_expr_65.Person(name: "Joe", age: 12)])

let encoder = JSONEncoder()
let data = try! encoder.encode(o)
print(String(data: data, encoding: .utf8)!) // {"persons":[{"name":"Joe","age":12}]}

