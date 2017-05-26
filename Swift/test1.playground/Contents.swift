//: Playground - noun: a place where people can play

import Cocoa

var str = "Hello, playground"

func f(n: Int) -> Int {return n + 1}

f(n: 7)


let a = [1,2,3]
let d = [1:"a", 2:"b"]
let t = (1, "a")

var v: Int = 10 {
willSet {print(newValue)}
didSet {print(oldValue)}
}
v = 40

for i in 1...4 {print(i, terminator: "")}; print()
// for i in 4...1 {print(i, terminator: "")}; print() // error
for i in (1...4).reversed() {print(i, terminator: "")}; print()
for i in stride(from: 4, through: 1, by: -1) {print(i, terminator: "")}; print()
for i in stride(from: 4, to: 0, by: -1) {print(i, terminator: "")}; print()
for i in stride(from: 1, through: 4, by: 2) {print(i, terminator: "")}; print()
for i in stride(from: 1, to: 4, by: 2) {print(i, terminator: "")}; print()
for i in stride(from: 4, through: 1, by: -2) {print(i, terminator: "")}; print()
for i in stride(from: 4, to: 1, by: -2) {print(i, terminator: "")}; print()
for i in 1..<10 {print(i, terminator: "")}; print()

let c = 1

let http200Status: (statusCode: Int, description: String) = (statusCode: 200, description: "OK")

http200Status.statusCode
print("\(type(of: http200Status))")

var http404Error = (404, "Not Found")

var possibleString: String? = "a"
if let definiteString = possibleString {
    print("\(type(of: definiteString))")
    print(definiteString)
}

if let definiteString2 = possibleString?.lowercased() {}

for ch in "Hello".characters {ch}

//let (x, y, z) = (1, 2, 3)
let x2 = 1, y2 = 2, z2 = 3.3

var (a1, b1) = (3, 4)
(a1, b1) = (b1, a1)
print(a1, b1)

if case 1...255 = a1 { print(3)}
if 1...255 ~= a1 {print(3)}
if case 1...255 = a1, case 0...1 = b1 {print(3)}
if 1...255 ~= a1, 0...1 ~= b1 {print(3)}

enum SimpleToken : Equatable {
    case name(String)
    case number(Int)
}
func == (lhs: SimpleToken, rhs: SimpleToken) -> Bool {
    switch (lhs, rhs) {
    case let (.name(a), .name(b)) : return a == b
    case (.name, _) : return false
    case let (.number(a), .number(b)): return a == b
    case (.number, _): return false
    }
}
let t1 = SimpleToken.number(123) // the string representation is "Number(123)"
let t2 = SimpleToken.number(123)
let t3 = SimpleToken.name("bob") // the string representation is "Name(\"bob\")"
print(t1 == t2)
print(t1 == t3)
print(String(describing: t1) == String(describing: t2))
print(String(describing: t1) == String(describing: t3))

if 1 == 1 {
    // ;
    ()
    do{}
    {}()
}

let row = 3, col = 4, rows = 6, cols = 6
if row == 3 && 1..<rows - 1 ~= row && 1..<cols - 1 ~= col {print("ok")}

