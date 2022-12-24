//: [Previous](@previous)

import Foundation

let s = "123-4567-89,987-6543-21"
let r = /\d{3}-(\d{4})-\d{2}/
let results = s.matches(of: r)
for (i, m) in results.enumerated() {
    print("group \(i),0 : \(m.0)")
    print("group \(i),1 : \(m.1)")
}

let r2 = /(\d+)-(\d+)-(\d+)/
// runtime regex
// let r22: Regex<(Substring, Substring, Substring, Substring)> = try! Regex(#"(\d+)-(\d+)-(\d+)"#)
let s2 = s.replacing(r2) { m in
    "\(m.3)-\(m.1)-\(m.2)"
}
print(s2)

let r3 = /\d+/
let s3 = s.replacing(r3) { m in
    m.0.reversed()
}
print(s3)

let r4 = /%(begin|next|end)%/
let s4 = "%begin%hello%next%world%end%"
print(s4.split(separator: r4))

func replacementToFunc(_ replacement: String) -> (Regex<AnyRegexOutput>.Match) -> String {
    { m in
        var s = replacement
        for i in 1..<10 {
            if s.contains("$\(i)") {
                s = s.replacing("$\(i)", with: m[i].substring ?? "")
            }
        }
        return s
    }
}

// runtime regex
let r22 = try! Regex(#"(\d+)-(\d+)-(\d+)"#) // Regex<AnyRegexOutput>
let s22 = s.replacing(r22, with: replacementToFunc("$3-$1-$2"))
print(s22)
