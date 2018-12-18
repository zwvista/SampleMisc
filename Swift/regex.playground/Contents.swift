import Foundation

let s = "123-4567-89"
let r = try NSRegularExpression(pattern: "\\d{3}-(\\d{4})-\\d{2}")
let results = r.matches(in: s, options: [], range: NSRange(s.startIndex..., in: s))
results.flatMap{m in (0..<m.numberOfRanges).map{String(s[Range(m.range(at: $0), in: s)!])}}.enumerated().forEach{print("group \($0) : \($1)")}


let r2 = try NSRegularExpression(pattern: "(\\d+)-(\\d+)-(\\d+)")
let s2 = r2.stringByReplacingMatches(in: s, options: [], range: NSRange(s.startIndex..., in: s), withTemplate: "$3-$1-$2")
print(s2)

let r3 = try NSRegularExpression(pattern: "\\d+")
let results2 = r3.matches(in: s, options: [], range: NSRange(s.startIndex..., in: s))
var s3 = s
for i in (0..<results2.count).reversed() {
    let rng = Range(results2[i].range, in: s3)!
    s3.replaceSubrange(rng, with: s3[rng].reversed())
}
print(s3)

// https://stackoverflow.com/questions/25818197/how-to-split-a-string-in-swift
extension String {
    
    func split(regex pattern: String) -> [String] {
        
        guard let re = try? NSRegularExpression(pattern: pattern, options: [])
            else { return [] }
        
        let nsString = self as NSString // needed for range compatibility
        let stop = "<SomeStringThatYouDoNotExpectToOccurInSelf>"
        let modifiedString = re.stringByReplacingMatches(
            in: self,
            options: [],
            range: NSRange(location: 0, length: nsString.length),
            withTemplate: stop)
        return modifiedString.components(separatedBy: stop)
    }
}

let r4 = "%(begin|next|end)%"
let s4 = "%begin%hello%next%world%end%"
print(s4.split(regex: r4))

/*
group 0 : 123-4567-89
group 1 : 4567
89-123-4567
321-7654-98
["", "hello", "world", ""]
*/
