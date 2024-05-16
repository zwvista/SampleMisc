//
//  main.swift
//  BB
//
//  Created by 趙偉 on 2018/12/18.
//  Copyright © 2018 趙偉. All rights reserved.
//

import Foundation

let s = "123-4567-89,987-6543-21"
let r = #"\d{3}-(\d{4})-\d{2}"#.r!
r.findAll(in: s).enumerated().forEach { (i, m) in
    for j in 0..<m.ranges.count {
        print("group \(i),\(j) : \(m.group(at: j)!)")
    }
}

let r2 = #"(\d+)-(\d+)-(\d+)"#.r!
let s2 = r2.replaceAll(in: s, with: "$3-$1-$2")
print(s2)

let r3 = #"\d+"#.r!
let s3 = r3.replaceAll(in: s) { String($0.matched.reversed()) }
print(s3)

let r4 = "%(?:begin|next|end)%".r!
let s4 = "%begin%hello%next%world%end%"
print(r4.split(s4))

/*
group 0,0 : 123-4567-89
group 0,1 : 4567
group 1,0 : 987-6543-21
group 1,1 : 6543
89-123-4567,21-987-6543
321-7654-98,789-3456-12
["", "hello", "world", ""]
*/
