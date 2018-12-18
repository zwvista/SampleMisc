//
//  main.swift
//  BB
//
//  Created by 趙偉 on 2018/12/18.
//  Copyright © 2018 趙偉. All rights reserved.
//

import Foundation

let s = "123-4567-89"
let r = "\\d{3}-(\\d{4})-\\d{2}".r!
r.findAll(in: s).forEach { m in
    for i in 0..<m.ranges.count {
        print("group \(i) : \(m.group(at: i)!)")
    }
}

let r2 = "(\\d+)-(\\d+)-(\\d+)".r!
let s2 = r2.replaceAll(in: s, with: "$3-$1-$2")
print(s2)

let r3 = "\\d+".r!
let s3 = r3.replaceAll(in: s, using: { String($0.matched.reversed()) })
print(s3)

let r4 = "%(?:begin|next|end)%".r!
let s4 = "%begin%hello%next%world%end%"
print(r4.split(s4))

/*
group 0 : 123-4567-89
group 1 : 4567
89-123-4567
321-7654-98
["", "hello", "world", ""]
*/
