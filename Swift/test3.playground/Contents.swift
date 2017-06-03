// https://stackoverflow.com/questions/31220002/how-to-group-by-the-elements-of-an-array-in-swift
extension Sequence {
    func group<GroupingType: Hashable>(by key: (Iterator.Element) -> GroupingType) -> [[Iterator.Element]] {
        var groups: [GroupingType: [Iterator.Element]] = [:]
        var groupsOrder: [GroupingType] = []
        forEach { element in
            let key = key(element)
            if case nil = groups[key]?.append(element) {
                groups[key] = [element]
                groupsOrder.append(key)
            }
        }
        return groupsOrder.map { groups[$0]! }
    }
}

func verticalWriting(txt:String, offset:Int) {
    txt.characters.enumerated()
        .group(by: {(i, c) in i % offset})
        .forEach{print(
            $0.map({String($0.element)}).reversed().joined(separator: "|")
        )}
}

verticalWriting(txt: "床前明月光疑是地上霜举头望明月低头思故乡", offset: 5)

/*
低|举|疑|床
头|头|是|前
思|望|地|明
故|明|上|月
乡|月|霜|光
*/