void regTest() {
  final s = "123-4567-89,987-6543-21";
  final r = RegExp(r"\d{3}-(\d{4})-\d{2}");
  final m = r.firstMatch(s);
  if (m != null) print("Found matches:");
  final ms = r.allMatches(s);
  ms.toList().asMap().forEach((i, m) {
    for (var j = 0; j <= m.groupCount; j++)
      print("group $i,$j : ${m.group(j)}");
  });

  print(s.replaceAllMapped(RegExp(r"(\d+)-(\d+)-(\d+)"),
          (m) => r"$3-$1-$2".replaceAllMapped(RegExp(r"\$(\d)"),
              (m2) => m.group(int.parse(m2.group(1)!))!)));

  // https://stackoverflow.com/questions/21521729/how-do-i-reverse-a-string-in-dart
  print(s.replaceAllMapped(RegExp(r"\d+"), (m) => m.group(0)!.split("").reversed.join()));

  print("%begin%hello%next%world%end%".split(RegExp("%(begin|next|end)%")));
}

/*
Found matches:
group 0,0 : 123-4567-89
group 0,1 : 4567
group 1,0 : 987-6543-21
group 1,1 : 6543
89-123-4567,21-987-6543
321-7654-98,789-3456-12
[, hello, world, ]
*/
