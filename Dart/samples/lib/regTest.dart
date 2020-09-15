void regTest() {
  final s = "123-4567-89,987-6543-21";
  final r = RegExp(r"\d{3}-(\d{4})-\d{2}");
  final ms = r.allMatches(s);
  ms.toList().asMap().forEach((i, m) {
    for (var i = 0; i <= m.groupCount; i++)
      print("group $i : ${m.group(i)}");
  });

  print(s.replaceAllMapped(RegExp(r"(\d+)-(\d+)-(\d+)"), (m) => "${m.group(3)}-${m.group(1)}-${m.group(2)}"));

  // https://stackoverflow.com/questions/21521729/how-do-i-reverse-a-string-in-dart
  print(s.replaceAllMapped(RegExp(r"\d+"), (m) => m.group(0).split("").reversed.join()));

  print("%begin%hello%next%world%end%".split(RegExp("%(begin|next|end)%")));
}

/*
group 0 : 123-4567-89
group 1 : 4567
group 0 : 987-6543-21
group 1 : 6543
89-123-4567,21-987-6543
321-7654-98,789-3456-12
[, hello, world, ]
*/
