import 'package:collection/collection.dart';

void jys() {
  final text = "床前明月光疑是地上霜举头望明月低头思故乡";
  final offset = 5;
  groupBy(text.split("").asMap().entries, (MapEntry<int, String> kv) => kv.key % offset).forEach((k, lst) {
    final s = lst.map((kv2) => kv2.value).toList().reversed.join("|");
    print(s);
  });
}

/*
低|举|疑|床
头|头|是|前
思|望|地|明
故|明|上|月
乡|月|霜|光
*/
