package fp;

import javafx.util.Pair;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Jys {
    public static void main(String[] args) {
        String ss = "床前明月光疑是地上霜举头望明月低头思故乡";
        IntStream.range(0, ss.length())
        .mapToObj(i -> new Pair<>(i, ss.substring(i, i + 1)))
        .collect(Collectors.groupingBy((Pair<Integer, String> vv) -> vv.getKey() % 5))
        .forEach((k, vvs) -> System.out.println(
        vvs.stream().map(Pair::getValue)
        .reduce((acc, s) -> s + "|" + acc).get()));
    }
}

/*
低|举|疑|床
头|头|是|前
思|望|地|明
故|明|上|月
乡|月|霜|光
*/