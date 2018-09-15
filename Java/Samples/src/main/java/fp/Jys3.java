package fp;

import java.util.stream.Collectors;

import static org.jooq.lambda.Seq.seq;

public class Jys3 {
	public static void main(String[] args) {
		String ss = "床前明月光疑是地上霜举头望明月低头思故乡";
		seq(ss.chars()).zipWithIndex()
		.groupBy(kv -> kv.v2 % 5)
		.values().forEach(vvs -> System.out.println(
		seq(vvs).map(vv -> String.valueOf((char)vv.v1.intValue())).reverse()
		.collect(Collectors.joining(("|")))));
	}
}

/*
低|举|疑|床
头|头|是|前
思|望|地|明
故|明|上|月
乡|月|霜|光
*/
