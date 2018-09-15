package fp;

import static fj.Ord.intOrd;
import static fj.data.List.fromString;
public class Jys2 {
	public static void main(String[] args) {		
		String ss = "床前明月光疑是地上霜举头望明月低头思故乡";
		fromString(ss).zipIndex()
		.groupBy(kv -> kv._2() % 5, intOrd)
		.values().toStream().foreachDoEffect(vvs -> System.out.println(
		vvs.toStream().map(vv -> vv._1().toString())
		.foldLeft1((acc, s) -> s + "|" + acc)));
	}
}
