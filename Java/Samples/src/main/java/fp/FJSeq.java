package fp;

import fj.data.Seq;

import static fj.data.Seq.seq;

public class FJSeq {

	static void f(Object o) { System.out.println(o); }
	public static void main(String[] args) {
		f(seq(1, 2).append(seq(3))); // Seq(1,2,3)
		f(seq(1, 2).cons(3)); // Seq(3,1,2)
		f(seq(1, 2, 3, 4, 5).delete(2)); // Seq(1,2,4,5)
		f(seq(1, 2, 3, 4, 5).drop(2)); // Seq(3,4,5)
		f(Seq.empty()); // Seq()
		f(seq(1, 2, 3, 4, 5).filter(i -> i % 2 == 1)); // Seq(1,3,5)
		f(seq(1, 2, 3, 4, 5).foldLeft((acc, i) -> acc + i - 1, 0)); // 10
		f(seq(1, 2, 3, 4, 5).foldRight((acc, i) -> acc + i - 1, 0)); // 10
		f(seq(1, 2, 3, 4, 5).head()); // 1
		f(seq(1, 2, 3, 4, 5).headOption()); // Some(1)
		f(seq(1, 2, 3, 4, 5).index(2)); // 3
		f(seq(1, 2, 3, 4, 5).init()); // Seq(1,2,3,4)
		f(seq(1, 2, 3, 4, 5).insert(3, 6)); // Seq(1,2,3,6,4,5)
		f(seq(1, 2, 3).isEmpty()); // false
		f(seq(1, 2, 3).isNotEmpty()); // true
		f(seq(1, 2, 3, 4, 5).last()); // 5
		f(seq(1, 2, 3, 4, 5).length()); // 5
		f(seq(1, 2, 3).map(i -> i * 2)); // Seq(2,4,6)
		f(Seq.single(3)); // Seq(3)
		f(seq(1, 2, 3).snoc(4)); // Seq(1,2,3,4)
		f(seq(1, 2, 3).split(1)); // (Seq(1),Seq(2,3))
		f(seq(1, 2, 3, 4, 5).tail()); // Seq(2,3,4,5)
		f(seq(1, 2, 3, 4, 5).take(3)); // Seq(1,2,3)
		f(seq(1, 2, 3, 4, 5).toList()); // List(1,2,3,4,5)
		f(seq(1, 2, 3, 4, 5).toJavaList()); // [1, 2, 3, 4, 5]
		f(seq(1, 2, 3, 4, 5).update(2, 6)); // Seq(1,2,6,4,5)
	}

}
