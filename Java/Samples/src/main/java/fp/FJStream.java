package fp;

import fj.P;
import fj.P2;
import fj.data.Option;
import fj.data.Stream;
import fj.function.Effect1;

import static fj.Equal.intEqual;
import static fj.Ord.intOrd;
import static fj.Unit.unit;
import static fj.data.Stream.stream;

public class FJStream {

	static void f(Object o) { System.out.println(o); }
	public static void main(String[] args) {
		Effect1<Object> f = o -> System.out.println(o);
		f(stream(1, 2).append(stream(3)).toList()); // List(1,2,3)
		f(stream(1, 2, 3).apply(stream(i -> i + 3, i -> i * 2)).toList()); // List(4,5,6,2,4,6)
		f(Stream.asString(stream('a', 'b', 'c'))); // abc
		f(stream(1, 2, 3).bind(i -> stream(i, i * 2)).toList()); // List(1,2,2,4,3,6)
		f(stream(1, 2, 3).bind(stream(4, 5, 6), x -> y -> x * y).toList()); // List(4,5,6,8,10,12,12,15,18)
		f(stream(1, 2, 3).bind(stream(4, 5, 6), (x, y) -> x * y).toList()); // List(4,5,6,8,10,12,12,15,18)
		f(stream(1, 2, 3).cobind(s -> s.toList()).toList()); // List(List(),List(1),List(1,2),List(1,2,3),List(),List(2),List(2,3),List(),List(3))
		f(stream(1, 2).cons(3).toList()); // List(3,1,2)
		f(Stream.cycle(stream(1, 2, 3)).take(9).toList()); // List(1,2,3,1,2,3,1,2,3)
		f(stream(1, 2, 3, 4, 5).drop(2).toList()); // List(3,4,5)
		f(stream(1, 2, 3, 4, 5).dropWhile(i -> i < 3).toList()); // List(3,4,5)
		f(stream(1, 2, 3, 4, 5).exists(i -> i % 3 == 1)); // true
		f(stream(1, 2, 3, 4, 5).filter(i -> i % 2 == 1).toList()); // List(1,3,5)
		f(stream(1, 2, 3, 4, 5).find(i -> i / 3 == 1)); // Some(3)
		f(stream(1, 2, 3, 4, 5).foldLeft(acc -> i -> acc + i - 1, 0)); // 10
		f(stream(1, 2, 3, 4, 5).foldLeft((acc, i) -> acc + i - 1, 0)); // 10
		f(stream(1, 2, 3, 4, 5).foldLeft1(acc -> i -> acc + i - 1)); // 11
		f(stream(1, 2, 3, 4, 5).foldLeft1((acc, i) -> acc + i - 1)); // 11
		f(stream(1, 2, 3, 4, 5).foldRight(acc -> i -> acc + i._1() - 1, 0)); // 10
		f(stream(1, 2, 3, 4, 5).foldRight((acc, i) -> acc + i._1() - 1, 0)); // 10
		f(stream(1, 2, 3, 4, 5).foldRight1(acc -> i -> acc + i - 1, 0)); // 10
		f(stream(1, 2, 3, 4, 5).foldRight1((acc, i) -> acc + i - 1, 0)); // 10
		f(stream(1, 2, 3, 4, 5).forall(i -> i % 2 == 0)); // false
		stream(1, 2, 3, 4, 5).foreach(i -> {System.out.print(i); return unit();});System.out.println(); // 12345
		stream(1, 2, 3, 4, 5).foreachDoEffect(i -> System.out.print(i));System.out.println(); // 12345
		f(Stream.fromString("abcde").toList()); // List(a,b,c,d,e)
		f(stream(1, 2, 3, 4, 5).head()); // 1
		f(stream(1, 2, 3, 4, 5).index(2)); // 3
		f(stream(1, 2, 3, 4, 5).inits().map(s -> s.toList()).toList()); // List(List(),List(1),List(1,2),List(1,2,3),List(1,2,3,4),List(1,2,3,4,5))
		f(stream(1, 2, 3).intersperse(9).toList()); // List(1,9,2,9,3)
		f(stream(1, 2, 3).isEmpty()); // false
		f(stream(1, 2, 3).isNotEmpty()); // true
		f(Stream.iterateWhile(i -> i + 1, i -> i < 10, 1).toList()); // List(1,2,3,4,5,6,7,8,9)
		f(Stream.join(stream(stream(1, 2), stream(3, 4))).toList()); // List(1,2,3,4)
		f(stream(1, 2, 3).last()); // 3
		f(stream(1, 2, 3).length()); // 3
		f(stream(1, 2, 3).map(i -> i * 2).toList()); // List(2,4,6)
		f(stream(1, 2, 3).<Integer, Integer>mapM(a -> b -> a * b).f(3).toList()); // List(3,6,9)
		f(stream(1, 2, 3).minus(intEqual, stream(3, 4, 5)).toList()); // List(1,2)
		f(Stream.nil().toList()); // List()
		f(stream().orHead(() -> -1)); // -1
		f(stream().orTail(() -> stream(1, 2))._1().toList()); // List(1,2)
		f(Stream.range(1, 10).toList()); // List(1,2,3,4,5,6,7,8,9)
		f(stream(1, 2, 3, 4, 5).removeAll(i -> i % 2 == 0).toList()); // List(1,3,5)
		f(stream(1, 2, 3, 4, 5).reverse().toList()); // List(5,4,3,2,1)
		f(stream(1, 2, 3).sequence(stream('a', 'b')).toList()); // List(a,b,a,b,a,b)
		f(Stream.single(3).toList()); // List(3)
		f(stream(1, 2, 3).snoc(4).toList()); // List(1,2,3,4)
		f(stream(3, 2, 1).sort(intOrd).toList()); // List(1,2,3)
		f(P2.map(kv -> kv.toList(), stream(1, 2, 3, 4, 5).span(i -> i < 3))); // (List(1,2),List(3,4,5))
		f(stream(1, 2, 3, 4, 5).tail()._1().toList()); // List(2,3,4,5)
		f(stream(1, 2, 3, 4, 5).tails().map(s -> s.toList()).toList()); // List(List(1,2,3,4,5),List(2,3,4,5),List(3,4,5),List(4,5),List(5),List())
		f(stream(1, 2, 3, 4, 5).take(3).toList()); // List(1,2,3)
		f(stream(1, 2, 3, 4, 5).takeWhile(i -> i < 3).toList()); // List(1,2)
		f(stream(1, 2, 3, 4, 5).toArray()); // Array(1,2,3,4,5)
		// traverse (\i -> [i, i + 1]) [1, 2, 3]
		f(stream(1, 2, 3).traverseOption(i -> i % 2 == 0 ? Option.some(i / 2) : Option.none())); // None
		f(stream(2, 4, 6).traverseOption(i -> i % 2 == 0 ? Option.some(i / 2) : Option.none()).map(s -> s.toList())); // Some(List(1,2,3))
		f(stream(1, 2, 3).uncons(0, a -> b -> b._1().toList())); // List(2,3)
		// unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
		f(Stream.unfold(b -> b == 0 ? Option.none() : Option.some(P.p(b, b - 1)), 10).toList()); // List(10,9,8,7,6,5,4,3,2,1)
		f(P2.map(kv -> kv.toList(), Stream.unzip(stream(P.p(1, 2), P.p(3, 4))))); // (List(1,3),List(2,4))
		f(stream(1, 2, 3).zip(stream('a', 'b', 'c')).toList()); // List((1,a),(2,b),(3,c))
		f(stream(1, 2, 3).zipIndex().toList()); // List((1,0),(2,1),(3,2))
		f(stream(1, 2, 3).zipWith(stream(1, 2, 3), a -> b -> a * b).toList()); // List(1,4,9)
		f(stream(1, 2, 3).zipWith(stream(1, 2, 3), (a, b) -> a * b).toList()); // List(1,4,9)
	}

}
