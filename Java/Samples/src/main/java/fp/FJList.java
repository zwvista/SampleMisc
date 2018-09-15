package fp;

import fj.Equal;
import fj.Ord;
import fj.Ordering;
import fj.P;
import fj.data.List;
import fj.data.Option;

import static fj.Equal.intEqual;
import static fj.Ord.intOrd;
import static fj.data.List.list;

public class FJList {

	static void f(Object o) { System.out.println(o); }
	public static void main(String[] args) {
		f(list(1, 1, 1).allEqual(intEqual)); // true
		f(list(1, 2).append(list(3))); // List(1,2,3)
		f(list(1, 2, 3).apply(list(i -> i + 3, i -> i * 2))); // List(4,5,6,2,4,6)
		Integer[] arr = list(1, 2, 3).array(Integer[].class); // 123
		f(List.asString(list('a', 'b', 'c'))); // abc
		f(list(1, 2, 3).bind(i -> list(i, i * 2))); // List(1,2,2,4,3,6)
		f(list(1, 2, 3).bind(list(4, 5, 6), x -> y -> x * y)); // List(4,5,6,8,10,12,12,15,18)
		f(list(1, 2, 3).bind(list(4, 5, 6), (x, y) -> x * y)); // List(4,5,6,8,10,12,12,15,18)
		// takeUntil + dropUntil
		f(list(1, 2, 3).breakk(i -> i % 2 == 0)); // (List(1),List(2,3))
		f(List.cons(3, list(1, 2))); // List(3,1,2)
		f(list(1, 2).cons(3)); // List(3,1,2)
		f(list(1, 2, 3).delete(2, intEqual)); // List(1,3)
		f(list(1, 2, 3, 4, 5).drop(2)); // List(3,4,5)
		f(list(1, 2, 3, 4, 5).dropWhile(i -> i < 3)); // List(3,4,5)
		f(list(1, 2, 3, 4, 5).elementIndex(intEqual, 3)); // Some(2)
		f(list(1, 2, 3, 4, 5).exists(i -> i % 3 == 1)); // true
		f(list(1, 2, 3, 4, 5).filter(i -> i % 2 == 1)); // List(1,3,5)
		f(list(1, 2, 3, 4, 5).find(i -> i / 3 == 1)); // Some(3)
		f(list(1, 2, 3, 4, 5).foldLeft(acc -> i -> acc + i - 1, 0)); // 10
		f(list(1, 2, 3, 4, 5).foldLeft((acc, i) -> acc + i - 1, 0)); // 10
		f(list(1, 2, 3, 4, 5).foldLeft1(acc -> i -> acc + i - 1)); // 11
		f(list(1, 2, 3, 4, 5).foldLeft1((acc, i) -> acc + i - 1)); // 11
		f(list(1, 2, 3, 4, 5).foldRight(acc -> i -> acc + i - 1, 0)); // 10
		f(list(1, 2, 3, 4, 5).foldRight((acc, i) -> acc + i - 1 , 0)); // 10
		f(list(1, 2, 3, 4, 5).forall(i -> i % 2 == 0)); // false
		list(1, 2, 3, 4, 5).foreachDoEffect(i -> System.out.print(i));System.out.println(); // 12345
		f(List.fromString("abcde")); // List(a,b,c,d,e)
		f(list(2, 2, 3, 3).group(Equal.equal(a -> b -> a == b))); // List(List(2,2),List(3,3))
		f(list(1, 2, 3, 4, 5).groupBy(i -> i % 2, intOrd)); // TreeMap((0: List(4,2)),(1: List(5,3,1)))
		f(list(1, 2, 3, 4, 5).groupBy(i -> i % 2, i -> i * 2, Ord.ord(a -> b -> Ordering.fromInt(a - b)))); // TreeMap((0: List(8,4)),(1: List(10,6,2)))
		f(list(1, 2, 3, 4, 5).head()); // 1
		f(list(1, 2, 3, 4, 5).headOption()); // Some(1)
		f(list(1, 2, 3, 4, 5).index(2)); // 3
		f(list(1, 2, 3, 4, 5).init()); // List(1,2,3,4)
		f(list(1, 2, 3, 4, 5).inits()); // List(List(),List(1),List(1,2),List(1,2,3),List(1,2,3,4),List(1,2,3,4,5))
		f(list(1, 2, 3, 4, 5).insertBy(a -> b -> Ordering.fromInt(a - b), 3)); // List(1,2,3,3,4,5)
		f(list(1, 2, 3).intercalate(list(list(9, 9), list(10, 10), list(11, 11)))); // List(9,9,1,2,3,10,10,1,2,3,11,11)
		f(list(1, 2, 3).intersperse(9)); // List(1,9,2,9,3)
		f(list(1, 2, 3).isEmpty()); // false
		f(list(1, 2, 3).isNotEmpty()); // true
		f(list(1, 2, 3).isPrefixOf(intEqual, list(1, 2, 3, 4, 5))); // true
		f(list(1).isSingle()); // true
		f(list(1, 2, 3).isSuffixOf(intEqual, list(0, 1, 2, 3))); // true
		f(List.iterableList(java.util.Arrays.asList(1, 2, 3))); // List(1,2,3)
		f(List.iterateWhile(i -> i + 1, i -> i < 10, 1)); // List(1,2,3,4,5,6,7,8,9)
		f(List.join(list(list(1, 2), list(3, 4)))); // List(1,2,3,4)
		f(list(1, 2, 3).last()); // 3
		f(list(1, 2, 3).length()); // 3
		f(List.<Integer, Integer, Integer>liftM2(a -> b -> a * b).f(list(1, 2, 3)).f(list(1, 2, 3))); // List(1,2,3,2,4,6,3,6,9)
		f(List.lookup(intEqual, list(P.p(1, 2), P.p(3, 4)), 1)); // Some(2)
		f(list(1, 2, 3).map(i -> i * 2)); // List(2,4,6)
		f(list(1, 2, 3).<Integer, Integer>mapM(a -> b -> a * b).f(3)); // List(3,6,9)
		f(list(1, 2, 3).mapMOption(i -> i % 2 == 0 ? Option.some(i / 2) : Option.none())); // None
		f(list(1, 2, 3).mapMOption(i -> i > 0 ? Option.some(i / 2) : Option.none())); // Some(List(1,2,3))
		f(list(1, 2, 3).maximum(intOrd)); // 3
		f(list(1, 2, 3).minimumOption(intOrd)); // Some(1)
		f(list(1, 2, 3).minus(intEqual, list(3, 4, 5))); // List(1,2)
		f(list(1, 2, 2, 2, 3).mode(intOrd)); // 2
		f(List.nil()); // List()
		f(list(1, 2, 2, 3, 3).nub()); // List(1,2,3)
		f(list().orHead(() -> -1)); // -1
		f(list().orTail(() -> list(1, 2))); // List(1,2)
		f(list(1, 2, 3, 4, 5).partition(i -> i % 2 == 0)); // (List(2,4),List(1,3,5))
		f(list(1, 2, 3, 4, 5).partition(3)); // List(List(1,2,3),List(4,5))
		f(List.range(1, 10)); // List(1,2,3,4,5,6,7,8,9)
		f(list(1, 2, 3, 4, 5).removeAll(i -> i % 2 == 0)); // List(1,3,5)
		f(List.replicate(3, 3)); // List(3,3,3)
		f(list(1, 2, 3, 4, 5).reverse()); // List(5,4,3,2,1)
		f(list(1, 2, 3).sequence(list('a', 'b'))); // List(a,b,a,b,a,b)
		f(List.single(3)); // List(3)
		f(list(1, 2, 3).snoc(4)); // List(1,2,3,4)
		f(list(3, 2, 1).sort(intOrd)); // List(1,2,3)
		// takeWhile + dropWhile
		f(list(1, 2, 3, 4, 5).span(i -> i < 3)); // (List(1,2),List(3,4,5))
		f(list(1, 2, 3, 4, 5).splitAt(3)); // (List(1,2,3),List(4,5))
		f(list(1, 2, 3, 4, 5).tail()); // List(2,3,4,5)
		f(list(1, 2, 3, 4, 5).tailOption()); // Some(List(2,3,4,5))
		f(list(1, 2, 3, 4, 5).tails()); // List(List(1,2,3,4,5),List(2,3,4,5),List(3,4,5),List(4,5),List(5),List())
		f(list(1, 2, 3, 4, 5).take(3)); // List(1,2,3)
		f(list(1, 2, 3, 4, 5).takeWhile(i -> i < 3)); // List(1,2)
		f(list(1, 2, 3, 4, 5).toArray()); // Array(1,2,3,4,5)
		f(list(1, 2, 3, 4, 5).toJavaList()); // [1, 2, 3, 4, 5]
		// traverse (\i -> [i, i + 1]) [1, 2, 3]
		f(list(1, 2, 3).traverseList(i -> list(i, i + 1))); // List(List(1,2,3),List(1,2,4),List(1,3,3),List(1,3,4),List(2,2,3),List(2,2,4),List(2,3,3),List(2,3,4))
		f(list(1, 2, 3).traverseOption(i -> i % 2 == 0 ? Option.some(i / 2) : Option.none())); // None
		f(list(2, 4, 6).traverseOption(i -> i % 2 == 0 ? Option.some(i / 2) : Option.none())); // Some(List(1,2,3))
		f(list(1, 2, 3).uncons((a, b) -> b.head(), 0)); // 2
		// unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
		f(List.unfold(b -> b == 0 ? Option.none() : Option.some(P.p(b, b - 1)), 10)); // List(10,9,8,7,6,5,4,3,2,1)
		f(List.unzip(list(P.p(1, 2), P.p(3, 4)))); // (List(1,3),List(2,4))
		f(list(1, 2, 3).zip(list('a', 'b', 'c'))); // List((1,a),(2,b),(3,c))
		f(list(1, 2, 3).zipIndex()); // List((1,0),(2,1),(3,2))
		f(list(1, 2, 3).zipWith(list(1, 2, 3), a -> b -> a * b)); // List(1,4,9)
		f(list(1, 2, 3).zipWith(list(1, 2, 3), (a, b) -> a * b)); // List(1,4,9)
	}

}
