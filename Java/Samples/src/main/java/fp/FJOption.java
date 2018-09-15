package fp;

import fj.data.List;
import fj.data.Option;
import fj.function.Effect1;

import static fj.Unit.unit;
import static fj.data.Option.some;

public class FJOption {

	static void f(Object o) { System.out.println(o); }
	public static void main(String[] args) {
		Effect1<Object> f = o -> System.out.println(o);
		f(some(3).apply(some(i -> i * 2))); // Some(6)
		f(some(3).bind(i -> some(i * 2))); // Some(6)
		f(some(3).bind(some(2), a -> b -> a * b)); // Some(6)
		f(some(3).bindProduct(some('a'))); // Some((3,a))
		f(some(3).exists(i -> i % 2 == 1)); // true
		f(some(2).exists(i -> i % 2 == 1)); // false
		f(Option.<Integer>none().exists(i -> i % 2 == 1)); // false
		f(some(3).filter(i -> i % 2 == 1)); // Some(3)
		f(some(2).filter(i -> i % 2 == 1)); // None
		f(Option.<Integer>none().filter(i -> i % 2 == 1)); // None
		f(some(3).forall(i -> i % 2 == 1)); // true
		f(some(2).forall(i -> i % 2 == 1)); // false
		f(Option.<Integer>none().forall(i -> i % 2 == 1)); // true
		some(3).foreach(i -> {System.out.print(i); return unit();});System.out.println(); // 3
		some(3).foreachDoEffect(i -> System.out.print(i));System.out.println(); // 3
		f(Option.fromNull(1)); // Some(1)
		f(Option.fromString("abc")); // Some(abc)
		f(Option.iif(true, 3)); // Some(3)
		f(Option.iif(false, 3)); // None
		f(Option.iif(i -> i % 2 == 1, 3)); // Some(3)
		f(some(3).isNone()); // false
		f(some(3).isSome()); // true
		f(Option.join(some(some(3)))); // Some(3)
		f(some(3).length()); // 1
		f(Option.<Integer>none().length()); // 0
		f(some(3).liftM2(some(2), (x, y) -> x * y)); // Some(6)
		f(some(3).map(i -> i * 3)); // Some(9)
		f(some(3).orElse(some(0))); // Some(3)
		f(Option.<Integer>none().orElse(some(0))); // Some(0)
		f(Option.<Integer>none().orSome(3)); // 3
		f(Option.sequence(List.list(some(1), some(2), some(3)))); // Some(List(1,2,3))
		f(some(3).sequence(some('a'))); // Some(a)
		f(Option.somes(List.list(some(1), some(2), some(3)))); // List(1,2,3)
		f(some(3).traverseList(i -> List.list(1, 2, 3))); // List(Some(1),Some(2),Some(3))
		f(some(3).traverseOption(i -> i % 2 == 1 ? some(i) : Option.none())); // Some(Some(3))
	}

}
