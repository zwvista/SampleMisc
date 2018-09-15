package fp;

import fj.P;
import fj.data.Array;

import static fj.Unit.unit;
import static fj.data.Array.array;
import static fj.data.Array.iterableArray;

public class FJArray {

    static void f(Object o) { System.out.println(o); }
	public static void main(String[] args) {
		f(array(1, 2).append(array(3))); // Array(1,2,3)
		f(array(1, 2, 3).apply(array(i -> i + 3, i -> i * 2))); // Array(4,5,6,2,4,6)
		f(array(new Integer[]{1, 2, 3})); // Array(1,2,3)
		f(array(1, 2, 3).bind(i -> array(i, i * 2))); // Array(1,2,2,4,3,6)
		f(array(1, 2, 3).bind(array(4, 5, 6), x -> y -> x * y)); // Array(4,5,6,8,10,12,12,15,18)
		f(array(1, 2, 3).bind(array(4, 5, 6), (x, y) -> x * y)); // Array(4,5,6,8,10,12,12,15,18)
		f(Array.empty()); // Array()
		f(array(1, 2, 3, 4, 5).exists(i -> i % 3 == 1)); // true
		f(array(1, 2, 3, 4, 5).filter(i -> i % 2 == 1)); // Array(1,3,5)
		f(array(1, 2, 3, 4, 5).find(i -> i / 3 == 1)); // Some(3)
		f(array(1, 2, 3, 4, 5).foldLeft(acc -> i -> acc + i - 1, 0)); // 10
		f(array(1, 2, 3, 4, 5).foldLeft((acc, i) -> acc + i - 1, 0)); // 10
		f(array(1, 2, 3, 4, 5).foldRight(acc -> i -> acc + i - 1, 0)); // 10
		f(array(1, 2, 3, 4, 5).foldRight((acc, i) -> acc + i - 1, 0)); // 10
		f(array(1, 2, 3, 4, 5).forall(i -> i % 2 == 0)); // false
		array(1, 2, 3, 4, 5).foreach(i -> {System.out.print(i); return unit();});System.out.println(); // 12345
		array(1, 2, 3, 4, 5).foreachDoEffect(i -> System.out.print(i));System.out.println(); // 12345
		f(array(1, 2, 3, 4, 5).get(3)); // 4
		f(array(1, 2, 3).isEmpty()); // false
		f(array(1, 2, 3).isNotEmpty()); // true
		f(iterableArray(java.util.Arrays.asList(1, 2, 3))); // Array(1,2,3)
		f(Array.join(array(array(1, 2), array(3, 4)))); // Array(1,2,3,4)
		f(array(1, 2, 3).length()); // 3
		f(array(1, 2, 3).map(i -> i * 2)); // Array(2,4,6)
		f(Array.range(1, 10)); // Array(1,2,3,4,5,6,7,8,9)
		f(array(1, 2, 3, 4, 5).reverse()); // Array(5,4,3,2,1)
		f(array(1, 2, 3, 4, 5).scanLeft(acc -> i -> acc + i - 1, 0)); // Array(0,1,3,6,10)
		f(array(1, 2, 3, 4, 5).scanLeft((acc, i) -> acc + i - 1, 0)); // Array(0,1,3,6,10)
		f(array(1, 2, 3, 4, 5).scanLeft1(acc -> i -> acc + i - 1)); // Array(1,2,4,7,11)
		f(array(1, 2, 3, 4, 5).scanLeft1((acc, i) -> acc + i - 1)); // Array(1,2,4,7,11)
		f(array(1, 2, 3, 4, 5).scanRight(acc -> i -> acc + i - 1, 0)); // Array(10,10,9,7,4)
		f(array(1, 2, 3, 4, 5).scanRight((acc, i) -> acc + i - 1, 0)); // Array(10,10,9,7,4)
		f(array(1, 2, 3, 4, 5).scanRight1(acc -> i -> acc + i - 1)); // Array(11,11,10,8,5)
		f(array(1, 2, 3, 4, 5).scanRight1((acc, i) -> acc + i - 1)); // Array(11,11,10,8,5)
		f(array(1, 2, 3).sequence(array('a', 'b'))); // Array(a,b,a,b,a,b)
		f(Array.single(3)); // Array(3)
		f(Array.unzip(array(P.p(1, 2), P.p(3, 4)))); // (Array(1,3),Array(2,4))
		f(array(1, 2, 3).zip(array('a', 'b', 'c'))); // Array((1,a),(2,b),(3,c))
		f(array(1, 2, 3).zipIndex()); // Array((1,0),(2,1),(3,2))
		f(array(1, 2, 3).zipWith(array(1, 2, 3), a -> b -> a * b)); // Array(1,4,9)
		f(array(1, 2, 3).zipWith(array(1, 2, 3), (a, b) -> a * b)); // Array(1,4,9)
	}

}
