package fp;

import fj.*;
import fj.data.List;
import fj.data.Stream;
import fj.data.vector.V;
import fj.data.vector.V2;
import fj.function.Effect1;
import fj.function.Integers;

import static fj.Ord.intOrd;
import static fj.Primitive.Integer_Double;
import static fj.Show.*;
import static fj.data.Array.array;
import static fj.data.List.arrayList;
import static fj.function.Integers.add;

public class ABC2 {
	static int nnnn = 1;
	public static void main(String[] args) {
		List<String> list = arrayList("Java", "Scala", "Groovy");
		list.sort(intOrd.contramap((String s) -> s.length()));
		listShow(stringShow).println(list);
		arrayShow(stringShow).println(list.toArray());
		
		List<Integer> v = arrayList(5, 2, 3, 4, 8);
		v.toStream()
		.filter(i -> i % 2 == 0)
		.map(i -> i * 2)
		.sort(intOrd)
		.foreachDoEffect(System.out::println);
		
		System.out.println(v.forall(i -> i % 2 == 0));
		System.out.println(v.exists(i -> i % 2 == 0));
		System.out.println(v.maximum(intOrd));
		treeMapShow(intShow, listShow(intShow)).println(v.groupBy(i -> i % 2, intOrd));

		System.out.println(Integers.product(arrayList(1, 2, 3, 4, 5)));
		System.out.println(Integers.sum(arrayList(1, 2, 3, 4, 5)));
		
		Integer[] numbers = {5, 4, 1, 3, 9, 8, 6, 7, 2, 0};
		long oddNumbers = arrayList(numbers).filter(num -> num % 2 == 1).length();
		System.out.println(oddNumbers);
		int numSum = arrayList(numbers).foldLeft1(add);
		System.out.println(numSum);
		int minNum = arrayList(numbers).minimum(intOrd);
		System.out.println(minNum);
		int maxNum = arrayList(numbers).maximum(intOrd);
		System.out.println(maxNum);
		double averageNum = (double)Integers.sum(arrayList(numbers)) / numbers.length;
		System.out.println(averageNum);
		Double[] doubles = {1.7, 2.3, 1.9, 4.1, 2.9};
		double productOfDoubles = arrayList(doubles).foldLeft1((acc, n) -> acc * n);
		System.out.println(productOfDoubles);
		double startBalance = 100.0;
		Integer[] attemptedWithdrawals = {20, 10, 40, 50, 10, 70, 30};
		double endBalance = arrayList(attemptedWithdrawals)
				.foldLeft((balance, nextWithdrawal) -> 
				(nextWithdrawal <= balance) ? (balance - nextWithdrawal) : balance, startBalance);
		System.out.println(endBalance);
		
		Stream.range(1, 100).drop(5).take(5).foreachDoEffect(System.out::println);
		
		
		arrayList(1, 2, 3).bind(i -> arrayList(i, i, i)).foreachDoEffect(System.out::println);
		//Stream.iterate(i -> i + 1, 1).take(10).foreachDoEffect(System.out::println);
		
		Stream.range(1, 10).bind(i ->
			Stream.range(1, 10).map(j ->
				String.format("%d * %d = %d", i, j, i * j)
			)
		).foreachDoEffect(System.out::println);
		
		F2<Integer, Integer, Integer> f = (a, b) -> a + b;
		System.out.println(f.f(6, 7));
		
		F<Integer, Integer> twice = n -> n * 2;
		System.out.println(twice.f(6));
		
		Effect1<Integer> abc = n -> System.out.println(n);
		abc.f(333);
		
		F3<Integer, Integer, Integer, Integer> g = (a, b, c) -> a + b + c;
		System.out.println(g.f(3, 4, 5));
		
		Integer nn = 2;
		int[] arr = {0}; 
		Effect1<Integer> ccc = n -> {
			System.out.println(n);
			++arr[0];
			++nnnn;
			//nn = 3;
		};
		ccc.f(5);
		ccc.f(5);
		System.out.println(arr[0]);
		
		double d = Integer_Double.f(1);
		// d = (double)1;
		Double[] ds = array(1, 2, 3).map(Integer_Double).array(Double[].class);
		
		
		P2<Integer, String> kv = P.p(1, "a");
		System.out.println(kv);  // (1,a)
		kv = P.<Integer, String>p2().f(2).f("b");
		System.out.println(kv);  // (2,b)
		
		V2<Integer> kv2 = V.v(1, 3);
		System.out.println(kv2.p());  // (1,3)
		kv2 = V.<Integer>v2().f(2, 4);
		System.out.println(kv2.p());  // (2,4)
	}

}
