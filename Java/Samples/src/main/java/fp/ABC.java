package fp;

import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.IntConsumer;
import java.util.function.IntUnaryOperator;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
class Value{
	String ch;
	int id;
}

@FunctionalInterface
interface TriFunction {
	public int example(int n1, int n2, int n3);
}

public class ABC {
	static int nnnn = 1;
	public static void main(String[] args) {
		List<String> list = Arrays.asList("Java", "Scala", "Groovy");
		list.sort((String s1, String s2) -> s1.length() - s2.length());
		System.out.println(list);
		
		List<Integer> v = Arrays.asList(5, 2, 3, 4, 8);
		v.stream()
		.filter(i -> i % 2 == 0)
		.map(i -> i * 2)
		.sorted()
		.forEach(System.out::println);
		
		System.out.println(v.stream().allMatch(i -> i % 2 == 0));
		System.out.println(v.stream().anyMatch(i -> i % 2 == 0));
		System.out.println(v.stream().noneMatch(i -> i % 2 == 0));
		System.out.println(v.stream().max((i1, i2) -> i1 - i2).get());
		System.out.println(v.stream().collect(Collectors.groupingBy((Integer i) -> i % 2)));
		

		System.out.println(Stream.of(1, 2, 3, 4, 5).reduce((acc, n) -> acc * n).get());
		System.out.println(IntStream.of(1, 2, 3, 4, 5).sum());
		
		int[] numbers = {5, 4, 1, 3, 9, 8, 6, 7, 2, 0};
		long oddNumbers = Arrays.stream(numbers).filter(num -> num % 2 == 1).count();
		System.out.println(oddNumbers);
		int numSum = Arrays.stream(numbers).sum();
		System.out.println(numSum);
		int minNum = Arrays.stream(numbers).min().orElse(-1);
		System.out.println(minNum);
		int maxNum = Arrays.stream(numbers).max().orElse(-1);
		System.out.println(maxNum);
		double averageNum = Arrays.stream(numbers).average().orElse(-1);
		System.out.println(averageNum);
		double[] doubles = {1.7, 2.3, 1.9, 4.1, 2.9};
		double productOfDoubles = Arrays.stream(doubles).reduce((acc, n) -> acc * n).getAsDouble();
		System.out.println(productOfDoubles);
		double startBalance = 100.0;
		int[] attemptedWithdrawals = {20, 10, 40, 50, 10, 70, 30};
		double endBalance = Arrays.stream(attemptedWithdrawals).boxed()
				.reduce(startBalance, (balance, nextWithdrawal) -> 
				(nextWithdrawal <= balance) ? (balance - nextWithdrawal) : balance,
				Double::sum);
		System.out.println(endBalance);
		
		IntStream.rangeClosed(1, 100).skip(5).limit(5).forEach(System.out::println);
		
		
		Stream.of(1, 2, 3).flatMap(i -> Stream.of(i, i, i)).forEach(System.out::println);
		//Stream.iterate(1, i -> i + 1).limit(10).forEach(System.out::println);
		
		IntStream.range(1, 10).boxed().flatMap(i ->
			IntStream.range(1, 10).mapToObj(j ->
				String.format("%d * %d = %d", i, j, i * j)
			)
		).forEach(System.out::println);
		
		BiFunction<Integer, Integer, Integer> f = (a, b) -> a + b;
		System.out.println(f.apply(6, 7));
		
		IntUnaryOperator twice = n -> n * 2;
		System.out.println(twice.applyAsInt(6));
		
		IntConsumer abc = n -> System.out.println(n);
		abc.accept(333);
		
		TriFunction g = (a, b, c) -> a + b + c;
		System.out.println(g.example(3, 4, 5));
		
		Integer nn = 2;
		int[] arr = {0}; 
		IntConsumer ccc = n -> {
			System.out.println(n);
			++arr[0];
			++nnnn;
			//nn = 3;
		};
		ccc.accept(5);
		ccc.accept(5);
		System.out.println(arr[0]);
	}

}
