package fp;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class FPDemo {
	public static void main(String[] args) {
		int[] numbers = {5, 4, 1, 3, 9, 8, 6, 7, 2, 0};
		IntStream lowNums = Arrays.stream(numbers).filter(num -> num < 5);
		System.out.println(Arrays.toString(lowNums.toArray()));
		
		IntStream numsPlusOne = Arrays.stream(numbers).map(num -> num + 1);
		System.out.println(Arrays.toString(numsPlusOne.toArray()));
		
		String[] words = {"cherry", "apple", "blueberry"};
		Stream<String> sortedWords = Arrays.stream(words).sorted();
		System.out.println(Arrays.toString(sortedWords.toArray()));
		
		Map<Integer, List<Integer>> numberGroups = Arrays.stream(numbers).boxed().collect(Collectors.groupingBy((Integer num) -> num % 5));
		System.out.println(Arrays.toString(numberGroups.entrySet().toArray()));
		
		String[] words2 = {"believe", "relief", "receipt", "field"};
		boolean iAfterE = Arrays.stream(words2).anyMatch(w -> w.contains("ei"));
		System.out.println(iAfterE);
		
		int[] numbers2 = {1, 11, 3, 19, 41, 65, 19};
		boolean onlyOdd = Arrays.stream(numbers2).allMatch(n -> n % 2 == 1);
		System.out.println(onlyOdd);
	}
}


