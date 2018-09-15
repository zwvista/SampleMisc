package fp;

import java.util.function.IntConsumer;

public class CPS {
	static int add(int x, int y) {return x + y;}
	static int square(int x) {return x * x;}
	static int pythagoras(int x, int y){return add(square(x), square(y));}
	static void add_cps(int x, int y, IntConsumer f) {f.accept(add(x, y));}
	static void square_cps(int x, IntConsumer f) {f.accept(square(x));}
	static void pythagoras_cps(int x, int y, IntConsumer f) {
		square_cps(x, x_squared ->
		square_cps(y, y_squared ->
		add_cps(x_squared, y_squared, f)));
	}
	public static void main(String[] args) {
		System.out.println(pythagoras(3, 4));
		pythagoras_cps(3, 4, x -> System.out.println(x));
	}
}


