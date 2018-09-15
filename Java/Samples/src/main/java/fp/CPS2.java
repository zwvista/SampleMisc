package fp;

import fj.function.Effect1;

public class CPS2 {
	static int add(int x, int y) {return x + y;}
	static int square(int x) {return x * x;}
	static int pythagoras(int x, int y){return add(square(x), square(y));}
	static void add_cps(int x, int y, Effect1<Integer> f) {f.f(add(x, y));}
	static void square_cps(int x, Effect1<Integer> f) {f.f(square(x));}
	static void pythagoras_cps(int x, int y, Effect1<Integer> f) {
		square_cps(x, x_squared ->
		square_cps(y, y_squared ->
		add_cps(x_squared, y_squared, f)));
	}
	public static void main(String[] args) {
		System.out.println(pythagoras(3, 4));
		pythagoras_cps(3, 4, x -> System.out.println(x));
	}
}


