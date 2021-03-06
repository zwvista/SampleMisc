/**
 * Created by zwvista on 2017/05/26.
 */



fun describe(obj: Any): String =
    when (obj) {
        1          -> "One"
        2, 3       -> "Two or Three"
        in 4..6    -> "Four to Six"
        "Hello"    -> "Greeting"
        is Long    -> "Long"
        !is String -> "Not a string"
        else       -> "Unknown"
    }

fun Int.isOdd() = this % 2 == 1
fun Int.isEven() = this % 2 == 0

var sum: (Int, Int) -> Unit = fun(a, b) = println(a + b)

fun main(args: Array<String>) {
    println(describe(1));
    println(describe(2));
    println(describe(4));
    println(describe(7));
    println(describe(9L));
    println(describe(10.0));
    println(describe("abc"));

    val x = 3
    when {
        x.isOdd() -> print("x is odd")
        x.isEven() -> print("x is even")
        else -> print("x is funny")
    }

    var (a, b) = 1 to 2 // a == 1, b == 2
    run { val temp = a; a = b; b = temp } // a == 2, b == 1


}