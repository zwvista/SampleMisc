package SampleKotlin

/**
 * Created by zwvista on 2017/05/20.
 */
fun f1(n: Int): Int {return n + 1}
fun f2(n: Int): Int = n + 1
fun f3(n: Int) = n + 1
fun f4(n: Int): Unit {println(n)}
fun f5(n: Int) {println(n)}
fun f6(n: Int): Unit = println(n)
fun f7(n: Int) = println(n)

fun describe(obj: Any): String =
    when (obj) {
        1          -> "One"
        "Hello"    -> "Greeting"
        is Long    -> "Long"
        !is String -> "Not a string"
        else       -> "Unknown"
    }

fun outside() {
    var a = 1
    fun inside() {
        a++;
    }
    inside();
    println(a);
}

fun add(a: Int = 1, b: Int = 2) = a + b
fun avg(vararg numbers: Double): Double {return 0.0}

class C {
    fun foo() { println("member") }
}
fun C.foo() { println("extension") }
fun C.foo(i: Int) { println("extension") }

fun main(args: Array<String>) {
    println(f1(0))
    println(f2(1))
    println(f3(2))
    f4(4); f5(5); f6(6); f7(7)

    for (i in 1..4) print(i); println() // prints "1234"
    for (i in 4..1) print(i); println() // prints nothing
    for (i in (1..4).reversed()) print(i); println() // prints "4321"
    for (i in 4 downTo 1) print(i); println() // prints "4321"
    for (i in 1..4 step 2) print(i); println() // prints "13"
    for (i in 4 downTo 1 step 2) print(i); println() // prints "42"
    for (i in 1 until 10) print(i); println() // prints "123456789"

    val x = 3
    if (x in 1..10) println(x)
    if (x !in 1..10) println(x)

    val x1 = 10; val y1 = 20
    val s = "x=$x1, y=${y1 + 1}" // x=10, y=21
    println(s)

    val price = """
${'$'}9.99
"""
    println(price)

    println(describe(1));
    println(describe(2L));
    println(describe(3.0));
    println(describe("abc"));

    outside()

    println(add());
    println(add(3));
    println(add(b = 3));

    C().foo() // member
    C().foo(1) // extension
}
