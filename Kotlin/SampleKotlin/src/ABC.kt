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
}
