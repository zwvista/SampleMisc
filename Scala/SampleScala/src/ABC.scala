// ABC.scala

object ABC {
    def f1(n: Int): Int = {return n + 1}
    def f2(n: Int) = {n + 1}
    def f3(n: Int) = n + 1
    def f4(n: Int): Unit = {println(n)}
    def f5(n: Int) = {println(n)}
    def f6(n: Int) {println(n)}
    def f7(n: Int) = println(n)
    def f8() = println("hello")
    def f9 = println("hello")
    lazy val v = {print("v="); 3}
    val f10: Int => Unit = n => println(n)
    val f11 = (n:Int) => println(n)
    val f12: Unit => Unit = u => println("hello")
    val f13 = (u: Unit) => println("hello")
    val f14: () => Unit = () => println("hello")
    val f15 = () => println("hello")

    def main(args:Array[String]) = {
        println(f1(3))
        println(f2(3))
        println(f3(3))
        f4(3); f5(3); f6(3); f7(3); f10(3); f11(3)
        f8(); f8; f9; f12(); f13(); f14(); f15()
        val (x, y) = (10, 20)
        println(s"x=$x, y=${y + 1}")
        println(v)
    }
}
