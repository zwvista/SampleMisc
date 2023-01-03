import kotlinx.coroutines.*

fun main() {
//    basics1()
//    basics2()
//    basics3()
//    basics4()
//    basics5()
    basics6()
}

fun basics1() = runBlocking { // this: CoroutineScope
    launch { // launch a new coroutine and continue
        delay(1000L) // non-blocking delay for 1 second (default time unit is ms)
        println("World!") // print after delay
    }
    println("Hello") // main coroutine continues while a previous one is delayed

//    Hello
//    World!
}

fun basics2() = runBlocking { // this: CoroutineScope
    launch { doWorld2() }
    println("Hello")

//    Hello
//    World!
}

// this is your first suspending function
suspend fun doWorld2() {
    delay(1000L)
    println("World!")
}

fun basics3() = runBlocking {
    doWorld3()

//    Hello
//    World!
}

suspend fun doWorld3() = coroutineScope {  // this: CoroutineScope
    launch {
        delay(1000L)
        println("World!")
    }
    println("Hello")
}

// Sequentially executes doWorld followed by "Done"
fun basics4() = runBlocking {
    doWorld4()
    println("Done")

//    Hello
//    World 1
//    World 2
//    Done
}

// Concurrently executes both sections
suspend fun doWorld4() = coroutineScope { // this: CoroutineScope
    launch {
        delay(2000L)
        println("World 2")
    }
    launch {
        delay(1000L)
        println("World 1")
    }
    println("Hello")
}

fun basics5() = runBlocking {
    val job = launch { // launch a new coroutine and keep a reference to its Job
        delay(1000L)
        println("World!")
    }
    println("Hello")
    job.join() // wait until child coroutine completes
    println("Done")

//    Hello
//    World!
//    Done
}

fun basics6() = runBlocking {
    repeat(100_000) { // launch a lot of coroutines
        launch {
            delay(5000L)
            print(".")
        }
    }
}
