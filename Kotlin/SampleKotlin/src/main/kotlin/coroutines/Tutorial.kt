package coroutines

import kotlinx.coroutines.*
import java.util.concurrent.atomic.AtomicLong

fun main() {
//    tutorial1()
//    tutorial2()
//    tutorial3()
//    tutorial4()
//    tutorial5()
    tutorial6()
}

fun tutorial1() {
    println("Start")

    // Start a coroutine
    GlobalScope.launch {
        delay(1000)
        println("Hello")
    }

    runBlocking {
        delay(2000)
    }
    println("Stop")
}

fun tutorial2() {
    val c = AtomicLong()

    for (i in 1..1_000_000L)
        GlobalScope.launch {
            c.addAndGet(i)
        }

    println(c.get())
}

fun tutorial3() {
    val deferred = (1..1_000_000).map { n ->
        GlobalScope.async {
            delay(1000)
            n
        }
    }
    runBlocking {
        val sum = deferred.sumOf { it.await().toLong() }
        println("Sum: $sum")
    }
    // Sum: 500000500000
}

fun tutorial4() {
    val deferred = (1..1_000_000).map { n ->
        GlobalScope.async {
            workload(n)
        }
    }
    runBlocking {
        val sum = deferred.sumOf { it.await().toLong() }
        println("Sum: $sum")
    }
    // Sum: 500000500000
}

suspend fun workload(n: Int): Int {
    delay(1000)
    return n
}

fun tutorial5() = runBlocking {
    val deferred: Deferred<Int> = async {
        loadData()
    }
    log("waiting...")
    log(deferred.await().toString())
}

fun tutorial6() = runBlocking {
    val deferred: Deferred<Int> = async(Dispatchers.Default) {
        loadData()
    }
    log("waiting...")
    log(deferred.await().toString())
}

suspend fun loadData(): Int {
    log("loading...")
    delay(1000L)
    log("loaded!")
    return 42
}