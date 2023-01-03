package coroutines

import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*
import kotlin.system.measureTimeMillis

fun main() {
//    flow1()
//    flow2()
//    flow3()
//    flow4()
//    flow5()
//    flow6()
//    flow7()
//    flow8()
//    flow9()
//    flowA()
//    flowB()
//    flowC()
//    flowD()
//    flowE()
//    flowF()
//    flowG()
//    flowH()
//    flowI()
//    flowJ()
//    flowK()
//    flowL()
//    flowM()
//    flowN()
//    flowO()
//    flowP()
//    flowQ()
//    flowR()
//    flowS()
//    flowT()
//    flowU()
//    flowV()
//    flowW()
//    flowX()
//    flowY()
//    flowZ()
    flowAA()
}

fun simple1(): List<Int> = listOf(1, 2, 3)

fun flow1() {
    simple1().forEach { value -> println(value) }
}

fun simple2(): Sequence<Int> = sequence { // sequence builder
    for (i in 1..3) {
        Thread.sleep(100) // pretend we are computing it
        yield(i) // yield next value
    }
}

fun flow2() {
    simple2().forEach { value -> println(value) }
}

suspend fun simple3(): List<Int> {
    delay(1000) // pretend we are doing something asynchronous here
    return listOf(1, 2, 3)
}

fun flow3() = runBlocking {
    simple3().forEach { value -> println(value) }
}

fun simple4(): Flow<Int> = flow { // flow builder
    for (i in 1..3) {
        delay(100) // pretend we are doing something useful here
        emit(i) // emit next value
    }
}

fun flow4() = runBlocking {
    // Launch a concurrent coroutine to check if the main thread is blocked
    launch {
        for (k in 1..3) {
            println("I'm not blocked $k")
            delay(100)
        }
    }
    // Collect the flow
    simple4().collect { value -> println(value) }
}

fun simple5(): Flow<Int> = flow {
    println("Flow started")
    for (i in 1..3) {
        delay(100)
        emit(i)
    }
}

fun flow5() = runBlocking {
    println("Calling simple function...")
    val flow = simple5()
    println("Calling collect...")
    flow.collect { value -> println(value) }
    println("Calling collect again...")
    flow.collect { value -> println(value) }
}

fun simple6(): Flow<Int> = flow {
    for (i in 1..3) {
        delay(100)
        println("Emitting $i")
        emit(i)
    }
}

fun flow6() = runBlocking {
    withTimeoutOrNull(250) { // Timeout after 250ms
        simple6().collect { value -> println(value) }
    }
    println("Done")
}

fun flow7() = runBlocking {
    (1..3).asFlow().collect { value -> println(value) }
}

suspend fun performRequest(request: Int): String {
    delay(1000) // imitate long-running asynchronous work
    return "response $request"
}

fun flow8() = runBlocking {
    (1..3).asFlow() // a flow of requests
        .map { request -> performRequest(request) }
        .collect { response -> println(response) }
}

fun flow9() = runBlocking {
    (1..3).asFlow() // a flow of requests
        .transform { request ->
            emit("Making request $request")
            emit(performRequest(request))
        }
        .collect { response -> println(response) }
}

fun numbers(): Flow<Int> = flow {
    try {
        emit(1)
        emit(2)
        println("This line will not execute")
        emit(3)
    } finally {
        println("Finally in numbers")
    }
}

fun flowA() = runBlocking {
    numbers()
        .take(2) // take only the first two
        .collect { value -> println(value) }
}

fun flowB() = runBlocking {
    val sum = (1..5).asFlow()
        .map { it * it } // squares of numbers from 1 to 5
        .reduce { a, b -> a + b } // sum them (terminal operator)
    println(sum)
}

fun flowC() = runBlocking {
    (1..5).asFlow()
        .filter {
            println("Filter $it")
            it % 2 == 0
        }
        .map {
            println("Map $it")
            "string $it"
        }.collect {
            println("Collect $it")
        }
}

fun simpleD(): Flow<Int> = flow {
    log("Started simple flow")
    for (i in 1..3) {
        emit(i)
    }
}

fun flowD() = runBlocking {
    simpleD().collect { value -> log("Collected $value") }
}

fun simpleE(): Flow<Int> = flow {
    // The WRONG way to change context for CPU-consuming code in flow builder
    withContext(Dispatchers.Default) {
        for (i in 1..3) {
            Thread.sleep(100) // pretend we are computing it in CPU-consuming way
            emit(i) // emit next value
        }
    }
}

fun flowE() = runBlocking<Unit> {
    simpleE().collect { value -> println(value) }
}

fun simpleF(): Flow<Int> = flow {
    for (i in 1..3) {
        Thread.sleep(100) // pretend we are computing it in CPU-consuming way
        log("Emitting $i")
        emit(i) // emit next value
    }
}.flowOn(Dispatchers.Default) // RIGHT way to change context for CPU-consuming code in flow builder

fun flowF() = runBlocking<Unit> {
    simpleF().collect { value ->
        log("Collected $value")
    }
}

fun simpleG(): Flow<Int> = flow {
    for (i in 1..3) {
        delay(100) // pretend we are asynchronously waiting 100 ms
        emit(i) // emit next value
    }
}

fun flowG() = runBlocking<Unit> {
    val time = measureTimeMillis {
        simpleG().collect { value ->
            delay(300) // pretend we are processing it for 300 ms
            println(value)
        }
    }
    println("Collected in $time ms")
}

fun flowH() = runBlocking {
    val time = measureTimeMillis {
        simpleG()
            .buffer() // buffer emissions, don't wait
            .collect { value ->
                delay(300) // pretend we are processing it for 300 ms
                println(value)
            }
    }
    println("Collected in $time ms")
}

fun flowI() = runBlocking {
    val time = measureTimeMillis {
        simpleG()
            .conflate() // conflate emissions, don't process each one
            .collect { value ->
                delay(300) // pretend we are processing it for 300 ms
                println(value)
            }
    }
    println("Collected in $time ms")
}

fun flowJ() = runBlocking {
    val time = measureTimeMillis {
        simpleG()
            .collectLatest { value -> // cancel & restart on the latest value
                println("Collecting $value")
                delay(300) // pretend we are processing it for 300 ms
                println("Done $value")
            }
    }
    println("Collected in $time ms")
}

fun flowK() = runBlocking {
    val nums = (1..3).asFlow() // numbers 1..3
    val strs = flowOf("one", "two", "three") // strings
    nums.zip(strs) { a, b -> "$a -> $b" } // compose a single string
        .collect { println(it) } // collect and print
}

fun flowL() = runBlocking {
    val nums = (1..3).asFlow().onEach { delay(300) } // numbers 1..3 every 300 ms
    val strs = flowOf("one", "two", "three").onEach { delay(400) } // strings every 400 ms
    val startTime = System.currentTimeMillis() // remember the start time
    nums.zip(strs) { a, b -> "$a -> $b" } // compose a single string with "zip"
        .collect { value -> // collect and print
            println("$value at ${System.currentTimeMillis() - startTime} ms from start")
        }
}

fun flowM() = runBlocking {
    val nums = (1..3).asFlow().onEach { delay(300) } // numbers 1..3 every 300 ms
    val strs = flowOf("one", "two", "three").onEach { delay(400) } // strings every 400 ms
    val startTime = System.currentTimeMillis() // remember the start time
    nums.combine(strs) { a, b -> "$a -> $b" } // compose a single string with "combine"
        .collect { value -> // collect and print
            println("$value at ${System.currentTimeMillis() - startTime} ms from start")
        }
}

fun requestFlow(i: Int): Flow<String> = flow {
    emit("$i: First")
    delay(500) // wait 500 ms
    emit("$i: Second")
}

fun flowN() = runBlocking {
    val startTime = System.currentTimeMillis() // remember the start time
    (1..3).asFlow().onEach { delay(100) } // a number every 100 ms
        .flatMapConcat { requestFlow(it) }
        .collect { value -> // collect and print
            println("$value at ${System.currentTimeMillis() - startTime} ms from start")
        }
}

fun flowO() = runBlocking {
    val startTime = System.currentTimeMillis() // remember the start time
    (1..3).asFlow().onEach { delay(100) } // a number every 100 ms
        .flatMapMerge { requestFlow(it) }
        .collect { value -> // collect and print
            println("$value at ${System.currentTimeMillis() - startTime} ms from start")
        }
}

fun flowP() = runBlocking {
    val startTime = System.currentTimeMillis() // remember the start time
    (1..3).asFlow().onEach { delay(100) } // a number every 100 ms
        .flatMapLatest { requestFlow(it) }
        .collect { value -> // collect and print
            println("$value at ${System.currentTimeMillis() - startTime} ms from start")
        }
}

fun simpleQ(): Flow<Int> = flow {
    for (i in 1..3) {
        println("Emitting $i")
        emit(i) // emit next value
    }
}

fun flowQ() = runBlocking {
    try {
        simpleQ().collect { value ->
            println(value)
            check(value <= 1) { "Collected $value" }
        }
    } catch (e: Throwable) {
        println("Caught $e")
    }
}

fun simpleR(): Flow<String> =
    flow {
        for (i in 1..3) {
            println("Emitting $i")
            emit(i) // emit next value
        }
    }
    .map { value ->
        check(value <= 1) { "Crashed on $value" }
        "string $value"
    }

fun flowR() = runBlocking {
    try {
        simpleR().collect { value -> println(value) }
    } catch (e: Throwable) {
        println("Caught $e")
    }
}

fun simpleS(): Flow<Int> = (1..3).asFlow()

fun flowS() = runBlocking {
    try {
        simpleS().collect { value -> println(value) }
    } finally {
        println("Done")
    }
}

fun flowT() = runBlocking {
    simpleS()
        .onCompletion { println("Done") }
        .collect { value -> println(value) }
}

fun simpleU(): Flow<Int> = flow {
    emit(1)
    throw RuntimeException()
}

fun flowU() = runBlocking {
    simpleU()
        .onCompletion { cause -> if (cause != null) println("Flow completed exceptionally") }
        .catch { cause -> println("Caught exception") }
        .collect { value -> println(value) }
}

fun simpleV(): Flow<Int> = (1..3).asFlow()

fun flowV() = runBlocking {
    simpleV()
        .onCompletion { cause -> println("Flow completed with $cause") }
        .collect { value ->
            check(value <= 1) { "Collected $value" }
            println(value)
        }
}
// Imitate a flow of events
fun events(): Flow<Int> = (1..3).asFlow().onEach { delay(100) }

fun flowW() = runBlocking {
    events()
        .onEach { event -> println("Event: $event") }
        .collect() // <--- Collecting the flow waits
    println("Done")
}

fun flowX() = runBlocking {
    events()
        .onEach { event -> println("Event: $event") }
        .launchIn(this) // <--- Launching the flow in a separate coroutine
    println("Done")
}

fun foo(): Flow<Int> = flow {
    for (i in 1..5) {
        println("Emitting $i")
        emit(i)
    }
}

fun flowY() = runBlocking {
    foo().collect { value ->
        if (value == 3) cancel()
        println(value)
    }
}

fun flowZ() = runBlocking {
    (1..5).asFlow().collect { value ->
        if (value == 3) cancel()
        println(value)
    }
}

fun flowAA() = runBlocking {
    (1..5).asFlow().cancellable().collect { value ->
        if (value == 3) cancel()
        println(value)
    }
}
