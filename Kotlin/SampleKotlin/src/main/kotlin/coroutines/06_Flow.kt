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
//    flowAA()
//    flowAB()
    flowAC()
}

fun simple1(): List<Int> = listOf(1, 2, 3)

fun flow1() {
    simple1().forEach { value -> println(value) }

//    1
//    2
//    3
}

fun simple2(): Sequence<Int> = sequence { // sequence builder
    for (i in 1..3) {
        Thread.sleep(100) // pretend we are computing it
        yield(i) // yield next value
    }
}

fun flow2() {
    simple2().forEach { value -> println(value) }

//    1
//    2
//    3
}

suspend fun simple3(): List<Int> {
    delay(1000) // pretend we are doing something asynchronous here
    return listOf(1, 2, 3)
}

fun flow3() = runBlocking {
    simple3().forEach { value -> println(value) }

//    1
//    2
//    3
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

//    I'm not blocked 1
//    1
//    I'm not blocked 2
//    2
//    I'm not blocked 3
//    3
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

//    Calling simple function...
//    Calling collect...
//    Flow started
//    1
//    2
//    3
//    Calling collect again...
//    Flow started
//    1
//    2
//    3
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

//    Emitting 1
//    1
//    Emitting 2
//    2
//    Done
}

fun flow7() = runBlocking {
    (1..3).asFlow().collect { value -> println(value) }

//    1
//    2
//    3
}

suspend fun performRequest(request: Int): String {
    delay(1000) // imitate long-running asynchronous work
    return "response $request"
}

fun flow8() = runBlocking {
    (1..3).asFlow() // a flow of requests
        .map { request -> performRequest(request) }
        .collect { response -> println(response) }

//    response 1
//    response 2
//    response 3
}

fun flow9() = runBlocking {
    (1..3).asFlow() // a flow of requests
        .transform { request ->
            emit("Making request $request")
            emit(performRequest(request))
        }
        .collect { response -> println(response) }

//    Making request 1
//    response 1
//    Making request 2
//    response 2
//    Making request 3
//    response 3
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

//    1
//    2
//    Finally in numbers
}

fun flowB() = runBlocking {
    val sum = (1..5).asFlow()
        .map { it * it } // squares of numbers from 1 to 5
        .reduce { a, b -> a + b } // sum them (terminal operator)
    println(sum)

//    55
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

//    Filter 1
//    Filter 2
//    Map 2
//    Collect string 2
//    Filter 3
//    Filter 4
//    Map 4
//    Collect string 4
//    Filter 5
}

fun simpleD(): Flow<Int> = flow {
    log("Started simple flow")
    for (i in 1..3) {
        emit(i)
    }
}

fun flowD() = runBlocking {
    simpleD().collect { value -> log("Collected $value") }

//    [main @coroutine#1] Started simple flow
//    [main @coroutine#1] Collected 1
//    [main @coroutine#1] Collected 2
//    [main @coroutine#1] Collected 3
}

fun simpleE(): Flow<Int> = flow {
    // The WRONG way to change context for CPU-consuming code in flow builder
    withContext(Dispatchers.Default) {
        for (i in 1..3) {
            Thread.sleep(100) // pretend we are computing it in CPU-consuming way
            emit(i) // emit next value
        }
    }

//    Exception in thread "main" java.lang.IllegalStateException: Flow invariant is violated:
//    Flow was collected in [CoroutineId(1), "coroutine#1":BlockingCoroutine{Active}@2255421, BlockingEventLoop@5c2e61c0],
//    but emission happened in [CoroutineId(1), "coroutine#1":DispatchedCoroutine{Active}@35ea2068, Dispatchers.Default].
//    Please refer to 'flow' documentation or use 'flowOn' instead
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

//    [DefaultDispatcher-worker-1 @coroutine#2] Emitting 1
//    [main @coroutine#1] Collected 1
//    [DefaultDispatcher-worker-1 @coroutine#2] Emitting 2
//    [main @coroutine#1] Collected 2
//    [DefaultDispatcher-worker-1 @coroutine#2] Emitting 3
//    [main @coroutine#1] Collected 3
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

//    1
//    2
//    3
//    Collected in 1246 ms
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

//    1
//    2
//    3
//    Collected in 1122 ms
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

//    1
//    3
//    Collected in 795 ms
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

//    Collecting 1
//    Collecting 2
//    Collecting 3
//    Done 3
//    Collected in 806 ms
}

fun flowK() = runBlocking {
    val nums = (1..3).asFlow() // numbers 1..3
    val strs = flowOf("one", "two", "three") // strings
    nums.zip(strs) { a, b -> "$a -> $b" } // compose a single string
        .collect { println(it) } // collect and print

//    1 -> one
//    2 -> two
//    3 -> three
}

fun flowL() = runBlocking {
    val nums = (1..3).asFlow().onEach { delay(300) } // numbers 1..3 every 300 ms
    val strs = flowOf("one", "two", "three").onEach { delay(400) } // strings every 400 ms
    val startTime = System.currentTimeMillis() // remember the start time
    nums.zip(strs) { a, b -> "$a -> $b" } // compose a single string with "zip"
        .collect { value -> // collect and print
            println("$value at ${System.currentTimeMillis() - startTime} ms from start")
        }

//    1 -> one at 441 ms from start
//    2 -> two at 841 ms from start
//    3 -> three at 1265 ms from start
}

fun flowM() = runBlocking {
    val nums = (1..3).asFlow().onEach { delay(300) } // numbers 1..3 every 300 ms
    val strs = flowOf("one", "two", "three").onEach { delay(400) } // strings every 400 ms
    val startTime = System.currentTimeMillis() // remember the start time
    nums.combine(strs) { a, b -> "$a -> $b" } // compose a single string with "combine"
        .collect { value -> // collect and print
            println("$value at ${System.currentTimeMillis() - startTime} ms from start")
        }

//    1 -> one at 457 ms from start
//    2 -> one at 669 ms from start
//    2 -> two at 859 ms from start
//    3 -> two at 975 ms from start
//    3 -> three at 1266 ms from start
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

//    1: First at 143 ms from start
//    1: Second at 649 ms from start
//    2: First at 754 ms from start
//    2: Second at 1260 ms from start
//    3: First at 1369 ms from start
//    3: Second at 1871 ms from start
}

fun flowO() = runBlocking {
    val startTime = System.currentTimeMillis() // remember the start time
    (1..3).asFlow().onEach { delay(100) } // a number every 100 ms
        .flatMapMerge { requestFlow(it) }
        .collect { value -> // collect and print
            println("$value at ${System.currentTimeMillis() - startTime} ms from start")
        }

//    1: First at 196 ms from start
//    2: First at 292 ms from start
//    3: First at 394 ms from start
//    1: Second at 697 ms from start
//    2: Second at 797 ms from start
//    3: Second at 905 ms from start
}

fun flowP() = runBlocking {
    val startTime = System.currentTimeMillis() // remember the start time
    (1..3).asFlow().onEach { delay(100) } // a number every 100 ms
        .flatMapLatest { requestFlow(it) }
        .collect { value -> // collect and print
            println("$value at ${System.currentTimeMillis() - startTime} ms from start")
        }

//    1: First at 194 ms from start
//    2: First at 376 ms from start
//    3: First at 483 ms from start
//    3: Second at 994 ms from start
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

//    Emitting 1
//    1
//    Emitting 2
//    2
//    Caught java.lang.IllegalStateException: Collected 2
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

//    Emitting 1
//    string 1
//    Emitting 2
//    Caught java.lang.IllegalStateException: Crashed on 2
}

fun flowS() = runBlocking {
    simpleR()
        .catch { e -> emit("Caught $e") } // emit on exception
        .collect { value -> println(value) }

//    Emitting 1
//    string 1
//    Emitting 2
//    Caught java.lang.IllegalStateException: Crashed on 2
}

fun simpleT(): Flow<Int> = flow {
    for (i in 1..3) {
        println("Emitting $i")
        emit(i)
    }
}

fun flowT() = runBlocking {
    simpleT()
        .catch { e -> println("Caught $e") } // does not catch downstream exceptions
        .collect { value ->
            check(value <= 1) { "Collected $value" }
            println(value)
        }

//    Emitting 1
//    1
//    Emitting 2
//    Exception in thread "main" java.lang.IllegalStateException: Collected 2
}

fun simpleU(): Flow<Int> = (1..3).asFlow()

fun flowU() = runBlocking {
    try {
        simpleU().collect { value -> println(value) }
    } finally {
        println("Done")
    }

//    1
//    2
//    3
//    Done
}

fun flowV() = runBlocking {
    simpleU()
        .onCompletion { println("Done") }
        .collect { value -> println(value) }

//    1
//    2
//    3
//    Done
}

fun simpleW(): Flow<Int> = flow {
    emit(1)
    throw RuntimeException()
}

fun flowW() = runBlocking {
    simpleW()
        .onCompletion { cause -> if (cause != null) println("Flow completed exceptionally") }
        .catch { cause -> println("Caught exception") }
        .collect { value -> println(value) }

//    1
//    Flow completed exceptionally
//    Caught exception
}

fun simpleX(): Flow<Int> = (1..3).asFlow()

fun flowX() = runBlocking {
    simpleX()
        .onCompletion { cause -> println("Flow completed with $cause") }
        .collect { value ->
            check(value <= 1) { "Collected $value" }
            println(value)
        }

//    1
//    Flow completed with java.lang.IllegalStateException: Collected 2
//    Exception in thread "main" java.lang.IllegalStateException: Collected 2
}

// Imitate a flow of events
fun events(): Flow<Int> = (1..3).asFlow().onEach { delay(100) }

fun flowY() = runBlocking {
    events()
        .onEach { event -> println("Event: $event") }
        .collect() // <--- Collecting the flow waits
    println("Done")

//    Event: 1
//    Event: 2
//    Event: 3
//    Done
}

fun flowZ() = runBlocking {
    events()
        .onEach { event -> println("Event: $event") }
        .launchIn(this) // <--- Launching the flow in a separate coroutine
    println("Done")

//    Done
//    Event: 1
//    Event: 2
//    Event: 3
}

fun foo(): Flow<Int> = flow {
    for (i in 1..5) {
        println("Emitting $i")
        emit(i)
    }
}

fun flowAA() = runBlocking {
    foo().collect { value ->
        if (value == 3) cancel()
        println(value)
    }

//    Emitting 1
//    1
//    Emitting 2
//    2
//    Emitting 3
//    3
//    Emitting 4
//    Exception in thread "main" kotlinx.coroutines.JobCancellationException: BlockingCoroutine was cancelled; job="coroutine#1":BlockingCoroutine{Cancelled}@1a8a8f7c
}

fun flowAB() = runBlocking {
    (1..5).asFlow().collect { value ->
        if (value == 3) cancel()
        println(value)
    }

//    1
//    2
//    3
//    4
//    5
//    Exception in thread "main" kotlinx.coroutines.JobCancellationException: BlockingCoroutine was cancelled; job="coroutine#1":BlockingCoroutine{Cancelled}@305fd85d
}

fun flowAC() = runBlocking {
    (1..5).asFlow().cancellable().collect { value ->
        if (value == 3) cancel()
        println(value)
    }

//    1
//    2
//    3
//    Exception in thread "main" kotlinx.coroutines.JobCancellationException: BlockingCoroutine was cancelled; job="coroutine#1":BlockingCoroutine{Cancelled}@4b553d26
}
