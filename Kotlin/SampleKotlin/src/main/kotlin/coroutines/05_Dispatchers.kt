package coroutines

import Post
import kotlinx.coroutines.*
import javax.swing.text.html.HTML.Tag.I

fun main() {
//    dispatchers1()
//    dispatchers2()
//    dispatchers3()
//    dispatchers4()
//    dispatchers5()
//    dispatchers6()
//    dispatchers7()
//    dispatchers8()
//    dispatchers9()
//    dispatchersA()
    dispatchersB()
}

fun dispatchers1() = runBlocking {
    launch { // context of the parent, main runBlocking coroutine
        println("main runBlocking      : I'm working in thread ${Thread.currentThread().name}")
    }
    launch(Dispatchers.Unconfined) { // not confined -- will work with main thread
        println("Unconfined            : I'm working in thread ${Thread.currentThread().name}")
    }
    launch(Dispatchers.Default) { // will get dispatched to DefaultDispatcher
        println("Default               : I'm working in thread ${Thread.currentThread().name}")
    }
    launch(newSingleThreadContext("MyOwnThread")) { // will get its own new thread
        println("newSingleThreadContext: I'm working in thread ${Thread.currentThread().name}")
    }

//    Unconfined            : I'm working in thread main
//    Default               : I'm working in thread DefaultDispatcher-worker-1
//    main runBlocking      : I'm working in thread main
//    newSingleThreadContext: I'm working in thread MyOwnThread
}

fun dispatchers2() = runBlocking {
    launch(Dispatchers.Unconfined) { // not confined -- will work with main thread
        println("Unconfined      : I'm working in thread ${Thread.currentThread().name}")
        delay(500)
        println("Unconfined      : After delay in thread ${Thread.currentThread().name}")
    }
    launch { // context of the parent, main runBlocking coroutine
        println("main runBlocking: I'm working in thread ${Thread.currentThread().name}")
        delay(1000)
        println("main runBlocking: After delay in thread ${Thread.currentThread().name}")
    }

//    Unconfined      : I'm working in thread main
//    main runBlocking: I'm working in thread main
//    Unconfined      : After delay in thread kotlinx.coroutines.DefaultExecutor
//    main runBlocking: After delay in thread main
}

// https://stackoverflow.com/questions/53250953/how-to-enable-dkotlinx-coroutines-debug-in-intellij-idea
// Run -> Edit configuration
// -Dkotlinx.coroutines.debug
fun log(msg: String) = println("[${Thread.currentThread().name}] $msg")

fun dispatchers3() = runBlocking {
    val a = async {
        log("I'm computing a piece of the answer")
        6
    }
    val b = async {
        log("I'm computing another piece of the answer")
        7
    }
    log("The answer is ${a.await() * b.await()}")

//    [main @coroutine#2] I'm computing a piece of the answer
//    [main @coroutine#3] I'm computing another piece of the answer
//    [main @coroutine#1] The answer is 42
}

fun dispatchers4() = runBlocking {
    newSingleThreadContext("Ctx1").use { ctx1 ->
        newSingleThreadContext("Ctx2").use { ctx2 ->
            runBlocking(ctx1) {
                log("Started in ctx1")
                withContext(ctx2) {
                    log("Working in ctx2")
                }
                log("Back to ctx1")
            }
        }
    }

//    [Ctx1 @coroutine#2] Started in ctx1
//    [Ctx2 @coroutine#2] Working in ctx2
//    [Ctx1 @coroutine#2] Back to ctx1
}

fun dispatchers5() = runBlocking {
    println("My job is ${coroutineContext[Job]}")

//    My job is BlockingCoroutine{Active}@13a5fe33
}

fun dispatchers6() = runBlocking {
    // launch a coroutine to process some kind of incoming request
    val request = launch {
        // it spawns two other jobs
        launch(Job()) {
            println("job1: I run in my own Job and execute independently!")
            delay(1000)
            println("job1: I am not affected by cancellation of the request")
        }
        // and the other inherits the parent context
        launch {
            delay(100)
            println("job2: I am a child of the request coroutine")
            delay(1000)
            println("job2: I will not execute this line if my parent request is cancelled")
        }
    }
    delay(500)
    request.cancel() // cancel processing of the request
    println("main: Who has survived request cancellation?")
    delay(1000) // delay the main thread for a second to see what happens

//    job1: I run in my own Job and execute independently!
//    job2: I am a child of the request coroutine
//    main: Who has survived request cancellation?
//    job1: I am not affected by cancellation of the request
}

fun dispatchers7() = runBlocking {
    // launch a coroutine to process some kind of incoming request
    val request = launch {
        repeat(3) { i -> // launch a few children jobs
            launch  {
                delay((i + 1) * 200L) // variable delay 200ms, 400ms, 600ms
                println("Coroutine $i is done")
            }
        }
        println("request: I'm done and I don't explicitly join my children that are still active")
    }
    request.join() // wait for completion of the request, including all its children
    println("Now processing of the request is complete")

//    request: I'm done and I don't explicitly join my children that are still active
//    Coroutine 0 is done
//    Coroutine 1 is done
//    Coroutine 2 is done
//    Now processing of the request is complete
}

fun dispatchers8() = runBlocking {
    log("Started main coroutine")
// run two background value computations
    val v1 = async(CoroutineName("v1coroutine")) {
        delay(500)
        log("Computing v1")
        252
    }
    val v2 = async(CoroutineName("v2coroutine")) {
        delay(1000)
        log("Computing v2")
        6
    }
    log("The answer for v1 / v2 = ${v1.await() / v2.await()}")

//    [main @coroutine#1] Started main coroutine
//    [main @v1coroutine#2] Computing v1
//    [main @v2coroutine#3] Computing v2
//    [main @coroutine#1] The answer for v1 / v2 = 42
}

fun dispatchers9() = runBlocking {
    launch(Dispatchers.Default + CoroutineName("test")) {
        println("I'm working in thread ${Thread.currentThread().name}")
    }

//    I'm working in thread DefaultDispatcher-worker-1 @test#2
}
class Activity {
    private val mainScope = CoroutineScope(Dispatchers.Default) // use Default for test purposes

    fun destroy() {
        mainScope.cancel()
    }

    fun doSomething() {
        // launch ten coroutines for a demo, each working for a different time
        repeat(10) { i ->
            mainScope.launch {
                delay((i + 1) * 200L) // variable delay 200ms, 400ms, ... etc
                println("Coroutine $i is done")
            }
        }
    }
} // class Activity ends

fun dispatchersA() = runBlocking<Unit> {
    val activity = Activity()
    activity.doSomething() // run test function
    println("Launched coroutines")
    delay(500L) // delay for half a second
    println("Destroying activity!")
    activity.destroy() // cancels all coroutines
    delay(1000) // visually confirm that they don't work

//    Launched coroutines
//    Coroutine 0 is done
//    Coroutine 1 is done
//    Destroying activity!
}

val threadLocal = ThreadLocal<String?>() // declare thread-local variable

fun dispatchersB() = runBlocking {
    threadLocal.set("main")
    println("Pre-main, current thread: ${Thread.currentThread()}, thread local value: '${threadLocal.get()}'")
    val job = launch(Dispatchers.Default + threadLocal.asContextElement(value = "launch")) {
        println("Launch start, current thread: ${Thread.currentThread()}, thread local value: '${threadLocal.get()}'")
        yield()
        println("After yield, current thread: ${Thread.currentThread()}, thread local value: '${threadLocal.get()}'")
    }
    job.join()
    println("Post-main, current thread: ${Thread.currentThread()}, thread local value: '${threadLocal.get()}'")

//    Pre-main, current thread: Thread[main @coroutine#1,5,main], thread local value: 'main'
//    Launch start, current thread: Thread[DefaultDispatcher-worker-1 @coroutine#2,5,main], thread local value: 'launch'
//    After yield, current thread: Thread[DefaultDispatcher-worker-1 @coroutine#2,5,main], thread local value: 'launch'
//    Post-main, current thread: Thread[main @coroutine#1,5,main], thread local value: 'main'
}
