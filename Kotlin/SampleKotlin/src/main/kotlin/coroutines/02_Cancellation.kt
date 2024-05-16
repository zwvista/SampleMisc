package coroutines

import kotlinx.coroutines.*

fun main() {
//    cancellation1()
//    cancellation2()
//    cancellation3()
//    cancellation4()
//    cancellation5()
    cancellation6()
}

fun cancellation1() = runBlocking {
    val job = launch {
        repeat(1000) { i ->
            println("job: I'm sleeping $i ...")
            delay(500L)
        }
    }
    delay(1300L) // delay a bit
    println("main: I'm tired of waiting!")
    job.cancel() // cancels the job
    job.join() // waits for job's completion
    println("main: Now I can quit.")

//    job: I'm sleeping 0 ...
//    job: I'm sleeping 1 ...
//    job: I'm sleeping 2 ...
//    main: I'm tired of waiting!
//    main: Now I can quit.
}

fun cancellation2() = runBlocking {
    val startTime = System.currentTimeMillis()
    val job = launch(Dispatchers.Default) {
        var nextPrintTime = startTime
        var i = 0
        while (i < 5) { // computation loop, just wastes CPU
            // print a message twice a second
            if (System.currentTimeMillis() >= nextPrintTime) {
                println("job: I'm sleeping ${i++} ...")
                nextPrintTime += 500L
            }
        }
    }
    delay(1300L) // delay a bit
    println("main: I'm tired of waiting!")
    job.cancelAndJoin() // cancels the job and waits for its completion
    println("main: Now I can quit.")

//    job: I'm sleeping 0 ...
//    job: I'm sleeping 1 ...
//    job: I'm sleeping 2 ...
//    main: I'm tired of waiting!
//    job: I'm sleeping 3 ...
//    job: I'm sleeping 4 ...
//    main: Now I can quit.
}

fun cancellation3() = runBlocking {
    val job = launch(Dispatchers.Default) {
        repeat(5) { i ->
            try {
                // print a message twice a second
                println("job: I'm sleeping $i ...")
                delay(500)
            } catch (e: Exception) {
                // log the exception
                println(e)
            }
        }
    }
    delay(1300L) // delay a bit
    println("main: I'm tired of waiting!")
    job.cancelAndJoin() // cancels the job and waits for its completion
    println("main: Now I can quit.")

//    job: I'm sleeping 0 ...
//    job: I'm sleeping 1 ...
//    job: I'm sleeping 2 ...
//    main: I'm tired of waiting!
//    kotlinx.coroutines.JobCancellationException: StandaloneCoroutine was cancelled; job=StandaloneCoroutine{Cancelling}@6eaa8ced
//    job: I'm sleeping 3 ...
//    kotlinx.coroutines.JobCancellationException: StandaloneCoroutine was cancelled; job=StandaloneCoroutine{Cancelling}@6eaa8ced
//    job: I'm sleeping 4 ...
//    kotlinx.coroutines.JobCancellationException: StandaloneCoroutine was cancelled; job=StandaloneCoroutine{Cancelling}@6eaa8ced
//    main: Now I can quit.
}

fun cancellation4() = runBlocking {
    val startTime = System.currentTimeMillis()
    val job = launch(Dispatchers.Default) {
        var nextPrintTime = startTime
        var i = 0
        while (isActive) { // cancellable computation loop
            // print a message twice a second
            if (System.currentTimeMillis() >= nextPrintTime) {
                println("job: I'm sleeping ${i++} ...")
                nextPrintTime += 500L
            }
        }
    }
    delay(1300L) // delay a bit
    println("main: I'm tired of waiting!")
    job.cancelAndJoin() // cancels the job and waits for its completion
    println("main: Now I can quit.")

//    job: I'm sleeping 0 ...
//    job: I'm sleeping 1 ...
//    job: I'm sleeping 2 ...
//    main: I'm tired of waiting!
//    main: Now I can quit.
}

fun cancellation5() = runBlocking {
    val job = launch {
        try {
            repeat(1000) { i ->
                println("job: I'm sleeping $i ...")
                delay(500L)
            }
        } finally {
            println("job: I'm running finally")
        }
    }
    delay(1300L) // delay a bit
    println("main: I'm tired of waiting!")
    job.cancelAndJoin() // cancels the job and waits for its completion
    println("main: Now I can quit.")

//    job: I'm sleeping 0 ...
//    job: I'm sleeping 1 ...
//    job: I'm sleeping 2 ...
//    main: I'm tired of waiting!
//    job: I'm running finally
//    main: Now I can quit.
}

fun cancellation6() = runBlocking {
    val job = launch {
        try {
            repeat(1000) { i ->
                println("job: I'm sleeping $i ...")
                delay(500L)
            }
        } finally {
            withContext(NonCancellable) {
                println("job: I'm running finally")
                delay(1000L)
                println("job: And I've just delayed for 1 sec because I'm non-cancellable")
            }
        }
    }
    delay(1300L) // delay a bit
    println("main: I'm tired of waiting!")
    job.cancelAndJoin() // cancels the job and waits for its completion
    println("main: Now I can quit.")

//    job: I'm sleeping 0 ...
//    job: I'm sleeping 1 ...
//    job: I'm sleeping 2 ...
//    main: I'm tired of waiting!
//    job: I'm running finally
//    job: And I've just delayed for 1 sec because I'm non-cancellable
//    main: Now I can quit.
}
