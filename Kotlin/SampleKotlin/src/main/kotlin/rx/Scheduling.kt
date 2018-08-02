package rx

import io.reactivex.Observable
import io.reactivex.schedulers.Schedulers
import io.reactivex.subjects.BehaviorSubject
import java.util.*
import java.util.concurrent.TimeUnit


fun main(args: Array<String>) {
//    exampleSingleThreaded()
//
//    exampleBlocking()
//    exampleSubscribeOn()
//    exampleIntervalThread()
//
//    exampleObserveOn()
//    exampleObserveOnBeforeAfter()
//
//    exampleUnsubscribeOn()
//
//    exampleSchedule()
//    exampleScheduleFuture()
//    exampleCancelWork()
//    exampleCancelWithInterrupt()
    
    exampleTrampoline()
    exampleNewThread()
}

private fun exampleSingleThreaded() {
    println(object{}.javaClass.enclosingMethod.name)
    val subject = BehaviorSubject.create<Int>()
    subject.subscribe { i -> println("Received $i on ${Thread.currentThread().id}") }
    val i = intArrayOf(1) // naughty side-effects for examples only ;)
    val r = Runnable {
        synchronized(i) {
            println("onNext(${i[0]}) on ${Thread.currentThread().id}")
            subject.onNext(i[0]++)
        }
    }
    r.run() // Execute on main thread
    Thread(r).start()
    Thread(r).start()

    // onNext(1) on 1
    // Received 1 on 1
    // onNext(2) on 11
    // Received 2 on 11
    // onNext(3) on 12
    // Received 3 on 12
}


private fun exampleBlocking() {
    println(object{}.javaClass.enclosingMethod.name)
    println("Main: ${Thread.currentThread().id}")
    Observable.create<Int> { o ->
        println("Created on ${Thread.currentThread().id}")
        o.onNext(1)
        o.onNext(2)
        o.onComplete()
    }
        .subscribe { i -> println("Received $i on ${Thread.currentThread().id}") }
    println("Finished main: ${Thread.currentThread().id}")

    // Main: 1
    // Created on 1
    // Received 1 on 1
    // Received 2 on 1
    // Finished main: 1
}

private fun exampleSubscribeOn() {
    println(object{}.javaClass.enclosingMethod.name)
    println("Main: ${Thread.currentThread().id}")
    Observable.create<Int> { o ->
        println("Created on ${Thread.currentThread().id}")
        o.onNext(1)
        o.onNext(2)
        o.onComplete()
    }
        .subscribeOn(Schedulers.newThread())
        .subscribe { i -> println("Received $i on ${Thread.currentThread().id}") }
    println("Finished main: ${Thread.currentThread().id}")

    // Main: 1
    // Created on 1
    // Received 1 on 11
    // Received 2 on 11
    // Finished main: 11
}

private fun exampleIntervalThread() {
    println(object{}.javaClass.enclosingMethod.name)
    println("Main: ${Thread.currentThread().id}")
    val s = Observable.interval(100, TimeUnit.MILLISECONDS)
        .subscribe { i -> println("Received $i on ${Thread.currentThread().id}") }
    println("Finished main: ${Thread.currentThread().id}")
    readLine()
    s.dispose()

    // Main: 1
    // Finished main: 1
    // Received 0 on 11
    // Received 1 on 11
    // Received 2 on 11
}


private fun exampleObserveOn() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.create<Int> { o ->
        println("Created on ${Thread.currentThread().id}")
        o.onNext(1)
        o.onNext(2)
        o.onComplete()
    }
        .observeOn(Schedulers.newThread())
        .subscribe { i -> println("Received $i on ${Thread.currentThread().id}") }

    // Created on 1
    // Received 1 on 13
    // Received 2 on 13
}

private fun exampleObserveOnBeforeAfter() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.create<Int> { o ->
        println("Created on " + Thread.currentThread().id)
        o.onNext(1)
        o.onNext(2)
        o.onComplete()
    }
        .doOnNext { i -> println("Before $i on ${Thread.currentThread().id}") }
        .observeOn(Schedulers.newThread())
        .doOnNext { i -> println("After $i on ${Thread.currentThread().id}") }
        .subscribe()

    // Created on 1
    // Before 1 on 1
    // Before 2 on 1
    // After 1 on 13
    // After 2 on 13
}

private fun exampleUnsubscribeOn() {
    println(object{}.javaClass.enclosingMethod.name)
    val source = Observable.using<Int, List<Int>>(
        {
            println("Subscribed on ${Thread.currentThread().id}")
            Arrays.asList(1, 2)
        },
        { ints ->
            println("Producing on ${Thread.currentThread().id}")
            Observable.fromIterable(ints)
        },
        { ints -> println("Unsubscribed on ${Thread.currentThread().id}") }
    )
    source
        .unsubscribeOn(Schedulers.newThread())
        .subscribe { println(it) }

    // Subscribed on 1
    // Producing on 1
    // 1
    // 2
    // Unubscribed on 11
}

private fun exampleSchedule() {
    println(object{}.javaClass.enclosingMethod.name)
    val scheduler = Schedulers.trampoline()
    val worker = scheduler.createWorker()
    worker.schedule(
        { println("Action") })
}

private fun exampleScheduleFuture() {
    println(object{}.javaClass.enclosingMethod.name)
    val scheduler = Schedulers.newThread()
    val start = System.currentTimeMillis()
    val worker = scheduler.createWorker()
    worker.schedule(
        { println(System.currentTimeMillis() - start) },
        5, TimeUnit.SECONDS)
    worker.schedule(
        { println(System.currentTimeMillis() - start) },
        5, TimeUnit.SECONDS)
    readLine()

    // 5033
    // 5035
}

private fun exampleCancelWork() {
    println(object{}.javaClass.enclosingMethod.name)
    val scheduler = Schedulers.newThread()
    val start = System.currentTimeMillis()
    val worker = scheduler.createWorker()
    worker.schedule(
        {
            println(System.currentTimeMillis() - start)
            worker.dispose()
        },
        5, TimeUnit.SECONDS)
    worker.schedule(
        { println(System.currentTimeMillis() - start) },
        5, TimeUnit.SECONDS)
    readLine()

    // 5032
}

private fun exampleCancelWithInterrupt() {
    println(object{}.javaClass.enclosingMethod.name)
    val scheduler = Schedulers.newThread()
    val worker = scheduler.createWorker()
    worker.schedule {
        try {
            Thread.sleep(2000)
            println("Action completed")
        } catch (e: InterruptedException) {
            println("Action interrupted")
        }
    }
    Thread.sleep(500)
    worker.dispose()
    readLine()

    // Action interrupted
}

private fun exampleTrampoline() {
    println(object{}.javaClass.enclosingMethod.name)
    val scheduler = Schedulers.trampoline()
    val worker = scheduler.createWorker()
    worker.schedule {
        println("Start")
        worker.schedule { println("Inner") }
        println("End")
    }

    // Start
    // End
    // Inner
}

private fun printThread(message: String) {
    println("$message on ${Thread.currentThread().id}")
}

private fun exampleNewThread() {
    println(object{}.javaClass.enclosingMethod.name)
    printThread("Main")
    val scheduler = Schedulers.newThread()
    val worker = scheduler.createWorker()
    worker.schedule {
        printThread("Start")
        worker.schedule { printThread("Inner") }
        printThread("End")
    }
    Thread.sleep(500)
    worker.schedule { printThread("Again") }
    readLine()

    // Main on 1
    // Start on 11
    // End on 11
    // Inner on 11
    // Again on 11
}
