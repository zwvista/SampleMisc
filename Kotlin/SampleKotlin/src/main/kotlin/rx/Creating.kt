package rx

import io.reactivex.rxjava3.core.Observable
import io.reactivex.rxjava3.core.Single
import io.reactivex.rxjava3.functions.BiFunction
import java.util.*
import java.util.concurrent.FutureTask
import java.util.concurrent.TimeUnit


fun main(args: Array<String>) {
    exampleCreate()
    exampleGenerate()

    exampleShouldDefer()
    exampleDefer()

    exampleEmpty()
    exampleNever()
    exampleError()
    exampleErrorSingle()

    exampleFromFuture()
    exampleFromFutureTimeout()
    exampleFromArray()
    exampleFromIterable()
    exampleFromCallable()

    exampleInterval()
    exampleJust()
    exampleJustSingle()
    exampleRange()
    exampleRepeat()
    exampleRepeat2()
    exampleRepeatWhen2()
    exampleRepeatWithInterval()
    exampleTimer()
}

private fun exampleCreate() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.create<String> { o ->
        o.onNext("Hello")
        o.onComplete()
    }
    values.dump()

    // Received: Hello
    // Completed
}

private fun exampleGenerate() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.generate<Int, Int>( { 0 }, BiFunction { i, o ->
        if (i < 10) {
            o.onNext(i * i); i + 1
        } else {
            o.onComplete(); i
        }
    }, { i -> println(i) } )
    values.dump()

    // 0
    // 1
    // 4
    // 9
    // ...
    // 81
    // Completed
    // 10
}

private fun exampleShouldDefer() {
    println(object{}.javaClass.enclosingMethod.name)
//    val now = Observable.just(System.currentTimeMillis())
    val now = Single.just(System.currentTimeMillis())
    now.dump()
    Thread.sleep(1000)
    now.dump()

    // 1431443908375
    // 1431443908375
}

private fun exampleDefer() {
    println(object{}.javaClass.enclosingMethod.name)
//    val now = Observable.defer { Observable.just(System.currentTimeMillis()) }
    val now = Single.defer { Single.just(System.currentTimeMillis()) }
    now.dump()
    Thread.sleep(1000)
    now.dump()

    // 1431444107854
    // 1431444108858
}

private fun exampleEmpty() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.empty<String>()
    values.dump()

    // Completed
}

private fun exampleNever() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.never<String>()
    values.dump()
}

private fun exampleError() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.error<String>(Exception("Oops"))
    values.dump()

    // Error: java.lang.Exception: Oops
}

private fun exampleErrorSingle() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Single.error<String>(Exception("Oops"))
    values.dump()

    // Error: java.lang.Exception: Oops
}

private fun exampleFromFuture() {
    println(object{}.javaClass.enclosingMethod.name)
    val f = FutureTask {
        Thread.sleep(2000)
        21
    }
    Thread(f).start()
//    val values = Observable.fromFuture(f)
    val values = Single.fromFuture(f)
    values.dump()
    readLine()

    // Received: 21
    // Completed
}

private fun exampleFromFutureTimeout() {
    println(object{}.javaClass.enclosingMethod.name)
    val f = FutureTask {
        Thread.sleep(2000)
        21
    }
    Thread(f).start()
//    val values = Observable.fromFuture(f, 1000, TimeUnit.MILLISECONDS)
    val values = Single.fromFuture(f, 1000, TimeUnit.MILLISECONDS)
    values.dump()
    readLine()

    // Error: java.util.concurrent.TimeoutException
}

private fun exampleFromArray() {
    println(object{}.javaClass.enclosingMethod.name)
    val `is` = arrayOf(1, 2, 3)
    val values = Observable.fromArray(*`is`)
    // val values = `is`.toObservable()
    values.dump()

    // Received: 1
    // Received: 2
    // Received: 3
    // Completed
}

private fun exampleFromCallable() {
    println(object{}.javaClass.enclosingMethod.name)
//    val now = Observable.fromCallable { System.currentTimeMillis() }
    val now = Single.fromCallable { System.currentTimeMillis() }
    now.dump()
    Thread.sleep(1000)
    now.dump()

    // 1431444107854
    // 1431444108858
}

private fun exampleFromIterable() {
    println(object{}.javaClass.enclosingMethod.name)
    val input = Arrays.asList(1, 2, 3)
    val values = Observable.fromIterable(input)
    // val values = input.toObservable()
    values.dump()

    // Received: 1
    // Received: 2
    // Received: 3
    // Completed
}

private fun exampleInterval() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.interval(1000, TimeUnit.MILLISECONDS)
    val s = values.dump()
    readLine()
    s.dispose()

    // Received: 0
    // Received: 1
    // Received: 2
    // Received: 3
    // ...
}

private fun exampleJust() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.just("one", "two", "three")
    values.dump()

    // Received: one
    // Received: two
    // Received: three
    // Completed
}

private fun exampleJustSingle() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Single.just("one")
    values.dump()

    // Received: one
}

private fun exampleRange() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(10, 15)
    values.dump()

    // 10
    // ...
    // 24
}

private fun exampleRepeat() {
    println(object{}.javaClass.enclosingMethod.name)
    val words = Observable.range(0, 2)
    words.repeat()
        .take(4)
        .dump()

    // 0
    // 1
    // 0
    // 1
}

private fun exampleRepeat2() {
    println(object{}.javaClass.enclosingMethod.name)
    val words = Observable.range(0, 2)
    words.repeat(2)
        .dump()

    // 0
    // 1
    // 0
    // 1
}

private fun exampleRepeatWhen2() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.interval(100, TimeUnit.MILLISECONDS)
    values
        .take(2)
        .repeatWhen { ob -> ob.take(2) }
        .dump()
    readLine()

    // repeatWhen: 0
    // repeatWhen: 1
    // repeatWhen: 0
    // repeatWhen: 1
    // repeatWhen: Completed
}

private fun exampleRepeatWithInterval() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.interval(100, TimeUnit.MILLISECONDS)
    values
        .take(5) // Numbers 0 to 4
        .repeatWhen { ob ->
            ob.subscribe()
            Observable.interval(2, TimeUnit.SECONDS)
        } // Repeat 0 to 4 every 2s, forever
        .take(2) // Stop after second repetition
        .dump()
    readLine()

    // repeatWhen: 0
    // repeatWhen: 1
    // repeatWhen: 2
    // repeatWhen: 3
    // repeatWhen: 4
    // repeatWhen: 0
    // repeatWhen: 1
    // repeatWhen: 2
    // repeatWhen: 3
    // repeatWhen: 4
}

private fun exampleTimer() {
    println(object{}.javaClass.enclosingMethod.name)
//    val values = Observable.timer(1, TimeUnit.SECONDS)
    val values = Single.timer(1, TimeUnit.SECONDS)
    values.dump()
    readLine()

    // Received: 0
    // Completed
}
