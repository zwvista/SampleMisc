package rx

import io.reactivex.Observable
import io.reactivex.functions.BiPredicate
import java.util.concurrent.TimeUnit


fun main(args: Array<String>) {
    exampleAmb()
    exampleAmbArray()
    exampleAmbWith()

    exampleAll()
    exampleAllEarlyFalse()
    exampleAllError()
    //exampleAllErrorAfterComplete()

    exampleContains()

    exampleDefaultIfEmpty()
    exampleDefaultIfEmptyError()

    exampleSequenceEqualTrue()
    exampleSequenceEqualFalse()
    exampleSequenceEqualError()

    exampleSkipUntil()
    exampleSkipWhile()
    exampleTakeUntil()
    exampleTakeWhile()
}

private fun exampleAmb() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.amb(listOf(
        Observable.timer(100, TimeUnit.MILLISECONDS).map { i -> "First" },
        Observable.timer(50, TimeUnit.MILLISECONDS).map { i -> "Second" },
        Observable.timer(70, TimeUnit.MILLISECONDS).map { i -> "Third" }))
        .dump()
    readLine()

    // Second
    // Completed
}

private fun exampleAmbArray() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.ambArray(
        Observable.timer(100, TimeUnit.MILLISECONDS).map { i -> "First" },
        Observable.timer(50, TimeUnit.MILLISECONDS).map { i -> "Second" },
        Observable.timer(70, TimeUnit.MILLISECONDS).map { i -> "Third" })
        .dump()
    readLine()

    // Second
    // Completed
}

private fun exampleAmbWith() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.timer(100, TimeUnit.MILLISECONDS).map { i -> "First" }
        .ambWith(Observable.timer(50, TimeUnit.MILLISECONDS).map { i -> "Second" })
        .ambWith(Observable.timer(70, TimeUnit.MILLISECONDS).map { i -> "Third" })
        .dump()
    readLine()

    // Second
    // Completed
}

private fun exampleAll() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.create<Int> { o ->
        o.onNext(0)
        o.onNext(10)
        o.onNext(10)
        o.onNext(2)
        o.onComplete()
    }
    values
        .all { i -> i % 2 == 0 }
        .dump()

    // true
    // Completed
}

private fun exampleAllEarlyFalse() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.interval(150, TimeUnit.MILLISECONDS).take(5)
    val subscription = values
        .all { i -> i < 3 }
        .dump()
    val subscription2 = values
        .dump()
    readLine()
    subscription.dispose()
    subscription2.dispose()

    // 0
    // 1
    // 2
    // All: false
    // All: Completed
    // 3
    // 4
    // Completed
}

private fun exampleAllError() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.create<Int> { o ->
        o.onNext(0)
        o.onNext(2)
        o.onError(Exception())
    }
    values
        .all { i -> i % 2 == 0 }
        .dump()

    // Error: java.lang.Exception
}

private fun exampleAllErrorAfterComplete() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.create<Int> { o ->
        o.onNext(1)
        o.onNext(2)
        o.onError(Exception())
    }
    values
        .all { i -> i % 2 == 0 }
        .dump()

    // false
    // Completed
}

private fun exampleContains() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.interval(100, TimeUnit.MILLISECONDS)
    val subscription = values
        .contains(4L)
        .dump()
    readLine()
    subscription.dispose()
    // true
    // Completed
}

private fun exampleDefaultIfEmpty() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.empty<Int>()
    values
        .defaultIfEmpty(2)
        .dump()

    // 2
    // Completed
}

private fun exampleDefaultIfEmptyError() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.error<Int>(Exception())

    values
        .defaultIfEmpty(2)
        .dump()

    // Error: java.lang.Exception
}

private fun exampleSequenceEqualTrue() {
    println(object{}.javaClass.enclosingMethod.name)
    val strings = Observable.just("1", "2", "3")
    val ints = Observable.just(1, 2, 3)
    Observable.sequenceEqual(strings, ints, BiPredicate { s, i -> s == i.toString() } )
        .dump()

    // true
}

private fun exampleSequenceEqualFalse() {
    println(object{}.javaClass.enclosingMethod.name)
    val strings = Observable.just("1", "2", "3")
    val ints = Observable.just(1, 2, 3)
    Observable.sequenceEqual(strings, ints)
        .dump()

    // false
}

private fun exampleSequenceEqualError() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.create<Int> { o ->
        o.onNext(1)
        o.onNext(2)
        o.onError(Exception())
    }
    Observable.sequenceEqual(values, values)
        .dump()

    // Error: java.lang.Exception
}

private fun exampleSkipUntil() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.interval(100, TimeUnit.MILLISECONDS)
    val cutoff = Observable.timer(250, TimeUnit.MILLISECONDS)
    val subscription = values
        .skipUntil(cutoff)
        .dump()
    readLine()
    subscription.dispose()

    // 2
    // 3
    // 4
    // ...
}

private fun exampleSkipWhile() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.interval(100, TimeUnit.MILLISECONDS)
    val subscription = values
        .skipWhile { v -> v < 2 }
        .dump()
    readLine()
    subscription.dispose()

    // 2
    // 3
    // 4
    // ...
}

private fun exampleTakeUntil() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.interval(100, TimeUnit.MILLISECONDS)
    val cutoff = Observable.timer(250, TimeUnit.MILLISECONDS)
    val subscription = values
        .takeUntil(cutoff)
        .dump()
    readLine()
    subscription.dispose()

    // 0
    // 1
    // Completed
}

private fun exampleTakeWhile() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.interval(100, TimeUnit.MILLISECONDS)
    val subscription = values
        .takeWhile { v -> v < 2 }
        .dump()
    readLine()
    subscription.dispose()

    // 0
    // 1
    // Completed
}
