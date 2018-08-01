package rx

import io.reactivex.Observable
import java.util.concurrent.TimeUnit


fun main(args: Array<String>) {
    exampleDebounce()
    exampleDebounceDynamic()
    exampleThrottleWithTimeout()
    exampleThrottleFirst()
    exampleThrottleLast()

    exampleDistinct()
    exampleDistinctKey()
    exampleDistinctUntilChanged()
    exampleDistinctUntilChangedKey()

    exampleElementAt()
    exampleElementAtOrDefault()

    exampleFilter()

    exampleFirst()
    exampleFirstOrDefault()
    exampleSingle()
    exampleSingleOrDefault()

    exampleIgnoreElements()

    exampleLast()
    exampleLastOrDefault()

    exampleSample()

    exampleSkip()
    exampleSkipTime()
    exampleSkipLast()

    exampleTake()
    exampleTakeTime()
    exampleTakeLast()
}

private fun exampleDebounce() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.concat(
        Observable.interval(100, TimeUnit.MILLISECONDS).take(3),
        Observable.interval(500, TimeUnit.MILLISECONDS).take(3),
        Observable.interval(100, TimeUnit.MILLISECONDS).take(3))
        .scan(0) { acc, v -> acc + 1 }
        .debounce(150, TimeUnit.MILLISECONDS)
        .dump()
    readLine()

    // 3
    // 4
    // 5
    // 9
}

private fun exampleDebounceDynamic() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.concat(
        Observable.interval(100, TimeUnit.MILLISECONDS).take(3),
        Observable.interval(500, TimeUnit.MILLISECONDS).take(3),
        Observable.interval(100, TimeUnit.MILLISECONDS).take(3))
        .scan(0) { acc, v -> acc + 1 }
        .debounce { i -> Observable.timer(i * 50L, TimeUnit.MILLISECONDS) }
        .dump()
    readLine()

    // 1
    // 3
    // 4
    // 5
    // 9
}

private fun exampleThrottleWithTimeout() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.concat(
        Observable.interval(100, TimeUnit.MILLISECONDS).take(3),
        Observable.interval(500, TimeUnit.MILLISECONDS).take(3),
        Observable.interval(100, TimeUnit.MILLISECONDS).take(3))
        .scan(0) { acc, v -> acc + 1 }
        .throttleWithTimeout(150, TimeUnit.MILLISECONDS)
        .dump()
    readLine()

    // 3
    // 4
    // 5
    // 9
}

private fun exampleThrottleFirst() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(150, TimeUnit.MILLISECONDS)
        .throttleFirst(1, TimeUnit.SECONDS)
        .take(3)
        .dump()
    readLine()

    // 0
    // 7
    // 14
}

private fun exampleThrottleLast() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(150, TimeUnit.MILLISECONDS)
        .throttleLast(1, TimeUnit.SECONDS)
        .take(3)
        .dump()
    readLine()

    // 5
    // 12
    // 18
}

private fun exampleDistinct() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.create<Int> { o ->
        o.onNext(1)
        o.onNext(1)
        o.onNext(2)
        o.onNext(3)
        o.onNext(2)
        o.onComplete()
    }
    values
        .distinct()
        .dump()

    // 1
    // 2
    // 3
    // Completed
}

private fun exampleDistinctKey() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.create<String> { o ->
        o.onNext("First")
        o.onNext("Second")
        o.onNext("Third")
        o.onNext("Fourth")
        o.onNext("Fifth")
        o.onComplete()
    }
    values
        .distinct { v -> v[0] }
        .dump()

    // First
    // Second
    // Third
    // Completed
}

private fun exampleDistinctUntilChanged() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.create<Int> { o ->
        o.onNext(1)
        o.onNext(1)
        o.onNext(2)
        o.onNext(3)
        o.onNext(2)
        o.onComplete()
    }
    values
        .distinctUntilChanged()
        .dump()

    // 1
    // 2
    // 3
    // 2
    // Completed
}

private fun exampleDistinctUntilChangedKey() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.create<String> { o ->
        o.onNext("First")
        o.onNext("Second")
        o.onNext("Third")
        o.onNext("Fourth")
        o.onNext("Fifth")
        o.onComplete()
    }
    values
        .distinctUntilChanged { v -> v[0] }
        .dump()

    // First
    // Second
    // Third
    // Fourth
    // Completed
}

private fun exampleElementAt() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(100, 10)
    values
        .elementAt(2)
        .dump()

    // 102
    // Completed
}

private fun exampleElementAtOrDefault() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(100, 10)
    values
        .elementAt(22, 0)
        .dump()

    // 0
    // Completed
}

private fun exampleFilter() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(0, 10)
    values
        .filter { v -> v % 2 == 0 }
        .dump()

    // 0
    // 2
    // 4
    // 6
    // 8
    // Completed
}

private fun exampleFirst() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.interval(100, TimeUnit.MILLISECONDS)
    values
        .firstOrError()
        .dump()
    values
        .filter { v -> v > 5 }
        .firstElement()
        .dump()
    readLine()

    // 0
}

private fun exampleFirstOrDefault() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.empty<Long>()
    values
        .filter { v -> v > 5 }
        .first(-1L)
        .dump()
    // -1
}

private fun exampleSingle() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.interval(100, TimeUnit.MILLISECONDS)
    values.take(10)
        .filter { v -> v == 5L } // Emits a result
        .singleOrError()
        .dump()
    values
        .filter { v -> v == 5L } // Never emits
        .singleElement()
        .dump()
    readLine()

    // Single1: 5
    // Single1: Completed
}

private fun exampleSingleOrDefault() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.empty<Int>()
    values
        .single(-1)
        .dump()

    // SingleOrDefault: -1
    // SingleOrDefault: Completed
}

private fun exampleIgnoreElements() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(0, 10)
    values
        .ignoreElements()
        .dump()

    // Completed
}

private fun exampleLast() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(0, 10)
    values
        .lastOrError()
        .dump()
    values
        .filter { v -> v < 5 }
        .lastElement()
        .dump()

    // 9
    // 4
}

private fun exampleLastOrDefault() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.empty<Int>()
    values
        .filter { v -> v > 5 }
        .last(-1)
        .dump()

    // -1
}

private fun exampleSample() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(150, TimeUnit.MILLISECONDS)
        .sample(1, TimeUnit.SECONDS)
        .take(3)
        .dump()
    readLine()

    // 5
    // 12
    // 18
}

private fun exampleSkip() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(0, 5)
    values
        .skip(2)
        .dump()

    // 2
    // 3
    // 4
    // Completed
}

private fun exampleSkipTime() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.interval(100, TimeUnit.MILLISECONDS)
    val d = values
        .skip(250, TimeUnit.MILLISECONDS)
        .dump()
    readLine()
    d.dispose()

    // 2
    // 3
    // 4
    // Completed
}

private fun exampleSkipLast() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(0, 5)
    values
        .skipLast(2)
        .dump()

    // 0
    // 1
    // 2
    // Completed
}

private fun exampleTake() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(0, 5)
    values
        .take(2)
        .dump()

    // 0
    // 1
    // Completed
}

private fun exampleTakeTime() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.interval(100, TimeUnit.MILLISECONDS)
    values
        .take(250, TimeUnit.MILLISECONDS)
        .dump()
    readLine()

    // 0
    // 1
    // Completed
}

private fun exampleTakeLast() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(0, 5)
    values
        .takeLast(2)
        .dump()

    // 3
    // 4
    // ...
}
