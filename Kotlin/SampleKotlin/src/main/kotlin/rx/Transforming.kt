package rx

import io.reactivex.rxjava3.core.Observable
import io.reactivex.rxjava3.core.Single
import io.reactivex.rxjava3.functions.Function
import io.reactivex.rxjava3.subjects.ReplaySubject
import java.util.concurrent.TimeUnit


fun main(args: Array<String>) {
    exampleBufferByCount()
    exampleBufferByTime()
    exampleBufferByCountAndTime()
    exampleBufferWithSignal()
    exampleBufferOverlappingByCount()
    exampleBufferOverlappingByTime()
    exampleBufferOverlappingBySignal()

    exampleFlatMap()
    exampleFlatMapMultipleValues()
    exampleFlatMapNewType()
    exampleFlatMapNewTypeSingle()
    exampleFlatMapFilter()
    exampleFlatMapAsynchronous()
    exampleConcatMap()
    exampleSwitchMap()

    exampleFlatMapIterable()
    exampleFlatMapIterableWithSelector()
    exampleFlatMapLazyIterable()

    exampleGroupBy()

    exampleMap()
    exampleMap2()

    exampleCast()
    exampleCastFail()
    exampleTypeOf()

    exampleRunningSum()
    exampleRunningMin()

    exampleWindowParallel()
    exampleWindowByCount()
    exampleWindowByTime()
    exampleWindowBySignal()
}

private fun exampleBufferByCount() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.range(0, 10)
        .buffer(4)
        .dump()

    // [0, 1, 2, 3]
    // [4, 5, 6, 7]
    // [8, 9]
}

private fun exampleBufferByTime() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(100, TimeUnit.MILLISECONDS).take(10)
        .buffer(250, TimeUnit.MILLISECONDS)
        .dump()
    readLine()

    // [0, 1]
    // [2, 3]
    // [4, 5, 6]
    // [7, 8]
    // [9]
}

private fun exampleBufferByCountAndTime() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(100, TimeUnit.MILLISECONDS)
        .take(10)
        .buffer(250, TimeUnit.MILLISECONDS, 2)
        .dump()
    readLine()

    // [0, 1]
    // []
    // [2, 3]
    // []
    // [4, 5]
    // [6]
    // [7, 8]
    // []
    // [9]
}

private fun exampleBufferWithSignal() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(100, TimeUnit.MILLISECONDS).take(10)
        .buffer(Observable.interval(250, TimeUnit.MILLISECONDS))
        .dump()
    readLine()

    // [0, 1]
    // [2, 3]
    // [4, 5, 6]
    // [7, 8]
    // [9]
}

private fun exampleBufferOverlappingByCount() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.range(0, 10)
        .buffer(4, 3)
        .dump()

    // [0, 1, 2, 3]
    // [3, 4, 5, 6]
    // [6, 7, 8, 9]
    // [9]
}

private fun exampleBufferOverlappingByTime() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(100, TimeUnit.MILLISECONDS).take(10)
        .buffer(350, 200, TimeUnit.MILLISECONDS)
        .dump()
    readLine()

    // [0, 1, 2]
    // [2, 3, 4]
    // [3, 4, 5, 6]
    // [5, 6, 7, 8]
    // [7, 8, 9]
    // [9]
}

private fun exampleBufferOverlappingBySignal() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(100, TimeUnit.MILLISECONDS).take(10)
        .buffer<Long, Long>(
            Observable.interval(250, TimeUnit.MILLISECONDS),
            Function { i -> Observable.timer(200, TimeUnit.MILLISECONDS) } )
        .dump()
    readLine()

    // [2, 3]
    // [4, 5]
    // [7, 8]
    // [9]
}

private fun exampleFlatMap() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.just(2)
    values
        .flatMap { i -> Observable.range(0, i) }
        .dump()

    // flatMap: 0
    // flatMap: 1
    // flatMap: Completed
}

private fun exampleFlatMapMultipleValues() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(1, 3)
    values
        .flatMap { i -> Observable.range(0, i) }
        .dump()

    // flatMap: 0
    // flatMap: 0
    // flatMap: 1
    // flatMap: 0
    // flatMap: 1
    // flatMap: 2
    // flatMap: Completed
}

private fun exampleFlatMapNewType() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.just(1)
    values
        .flatMap { i ->
            Observable.just(
                Character.valueOf((i + 64).toChar())
            )
        }
        .dump()

    // flatMap: A
    // flatMap: Completed
}

private fun exampleFlatMapNewTypeSingle() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Single.just(1)
    values
        .flatMap { i ->
            Single.just(
                Character.valueOf((i + 64).toChar())
            )
        }
        .dump()

    // flatMap: A
}

private fun exampleFlatMapFilter() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(0, 30)
    values
        .flatMap<Char> { i ->
            if (i in 1..26)
                Observable.just(Character.valueOf((i + 64).toChar()))
            else
                Observable.empty()
        }
        .dump()

    // flatMap: A
    // flatMap: B
    // flatMap: C
    // ...
    // flatMap: X
    // flatMap: Y
    // flatMap: Z
    // flatMap: Completed
}

private fun exampleFlatMapAsynchronous() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.just(100, 150)
        .flatMap { i ->
            Observable.interval(i.toLong(), TimeUnit.MILLISECONDS)
                .map { v -> i }
        }
        .take(10)
        .dump()
    readLine()

    // flatMap: 100
    // flatMap: 150
    // flatMap: 100
    // flatMap: 100
    // flatMap: 150
    // flatMap: 100
    // flatMap: 150
    // flatMap: 100
    // flatMap: 100
    // flatMap: 150
    // flatMap: Completed
}

private fun exampleConcatMap() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.just(100, 150)
        .concatMap { i ->
            Observable.interval(i.toLong(), TimeUnit.MILLISECONDS)
                .map { v -> i }
                .take(3)
        }
        .dump()
    readLine()

    // 100
    // 100
    // 100
    // 150
    // 150
    // 150
    // Completed
}

private fun exampleSwitchMap() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(100, TimeUnit.MILLISECONDS)
        .switchMap { i ->
            Observable.interval(30, TimeUnit.MILLISECONDS)
                .map { l -> i }
        }
        .take(9)
        .dump()
    readLine()

    // 0
    // 0
    // 0
    // 1
    // 1
    // 1
    // 2
    // 2
    // 2
}

private fun exampleFlatMapIterable() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.range(1, 3)
        .flatMapIterable { i -> 1..i }
        .dump()

    // 1
    // 1
    // 2
    // 1
    // 2
    // 3
}

private fun exampleFlatMapIterableWithSelector() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.range(1, 3)
        .flatMapIterable<Int, Int>(
            { i -> 1..i },
            { ori, rv -> ori * rv })
        .dump()

    // 1
    // 2
    // 4
    // 3
    // 6
    // 9
}

private fun exampleFlatMapLazyIterable() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.range(1, 3)
        .flatMapIterable<Int, Int>(
            { i -> generateSequence(1) { (it + 1).takeIf { it <= i } }.asIterable() },
            { ori, rv -> ori * rv })
        .dump()

    // 1
    // 2
    // 4
    // 3
    // 6
    // 9
}

private fun exampleGroupBy() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.just(
        "first",
        "second",
        "third",
        "forth",
        "fifth",
        "sixth"
    )
    values.groupBy { word -> word[0] }
        .flatMap<Any> { group -> group.lastOrError().toObservable().map { v -> "${group.key}: $v" } }
        .dump()

    // s: sixth
    // t: third
    // f: fifth
}

private fun exampleMap() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(0, 4)
    values
        .map { i -> i + 3 }
        .dump()

    // Map: 3
    // Map: 4
    // Map: 5
    // Map: 6
    // Map: Completed
}

private fun exampleMap2() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.just("0", "1", "2", "3")
        .map { Integer.parseInt(it) }
    values.dump()

    // Map: 0
    // Map: 1
    // Map: 2
    // Map: 3
    // Map: Completed
}

private fun exampleCast() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.just<Int>(0, 1, 2, 3)
    values
        .cast(java.lang.Integer::class.java)
        .dump()

    // Map: 0
    // Map: 1
    // Map: 2
    // Map: 3
    // Map: Completed
}

private fun exampleCastFail() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.just(0, 1, 2, "3")
    values
        .cast(java.lang.Integer::class.java)
        .dump()

    // Map: 0
    // Map: 1
    // Map: 2
    // Map: Error: java.lang.ClassCastException: Cannot cast java.lang.String to java.lang.Integer
}

private fun exampleTypeOf() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.just(0, 1, "2", 3)
    values
        .ofType(java.lang.Integer::class.java)
        .dump()

    // Map: 0
    // Map: 1
    // Map: 3
    // Map: Completed
}


private fun exampleRunningSum() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(0, 5)
    values
        .scan { i1, i2 -> i1 + i2 }
        .dump()

    // Sum: 0
    // Sum: 1
    // Sum: 3
    // Sum: 6
    // Sum: 10
    // Sum: Completed
}

private fun exampleRunningMin() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = ReplaySubject.create<Int>()
    values
        .dump("Values")
    values
        .scan { i1, i2 -> if (i1 < i2) i1 else i2 }
        .distinctUntilChanged()
        .dump("Min")
    values.onNext(2)
    values.onNext(3)
    values.onNext(1)
    values.onNext(4)
    values.onComplete()

    // Values: 2
    // Min: 2
    // Values: 3
    // Values: 1
    // Min: 1
    // Values: 4
    // Values: Completed
    // Min: Completed
}

private fun exampleWindowParallel() {
    println(object{}.javaClass.enclosingMethod.name)
Observable
    .merge(
        Observable.range(0, 5)
            .window(3, 1))
    .dump()

    // 0
    // 1
    // 1
    // 2
    // 2
    // 2
    // 3
    // 3
    // 3
    // 4
    // 4
    // 4
}

private fun exampleWindowByCount() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.range(0, 5)
        .window(3, 1)
        .flatMap { o -> o.toList().toObservable() }
        .dump()

    // [0, 1, 2]
    // [1, 2, 3]
    // [2, 3, 4]
    // [3, 4]
    // [4]

}

private fun exampleWindowByTime() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(100, TimeUnit.MILLISECONDS)
        .take(5)
        .window(250, 100, TimeUnit.MILLISECONDS)
        .flatMap { o -> o.toList().toObservable() }
        .dump()
    readLine()

    // [0, 1]
    // [0, 1, 2]
    // [1, 2, 3]
    // [2, 3, 4]
    // [3, 4]
    // [4]
}

private fun exampleWindowBySignal() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(100, TimeUnit.MILLISECONDS)
        .take(5)
        .window<Long, Long>(
            Observable.interval(100, TimeUnit.MILLISECONDS),
            Function { _ -> Observable.timer(250, TimeUnit.MILLISECONDS) })
        .flatMap { o -> o.toList().toObservable() }
        .dump()
    readLine()

    // [1, 2]
    // [2, 3]
    // [3, 4]
    // [4]
    // []
}
