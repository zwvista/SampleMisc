package rx

import io.reactivex.rxjava3.core.Observable
import io.reactivex.rxjava3.core.Single
import java.util.concurrent.TimeUnit


fun main(args: Array<String>) {
    exampleCombineLatest()
    exampleWithLatestFrom()

    exampleJoinSimple()
    exampleJoin2Way()
    exampleGroupJoin()

    exampleMerge()
    exampleMergeSingle()
    exampleMergeWith()
    exampleMergeWithSingle()
    exampleMergeDelayError1()
    exampleMergeDelayError2()

    exampleStartWith()
    exampleSwitchOnNext()

    exampleZip()
    exampleZipSingle()
    exampleZipMultiple()
    exampleZipMultipleSingle()
    exampleZipUneven()
    exampleZipWith()
    exampleZipWithIterable()
}

private fun exampleCombineLatest() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.combineLatest(
        Observable.interval(100, TimeUnit.MILLISECONDS)
            .doOnNext { i -> println("Left emits") },
        Observable.interval(150, TimeUnit.MILLISECONDS)
            .doOnNext { i -> println("Right emits") })
        { i1, i2 -> "$i1 - $i2" }
        .take(6)
        .dump()
    readLine()


    // Left emits
    // Right emits
    // 0 - 0
    // Left emits
    // 1 - 0
    // Left emits
    // 2 - 0
    // Right emits
    // 2 - 1
    // Left emits
    // 3 - 1
    // Right emits
    // 3 - 2
}

private fun exampleWithLatestFrom() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(100, TimeUnit.MILLISECONDS)
        .doOnNext { i -> println("Left emits") }.withLatestFrom(
        Observable.interval(150, TimeUnit.MILLISECONDS)
            .doOnNext { i -> println("Right emits") })
        { i1, i2 -> "$i1 - $i2" }
        .take(6)
        .dump()
    readLine()


    // Left emits
    // Right emits
    // Left emits
    // 1 - 0
    // Left emits
    // Right emits
    // 2 - 0
    // Left emits
    // 3 - 1
    // Right emits
    // Left emits
    // 4 - 2
    // Right emits
    // Left emits
    // 5 - 3
    // Left emits
    // 6 - 3
}

private fun exampleJoinSimple() {
    println(object{}.javaClass.enclosingMethod.name)
    val left = Observable.interval(100, TimeUnit.MILLISECONDS)
        .map { i -> "L$i" }
    val right = Observable.interval(200, TimeUnit.MILLISECONDS)
        .map { i -> "R$i" }
    left
        .join<String, Any, Long, String>(
            right,
            { i -> Observable.never() },
            { i -> Observable.timer(0, TimeUnit.MILLISECONDS) },
            { l, r -> "$l - $r" }
        )
        .take(10)
        .dump()
    readLine()

    // L0 - R0
    // L1 - R0
    // L0 - R1
    // L1 - R1
    // L2 - R1
    // L3 - R1
    // L0 - R2
    // L1 - R2
    // L2 - R2
    // L3 - R2
}

private fun exampleJoin2Way() {
    println(object{}.javaClass.enclosingMethod.name)
    val left = Observable.interval(100, TimeUnit.MILLISECONDS)
        .map { i -> "L$i" }
    val right = Observable.interval(100, TimeUnit.MILLISECONDS)
        .map { i -> "R$i" }
    left
        .join(
            right,
            { i -> Observable.timer(150, TimeUnit.MILLISECONDS) },
            { i -> Observable.timer(0, TimeUnit.MILLISECONDS) },
            { l, r -> "$l - $r" }
        )
        .take(10)
        .dump()
    readLine()

    // L0 - R0
    // L0 - R1
    // L1 - R1
    // L1 - R2
    // L2 - R2
    // L2 - R3
    // L3 - R3
    // L3 - R4
    // L4 - R4
    // L4 - R5
}

private fun exampleGroupJoin() {
    println(object{}.javaClass.enclosingMethod.name)
    val left = Observable.interval(100, TimeUnit.MILLISECONDS)
        .map { i -> "L$i" }
        .take(6)
    val right = Observable.interval(200, TimeUnit.MILLISECONDS)
        .map { i -> "R$i" }
        .take(3)
    left
        .groupJoin<String, Any, Long, Any>(
            right,
            { i -> Observable.never<Any>() },
            { i -> Observable.timer(0, TimeUnit.MILLISECONDS) },
            { l, rs -> rs.toList().subscribe { list -> println("$l: $list") } }
        )
        .subscribe()
    readLine()

    // L0: [R0, R1, R2]
    // L1: [R0, R1, R2]
    // L2: [R1, R2]
    // L3: [R1, R2]
    // L4: [R2]
    // L5: [R2]
}

private fun exampleMerge() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.merge(
        Observable.interval(250, TimeUnit.MILLISECONDS).map { "First" },
        Observable.interval(150, TimeUnit.MILLISECONDS).map { "Second" })
        .take(10)
        .dump()
    readLine()

    // Second
    // First
    // Second
    // Second
    // First
    // Second
    // Second
    // First
    // Second
    // First
}


private fun exampleMergeSingle() {
    println(object{}.javaClass.enclosingMethod.name)
    Single.merge(
        Single.timer(250, TimeUnit.MILLISECONDS).map { "First" },
        Single.timer(150, TimeUnit.MILLISECONDS).map { "Second" })
        .dump()
    readLine()

    // Second
    // First
}

private fun exampleMergeWith() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(250, TimeUnit.MILLISECONDS).map { "First" }
        .mergeWith(Observable.interval(150, TimeUnit.MILLISECONDS).map { "Second" })
        .take(10)
        .dump()
    readLine()

    // Second
    // First
    // Second
    // Second
    // First
    // Second
    // First
    // Second
    // Second
    // First
}

private fun exampleMergeWithSingle() {
    println(object{}.javaClass.enclosingMethod.name)
    Single.timer(250, TimeUnit.MILLISECONDS).map { "First" }
        .mergeWith(Single.timer(150, TimeUnit.MILLISECONDS).map { "Second" })
        .dump()
    readLine()

    // Second
    // First
}

private fun exampleMergeDelayError1() {
    println(object{}.javaClass.enclosingMethod.name)
    val failAt200 = Observable.concat(
        Observable.interval(100, TimeUnit.MILLISECONDS).take(2),
        Observable.error(Exception("Failed")))
    val completeAt400 = Observable.interval(100, TimeUnit.MILLISECONDS)
        .take(4)
    Observable.mergeDelayError(failAt200, completeAt400)
        .dump()
    readLine()

    // 0
    // 0
    // 1
    // 1
    // 2
    // 3
    // java.lang.Exception: Failed
}

private fun exampleMergeDelayError2() {
    println(object{}.javaClass.enclosingMethod.name)
    val failAt200 = Observable.concat(
        Observable.interval(100, TimeUnit.MILLISECONDS).take(2),
        Observable.error(Exception("Failed")))
    val failAt300 = Observable.concat(
        Observable.interval(100, TimeUnit.MILLISECONDS).take(3),
        Observable.error(Exception("Failed")))
    val completeAt400 = Observable.interval(100, TimeUnit.MILLISECONDS)
        .take(4)
    Observable.mergeDelayError(failAt200, failAt300, completeAt400)
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
    // 3
    // rx.exceptions.CompositeException: 2 exceptions occurred.
}

private fun exampleStartWith() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(0, 3)
    values.startWithIterable(listOf(-1, -2))
        .dump()

    // -1
    // -2
    // 0
    // 1
    // 2
}

private fun exampleSwitchOnNext() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.switchOnNext(
        Observable.interval(100, TimeUnit.MILLISECONDS)
            .map { i ->
                Observable.interval(30, TimeUnit.MILLISECONDS)
                    .map { _ -> i }
            }
    )
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

private fun exampleZip() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.zip(
        Observable.interval(100, TimeUnit.MILLISECONDS)
            .doOnNext { i -> println("Left emits $i") },
        Observable.interval(150, TimeUnit.MILLISECONDS)
            .doOnNext { i -> println("Right emits $i") })
        { i1, i2 -> "$i1 - $i2" }
        .take(6)
        .dump()
    readLine()

    // Left emits
    // Right emits
    // 0 - 0
    // Left emits
    // Right emits
    // Left emits
    // 1 - 1
    // Left emits
    // Right emits
    // 2 - 2
    // Left emits
    // Left emits
    // Right emits
    // 3 - 3
    // Left emits
    // Right emits
    // 4 - 4
    // Left emits
    // Right emits
    // Left emits
    // 5 - 5
}

private fun exampleZipSingle() {
    println(object{}.javaClass.enclosingMethod.name)
    Single.zip(
        Single.timer(100, TimeUnit.MILLISECONDS)
            .doAfterSuccess { i -> println("Left emits $i") },
        Single.timer(150, TimeUnit.MILLISECONDS)
            .doAfterSuccess { i -> println("Right emits $i") })
        { i1, i2 -> "$i1 - $i2" }
        .dump()
    readLine()

    // Left emits
    // 0 - 0
    // Right emits
}

private fun exampleZipMultiple() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.zip(
        Observable.interval(100, TimeUnit.MILLISECONDS),
        Observable.interval(150, TimeUnit.MILLISECONDS),
        Observable.interval(40, TimeUnit.MILLISECONDS))
        { i1, i2, i3 -> "$i1 - $i2 - $i3" }
        .take(6)
        .dump()
    readLine()

    // 0 - 0 - 0
    // 1 - 1 - 1
    // 2 - 2 - 2
    // 3 - 3 - 3
    // 4 - 4 - 4
    // 5 - 5 - 5
}

private fun exampleZipMultipleSingle() {
    println(object{}.javaClass.enclosingMethod.name)
    Single.zip(
        Single.timer(100, TimeUnit.MILLISECONDS),
        Single.timer(150, TimeUnit.MILLISECONDS),
        Single.timer(40, TimeUnit.MILLISECONDS))
        { i1, i2, i3 -> "$i1 - $i2 - $i3" }
        .dump()
    readLine()

    // 0 - 0 - 0
}

private fun exampleZipUneven() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.zip(
        Observable.range(0, 5),
        Observable.range(0, 3),
        Observable.range(0, 8))
        { i1, i2, i3 -> "$i1 - $i2 - $i3" }
        .count()
        .dump()
    readLine()

    // 3
}

private fun exampleZipWith() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(100, TimeUnit.MILLISECONDS)
        .zipWith(Observable.interval(150, TimeUnit.MILLISECONDS))
        { i1, i2 -> "$i1 - $i2" }
        .take(6)
        .dump()
    readLine()

    // 0 - 0
    // 1 - 1
    // 2 - 2
    // 3 - 3
    // 4 - 4
    // 5 - 5
}

private fun exampleZipWithIterable() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.range(0, 5)
        .zipWith(listOf(0, 2, 4, 6, 8))
        { i1, i2 -> "$i1 - $i2" }
        .dump()
    readLine()

    // 0 - 0
    // 1 - 2
    // 2 - 4
    // 3 - 6
    // 4 - 8
}

