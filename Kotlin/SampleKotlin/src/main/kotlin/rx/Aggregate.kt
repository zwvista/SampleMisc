package rx

import io.reactivex.rxjava3.core.Observable


fun main(args: Array<String>) {
    exampleConcat()
    exampleConcatDynamic()
    exampleConcatWith()

    exampleCount()

    exampleReduce()
    exampleReduceWithAccumulator()
    exampleCollect()
}

private fun exampleConcat() {
    println(object{}.javaClass.enclosingMethod.name)
    val seq1 = Observable.range(0, 3)
    val seq2 = Observable.range(10, 3)
    Observable.concat(seq1, seq2)
        .dump()

    // 0
    // 1
    // 2
    // 10
    // 11
    // 12
}

private fun exampleConcatDynamic() {
    println(object{}.javaClass.enclosingMethod.name)
    val words = Observable.just(
        "First",
        "Second",
        "Third",
        "Fourth",
        "Fifth",
        "Sixth"
    )
    Observable.concat(words.groupBy { v -> v[0] })
        .dump()

    // First
    // Fourth
    // Fifth
    // Second
    // Sixth
    // Third
}

private fun exampleConcatWith() {
    println(object{}.javaClass.enclosingMethod.name)
    val seq1 = Observable.range(0, 3)
    val seq2 = Observable.range(10, 3)
    val seq3 = Observable.just(20)
    seq1.concatWith(seq2)
        .concatWith(seq3)
        .dump()

    // 0
    // 1
    // 2
    // 10
    // 11
    // 12
    // 20
}

private fun exampleCount() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(0, 3)
    values
        .dump("Values")
    values
        .count()
        .dump("Count")

    // Values: 0
    // Values: 1
    // Values: 2
    // Values: Completed
    // Count: 3
    // Count: Completed
}

private fun exampleReduce() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(0, 5)
    values
        .reduce { i1, i2 -> i1 + i2 }
        .dump("Sum")
    values
        .reduce { i1, i2 -> if (i1 > i2) i2 else i1 }
        .dump("Min")

    // Sum: 10
    // Sum: Completed
    // Min: 0
    // Min: Completed
}

private fun exampleReduceWithAccumulator() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.just("Rx", "is", "easy")
    values
        .reduce(0) { acc, next -> acc + 1 }
        .dump("Count")

    // Count: 3
    // Count: Completed
}

private fun exampleCollect() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(10, 5)
    values
        .collect(
            { ArrayList<Int>() },
            { acc, value -> acc.add(value) })
        .dump()

    // [10, 11, 12, 13, 14]
}
