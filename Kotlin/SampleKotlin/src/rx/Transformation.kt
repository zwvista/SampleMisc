package rx

import io.reactivex.Observable

fun main(args: Array<String>) {
    Observable.range(1, 5)
        .buffer(3)
        .subscribe { println(it) }
    println()
    Observable.range(1, 5)
        .buffer(3, 1)
        .subscribe { println(it) }
    println()
    Observable.just(1, 2, 3)
        .flatMap<Int> { i -> Observable.range(i, i * 2) }
        .subscribe { print(it) }
    println()
    Observable.range(1, 10)
        .groupBy<Int> { it % 3 }
        .subscribe { g -> g.toList().subscribe { lst -> println(lst) } }
    Observable.just(1, 2, 3)
        .map<Int> { it * 10 }
        .subscribe { println(it) }
    Observable.range(1, 5)
        .scan { sum, item -> sum + item }
        .subscribe { println(it) }
    Observable.range(1, 5)
        .window(3)
        .subscribe { g -> g.toList().subscribe { lst -> println(lst) } }
    println()
    Observable.range(1, 5)
        .window(3, 1)
        .subscribe { g -> g.toList().subscribe { lst -> println(lst) } }
}