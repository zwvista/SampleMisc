package rx

import io.reactivex.Observable
import io.reactivex.rxkotlin.toObservable
import java.util.concurrent.TimeUnit
import java.util.Random
import io.reactivex.schedulers.TestScheduler


fun concatMap() {
    val items = listOf("a", "b", "c", "d", "e", "f")
    val scheduler = TestScheduler()
    items.toObservable()
        .concatMap { s ->
            val delay = Random().nextInt(10)
            Observable.just(s + "x")
                .delay(delay.toLong(), TimeUnit.SECONDS, scheduler)
        }.toList()
        .doAfterSuccess { println(it) }
        .subscribe()
    scheduler.advanceTimeBy(1, TimeUnit.MINUTES)
}

fun flatMap() {
    val items = listOf("a", "b", "c", "d", "e", "f")
    val scheduler = TestScheduler()
    items.toObservable()
        .flatMap { s ->
            val delay = Random().nextInt(10)
            Observable.just(s + "x")
                .delay(delay.toLong(), TimeUnit.SECONDS, scheduler)
        }.toList()
        .doAfterSuccess { println(it) }
        .subscribe()
    scheduler.advanceTimeBy(1, TimeUnit.MINUTES)
}

fun switchMap() {
    val items = listOf("a", "b", "c", "d", "e", "f")
    val scheduler = TestScheduler()
    items.toObservable()
        .switchMap { s ->
            val delay = Random().nextInt(10)
            Observable.just(s + "x")
                .delay(delay.toLong(), TimeUnit.SECONDS, scheduler)
        }.toList()
        .doAfterSuccess { println(it) }
        .subscribe()
    scheduler.advanceTimeBy(1, TimeUnit.MINUTES)
}

fun main(args: Array<String>) {
    buffer()

    concatMap()
    flatMap()
    switchMap()

    Observable.just(1, 2, 3)
        .flatMap<Int> { i -> Observable.range(1, i) }
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

private fun buffer() {
    Observable.range(1, 5)
        .buffer(3)
        .subscribe { println(it) }
    println()
    Observable.range(1, 5)
        .buffer(3, 1)
        .subscribe { println(it) }
    println()
}