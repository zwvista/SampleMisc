package rx

import io.reactivex.rxjava3.core.Observable
import io.reactivex.rxjava3.kotlin.toObservable
import io.reactivex.rxjava3.schedulers.TestScheduler
import java.util.*
import java.util.concurrent.TimeUnit


fun main(args: Array<String>) {

    concatMap()
    flatMap()
    switchMap()

}

private fun concatMap() {
    println(object{}.javaClass.enclosingMethod.name)
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

private fun flatMap() {
    println(object{}.javaClass.enclosingMethod.name)
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

private fun switchMap() {
    println(object{}.javaClass.enclosingMethod.name)
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
