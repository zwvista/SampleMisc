package rx

import io.reactivex.Observable
import java.util.concurrent.TimeUnit

fun main(args: Array<String>) {
    Observable.range(0, 10)
        .elementAt(2)
        .subscribe({ println("onNext: " + it) }, { throwable -> println("onError") }, { println("onComplete") })
    println()
    Observable.range(0, 10)
        .filter { it % 2 == 0 }
        .subscribe({ println("onNext: " + it) }, { throwable -> println("onError") }, { println("onComplete") })
    Observable.range(0, 10)
        .first(2)
        .subscribe { num -> println("single: " + num) }
    Observable.range(0, 10)
        .last(2)
        .subscribe { num -> println("single: " + num) }
    Observable.range(0, 10)
        .take(3)
        .subscribe({ println("onNext: " + it) }, { throwable -> println("onError") }, { println("onComplete") })
    Observable.range(0, 10)
        .takeLast(3)
        .subscribe({ println("onNext: " + it) }, { throwable -> println("onError") }, { println("onComplete") })
    Observable.range(0, 10)
        .skip(3)
        .subscribe({ println("onNext: " + it) }, { throwable -> println("onError") }, { println("onComplete") })
    Observable.range(0, 10)
        .skipLast(3)
        .subscribe({ println("onNext: " + it) }, { throwable -> println("onError") }, { println("onComplete") })
    Observable.just(1,2,2,3,3,2,2,3,3,4)
        .distinct()
        .subscribe({ println("onNext: " + it) }, { throwable -> println("onError") }, { println("onComplete") })
    Observable.just(1,2,2,3,3,2,2,3,3,4)
        .distinctUntilChanged()
        .subscribe({ println("onNext: " + it) }, { throwable -> println("onError") }, { println("onComplete") })
    println()
}
