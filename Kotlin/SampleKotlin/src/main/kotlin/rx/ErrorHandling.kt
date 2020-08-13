package rx

import io.reactivex.rxjava3.core.Observable
import java.util.*
import java.util.concurrent.TimeUnit


fun main(args: Array<String>) {
    exampleOnErrorReturn()
    exampleOnErrorResumeWith()
    exampleOnErrorResumeNextRethrow()

    exampleRetry()
    exampleRetryWhen()
}

private fun exampleOnErrorReturn() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.create<String> { o ->
        o.onNext("Rx")
        o.onNext("is")
        o.onError(Exception("adjective unknown"))
    }
    values
        .onErrorReturn { e -> "Error: " + e.message }
        .dump()

    // Rx
    // is
    // Error: adjective unknown
}

private fun exampleOnErrorResumeWith() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.create<Int> { o ->
        o.onNext(1)
        o.onNext(2)
        o.onError(Exception("Oops"))
    }
    values
        .onErrorResumeWith(Observable.just(Int.MAX_VALUE))
        .dump()

    // with onError: 1
    // with onError: 2
    // with onError: 2147483647
    // with onError: Completed
}

private fun exampleOnErrorResumeNextRethrow() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.create<Int> { o ->
        o.onNext(1)
        o.onNext(2)
        o.onError(Exception("Oops"))
    }
    values
        .onErrorResumeNext { e -> Observable.error(UnsupportedOperationException(e)) }
        .dump()

    // with onError: : 1
    // with onError: : 2
    // with onError: : Error: java.lang.UnsupportedOperationException: java.lang.Exception: Oops
}

private fun exampleRetry() {
    println(object{}.javaClass.enclosingMethod.name)
    val random = Random()
    val values = Observable.create<Int> { o ->
        o.onNext(random.nextInt() % 20)
        o.onNext(random.nextInt() % 20)
        o.onError(Exception())
    }
    values
        .retry(1)
        .dump()

    // 0
    // 13
    // 9
    // 15
    // java.lang.Exception
}

private fun exampleRetryWhen() {
    println(object{}.javaClass.enclosingMethod.name)
    val source = Observable.create<Int> { o ->
        o.onNext(1)
        o.onNext(2)
        o.onError(Exception("Failed"))
    }
    source.retryWhen { o ->
        o.take(2).delay(100, TimeUnit.MILLISECONDS)
    }
        .timeInterval()
        .dump()
    readLine()

    // TimeInterval [intervalInMilliseconds=17, value=1]
    // TimeInterval [intervalInMilliseconds=0, value=2]
    // TimeInterval [intervalInMilliseconds=102, value=1]
    // TimeInterval [intervalInMilliseconds=0, value=2]
    // TimeInterval [intervalInMilliseconds=102, value=1]
    // TimeInterval [intervalInMilliseconds=0, value=2]
}