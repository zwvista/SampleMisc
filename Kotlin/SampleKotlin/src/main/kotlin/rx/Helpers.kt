package rx

import io.reactivex.rxjava3.core.*
import io.reactivex.rxjava3.disposables.Disposable


class PrintSubscriber<T>(private val name: String) : Observer<T> {
    override fun onSubscribe(d: Disposable) {

    }

    override fun onComplete() {
        println("$name: onComplete")
    }

    override fun onError(e: Throwable) {
        println("$name: onError: $e")
    }

    override fun onNext(t: T) {
        println("$name: onNext: $t")
    }
}

fun <T> Observable<T>.dump() =
    this.subscribe({ println("onNext: $it") },
        { println("onError: $it: ${it.message}") },
        { println("onComplete") })

fun <T> Observable<T>.dump(name: String) =
    this.subscribe({ println("$name: onNext: $it") },
        { println("$name: onError: $it: ${it.message}") },
        { println("$name: onComplete") })

fun <T> Single<T>.dump() =
    this.subscribe({ println("onSuccess: $it") },
        { println("onError: $it: ${it.message}") })

fun <T> Single<T>.dump(name: String) =
    this.subscribe({ println("$name: onSuccess: $it") },
        { println("$name: onError: $it: ${it.message}") })

fun <T> Maybe<T>.dump() =
    this.subscribe({ println("onSuccess: $it") },
        { println("onError: $it: ${it.message}") })

fun <T> Maybe<T>.dump(name: String) =
    this.subscribe({ println("$name: onSuccess: $it") },
        { println("$name: onError: $it: ${it.message}") })

fun Completable.dump() =
    this.subscribe({ println("onComplete") },
        { println("onError: $it: ${it.message}") })
