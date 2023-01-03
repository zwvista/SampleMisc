package rx

import io.reactivex.rxjava3.core.*
import io.reactivex.rxjava3.disposables.Disposable


class PrintSubscriber<T: Any>(private val name: String) : Observer<T> {
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

fun <T: Any> Observable<T>.dump(): Disposable =
    this.subscribe({ println("onNext: $it") },
        { println("onError: $it: ${it.message}") },
        { println("onComplete") })

fun <T: Any> Observable<T>.dump(name: String): Disposable =
    this.subscribe({ println("$name: onNext: $it") },
        { println("$name: onError: $it: ${it.message}") },
        { println("$name: onComplete") })

fun <T: Any> Flowable<T>.dump(): Disposable =
    this.subscribe({ println("onNext: $it") },
        { println("onError: $it: ${it.message}") },
        { println("onComplete") })

fun <T: Any> Flowable<T>.dump(name: String): Disposable =
    this.subscribe({ println("$name: onNext: $it") },
        { println("$name: onError: $it: ${it.message}") },
        { println("$name: onComplete") })

fun <T: Any> Single<T>.dump(): Disposable =
    this.subscribe({ println("onSuccess: $it") },
        { println("onError: $it: ${it.message}") })

fun <T: Any> Single<T>.dump(name: String): Disposable =
    this.subscribe({ println("$name: onSuccess: $it") },
        { println("$name: onError: $it: ${it.message}") })

fun <T: Any> Maybe<T>.dump(): Disposable =
    this.subscribe({ println("onSuccess: $it") },
        { println("onError: $it: ${it.message}") })

fun <T: Any> Maybe<T>.dump(name: String): Disposable =
    this.subscribe({ println("$name: onSuccess: $it") },
        { println("$name: onError: $it: ${it.message}") })

fun Completable.dump(): Disposable =
    this.subscribe({ println("onComplete") },
        { println("onError: $it: ${it.message}") })
