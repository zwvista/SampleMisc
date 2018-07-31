package rx

import io.reactivex.Completable
import io.reactivex.Maybe
import io.reactivex.Observable
import io.reactivex.Single

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
