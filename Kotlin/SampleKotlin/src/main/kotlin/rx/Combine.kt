package rx

import io.reactivex.Observable
import java.util.concurrent.TimeUnit
import com.sun.deploy.ref.Helpers
import io.reactivex.Single
import io.reactivex.rxkotlin.Singles
import java.util.concurrent.CompletableFuture
import io.reactivex.Flowable
import io.reactivex.rxkotlin.Flowables


fun main(args: Array<String>) {
    run {
        val isUserBlockedStream = Single.fromFuture(CompletableFuture.supplyAsync {
            Thread.sleep(200)
            false
        })
        val userCreditScoreStream = Single.fromFuture(CompletableFuture.supplyAsync {
            Thread.sleep(2300)
            5
        })
        val userCheckStream = Singles.zip(isUserBlockedStream, userCreditScoreStream)
        userCheckStream.subscribe { pair -> println("Received $pair") }
        readLine()
    }
    run {
        val colors = Flowable.just("red", "green", "blue")
        val timer = Flowable.interval(2, TimeUnit.SECONDS)
        Flowables.zip(colors, timer)
            .subscribe({ println("onNext: " + it) }, { throwable -> println("onError") }, { println("onComplete") })
        readLine()
    }
    run {
        val colors = Flowables.zip(Flowable.just("red", "green", "blue"),
            Flowable.interval(2, TimeUnit.SECONDS))
        val numbers = Flowable.interval(1, TimeUnit.SECONDS)
            .take(5)
        Flowable.merge(colors, numbers)
            .subscribe({ println("onNext: " + it) }, { throwable -> println("onError") }, { println("onComplete") })
        readLine()
    }
    run {
        val colors = Flowables.zip(Flowable.just("red", "green", "blue"),
            Flowable.interval(3, TimeUnit.SECONDS))
        val numbers = Flowable.interval(1, TimeUnit.SECONDS)
            .take(4)
        Flowables.combineLatest(colors, numbers)
            .subscribe({ println("onNext: " + it) }, { throwable -> println("onError") }, { println("onComplete") })
        readLine()
    }
}
