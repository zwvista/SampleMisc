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
        val colors = Flowables.zip(Flowable.just("red", "green", "blue"),
            Flowable.interval(2, TimeUnit.SECONDS))
        val numbers = Flowable.interval(1, TimeUnit.SECONDS)
            .take(4)
        Flowable.concat(colors, numbers)
            .subscribe({ println("onNext: " + it) }, { throwable -> println("onError") }, { println("onComplete") })
        readLine()
    }
}
