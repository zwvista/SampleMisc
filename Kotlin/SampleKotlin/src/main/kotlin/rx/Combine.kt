package rx

import io.reactivex.Observable
import java.util.concurrent.TimeUnit

fun main(args: Array<String>) {
    val one = Observable.interval(1, TimeUnit.SECONDS).take(5)
    val two = Observable.interval(250, TimeUnit.MILLISECONDS).take(5)
    val three = Observable.interval(150, TimeUnit.MILLISECONDS).take(5)
    one.a
    readLine()
}
