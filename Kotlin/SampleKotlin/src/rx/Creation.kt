package rx

import io.reactivex.Observable
import java.util.concurrent.TimeUnit

fun main(args: Array<String>) {
    Observable.just<Any>(3, 1, 5, 4, "test")
        .subscribe({ num -> println("onNext: $num") }, { throwable -> println("onError") }, { println("onComplete") })
    Observable.create<Person> { e ->
        val person = Person(100, "nshiba")
        e.onNext(person)
        e.onComplete()
    } .subscribe(System.out::print)
    Observable.range(0, 10)
        .subscribe({ i -> println("onNext: " + i!!) }, { throwable -> println("onError") }, { println("onComplete") })
    Observable.just(1, 2, 3)
        .repeat(3)
        .subscribe({ i -> println("onNext: " + i!!) }, { throwable -> println("onError") }, { println("onComplete") })
    println(System.currentTimeMillis())
    Observable.timer(3, TimeUnit.SECONDS)
        .subscribe({ aLong -> println(System.currentTimeMillis()) }, { throwable -> println("onError") }, { println("onComplete") })
    readLine()
    Observable
        .interval(1, TimeUnit.SECONDS)
        .subscribe(System.out::print)
    readLine()
}

private class Person(val age: Int, val name: String) {
    override fun toString() = name + ":" + age.toString()
}
