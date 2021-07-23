package rx

import io.reactivex.rxjava3.core.Observable
import java.util.concurrent.TimeUnit


fun main(args: Array<String>) {

    exampleCold()

    exampleConnect()
    exampleDisconnect()
    exampleDispose()
    exampleRefcount()

    exampleReplay()
    exampleReplayWithBufferSize()
    exampleReplayWithTime()

    exampleCache()
    exampleCacheDispose()
}

private fun exampleCold() {
    println(object{}.javaClass.enclosingMethod.name)
    val cold = Observable
        .interval(200, TimeUnit.MILLISECONDS)
        .take(5)
    cold.dump("First")
    Thread.sleep(500)
    cold.dump("Second")
    readLine()

    // First: 0
    // First: 1
    // First: 2
    // Second: 0
    // First: 3
    // Second: 1
    // First: 4
    // Second: 2
    // Second: 3
    // Second: 4
}


private fun exampleConnect() {
    println(object{}.javaClass.enclosingMethod.name)
    val cold = Observable.interval(200, TimeUnit.MILLISECONDS).publish()
    cold.connect()
    val s1 = cold.dump("First")
    Thread.sleep(500)
    val s2 = cold.dump("Second")
    readLine()
    s1.dispose()
    s2.dispose()

    // First: 0
    // First: 1
    // First: 2
    // Second: 2
    // First: 3
    // Second: 3
    // First: 4
    // Second: 4
    // First: 5
    // Second: 5
}

private fun exampleDisconnect() {
    println(object{}.javaClass.enclosingMethod.name)
    val connectable = Observable.interval(200, TimeUnit.MILLISECONDS).publish()
    var s = connectable.connect()
    connectable.dump()
    Thread.sleep(1000)
    println("Closing connection")
    s.dispose()
    Thread.sleep(1000)
    println("Reconnecting")
    s = connectable.connect()
    connectable.dump()
    readLine()
    s.dispose()

    // 0
    // 1
    // 2
    // 3
    // 4
    // Closing connection
    // Reconnecting
    // 0
    // 1
    // 2
}

private fun exampleDispose() {
    println(object{}.javaClass.enclosingMethod.name)
    val connectable = Observable.interval(200, TimeUnit.MILLISECONDS).publish()
    connectable.connect()
    val s1 = connectable.dump("First")
    Thread.sleep(500)
    val s2 = connectable.dump("Second")
    Thread.sleep(500)
    println("Disposing second")
    s2.dispose()
    readLine()
    s1.dispose()

    // First: 0
    // First: 1
    // First: 2
    // Seconds: 2
    // First: 3
    // Seconds: 3
    // First: 4
    // Seconds: 4
    // Unsubscribing second
    // First: 5
    // First: 6
}

private fun exampleRefcount() {
    println(object{}.javaClass.enclosingMethod.name)
    val cold = Observable.interval(200, TimeUnit.MILLISECONDS).publish().refCount()
    var s1 = cold.dump("First")
    Thread.sleep(500)
    val s2 = cold.dump("Second")
    Thread.sleep(500)
    println("Dispose Second")
    s2.dispose()
    Thread.sleep(500)
    println("Dispose First")
    s1.dispose()
    println("First connection again")
    Thread.sleep(500)
    s1 = cold.dump("First")
    readLine()
    s1.dispose()

    // First: 0
    // First: 1
    // First: 2
    // Second: 2
    // First: 3
    // Second: 3
    // Unsubscribe first
    // First: 4
    // First: 5
    // First: 6
    // Unsubscribe first
    // First connection again
    // First: 0
    // First: 1
    // First: 2
    // First: 3
    // First: 4
}

private fun exampleReplay() {
    println(object{}.javaClass.enclosingMethod.name)
    val cold = Observable.interval(200, TimeUnit.MILLISECONDS).replay()
    cold.connect()
    println("Subscribe first")
    val s1 = cold.dump("First")
    Thread.sleep(700)
    println("Subscribe second")
    val s2 = cold.dump("Second")
    Thread.sleep(500)
    readLine()
    s1.dispose()
    s2.dispose()

    // Subscribe first
    // First: 0
    // First: 1
    // First: 2
    // Subscribe second
    // Second: 0
    // Second: 1
    // Second: 2
    // First: 3
    // Second: 3
}

private fun exampleReplayWithBufferSize() {
    println(object{}.javaClass.enclosingMethod.name)
    val source = Observable.interval(1000, TimeUnit.MILLISECONDS)
        .take(5)
        .replay(2)
    source.connect()
    Thread.sleep(4500)
    source.dump()
    readLine()

    // 2
    // 3
    // 4
}

private fun exampleReplayWithTime() {
    println(object{}.javaClass.enclosingMethod.name)
    val source = Observable.interval(1000, TimeUnit.MILLISECONDS)
        .take(5)
        .replay(2000, TimeUnit.MILLISECONDS)
    source.connect()
    Thread.sleep(4500)
    source.dump()
    readLine()

    // 2
    // 3
    // 4
}

private fun exampleCache() {
    println(object{}.javaClass.enclosingMethod.name)
    val obs = Observable.interval(100, TimeUnit.MILLISECONDS)
        .take(5)
        .cache()
    Thread.sleep(500)
    obs.dump("First")
    Thread.sleep(300)
    obs.dump("Second")
    readLine()

    // First: 0
    // First: 1
    // First: 2
    // Second: 0
    // Second: 1
    // Second: 2
    // First: 3
    // Second: 3
    // First: 4
    // Second: 4
}

private fun exampleCacheDispose() {
    println(object{}.javaClass.enclosingMethod.name)
    val obs = Observable.interval(100, TimeUnit.MILLISECONDS)
        .take(5)
        .doOnNext { println(it) }
        .cache()
        .doOnSubscribe { println("Subscribed") }
        .doOnDispose { println("Disposed") }
    val subscription = obs.subscribe()
    Thread.sleep(150)
    subscription.dispose()
    readLine()

    // Subscribed
    // 0
    // Unsubscribed
    // 1
    // 2
    // 3
    // 4
}
