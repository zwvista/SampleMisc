package rx

import io.reactivex.Observable
import io.reactivex.functions.Function
import io.reactivex.subjects.ReplaySubject
import java.util.concurrent.TimeUnit


fun main(args: Array<String>) {
    exampleDelay()
    exampleDelaySubscription()
    exampleDelaySubscriptionWithSignal()

    exampleDoOnEach()
    exampleDoOnEachEncapsulation()
    exampleDoOnSubscriber()

    exampleMaterialize()

    exampleSafeSubscribe()
    exampleSerialize()

    exampleTimeInterval()

    exampleTimeout()
    exampleTimeoutWithResume()
    exampleTimeoutPerItem()
    exampleTimeoutPerItemWithResume()

    exampleTimestamp()

    exampleUsing()
}

private fun exampleDelay() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(100, TimeUnit.MILLISECONDS)
        .delay { i -> Observable.timer(i * 100, TimeUnit.MILLISECONDS) }
        .timeInterval()
        .take(5)
        .dump()
    readLine()

    // TimeInterval [intervalInMilliseconds=152, value=0]
    // TimeInterval [intervalInMilliseconds=173, value=1]
    // TimeInterval [intervalInMilliseconds=199, value=2]
    // TimeInterval [intervalInMilliseconds=201, value=3]
    // TimeInterval [intervalInMilliseconds=199, value=4]
}

private fun exampleDelaySubscription() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(100, TimeUnit.MILLISECONDS)
        .delaySubscription(1000, TimeUnit.MILLISECONDS)
        .timeInterval()
        .take(5)
        .dump()
    readLine()

    // TimeInterval [intervalInMilliseconds=1114, value=0]
    // TimeInterval [intervalInMilliseconds=92, value=1]
    // TimeInterval [intervalInMilliseconds=101, value=2]
    // TimeInterval [intervalInMilliseconds=100, value=3]
    // TimeInterval [intervalInMilliseconds=99, value=4]
}

private fun exampleDelaySubscriptionWithSignal() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.interval(100, TimeUnit.MILLISECONDS)
        .delaySubscription(Observable.timer(1000, TimeUnit.MILLISECONDS))
        .timeInterval()
        .take(5)
        .dump()
    readLine()

    // TimeInterval [intervalInMilliseconds=1114, value=0]
    // TimeInterval [intervalInMilliseconds=92, value=1]
    // TimeInterval [intervalInMilliseconds=101, value=2]
    // TimeInterval [intervalInMilliseconds=100, value=3]
    // TimeInterval [intervalInMilliseconds=99, value=4]
}

private fun exampleDoOnEach() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.just("side", "effects")
    values
        .doOnEach(PrintSubscriber("Log"))
        .map { s -> s.toUpperCase() }
        .dump("Process")

    // Log: side
    // Process: SIDE
    // Log: effects
    // Process: EFFECTS
    // Log: Completed
    // Process: Completed
}

private fun exampleDoOnEachEncapsulation() {
    println(object{}.javaClass.enclosingMethod.name)
    val service = {
        Observable
            .just("First", "Second", "Third")
            .doOnEach(PrintSubscriber("Log"))
    }
    service.invoke()
        .map { s -> s.toUpperCase() }
        .filter { s -> s.length > 5 }
        .dump("Process")

    // Log: First
    // Log: Second
    // Process: SECOND
    // Log: Third
    // Log: Completed
    // Process: Completed
}

private fun exampleDoOnSubscriber() {
    println(object{}.javaClass.enclosingMethod.name)
    val subject = ReplaySubject.create<Int>()
    val values = subject
        .doOnSubscribe { println("New subscription") }
        .doOnDispose() { println("Subscription over") }
    val s1 = values.dump("1st")
    subject.onNext(0)
    values.dump("2st")
    subject.onNext(1)
    s1.dispose()
    subject.onNext(2)
    subject.onNext(3)
    subject.onComplete()

    // New subscription
    // 1st: 0
    // New subscription
    // 2st: 0
    // 1st: 1
    // 2st: 1
    // Subscription over
    // 2st: 2
    // 2st: 3
    // 2st: Completed
    // Subscription over
}

private fun exampleMaterialize() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(0, 3)
    values.take(3)
        .materialize()
        .dump()

    // Materialize: [rx.Notification@a4c802e9 OnNext 0]
    // Materialize: [rx.Notification@a4c802ea OnNext 1]
    // Materialize: [rx.Notification@a4c802eb OnNext 2]
    // Materialize: [rx.Notification@18d48ace OnCompleted]
    // Materialize: Completed
}

private fun exampleSafeSubscribe() {
    println(object{}.javaClass.enclosingMethod.name)
    val source = Observable.create<Int> { o ->
        o.onNext(1)
        o.onNext(2)
        o.onComplete()
        o.onNext(3)
        o.onComplete()
    }
    source
        .dump()

    // 1
    // 2
    // Completed
    // Unsubscribed
}

private fun exampleSerialize() {
    println(object{}.javaClass.enclosingMethod.name)
    val source = Observable.create<Any> { o ->
        o.onNext(1)
        o.onNext(2)
        o.onComplete()
        o.onNext(3)
        o.onComplete()
    }
        .cast(java.lang.Integer::class.java)
        .serialize()
    source
        .dump()

    //		1
    //		2
    //		Completed
}

private fun exampleTimeInterval() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.interval(100, TimeUnit.MILLISECONDS)
    values.take(3)
        .timeInterval()
        .dump()
    readLine()

    // TimeInterval: TimeInterval [intervalInMilliseconds=131, value=0]
    // TimeInterval: TimeInterval [intervalInMilliseconds=75, value=1]
    // TimeInterval: TimeInterval [intervalInMilliseconds=100, value=2]
    // TimeInterval: Completed
}

private fun exampleTimeout() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.concat(
        Observable.interval(100, TimeUnit.MILLISECONDS).take(3),
        Observable.interval(500, TimeUnit.MILLISECONDS).take(3),
        Observable.interval(100, TimeUnit.MILLISECONDS).take(3))
        .scan(0) { acc, v -> acc + 1 }
        .timeout(200, TimeUnit.MILLISECONDS)
        .dump()
    readLine()

    // 0
    // 1
    // 2
    // 3
    // java.util.concurrent.TimeoutException
}

private fun exampleTimeoutWithResume() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.concat(
        Observable.interval(100, TimeUnit.MILLISECONDS).take(3),
        Observable.interval(500, TimeUnit.MILLISECONDS).take(3),
        Observable.interval(100, TimeUnit.MILLISECONDS).take(3))
        .scan(0) { acc, v -> acc + 1 }
        .timeout(200, TimeUnit.MILLISECONDS, Observable.just(-1))
        .dump()
    readLine()

    // 0
    // 1
    // 2
    // 3
    // -1
}

private fun exampleTimeoutPerItem() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.concat(
        Observable.interval(100, TimeUnit.MILLISECONDS).take(3),
        Observable.interval(500, TimeUnit.MILLISECONDS).take(3),
        Observable.interval(100, TimeUnit.MILLISECONDS).take(3))
        .scan(0) { acc, v -> acc + 1 }
        .timeout { i -> Observable.timer(200, TimeUnit.MILLISECONDS) }
        .dump()
    readLine()

    // 0
    // 1
    // 2
    // 3
    // java.util.concurrent.TimeoutException
}

private fun exampleTimeoutPerItemWithResume() {
    println(object{}.javaClass.enclosingMethod.name)
    Observable.concat(
        Observable.interval(100, TimeUnit.MILLISECONDS).take(3),
        Observable.interval(500, TimeUnit.MILLISECONDS).take(3),
        Observable.interval(100, TimeUnit.MILLISECONDS).take(3))
        .scan(0) { acc, v -> acc + 1 }
        .timeout<Long>(Function { i -> Observable.timer(200, TimeUnit.MILLISECONDS) }, Observable.just(-1))
        .dump()
    readLine()

    // 0
    // 1
    // 2
    // 3
    // -1
}

private fun exampleTimestamp() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.interval(100, TimeUnit.MILLISECONDS)
    values.take(3)
        .timestamp()
        .dump()
    readLine()

    // Timestamp: Timestamped(timestampMillis = 1428611094943, value = 0)
    // Timestamp: Timestamped(timestampMillis = 1428611095037, value = 1)
    // Timestamp: Timestamped(timestampMillis = 1428611095136, value = 2)
    // Timestamp: Completed
}

private fun exampleUsing() {
    val values = Observable.using(
        {
            val resource = "MyResource"
            println("Leased: $resource")
            resource
        },
        { resource ->
            Observable.create<Char> { o ->
                for (c in resource.toCharArray())
                    o.onNext(c)
                o.onComplete()
            }
        },
        { resource -> println("Disposed: $resource") })
    values
        .dump()

    // Leased: MyResource
    // M
    // y
    // R
    // e
    // s
    // o
    // u
    // r
    // c
    // e
    // Disposed: MyResource
}
