package rx

import io.reactivex.schedulers.Schedulers
import io.reactivex.subjects.AsyncSubject
import io.reactivex.subjects.BehaviorSubject
import io.reactivex.subjects.PublishSubject
import io.reactivex.subjects.ReplaySubject
import java.util.concurrent.TimeUnit



fun main(args: Array<String>) {
    examplePublishSubject()

    exampleReplaySubjectEarlyLate()
    exampleReplaySubjectWithSize()
    exampleReplaySubjectWithTime()
    exampleReplaySubjectUnsubscribe()
    exampleReplaySubjectIndependentSubscriptions()

    exampleBehaviorSubjectLate()
    exampleBehaviorSubjectCompleted()
    exampleBehaviorSubjectInitialvalue()

    exampleAsyncSubjectLastValue()
    exampleAsyncSubjectNoCompletion()

    exampleRxContract()
    exampleRxContractPrintCompletion()

}

private fun examplePublishSubject() {
    println(object{}.javaClass.enclosingMethod.name)
    val subject = PublishSubject.create<Int>()
    subject.onNext(1)
    subject.subscribe { println(it) }
    subject.onNext(2)
    subject.onNext(3)
    subject.onNext(4)

    // 2
    // 3
    // 4
}

private fun exampleReplaySubjectEarlyLate() {
    println(object{}.javaClass.enclosingMethod.name)
    val s = ReplaySubject.create<Int>()
    s.subscribe { v -> println("Early:$v") }
    s.onNext(0)
    s.onNext(1)
    s.subscribe { v -> println("Late: $v") }
    s.onNext(2)

    // Early:0
    // Early:1
    // Late: 0
    // Late: 1
    // Early:2
    // Late: 2
}

private fun exampleReplaySubjectWithSize() {
    println(object{}.javaClass.enclosingMethod.name)
    val s = ReplaySubject.createWithSize<Int>(2)
    s.onNext(0)
    s.onNext(1)
    s.onNext(2)
    s.subscribe { v -> println("Late: $v") }
    s.onNext(3)

    // Late: 1
    // Late: 2
    // Late: 3
}

private fun exampleReplaySubjectWithTime() {
    println(object{}.javaClass.enclosingMethod.name)
    val s = ReplaySubject.createWithTime<Int>(150, TimeUnit.MILLISECONDS, Schedulers.trampoline())
    s.onNext(0)
    Thread.sleep(100)
    s.onNext(1)
    Thread.sleep(100)
    s.onNext(2)
    s.subscribe { v -> println("Late: $v") }
    s.onNext(3)

    // Late: 1
    // Late: 2
    // Late: 3
}

private fun exampleReplaySubjectUnsubscribe() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = ReplaySubject.create<Int>()
    val subscription = values.subscribe(
        { v -> println(v) },
        { e -> println(e) },
        { println("Done") })
    values.onNext(0)
    values.onNext(1)
    subscription.dispose()
    values.onNext(2)

    // 0
    // 1
}

private fun exampleReplaySubjectIndependentSubscriptions() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = ReplaySubject.create<Int>()
    val subscription1 = values.subscribe { v -> println("First: $v") }
    values.subscribe { v -> println("Second: $v") }
    values.onNext(0)
    values.onNext(1)
    subscription1.dispose()
    println("Unsubscribed first")
    values.onNext(2)

    // First: 0
    // Second: 0
    // First: 1
    // Second: 1
    // Unsubscribed first
    // Second: 2
}

private fun exampleBehaviorSubjectLate() {
    println(object{}.javaClass.enclosingMethod.name)
    val s = BehaviorSubject.create<Int>()
    s.onNext(0)
    s.onNext(1)
    s.onNext(2)
    s.subscribe { v -> println("Late: $v") }
    s.onNext(3)

    // Late: 2
    // Late: 3
}

private fun exampleBehaviorSubjectCompleted() {
    println(object{}.javaClass.enclosingMethod.name)
    val s = BehaviorSubject.create<Int>()
    s.onNext(0)
    s.onNext(1)
    s.onNext(2)
    s.onComplete()
    s.subscribe(
        { v -> println("Late: $v") },
        { e -> println("Error") },
        { println("Completed") }
    )
}

private fun exampleBehaviorSubjectInitialvalue() {
    println(object{}.javaClass.enclosingMethod.name)
    val s = BehaviorSubject.createDefault(0)
    s.subscribe { v -> println(v) }
    s.onNext(1)

    // 0
    // 1
}

private fun exampleAsyncSubjectLastValue() {
    println(object{}.javaClass.enclosingMethod.name)
    val s = AsyncSubject.create<Int>()
    s.subscribe { v -> println(v) }
    s.onNext(0)
    s.onNext(1)
    s.onNext(2)
    s.onComplete()

    // 2
}

private fun exampleAsyncSubjectNoCompletion() {
    println(object{}.javaClass.enclosingMethod.name)
    val s = AsyncSubject.create<Int>()
    s.subscribe { v -> println(v) }
    s.onNext(0)
    s.onNext(1)
    s.onNext(2)
}

private fun exampleRxContract() {
    println(object{}.javaClass.enclosingMethod.name)
    val s = ReplaySubject.create<Int>()
    s.subscribe { v -> println(v) }
    s.onNext(0)
    s.onComplete()
    s.onNext(1)
    s.onNext(2)

    // 0
}

private fun exampleRxContractPrintCompletion() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = ReplaySubject.create<Int>()
    values.subscribe(
        { v -> println(v) },
        { e -> println(e) },
        { println("Completed") }
    )
    values.onNext(0)
    values.onNext(1)
    values.onComplete()
    values.onNext(2)

    // 0
    // 1
}
