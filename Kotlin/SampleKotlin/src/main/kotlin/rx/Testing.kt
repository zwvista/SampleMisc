package rx

import io.reactivex.rxjava3.core.Observable
import io.reactivex.rxjava3.schedulers.TestScheduler
import java.util.*
import java.util.concurrent.TimeUnit


fun main(args: Array<String>) {
    exampleTick()

    exampleAdvanceTo()
    exampleTimeBy()
    exampleTriggerActions()
    exampleCollision()
}


private fun exampleTick() {
    println(object{}.javaClass.enclosingMethod.name)
    val scheduler = TestScheduler()
    val expected = listOf(0L, 1L, 2L, 3L, 4L)
    val result = ArrayList<Long>()
    Observable
        .interval(1, TimeUnit.SECONDS, scheduler)
        .take(5)
        .subscribe { i -> result.add(i) }
    println(result.isEmpty())
    scheduler.advanceTimeBy(5, TimeUnit.SECONDS)
    println(result == expected)
}

private fun exampleAdvanceTo() {
    println(object{}.javaClass.enclosingMethod.name)
    val s = TestScheduler()
    s.createWorker().schedule(
        { println("Immediate") })
    s.createWorker().schedule(
        { println("20s") },
        20, TimeUnit.SECONDS)
    s.createWorker().schedule(
        { println("40s") },
        40, TimeUnit.SECONDS)
    println("Advancing to 1ms")
    s.advanceTimeTo(1, TimeUnit.MILLISECONDS)
    println("Virtual time: " + s.now(TimeUnit.SECONDS))
    println("Advancing to 10s")
    s.advanceTimeTo(10, TimeUnit.SECONDS)
    println("Virtual time: " + s.now(TimeUnit.SECONDS))
    println("Advancing to 40s")
    s.advanceTimeTo(40, TimeUnit.SECONDS)
    println("Virtual time: " + s.now(TimeUnit.SECONDS))

    // Advancing to 1ms
    // Immediate
    // Virtual time: 1
    // Advancing to 10s
    // Virtual time: 10000
    // Advancing to 40s
    // 20s
    // 40s
    // Virtual time: 40000
}

private fun exampleTimeBy() {
    println(object{}.javaClass.enclosingMethod.name)
    val s = TestScheduler()
    s.createWorker().schedule { println("Immediate") }
    s.createWorker().schedule(
        { println("20s") },
        20, TimeUnit.SECONDS)
    s.createWorker().schedule(
        { println("40s") },
        40, TimeUnit.SECONDS)
    println("Advancing by 1ms")
    s.advanceTimeBy(1, TimeUnit.MILLISECONDS)
    println("Virtual time: " + s.now(TimeUnit.SECONDS))
    println("Advancing by 10s")
    s.advanceTimeBy(10, TimeUnit.SECONDS)
    println("Virtual time: " + s.now(TimeUnit.SECONDS))
    println("Advancing by 40s")
    s.advanceTimeBy(40, TimeUnit.SECONDS)
    println("Virtual time: " + s.now(TimeUnit.SECONDS))

    // Advancing by 1ms
    // Immediate
    // Virtual time: 1
    // Advancing by 10s
    // Virtual time: 10001
    // Advancing by 40s
    // 20s
    // 40s
    // Virtual time: 50001
}

private fun exampleTriggerActions() {
    println(object{}.javaClass.enclosingMethod.name)
    val s = TestScheduler()
    s.createWorker().schedule { println("Immediate") }
    s.createWorker().schedule(
        { println("20s") },
        20, TimeUnit.SECONDS)
    s.triggerActions()
    println("Virtual time: " + s.now(TimeUnit.SECONDS))

    // Immediate
    // Virtual time: 0
}

private fun exampleCollision() {
    println(object{}.javaClass.enclosingMethod.name)
    val s = TestScheduler()
    s.createWorker().schedule(
        { println("First") },
        20, TimeUnit.SECONDS)
    s.createWorker().schedule(
        { println("Second") },
        20, TimeUnit.SECONDS)
    s.createWorker().schedule(
        { println("Third") },
        20, TimeUnit.SECONDS)
    s.advanceTimeTo(20, TimeUnit.SECONDS)

    // First
    // Second
    // Third
}

