package rx

import io.reactivex.rxjava3.core.Observable
import java.util.*


fun main(args: Array<String>) {
    exampleCustom()
    exampleToList()
    exampleToSortedList()

    exampleToMap()
    exampleToMapWithSelector()
    exampleToMapWithCustomContainer()
    exampleToMultimap()
    exampleToMultimapWithCustomContainers()
}

private fun exampleCustom() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(10, 5)
    values
        .reduce(
            ArrayList<Int>()
        ) { acc, value ->
            acc.add(value)
            acc
        }
        .dump()

    // [10, 11, 12, 13, 14]
}

private fun exampleToList() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(10, 5)
    values
        .toList()
        .dump()

    // [10, 11, 12, 13, 14]
}

private fun exampleToSortedList() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.range(10, 5)
    values
        .toSortedList { i1, i2 -> i2 - i1 }
        .dump()

    // [14, 13, 12, 11, 10]
}

private data class Person(val name: String, val age: Int)


private fun exampleToMap() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.just(
        Person("Will", 25),
        Person("Nick", 40),
        Person("Saul", 35)
    )
    values
        .toMap { person -> person.name }
        .dump()

    // toMap: {Saul=Person@7cd84586, Nick=Person@30dae81, Will=Person@1b2c6ec2}
    // toMap: Completed
}

private fun exampleToMapWithSelector() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.just(
        Person("Will", 25),
        Person("Nick", 40),
        Person("Saul", 35)
    )
    values
        .toMap<String, Int>(
            { person -> person.name },
            { person -> person.age })
        .dump()

    // toMap: {Saul=35, Nick=40, Will=25}
    // toMap: Completed
}

private fun exampleToMapWithCustomContainer() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.just(
        Person("Will", 25),
        Person("Nick", 40),
        Person("Saul", 35)
    )
    values
        .toMap<String, Int>(
            { person -> person.name },
            { person -> person.age },
            { HashMap() })
        .dump()

    // toMap: {Saul=35, Nick=40, Will=25}
    // toMap: Completed
}

private fun exampleToMultimap() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.just(
        Person("Will", 35),
        Person("Nick", 40),
        Person("Saul", 35)
    )
    values
        .toMultimap<Int, String>(
            { person -> person.age },
            { person -> person.name })
        .dump()

    // toMap: {35=[Will, Saul], 40=[Nick]}
    // toMap: Completed
}

private fun exampleToMultimapWithCustomContainers() {
    println(object{}.javaClass.enclosingMethod.name)
    val values = Observable.just(
        Person("Will", 35),
        Person("Nick", 40),
        Person("Saul", 35)
    )
    values
        .toMultimap(
            { person -> person.age },
            { person -> person.name },
            { HashMap<Int, Collection<String>>() },
            { key -> ArrayList() })
        .dump()

    // toMap: {35=[Will, Saul], 40=[Nick]}
    // toMap: Completed
}
