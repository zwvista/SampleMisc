
fun main(args: Array<String>) {
    val s = "123-4567-89,987-6543-21"
    val r = Regex("""\d{3}-(\d{4})-\d{2}""")
    val ms = r.findAll(s)
    for ((i, m) in ms.withIndex())
        for ((j, v) in m.groupValues.withIndex())
            println("group $i,$j : $v")

    val r2 = Regex("""(\d+)-(\d+)-(\d+)""")
    println(r2.replace(s, "\$3-\$1-\$2"))

    val r3 = Regex("""\d+""")
    val s3 = r3.replace(s) {
        it.groupValues[0].reversed()
    }
    println(s3)

    val r4 = Regex("%(begin|next|end)%")
    val s4 = "%begin%hello%next%world%end%"
    println(r4.split(s4))
}

/*
group 0,0 : 123-4567-89
group 0,1 : 4567
group 1,0 : 987-6543-21
group 1,1 : 6543
89-123-4567,21-987-6543
321-7654-98,789-3456-12
[, hello, world, ]
 */