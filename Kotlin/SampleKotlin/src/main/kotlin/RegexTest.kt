
fun main(args: Array<String>) {
    val s = "123-4567-89,987-6543-21"
    val r = Regex("\\d{3}-(\\d{4})-\\d{2}")
    val ms = r.findAll(s)
    for (m in ms)
        for ((k, v) in m.groupValues.withIndex())
            println("group $k : $v")

    val r2 = Regex("(\\d+)-(\\d+)-(\\d+)")
    println(r2.replace(s, "\$3-\$1-\$2"))

    val r3 = Regex("\\d+")
    val s3 = r3.replace(s) {
        it.groupValues[0].reversed()
    }
    println(s3)

    val r4 = Regex("%(begin|next|end)%")
    val s4 = "%begin%hello%next%world%end%"
    println(r4.split(s4))
}
