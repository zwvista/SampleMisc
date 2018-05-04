
fun main(args: Array<String>) {
    val s = "123-4567-89"
    val r = Regex("\\d{3}-(\\d{4})-\\d{2}")
    val m = r.find(s)
    if (m != null)
        for (kv in m.groupValues.withIndex())
            println("group ${kv.index} : ${kv.value}")

    val r2 = Regex("\\d+")
    val s2 = r2.replace(s) {
        it.groupValues[0].reversed()
    }
    println(s2)

    val r3 = Regex("%(begin|next|end)%")
    val s3 = "%begin%hello%next%world%end%"
    println(r3.split(s3))
}
