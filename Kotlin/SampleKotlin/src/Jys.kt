
import java.util.*

/**
 * Created by zwvista on 2017/05/26.
 */


fun verticalWriting(txt:String, offset:Int) {
    txt.mapIndexed { i, c -> Pair<Int, Char>(i, c) }
        .groupByTo(TreeMap(), {it.first % offset}, {it.second.toString()})
        .forEach{println(it.value.reversed().joinToString("|"))}
}

fun main(args: Array<String>) {
    verticalWriting("床前明月光疑是地上霜举头望明月低头思故乡", 5)
}