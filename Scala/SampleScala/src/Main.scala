// Main.scala

object Main {
    def print(txt:String, offset:Int) : Unit = {
        txt.zipWithIndex
        .groupBy(_._2 % offset)
        .foreach({case (_, kvs) => println(
            kvs.map(_._1.toString).reverse.mkString("|")
        )});
    }
    def main(args:Array[String]) = {
        print("床前明月光疑是地上霜举头望明月低头思故乡", 5)
    }
  
}

//低|举|疑|床
//头|头|是|前
//思|望|地|明
//故|明|上|月
//乡|月|霜|光
