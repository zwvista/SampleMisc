package com.example.myapplication

import android.app.Application
import android.content.Context
import android.view.View
import android.view.ViewGroup
import android.widget.ArrayAdapter
import android.widget.TextView
import androidx.annotation.IdRes
import androidx.annotation.LayoutRes
import com.androidisland.vita.startVita
import java.io.Serializable

class MyApplication : Application() {
    override fun onCreate() {
        super.onCreate()
        startVita()
    }
}

class MSelectItem(val value: Int, val label: String) : Serializable

val items = listOf(
    MSelectItem(1, "aaa"),
    MSelectItem(2, "bbb"),
    MSelectItem(3, "ccc"),
)

fun <T> makeAdapter(context: Context, @LayoutRes resource: Int, @IdRes textViewResourceId: Int, objects: List<T>, convert: ArrayAdapter<T>.(View, Int) -> View): ArrayAdapter<T> =
    object : ArrayAdapter<T>(context, resource, textViewResourceId, objects) {
        override fun getView(position: Int, convertView: View?, parent: ViewGroup) =
            convert(super.getView(position, convertView, parent), position)

        override fun getDropDownView(position: Int, convertView: View?, parent: ViewGroup) =
            convert(super.getDropDownView(position, convertView, parent), position)
    }

fun <T> makeCustomAdapter(context: Context, objects: List<T>, labelFunc: (T) -> String): ArrayAdapter<T> =
    makeAdapter(context, android.R.layout.simple_spinner_item, 0, objects) { v, position ->
        val tv = v.findViewById<TextView>(android.R.id.text1)
        tv.text = labelFunc(getItem(position)!!)
        v
    }.apply {
        setDropDownViewResource(android.R.layout.simple_list_item_single_choice)
    }
