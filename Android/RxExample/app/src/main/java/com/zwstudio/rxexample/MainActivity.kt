package com.zwstudio.rxexample

import android.support.v7.app.AppCompatActivity
import android.os.Bundle
import android.widget.EditText
import android.widget.TextView
import com.jakewharton.rxbinding2.widget.RxTextView
import io.reactivex.android.schedulers.AndroidSchedulers
import io.reactivex.rxkotlin.Observables
import io.reactivex.schedulers.Schedulers

class MainActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        val et1 = findViewById<EditText>(R.id.et1)
        val et2 = findViewById<EditText>(R.id.et2)
        val et3 = findViewById<EditText>(R.id.et3)
        val tvResult = findViewById<TextView>(R.id.tvResult)
        Observables.combineLatest(RxTextView.textChanges(et1), RxTextView.textChanges(et2), RxTextView.textChanges(et3))
            {s1, s2, s3 -> ((s1.toString().toInt() ?: 0) + (s2.toString().toInt() ?: 0) + (s3.toString().toInt() ?: 0)).toString()}
            .subscribeOn(Schedulers.io())
            .observeOn(AndroidSchedulers.mainThread())
            .subscribe(RxTextView.text(tvResult))
    }
}
