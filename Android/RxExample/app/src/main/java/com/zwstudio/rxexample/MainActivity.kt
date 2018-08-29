package com.zwstudio.rxexample

import android.support.v7.app.AppCompatActivity
import android.os.Bundle
import android.text.Editable
import android.text.TextWatcher
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

        val etNumber1 = findViewById<EditText>(R.id.etNumber1)
        val etNumber2 = findViewById<EditText>(R.id.etNumber2)
        val etNumber3 = findViewById<EditText>(R.id.etNumber3)
        val tvResult = findViewById<TextView>(R.id.tvResult)

//        val textWatcher = object : TextWatcher {
//            override fun afterTextChanged(s: Editable?) {
//            }
//
//            override fun beforeTextChanged(s: CharSequence?, start: Int, count: Int, after: Int) {
//            }
//
//            override fun onTextChanged(s: CharSequence?, start: Int, before: Int, count: Int) {
//                val num1 = etNumber1.text.toString().toInt() ?: 0
//                val num2 = etNumber2.text.toString().toInt() ?: 0
//                val num3 = etNumber3.text.toString().toInt() ?: 0
//                tvResult.text = (num1 + num2 + num3).toString()
//            }
//
//        }
//        etNumber1.addTextChangedListener(textWatcher)
//        etNumber2.addTextChangedListener(textWatcher)
//        etNumber3.addTextChangedListener(textWatcher)
//        etNumber1.text = etNumber1.text

        Observables.combineLatest(RxTextView.textChanges(etNumber1), RxTextView.textChanges(etNumber2), RxTextView.textChanges(etNumber3))
            {s1, s2, s3 -> ((s1.toString().toInt() ?: 0) + (s2.toString().toInt() ?: 0) + (s3.toString().toInt() ?: 0)).toString()}
            .subscribeOn(Schedulers.io())
            .observeOn(AndroidSchedulers.mainThread())
            .subscribe(RxTextView.text(tvResult))
    }
}
