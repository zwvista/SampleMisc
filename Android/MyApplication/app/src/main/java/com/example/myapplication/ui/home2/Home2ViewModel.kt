package com.example.myapplication.ui.home2

import androidx.lifecycle.LiveData
import androidx.lifecycle.MutableLiveData
import androidx.lifecycle.ViewModel

class Home2ViewModel : ViewModel() {

    private val _text = MutableLiveData<String>().apply {
        value = "This is home2 Fragment"
    }
    val text: LiveData<String> = _text
}