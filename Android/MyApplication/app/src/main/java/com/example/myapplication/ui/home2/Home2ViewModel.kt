package com.example.myapplication.ui.home2

import androidx.lifecycle.MutableLiveData
import androidx.lifecycle.ViewModel

class Home2ViewModel : ViewModel() {
    val text = MutableLiveData("This is home2 Fragment")

    override fun onCleared() {
        super.onCleared()
    }
}