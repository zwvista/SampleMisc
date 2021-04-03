package com.example.myapplication.ui.home

import androidx.lifecycle.MutableLiveData
import androidx.lifecycle.ViewModel

class HomeViewModel : ViewModel() {
    val text = MutableLiveData("This is home Fragment")
    val itemPosition = MutableLiveData(1)
}