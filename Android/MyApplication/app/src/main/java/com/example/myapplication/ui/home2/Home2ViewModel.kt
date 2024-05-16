package com.example.myapplication.ui.home2

import androidx.lifecycle.ViewModel
import kotlinx.coroutines.flow.MutableStateFlow

class Home2ViewModel : ViewModel() {
    val text = MutableStateFlow("This is home2 Fragment")

    override fun onCleared() {
        super.onCleared()
    }
}