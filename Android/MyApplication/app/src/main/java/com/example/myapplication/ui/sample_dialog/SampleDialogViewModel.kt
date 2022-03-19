package com.example.myapplication.ui.sample_dialog

import androidx.lifecycle.MutableLiveData
import androidx.lifecycle.ViewModel

class SampleDialogViewModel : ViewModel() {
    val text = MutableLiveData("This is Sample Dialog Fragment")
}