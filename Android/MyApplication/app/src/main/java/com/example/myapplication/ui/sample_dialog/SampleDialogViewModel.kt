package com.example.myapplication.ui.sample_dialog

import androidx.lifecycle.ViewModel
import kotlinx.coroutines.flow.MutableStateFlow

class SampleDialogViewModel : ViewModel() {
    val text = MutableStateFlow("This is Sample Dialog Fragment")
}