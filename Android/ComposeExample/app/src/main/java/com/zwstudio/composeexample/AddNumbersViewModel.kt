package com.zwstudio.composeexample

import androidx.lifecycle.ViewModel
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.combine

class AddNumbersViewModel : ViewModel() {
    val number1 = MutableStateFlow("1")
    val number2 = MutableStateFlow("2")
    val number3 = MutableStateFlow("3")
    val result = combine(number1, number2, number3) { n1, n2, n3 ->
        ((n1.toIntOrNull() ?: 0) + (n2.toIntOrNull() ?: 0) + (n3.toIntOrNull() ?: 0)).toString()
    }
}