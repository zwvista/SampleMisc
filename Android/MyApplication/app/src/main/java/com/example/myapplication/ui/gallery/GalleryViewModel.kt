package com.example.myapplication.ui.gallery

import androidx.lifecycle.ViewModel
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow

class GalleryViewModel : ViewModel() {

    private val _text = MutableStateFlow("This is gallery Fragment")
    val text: StateFlow<String> = _text

    override fun onCleared() {
        super.onCleared()
    }
}