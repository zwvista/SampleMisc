package com.example.myapplication.ui.home

import android.util.Log
import android.view.View
import android.widget.AdapterView
import androidx.lifecycle.ViewModel
import com.example.myapplication.MSelectItem
import kotlinx.coroutines.flow.MutableStateFlow

class HomeViewModel : ViewModel() {
    val text = MutableStateFlow("This is home Fragment")
    val itemPosition = MutableStateFlow(1)
    fun onItemSelected(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
        Log.d("Spinner", "Last item position: ${itemPosition.value}")
        Log.d("Spinner", "Selected item position: $position")
        Log.d("Spinner", "Selected item value: ${(parent?.selectedItem as MSelectItem).value}")
    }
}