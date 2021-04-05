package com.example.myapplication.ui.home

import android.util.Log
import android.view.View
import android.widget.AdapterView
import androidx.lifecycle.MutableLiveData
import androidx.lifecycle.ViewModel
import com.example.myapplication.MSelectItem

class HomeViewModel : ViewModel() {
    val text = MutableLiveData("This is home Fragment")
    val itemPosition = MutableLiveData(1)
    fun onItemSelected(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
        Log.d("Spinner", "Last item position: ${itemPosition.value}")
        Log.d("Spinner", "Selected item position: $position")
        Log.d("Spinner", "Selected item value: ${(parent?.selectedItem as MSelectItem).value}")
    }
}