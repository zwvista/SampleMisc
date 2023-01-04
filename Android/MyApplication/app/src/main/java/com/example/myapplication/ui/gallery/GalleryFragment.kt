package com.example.myapplication.ui.gallery

import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.fragment.app.Fragment
import androidx.lifecycle.viewModelScope
import com.example.myapplication.R
import kotlinx.coroutines.launch
import org.koin.androidx.viewmodel.ext.android.viewModel

class GalleryFragment : Fragment() {

    private val vm by viewModel<GalleryViewModel>()

    override fun onCreateView(inflater: LayoutInflater, container: ViewGroup?, savedInstanceState: Bundle?): View? {
        val root = inflater.inflate(R.layout.fragment_gallery, container, false)
        val textView: TextView = root.findViewById(R.id.text_gallery)
        vm.viewModelScope.launch {
            vm.text.collect {
                textView.text = it
            }
        }
        return root
    }
}