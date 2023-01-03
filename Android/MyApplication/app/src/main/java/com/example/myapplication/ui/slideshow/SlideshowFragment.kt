package com.example.myapplication.ui.slideshow

import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.fragment.app.Fragment
import com.example.myapplication.R
import org.koin.androidx.viewmodel.ext.android.viewModel

class SlideshowFragment : Fragment() {

    private val vm by viewModel<SlideshowViewModel>()

    override fun onCreateView(inflater: LayoutInflater, container: ViewGroup?, savedInstanceState: Bundle?): View? {
        val root = inflater.inflate(R.layout.fragment_slideshow, container, false)
        val textView: TextView = root.findViewById(R.id.text_slideshow)
        vm.text.observe(viewLifecycleOwner) {
            textView.text = it
        }
        return root
    }
}