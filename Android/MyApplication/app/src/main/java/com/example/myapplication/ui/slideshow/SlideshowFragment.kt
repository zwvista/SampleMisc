package com.example.myapplication.ui.slideshow

import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.fragment.app.Fragment
import androidx.lifecycle.Observer
import androidx.lifecycle.ViewModelProvider
import com.example.myapplication.R

class SlideshowFragment : Fragment() {

    private lateinit var vm: SlideshowViewModel

    override fun onCreateView(inflater: LayoutInflater, container: ViewGroup?, savedInstanceState: Bundle?): View? {
        vm = ViewModelProvider(this)[SlideshowViewModel::class.java]
        val root = inflater.inflate(R.layout.fragment_slideshow, container, false)
        val textView: TextView = root.findViewById(R.id.text_slideshow)
        vm.text.observe(viewLifecycleOwner, Observer {
            textView.text = it
        })
        return root
    }
}