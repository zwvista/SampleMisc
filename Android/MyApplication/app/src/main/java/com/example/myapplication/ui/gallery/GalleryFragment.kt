package com.example.myapplication.ui.gallery

import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.fragment.app.Fragment
import androidx.lifecycle.Observer
import androidx.lifecycle.ViewModelProvider
import com.example.myapplication.R

class GalleryFragment : Fragment() {

    private lateinit var vm: GalleryViewModel

    override fun onCreateView(inflater: LayoutInflater, container: ViewGroup?, savedInstanceState: Bundle?): View? {
        vm = ViewModelProvider(this)[GalleryViewModel::class.java]
        val root = inflater.inflate(R.layout.fragment_gallery, container, false)
        val textView: TextView = root.findViewById(R.id.text_gallery)
        vm.text.observe(viewLifecycleOwner, Observer {
            textView.text = it
        })
        return root
    }
}