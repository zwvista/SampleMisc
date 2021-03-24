package com.example.myapplication.ui.home2

import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.fragment.app.Fragment
import androidx.lifecycle.Observer
import androidx.lifecycle.ViewModelProvider
import com.example.myapplication.R

class Home2Fragment : Fragment() {

    private lateinit var home2ViewModel: Home2ViewModel

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        home2ViewModel =
            ViewModelProvider(this).get(Home2ViewModel::class.java)
        val root = inflater.inflate(R.layout.fragment_home2, container, false)
        val textView: TextView = root.findViewById(R.id.text_home)
        home2ViewModel.text.observe(viewLifecycleOwner, Observer {
            textView.text = it
        })
        return root
    }
}