package com.example.myapplication.ui.home2

import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import androidx.fragment.app.Fragment
import androidx.navigation.fragment.findNavController
import com.androidisland.vita.VitaOwner
import com.androidisland.vita.vita
import com.example.myapplication.databinding.FragmentHome2Binding
import com.example.myapplication.setNavigationResult
import com.example.myapplication.ui.autoCleared

class Home2Fragment : Fragment() {

    private val homeViewModel by lazy { vita.with(VitaOwner.Multiple(this)).getViewModel<Home2ViewModel>() }
    private var binding by autoCleared<FragmentHome2Binding>()

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        binding = FragmentHome2Binding.inflate(inflater, container, false).apply {
            lifecycleOwner = viewLifecycleOwner
            model = homeViewModel
        }
        binding.button.setOnClickListener {
            setNavigationResult( "1")
            findNavController().navigateUp()
        }
        return binding.root
    }
}