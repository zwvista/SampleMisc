package com.example.myapplication.ui.home2

import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import androidx.core.os.bundleOf
import androidx.fragment.app.Fragment
import androidx.fragment.app.setFragmentResult
import androidx.navigation.fragment.findNavController
import com.example.myapplication.databinding.FragmentHome2Binding
import com.example.myapplication.ui.autoCleared
import com.example.myapplication.ui.home.HomeViewModel
import org.koin.androidx.viewmodel.ext.android.getViewModel
import org.koin.androidx.viewmodel.ext.android.viewModel

class Home2Fragment : Fragment() {

    private val vm by lazy { requireParentFragment().getViewModel<HomeViewModel>() }
    private val vm2: Home2ViewModel by viewModel()
    private var binding by autoCleared<FragmentHome2Binding>()

    override fun onCreateView(inflater: LayoutInflater, container: ViewGroup?, savedInstanceState: Bundle?): View {
        binding = FragmentHome2Binding.inflate(inflater, container, false).apply {
            lifecycleOwner = viewLifecycleOwner
            model = vm
            model2 = vm2
        }
        binding.button.setOnClickListener {
            setFragmentResult("requestKey", bundleOf("bundleKey" to "1"))
            findNavController().navigateUp()
        }
        return binding.root
    }

    override fun onDestroyView() {
        super.onDestroyView()
    }

    override fun onDestroy() {
        super.onDestroy()
    }
}