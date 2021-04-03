package com.example.myapplication.ui.home

import android.os.Bundle
import android.view.*
import android.widget.Toast
import androidx.fragment.app.Fragment
import androidx.fragment.app.setFragmentResultListener
import androidx.navigation.fragment.findNavController
import com.androidisland.vita.VitaOwner
import com.androidisland.vita.vita
import com.example.myapplication.R
import com.example.myapplication.databinding.FragmentHomeBinding
import com.example.myapplication.items
import com.example.myapplication.makeCustomAdapter
import com.example.myapplication.ui.autoCleared

class HomeFragment : Fragment() {

    private val homeViewModel by lazy { vita.with(VitaOwner.Multiple(this)).getViewModel<HomeViewModel>() }
    private var binding by autoCleared<FragmentHomeBinding>()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setHasOptionsMenu(true)
    }

    override fun onCreateView(inflater: LayoutInflater, container: ViewGroup?, savedInstanceState: Bundle?): View? {
        binding = FragmentHomeBinding.inflate(inflater, container, false).apply {
            lifecycleOwner = viewLifecycleOwner
            model = homeViewModel
        }
        return binding.root
    }

    override fun onViewCreated(view: View, savedInstanceState: Bundle?) {
        super.onViewCreated(view, savedInstanceState)
        binding.button.setOnClickListener {
            val a = HomeFragmentDirections.actionNavHomeToNavHome2()
            findNavController().navigate(a)
        }
        binding.spinner.adapter = makeCustomAdapter(requireContext(), items) { it.label }
        homeViewModel.itemPosition.observe(viewLifecycleOwner) {
            Toast.makeText(requireContext(), "Result from spinner: ${items[it].value}", Toast.LENGTH_SHORT).show()
        }
        setFragmentResultListener("requestKey") { requestKey, bundle ->
            val result = bundle.getString("bundleKey")
            Toast.makeText(requireContext(), "Result from home2: $result", Toast.LENGTH_SHORT).show()
        }
    }

    override fun onCreateOptionsMenu(menu: Menu, inflater: MenuInflater) {
        inflater.inflate(R.menu.home, menu)
        super.onCreateOptionsMenu(menu, inflater)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean =
        when (item.itemId) {
            R.id.action_settings -> {
                Toast.makeText(requireContext(), "Settings", Toast.LENGTH_SHORT).show()
                true
            }
            R.id.action_settings2 -> {
                Toast.makeText(requireContext(), "Settings2", Toast.LENGTH_SHORT).show()
                true
            }
            else -> super.onOptionsItemSelected(item)
        }
}