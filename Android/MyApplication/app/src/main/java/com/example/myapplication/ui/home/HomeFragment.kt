package com.example.myapplication.ui.home

import android.os.Bundle
import android.view.*
import android.widget.Toast
import androidx.fragment.app.Fragment
import androidx.navigation.fragment.findNavController
import com.androidisland.vita.VitaOwner
import com.androidisland.vita.vita
import com.example.myapplication.R
import com.example.myapplication.databinding.FragmentHomeBinding
import com.example.myapplication.getNavigationResult
import com.example.myapplication.ui.autoCleared

class HomeFragment : Fragment() {

    private val homeViewModel by lazy { vita.with(VitaOwner.Multiple(this)).getViewModel<HomeViewModel>() }
    private var binding by autoCleared<FragmentHomeBinding>()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setHasOptionsMenu(true)
    }

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        binding = FragmentHomeBinding.inflate(inflater, container, false).apply {
            lifecycleOwner = viewLifecycleOwner
            model = homeViewModel
        }
        binding.button.setOnClickListener {
            val a = HomeFragmentDirections.actionNavHomeToNavHome2()
            findNavController().navigate(a)
        }
        getNavigationResult<String> {
            Toast.makeText(requireContext(), "Result from home2: $it", Toast.LENGTH_SHORT).show()
        }
        return binding.root
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