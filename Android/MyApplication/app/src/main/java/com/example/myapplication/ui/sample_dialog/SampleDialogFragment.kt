package com.example.myapplication.ui.sample_dialog

import android.app.AlertDialog
import android.app.Dialog
import android.os.Bundle
import androidx.fragment.app.DialogFragment
import com.example.myapplication.R
import com.example.myapplication.databinding.FragmentSampleDialogBinding
import com.example.myapplication.ui.autoCleared
import org.koin.androidx.viewmodel.ext.android.viewModel

class SampleDialogFragment : DialogFragment() {

    private val vm: SampleDialogViewModel by viewModel()
    private var binding by autoCleared<FragmentSampleDialogBinding>()

    override fun onCreateDialog(savedInstanceState: Bundle?): Dialog {
        binding = FragmentSampleDialogBinding.inflate(requireActivity().layoutInflater).apply {
            // https://stackoverflow.com/questions/54766112/getviewlifecycleowner-in-dialogfragment-leads-to-crash
            lifecycleOwner = this@SampleDialogFragment
            model = vm
        }
        return AlertDialog.Builder(requireActivity())
            .setTitle(R.string.title_sample_dialog)
            .setView(binding.root)
            .setPositiveButton("OK") { _, _ ->
            }
            .create()
    }
}