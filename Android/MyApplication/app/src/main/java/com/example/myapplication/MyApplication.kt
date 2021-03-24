package com.example.myapplication

import android.app.Application
import androidx.fragment.app.Fragment
import androidx.lifecycle.Lifecycle
import androidx.lifecycle.LifecycleEventObserver
import androidx.navigation.fragment.findNavController
import com.androidisland.vita.startVita

class MyApplication : Application() {
    override fun onCreate() {
        super.onCreate()
        startVita()
    }
}

// https://stackoverflow.com/questions/50754523/how-to-get-a-result-from-fragment-using-navigation-architecture-component
// https://stackoverflow.com/questions/56624895/android-jetpack-navigation-component-result-from-dialog/62054347#62054347
fun <T>Fragment.setNavigationResult(value: T, key: String = "result") =
    findNavController().previousBackStackEntry?.savedStateHandle?.set(key, value)

fun <T>Fragment.getNavigationResult(key: String = "result", onResult: (result: T) -> Unit) {
    val navBackStackEntry = findNavController().currentBackStackEntry!!

    val observer = LifecycleEventObserver { _, event ->
        if (event == Lifecycle.Event.ON_RESUME
            && navBackStackEntry.savedStateHandle.contains(key)
        ) {
            val result = navBackStackEntry.savedStateHandle.get<T>(key)
            result?.let(onResult)
            navBackStackEntry.savedStateHandle.remove<T>(key)
        }
    }
    navBackStackEntry.lifecycle.addObserver(observer)

    viewLifecycleOwner.lifecycle.addObserver(LifecycleEventObserver { _, event ->
        if (event == Lifecycle.Event.ON_DESTROY) {
            navBackStackEntry.lifecycle.removeObserver(observer)
        }
    })
}