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
