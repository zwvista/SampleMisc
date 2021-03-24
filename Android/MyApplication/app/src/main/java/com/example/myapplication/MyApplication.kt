package com.example.myapplication

import android.app.Application
import com.androidisland.vita.startVita

class MyApplication : Application() {
    override fun onCreate() {
        super.onCreate()
        startVita()
    }
}