package com.zwstudio.composeexample

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material.Button
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.navigation.NavHostController
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.rememberNavController

@Composable
fun AddHost() {
    val vm = remember { AddNumbersViewModel() }
    val navController = rememberNavController()
    NavHost(navController = navController, startDestination = AddScreens.AddMain.route) {
        composable(route = AddScreens.AddMain.route) {
            AddMain(navController = navController)
        }
        composable(route = AddScreens.AddNumbers.route) {
            AddNumbers(vm)
        }
    }
}

@Composable
fun AddMain(navController: NavHostController?) {
    Column(
        modifier = Modifier.fillMaxSize(),
        verticalArrangement = Arrangement.Center,
        horizontalAlignment = Alignment.CenterHorizontally
    ) {
        Button(
            onClick = {
                navController?.navigate(AddScreens.AddNumbers.route)
            }
        ) {
            Text(text = "Add Numbers")
        }
    }
}
