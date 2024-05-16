package com.zwstudio.composeexample

import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.Home
import androidx.compose.ui.graphics.vector.ImageVector

sealed class TabScreens(val route: String, val label: String, val icon: ImageVector) {
    object Home : TabScreens("Home", "Home", Icons.Default.Home)
    object AddNumbers : TabScreens("AddNumbers", "Add", Icons.Default.Add)
}

sealed class AddScreens(val route: String) {
    object AddMain : AddScreens("AddMain")
    object AddNumbers : AddScreens("AddNumbers")
}
