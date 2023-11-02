package com.zwstudio.composeexample

import android.annotation.SuppressLint
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.AccountBox
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.Done
import androidx.compose.material.icons.filled.Home
import androidx.compose.material.icons.filled.Notifications
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import com.zwstudio.composeexample.ui.theme.ComposeExampleTheme

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            ComposeExampleTheme {
                // A surface container using the 'background' color from the theme
                Surface(
                    modifier = Modifier.fillMaxSize(),
                    color = MaterialTheme.colors.background
                ) {
                    BottomNavigationExample()
                }
            }
        }
    }
}

@SuppressLint("UnusedMaterialScaffoldPaddingParameter")
@Composable
fun BottomNavigationExample() {
    val screens = listOf("Home", "Add", "Feed", "Alert", "Jobs")
    var selectedScreen by remember { mutableStateOf(screens.first()) }

    Scaffold(
        bottomBar = {
            BottomNavigation {
                screens.forEach { screen ->
                    BottomNavigationItem(
                        icon = { Icon(getIconForScreen(screen), contentDescription = screen) },
                        label = { Text(screen) },
                        selected = screen == selectedScreen,
                        onClick = { selectedScreen = screen },
                        modifier = Modifier.padding(8.dp)
                    )
                }
            }
        },
        content = {
            if (selectedScreen == "Add")
                AddNumbers()
            else
                Column(
                    modifier = Modifier.fillMaxSize(),
                    verticalArrangement = Arrangement.Center,
                    horizontalAlignment = Alignment.CenterHorizontally
                ) {
                    Text(text = "Selected Screen: $selectedScreen")
                }
        }
    )
}

@Composable
fun getIconForScreen(screen: String): ImageVector {
    return when (screen) {
        "Home" -> Icons.Default.Home
        "Add" -> Icons.Default.Add
        "Feed" -> Icons.Default.AccountBox
        "Alert" -> Icons.Default.Notifications
        "Jobs" -> Icons.Default.Done
        else -> Icons.Default.Home
    }
}


@Preview(showBackground = true)
@Composable
fun DefaultPreview() {
    ComposeExampleTheme {
        AddNumbers()
    }
}