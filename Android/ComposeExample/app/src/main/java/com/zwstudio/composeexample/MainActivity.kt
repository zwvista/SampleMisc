package com.zwstudio.composeexample

import android.annotation.SuppressLint
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.currentBackStackEntryAsState
import androidx.navigation.compose.rememberNavController
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

// https://blog.kumano-te.com/activities/bottom-navigation-with-jetpack-compose
@SuppressLint("UnusedMaterialScaffoldPaddingParameter")
@Composable
fun BottomNavigationExample() {
    val navController = rememberNavController()
    val screens = listOf(TabScreens.Home, TabScreens.AddNumbers)
    Scaffold(
        bottomBar = {
            BottomNavigation {
                screens.forEach { screen ->
                    val navBackStackEntry by navController.currentBackStackEntryAsState()
                    val currentRoute = navBackStackEntry?.destination?.route
                    BottomNavigationItem(
                        icon = { Icon(imageVector = screen.icon, contentDescription = screen.route) },
                        label = { Text(screen.label) },
                        selected = screen.route == currentRoute,
                        modifier = Modifier.padding(8.dp),
                        onClick = {
                            navController.navigate(screen.route) {
                                // Pop up to the start destination of the graph to
                                // avoid building up a large stack of destinations
                                // on the back stack as users select items
                                navController.graph.startDestinationRoute?.let {
                                    popUpTo(it) {
                                        saveState = true
                                    }
                                }
                                // Avoid multiple copies of the same destination when
                                // reselecting the same item
                                launchSingleTop = true
                                // Restore state when reselecting a previously selected item
                                restoreState = true
                            }
                        },
                    )
                }
            }
        }
    ) {
        NavHost(navController, startDestination = TabScreens.Home.route) {
            composable(TabScreens.Home.route) {
                Column(
                    modifier = Modifier.fillMaxSize(),
                    verticalArrangement = Arrangement.Center,
                    horizontalAlignment = Alignment.CenterHorizontally
                ) {
                    Text(text = "Selected Screen: Home")
                }
            }
            composable(TabScreens.AddNumbers.route) { AddHost() }
        }
    }
}


@Preview(showBackground = true)
@Composable
fun DefaultPreview() {
    ComposeExampleTheme {
        AddHost()
    }
}