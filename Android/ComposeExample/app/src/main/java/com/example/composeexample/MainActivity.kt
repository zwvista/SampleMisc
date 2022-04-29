package com.example.composeexample

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import com.example.composeexample.ui.theme.ComposeExampleTheme

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            ComposeExampleTheme {
                // A surface container using the 'background' color from the theme
                Surface(modifier = Modifier.fillMaxSize(), color = MaterialTheme.colors.background) {
                    AddNumbers()
                }
            }
        }
    }
}

@Composable
fun LeftText(
    text: String = "",
) {
    Text(
        text = text,
        modifier = Modifier.width(40.dp),
        textAlign = TextAlign.Center,
    )
}

@Composable
fun RightTextField(
    value: String,
    onValueChange: (String) -> Unit,
    readOnly: Boolean = false,
) {
    TextField(
        value = value,
        onValueChange = onValueChange,
        readOnly = readOnly,
        modifier = Modifier.width(200.dp),
        textStyle = LocalTextStyle.current.copy(textAlign = TextAlign.End)
    )
}

@Composable
fun AddNumbers(vm: NumbersViewModel = NumbersViewModel()) {
    Column(
        modifier = Modifier.fillMaxSize(),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.Center
    ) {
        Row {
            LeftText()
            RightTextField(
                value = vm.number1.collectAsState().value,
                onValueChange = { vm.number1.value = it },
            )
        }
        Row {
            LeftText()
            RightTextField(
                value = vm.number2.collectAsState().value,
                onValueChange = { vm.number2.value = it }
            )
        }
        Row(verticalAlignment = Alignment.CenterVertically) {
            LeftText("+")
            RightTextField(
                value = vm.number3.collectAsState().value,
                onValueChange = { vm.number3.value = it }
            )
        }
        Row {
            LeftText()
            RightTextField(
                value = vm.result.collectAsState("").value,
                onValueChange = {},
                readOnly = true
            )
        }
    }
}

@Preview(showBackground = true)
@Composable
fun DefaultPreview() {
    ComposeExampleTheme {
        AddNumbers()
    }
}