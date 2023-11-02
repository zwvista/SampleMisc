package com.zwstudio.composeexample

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.width
import androidx.compose.material.LocalTextStyle
import androidx.compose.material.Text
import androidx.compose.material.TextField
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp


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
fun AddNumbers() {
    val vm = remember { NumbersViewModel() }
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
            LeftText("=")
            RightTextField(
                value = vm.result.collectAsState("").value,
                onValueChange = {},
                readOnly = true
            )
        }
    }
}
