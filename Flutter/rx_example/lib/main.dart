import 'package:flutter/material.dart';
import 'package:rx_example/rx_example_viewmodel.dart';

void main() {
  runApp(MyApp());
}

class MyApp extends StatelessWidget {
  // This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Flutter Demo',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: RxExamplePage(title: 'Rx Example Page'),
    );
  }
}

class RxExamplePage extends StatefulWidget {
  RxExamplePage({Key key, this.title}) : super(key: key);

  final String title;

  @override
  _RxExamplePageState createState() => _RxExamplePageState();
}

class _RxExamplePageState extends State<RxExamplePage> {
  final vm = RxExampleViewModel();
  TextEditingController number1Ctrl;
  TextEditingController number2Ctrl;
  TextEditingController number3Ctrl;

  _RxExamplePageState() {
    number1Ctrl = TextEditingController(text: vm.number1.lastResult);
    number2Ctrl = TextEditingController(text: vm.number2.lastResult);
    number3Ctrl = TextEditingController(text: vm.number3.lastResult);
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(widget.title),
      ),
      body: Center(
        child: Container(
          width: 150,
          child: Table(
              columnWidths: {0: FractionColumnWidth(.2)},
              defaultVerticalAlignment: TableCellVerticalAlignment.middle,
              children: [
                TableRow(children: [
                  Container(),
                  TextField(
                      controller: number1Ctrl,
                      textAlign: TextAlign.end,
                      onChanged: vm.number1)
                ]),
                TableRow(children: [
                  Container(),
                  TextField(
                      controller: number2Ctrl,
                      textAlign: TextAlign.end,
                      onChanged: vm.number2)
                ]),
                TableRow(children: [
                  Center(child: Text("+")),
                  TextField(
                      controller: number3Ctrl,
                      textAlign: TextAlign.end,
                      onChanged: vm.number3)
                ]),
                TableRow(children: [
                  Center(child: Text("=")),
                  StreamBuilder(
                    stream: vm.result,
                    builder: (context, snapshot) =>
                        Text(vm.result.lastResult, textAlign: TextAlign.end),
                  ),
                ]),
              ]),
        ),
      ),
    );
  }
}
