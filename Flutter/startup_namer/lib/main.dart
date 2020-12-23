import 'dart:async';

import 'package:flutter/material.dart';
import 'package:english_words/english_words.dart';

void main() => runApp(MyApp());

// #docregion MyApp
class MyApp extends StatelessWidget {
  // #docregion build
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Startup Name Generator',
      theme: ThemeData(
        primaryColor: Colors.orange,
      ),
      home: HomeWidget(),
    );
  }
// #enddocregion build
}
// #enddocregion MyApp

// #docregion RWS-var
class HomeWidget extends StatefulWidget {
  @override
  HomeState createState() => HomeState();
}

class HomeState extends State<HomeWidget> {
  final _onTimeChange = StreamController<TimeOfDay>();

  @override
  void dispose() {
    // StreamControllerは必ず開放する
    _onTimeChange.close();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Column(
      mainAxisAlignment: MainAxisAlignment.center,
      children: <Widget>[
        // 2つのWidgetは親のもつStreamControllerのstreamとsinkを使ってデータの受け渡しを行う
        TimeText(stream: _onTimeChange.stream),
        TimeSelector(sink: _onTimeChange.sink),
      ],
    );
  }
}

/// 時刻を受け取って表示する
class TimeText extends StatelessWidget {
  /// 受け口
  final Stream<TimeOfDay> stream;
  TimeText({this.stream});

  @override
  Widget build(BuildContext context) {
    return Container(
      decoration: BoxDecoration(
        border: Border(
          bottom: BorderSide(width: 3),
        ),
      ),
      child: StreamBuilder(
        // 指定したstreamにデータが流れてくると再描画される
        stream: this.stream,
        builder: (BuildContext context, AsyncSnapshot<TimeOfDay> snapShot) {
          // StreamControllerから流れてきたデータを使って再描画
          return Text(
            snapShot.hasData ? snapShot.data.format(context) : "未選択",
            style: TextStyle(fontSize: 50),
          );
        },
      ),
    );
  }
}

/// 時刻を選択して、選択結果を渡す
class TimeSelector extends StatelessWidget {
  /// 渡し口
  final StreamSink<TimeOfDay> sink;
  TimeSelector({this.sink});

  @override
  Widget build(BuildContext context) {
    return RaisedButton(
      child: const Text(
        "時刻を選択する",
        style: TextStyle(fontSize: 20),
      ),
      color: Colors.deepOrangeAccent,
      onPressed: () async {
        final select = await showTimePicker(
          context: context,
          initialTime: TimeOfDay.now(),
        );
        // sinkに選択した時刻を流す
        this.sink.add(select);
      },
    );
  }
}