import 'package:rx_command/rx_command.dart';
import 'package:rxdart/rxdart.dart';

class RxExampleViewModel {
  final number1 = RxCommand.createSync((String v) => v, initialLastResult: "1");
  final number2 = RxCommand.createSync((String v) => v, initialLastResult: "2");
  final number3 = RxCommand.createSync((String v) => v, initialLastResult: "3");
  late RxCommand<void, String> result;

  RxExampleViewModel() {
    int f(String s) => int.tryParse(s) ?? 0;
    result = RxCommand.createFromStream((_) => CombineLatestStream.combine3(
        number1.startWith(number1.lastResult!),
        number2.startWith(number2.lastResult!),
        number3.startWith(number3.lastResult!),
        (String a, String b, String c) => (f(a) + f(b) + f(c)).toString()));
    result(result.lastResult);
  }
}
