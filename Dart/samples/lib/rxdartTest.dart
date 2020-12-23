import 'package:rxdart/rxdart.dart';

void _test1() {
  final subject = PublishSubject<int>();
  subject.stream.listen(print); // prints 1,2,3
  subject.add(1);
  subject.add(2);
  subject.stream.listen(print); // prints 3
  subject.add(3);
  subject.close();
}

void _test2() {
  final subject = BehaviorSubject<int>();
  subject.stream.listen(print); // prints 1,2,3
  subject.add(1);
  subject.add(2);
  subject.add(3);
  subject.stream.listen(print); // prints 3
}

void _test3() {
  final subject = ReplaySubject<int>();
  subject.add(1);
  subject.add(2);
  subject.add(3);
  subject.stream.listen(print); // prints 1,2,3
}

void rxdartTest() {
  _test1();
  _test2();
  _test3();
}
