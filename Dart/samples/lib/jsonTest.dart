import 'dart:convert';

import 'package:json_annotation/json_annotation.dart';

part 'jsonTest.g.dart';

@JsonSerializable()
class Persons {
  List<Person> persons = [];

  Persons() {}
  factory Persons.fromJson(Map<String, dynamic> json) => _$PersonsFromJson(json);
  Map<String, dynamic> toJson() => _$PersonsToJson(this);
  @override
  String toString() => "persons=$persons";
}

@JsonSerializable()
class Person {
  @JsonKey(name: 'name')
  String name = "";
  int age = 0;

  Person() {}
  factory Person.fromJson(Map<String, dynamic> json) => _$PersonFromJson(json);
  Map<String, dynamic> toJson() => _$PersonToJson(this);
  @override
  String toString() => "name=$name,age=$age";
}

void jsonTest() {
  final jsonString = """
{
  "persons" : [
    {
      "name" : "Joe",
      "age" : 12
    }
  ]
}
""";
  final o = Persons.fromJson(jsonDecode(jsonString) as Map<String, dynamic>);
  print(o);
  final s = jsonEncode(o);
  print(s);
  final s2 = JsonEncoder.withIndent("  ").convert(o);
  print(s2);
}

/*
persons=[name=Joe,age=12]
{"persons":[{"name":"Joe","age":12}]}
{
  "persons": [
    {
      "name": "Joe",
      "age": 12
    }
  ]
}
*/
