// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'jsonTest.dart';

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Persons _$PersonsFromJson(Map<String, dynamic> json) => Persons()
  ..persons = (json['persons'] as List<dynamic>)
      .map((e) => Person.fromJson(e as Map<String, dynamic>))
      .toList();

Map<String, dynamic> _$PersonsToJson(Persons instance) => <String, dynamic>{
      'persons': instance.persons,
    };

Person _$PersonFromJson(Map<String, dynamic> json) => Person()
  ..name = json['name'] as String
  ..age = json['age'] as int;

Map<String, dynamic> _$PersonToJson(Person instance) => <String, dynamic>{
      'name': instance.name,
      'age': instance.age,
    };
