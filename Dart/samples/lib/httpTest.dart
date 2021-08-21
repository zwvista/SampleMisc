import 'dart:convert';

import 'package:json_annotation/json_annotation.dart';
import 'package:http/http.dart' as http;

part 'httpTest.g.dart';

@JsonSerializable()
class Post {
  int userId = 0;
  int id = 0;
  String title = "";
  String body = "";

  Post() {}
  factory Post.fromJson(Map<String, dynamic> json) => _$PostFromJson(json);
  Map<String, dynamic> toJson() => _$PostToJson(this);
  @override
  String toString() => 'Post {userId = $userId, id = $id, title = "$title", body = "${body.replaceAll("\n", r"\n")}"}';
}

String baseUrl = 'http://jsonplaceholder.typicode.com/';

Future<String> getPostAsString() async {
  final response = await http.get(Uri(scheme: "${baseUrl}posts/1"));
  return response.body;
}

Future<Post> getPostAsJson() async {
  final response = await http.get(Uri(scheme: "${baseUrl}posts/1"));
  return Post.fromJson(json.decode(response.body));
}

Future<List<Post>> getPosts() async {
  final response = await http.get(Uri(scheme: "${baseUrl}posts"));
  final j = json.decode(response.body);
  return (j as List).map((e) => Post.fromJson(e)).take(2).toList();
}

Future<String> createPost() async {
  final o = Post()
    ..id = 0
    ..userId = 1
    ..title = 'test title'
    ..body = 'test body';
  final body = json.encode(o);
  print(body);
  final response = await http.post(Uri(scheme: "${baseUrl}posts"), body: body);
  return response.body;
}

Future<String> updatePost() async {
  final o = Post()
    ..id = 1
    ..userId = 1
    ..title = 'test title'
    ..body = 'test body';
  final body = json.encode(o);
  print(body);
  final response = await http.put(Uri(scheme: "${baseUrl}posts/1"), body: body);
  return response.body;
}

Future<String> deletePost() async {
  final response = await http.delete(Uri(scheme: "${baseUrl}posts/1"));
  return response.body;
}

Future httpTest() async {
  print(await getPostAsString());
  print(await getPostAsJson());
  print(await getPosts());
  print(await createPost());
  print(await updatePost());
  print(await deletePost());
}
