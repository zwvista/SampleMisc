//
//  json2.cpp
//  RestExample
//
//  Created by 趙偉 on 2018/09/05.
//  Copyright © 2018年 趙偉. All rights reserved.
//

#include <iostream>
#include <nlohmann/json.hpp>
using nlohmann::json;
using namespace std;

struct Person {
    string name;
    int age;
};
struct Persons {
    vector<Person> persons;
};
void to_json(json& j, const Person& p) {
    j = json{{"name", p.name}, {"age", p.age}};
}
void from_json(const json& j, Person& p) {
    p.name = j.at("name").get<string>();
    p.age = j.at("age").get<int>();
}
void to_json(json& j, const Persons& ps) {
    j = json{{"persons", ps.persons}};
}
void from_json(const json& j, Persons& ps) {
    ps.persons = j.at("persons").get<vector<Person>>();
}

string jsonString = R"({
  "persons" : [
    {
      "name" : "Joe",
      "age" : 12
    }
  ]
})";

void json2()
{
    json j = json::parse(jsonString);
    Persons ps = j;
    auto &p = ps.persons[0];
    cout << "name: " << p.name << endl
        << "age: " << p.age << endl;
    j = ps;
    cout << j << endl;
    cout << j.dump(2) << endl;
}
