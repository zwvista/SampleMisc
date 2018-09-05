//
//  Post.cpp
//  RestExample
//
//  Created by 趙偉 on 2018/09/05.
//  Copyright © 2018年 趙偉. All rights reserved.
//

#include "Post.hpp"
#include <boost/algorithm/string/replace.hpp>
using namespace std;

void to_json(json& j, const Post& p) {
    j = json{{"userId", p.userId}, {"id", p.id}, {"title", p.title}, {"body", p.body}};
}
void from_json(const json& j, Post& p) {
    p.userId = j.at("userId").get<int>();
    p.id = j.at("id").get<int>();
    p.title = boost::algorithm::replace_all_copy(j.at("title").get<string>(), "\n", "\\n");
    p.body = boost::algorithm::replace_all_copy(j.at("body").get<string>(), "\n", "\\n");
}
std::ostream& operator<<(std::ostream& out, Post& p) {
    out << "userId: " << p.userId << endl
    << "id: " << p.id << endl
    << "title: " << p.title << endl
    << "body: " << p.body << endl;
    return out;
}
