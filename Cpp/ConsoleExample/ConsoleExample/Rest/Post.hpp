//
//  Post.hpp
//  RestExample
//
//  Created by 趙偉 on 2018/09/05.
//  Copyright © 2018年 趙偉. All rights reserved.
//

#ifndef Post_hpp
#define Post_hpp

#include <iostream>
#include <nlohmann/json.hpp>
using nlohmann::json;

struct Post {
    int userId;
    int id;
    std::string title;
    std::string body;
};
void to_json(json& j, const Post& p);
void from_json(const json& j, Post& p);
std::ostream& operator<<(std::ostream& out, const Post& p);

#endif /* Post_hpp */
