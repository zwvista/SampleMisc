//
//  json1.cpp
//  RestExample
//
//  Created by 趙偉 on 2018/09/04.
//  Copyright © 2018年 趙偉. All rights reserved.
//

#include "Post.hpp"
#include "RestApi.hpp"

using namespace std;

void json3()
{
    RestApi<Post> api(U("https://jsonplaceholder.typicode.com/"));
    api.getString(uri_builder(U("posts/1")).to_string()).subscribe([](string_t v){
        cout << v << endl;
    });
    api.getObject(uri_builder(U("posts/1")).to_string()).subscribe([](Post v){
        cout << v << endl;
    });
    Post o;
    o.id = 0;
    o.userId = 101;
    o.title = U("test title");
    o.body = U("test body");
    api.createObject(U("posts"), o).subscribe([](string_t v){
        cout << v << endl;
    });
    api.updateObject(U("posts/1"), o).subscribe([](string_t v){
        cout << v << endl;
    });
    api.deleteObject(U("posts/1")).subscribe([](string_t v){
        cout << v << endl;
    });
}
