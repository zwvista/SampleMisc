//
//  json1.cpp
//  RestExample
//
//  Created by 趙偉 on 2018/09/04.
//  Copyright © 2018年 趙偉. All rights reserved.
//

#include <cpprest/http_client.h>
#include <cpprest/filestream.h>
#include <cpprest/json.h>

#include <iostream>

using namespace utility;                    // Common utilities like string conversions
//using namespace web;                        // Common features like URIs.
using namespace web::http;                  // Common HTTP functionality
using namespace web::http::client;          // HTTP client features
using namespace concurrency::streams;       // Asynchronous streams

#include "rxcpp/rx.hpp"
namespace Rx {
    using namespace rxcpp;
    using namespace rxcpp::sources;
    using namespace rxcpp::operators;
    using namespace rxcpp::util;
}
using namespace Rx;

#include "Post.hpp"
using namespace std;

static void print_results(string_t const & value)
{
    json j = json::parse(value);
    Post p = j;
    cout << p;
}

void json4()
{
    auto o = observable<>::create<string_t>(
        [](subscriber<string_t> s){
            http_client client(U("https://jsonplaceholder.typicode.com/posts/1"));
            
            client
            .request(methods::GET)
            .then([](http_response response) -> pplx::task<string_t> {
                if(response.status_code() == status_codes::OK) {
                    return response.extract_string();
                }
                return pplx::task_from_result(string_t());
            })
            .then([&](pplx::task<string_t> previousTask) {
                try {
                    string_t const & v = previousTask.get();
                    s.on_next(v);
                    s.on_completed();
                } catch (http_exception const & e) {
                    s.on_error(std::current_exception());
                }
            })
            .wait();
        });
    o.subscribe([](string_t v){print_results(v);});
}
