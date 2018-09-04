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
#include <boost/algorithm/string/replace.hpp>

#include <iostream>

using namespace utility;                    // Common utilities like string conversions
using namespace web;                        // Common features like URIs.
using namespace web::http;                  // Common HTTP functionality
using namespace web::http::client;          // HTTP client features
using namespace concurrency::streams;       // Asynchronous streams

using namespace std;

void print_results(json::value const & value)
{
    if(!value.is_null()) {
        auto userId = value.at(U("userId")).as_integer();
        auto id = value.at(U("id")).as_integer();
        auto title = boost::algorithm::replace_all_copy(value.at(U("body")).as_string(), "\n", "\\n");
        auto body = boost::algorithm::replace_all_copy(value.at(U("body")).as_string(), "\n", "\\n");

        cout << "userId: " << userId << endl
            << "id: " << id << endl
            << "title: " << title << endl
            << "body: " << body << endl;
    }
}

void json1()
{
    http_client client(U("https://jsonplaceholder.typicode.com/posts/1"));
    
    client
    // send the HTTP GET request asynchronous
    .request(methods::GET)
    // continue when the response is available
    .then([](http_response response) -> pplx::task<json::value> {
        // if the status is OK extract the body of the response into a JSON value
        // works only when the content type is application\json
        if(response.status_code() == status_codes::OK) {
          return response.extract_json();
        }

        // return an empty JSON value
        return pplx::task_from_result(json::value());
    })
    // continue when the JSON value is available
    .then([](pplx::task<json::value> previousTask) {
        // get the JSON value from the task and display content from it
        try {
            json::value const & v = previousTask.get();
            print_results(v);
        } catch (http_exception const & e) {
            printf("Error exception:%s\n", e.what());
        }
    })
    .wait();
}
