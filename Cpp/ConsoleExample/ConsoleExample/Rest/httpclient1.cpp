//
//  httpclient1.cpp
//  RestExample
//
//  Created by 趙偉 on 2018/09/04.
//  Copyright © 2018年 趙偉. All rights reserved.
//

#include <cpprest/http_client.h>
#include <cpprest/filestream.h>

using namespace utility;                    // Common utilities like string conversions
using namespace web;                        // Common features like URIs.
using namespace web::http;                  // Common HTTP functionality
using namespace web::http::client;          // HTTP client features
using namespace concurrency::streams;       // Asynchronous streams

void httpclient1()
{
    auto fileStream = std::make_shared<ostream>();
    
    // Open stream to output file.
    pplx::task<void> requestTask = fstream::open_ostream(U("results.html")).then([=](ostream outFile) {
         *fileStream = outFile;
        
         uri a(U("http://www.bing.com/"));
         // Create http_client to send the request.
         http_client client(a);

         uri b(U("/search"));
        
         // Build request URI and start the request.
         uri_builder builder(b);
         builder.append_query(U("q"), U("cpprestsdk github"));
         return client.request(methods::GET, builder.to_string());
    })
    
    // Handle response headers arriving.
    .then([=](http_response response) {
        printf("Received response status code:%u\n", response.status_code());

        // Write response body into the file.
        return response.body().read_to_end(fileStream->streambuf());
    })
    
    // Close the file stream.
    .then([=](size_t) {
        return fileStream->close();
    });
    
    // Wait for all the outstanding I/O to complete and handle any exceptions
    try {
        requestTask.wait();
    } catch (const std::exception &e) {
        printf("Error exception:%s\n", e.what());
    }
}
