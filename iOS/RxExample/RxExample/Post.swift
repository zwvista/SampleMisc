//
//  Post.swift
//  RestExample
//
//  Created by 趙偉 on 2018/08/23.
//  Copyright © 2018年 趙偉. All rights reserved.
//

import Foundation
import RxSwift

struct Post : Codable {
    let userId: Int
    let id: Int
    let title: String
    let body: String
    var description: String {
        return "Post {userId = \(userId), id = \(id), title = \"\(title)\", body = \"\(body.replacingOccurrences(of: "\n", with: "\\n"))\"}";
    }
    
    static let url = "https://jsonplaceholder.typicode.com/"

    static func getPostAsString() -> Observable<String> {
        return RestApi.getString(url: "\(url)posts/1")
    }
    static func getPostAsJson() -> Observable<Post> {
        return RestApi.getObject(url: "\(url)posts/1")
    }
    static func getPosts(n: Int) -> Observable<Post> {
        return RestApi.getArray(url: "\(url)posts").flatMap { Observable.from($0) }.take(n)
    }
    static func createPost() -> Observable<String> {
        let post = Post(userId: 101, id: 0, title: "test title", body: "test body")
        return RestApi.create(url: "\(url)posts", body: try! post.toJSONString(prettyPrint: false)!)
    }
    static func updatePost() -> Observable<String> {
        let post = Post(userId: 101, id: 1, title: "test title", body: "test body")
        return RestApi.update(url: "\(url)posts/1", body: try! post.toJSONString(prettyPrint: false)!)
    }
    static func deletePost() -> Observable<String> {
        return RestApi.delete(url: "\(url)posts/1")
    }
}
