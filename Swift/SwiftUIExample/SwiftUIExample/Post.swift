//
//  Post.swift
//  SwiftUIExample
//
//  Created by cho.i on 2022/04/29.
//

import Foundation

struct Post : Codable {
    let userId: Int
    let id: Int
    let title: String
    let body: String
    var description: String {
        return "Post {userId = \(userId), id = \(id), title = \"\(title)\", body = \"\(body.replacingOccurrences(of: "\n", with: "\\n"))\"}";
    }
    
    static let url = "https://jsonplaceholder.typicode.com/"

    static func getPostAsString() async -> String {
        await RestApi.getString(url: "\(url)posts/1")
    }
    static func getPostAsJson() async -> Post {
        await RestApi.getObject(url: "\(url)posts/1")
    }
    static func getPosts(n: Int) async -> [Post] {
        let posts: [Post] = await RestApi.getArray(url: "\(url)posts")
        return Array(posts[0..<n])
    }
    static func createPost() async -> String {
        let post = Post(userId: 101, id: 0, title: "test title", body: "test body")
        return await RestApi.create(url: "\(url)posts", body: post)
    }
    static func updatePost() async -> String {
        let post = Post(userId: 101, id: 1, title: "test title", body: "test body")
        return await RestApi.update(url: "\(url)posts/1", body: post)
    }
    static func deletePost() async -> String {
        return await RestApi.delete(url: "\(url)posts/1")
    }
}
