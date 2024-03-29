//
//  SwiftUIExampleApp.swift
//  SwiftUIExample
//
//  Created by cho.i on 2022/04/29.
//

import SwiftUI

@main
struct SwiftUIExampleApp: App {
    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }
    
    init() {
        Task {
            print(await Post.getPostAsString())
            print(await Post.getPostAsJson())
            print(await Post.getPosts(n: 2))
            print(await Post.createPost())
            print(await Post.updatePost())
            print(await Post.deletePost())
        }
    }
}
