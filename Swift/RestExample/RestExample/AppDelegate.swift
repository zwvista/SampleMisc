//
//  AppDelegate.swift
//  RestExample
//
//  Created by 趙偉 on 2018/08/23.
//  Copyright © 2018年 趙偉. All rights reserved.
//

import Cocoa
import RxSwift

@NSApplicationMain
class AppDelegate: NSObject, NSApplicationDelegate {



    func applicationDidFinishLaunching(_ aNotification: Notification) {
        // Insert code here to initialize your application
        Post.getPostAsString().do(onNext: { print($0) }).subscribe()
        Post.getPostAsJson().do(onNext: { print($0.description) }).subscribe()
        Post.getPosts(n: 2).do(onNext: { print($0.description) }).subscribe()
        Post.createPost().do(onNext: { print($0) }).subscribe()
        Post.updatePost().do(onNext: { print($0) }).subscribe()
        Post.deletePost().do(onNext: { print($0) }).subscribe()
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        // Insert code here to tear down your application
    }


}

