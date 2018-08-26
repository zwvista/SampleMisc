//
//  AppDelegate.swift
//  RxExample
//
//  Created by 趙偉 on 2018/08/26.
//  Copyright © 2018年 趙偉. All rights reserved.
//

import UIKit
import RxSwift

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {

    var window: UIWindow?
    var disposeBag = DisposeBag()

    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey: Any]?) -> Bool {
        // Override point for customization after application launch.
        Post.getPostAsString().do(onNext: { print($0) }).subscribe().disposed(by: disposeBag)
        Post.getPostAsJson().do(onNext: { print($0.description) }).subscribe().disposed(by: disposeBag)
        Post.getPosts(n: 2).do(onNext: { print($0.description) }).subscribe().disposed(by: disposeBag)
        Post.createPost().do(onNext: { print($0) }).subscribe().disposed(by: disposeBag)
        Post.updatePost().do(onNext: { print($0) }).subscribe().disposed(by: disposeBag)
        Post.deletePost().do(onNext: { print($0) }).subscribe().disposed(by: disposeBag)
        return true
    }

    func applicationWillResignActive(_ application: UIApplication) {
        // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
        // Use this method to pause ongoing tasks, disable timers, and invalidate graphics rendering callbacks. Games should use this method to pause the game.
    }

    func applicationDidEnterBackground(_ application: UIApplication) {
        // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later.
        // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
    }

    func applicationWillEnterForeground(_ application: UIApplication) {
        // Called as part of the transition from the background to the active state; here you can undo many of the changes made on entering the background.
    }

    func applicationDidBecomeActive(_ application: UIApplication) {
        // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
    }

    func applicationWillTerminate(_ application: UIApplication) {
        // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
    }


}

