//
//  IncrementalSearchViewController.swift
//  RxExample
//
//  Created by 趙偉 on 2020/08/07.
//  Copyright © 2020 趙偉. All rights reserved.
//

// https://dev.classmethod.jp/articles/incremental-search-using-rxswift-and-rxcocoa/

import UIKit
import RxSwift
import RxCocoa

struct User {
    let name: String
}

class IncrementalSearchViewController: UIViewController {
    @IBOutlet weak var searchBar: UISearchBar!
    @IBOutlet weak var tableView: UITableView!

    private let allUsers = [
        User(name: "かとう"),
        User(name: "たなか"),
        User(name: "ひらや"),
        User(name: "おおはし"),
        User(name: "やまもと"),
        User(name: "ふかさわ"),
        User(name: "さいとう"),
        User(name: "くどう"),
        User(name: "すわ"),
        User(name: "わたなべ")
    ]

    private var filteredUsers = [User]()

    private let disposeBag = DisposeBag()

    override func viewDidLoad() {
        super.viewDidLoad()

        setup()
    }
}

// MARK: - Private
private extension IncrementalSearchViewController {
    func setup() {
        filteredUsers = allUsers
        tableView.dataSource = self
        tableView.keyboardDismissMode = .onDrag

        setupSearchBar()
    }

    func setupSearchBar() {
        searchBar.delegate = self

        let debounceInterval = 300

        // インクリメンタルサーチのテキストを取得するためのObservable
        let incrementalSearchTextObservable = rx
            // UISearchBarに文字列入力中に呼ばれるUISearchBarDelegateのメソッドをフック
            .methodInvoked(#selector(UISearchBarDelegate.searchBar(_:shouldChangeTextIn:replacementText:)))
            // searchBar.textの値が確定するまで0.3待つ
            .debounce(DispatchTimeInterval.milliseconds(debounceInterval), scheduler: MainScheduler.instance)
            // 確定したsearchBar.textを取得
            .flatMap { [unowned self] _ in Observable.just(self.searchBar.text ?? "") }

        // UISearchBarのクリア（×）ボタンや確定ボタンタップにテキストを取得するためのObservable
        let textObservable = searchBar.rx.text.orEmpty.asObservable()

        // 2つのObservableをマージ
        let searchTextObservable = Observable.merge(incrementalSearchTextObservable, textObservable)
            // 初期化時に空文字が流れてくるので無視
            .skip(1)
            // 0.3秒経過したら入力確定とみなす
            .debounce(DispatchTimeInterval.milliseconds(debounceInterval), scheduler: MainScheduler.instance)
            // 変化があるまで文字列が流れないようにする、つまり連続して同じテキストで検索しないようにする。
            .distinctUntilChanged()

        // subscribeして流れてくるテキストを使用して検索
        searchTextObservable.subscribe(onNext: { [unowned self] text in
            if text.isEmpty {
                // 空文字の場合は全件表示
                self.filteredUsers = self.allUsers
            } else {
                // 入力文字列がある場合はデータをフィルタリングして表示
                self.filteredUsers = self.allUsers.filter { $0.name.contains(text) }
            }
            self.tableView.reloadData()
        }).disposed(by: disposeBag)
    }
}

// MARK: - UITableViewDataSource
extension IncrementalSearchViewController: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return filteredUsers.count
    }

    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "Cell", for: indexPath)
        cell.textLabel?.text = filteredUsers[indexPath.row].name
        return cell
    }
}
// MARK: - UISearchBarDelegate
extension IncrementalSearchViewController: UISearchBarDelegate {
    func searchBar(_ searchBar: UISearchBar, shouldChangeTextIn range: NSRange, replacementText text: String) -> Bool {
        return true
    }
}
