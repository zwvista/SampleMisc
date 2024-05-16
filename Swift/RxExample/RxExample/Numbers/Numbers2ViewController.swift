//
//  Numbers2ViewController.swift
//  RxExample
//
//  Created by Krunoslav Zaher on 12/6/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import UIKit
import RxSwift
import RxCocoa
@_exported import RxBinding
import NSObject_Rx

class Numbers2ViewController: UIViewController {
    @IBOutlet weak var tfNumber1: UITextField!
    @IBOutlet weak var tfNumber2: UITextField!
    @IBOutlet weak var tfNumber3: UITextField!
    @IBOutlet weak var lblResult: UILabel!

    var vm: Numbers2ViewModel!

    override func viewDidLoad() {
        super.viewDidLoad()
//        Observable.combineLatest(tfNumber1.rx.text.orEmpty, tfNumber2.rx.text.orEmpty, tfNumber3.rx.text.orEmpty) { num1, num2, num3 -> Int in
//            (Int(num1) ?? 0) + (Int(num2) ?? 0) + (Int(num3) ?? 0) }
//            .map { String($0) }
//            .bind(to: lblResult.rx.text)
//            .disposed(by: rx.disposeBag)

        vm = Numbers2ViewModel()
        _ = vm.number1 <~> tfNumber1.rx.textInput
        _ = vm.number2 <~> tfNumber2.rx.textInput
        _ = vm.number3 <~> tfNumber3.rx.textInput
        _ = vm.result ~> lblResult.rx.text
    }

    deinit {
        print("DEBUG: \(String(describing: type(of: self))) deinit")
    }
}

class Numbers2ViewModel: NSObject {
    var number1 = BehaviorRelay(value: "1")
    var number2 = BehaviorRelay(value: "2")
    var number3 = BehaviorRelay(value: "3")
    var result = BehaviorRelay(value: "")

    override init() {
        super.init()
        _ = Observable.combineLatest(number1, number2, number3) { num1, num2, num3 -> Int in
            (Int(num1) ?? 0) + (Int(num2) ?? 0) + (Int(num3) ?? 0) }
            .map { String($0) }
            ~> result
    }

    deinit {
        print("DEBUG: \(String(describing: type(of: self))) deinit")
    }
}
