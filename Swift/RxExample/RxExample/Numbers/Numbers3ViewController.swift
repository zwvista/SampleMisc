//
//  Numbers3ViewController.swift
//  RxExample
//
//  Created by Krunoslav Zaher on 12/6/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import UIKit
import Combine
import CombineCocoa

class Numbers3ViewController: UIViewController {
    @IBOutlet weak var tfNumber1: UITextField!
    @IBOutlet weak var tfNumber2: UITextField!
    @IBOutlet weak var tfNumber3: UITextField!
    @IBOutlet weak var lblResult: UILabel!

    var vm: Numbers3ViewModel!
    var subscriptions = Set<AnyCancellable>()

    override func viewDidLoad() {
        super.viewDidLoad()
        vm = Numbers3ViewModel()
        vm.$number1.assign(to: \Numbers3ViewController.tfNumber1!.text!, on: self).store(in: &subscriptions)
        vm.$number2.assign(to: \Numbers3ViewController.tfNumber2!.text!, on: self).store(in: &subscriptions)
        vm.$number3.assign(to: \Numbers3ViewController.tfNumber3!.text!, on: self).store(in: &subscriptions)
        vm.$result.assign(to: \Numbers3ViewController.lblResult!.text!, on: self).store(in: &subscriptions)
        tfNumber1.textPublisher.compactMap{$0}.assign(to: &vm.$number1)
        tfNumber2.textPublisher.compactMap{$0}.assign(to: &vm.$number2)
        tfNumber3.textPublisher.compactMap{$0}.assign(to: &vm.$number3)
    }

    deinit {
        print("DEBUG: \(String(describing: type(of: self))) deinit")
    }
}

class Numbers3ViewModel: ObservableObject {
    @Published
    var number1 = "1"
    @Published
    var number2 = "2"
    @Published
    var number3 = "3"
    @Published
    var result = ""

    init() {
        $number1.combineLatest($number2, $number3)
            .map { String((Int($0) ?? 0) + (Int($1) ?? 0) + (Int($2) ?? 0)) }
            .assign(to: &$result)
    }

    deinit {
        print("DEBUG: \(String(describing: type(of: self))) deinit")
    }
}
