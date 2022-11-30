//
//  ViewController.swift
//  CombineUIKitExample
//
//  Created by 趙偉 on 2022/11/30.
//

import UIKit
import Combine
import CombineCocoa

class ViewController: UIViewController {
    @IBOutlet weak var tfNumber1: UITextField!
    @IBOutlet weak var tfNumber2: UITextField!
    @IBOutlet weak var tfNumber3: UITextField!
    @IBOutlet weak var lblResult: UILabel!

    var vm: NumbersViewModel!
    var subscriptions = Set<AnyCancellable>()

    override func viewDidLoad() {
        super.viewDidLoad()
        vm = NumbersViewModel()
        vm.$number1 <~> tfNumber1 ~ subscriptions
        vm.$number2 <~> tfNumber2 ~ subscriptions
        vm.$number3 <~> tfNumber3 ~ subscriptions
        vm.$result ~> lblResult ~ subscriptions
    }

    deinit {
        print("DEBUG: \(String(describing: type(of: self))) deinit")
    }
}
