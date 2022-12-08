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

    var vm = NumbersViewModel()
    var subscriptions = Set<AnyCancellable>()

    override func viewDidLoad() {
        super.viewDidLoad()
        vm.$number1 <~> tfNumber1.textProperty ~ subscriptions
        vm.$number2 <~> tfNumber2.textProperty ~ subscriptions
        vm.$number3 <~> tfNumber3.textProperty ~ subscriptions
        vm.$result ~> (lblResult, \.text!) ~ subscriptions
    }

    deinit {
        print("DEBUG: \(String(describing: type(of: self))) deinit")
    }
}
