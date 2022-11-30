//
//  ViewController.swift
//  CombineMacExample
//
//  Created by 趙偉 on 2022/11/29.
//

import Cocoa
import Combine

class ViewController: NSViewController {

    @IBOutlet weak var tfNumber1: NSTextField!
    @IBOutlet weak var tfNumber2: NSTextField!
    @IBOutlet weak var tfNumber3: NSTextField!
    @IBOutlet weak var lblResult: NSTextField!
    
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

    override var representedObject: Any? {
        didSet {
        // Update the view, if already loaded.
        }
    }


}


class NumbersViewModel: ObservableObject {
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
