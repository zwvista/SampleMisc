//
//  NumbersViewController.swift
//  RxExample
//
//  Created by Krunoslav Zaher on 12/6/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import UIKit
import RxSwift
import RxCocoa
@_exported import RxBinding

class NumbersViewController: UIViewController {
    @IBOutlet weak var tfNumber1: UITextField!
    @IBOutlet weak var tfNumber2: UITextField!
    @IBOutlet weak var tfNumber3: UITextField!
    @IBOutlet weak var lblResult: UILabel!
    
    var disposeBag = DisposeBag()
    var vm: NumbersViewModel!

    override func viewDidLoad() {
        super.viewDidLoad()
        vm = NumbersViewModel(disposeBag: disposeBag)
//        tfNumberChanged(self)

//        Observable.combineLatest(tfNumber1.rx.text.orEmpty, tfNumber2.rx.text.orEmpty, tfNumber3.rx.text.orEmpty) { num1, num2, num3 -> Int in
//            (Int(num1) ?? 0) + (Int(num2) ?? 0) + (Int(num3) ?? 0) }
//            .map { String($0) }
//            .bind(to: lblResult.rx.text)
//            .disposed(by: disposeBag)
        
        vm.number1 <~> tfNumber1.rx.textInput ~ disposeBag
        vm.number2 <~> tfNumber2.rx.textInput ~ disposeBag
        vm.number3 <~> tfNumber3.rx.textInput ~ disposeBag
        vm.result ~> lblResult.rx.text ~ disposeBag
    }
    
    @IBAction func tfNumberChanged(_ sender: Any) {
//        let num1 = Int(tfNumber1.text!) ?? 0
//        let num2 = Int(tfNumber2.text!) ?? 0
//        let num3 = Int(tfNumber3.text!) ?? 0
//        lblResult.text = String(num1 + num2 + num3)
    }
}

class NumbersViewModel {
    var number1 = BehaviorRelay(value: "1")
    var number2 = BehaviorRelay(value: "2")
    var number3 = BehaviorRelay(value: "3")
    var result = BehaviorRelay(value: "")
    var disposeBag: DisposeBag!
    
    init(disposeBag: DisposeBag) {
        self.disposeBag = disposeBag
        Observable.combineLatest(number1, number2, number3) { num1, num2, num3 -> Int in
            (Int(num1) ?? 0) + (Int(num2) ?? 0) + (Int(num3) ?? 0) }
            .map { String($0) }
            ~> result
            ~ disposeBag
    }
}
