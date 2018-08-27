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

class NumbersViewController: UIViewController {
    @IBOutlet weak var tfNumber1: UITextField!
    @IBOutlet weak var tfNumber2: UITextField!
    @IBOutlet weak var tfNumber3: UITextField!
    @IBOutlet weak var lblResult: UILabel!
    
    var disposeBag = DisposeBag()

    override func viewDidLoad() {
        super.viewDidLoad()
//        tfNumberChanged(self)

        Observable.combineLatest(tfNumber1.rx.text.orEmpty, tfNumber2.rx.text.orEmpty, tfNumber3.rx.text.orEmpty) { num1, num2, num3 -> Int in
            (Int(num1) ?? 0) + (Int(num2) ?? 0) + (Int(num3) ?? 0) }
            .map { String($0) }
            .bind(to: lblResult.rx.text)
            .disposed(by: disposeBag)
    }
    
    @IBAction func tfNumberChanged(_ sender: Any) {
//        let num1 = Int(tfNumber1.text!) ?? 0
//        let num2 = Int(tfNumber2.text!) ?? 0
//        let num3 = Int(tfNumber3.text!) ?? 0
//        lblResult.text = String(num1 + num2 + num3)
    }
}
