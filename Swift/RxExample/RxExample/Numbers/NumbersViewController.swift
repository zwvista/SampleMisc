//
//  NumbersViewController.swift
//  RxExample
//
//  Created by Krunoslav Zaher on 12/6/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import UIKit

class NumbersViewController: UIViewController {
    @IBOutlet weak var tfNumber1: UITextField!
    @IBOutlet weak var tfNumber2: UITextField!
    @IBOutlet weak var tfNumber3: UITextField!
    @IBOutlet weak var lblResult: UILabel!
    
    override func viewDidLoad() {
        super.viewDidLoad()
        tfNumberChanged(self)
    }
    
    @IBAction func tfNumberChanged(_ sender: Any) {
        let num1 = Int(tfNumber1.text!) ?? 0
        let num2 = Int(tfNumber2.text!) ?? 0
        let num3 = Int(tfNumber3.text!) ?? 0
        lblResult.text = String(num1 + num2 + num3)
    }
    
    deinit {
        print("DEBUG: \(String(describing: type(of: self))) deinit")
    }
}
