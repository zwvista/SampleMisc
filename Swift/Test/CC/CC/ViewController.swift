//
//  ViewController.swift
//  CC
//
//  Created by 趙偉 on 2019/07/29.
//  Copyright © 2019 趙偉. All rights reserved.
//

import UIKit

class ViewController: UIViewController {

    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view.
    }
    @IBAction func unwindToRootViewController(segue: UIStoryboardSegue) {
        print("Unwind to Root View Controller")
    }

}

