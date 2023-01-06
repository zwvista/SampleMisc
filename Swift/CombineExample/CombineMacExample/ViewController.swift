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
    @IBOutlet weak var scLetter: NSSegmentedControl!
    @IBOutlet weak var pbItems: NSPopUpButton!
    @IBOutlet weak var button1: NSButton!
    @IBOutlet weak var check1: NSButton!
    
    var vm = NumbersViewModel()
    var vm2 = ControlViewModel()
    var subscriptions = Set<AnyCancellable>()

    override func viewDidLoad() {
        super.viewDidLoad()
        vm.$number1 <~> tfNumber1.textProperty ~ subscriptions
        vm.$number2 <~> tfNumber2.textProperty ~ subscriptions
        vm.$number3 <~> tfNumber3.textProperty ~ subscriptions
        vm.$result ~> (lblResult, \.stringValue) ~ subscriptions

        vm2.$letterIndex <~> scLetter.selectedSegmentProperty ~ subscriptions
        vm2.$letterIndex.didSet.sink {
            print($0)
        } ~ subscriptions

//        vm2.$letter <~> scLetter.selectedLabelProperty ~ subscriptions
//        vm2.$letter.didSet.sink {
//            print($0)
//        } ~ subscriptions

        vm2.$itemIndex <~> pbItems.selectedItemIndexProperty ~ subscriptions
        vm2.$itemIndex.sink {
            print($0)
        } ~ subscriptions

        vm2.$isOn <~> check1.isOnProperty ~ subscriptions
        vm2.$isOn.sink {
            print($0)
        } ~ subscriptions

        button1.tapPublisher.sink {
            print(self.vm2.letterIndex)
        } ~ subscriptions
    }

}
