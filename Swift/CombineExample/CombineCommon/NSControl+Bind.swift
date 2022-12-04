//
//  NSControl+Bind.swift
//  CombineMacExample
//
//  Created by 趙偉 on 2022/12/01.
//

import Cocoa
import Combine

infix operator <~> : DefaultPrecedence
infix operator ~> : DefaultPrecedence

func <~><Control: NSControl, Value> (pub: inout Published<Value>.Publisher, controlProperty: Publishers.ControlProperty2<Control, Value>) -> AnyCancellable {
    controlProperty.control[keyPath: controlProperty.getter].assign(to: &pub)
    return pub.assign(to: controlProperty.setter, on: controlProperty.control)
}
func ~><Control: NSControl, Value> (pub: Published<Value>.Publisher, controlAndKeyPath: (Control, ReferenceWritableKeyPath<Control, Value>)) -> AnyCancellable {
    pub.assign(to: controlAndKeyPath.1, on: controlAndKeyPath.0)
}
