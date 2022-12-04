//
//  NSTextField+Combine.swift
//  CombineMacExample
//
//  Created by 趙偉 on 2022/11/29.
//

import Cocoa
import Combine

extension NSButton {
    var tapPublisher: AnyPublisher<Void, Never> {
        Publishers.ControlTarget(control: self,
                                 addTargetAction: { control, target, action in
            control.target = target
            control.action = action
        },
                                 removeTargetAction: { control, _, _ in
            control?.target = nil
            control?.action = nil
        })
        .eraseToAnyPublisher()
    }
}
