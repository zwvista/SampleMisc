//
//  NumbersViewModel.swift
//  SwiftUIExample
//
//  Created by cho.i on 2022/11/29.
//

import Foundation
import Combine

@MainActor
class NumbersViewModel: ObservableObject {
    @Published
    var number1 = "1"
    @Published
    var number2 = "2"
    @Published
    var number3 = "3"
    @Published
    var result = ""
//    var subscriber: AnyCancellable?

    init() {
//        subscriber = $number1.combineLatest($number2, $number3)
//            .map { String((Int($0) ?? 0) + (Int($1) ?? 0) + (Int($2) ?? 0)) }
//            .sink { self.result = $0 }
        $number1.combineLatest($number2, $number3)
            .map { String((Int($0) ?? 0) + (Int($1) ?? 0) + (Int($2) ?? 0)) }
            .assign(to: &$result)
    }

    deinit {
        print("DEBUG: \(String(describing: type(of: self))) deinit")
    }
}
