//
//  ControlViewModel.swift
//  CombineMacExample
//
//  Created by 趙偉 on 2022/12/03.
//

import Foundation

@MainActor
class ControlViewModel: ObservableObject {
    @Published var letter = "C"
    @Published var letterIndex = 2
    @Published var itemIndex = 2
    @Published var isOn = true
}
