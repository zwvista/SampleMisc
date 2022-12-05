//
//  ControlViewModel.swift
//  CombineMacExample
//
//  Created by 趙偉 on 2022/12/03.
//

import Foundation

class ControlViewModel: ObservableObject {
    @Published var letter = "B"
    @Published var letterIndex = 2
    @Published var itemIndex = 2
    @Published var isOn = true
}
