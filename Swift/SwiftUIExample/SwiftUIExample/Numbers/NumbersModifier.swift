//
//  NumbersModifier.swift
//  SwiftUIExample
//
//  Created by cho.i on 2022/04/29.
//

import SwiftUI

struct LeftModifier: ViewModifier {
    func body(content: Content) -> some View {
        content
            .padding(8)
            .multilineTextAlignment(.trailing)
            .frame(width: 150, alignment: .trailing)
    }
}

struct RightModifier: ViewModifier {
    func body(content: Content) -> some View {
        content
            .modifier(LeftModifier())
            .keyboardType(.numberPad)
            .overlay(
                RoundedRectangle(cornerRadius: 14)
                    .stroke(Color.green, lineWidth: 2)
            )
    }
}
