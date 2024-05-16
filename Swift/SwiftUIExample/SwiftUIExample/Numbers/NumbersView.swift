//
//  NumbersView.swift
//  SwiftUIExample
//
//  Created by cho.i on 2022/04/29.
//

import SwiftUI

struct NumbersView: View {
    @StateObject var vm = NumbersViewModel()
    var body: some View {
        VStack {
            HStack {
                Text("")
                    .modifier(LeftModifier())
                TextField("Number1", text: $vm.number1)
                    .modifier(RightModifier())
            }
            HStack {
                Text("")
                    .modifier(LeftModifier())
                TextField("Number2", text: $vm.number2)
                    .modifier(RightModifier())
            }
            HStack {
                Text("+")
                    .modifier(LeftModifier())
                TextField("Number3", text: $vm.number3)
                    .modifier(RightModifier())
            }
            Divider()
            HStack {
                Text("")
                    .modifier(LeftModifier())
                TextField("Result", text: $vm.result)
                    .disabled(true)
                    .modifier(RightModifier())
            }
        }.frame(width: 200, height: 200, alignment: .center)
    }
}

struct NumbersView_Previews: PreviewProvider {
    static var previews: some View {
        NumbersView()
    }
}
