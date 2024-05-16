//
//  ContentView.swift
//  CombineSwiftUIExample
//
//  Created by 趙偉 on 2022/11/30.
//

import SwiftUI

struct ContentView: View {
    var body: some View {
        VStack {
            NumbersView()
        }
        .padding()
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
