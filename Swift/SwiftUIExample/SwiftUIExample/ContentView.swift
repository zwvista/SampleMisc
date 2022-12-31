//
//  ContentView.swift
//  SwiftUIExample
//
//  Created by cho.i on 2022/04/29.
//

import SwiftUI

struct ContentView: View {
    @State var showNumbers = false
    var body: some View {
        NavigationStack {
            VStack {
                Button("Add Numbers") {
                    showNumbers.toggle()
                }
            }
            .navigationDestination(isPresented: $showNumbers) {
                NumbersView()
            }
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
