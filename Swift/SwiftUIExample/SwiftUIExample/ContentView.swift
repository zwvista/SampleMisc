//
//  ContentView.swift
//  SwiftUIExample
//
//  Created by cho.i on 2022/04/29.
//

import SwiftUI

struct ContentView: View {
    @State var page : Int? = 0
    var body: some View {
        NavigationView {
            VStack {
                NavigationLink(destination: NumbersView(),
                    tag: 1, selection: $page) { EmptyView() }
                Button(action : {
                   page = 1
                }){
                   Text("Add Numbers")
                }
            }
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
