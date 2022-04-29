//
//  RestApi.swift
//  SwiftUIExample
//
//  Created by cho.i on 2022/04/29.
//

import Foundation
import Alamofire

class RestApi {
    static func getObject<T: Decodable>(url: String) async -> T {
        try! await AF.request(url).serializingDecodable(T.self).value
    }
    static func getArray<T: Decodable>(url: String) async -> [T] {
        try! await AF.request(url).serializingDecodable([T].self).value
    }
    static func update<T: Encodable>(url: String, body: T) async -> String {
        try! await AF.request(url, method: .put, parameters: body).serializingString().value
    }
    static func create<T: Encodable>(url: String, body: T) async -> String {
        try! await AF.request(url, method: .post, parameters: body).serializingString().value
    }
    static func delete(url: String) async -> String {
        try! await AF.request(url, method: .delete).serializingString().value
    }
    static func getString(url: String) async -> String {
        try! await AF.request(url).serializingString().value
    }

}
