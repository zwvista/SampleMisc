//
//  RestApi.swift
//  LollySwiftiOS
//
//  Created by 趙偉 on 2017/06/24.
//  Copyright © 2017年 趙 偉. All rights reserved.
//

import Foundation
import Alamofire
import RxSwift
import RxAlamofire

// https://stackoverflow.com/questions/27855319/post-request-with-a-simple-string-in-body-with-alamofire
class StringEncoding: ParameterEncoding {
    let body: String;
    
    public init(body: String) {
        self.body = body
    }
    
    public func encode(_ urlRequest: URLRequestConvertible, with parameters: Parameters?) throws -> URLRequest {
        var request = try urlRequest.asURLRequest()
        request.httpBody = body.data(using: .utf8, allowLossyConversion: false)
        return request
    }
}

extension Encodable {
    
    public func toJSONString(prettyPrint: Bool) throws -> String? {
        let encoder = JSONEncoder()
        if prettyPrint { encoder.outputFormatting = .prettyPrinted }
        let data = try! encoder.encode(self)
        let jsonString = String(data: data, encoding: .utf8)
        return jsonString
    }
    
}

class RestApi {
    static func getObject<T: Decodable>(url: String, keyPath: String? = nil) -> Observable<T> {
        RxCodableAlamofire.object(.get, url, keyPath: keyPath)
    }
    static func getArray<T: Decodable>(url: String, keyPath: String? = nil) -> Observable<[T]> {
        RxCodableAlamofire.object(.get, url, keyPath: keyPath)
    }
    static func update(url: String, body: String) -> Observable<String> {
        RxAlamofire.string(.put, url, encoding: StringEncoding(body: body))
    }
    static func create(url: String, body: String) -> Observable<String> {
        RxAlamofire.string(.post, url, encoding: StringEncoding(body: body))
    }
    static func delete(url: String) -> Observable<String> {
        RxAlamofire.string(.delete, url)
    }
    static func getString(url: String) -> Observable<String> {
        RxAlamofire.string(.get, url)
    }
}

