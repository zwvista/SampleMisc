//
//  RxCodableAlamofire.swift
//  LollySwiftiOS
//
//  Created by 趙偉 on 2018/05/06.
//  Copyright © 2018年 趙 偉. All rights reserved.
//

import Foundation
import RxSwift
import Alamofire
import RxAlamofire
import CodableAlamofire

class RxCodableAlamofire {
    public static func object<T: Decodable>(_ method: Alamofire.HTTPMethod, _ url: URLConvertible, parameters: Parameters? = nil, encoding: ParameterEncoding = URLEncoding.default, headers: HTTPHeaders? = nil, queue: DispatchQueue = .main, keyPath: String? = nil, mapToObject object: T? = nil, decoder: JSONDecoder = JSONDecoder()) -> Observable<T> {
        return Alamofire.Session.default.rx.object(method, url, parameters: parameters, encoding: encoding, headers: headers, queue: queue, keyPath: keyPath, mapToObject: object, decoder: decoder)
    }
}

extension Reactive where Base: Alamofire.Session {
    public func object<T: Decodable>(_ method: Alamofire.HTTPMethod, _ url: URLConvertible, parameters: Parameters? = nil, encoding: ParameterEncoding = URLEncoding.default, headers: HTTPHeaders? = nil, queue: DispatchQueue = .main, keyPath: String? = nil, mapToObject object: T? = nil, decoder: JSONDecoder = JSONDecoder()) -> Observable<T> {
        return RxAlamofire.request(method, url, parameters: parameters, encoding: encoding, headers: headers).flatMap { $0.rx.object(queue: queue, keyPath: keyPath, mapToObject: object, decoder: decoder) }
    }
}

extension Reactive where Base: DataRequest {
    public func object<T: Decodable>(queue: DispatchQueue = .main, keyPath: String? = nil, mapToObject object: T? = nil, decoder: JSONDecoder = JSONDecoder()) -> Observable<T> {
        return result(queue: queue, responseSerializer: DataKeyPathSerializer(keyPath: keyPath, decoder: decoder))
    }
}
