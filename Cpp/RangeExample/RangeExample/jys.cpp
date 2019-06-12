//
//  jys.cpp
//  RangeExample
//
//  Created by 趙偉 on 2019/06/12.
//  Copyright © 2019 趙偉. All rights reserved.
//

#include "jys.hpp"

#include <range/v3/all.hpp>
#include <vector>
using namespace std;
using namespace ranges;

void jys1(wstring_view text, int offset) {
    auto v = text | view::enumerate | to_vector;
    auto v2 = action::sort(v, [&](auto& a, auto& b){return pair{a.first % offset, -a.first} < pair{b.first % offset, -b.first};}) |
    view::group_by([&](auto& a, auto& b){return a.first % offset == b.first % offset;}) | to_vector;
    auto rng = v2 | view::transform([](auto& o){
        return o | view::transform([](auto& o2){return o2.second;}) | view::intersperse(L'|');
    });
    for (wstring o : rng)
        wcout << o << endl;
}

void jys2(wstring_view text, int offset) {
    auto v = view::iota(0, text.size()) | to_vector;
    auto v2 = action::sort(v, [&](int a, int b){return pair{a % offset, -a} < pair{b % offset, -b};}) |
    view::group_by([&](int a, int b){return a % offset == b % offset;}) | to_vector;
    auto rng = v2 | view::transform([&](auto& o){
        return o | view::transform([&](int i){return text[i];}) | view::intersperse(L'|');
    });
    for (wstring o : rng)
        wcout << o << endl;
}
