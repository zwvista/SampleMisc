//
//  main.cpp
//  RangeV3Example
//
//  Created by 趙偉 on 2019/06/05.
//  Copyright © 2019 趙偉. All rights reserved.
//

#include <iostream>
#include <range/v3/all.hpp>
#include <vector>
#include <locale>
#include <locale.h>
using namespace std;
using namespace ranges;

void f1(wstring_view text, int offset) {
    auto v = text | view::enumerate | to_vector;
    auto v2 = action::sort(v, [&](auto& a, auto& b){return pair{a.first % offset, -a.first} < pair{b.first % offset, -b.first};}) |
        view::group_by([&](auto& a, auto& b){return a.first % offset == b.first % offset;}) | to_vector;
    auto rng = v2 | view::transform([](auto& o){
        return o | view::transform([](auto& o2){return o2.second;}) | view::intersperse(L'|');
    });
    for (wstring o : rng)
        wcout << o << endl;
}

void f2(wstring_view text, int offset) {
    auto v = view::iota(0, text.size()) | to_vector;
    auto v2 = action::sort(v, [&](int a, int b){return pair{a % offset, -a} < pair{b % offset, -b};}) |
        view::group_by([&](int a, int b){return a % offset == b % offset;}) | to_vector;
    auto rng = v2 | view::transform([&](auto& o){
        return o | view::transform([&](int i){return text[i];}) | view::intersperse(L'|');
    });
    for (wstring o : rng)
        wcout << o << endl;
}

int main(int argc, const char * argv[]) {
    constexpr char locale_name[] = "en_US.UTF-8";
    setlocale( LC_ALL, locale_name );
    locale::global(locale(locale_name));
    wcout.imbue(locale());
    wstring text = L"床前明月光疑是地上霜举头望明月低头思故乡";
    int offset = 5;
    f1(text, offset);
    f2(text, offset);
    return 0;
}
