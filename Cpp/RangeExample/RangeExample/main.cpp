//
//  main.cpp
//  RangeExample
//
//  Created by 趙偉 on 2019/06/12.
//  Copyright © 2019 趙偉. All rights reserved.
//

#include <iostream>
#include <locale>
#include <locale.h>
#include "jys.hpp"
using namespace std;

int main(int argc, const char * argv[]) {
    constexpr char locale_name[] = "en_US.UTF-8";
    setlocale( LC_ALL, locale_name );
    locale::global(locale(locale_name));
    wcout.imbue(locale());
    wstring text = L"床前明月光疑是地上霜举头望明月低头思故乡";
    int offset = 5;
    jys1(text, offset);
    jys2(text, offset);
    return 0;
}
