package utils;


import org.apache.commons.lang3.StringUtils;

import static org.apache.commons.lang3.StringUtils.*;

public class ACLStringUtils {
    static void f(Object o) { System.out.println(o); }
    public static void main(String[] args) {
        f(isEmpty(null) && isEmpty(""));
        f(isAnyEmpty("", "a"));
        f(isAllBlank(null, "  "));
        f(trim(" a "));
        f(trimToNull(" "));
        f(truncate("abcdefg", 4));
        f(strip(" axy", "xy"));
        f(stripStart("aabb ", "a"));
        f(stripAll(" a ", " b "));
        f(StringUtils.equals(null, null));
        f(compare(null, ""));
        f(equalsAny("a", "a", "b"));
        f(indexOf("aabaabaa", "a"));
        f(lastIndexOf("aabaabaa", 'a'));
        f(contains("abc", 'a'));
    }
}
