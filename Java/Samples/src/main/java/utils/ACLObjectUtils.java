package utils;


import static org.apache.commons.lang3.ObjectUtils.*;

public class ACLObjectUtils {
    static void f(Object o) { System.out.println(o); }
    public static void main(String[] args) {
        Integer a = null, b = 3, c = null;
        // a != null && b != null && c != null
        f(allNotNull(a, b, c)); // false
        // a != null || b != null || c != null
        f(anyNotNull(a, b, c)); // true
        f(CONST(3)); // 3
        // a != null ? a : 3
        f(defaultIfNull(a, 3)); // 3
        // a != null ? a : b ! = null ? b : c
        // a ?? b ?? c (in C#)
        f(firstNonNull(a, b, c)); // 3
        f(max(1, 3, 2)); // 3
        f(min(1, 3, 2)); // 1
    }
}
