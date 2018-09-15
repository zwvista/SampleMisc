package fp;

import fj.P;
import fj.data.HashMap;
import fj.data.List;
import fj.data.TreeMap;

import static fj.Ord.charOrd;
import static fj.Ord.intOrd;
import static fj.data.HashMap.arrayHashMap;
import static fj.data.TreeMap.treeMap;

public class FJMap {

    static void f(Object o) { System.out.println(o); }
	static void f2(Object o) { System.out.print(o); }
	public static void main(String[] args) {
		{
			HashMap<Integer, Character> m = arrayHashMap(P.p(1, 'a'), P.p(2, 'b')); f(m.toMap()); // {1=a, 2=b}
			m.delete(1); f(m.toMap()); // {2=b}
			m.clear(); f(m.toMap()); // {}
			f(arrayHashMap(P.p(1, 'a'), P.p(2, 'b')).contains(1)); // true
			arrayHashMap(P.p(1, 'a'), P.p(2, 'b')).foreachDoEffect(FJMap::f2); f(""); // (1,a)(2,b)
			f(arrayHashMap(P.p(1, 'a'), P.p(2, 'b')).get(1)); // Some(a)
			m = arrayHashMap(P.p(1, 'a'), P.p(2, 'b')); f2(m.getDelete(1)); f(m.toMap()); // Some(a){2=b}
			f(arrayHashMap(P.p(1, 'a'), P.p(2, 'b')).isEmpty()); // false
			f(arrayHashMap().isEmpty()); // true
			f(arrayHashMap(P.p(1, 'a'), P.p(2, 'b')).keys()); // List(1,2)
			f(arrayHashMap(P.p(1, 'a'), P.p(2, 'b')).map(i -> i + 1, c -> (char)(c + 1)).toMap()); // {2=b, 3=c}
			f(arrayHashMap(P.p(1, 'a'), P.p(2, 'b')).map(kv -> P.p(kv._1() + 1, (char)(kv._2() + 1))).toMap()); // {2=b, 3=c}
			f(arrayHashMap(P.p(1, 'a'), P.p(2, 'b')).mapKeys(i -> i + 1).toMap()); // {2=a, 3=b}
			f(arrayHashMap(P.p(1, 'a'), P.p(2, 'b')).mapValues(c -> (char)(c + 1)).toMap()); // {1=b, 2=c}
			m = arrayHashMap(P.p(1, 'a'), P.p(2, 'b')); m.set(3, 'c'); f(m.toMap()); // {1=a, 2=b, 3=c}
			f(arrayHashMap(P.p(1, 'a'), P.p(2, 'b')).size()); // 2
			f(arrayHashMap().size()); // 0
			f(arrayHashMap(P.p(1, 'a'), P.p(2, 'b')).toArray()); // Array((1,a),(2,b))
			f(arrayHashMap(P.p(1, 'a'), P.p(2, 'b')).toList()); // List((1,a),(2,b))
			f(arrayHashMap(P.p(1, 'a'), P.p(2, 'b')).values()); // List(a,b)
		}
		{
			TreeMap<Integer, Character> m = treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')); f(m.toMutableMap()); // {1=a, 2=b}
			m.delete(1); f(m.toMutableMap()); // {1=a, 2=b}
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')).contains(1)); // true
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')).get(1)); // Some(a)
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')).isEmpty()); // false
			f(treeMap(intOrd).isEmpty()); // true
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')).keys()); // List(1,2)
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')).map(c -> (char)(c + 1))); // TreeMap((1: b),(2: c))
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')).max()); // Some((2,b))
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')).minKey()); // Some(1)
			m = treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')); m.set(3, 'c'); f(m.toMutableMap()); // {1=a, 2=b}
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b'), P.p(3, 'c')).split(charOrd, 2)); // (Set(a),Some(b),Set(c))
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b'), P.p(3, 'c')).splitLookup(2)); // (TreeMap((1: a)),Some(b),TreeMap((3: c)))
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')).size()); // 2
			f(treeMap(intOrd).size()); // 0
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')).toList()); // List((1,a),(2,b))
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')).toListReverse()); // List((2,b),(1,a))
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')).union(List.list(P.p(3, 'c')))); // TreeMap((1: a),(2: b),(3: c))
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')).union(treeMap(intOrd, P.p(3, 'c')))); // TreeMap((1: a),(2: b),(3: c))
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')).update(2, c -> (char)(c + 1))); // (true,TreeMap((1: a),(2: c)))
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')).update(3, c -> (char)(c + 1), 'z')); // TreeMap((1: a),(2: b),(3: z))
			f(treeMap(intOrd, P.p(1, 'a'), P.p(2, 'b')).values()); // List(a,b)
		}
	}

}
