package fp;

import fj.Monoid;
import fj.Ord;
import fj.Ordering;
import fj.data.HashSet;
import fj.data.Set;

import static fj.Ord.intOrd;
import static fj.data.HashSet.hashSet;
import static fj.data.HashSet.iterableHashSet;
import static fj.data.Set.iterableSet;
import static fj.data.Set.set;

public class FJSet {

	static void f(Object o) { System.out.println(o); }
	static void f2(Object o) { System.out.println(o); }
	public static void main(String[] args) {
		{
			Set<Integer> s = set(intOrd, 1, 2); f(s.toJavaSet()); // [1, 2]
			f(s.bind(intOrd, a -> set(intOrd, a, a * 2))); // Set(1,2,4)
			f(s.delete(1)); // true
			f(s.size()); // 1
			f(Set.empty(intOrd).toJavaSet()); // []
            f(set(intOrd, 1, 2, 3, 4).filter(a -> a % 2 == 0)); // Set(2,4)
            f(set(intOrd, 1, 2, 3, 4).foldMap(a -> a, Monoid.intAdditionMonoid)); // 10
            f(set(intOrd, 1, 2, 3, 4).foldMapRight(a -> a, Monoid.intAdditionMonoid)); // 10
            f(set(intOrd, 1, 2).insert(3).toJavaHashSet()); // [1, 2, 3]
            f(set(intOrd, 1, 2, 3).intersect(set(intOrd, 3, 4, 5))); // Set(3)
            f(set(intOrd).isEmpty()); // true
			f(iterableSet(intOrd, java.util.Arrays.asList(1, 2, 3))); // Set(1,2,3)
            f(Set.join(intOrd, set(Ord.ord(a -> b -> Ordering.fromInt(a.size() - b.size())), set(intOrd, 1, 2, 3), set(intOrd, 3, 4, 5)))); // Set(3,4,5)
            f(set(intOrd, 1, 2, 3, 4).lookup(3)); // Some(3)
            f(set(intOrd, 1, 2, 3, 4).lookupGE(3)); // Some(3)
            f(set(intOrd, 1, 2, 3, 4).lookupGT(3)); // Some(4)
            f(set(intOrd, 1, 2, 3, 4).lookupLE(3)); // Some(3)
            f(set(intOrd, 1, 2, 3, 4).lookupLT(3)); // Some(2)
            f(set(intOrd, 1, 2, 3).map(intOrd, a -> a + 1)); // Set(2,3,4)
            f(set(intOrd, 1, 2, 3).max());
            f(set(intOrd, 1, 2, 3).member(3));
            f(set(intOrd, 1, 2, 3).minus(set(intOrd, 3, 4, 5)));
			f(set(intOrd, 1, 2).isEmpty()); // false
            f(Set.single(intOrd, 3)); // Set(3)
            f(set(intOrd, 1, 2, 3).size()); // 3
            f(set(intOrd, 1, 2, 3).split(2)); // (Set(1),Some(2),Set(3))
            f(set(intOrd, 1, 2).subsetOf(set(intOrd, 1, 2, 3))); // true
            f(set(intOrd, 1, 2).toList()); // List(1,2)
            f(set(intOrd, 1, 2).toListReverse()); // List(2,1)
            f(set(intOrd, 1, 2).toStreamReverse().toList()); // List(2,1)
            f(set(intOrd, 1, 2, 3).union(set(intOrd, 3, 4, 5))); // Set(1,2,3,4,5)
            f(set(intOrd, 1, 2, 3).update(2, a -> a + 1)); // (true,Set(1,3))
		}
		{
			HashSet<Integer> s = hashSet(1, 2); f(s.toJavaSet()); // [1, 2]
			f(s.contains(1)); // true
			f(s.delete(1)); // true
			s.clear(); f(s.toJavaSet()); // []
			f(iterableHashSet(java.util.Arrays.asList(1, 2, 3)).toJavaSet()); // [1, 2, 3]
			s.set(3); f(s.toJavaSet()); // [3]
			f(s.size()); // 1
			f(HashSet.empty().toJavaSet()); // []
			f(hashSet(1, 2).isEmpty()); // false
			f(hashSet().isEmpty()); // true
			f(hashSet(1, 2).toList()); // List(1,2)
		}
	}

}
