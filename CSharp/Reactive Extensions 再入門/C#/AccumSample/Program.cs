using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Subjects;
using System.Reactive.Linq;

namespace AccumSample
{
    class Program
    {
        static void Main(string[] args)
        {
            ToArraySample();
            ToDictionarySample();
            ToListSample();
            ToLookupSample();
            MaxAndMinAndAvgSample();
            MaxAndMinComparerSample();
            MaxByAndMinBySample();
            AggregateMaxSample();
            AggregateInitialValueSample();
            AggregateNoInitialValueSample();
            AggregateAccumSample();
            ScanInitialValueSample();
            ScanNoInitialValueSample();
            CountSample();
            SumSample();
            AnySample();
            AllSample();
        }

        static void ToArraySample()
        {
            Console.WriteLine("#ToArraySample");
            var s = new Subject<int>();
            // IO<T> -> IO<T[]>へ変換して購読
            s.ToArray().Subscribe(array =>
            {
                // 内容表示
                Console.WriteLine("start array dump");
                foreach (var i in array)
                {
                    Console.WriteLine("  array value : {0}", i);
                }
            });

            // 値の発行からCompleted
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("OnNext(2)");
            s.OnNext(2);
            Console.WriteLine("OnNext(3)");
            s.OnNext(3);
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        static void ToDictionarySample()
        {
            Console.WriteLine("#ToDictionarySample");
            var s = new Subject<Tuple<string, int>>();
            // IO<T> -> IO<IDictionary<TKey, T>>へ変換して購読
            // Keyを選択するラムダ式を渡す。
            s.ToDictionary(t => t.Item1).Subscribe(dict =>
            {
                Console.WriteLine("one : {0}", dict["one"]);
                Console.WriteLine("two : {0}", dict["two"]);
                Console.WriteLine("three : {0}", dict["three"]);
            });

            // 値の発行からCompleted
            Console.WriteLine("OnNext(one)");
            s.OnNext(Tuple.Create("one", 1));
            Console.WriteLine("OnNext(two)");
            s.OnNext(Tuple.Create("two", 2));
            Console.WriteLine("OnNext(three)");
            s.OnNext(Tuple.Create("three", 3));
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        static void ToListSample()
        {
            Console.WriteLine("#ToListSample");
            var s = new Subject<int>();
            // IO<T> -> IO<IList<T>>へ変換
            s.ToList().Subscribe(list =>
            {
                // 値を表示
                foreach (var i in list)
                {
                    Console.WriteLine("value : {0}", i);
                }
            });

            // 値の発行からCompleted
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("OnNext(2)");
            s.OnNext(2);
            Console.WriteLine("OnNext(3)");
            s.OnNext(3);
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        static void ToLookupSample()
        {
            Console.WriteLine("#ToLookupSample");
            var s = new Subject<Tuple<string, string>>();
            // IO<T>からIO<ILookup<TKey, T>>へ変換
            // Keyを選択するラムダ式を渡す
            s.ToLookup(t => t.Item1).Subscribe(l =>
            {
                // グループ単位に表示
                foreach (var g in l)
                {
                    Console.WriteLine("Key : {0}", g.Key);
                    foreach (var i in g)
                    {
                        Console.WriteLine("  item : {0}", i);
                    }
                }
            });

            Console.WriteLine("OnNext(group A)");
            s.OnNext(Tuple.Create("group A", "taro"));
            s.OnNext(Tuple.Create("group A", "jiro"));

            Console.WriteLine("OnNext(group B)");
            s.OnNext(Tuple.Create("group B", "foo"));
            s.OnNext(Tuple.Create("group B", "hoge"));
            s.OnNext(Tuple.Create("group B", "bar"));

            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        static void MaxAndMinAndAvgSample()
        {
            Console.WriteLine("#MaxAndMinAndAvgSample");
            var s = new Subject<int>();

            // 最大値を求めて表示
            s.Max().Subscribe(max =>
            {
                Console.WriteLine("Max {0}", max);
            },
            () => Console.WriteLine("Max Completed"));

            // 最小値を求めて表示
            s.Min().Subscribe(min =>
            {
                Console.WriteLine("Min {0}", min);
            },
            () => Console.WriteLine("Min Completed"));

            // 平均を求めて表示
            s.Average().Subscribe(avg =>
            {
                Console.WriteLine("Average {0}", avg);
            },
            () => Console.WriteLine("Average Completed"));

            // 値の発行～完了通知
            Console.WriteLine("OnNext(1-3)");
            s.OnNext(1);
            s.OnNext(2);
            s.OnNext(3);
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        static void MaxAndMinComparerSample()
        {
            Console.WriteLine("#MaxAndMinComparerSample");
            var s = new Subject<Tuple<int, int>>();
            // 最大値を求めて表示
            s.Max(new TupleIntIntComparer()).Subscribe(max =>
            {
                Console.WriteLine("Max {0}", max);
            },
            () => Console.WriteLine("Max Completed"));

            // 最小値を求めて表示
            s.Min(new TupleIntIntComparer()).Subscribe(min =>
            {
                Console.WriteLine("Min {0}", min);
            },
            () => Console.WriteLine("Min Completed"));


            // 値の発行～完了通知
            Console.WriteLine("OnNext");
            s.OnNext(Tuple.Create(1, 1));
            s.OnNext(Tuple.Create(1, 2));
            s.OnNext(Tuple.Create(3, 1));
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        // Tuple<int, int>の比較を行うクラス
        class TupleIntIntComparer : IComparer<Tuple<int, int>>
        {
            // Item1 + Item2の結果で比較を行う
            public int Compare(Tuple<int, int> x, Tuple<int, int> y)
            {
                if (x == y)
                {
                    return 0;
                }

                if (x == null)
                {
                    return -1;
                }

                if (y == null)
                {
                    return 1;
                }

                var xValue = x.Item1 + x.Item2;
                var yValue = y.Item1 + y.Item2;
                return Comparer<int>.Default.Compare(xValue, yValue);
            }
        }

        static void MaxByAndMinBySample()
        {
            Console.WriteLine("#MaxByAndMinBySample");
            var s = new Subject<Tuple<int, int>>();
            // 最大値を求めて表示, 比較はタプルのItem1を使用する
            s.MaxBy(t => t.Item1).Subscribe(max =>
            {
                foreach (var i in max)
                {
                    Console.WriteLine("MaxBy {0}", i);
                }
            },
            () => Console.WriteLine("MaxBy Completed"));

            // 最小値を求めて表示, 比較はタプルのItem1を使用する
            s.MinBy(t => t.Item1).Subscribe(min =>
            {
                foreach (var i in min)
                {
                    Console.WriteLine("MinBy {0}", i);
                }
            },
            () => Console.WriteLine("MinBy Completed"));

            // 値の発行～完了通知
            Console.WriteLine("OnNext");
            s.OnNext(Tuple.Create(1, 1));
            s.OnNext(Tuple.Create(1, 2));
            s.OnNext(Tuple.Create(3, 1));
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        static void AggregateMaxSample()
        {
            Console.WriteLine("#AggregateMaxSample");
            var s = new Subject<int>();

            // x, yから値の大きい方を返す
            s.Aggregate((x, y) => x > y ? x : y).Subscribe(
                i => Console.WriteLine("Aggregate OnNext({0})", i),
                () => Console.WriteLine("Aggregate OnCompleted()"));

            // 値の発行～完了通知
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("OnNext(10)");
            s.OnNext(10);
            Console.WriteLine("OnNext(100)");
            s.OnNext(100);
            Console.WriteLine("OnNext(50)");
            s.OnNext(50);
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        static void AggregateNoInitialValueSample()
        {
            Console.WriteLine("#AggregateNoInitialValueSample");
            var s = new Subject<int>();

            // IObservable<T>のシーケンスから渡される値の合計を求める
            s.Aggregate((x, y) =>
            {
                // Aggregateの引数に渡されるデリゲートがどのように動くのか確認するためのログ
                Console.WriteLine("log({0}, {1})", x, y);
                // 今までの合計(x)と、新たな値(y)を足して新たな合計値として返す。
                return x + y;
            })
            // 結果を購読
            .Subscribe(
                i => Console.WriteLine("Aggregate OnNext({0})", i),
                () => Console.WriteLine("Aggregate OnCompleted()"));

            // 値の発行～完了通知
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("OnNext(10)");
            s.OnNext(10);
            Console.WriteLine("OnNext(100)");
            s.OnNext(100);
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        static void AggregateInitialValueSample()
        {
            Console.WriteLine("#AggregateInitialValueSample");
            var s = new Subject<int>();
            // IObservable<T>のシーケンスから渡される値の合計を求める
            // ただし、初期値として5を使用する
            s.Aggregate(5, (x, y) =>
            {
                // Aggregateの引数に渡されるデリゲートがどのように動くのか確認するためのログ
                Console.WriteLine("log({0}, {1})", x, y);
                // 今までの合計(x)と、新たな値(y)を足して新たな合計値として返す。
                return x + y;
            })
                // 結果を購読
            .Subscribe(
                i => Console.WriteLine("Aggregate OnNext({0})", i),
                () => Console.WriteLine("Aggregate OnCompleted()"));

            // 値の発行～完了通知
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("OnNext(10)");
            s.OnNext(10);
            Console.WriteLine("OnNext(100)");
            s.OnNext(100);
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        static void AggregateAccumSample()
        {
            Console.WriteLine("#AggregateAccumSample");
            var s = new Subject<int>();
            // IObservable<T>のシーケンスから渡される値を集める
            // 初期値が空のリスト
            s.Aggregate(new List<int>(), (list, value) =>
            {
                // 通知された値をリストの保持しておく
                list.Add(value);
                return list;
            })
            // 購読。リストの内容を表示
            .Subscribe(list =>
            {
                foreach (var i in list)
                {
                    Console.WriteLine("value : {0}", i);
                }
            },
            () => Console.WriteLine("Aggregate OnCompleted()"));

            // 値の発行～完了通知
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("OnNext(10)");
            s.OnNext(10);
            Console.WriteLine("OnNext(100)");
            s.OnNext(100);
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        static void ScanNoInitialValueSample()
        {
            Console.WriteLine("#ScanNoInitialValueSample");
            var s = new Subject<int>();
            // Aggregateで合計値を求めるのと同じ処理をScanメソッドで行う。
            s.Scan((x, y) =>
            {
                Console.WriteLine("log({0}, {1})", x, y);
                return x + y;
            })
            // 購読
            .Subscribe(
                i => Console.WriteLine("Scan OnNext({0})", i),
                () => Console.WriteLine("Scan OnCompleted()"));

            // 値の発行～完了通知
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("OnNext(10)");
            s.OnNext(10);
            Console.WriteLine("OnNext(100)");
            s.OnNext(100);
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        static void ScanInitialValueSample()
        {
            Console.WriteLine("#ScanInitialValueSample");
            var s = new Subject<int>();
            // 初期値5でScanを使い合計を求める
            s.Scan(5, (x, y) =>
            {
                Console.WriteLine("log({0}, {1})", x, y);
                return x + y;
            })
            // 購読
            .Subscribe(
                i => Console.WriteLine("Scan OnNext({0})", i),
                () => Console.WriteLine("Scan OnCompleted()"));

            // 値の発行～完了通知
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("OnNext(10)");
            s.OnNext(10);
            Console.WriteLine("OnNext(100)");
            s.OnNext(100);
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        static void CountSample()
        {
            Console.WriteLine("#CountSample");
            var s = new Subject<int>();
            // 数を数える
            s.Count()
                // 購読
                .Subscribe(
                    i => Console.WriteLine("Count OnNext({0})", i),
                    () => Console.WriteLine("Count OnCompleted()"));

            // 値の発行～完了通知
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("OnNext(10)");
            s.OnNext(10);
            Console.WriteLine("OnNext(100)");
            s.OnNext(100);
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        static void AnySample()
        {
            Console.WriteLine("#AnySample");
            var s = new Subject<int>();
            // どれかが0以下かチェック
            s.Any(i => i <= 0)
                // 購読
                .Subscribe(
                    i => Console.WriteLine("Any(i => i <= 0) OnNext({0})", i),
                    () => Console.WriteLine("Any(i => i <= 0) OnCompleted()"));

            // どれかが偶数かチェック
            s.Any(i => i % 2 == 0)
                // 購読
                .Subscribe(
                    i => Console.WriteLine("Any(i => i % 2 == 0) OnNext({0})", i),
                    () => Console.WriteLine("Any(i => i % 2 == 0) OnCompleted()"));

            // 値の発行～完了通知
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("OnNext(10)");
            s.OnNext(10);
            Console.WriteLine("OnNext(100)");
            s.OnNext(100);
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        static void AllSample()
        {
            Console.WriteLine("#AllSample");
            var s = new Subject<int>();
            // 全てが偶数かどうかをチェック
            s.All(i => i % 2 == 0)
                // 購読
                .Subscribe(
                    i => Console.WriteLine("All(i => i % 2 == 0) OnNext({0})", i),
                    () => Console.WriteLine("All(i => i % 2 == 0) OnCompleted()"));

            // 全てが1000以下かどうかをチェック
            s.All(i => i <= 1000)
                // 購読
                .Subscribe(
                    i => Console.WriteLine("All(i => i <= 1000) OnNext({0})", i),
                    () => Console.WriteLine("All(i => i <= 1000) OnCompleted()"));

            // 値の発行～完了通知
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("OnNext(10)");
            s.OnNext(10);
            Console.WriteLine("OnNext(100)");
            s.OnNext(100);
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        static void SumSample()
        {
            Console.WriteLine("#SumSample");
            var s = new Subject<int>();
            // 数を合計する
            s.Sum()
                // 購読
                .Subscribe(
                    i => Console.WriteLine("Sum OnNext({0})", i),
                    () => Console.WriteLine("Sum OnCompleted()"));

            // 値の発行～完了通知
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("OnNext(10)");
            s.OnNext(10);
            Console.WriteLine("OnNext(100)");
            s.OnNext(100);
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }
    }
}
