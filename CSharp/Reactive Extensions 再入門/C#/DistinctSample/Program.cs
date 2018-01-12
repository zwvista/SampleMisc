namespace FilterSample
{
    using System;
    using System.Collections.Generic;
    using System.Reactive.Linq;
    using System.Reactive.Subjects;

    class Program
    {
        static void Main(string[] args)
        {
            DistinctSample();
            DistinctOverloadSample();
            DistinctUntilChangedSample();
        }

        static void DistinctSample()
        {
            Console.WriteLine("#DistinctSample");
            var s = new Subject<int>();
            // Distinctで重複を排除して購読
            s.Distinct()
                .Subscribe(
                    // 値を出力
                    i => Console.WriteLine("OnNext({0})", i),
                    // OnCompletedしたことを出力
                    () => Console.WriteLine("OnCompleted()"));

            // 1～3の値を発行
            Console.WriteLine("OnNext 1～3");
            s.OnNext(1);
            s.OnNext(2);
            s.OnNext(3);

            // 繰り返し1～3の値を発行
            Console.WriteLine("OnNext 1～3");
            s.OnNext(1);
            s.OnNext(2);
            s.OnNext(3);

            // 2～4の値を発行
            Console.WriteLine("OnNext 2～4");
            s.OnNext(2);
            s.OnNext(3);
            s.OnNext(4);

            Console.WriteLine("OnCompleted call.");
            s.OnCompleted();

            Console.WriteLine("-------------------------------");
        }

        /// <summary>
        /// 人
        /// </summary>
        class Person
        {
            public int Age { get; set; }
            public string Name { get; set; }
            public override string ToString()
            {
                return string.Format("{0}: {1}歳", this.Name, this.Age);
            }
        }

        /// <summary>
        /// 1の位を省いた状態で比較を行う。
        /// </summary>
        public class GenerationEqualityComparer : IEqualityComparer<int>
        {
            /// <summary>
            /// 1の位を除いた数が等しければtrueを返す
            /// </summary>
            public bool Equals(int x, int y)
            {
                return (x / 10) == (y / 10);
            }

            public int GetHashCode(int obj)
            {
                return (obj / 10).GetHashCode();
            }
        }

        static void DistinctOverloadSample()
        {
            Console.WriteLine("#DistinctOverloadSample");
            var s = new Subject<Person>();
            s.Distinct(
                // PersonクラスのAgeプロパティの値で比較する
                p => p.Age,
                // 比較方法はGenerationEqualityComparerを使用する
                new GenerationEqualityComparer())
                // 購読
                .Subscribe(
                    // 値を出力
                    p => Console.WriteLine(p),
                    // OnCompletedしたことを出力
                    () => Console.WriteLine("OnCompleted()"));

            // 10代, 20代, 30代の人を発行する
            Console.WriteLine("OnNext 10代～30代");
            s.OnNext(new Person { Name = "田中　一郎", Age = 15 });
            s.OnNext(new Person { Name = "田中　二郎", Age = 22 });
            s.OnNext(new Person { Name = "田中　三郎", Age = 38 });

            // 別の名前, 年齢の10代, 20代, 30代の人を発行する
            Console.WriteLine("OnNext 10代～30代");
            s.OnNext(new Person { Name = "木村　一郎", Age = 12 });
            s.OnNext(new Person { Name = "木村　二郎", Age = 28 });
            s.OnNext(new Person { Name = "木村　三郎", Age = 31 });

            // 40代の人を発行する
            Console.WriteLine("OnNext 40代");
            s.OnNext(new Person { Name = "井上　エリザベス", Age = 49 });

            Console.WriteLine("OnCompleted call.");
            s.OnCompleted();

            Console.WriteLine("-------------------------------");
        }

        static void DistinctUntilChangedSample()
        {
            Console.WriteLine("#DistinctUntilChangedSample");
            var s = new Subject<int>();
            // Distinctで重複を排除して購読
            s.DistinctUntilChanged()
                .Subscribe(
                    // 値を出力
                    i => Console.WriteLine("OnNext({0})", i),
                    // OnCompletedしたことを出力
                    () => Console.WriteLine("OnCompleted()"));

            // 1～3の値を2回ずつ発行
            Console.WriteLine("OnNext 1 -> 1 -> 2 -> 2 -> 3 -> 3");
            s.OnNext(1);
            s.OnNext(1);
            s.OnNext(2);
            s.OnNext(2);
            s.OnNext(3);
            s.OnNext(3);

            // 1～3の値を発行
            Console.WriteLine("OnNext 1～3");
            s.OnNext(1);
            s.OnNext(2);
            s.OnNext(3);

            Console.WriteLine("OnCompleted call.");
            s.OnCompleted();

            Console.WriteLine("-------------------------------");
        }

    }
}
