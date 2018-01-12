namespace GroupSample
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Reactive;
    using System.Reactive.Linq;
    using System.Reactive.Subjects;

    class Program
    {
        static void Main(string[] args)
        {
            //GroupBySample();
            GroupByUntilSample();
        }

        static void GroupBySample()
        {
            Console.WriteLine("#GroupBySample");
            var s = new Subject<int>();
            // 10で割った余りでグルーピングする。（つまり１の位の数字でグルーピング）
            s.GroupBy(i => i % 10)
                // IGroupedObservable<int, int>がSubscribeのOnNextに渡ってくる
                .Subscribe(
                    g =>
                    {
                        // GroupByのOnNextの処理
                        Console.WriteLine("集計開始 {0}", g.Key);
                        // 値を集計 配列にまとめて
                        g.ToArray()
                            // ,区切りの文字列にして
                            .Select(array => string.Join(", ", array))
                            // 結果を表示
                            .Subscribe(
                                values => Console.WriteLine("集計結果 Key: {0}, Values: {{{1}}}", g.Key, values),
                                () => Console.WriteLine("集計終了 Key: {0}", g.Key));
                    },
                    ex =>
                    {
                        // エラー処理（今回の例ではエラーは起きないけど・・・
                        Console.WriteLine("GroupBy OnError {0}", ex.Message);
                    },
                    () =>
                    {
                        // GroupByの結果がOnCompletedになった時の処理
                        Console.WriteLine("GroupBy OnCompleted");
                    });

            // 値をいくつか発行して終了
            Console.WriteLine("OnNext(13)");
            s.OnNext(13);
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("OnNext(11)");
            s.OnNext(11);
            Console.WriteLine("OnNext(42)");
            s.OnNext(42);
            Console.WriteLine("OnNext(21)");
            s.OnNext(21);
            Console.WriteLine("OnNext(12)");
            s.OnNext(12);
            Console.WriteLine("OnNext(23)");
            s.OnNext(23);
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("------------------------");
        }

        class Person
        {
            public Person(string name)
            {
                this.Name = name;
            }

            public string Name { get; private set; }
            public IList<string> History { get; private set; }
        }

        class Event
        {
            public string Name { get; set; }
            public int Point { get; set; }
        }

        class World
        {
            private Person[] people = new[]
            {
                new Person("田中　太郎"),
                new Person("木村　二郎"),
                new Person("砂糖　亜真央"),
                new Person("伊藤　ミント"),
                new Person("小麦色のオタク")
            };

            public Event[] lifeEvents = new[]
             {
                new Event { Name = "結婚", Point = 10 },
                new Event { Name = "離婚", Point = -10 },
                new Event { Name = "出産", Point = 15 },
                new Event { Name = "入学", Point = 5 },
                new Event { Name = "卒業", Point = 5 },
                new Event { Name = "留年", Point = -5 },
                new Event { Name = "ニート", Point = -15 },
                new Event { Name = "デスマーチ", Point = -100 },
            };

            public event Action<Tuple<Person, Event>> LifeEvent = pair => {};

            public event Action GameOver = () => { };

            public void OneYear()
            {
                var target = GetRandom(people);
                var e = GetRandom(lifeEvents);

                this.LifeEvent(Tuple.Create(target, e));
            }

            public void Meteo()
            {
                Console.WriteLine("## 世界はリセットされました");
                this.GameOver();
            }

            private static readonly Random Random = new Random();
            private static T GetRandom<T>(T[] items)
            {
                return items[Random.Next(items.Length)];
            }
       }

        static void GroupByUntilSample2()
        {
            var world = new World();
            Observable.Defer(() =>
                Observable.FromEvent<Tuple<Person, Event>>(
                    h => world.LifeEvent += h,
                    h => world.LifeEvent -= h)
                .GroupByUntil(
                    t => t.Item1, 
                    g => Observable.FromEvent(h => world.GameOver += h, h => world.GameOver -= h)))
                .Repeat()
                .Subscribe(pair =>
                {
                    pair.ToArray()
                        .Subscribe(pairs =>
                        {
                            Console.WriteLine("{0}さんの人生の記録", pair.Key.Name);
                            Console.WriteLine("  {0}", string.Join(" → ", pairs.Select(p => p.Item2.Name).ToArray()));
                            Console.WriteLine("  トータルスコア {0}", pairs.Select(p => p.Item2.Point).Sum());
                        });
                });

            foreach (var i in Enumerable.Range(1, 30))
            {
                world.OneYear();
                if (i % 10 == 0)
                {
                    world.Meteo();
                }
            }
        }

        static void GroupByUntilSample()
        {
            Console.WriteLine("#GroupByUntilSample");
            // 値をランダムで出したいので
            var r = new Random();
            // グループピングの終了を通知するためのIObservable
            var duration = new Subject<Unit>();

            // GroupByUntilの使用例
            var subscriber = Observable.Interval(TimeSpan.FromMilliseconds(500))
                // 500ミリ秒間隔で乱数発生
                .Select(_ => r.Next(1000))
                .GroupByUntil(
                    // 1の位の数でグルーピング
                    l => l % 10,
                    // durationでOnNextが発行されるまでの間グルーピングをする。
                    // 全てのグループに対して、同じIObservableを返しているので、同じタイミングで
                    // グルーピングが終わる。
                    l => duration.AsObservable())
                // 購読
                .Subscribe(
                    g =>
                    {
                        // GroupByのOnNextの処理
                        Console.WriteLine("集計開始 {0}", g.Key);
                        // 値を集計 配列にまとめて
                        g.ToArray()
                            // ,区切りの文字列にして
                            .Select(array => string.Join(", ", array))
                            // 結果を表示
                            .Subscribe(
                                values => Console.WriteLine("集計結果 Key: {0}, Values: {{{1}}}", g.Key, values),
                                () => Console.WriteLine("集計終了 Key: {0}", g.Key));
                    },
                    ex =>
                    {
                        // エラー処理（今回の例ではエラーは起きないけど・・・
                        Console.WriteLine("GroupBy OnError {0}", ex.Message);
                    },
                    () =>
                    {
                        // GroupByの結果がOnCompletedになった時の処理
                        Console.WriteLine("GroupBy OnCompleted");
                    });

            while(true)
            {
                // 待ち
                Console.WriteLine("Enterを押すまで集計します。(終了したい場合はendと入力してください)");
                if (Console.ReadLine() == "end")
                {
                    break;
                }

                // durationのOnNextでいったんグルーピングの集計が一区切り
                duration.OnNext(Unit.Default);
            }
            // 後始末
            duration.OnCompleted();
            subscriber.Dispose();
            Console.WriteLine("------------------------");
        }
    }
}
