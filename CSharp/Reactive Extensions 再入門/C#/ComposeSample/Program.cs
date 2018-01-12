using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Threading;
using System.Reactive;
using System.Reactive.Joins;

namespace ComposeSample
{
    class Program
    {
        static void Main(string[] args)
        {
            //MergeSample();
            //SelectManySample();
            //SwitchSample();
            //ConcatSample();
            //ZipSample();
            //AmbSample();
            //CombineLatestSample();
            //StartWithSample();
            //JoinSample();
            //GroupJoinSample();
            AndThenWhenSample();
        }

        //-Merge Method : マージ（統合）
        static void MergeSample()
        {
            Console.WriteLine("## MergeSample");
            {
                Console.WriteLine("# 2つのIObservable<T>の統合");
                Observable
                    // 0～2の値を1秒間隔で発行
                    .Interval(TimeSpan.FromSeconds(1)).Take(3)
                    // 1～3の値に変換
                    .Select(i => i + 1)
                    // マージ（統合）
                    .Merge(
                        Observable
                    // 0～2の値を1秒間隔で発行
                            .Interval(TimeSpan.FromSeconds(1)).Take(3)
                    // 10, 20, 30に変換
                            .Select(i => (i + 1) * 10))
                    // 購読
                    .Subscribe(
                        i => Console.WriteLine("OnNext: {0}", i),
                        () => Console.WriteLine("OnCompleted"));
                // Enter押すまで待機
                Console.ReadLine();
            }
            Console.WriteLine("------------------------------");
            {
                Console.WriteLine("# 複数のIObservable<T>の統合");
                // Observable.Merge<T>(params IObservable<T>[] sources)
                Observable.Merge(
                    // 3つのIObservable<T>をマージ
                    Observable.Range(0, 3),
                    Observable.Range(10, 3),
                    Observable.Range(100, 3))
                    // 購読
                    .Subscribe(
                        i => Console.WriteLine("OnNext: {0}", i),
                        () => Console.WriteLine("OnCompleted"));
                // Enter押すまで待機
                Console.ReadLine();
            }
            Console.WriteLine("------------------------------");
            {
                Console.WriteLine("# 複数のIObservable<T>の統合2");
                // IObservable<T> Merge<T>(IObservable<IObservable<T>> source)
                var source = new Subject<int>();
                source
                    // IObservable<int>からIObservable<IObservable<int>>に変換
                    .Select(i => Observable
                        // 0～2の値を1秒間隔で3つ発行
                        .Interval(TimeSpan.FromSeconds(1)).Take(3)
                        // 値を変換
                        .Select(l => (l + 1) * i))
                    // IObservable<IObservable<T>>からIObservable<T>へマージ
                    .Merge()
                    // 購読
                    .Subscribe(
                        i => Console.WriteLine("OnNext: {0}", i),
                        () => Console.WriteLine("OnCompleted"));

                // 値の発行から完了
                Console.WriteLine("# OnNext(10)");
                source.OnNext(10);
                Console.WriteLine("# OnNext(100)");
                source.OnNext(100);
                Console.WriteLine("# OnNext(1000)");
                source.OnNext(1000);
                Console.WriteLine("# OnCompleted");
                source.OnCompleted();

                // Enter押すまで待機
                Console.ReadLine();
            }
            Console.WriteLine("------------------------------");
        }

        //-SelectMany Method : IO<T> -> IO<U>
        static void SelectManySample()
        {
            Console.WriteLine("## SelectManySample");
            {
                Console.WriteLine("# 複数のIObservable<T>の統合(SelectMany)");
                // IObservable<T> Merge<T>(IObservable<IObservable<T>> source)
                var source = new Subject<int>();
                source
                    // 発行された値からIObservable<T>を作成してマージ（統合）する
                    .SelectMany(i => Observable
                        // 0～2の値を1秒間隔で3つ発行
                        .Interval(TimeSpan.FromSeconds(1)).Take(3)
                        // 値を変換
                        .Select(l => (l + 1) * i))
                    // 購読
                    .Subscribe(
                        i => Console.WriteLine("OnNext: {0}", i),
                        () => Console.WriteLine("OnCompleted"));

                // 値の発行から完了
                Console.WriteLine("# OnNext(10)");
                source.OnNext(10);
                Console.WriteLine("# OnNext(100)");
                source.OnNext(100);
                Console.WriteLine("# OnNext(1000)");
                source.OnNext(1000);
                Console.WriteLine("# OnCompleted");
                source.OnCompleted();

                // Enter押すまで待機
                Console.ReadLine();
            }
            Console.WriteLine("------------------------------");
            {
                Console.WriteLine("# 複数のIObservable<T>の統合(SelectManyのオーバーロード)");
                var source = new Subject<int>();
                source
                    .SelectMany(
                    // sourceから発行された値を元にi, i+1, i+2の値を発行するIObservable<int>を返す
                        i => Observable.Range(i, 3),
                    // sourceから発行された値とi => Observable.Range(i, 3)で生成された値を
                    // 使って最終的に発行される値を作成する。ここでは匿名型にしただけ。
                        (x, y) => new { x, y })
                    .Subscribe(Console.WriteLine);

                // 値の発行から完了
                Console.WriteLine("# OnNext(10)");
                source.OnNext(10);
                Console.WriteLine("# OnNext(100)");
                source.OnNext(100);
                Console.WriteLine("# OnNext(1000)");
                source.OnNext(1000);
                Console.WriteLine("# OnCompleted");
                source.OnCompleted();

                // Enter押すまで待機
                Console.ReadLine();
            }
            Console.WriteLine("------------------------------");
        }


        //-Switch(TSource) Method : IO<IO<T>> -> IO<T>への変換
        static void SwitchSample()
        {
            Console.WriteLine("## SwitchSample");
            {
                Console.WriteLine("# 複数のIObservable<T>の統合(Merge)");
                // IObservable<T> Merge<T>(IObservable<IObservable<T>> source)
                var source = new Subject<int>();
                source
                    .Select(i => Observable
                        // 1 * i, 2 * i, 3 * iの値を1秒間隔で発行
                        .Interval(TimeSpan.FromSeconds(1)).Take(3)
                        // 値を変換
                        .Select(l => (l + 1) * i))
                    // 統合
                    .Merge()
                    .Subscribe(
                        i => Console.WriteLine("OnNext: {0}", i),
                        () => Console.WriteLine("OnCompleted"));

                // 値の発行から完了
                Console.WriteLine("# OnNext(10)");
                source.OnNext(10);
                Console.WriteLine("Sleep 2000ms...");
                Thread.Sleep(2000);

                Console.WriteLine("# OnNext(100)");
                source.OnNext(100);
                Console.WriteLine("Sleep 2000ms...");
                Thread.Sleep(2000);

                Console.WriteLine("# OnNext(1000)");
                source.OnNext(1000);
                Console.WriteLine("Sleep 2000ms...");
                Thread.Sleep(2000);

                Console.WriteLine("# OnCompleted");
                source.OnCompleted();

                Console.ReadLine();
            }
            Console.WriteLine("------------------------------");
            {
                Console.WriteLine("# 複数のIObservable<T>の統合(Switch)");
                // IObservable<T> Merge<T>(IObservable<IObservable<T>> source)
                var source = new Subject<int>();
                source
                    .Select(i => Observable
                        // 1 * i, 2 * i, 3 * iの値を1秒間隔で発行
                        .Interval(TimeSpan.FromSeconds(1)).Take(3)
                        .Select(l => (l + 1) * i))
                    // 最後
                    .Switch()
                    .Subscribe(
                        i => Console.WriteLine("OnNext: {0}", i),
                        () => Console.WriteLine("OnCompleted"));

                // 値の発行から完了
                Console.WriteLine("# OnNext(10)");
                source.OnNext(10);
                Console.WriteLine("Sleep 2000ms...");
                Thread.Sleep(2000);

                Console.WriteLine("# OnNext(100)");
                source.OnNext(100);
                Console.WriteLine("Sleep 2000ms...");
                Thread.Sleep(2000);

                Console.WriteLine("# OnNext(1000)");
                source.OnNext(1000);
                Console.WriteLine("Sleep 2000ms...");
                Thread.Sleep(2000);

                Console.WriteLine("# OnCompleted");
                source.OnCompleted();

                Console.ReadLine();
            }
            Console.WriteLine("------------------------------");
        }

        //-Concat Method : 繋げる
        static void ConcatSample()
        {
            Console.WriteLine("## ConcatSample");
            {
                Observable
                    // IObservable<T>のシーケンスを直列につなげる
                    .Concat(
                    // "1st: 0" ～ "1st: 2"までの値を1秒間隔で発行する
                        Observable.Interval(TimeSpan.FromSeconds(1)).Select(i => "1st: " + i).Take(3),
                    // "2nd: 0" ～ "2nd: 2"までの値を1秒間隔で発行する
                        Observable.Interval(TimeSpan.FromSeconds(1)).Select(i => "2nd: " + i).Take(3),
                    // "3rd: 0" ～ "3rd: 2"までの値を1秒間隔で発行する
                        Observable.Interval(TimeSpan.FromSeconds(1)).Select(i => "3rd: " + i).Take(3))
                    // 購読
                    .Subscribe(
                        s => Console.WriteLine("OnNext: {0}", s),
                        () => Console.WriteLine("OnCompleted"));

                Console.ReadLine();
            }
            Console.WriteLine("------------------------------");
            {
                // "1st: 0" ～ "1st: 2"までの値を1秒間隔で発行する
                Observable.Interval(TimeSpan.FromSeconds(1))
                    .Select(i => "1st: " + i)
                    .Take(3)
                    .Concat(
                    // "2nd: 0" ～ "2nd: 2"までの値を1秒間隔で発行する
                        Observable.Interval(TimeSpan.FromSeconds(1))
                            .Select(i => "2nd: " + i)
                            .Take(3))
                     .Concat(
                    // "3rd: 0" ～ "3rd: 2"までの値を1秒間隔で発行する
                        Observable.Interval(TimeSpan.FromSeconds(1))
                            .Select(i => "3rd: " + i)
                            .Take(3))
                    // 購読
                .Subscribe(
                    s => Console.WriteLine("OnNext: {0}", s),
                    () => Console.WriteLine("OnCompleted"));

                Console.ReadLine();
            }
            Console.WriteLine("------------------------------");
        }

        //-Zip Method : 2つのIO<T>を待ち合わせて後続へ流す
        static void ZipSample()
        {
            Console.WriteLine("## ZipSample");
            {
                // 1秒間隔で0から値をカウントアップしていくIObservable<T>のシーケンスの最初の3つ
                Observable.Interval(TimeSpan.FromSeconds(1)).Take(3)
                    // Zipでまとめる
                    .Zip(
                    // 100, 101, 102を発行するIObservable<T>のシーケンス
                        Observable.Range(100, 3),
                    // 渡された値を元に文字列化
                        (l, r) => string.Format("{0}-{1}", l, r))
                    // 購読
                    .Subscribe(
                        s => Console.WriteLine("OnNext: {0}", s),
                        () => Console.WriteLine("OnCompleted"));

                Console.ReadLine();
            }
            Console.WriteLine("------------------------------");
            {
                // 1秒間隔で0から値をカウントアップしていくIObservable<T>のシーケンスの最初の3つ
                Observable.Interval(TimeSpan.FromSeconds(1)).Take(3)
                    // Zipでまとめる
                    .Zip(
                    // 100を発行するIObservable<T>のシーケンス
                        Observable.Return(100),
                    // 渡された値を元に文字列化
                        (l, r) => string.Format("{0}-{1}", l, r))
                    // 購読
                    .Subscribe(
                        s => Console.WriteLine("OnNext: {0}", s),
                        () => Console.WriteLine("OnCompleted"));

                Console.ReadLine();
            }
            Console.WriteLine("------------------------------");
        }

        //-Amb Method ：一番最初に通知があったものを返す
        static void AmbSample()
        {
            Console.WriteLine("## AmbSample");
            Observable.Amb(
                // 3秒後に値を発行するIO<T>
                Observable.Timer(TimeSpan.FromSeconds(3)).Select(_ => "3sec"),
                // 10秒後に値を発行するIO<T>
                Observable.Timer(TimeSpan.FromSeconds(10)).Select(_ => "10sec"),
                // 2秒後に値を発行するIO<T>
                Observable.Timer(TimeSpan.FromSeconds(2)).Select(_ => "2sec"),
                // 6秒後に値を発行するIO<T>
                Observable.Timer(TimeSpan.FromSeconds(6)).Select(_ => "6sec"),
                // 22秒後に値を発行するIO<T>
                Observable.Timer(TimeSpan.FromSeconds(22)).Select(_ => "22sec"))
                .Subscribe(
                    s => Console.WriteLine("OnNext: {0}", s),
                    () => Console.WriteLine("OnCompleted"));

            Console.ReadLine();
            Console.WriteLine("------------------------------");
        }

        //-CombineLatest(TFirst, TSecond, TResult) Method : 最後の値をキャッシュして何かしらに変更があったときに再評価的な感じ
        static void CombineLatestSample()
        {
            Console.WriteLine("## CombineLatestSample");

            var source1 = new Subject<string>();
            var source2 = new Subject<int>();

            source1
                // source1とsource2を合成
                .CombineLatest(
                    source2,
                // 発行された値を結合して文字列化
                    (l, r) => string.Format("{0}-{1}", l, r))
                // 購読
                .Subscribe(
                    s => Console.WriteLine("OnNext: {0}", s),
                    () => Console.WriteLine("OnCompleted"));

            // 適当に値を発行する
            Console.WriteLine("source1.OnNext(foo)");
            source1.OnNext("foo");
            Console.WriteLine("source2.OnNext(100)");
            source2.OnNext(100);
            Console.WriteLine("source2.OnNext(200)");
            source2.OnNext(200);
            Console.WriteLine("source1.OnNext(bar)");
            source1.OnNext("bar");
            Console.WriteLine("source1.OnNext(boo)");
            source1.OnNext("boo");

            // source1完了
            Console.WriteLine("source1.OnCompleted()");
            source1.OnCompleted();
            // source1完了後にsource2から値を発行する
            Console.WriteLine("source2.OnNext(200)");
            source2.OnNext(999);
            // source2完了
            Console.WriteLine("source2.OnCompleted()");
            source2.OnCompleted();

            Console.WriteLine("------------------------------");
        }

        //-StartWith Method : IO<T>を引数で渡した値を最初に発行した後に続けて実行みたいな感じ。
        static void StartWithSample()
        {
            Console.WriteLine("## StartWithSample");
            Observable
                // 1～3の値を発行するIObservable<T>のシーケンス
                .Range(1, 3)
                // 頭に10, 20, 30をつける
                .StartWith(10, 20, 30)
                // 購読
                .Subscribe(
                    i => Console.WriteLine("OnNext: {0}", i));
            Console.WriteLine("------------------------------");
        }

        //-Join(TLeft, TRight, TLeftDuration, TRightDuration, TResult) Method : 内部結合？
        static void JoinSample()
        {
            Console.WriteLine("## JoinSample");
            {
                // Joinで合成するIObservable<T>
                var left = new Subject<int>();
                var right = new Subject<int>();

                left.Join(
                    right,
                    // leftから発行される値の有効期間は永久
                    _ => Observable.Never<Unit>(),
                    // rightから発行される値の有効期間は永久
                    _ => Observable.Never<Unit>(),
                    // 発行された値の組を作る
                    Tuple.Create)
                    // 購読
                    .Subscribe(
                    // 組を表示
                        tuple => Console.WriteLine("Left: {0}, Right: {1}", tuple.Item1, tuple.Item2),
                    // 完了を表示
                        () => Console.WriteLine("OnCompleted"));

                // 値の発行
                Console.WriteLine("left.OnNext(1)");
                left.OnNext(1);
                Console.WriteLine("right.OnNext(10)");
                right.OnNext(10);
                Console.WriteLine("right.OnNext(100)");
                right.OnNext(100);
                Console.WriteLine("left.OnNext(2)");
                left.OnNext(2);

                // 終了
                Console.WriteLine("left.OnCompleted()");
                left.OnCompleted();
                Console.WriteLine("right.OnCompleted()");
                right.OnCompleted();
            }
            Console.WriteLine("------------------------------");
            {
                // Joinで合成するIObservable<T>
                var left = new Subject<int>();
                var right = new Subject<int>();

                left.Join(
                    right,
                    // leftから発行される値の有効期間は永久
                    _ => Observable.Never<Unit>(),
                    // rightから発行される値の有効期間は一瞬
                    _ => Observable.Empty<Unit>(),
                    // 発行された値の組を作る
                    Tuple.Create)
                    // 購読
                    .Subscribe(
                        // 組を表示
                        tuple => Console.WriteLine("Left: {0}, Right: {1}", tuple.Item1, tuple.Item2),
                        // 完了を表示
                        () => Console.WriteLine("OnCompleted"));

                // 値の発行
                Console.WriteLine("left.OnNext(1)");
                left.OnNext(1);
                Console.WriteLine("right.OnNext(10)");
                right.OnNext(10);
                Console.WriteLine("right.OnNext(100)");
                right.OnNext(100);
                Console.WriteLine("left.OnNext(2)");
                left.OnNext(2);
                Console.WriteLine("right.OnNext(1000)");
                right.OnNext(1000);

                // 終了
                Console.WriteLine("left.OnCompleted()");
                left.OnCompleted();
                Console.WriteLine("right.OnCompleted()");
                right.OnCompleted();
            }
            Console.WriteLine("------------------------------");
        }

        //-GroupJoin(TLeft, TRight, TLeftDuration, TRightDuration, TResult) Method : 外部結合的なイメージ？
        static void GroupJoinSample()
        {
            Console.WriteLine("## GroupJoinSample");
            {
                // センサー名
                var sensors = new Subject<string>();
                // センサーが受信する値
                var values = new Subject<int>();

                // 値のリセット用Subject
                var valueReset = new Subject<Unit>();

                sensors.GroupJoin(
                    values,
                    // センサーは有効期限無し
                    _ => Observable.Never<Unit>(),
                    // センサーの値はvalueResetのOnNextで無効に出来る
                    _ => valueReset,
                    // センサーの名前と、センサーが受け取った値の現在の合計値を発行するLogにして後続に流す
                    (l, r) => new { Name = l, Log = r.Scan((x, y) => x + y) })
                    .Subscribe(
                        sensor =>
                        {
                            // Logを表示する
                            sensor
                                .Log
                                .Subscribe(i => Console.WriteLine("{0}: {1}", sensor.Name, i));
                        },
                        // 完了
                        () => Console.WriteLine("OnCompleted"));

                // センサーを2つ登録
                Console.WriteLine("sensors.OnNext(sensor1)");
                sensors.OnNext("sensor1");
                Console.WriteLine("sensors.OnNext(sensor2)");
                sensors.OnNext("sensor2");

                // 値を3つ発行
                Console.WriteLine("values.OnNext(100)");
                values.OnNext(100);
                Console.WriteLine("values.OnNext(10)");
                values.OnNext(10);
                Console.WriteLine("values.OnNext(1)");
                values.OnNext(1);

                // センサーの値を一旦リセット
                Console.WriteLine("valueReset.OnNext()");
                valueReset.OnNext(Unit.Default);

                // 新しいセンサーを追加
                Console.WriteLine("sensors.OnNext(sensor3)");
                sensors.OnNext("sensor3");
                // 値を3つ発行
                Console.WriteLine("values.OnNext(1)");
                values.OnNext(1);
                Console.WriteLine("values.OnNext(2)");
                values.OnNext(2);
                Console.WriteLine("values.OnNext(3)");
                values.OnNext(3);

                // 終了
                Console.WriteLine("values.OnCompleted()");
                values.OnCompleted();
                Console.WriteLine("sensors.OnCompleted()");
                sensors.OnCompleted();
            }
            Console.WriteLine("------------------------------");
        }

        //-And(TLeft, TRight) Method 
        //-When Method : Join系とあわせてつかう？
        //-Then(TSource, TResult) Method : Whenと合わせて使う？
        static void AndThenWhenSample()
        {
            Console.WriteLine("## AndThenWhenSample");
            {
                var plan = Observable
                    // plan1という文字列を10個発行するIObservable<string>
                    .Return("plan1").Repeat(10)
                    // 1秒間隔で0からのカウントアップにタイムスタンプをつけるIObservable<Timestamped<long>>
                    .And(Observable.Interval(TimeSpan.FromSeconds(1)).Timestamp())
                    // 100～110の値を発行するIObservable<int>
                    .And(Observable.Range(100, 10))
                    // 3つの値を文字列として纏める
                    .Then((planName, timestamped, value) => 
                        string.Format("{0} {1} {2}", planName, timestamped, value));
                // WhenでPlan<string>からIObservable<string>にして購読
                Observable.When(plan).Subscribe(
                    s => Console.WriteLine("OnNext: {0}", s),
                    () => Console.WriteLine("OnCompleted"));
                Console.ReadLine();
            }
            Console.WriteLine("------------------------------");
            {
                var plan1 = Observable
                    // plan1という文字列を10個発行するIObservable<string>
                    .Return("plan1").Repeat(10)
                    // 1秒間隔で0からのカウントアップにタイムスタンプをつけるIObservable<Timestamped<long>>
                    .And(Observable.Interval(TimeSpan.FromSeconds(1)).Timestamp())
                    // 100～110の値を発行するIObservable<int>
                    .And(Observable.Range(100, 10))
                    // 3つの値を文字列として纏める
                    .Then((planName, timestamped, value) =>
                        string.Format("{0} {1} {2}", planName, timestamped, value));
                
                var plan2 = Observable
                    // plan2という文字列を20個発行するIObservable<string>
                    .Return("plan2").Repeat(20)
                    // 0.5s間隔で0から値を発行していくIObservable<long>
                    .And(Observable.Interval(TimeSpan.FromSeconds(0.5)))
                    // Thenで文字列に纏める
                    .Then((s, l) => string.Format("{0} {1}", s, l));

                Observable.When(plan1, plan2).Subscribe(
                    s => Console.WriteLine("OnNext: {0}", s),
                    () => Console.WriteLine("OnCompleted"));
                Console.ReadLine();
            }
            Console.WriteLine("------------------------------");
        }

        static void ExecutePlan1(Subject<int>[] subjects)
        {
            Console.WriteLine("# ExecutePlan1");
            var r = new Random();
            foreach (var s in subjects)
            {
                s.OnNext(r.Next(100));
            }
        }

        static void ExecutePlan2(Subject<char>[] subjects)
        {
            Console.WriteLine("# ExecutePlan2");
            var r = new Random();
            var str = "abcdefghijklmnopqrstuvwxyz";
            foreach (var s in subjects)
            {
                s.OnNext(str[r.Next(str.Length) - 1]);
            }
        }

        static void ExecutePlan3(Subject<Unit>[] subjects)
        {
            Console.WriteLine("# ExecutePlan3");
            foreach (var s in subjects)
            {
                s.OnNext(Unit.Default);
            }
        }


    }
}
