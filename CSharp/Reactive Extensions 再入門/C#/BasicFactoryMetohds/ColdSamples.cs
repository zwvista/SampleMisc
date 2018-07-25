using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.IO;
using System.Reactive.Disposables;

namespace BasicFactoryMetohds
{
    static class ColdSamples
    {
        public static void ReturnSample()
        {
            Console.WriteLine("## ReturnSample");
            // 10を発行するIObservable<int>を作成する
            var source = Observable.Return(10);
            // 購読
            var subscription = source.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("Completed()"));
            // 購読の停止（この場合意味はない）
            subscription.Dispose();
            Console.WriteLine("-----------------------------------");
        }

        public static void RepeatSample()
        {
            Console.WriteLine("## RepeatSample");
            // 2を5回発行するIObservable<int>を作成する
            var source = Observable.Repeat(2, 5);
            // 購読
            var subscription = source.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("Completed()"));
            // 購読停止（この場合意味はない）
            subscription.Dispose();
            Console.WriteLine("-----------------------------------");
        }

        public static void RangeSample()
        {
            Console.WriteLine("## RangeSample");
            // 1から始まる値を10個発行するIObservable<int>を作成する
            var source = Observable.Range(1, 10);
            // 購読
            var subscription = source.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("Completed()"));
            // 購読停止（この場合意味はない）
            subscription.Dispose();
            Console.WriteLine("-----------------------------------");
        }

        public static void RepeatExSample()
        {
            Console.WriteLine("## RangeSample");
            // 1から始まる値を3個発行するIObservable<int>を作成する
            var source = Observable.Range(1, 3);
            // そして、それを3回繰り返すIObservable<int>を作成する
            source = source.Repeat(3);
            // 購読
            var subscription = source.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("Completed()"));
            // 購読停止（この場合意味はない）
            subscription.Dispose();
            Console.WriteLine("-----------------------------------");
        }

        public static void GenerateSample()
        {
            Console.WriteLine("## GenerateSample");
            // 初期値0, 値が10より小さい間, 値は1ずつインクリメントして, 値を二乗したものを発行する
            // IObservable<int>を作成する。
            // for (int i = 0; i < 10; i++) { yield return i * i; }のようなイメージ
            var source = Observable.Generate(0, i => i < 10, i => ++i, i => i * i);
            // 購読
            var subscription = source.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("Completed()"));
            // 購読停止（この場合意味はない）
            subscription.Dispose();
            Console.WriteLine("-----------------------------------");
        }

        public static void DeferSample()
        {
            Console.WriteLine("## DeferSample");
            // 1, 2, 3と順番に値を発行して終了するIObservable<int>を生成する
            var source = Observable.Defer<int>(() =>
            {
                Console.WriteLine("# Defer method called.");
                // ReplaySubject<T>はSubject<T>の亜種でSubscribeされると
                // 今まで行われた操作を全てリプレイする。
                var s = new ReplaySubject<int>();
                s.OnNext(1);
                s.OnNext(2);
                s.OnNext(3);
                s.OnCompleted();
                // AsObservableでIObservable<T>へ変換できる。
                return s.AsObservable();
            });
            // 購読(sourceはReplaySubjectで作っているのでDeferメソッド内でした操作が再生される)
            var subscription1 = source.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("Completed()"));
            var subscription2 = source.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("Completed()"));
            // 購読停止（この場合意味はない）
            subscription1.Dispose();
            subscription2.Dispose();
            Console.WriteLine("-----------------------------------");
        }

        public static void CreateSample()
        {
            Console.WriteLine("## CreateSample");
            // 1, 2, 3と順番に値を発行して終了するIObservable<int>を生成する
            var source = Observable.Create<int>(observer =>
            {
                Console.WriteLine("# Create method called.");
                // 引数のIObserver<int>に対してOn****メソッドを呼ぶ
                observer.OnNext(1);
                observer.OnNext(2);
                observer.OnNext(3);
                observer.OnCompleted();
                // Disposeが呼ばれた時の処理を返す。
                // リソースを確保していた場合は、ここで解放すると良い。
                return () => Console.WriteLine("Disposable action");
            });
            // 購読
            var subscription1 = source.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("Completed()"));
            var subscription2 = source.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("Completed()"));
            // 購読停止（この場合意味はない）
            Console.WriteLine("# Dispose method call.");
            subscription1.Dispose();
            subscription2.Dispose();
            Console.WriteLine("-----------------------------------");
        }

        public static void ThrowSample()
        {
            Console.WriteLine("## ThrowSample");
            // エラーを発行するだけのIObservable<int>を生成
            var source = Observable.Throw<int>(new Exception("Error message"));
            // 購読
            var subscription = source.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("Completed()"));
            // 購読停止（この場合意味はない）
            subscription.Dispose();
            Console.WriteLine("-----------------------------------");
        }

        public static void TimerSample()
        {
            Console.WriteLine("## TimerSample");
            // 3秒後から1秒間隔で値を発行するIObservable<long>を作成する
            var source = Observable.Timer(
                TimeSpan.FromSeconds(3),
                TimeSpan.FromSeconds(1));
            // 購読
            var subscription = source.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("Completed()"));

            // 3秒後からOnNext(回数)が表示される
            Console.WriteLine("please enter key...");
            Console.ReadLine();
            // Observableが発行する値の購読を停止
            Console.WriteLine("dispose method call.");
            subscription.Dispose();
            // Disposeをすると値が発行されても受け取らなくなる。
            Console.WriteLine("please enter key...");
            Console.ReadLine();
            Console.WriteLine("-----------------------------------");
        }

        public static void IntervalSample()
        {
            Console.WriteLine("## IntervalSample");
            // 500ms間隔で値を発行する
            var source = Observable.Interval(TimeSpan.FromMilliseconds(500));
            // 購読
            var subscription = source.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("Completed()"));

            Console.WriteLine("please enter key...");
            Console.ReadLine();
            // Observableが発行する値の購読を停止
            Console.WriteLine("dispose method call.");
            subscription.Dispose();
            // Disposeをすると値が発行されても受け取らなくなる。
            Console.WriteLine("please enter key...");
            Console.ReadLine();
            Console.WriteLine("-----------------------------------");
        }

        public static void GenerateSampleWithTimespan()
        {
            Console.WriteLine("## GenerateSampleWithTimespan");
            var source = Observable.Generate(
                // 0から
                0,
                // i < 10以下の間繰り返す
                i => i < 10,
                // iは1ずつ増える
                i => ++i,
                // 発行する値はiの二乗
                i => i * i,
                // 値は(発行する値 * 100)ms間隔で発行する
                i => TimeSpan.FromMilliseconds(i * 100));

            // 購読
            var subscription = source.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("Completed()"));

            Console.WriteLine("please enter key...");
            Console.ReadLine();
            // Observableが発行する値の購読を停止
            Console.WriteLine("dispose method call.");
            subscription.Dispose();
            // Disposeをすると値が発行されても受け取らなくなる。
            Console.WriteLine("please enter key...");
            Console.ReadLine();
            Console.WriteLine("-----------------------------------");
        }

        public static void UsingSample()
        {
            Console.WriteLine("## UsingSample");
            // SampleResource(IDisposableを実装)を使用してデータを取得する
            var source = Observable.Using(
                () => new SampleResource(),
                sr => sr.GetData());
            // 購読
            source.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("Completed()"));

            Console.WriteLine("-----------------------------------");
        }

        // サンプルのダミーリソースクラス
        class SampleResource : IDisposable
        {
            // データを取得する
            public IObservable<string> GetData()
            {
                return Observable.Create<string>(o =>
                {
                    o.OnNext("one");
                    o.OnNext("two");
                    o.OnNext("three");
                    o.OnCompleted();
                    return Disposable.Empty;
                });
            }

            // 解放処理
            public void Dispose()
            {
                Console.WriteLine("Resource.Dispose called");
            }
        }


    }
}
