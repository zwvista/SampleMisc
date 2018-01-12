namespace ConvertEnumerableSample
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Reactive.Subjects;
    using System.Reactive.Linq;
    using System.Threading.Tasks;
    using System.Threading;
    using System.Reactive.Concurrency;

    class Program
    {
        static void Main(string[] args)
        {
            MostRecentSample();
            LatestSample();
            LatestSample2();
            NextSample();
            ToEnumerableSample();
            ToEnumerableColdSample();
        }

        public static void MostRecentSample()
        {
            Console.WriteLine("# MostRecentSample");
            // 値の発行元
            var s = new Subject<int>();
            // 初期値を-1にしてIObservable<T>からIEnumerableに変換してIEnumeratorを取得
            var e = s.MostRecent(-1).GetEnumerator();

            // 一度も値を発行してない状態
            Console.WriteLine("一度も値を発行していない場合は初期値が返される");
            Console.WriteLine("MoveNext : {0}", e.MoveNext());
            Console.WriteLine("CurrentValue : {0}", e.Current);
            Console.WriteLine("MoveNext : {0}", e.MoveNext());
            Console.WriteLine("CurrentValue : {0}", e.Current);

            // 値を発行した状態の確認
            Console.WriteLine("-----");
            Console.WriteLine("OnNext(10)");
            s.OnNext(10);
            Console.WriteLine("最後に発行した値が返されるようになる");
            Console.WriteLine("MoveNext : {0}", e.MoveNext());
            Console.WriteLine("CurrentValue : {0}", e.Current);
            Console.WriteLine("MoveNext : {0}", e.MoveNext());
            Console.WriteLine("CurrentValue : {0}", e.Current);

            // IEnumeratorで値を取得する前に2回値が発行された状態の確認
            Console.WriteLine("-----");
            Console.WriteLine("OnNext(100)");
            s.OnNext(100);
            Console.WriteLine("OnNext(1000)");
            s.OnNext(1000);
            Console.WriteLine("最後に発行した値が返されるようになる");
            Console.WriteLine("MoveNext : {0}", e.MoveNext());
            Console.WriteLine("CurrentValue : {0}", e.Current);
            Console.WriteLine("MoveNext : {0}", e.MoveNext());
            Console.WriteLine("CurrentValue : {0}", e.Current);

            // OnCompletedの後の状態の確認
            Console.WriteLine("-----");
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("OnCompletedを呼ぶと、MoveNextがFalseを返すようになる");
            Console.WriteLine("MoveNext : {0}", e.MoveNext());
            Console.WriteLine("-------------------------");
        }

        public static void LatestSample()
        {
            Console.WriteLine("# LatestSample");
            // 値の発行元
            var s = new Subject<int>();

            // Latestで取得したIEnumerable<int>の値を印字
            Observable.Start(() =>
            {
                Console.WriteLine("Start latest loop");
                foreach (var i in s.Latest())
                {
                    Console.WriteLine("LatestValue : {0}", i);
                }
                Console.WriteLine("End latest loop");
            });

            // 1秒間隔で値1～10の値を発行
            Observable.Start(() =>
            {
                foreach (var i in Enumerable.Range(1, 10))
                {
                    Thread.Sleep(1000);
                    Console.WriteLine("OnNext({0})", i);
                    s.OnNext(i);
                }
                Console.WriteLine("OnCompleted()");
                s.OnCompleted();
            });

            // 終了しないため待つ
            Console.ReadLine();
            Console.WriteLine("-------------------------");
        }

        public static void LatestSample2()
        {
            Console.WriteLine("# LatestSample2");
            // 値の発行元
            var s = new Subject<int>();

            // Latestで変換したIEnumerableからIEnumeratorを取得
            var e = s.Latest().GetEnumerator();

            // 1を発行してIEnumeratorから値を取得
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("MoveNext : {0}", e.MoveNext());
            Console.WriteLine("Current : {0}", e.Current);

            // 10と100を発行してIEnumeratorから値を取得
            Console.WriteLine("OnNext(10)");
            s.OnNext(10);
            Console.WriteLine("OnNext(100)");
            s.OnNext(100);
            Console.WriteLine("MoveNext : {0}", e.MoveNext());
            Console.WriteLine("Current : {0}", e.Current);

            // IObservable<T>のシーケンスを終了したMoveNextを呼んでみる
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();
            Console.WriteLine("MoveNext : {0}", e.MoveNext());

            Console.WriteLine("-------------------------");
        }

        public static void NextSample()
        {
            Console.WriteLine("# NextSample");
            // 初期値 -1のBehaviorSubject
            var s = new BehaviorSubject<int>(-1);

            // Latestの動作
            Observable.Start(() =>
            {
                Console.WriteLine("Latest start");
                foreach (var i in s.Latest())
                {
                    Console.WriteLine("Latest : {0}", i);
                }
            });

            // Nextの動作
            Observable.Start(() =>
            {
                Console.WriteLine("Next start");
                foreach (var i in s.Next())
                {
                    Console.WriteLine("Next : {0}", i);
                }
            });

            // 1秒間隔で値を発行する
            Observable.Start(() =>
            {
                var i = 0;
                while (true)
                {
                    Thread.Sleep(1000);
                    Console.WriteLine("OnNext({0})", ++i);
                    s.OnNext(i);
                }
            });

            // 待機
            Console.WriteLine("Please any key.");
            Console.ReadLine();
            Console.WriteLine("End NextSample");
            Console.WriteLine("-------------------------");
        }

        static void ToEnumerableSample()
        {
            Console.WriteLine("# ToEnumerableSample");
            var s = new Subject<int>();
            // 進捗状況を管理するためのWaitHandle
            var gate = new EventWaitHandle(false, EventResetMode.AutoReset);
            Observable.Start(() =>
            {
                // IO<T> -> IE<T>への変換
                var e = s.ToEnumerable();
                // 待ち解除
                gate.Set();
                // 値の表示
                foreach (var i in e)
                {
                    Console.WriteLine("value : {0}", i);
                }
            })
                // 最後にWaitHandleを発火
            .Finally(() => gate.Set())
            .Subscribe();

            // ToEnumerableされるまで待つ
            gate.WaitOne();

            // 値の発行からCompleted
            Console.WriteLine("OnNext(1)");
            s.OnNext(1);
            Console.WriteLine("OnNext(2)");
            s.OnNext(2);
            Console.WriteLine("OnNext(3)");
            s.OnNext(3);
            Console.WriteLine("OnCompleted()");
            s.OnCompleted();

            // 列挙が終わるのを待つ
            gate.WaitOne();
            Console.WriteLine("----------------------------");
        }

        static void ToEnumerableColdSample()
        {
            Console.WriteLine("# ToEnumerableColdSample");
            // ColdなIObservable<int>を作成
            var s = Observable.Range(1, 5);
            // IEnumerable<int>に変換して列挙
            foreach (var i in s.ToEnumerable())
            {
                Console.WriteLine("value : {0}", i);
            }
            Console.WriteLine("----------------------------");
        }

    }

}
