namespace TakeAndSkipSample
{
    using System;
    using System.Reactive;
    using System.Reactive.Linq;
    using System.Reactive.Subjects;

    class Program
    {
        static void Main(string[] args)
        {
            //SkipAndTakeSample();
            //SkipAndTakeAndRepeatSample();
            //SkipWhileAndTakeWhileSample();
            //SkipUntilAndTakeUntilSample();
            SkipLastSample();
            TakeLastSample();
        }

        static void SkipAndTakeSample()
        {
            Console.WriteLine("## SkipAndTakeSample");
            // 値の発行元になるSubject
            var s = new Subject<int>();

            // 最初の3つをスキップして、その後3つを通知する
            s.Skip(3).Take(3).Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("OnCompleted()"));

            // 1-10の値を流し込む
            Observable.Range(1, 10).ForEach(i => s.OnNext(i));

            Console.WriteLine("---------------------------------");
        }

        static void SkipAndTakeAndRepeatSample()
        {
            Console.WriteLine("## SkipAndTakeAndRepeatSample");
            // 値の発行元になるSubject
            var s = new Subject<int>();

            // 最初の3つをスキップして、その後3つを通知することを繰り返す
            s.Skip(3).Take(3).Repeat().Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("OnCompleted()"));

            // 1-20の値を流し込む
            Observable.Range(1, 20).ForEach(i => s.OnNext(i));

            Console.WriteLine("---------------------------------");
        }

        static void SkipWhileAndTakeWhileSample()
        {
            Console.WriteLine("## SkipWhileAndTakeWhileSample");
            // 値の発行元になるSubject
            var s = new Subject<int>();

            // 発行される値が5より小さい間はスキップして10より小さい間は通過させる
            s.SkipWhile(i => i < 5).TakeWhile(i => i < 10).Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("OnCompleted()"));

            // 1-20の値を流し込む
            Console.WriteLine("1-20 OnNext start.");
            Observable.Range(1, 20).ForEach(i => s.OnNext(i));
            Console.WriteLine("1-20 OnNext end.");
            Console.WriteLine();

            Console.WriteLine("---------------------------------");
        }

        static void SkipUntilAndTakeUntilSample()
        {
            Console.WriteLine("## SkipUntilAndTakeUntilSample");
            // 値の発行元になるSubject
            var s = new Subject<int>();
            // 値のスキップを辞めるきっかけ
            var startTrigger = new Subject<Unit>();
            // 値を後続に流すことを辞めるきっかけ
            var endTrigger = new Subject<Unit>();

            // startTriggerのOnNextが呼ばれるまで値をスキップしてendTriggerのOnNextが
            // 呼ばれるまで後続に値を流す。
            s.SkipUntil(startTrigger).TakeUntil(endTrigger).Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("OnCompleted()"));

            // 1-5の値を流し込む
            Console.WriteLine("1-5 OnNext start.");
            Observable.Range(1, 5).ForEach(i => s.OnNext(i));
            Console.WriteLine("1-5 OnNext end.");
            Console.WriteLine();

            // startTriggerのOnNextを発行してから1-5の値を流し込む
            Console.WriteLine("startTrigger.OnNext called.");
            startTrigger.OnNext(Unit.Default);
            Console.WriteLine("1-5 OnNext start.");
            Observable.Range(1, 5).ForEach(i => s.OnNext(i));
            Console.WriteLine("1-5 OnNext end.");
            Console.WriteLine();

            // endTriggerのOnNextを発行してから1-5の値を流し込む
            Console.WriteLine("endTrigger.OnNext called.");
            endTrigger.OnNext(Unit.Default);
            Console.WriteLine("1-5 OnNext start.");
            Observable.Range(1, 5).ForEach(i => s.OnNext(i));
            Console.WriteLine("1-5 OnNext end.");
            Console.WriteLine();

            Console.WriteLine("---------------------------------");
        }

        static void SkipLastSample()
        {
            Console.WriteLine("## SkipLastSample");
            Observable
                // 1～10の値を発行する
                .Range(1, 10)
                // 最後3つをSkip
                .SkipLast(3)
                // 購読して表示
                .Subscribe(
                    i => Console.WriteLine("OnNext({0})", i),
                    () => Console.WriteLine("OnCompleted()"));
            Console.WriteLine("---------------------------------");
        }

        static void TakeLastSample()
        {
            Console.WriteLine("## TakeLastSample");
            Observable
                // 1～10の値を発行する
                .Range(1, 10)
                // 最後3つをTake
                .TakeLast(3)
                // 購読して表示
                .Subscribe(
                    i => Console.WriteLine("OnNext({0})", i),
                    () => Console.WriteLine("OnCompleted()"));
            Console.WriteLine("---------------------------------");
        }

    }
}
