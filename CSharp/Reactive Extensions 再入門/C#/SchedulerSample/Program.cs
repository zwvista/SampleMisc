using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Linq;
using System.Reactive.Concurrency;
using System.Threading;

namespace SchedulerSample
{
    class Program
    {
        static void Main(string[] args)
        {
            DiffrentCurrentThreadToImmediate();
            CurrentThreadSchedulerSample();
            ObserveOnSample();
            HistoricalSchedulerSample();
        }

        static void DiffrentCurrentThreadToImmediate()
        {
            Console.WriteLine("# CurrentThreadSchedulerSample");
            {
                Console.WriteLine("CurrentThread");
                // ChrrentThreadSchedulerで処理を実行
                Scheduler.CurrentThread.Schedule(() =>
                {
                    Console.WriteLine("Start");
                    // 処理の内部でさらに処理をCurrentThreadSchedulerに依頼
                    Scheduler.CurrentThread.Schedule(() =>
                    {
                        Console.WriteLine("    Start");
                        Console.WriteLine("    End");
                    });
                    Console.WriteLine("End");
                });

                Console.WriteLine("Immediate");
                // ImmediateSchedulerで処理を実行
                Scheduler.Immediate.Schedule(() =>
                {
                    Console.WriteLine("Start");
                    // 処理の内部でさらに処理をImmediateSchedulerに依頼
                    Scheduler.Immediate.Schedule(() =>
                    {
                        Console.WriteLine("    Start");
                        Console.WriteLine("    End");
                    });
                    Console.WriteLine("End");
                });
            }
            Console.WriteLine("---------------------------------");
        }
        
        static void CurrentThreadSchedulerSample()
        {
            Console.WriteLine("# CurrentThreadSchedulerSample");
            {
                Console.WriteLine("Normal");
                // 開始のログ
                Console.WriteLine("{0:HH:mm:ss}: Start!!", DateTime.Now);
                // 1秒後に値を発行する
                Observable.Timer(TimeSpan.FromSeconds(1))
                    .Subscribe(
                        i => Console.WriteLine("OnNext({0})", i),
                        () => Console.WriteLine("OnCompleted"));
                // 終了のログ
                Console.WriteLine("{0:HH:mm:ss}: End!!", DateTime.Now);
                // Enterが押されるまで待つ
                Console.ReadLine();

                Console.WriteLine("CurrentThreadScheduler");
                // 開始のログ
                Console.WriteLine("{0:HH:mm:ss}: Start!!", DateTime.Now);
                // 1秒後に値を発行する, 実行場所は現在のスレッド
                Observable.Timer(TimeSpan.FromSeconds(1), Scheduler.CurrentThread)
                    .Subscribe(
                        i => Console.WriteLine("OnNext({0})", i),
                        () => Console.WriteLine("OnCompleted"));
                // 終了のログ
                Console.WriteLine("{0:HH:mm:ss}: End!!", DateTime.Now);
                // Enterが押されるまで待つ
                Console.ReadLine();
            }
            Console.WriteLine("---------------------------------");
        }

        static void HistoricalSchedulerSample()
        {
            Console.WriteLine("# HistoricalSchedulerSample");
            {
                var scheduler = new HistoricalScheduler();
                // 1秒間隔で値を発行する
                Observable.Interval(TimeSpan.FromSeconds(1), scheduler)
                    // 購読
                    .Subscribe(
                        i => Console.WriteLine("OnNext({0})", i),
                        () => Console.WriteLine("OnCompleted"));

                // HistoricalSchedulerを使ってプログラム内でSchedulerの時間を進める
                // 0.5sec時間を進める
                Console.WriteLine("AdvanceBy(0.5sec)");
                scheduler.AdvanceBy(TimeSpan.FromSeconds(0.5));
                // 0.5sec時間を進める
                Console.WriteLine("AdvanceBy(0.5sec)");
                scheduler.AdvanceBy(TimeSpan.FromSeconds(0.5));
                // 0.5sec時間を進める
                Console.WriteLine("AdvanceBy(0.5sec)");
                scheduler.AdvanceBy(TimeSpan.FromSeconds(0.5));
                // 0.5sec時間を進める
                Console.WriteLine("AdvanceBy(0.5sec)");
                scheduler.AdvanceBy(TimeSpan.FromSeconds(0.5));
                // 0.5sec時間を進める
                Console.WriteLine("AdvanceBy(0.5sec)");
                scheduler.AdvanceBy(TimeSpan.FromSeconds(0.5));
            }
            Console.WriteLine("---------------------------------");
        }


        static void ObserveOnSample()
        {
            Console.WriteLine("# ObserveOnSample");
            {
                // 1秒間隔で3つ値を発行する
                Observable
                    .Interval(TimeSpan.FromSeconds(1))
                    .Take(3)
                    // 現在のスレッドIDを表示
                    .Do(_ => Console.WriteLine("ThreadId: {0}", Thread.CurrentThread.ManagedThreadId))
                    // 実行スレッドを新しいスレッドに切り替える
                    .ObserveOn(Scheduler.NewThread)
                    // 現在のスレッドIDを表示
                    .Do(_ => Console.WriteLine("ThreadId: {0}", Thread.CurrentThread.ManagedThreadId))
                    // 購読して、発行された値とスレッドIDを表示
                    .Subscribe(
                        i => Console.WriteLine("OnNext({0}), ThreadId: {1}", i, Thread.CurrentThread.ManagedThreadId),
                        () => Console.WriteLine("OnCompleted, ThreadId: {0}", Thread.CurrentThread.ManagedThreadId));

                Console.ReadLine();
            }
            Console.WriteLine("---------------------------------");
        }
    }
}
