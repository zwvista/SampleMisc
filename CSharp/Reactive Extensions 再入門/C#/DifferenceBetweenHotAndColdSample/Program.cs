using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Linq;
using System.Threading;
using System.Reactive.Disposables;
using System.Timers;

namespace DifferenceBetweenHotAndColdSample
{
    class Program
    {
        static void Main(string[] args)
        {
            //ColdSample();
            HotSample();
        }

        static void ColdSample()
        {
            Console.WriteLine("## ColdSample");
            // 1秒間隔で値を発行するIObservable<long>を作成する
            var source = Observable.Timer(
                TimeSpan.FromSeconds(1),
                TimeSpan.FromSeconds(1));
            // 購読
            var subscription1 = source.Subscribe(
                i => Console.WriteLine("{0:yyyy/MM/dd HH:mm:ss.FFF} 1##OnNext({1})", DateTime.Now, i),
                ex => Console.WriteLine("1##OnError({0})", ex.Message),
                () => Console.WriteLine("1##Completed()"));

            // 3秒後にもう一度購読
            Thread.Sleep(3000);
            // 購読
            var subscription2 = source.Subscribe(
                i => Console.WriteLine("{0:yyyy/MM/dd HH:mm:ss.FFF} 2##OnNext({1})", DateTime.Now, i),
                ex => Console.WriteLine("2##OnError({0})", ex.Message),
                () => Console.WriteLine("2##Completed()"));

            Console.ReadLine();
            subscription1.Dispose();
            subscription2.Dispose();
            Console.WriteLine("-----------------------------------");
        }

        static void HotSample()
        {
            Console.WriteLine("## HotSample");
            // 1秒間隔で値を発行するTimer
            var timer = new System.Timers.Timer(1000);
            var source = Observable.FromEvent<ElapsedEventHandler, ElapsedEventArgs>(
                h => (s, e) => h(e),
                h => timer.Elapsed += h,
                h => timer.Elapsed -= h);
            // タイマー開始
            timer.Start();

            // 購読
            var subscription1 = source.Subscribe(
                e => Console.WriteLine("{0:yyyy/MM/dd HH:mm:ss.FFF} 1##OnNext({1:yyyy/MM/dd HH:mm:ss.FFF})",DateTime.Now, e.SignalTime),
                ex => Console.WriteLine("1##OnError({0})", ex.Message),
                () => Console.WriteLine("1##Completed()"));

            // 3秒後にもう一度購読
            Thread.Sleep(3000);
            // 購読
            var subscription2 = source.Subscribe(
                e => Console.WriteLine("{0:yyyy/MM/dd HH:mm:ss.FFF} 2##OnNext({1:yyyy/MM/dd HH:mm:ss.FFF})",DateTime.Now, e.SignalTime),
                ex => Console.WriteLine("2##OnError({0})", ex.Message),
                () => Console.WriteLine("2##Completed()"));

            Console.ReadLine();
            subscription1.Dispose();
            subscription2.Dispose();
            timer.Stop();
            Console.WriteLine("-----------------------------------");
        }
    }
}
