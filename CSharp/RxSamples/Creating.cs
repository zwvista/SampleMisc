using System;
using System.Collections.Generic;
using System.Reactive.Linq;
using System.Reactive.Threading.Tasks;
using System.Reflection;
using System.Threading.Tasks;
using System.Timers;
using Timer = System.Timers.Timer;

namespace RxSamples
{
    public static class Creating
    {
        public static void Test()
        {
            ReturnEmptyNeverThrow();
            Create();
            Range();
            Interval();
            Timer();
            ToObservable1();
            ToObservable2();
        }

        private static void ReturnEmptyNeverThrow()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var singleValue = Observable.Return<string>("Value");
            var empty = Observable.Empty<string>();
            var never = Observable.Never<string>();
            var throws = Observable.Throw<string>(new Exception());
            singleValue.Subscribe(
                Console.WriteLine,
                () => Console.WriteLine("completed"));
        }

        private static void OnTimerElapsed(object? sender, ElapsedEventArgs e)
        {
            Console.WriteLine(e.SignalTime);
        }

        private static void Create()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var ob = Observable.Create<string>(
            observer =>
            {
                var timer = new Timer();
                timer.Interval = 1000;
                timer.Elapsed += (s, e) => observer.OnNext("tick");
                timer.Elapsed += OnTimerElapsed;
                timer.Start();
                return timer;
            });
            using (ob.Subscribe(Console.WriteLine))
                Console.ReadKey();
        }

        private static void Range()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var range = Observable.Range(10, 15);
            range.Subscribe(Console.WriteLine, () => Console.WriteLine("Completed"));
        }

        private static void Interval()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var interval = Observable.Interval(TimeSpan.FromMilliseconds(250));
            using (interval.Subscribe(
                Console.WriteLine,
                () => Console.WriteLine("Completed")))
                Console.ReadKey();
        }

        private static void Timer()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var timer = Observable.Timer(TimeSpan.FromSeconds(1));
            timer.Subscribe(
                Console.WriteLine,
                () => Console.WriteLine("Completed"));
            Console.ReadKey();
        }

        private static void ToObservable1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var values = new List<string> { "Rx", "is", "easy" };
            values.ToObservable().Subscribe(
                Console.WriteLine,
                () => Console.WriteLine("Completed"));
            Console.ReadKey();
        }

        private static void ToObservable2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var t = Task.Factory.StartNew(() => "Test");
            var source = t.ToObservable();
            source.Subscribe(
                Console.WriteLine,
                () => Console.WriteLine("Completed"));
            Console.ReadKey();
        }

    }
}
