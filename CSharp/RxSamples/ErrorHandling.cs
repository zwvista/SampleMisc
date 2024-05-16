using System;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Reflection;

namespace RxSamples
{
    public static class ErrorHandling
    {
        public static void Test()
        {
            Catch1();
            Catch2();
            Catch3();
            Finally1();
            Finally2();
            // Finally3();
            Using();
        }

        /*
            S1--1--2--3--X
            S2            -|
            R --1--2--3----|
        */
        private static void Catch1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var source = new Subject<int>();
            var result = source.Catch(Observable.Empty<int>());
            result.Dump("Catch");
            source.OnNext(1);
            source.OnNext(2);
            source.OnError(new Exception("Fail!"));
        }

        private static void Catch2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var source = new Subject<int>();
            var result = source.Catch<int, TimeoutException>(tx => Observable.Return(-1));
            result.Dump("Catch");
            source.OnNext(1);
            source.OnNext(2);
            source.OnError(new TimeoutException());
        }

        private static void Catch3()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var source = new Subject<int>();
            var result = source.Catch<int, TimeoutException>(tx => Observable.Return(-1));
            result.Dump("Catch");
            source.OnNext(1);
            source.OnNext(2);
            source.OnError(new ArgumentException("Fail!"));
        }

        private static void Finally1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var source = new Subject<int>();
            var result = source.Finally(() => Console.WriteLine("Finally action ran"));
            result.Dump("Finally");
            source.OnNext(1);
            source.OnNext(2);
            source.OnNext(3);
            source.OnCompleted();
        }

        private static void Finally2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var source = new Subject<int>();
            var result = source.Finally(() => Console.WriteLine("Finally"));
            var subscription = result.Subscribe(
            Console.WriteLine,
            Console.WriteLine,
            () => Console.WriteLine("Completed"));
            source.OnNext(1);
            source.OnNext(2);
            source.OnNext(3);
            subscription.Dispose();
        }

        private static void Finally3()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var source = new Subject<int>();
            var result = source.Finally(() => Console.WriteLine("Finally"));
            result.Subscribe(
            Console.WriteLine,
            //Console.WriteLine,
            () => Console.WriteLine("Completed"));
            source.OnNext(1);
            source.OnNext(2);
            source.OnNext(3);
            //Brings the app down. Finally action is not called.
            source.OnError(new Exception("Fail"));
        }

        private static void Using()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var source = Observable.Interval(TimeSpan.FromSeconds(1));
            var result = Observable.Using(
            () => new TimeIt("Subscription Timer"),
            timeIt => source);
            result.Take(5).Dump("Using");
            Console.ReadKey();
        }
    }
}
