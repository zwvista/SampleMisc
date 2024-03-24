using System;
using System.Reactive;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Reflection;

namespace RxSamples
{
    public static class Coincidence
    {
        public static void Test()
        {
            Window1();
            Window2();
        }

        /*
            source |-0-1-2-3-4-5-6-7-8-9|
            window0|-0-1-|
            window1      2-3-4|
            window2           -5-6-|
            window3                7-8-9|
        */
        private static void Window1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var windowIdx = 0;
            var source = Observable.Interval(TimeSpan.FromSeconds(1)).Take(10);
            source.Window(3)
            .Subscribe(window =>
            {
                var thisWindowIdx = windowIdx++;
                Console.WriteLine("--Starting new window");
                var windowName = "Window" + thisWindowIdx;
                window.Subscribe(
                value => Console.WriteLine("{0} : {1}", windowName, value),
                ex => Console.WriteLine("{0} : {1}", windowName, ex),
                () => Console.WriteLine("{0} Completed", windowName));
            },
            () => Console.WriteLine("Completed"));
            Console.ReadKey();
        }

        private static void Window2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var windowIdx = 0;
            var source = Observable.Interval(TimeSpan.FromSeconds(1)).Take(10);
            var closer = new Subject<Unit>();
            source.Window(() => closer)
            .Subscribe(window =>
            {
                var thisWindowIdx = windowIdx++;
                Console.WriteLine("--Starting new window");
                var windowName = "Window" + thisWindowIdx;
                window.Subscribe(
                value => Console.WriteLine("{0} : {1}", windowName, value),
                ex => Console.WriteLine("{0} : {1}", windowName, ex),
                () => Console.WriteLine("{0} Completed", windowName));
            },
            () => Console.WriteLine("Completed"));
            var input = "";
            while (input != "exit")
            {
                input = Console.ReadLine();
                closer.OnNext(Unit.Default);
            }
        }

    }
}
