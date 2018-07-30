using System;
using System.Reactive.Concurrency;
using System.Reactive.Disposables;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Reflection;
using System.Threading;

namespace RxSamples
{
    public class HotAndCold
    {
        public static void Test()
        {
            SimpleColdSample();
            //Publish1();
            //Publish2();
            //Publish3();
            //Publish4();
            //RefCount();
            //PublishLast();
            Replay();
        }

        public static void SimpleColdSample()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var observable = Observable.Interval(TimeSpan.FromSeconds(1));
            using (observable.Subscribe(i => Console.WriteLine("first subscription : {0}", i)))
            {
                Thread.Sleep(TimeSpan.FromSeconds(1.5));
                using (observable.Subscribe(i => Console.WriteLine("second subscription : {0}", i)))
                    Console.ReadKey();
            }
            /* Output: 
            first subscription : 0 
            first subscription : 1 
            second subscription : 0 
            first subscription : 2 
            second subscription : 1 
            first subscription : 3 
            second subscription : 2 
            */
        }

        public static void Publish1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var observable = Observable.Interval(TimeSpan.FromSeconds(1)).Publish();
            observable.Connect();
            using (observable.Subscribe(i => Console.WriteLine("first subscription : {0}", i)))
            {
                Thread.Sleep(TimeSpan.FromSeconds(2));
                using (observable.Subscribe(i => Console.WriteLine("second subscription : {0}", i)))
                    Console.ReadKey();
            }
        }

        public static void Publish2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var period = TimeSpan.FromSeconds(1);
            var observable = Observable.Interval(TimeSpan.FromSeconds(1)).Publish();
            using (observable.Subscribe(i => Console.WriteLine("first subscription : {0}", i)))
            {
                Thread.Sleep(TimeSpan.FromSeconds(2));
                using (observable.Subscribe(i => Console.WriteLine("second subscription : {0}", i)))
                {
                    observable.Connect();
                    Console.ReadKey();
                }
            }
        }

        public static void Publish3()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var period = TimeSpan.FromSeconds(1);
            var observable = Observable.Interval(period).Publish();
            observable.Subscribe(i => Console.WriteLine("subscription : {0}", i));
            var exit = false;
            while (!exit)
            {
                Console.WriteLine("Press enter to connect, esc to exit.");
                var key = Console.ReadKey(true);
                if (key.Key == ConsoleKey.Enter)
                {
                    var connection = observable.Connect(); //--Connects here--
                    Console.WriteLine("Press any key to dispose of connection.");
                    Console.ReadKey();
                    connection.Dispose(); //--Disconnects here--
                }
                if (key.Key == ConsoleKey.Escape)
                {
                    exit = true;
                }
            }
        }

        public static void Publish4()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var period = TimeSpan.FromSeconds(1);
            var observable = Observable.Interval(period)
            .Do(l => Console.WriteLine("Publishing {0}", l)) //Side effect to show it is running
            .Publish();
            var subscription2 = observable.Connect();
            Console.WriteLine("Press any key to subscribe");
            Console.ReadKey();
            var subscription = observable.Subscribe(i => Console.WriteLine("subscription : {0}", i));
            Console.WriteLine("Press any key to unsubscribe.");
            Console.ReadKey();
            subscription.Dispose();
            Console.WriteLine("Press any key to exit.");
            Console.ReadKey();
            subscription2.Dispose();
        }

        public static void RefCount()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var period = TimeSpan.FromSeconds(1);
            var observable = Observable.Interval(period)
            .Do(l => Console.WriteLine("Publishing {0}", l)) //side effect to show it is running
            .Publish()
            .RefCount();
            //observable.Connect(); Use RefCount instead now 
            Console.WriteLine("Press any key to subscribe");
            Console.ReadKey();
            var subscription = observable.Subscribe(i => Console.WriteLine("subscription : {0}", i));
            Console.WriteLine("Press any key to unsubscribe.");
            Console.ReadKey();
            subscription.Dispose();
            Console.WriteLine("Press any key to exit.");
            Console.ReadKey();
        }

        public static void PublishLast()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var period = TimeSpan.FromSeconds(1);
            var observable = Observable.Interval(period)
            .Take(5)
            .Do(l => Console.WriteLine("Publishing {0}", l)) //side effect to show it is running
            .PublishLast();
            observable.Connect();
            Console.WriteLine("Press any key to subscribe");
            Console.ReadKey();
            var subscription = observable.Subscribe(i => Console.WriteLine("subscription : {0}", i));
            Console.WriteLine("Press any key to unsubscribe.");
            Console.ReadKey();
            subscription.Dispose();
            Console.WriteLine("Press any key to exit.");
            Console.ReadKey();
        }

        public static void Replay()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var hot = Observable.Interval(TimeSpan.FromSeconds(1))
            .Take(3)
            .Publish();
            hot.Connect();
            Thread.Sleep(TimeSpan.FromSeconds(1.5)); //Run hot and ensure a value is lost.
            var observable = hot.Replay();
            observable.Connect();
            observable.Subscribe(i => Console.WriteLine("first subscription : {0}", i));
            Thread.Sleep(TimeSpan.FromSeconds(1.5));
            observable.Subscribe(i => Console.WriteLine("second subscription : {0}", i));
            Console.ReadKey();
            observable.Subscribe(i => Console.WriteLine("third subscription : {0}", i));
            Console.ReadKey();
        }

    }
}
