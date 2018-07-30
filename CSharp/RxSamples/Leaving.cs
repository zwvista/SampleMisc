using System;
using System.Reactive;
using System.Reactive.Linq;
using System.Reflection;

namespace RxSamples
{
    public static class Leaving
    {
        public static void Test()
        {
            ForEachAsync();
            ToEnumerable();
            ToArray();
            Wait1();
            Wait2();
            ToEvent();
            ToEventPattern();
        }

        public static void ForEachAsync()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Interval(TimeSpan.FromSeconds(1))
                .Take(5);
            source.ForEachAsync(i => Console.WriteLine("received {0} @ {1}", i, DateTime.Now)).Wait();
            Console.WriteLine("completed @ {0}", DateTime.Now);
        }

        public static void ToEnumerable()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var period = TimeSpan.FromMilliseconds(200);
            var source = Observable.Timer(TimeSpan.Zero, period)
            .Take(5);
            var result = source.ToEnumerable();
            foreach (var value in result)
            {
                Console.WriteLine(value);
            }
            Console.WriteLine("done");
        }

        public static void ToArray()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var period = TimeSpan.FromMilliseconds(200); 
            var source = Observable.Timer(TimeSpan.Zero, period).Take(5); 
            var result = source.ToArray(); 
            result.Subscribe( 
                arr => { 
                    Console.WriteLine("Received array"); 
                    foreach (var value in arr) 
                    { 
                    Console.WriteLine(value); 
                    } 
                }, 
                () => Console.WriteLine("Completed")
            ); 
            Console.WriteLine("Subscribed");
        }

        public static void Wait1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Interval(TimeSpan.FromSeconds(1)).Take(3);
            var result = source.Wait(); //Will arrive in 3 seconds. 
            Console.WriteLine(result);
        }

        public static void Wait2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Throw<long>(new Exception("Fail!"));
            try
            {
                source.Wait();
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
        }

        public static void ToEvent()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Interval(TimeSpan.FromSeconds(1)).Take(5);
            var result = source.ToEvent();
            result.OnNext += val => Console.WriteLine(val);
            Console.ReadKey();
        }

        public class MyEventArgs : EventArgs 
        { 
            private readonly long _value; 
            public MyEventArgs(long value) 
            { 
                _value = value; 
            } 
            public long Value 
            { 
                get { return _value; } 
            }
        } 

        public static void ToEventPattern()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Interval(TimeSpan.FromSeconds(1))
            .Select(i => new EventPattern<MyEventArgs>(null, new MyEventArgs(i)));
            var result = source.ToEventPattern();
            result.OnNext += (e, val) => Console.WriteLine(val.Value);
            Console.ReadKey();
        }

    }
}
