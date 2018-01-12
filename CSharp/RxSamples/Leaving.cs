using System;
using System.Reactive.Linq;
using System.Reflection;

namespace RxSamples
{
    public static class Leaving
    {
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
            Console.ReadLine();
        }
    }
}
