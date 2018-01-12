using System;
using System.Reactive.Linq;
using System.Reflection;

namespace RxSamples
{
    public static class TimeShifted
    {
        public static void Buffer1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var idealBatchSize = 15;
            var maxTimeDelay = TimeSpan.FromSeconds(3);
            var source = Observable.Interval(TimeSpan.FromSeconds(1)).Take(10)
            .Concat(Observable.Interval(TimeSpan.FromSeconds(0.01)).Take(100));
            source.Buffer(maxTimeDelay, idealBatchSize)
            .Subscribe(
            buffer => Console.WriteLine("Buffer of {1} @ {0}", DateTime.Now, buffer.Count),
            () => Console.WriteLine("Completed"));
            Console.ReadLine();
        }

        public static void Buffer2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Interval(TimeSpan.FromSeconds(1)).Take(10);
            source.Buffer(3, 1)
            .Subscribe(
            buffer =>
            {
                Console.WriteLine("--Buffered values");
                foreach (var value in buffer)
                {
                    Console.WriteLine(value);
                }
            }, () => Console.WriteLine("Completed"));
            source.Buffer(TimeSpan.FromSeconds(3), TimeSpan.FromSeconds(1))
            .Subscribe(
            buffer =>
            {
                Console.WriteLine("--Buffered values");
                foreach (var value in buffer)
                {
                    Console.WriteLine(value);
                }
            }, () => Console.WriteLine("Completed"));
            Console.ReadLine();
        }

        public static void Buffer3()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Interval(TimeSpan.FromSeconds(1)).Take(10);
            source.Buffer(3, 3)
            .Subscribe(
            buffer =>
            {
                Console.WriteLine("--Buffered values");
                foreach (var value in buffer)
                {
                    Console.WriteLine(value);
                }
            }, () => Console.WriteLine("Completed"));
            source.Buffer(TimeSpan.FromSeconds(3), TimeSpan.FromSeconds(3))
            .Subscribe(
            buffer =>
            {
                Console.WriteLine("--Buffered values");
                foreach (var value in buffer)
                {
                    Console.WriteLine(value);
                }
            }, () => Console.WriteLine("Completed"));
            Console.ReadLine();
        }

        public static void Buffer4()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Interval(TimeSpan.FromSeconds(1)).Take(10);
            source.Buffer(3, 5)
            .Subscribe(
            buffer =>
            {
                Console.WriteLine("--Buffered values");
                foreach (var value in buffer)
                {
                    Console.WriteLine(value);
                }
            }, () => Console.WriteLine("Completed"));
            source.Buffer(TimeSpan.FromSeconds(3), TimeSpan.FromSeconds(5))
            .Subscribe(
            buffer =>
            {
                Console.WriteLine("--Buffered values");
                foreach (var value in buffer)
                {
                    Console.WriteLine(value);
                }
            }, () => Console.WriteLine("Completed"));
            Console.ReadLine();
        }

        public static void Delay()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Interval(TimeSpan.FromSeconds(1))
            .Take(5)
            .Timestamp();
            var delay = source.Delay(TimeSpan.FromSeconds(2));
            source.Subscribe(
            value => Console.WriteLine("source : {0}", value),
            () => Console.WriteLine("source Completed"));
            delay.Subscribe(
            value => Console.WriteLine("delay : {0}", value),
            () => Console.WriteLine("delay Completed"));
            Console.ReadLine();
        }

        /*
            Relative time (ms) Source value    Sampled value
            150                 0   
            300                 1   
            450                 2   
            600                 3   
            750                 4   
            900                 5   
            1000                                5
            1050                6   
            1200                7   
            1350                8   
            1500                9   
            1650                10  
            1800                11  
            1950                12  
            2000                                12
            2100                13  
            2250                14  
            2400                15  
            2550                16  
            2700                17  
            2850                18  
            3000                19              19
         */
        public static void Sample()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var interval = Observable.Interval(TimeSpan.FromMilliseconds(150));
            using (interval.Sample(TimeSpan.FromSeconds(1))
                   .Subscribe(Console.WriteLine))
                Console.ReadLine();
        }

        public static void Timeout1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Interval(TimeSpan.FromMilliseconds(100)).Take(10)
            .Concat(Observable.Interval(TimeSpan.FromSeconds(2)));
            var timeout = source.Timeout(TimeSpan.FromSeconds(1));
            timeout.Subscribe(
            Console.WriteLine,
            Console.WriteLine,
            () => Console.WriteLine("Completed"));
            Console.ReadLine();
        }

        public static void Timeout2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var dueDate = DateTimeOffset.UtcNow.AddSeconds(4);
            var source = Observable.Interval(TimeSpan.FromSeconds(1));
            var timeout = source.Timeout(dueDate);
            timeout.Subscribe(
            Console.WriteLine,
            Console.WriteLine,
            () => Console.WriteLine("Completed"));
            Console.ReadLine();
        }
    }
}
