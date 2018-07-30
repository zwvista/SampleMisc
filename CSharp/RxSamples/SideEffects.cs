using System;
using System.Reactive;
using System.Reactive.Linq;
using System.Reactive.Subjects;

namespace RxSamples
{
    public static class SideEffects
    {
        public static void Test()
        {
            Do1();
            Do2();
        }

        private static void Log(object onNextValue)
        {
            Console.WriteLine("Logging OnNext({0}) @ {1}", onNextValue, DateTime.Now);
        }
        private static void Log(Exception onErrorValue)
        {
            Console.WriteLine("Logging OnError({0}) @ {1}", onErrorValue, DateTime.Now);
        }
        private static void Log()
        {
            Console.WriteLine("Logging OnCompleted()@ {0}", DateTime.Now);
        }

        public static void Do1()
        {
            var source = Observable
            .Interval(TimeSpan.FromSeconds(1))
            .Take(3);
            var result = source.Do(
            i => Log(i),
            ex => Log(ex),
            () => Log());
            result.Subscribe(
            Console.WriteLine,
            () => Console.WriteLine("completed"));
            Console.ReadLine();
        }

        private static IObservable<long> GetNumbers()
        {
            return Observable.Interval(TimeSpan.FromMilliseconds(250))
            .Do(i => Console.WriteLine("pushing {0} from GetNumbers", i));
        }

        public static void Do2()
        {
            var source = GetNumbers();
            var result = source.Where(i => i % 3 == 0)
            .Take(3)
            .Select(i => (char)(i + 65));
            result.Subscribe(
            Console.WriteLine,
            () => Console.WriteLine("completed"));
            Console.ReadLine();
        }

    }
}
