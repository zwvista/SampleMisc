using System;
using System.Reactive;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Reflection;

namespace RxSamples
{
    public static class SideEffects
    {
        public static void Test()
        {
            SideEffects1();
            SideEffects2();
            Do1();
            Do2();
            AsObservable();
        }

        private static void SideEffects1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Range(0, 3);
            var result = source.Select(
            (idx, value) => new
            {
                Index = idx,
                Letter = (char)(value + 65)
            });
            result.Subscribe(
                x => Console.WriteLine("Received {0} at index {1}", x.Letter, x.Index),
                () => Console.WriteLine("completed"));
            result.Subscribe(
                x => Console.WriteLine("Also received {0} at index {1}", x.Letter, x.Index),
                () => Console.WriteLine("2nd completed"));
        }

        private static void SideEffects2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Range(0, 3);
            var result = source.Scan(
                new
                {
                    Index = -1,
                    Letter = new char()
                },
                (acc, value) => new
                {
                    Index = acc.Index + 1,
                    Letter = (char)(value + 65)
                });
            result.Subscribe(
                x => Console.WriteLine("Received {0} at index {1}", x.Letter, x.Index),
                () => Console.WriteLine("completed"));
            result.Subscribe(
                x => Console.WriteLine("Also received {0} at index {1}", x.Letter, x.Index),
                () => Console.WriteLine("2nd completed"));
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

        private static void Do1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
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
            Console.ReadKey();
        }

        private static IObservable<long> GetNumbers()
        {
            return Observable.Interval(TimeSpan.FromMilliseconds(250))
            .Do(i => Console.WriteLine("pushing {0} from GetNumbers", i));
        }

        private static void Do2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = GetNumbers();
            var result = source.Where(i => i % 3 == 0)
                .Take(3)
                .Select(i => (char)(i + 65));
            result.Subscribe(
                Console.WriteLine,
                () => Console.WriteLine("completed"));
            Console.ReadKey();
        }

        public class LetterRepo
        {
            private readonly ReplaySubject<string> _letters;
            public LetterRepo()
            {
                _letters = new ReplaySubject<string>();
                _letters.OnNext("A");
                _letters.OnNext("B");
                _letters.OnNext("C");
            }
            public IObservable<string> GetLetters()
            {
                return _letters.AsObservable();
            }
        }

        private static void AsObservable()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var repo = new LetterRepo();
            var good = repo.GetLetters();
            var evil = repo.GetLetters();
            good.Subscribe(
            Console.WriteLine);
            //Be naughty
            var asSubject = evil as ISubject<string>;
            if (asSubject != null)
            {
                //So naughty, 1 is not a letter!
                asSubject.OnNext("1");
            }
            else
            {
                Console.WriteLine("could not sabotage");
            }
        }
    }
}
