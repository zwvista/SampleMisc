using System;
using System.Linq;
using System.Reactive.Concurrency;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Reflection;

namespace RxSamples
{
    public static class Transformation
    {

        public static void Test()
        {
            Select();
            Cast1();
            Cast2();
            OfType();
            Timestamp();
            TimeInterval();
            SelectMany();
            Materialize1();
            Materialize2();
            Dematerialize();
            ManySelect1();
            ManySelect2();
        }

        private static void Select()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var source = Observable.Range(0, 3);
            source.Select(i => i + 3).Dump("+3");
            source.Select(i => (char)(i + 64)).Dump("char");
            source.Select(i => new { Number = i, Character = (char)(i + 64) }).Dump("anon");
            var query = from i in source
                        select new { Number = i, Character = (char)(i + 64) };
            query.Dump("anon");
        }

        private static void Cast1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var objects = new Subject<object>();
            objects.Cast<int>().Dump("cast");
            objects.OnNext(1);
            objects.OnNext(2);
            objects.OnNext(3);
            objects.OnCompleted();
        }

        private static void Cast2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var objects = new Subject<object>();
            objects.Cast<int>().Dump("cast");
            objects.OnNext(1);
            objects.OnNext(2);
            objects.OnNext("3");//Fail
        }

        private static void OfType()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var objects = new Subject<object>();
            objects.OfType<int>().Dump("OfType");
            objects.OnNext(1);
            objects.OnNext(2);
            objects.OnNext("3");//Ignored
            objects.OnNext(4);
            objects.OnCompleted();
        }

        private static void Timestamp()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            Observable.Interval(TimeSpan.FromSeconds(1))
            .Take(3)
            .Timestamp()
            .Dump("TimeStamp");
            Console.ReadKey();
        }

        private static void TimeInterval()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            Observable.Interval(TimeSpan.FromSeconds(1))
            .Take(3)
            .TimeInterval()
            .Dump("TimeInterval");
            Console.ReadKey();
        }

        private static void SelectMany()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            Observable.Range(1, 3)
            .SelectMany(i => Observable.Range(1, i))
            .Dump("SelectMany");
        }

        private static void Materialize1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            Observable.Range(1, 3)
            .Materialize()
            .Dump("Materialize");
        }

        private static void Materialize2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var source = new Subject<int>();
            source.Materialize()
            .Dump("Materialize");
            source.OnNext(1);
            source.OnNext(2);
            source.OnNext(3);
            source.OnError(new Exception("Fail?"));
        }

        private static void Dematerialize()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            Observable.Range(1, 3)
            .Materialize()
            .Dematerialize()
            .Dump("Dematerialize");
        }

        // https://social.msdn.microsoft.com/Forums/en-US/e70fe8b6-6d9d-486a-a8d0-c1bc66551ded/what-does-the-new-manyselect-operator-do?forum=rx
        private static void ManySelect1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var xs = Observable.Range(1, 3).Do(x => Console.WriteLine("Generated: {0}", x));

            var projection = new[] { "A", "B", "C" };
            int counter = 0;

            var manySelect = xs.ManySelect(ys =>
            {
                var x = ++counter;

                return ys.Select(y => new { x, y = projection[y - 1] });
            },
                Scheduler.CurrentThread);

            using (manySelect.Concat().Subscribe(
                x => Console.WriteLine("Observed: {0}", x),
                () => Console.WriteLine("Completed")))
            {
                Console.ReadKey();
            }
        }

        private static void ManySelect2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            Observable.Range(1, 10).ManySelect(xs => xs.Sum(), Scheduler.CurrentThread).Concat().Dump("ManySelect21");
            Observable.Range(1, 10).SelectMany(x => Observable.Range(x, 10 - x + 1).Sum()).Dump("SelectMany21");
            Observable.Range(1, 10).ManySelect(xs => xs.Take(3).ToList(), Scheduler.CurrentThread)
                .Concat().Select(xs => string.Join(",", xs.Select(x => x.ToString()))).Dump("ManySelect22");
        }
    }
}
