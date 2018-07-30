using System;
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
        }

        public static void Select()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Range(0, 3);
            source.Select(i => i + 3).Dump("+3");
            source.Select(i => (char)(i + 64)).Dump("char");
            source.Select(i => new { Number = i, Character = (char)(i + 64) }).Dump("anon");
            var query = from i in source
                        select new { Number = i, Character = (char)(i + 64) };
            query.Dump("anon");
        }

        public static void Cast1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var objects = new Subject<object>();
            objects.Cast<int>().Dump("cast");
            objects.OnNext(1);
            objects.OnNext(2);
            objects.OnNext(3);
            objects.OnCompleted();
        }

        public static void Cast2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var objects = new Subject<object>();
            objects.Cast<int>().Dump("cast");
            objects.OnNext(1);
            objects.OnNext(2);
            objects.OnNext("3");//Fail
        }

        public static void OfType()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var objects = new Subject<object>();
            objects.OfType<int>().Dump("OfType");
            objects.OnNext(1);
            objects.OnNext(2);
            objects.OnNext("3");//Ignored
            objects.OnNext(4);
            objects.OnCompleted();
        }

        public static void Timestamp()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Observable.Interval(TimeSpan.FromSeconds(1))
            .Take(3)
            .Timestamp()
            .Dump("TimeStamp");
            Console.ReadKey();
        }

        public static void TimeInterval()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Observable.Interval(TimeSpan.FromSeconds(1))
            .Take(3)
            .TimeInterval()
            .Dump("TimeInterval");
            Console.ReadKey();
        }

        public static void SelectMany()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Observable.Range(1, 3)
            .SelectMany(i => Observable.Range(1, i))
            .Dump("SelectMany");
        }

        public static void Materialize1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Observable.Range(1, 3)
            .Materialize()
            .Dump("Materialize");
        }

        public static void Materialize2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = new Subject<int>();
            source.Materialize()
            .Dump("Materialize");
            source.OnNext(1);
            source.OnNext(2);
            source.OnNext(3);
            source.OnError(new Exception("Fail?"));
        }

        public static void Dematerialize()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Observable.Range(1, 3)
            .Materialize()
            .Dematerialize()
            .Dump("Dematerialize");
        }
    }
}
