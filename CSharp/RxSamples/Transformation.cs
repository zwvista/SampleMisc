using System;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Reflection;

namespace RxSamples
{
    public static class Transformation
    {
        public static void Select()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Range(0, 5);
            source.Select(i => i + 3)
            .Dump("+3");
            Observable.Range(1, 5)
            .Select(i => (char)(i + 64))
            .Dump("char");
            Observable.Range(1, 5)
            .Select(
            i => new { Number = i, Character = (char)(i + 64) })
            .Dump("anon");
            var query = from i in Observable.Range(1, 5)
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
            Console.ReadLine();
        }

        public static void TimeInterval()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Observable.Interval(TimeSpan.FromSeconds(1))
            .Take(3)
            .TimeInterval()
            .Dump("TimeInterval");
            Console.ReadLine();
        }

        public static void SelectMany()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Observable.Range(1, 3)
            .SelectMany(i => Observable.Range(1, i))
            .Dump("SelectMany");
        }
    }
}
