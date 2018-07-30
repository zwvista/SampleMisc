using System;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Reflection;

namespace RxSamples
{
    public class Aggregation
    {
        public static void Test()
        {
            Count();
            MinMaxSumAvg();
            Scan();
            GroupBy1();
            GroupBy2();
        }

        public static void Count()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var numbers = Observable.Range(0, 3);
            numbers.Dump("numbers");
            numbers.Count().Dump("count");
        }

        public static void MinMaxSumAvg()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var numbers = new Subject<int>();
            numbers.Dump("numbers");
            numbers.Max().Dump("Max");
            numbers.Min().Dump("Min");
            numbers.Sum().Dump("Sum");
            numbers.Average().Dump("Average");
            numbers.OnNext(1);
            numbers.OnNext(2);
            numbers.OnNext(3);
            numbers.OnCompleted();
        }

        public static void Scan()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var numbers = new Subject<int>();
            var scan = numbers.Scan(0, (acc, current) => acc + current);
            numbers.Dump("numbers");
            scan.Dump("scan");
            numbers.OnNext(1);
            numbers.OnNext(2);
            numbers.OnNext(3);
            numbers.OnCompleted();
        }

        public static void GroupBy1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Interval(TimeSpan.FromSeconds(0.1)).Take(10);
            var group = source.GroupBy(i => i % 3);
            group.Subscribe(
            grp =>
            grp.Min().Subscribe(
            minValue =>
            Console.WriteLine("{0} min value = {1}", grp.Key, minValue)),
            () => Console.WriteLine("Completed"));
            Console.ReadKey();
        }

        public static void GroupBy2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Interval(TimeSpan.FromSeconds(0.1)).Take(10);
            var group = source.GroupBy(i => i % 3);
            group.SelectMany(
            grp =>
            grp.Max()
            .Select(value => new { grp.Key, value }))
            .Dump("group");
            Console.ReadKey();
        }
    }
}
