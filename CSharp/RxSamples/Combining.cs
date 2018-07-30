using System;
using System.Collections.Generic;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Reflection;
using System.Threading;

namespace RxSamples
{
    public static class Combining
    {
        public static void Test()
        {
            Concat1();
            Concat2();
            Repeat();
            StartWith();
            Amb1();
            Amb2();
            Amb3();
            Merge1();
            Merge2();
            Zip1();
            Zip2();
            Zip3();
            AndThenWhen();
        }

        /*
            s1 --0--1--2-|
            s2           -5--6--7--8--|
            r  --0--1--2--5--6--7--8--|
        */
        public static void Concat1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            //Generate values 0,1,2 
            var s1 = Observable.Range(0, 3);
            //Generate values 5,6,7,8,9 
            var s2 = Observable.Range(5, 5);
            s1.Concat(s2)
            .Subscribe(Console.WriteLine);
        }

        private static IEnumerable<IObservable<long>> GetSequences()
        {
            Console.WriteLine("GetSequences() called");
            Console.WriteLine("Yield 1st sequence");
            yield return Observable.Create<long>(o =>
            {
                Console.WriteLine("1st subscribed to");
                return Observable.Timer(TimeSpan.FromMilliseconds(500))
                .Select(i => 1L)
                .Subscribe(o);
            });
            Console.WriteLine("Yield 2nd sequence");
            yield return Observable.Create<long>(o =>
            {
                Console.WriteLine("2nd subscribed to");
                return Observable.Timer(TimeSpan.FromMilliseconds(300))
                .Select(i => 2L)
                .Subscribe(o);
            });
            Thread.Sleep(1000);     //Force a delay
            Console.WriteLine("Yield 3rd sequence");
            yield return Observable.Create<long>(o =>
            {
                Console.WriteLine("3rd subscribed to");
                return Observable.Timer(TimeSpan.FromMilliseconds(100))
                .Select(i => 3L)
                .Subscribe(o);
            });
            Console.WriteLine("GetSequences() complete");
        }

        /*
            s1-----1|
            s2      ---2|
            s3          -3|
            rs-----1---2-3|
        */
        public static void Concat2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            GetSequences().Concat().Dump("Concat");
            Console.ReadKey();
        }

        public static void Repeat()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var source = Observable.Range(0, 3);
            var result = source.Repeat(3);
            result.Subscribe(
            Console.WriteLine,
            () => Console.WriteLine("Completed"));
        }

        public static void StartWith()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            //Generate values 0,1,2 
            var source = Observable.Range(0, 3);
            var result = source.StartWith(-3, -2, -1);
            result.Subscribe(
            Console.WriteLine,
            () => Console.WriteLine("Completed"));
        }

        /*
            s1 -1--1--|
            s2 --2--2--|
            s3 ---3--3--|
            r  -1--1--|
        */
        public static void Amb1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var s1 = new Subject<int>();
            var s2 = new Subject<int>();
            var s3 = new Subject<int>();
            var result = Observable.Amb(s1, s2, s3);
            result.Subscribe(
            Console.WriteLine,
            () => Console.WriteLine("Completed"));
            s1.OnNext(1);
            s2.OnNext(2);
            s3.OnNext(3);
            s1.OnNext(1);
            s2.OnNext(2);
            s3.OnNext(3);
            s1.OnCompleted();
            s2.OnCompleted();
            s3.OnCompleted();
        }

        /*
            s1 ---1--|
            s2 -2--2--|
            s3 --3--3--|
            r  -2--2--|
        */
        public static void Amb2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var s1 = new Subject<int>();
            var s2 = new Subject<int>();
            var s3 = new Subject<int>();
            var result = Observable.Amb(s1, s2, s3);
            result.Subscribe(
            Console.WriteLine,
            () => Console.WriteLine("Completed"));
            // s1.OnNext(1);
            s2.OnNext(2);
            s3.OnNext(3);
            s1.OnNext(1);
            s2.OnNext(2);
            s3.OnNext(3);
            s1.OnCompleted();
            s2.OnCompleted();
            s3.OnCompleted();
        }

        /*
            s1-----1|
            s2---2|
            s3-3|
            rs-3|
        */
        public static void Amb3()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            GetSequences().Amb().Dump("Amb");
            Console.ReadKey();
        }

        /*
            s1 ----0----0----0| 
            s2 --0--0--0--0--0|
            sR --0-00--00-0--00|
        */
        public static void Merge1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            //Generate values 0,1,2 
            var s1 = Observable.Interval(TimeSpan.FromMilliseconds(250))
            .Take(3);
            //Generate values 100,101,102,103,104 
            var s2 = Observable.Interval(TimeSpan.FromMilliseconds(150))
            .Take(5)
            .Select(i => i + 100);
            s1.Merge(s2)
            .Subscribe(
            Console.WriteLine,
            () => Console.WriteLine("Completed"));
            Console.ReadKey();
        }

        /*
            s1-----1|
            s2---2|
            s3          -3|
            rs---2-1-----3|
        */
        public static void Merge2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            GetSequences().Merge().Dump("Merge");
            Console.ReadKey();
        }

        /*
            nums  ----0----1----2| 
            chars --a--b--c--d--e--f| 
            result----0----1----2|
                      a    b    c|
        */
        public static void Zip1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            //Generate values 0,1,2 
            var nums = Observable.Interval(TimeSpan.FromMilliseconds(250))
            .Take(3);
            //Generate values a,b,c,d,e,f 
            var chars = Observable.Interval(TimeSpan.FromMilliseconds(150))
            .Take(6)
            .Select(i => Char.ConvertFromUtf32((int)i + 97));
            //Zip values together
            nums.Zip(chars, (lhs, rhs) => new { Left = lhs, Right = rhs })
            .Dump("Zip");
            Console.ReadKey();
        }

        /*
            MM --1--2--3--4--5
            S1    --2--3--4--5
            Zip   --1--2--3--4
                    2  3  4  5
        */
        public static void Zip2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var mm = new Subject<Coord>();
            var s1 = mm.Skip(1);
            var delta = mm.Zip(s1,
            (prev, curr) => new Coord
            {
                X = curr.X - prev.X,
                Y = curr.Y - prev.Y
            });
            delta.Subscribe(
            Console.WriteLine,
            () => Console.WriteLine("Completed"));
            mm.OnNext(new Coord { X = 0, Y = 0 });
            mm.OnNext(new Coord { X = 1, Y = 0 }); //Move across 1
            mm.OnNext(new Coord { X = 3, Y = 2 }); //Diagonally up 2
            mm.OnNext(new Coord { X = 0, Y = 0 }); //Back to 0,0
            mm.OnCompleted();
        }

        public static void Zip3()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var one = Observable.Interval(TimeSpan.FromSeconds(1)).Take(5);
            var two = Observable.Interval(TimeSpan.FromMilliseconds(250)).Take(10);
            var three = Observable.Interval(TimeSpan.FromMilliseconds(150)).Take(14);
            //lhs represents 'Left Hand Side'
            //rhs represents 'Right Hand Side'
            var zippedSequence = one
            .Zip(two, (lhs, rhs) => new { One = lhs, Two = rhs })
            .Zip(three, (lhs, rhs) => new { One = lhs.One, Two = lhs.Two, Three = rhs });
            zippedSequence.Subscribe(
            Console.WriteLine,
            () => Console.WriteLine("Completed"));
            Console.ReadKey();
        }
        public static void AndThenWhen()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var one = Observable.Interval(TimeSpan.FromSeconds(1)).Take(5);
            var two = Observable.Interval(TimeSpan.FromMilliseconds(250)).Take(10);
            var three = Observable.Interval(TimeSpan.FromMilliseconds(150)).Take(14);
            var zippedSequence = Observable.When(
                one.And(two)
                .And(three)
                .Then((first, second, third) =>
                new {
                    One = first,
                    Two = second,
                    Three = third
                })
            );
            zippedSequence.Subscribe(
            Console.WriteLine,
            () => Console.WriteLine("Completed"));
            Console.ReadKey();
        }
    }
}
