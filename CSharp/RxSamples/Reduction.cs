using System;
using System.Reactive;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Reflection;

namespace RxSamples
{
    public static class Reduction
    {
        public static void Where() {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var oddNumbers = Observable.Range(0, 10)
                .Where(i => i % 2 == 0)
                .Subscribe(
                Console.WriteLine,
                () => Console.WriteLine("Completed"));
        }

        public static void Distinct() {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new Subject<int>();
            var distinct = subject.Distinct();
            subject.Subscribe(
            i => Console.WriteLine("{0}", i),
            () => Console.WriteLine("subject.OnCompleted()"));
            distinct.Subscribe(
            i => Console.WriteLine("distinct.OnNext({0})", i),
            () => Console.WriteLine("distinct.OnCompleted()"));
            subject.OnNext(1);
            subject.OnNext(2);
            subject.OnNext(3);
            subject.OnNext(1);
            subject.OnNext(1);
            subject.OnNext(4);
            subject.OnCompleted();
        }

        public static void DistinctUntilChanged()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new Subject<int>();
            var distinct = subject.DistinctUntilChanged();
            subject.Subscribe(
            i => Console.WriteLine("{0}", i),
            () => Console.WriteLine("subject.OnCompleted()"));
            distinct.Subscribe(
            i => Console.WriteLine("distinct.OnNext({0})", i),
            () => Console.WriteLine("distinct.OnCompleted()"));
            subject.OnNext(1);
            subject.OnNext(2);
            subject.OnNext(3);
            subject.OnNext(1);
            subject.OnNext(1);
            subject.OnNext(4);
            subject.OnCompleted();
        }

        public static void IgnoreElements()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new Subject<int>();
            //Could use subject.Where(_=>false);
            var noElements = subject.IgnoreElements();
            subject.Subscribe(
            i => Console.WriteLine("subject.OnNext({0})", i),
            () => Console.WriteLine("subject.OnCompleted()"));
            noElements.Subscribe(
            i => Console.WriteLine("noElements.OnNext({0})", i),
            () => Console.WriteLine("noElements.OnCompleted()"));
            subject.OnNext(1);
            subject.OnNext(2);
            subject.OnNext(3);
            subject.OnCompleted();
        }

        public static void SkipAndTake() 
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Observable.Range(0, 10)
                .Skip(3)
                .Subscribe(Console.WriteLine, () => Console.WriteLine("Completed"));
            Observable.Range(0, 10)
                .Take(3)
                .Subscribe(Console.WriteLine, () => Console.WriteLine("Completed"));
            Observable.Interval(TimeSpan.FromMilliseconds(100))
                .Take(3)
                .Subscribe(Console.WriteLine, () => Console.WriteLine("Completed"));
       }

        public static void SkipWhile()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new Subject<int>();
            subject
            .SkipWhile(i => i < 4)
            .Subscribe(Console.WriteLine, () => Console.WriteLine("Completed"));
            subject.OnNext(1);
            subject.OnNext(2);
            subject.OnNext(3);
            subject.OnNext(4);
            subject.OnNext(3);
            subject.OnNext(2);
            subject.OnNext(1);
            subject.OnNext(0);
            subject.OnCompleted();
        }

        public static void TakeWhile()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new Subject<int>();
            subject
            .TakeWhile(i => i < 4)
            .Subscribe(Console.WriteLine, () => Console.WriteLine("Completed"));
            subject.OnNext(1);
            subject.OnNext(2);
            subject.OnNext(3);
            subject.OnNext(4);
            subject.OnNext(3);
            subject.OnNext(2);
            subject.OnNext(1);
            subject.OnNext(0);
            subject.OnCompleted();
        }

        public static void SkipLast()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new Subject<int>();
            subject
            .SkipLast(2)
            .Subscribe(Console.WriteLine, () => Console.WriteLine("Completed"));
            Console.WriteLine("Pushing 1");
            subject.OnNext(1);
            Console.WriteLine("Pushing 2");
            subject.OnNext(2);
            Console.WriteLine("Pushing 3");
            subject.OnNext(3);
            Console.WriteLine("Pushing 4");
            subject.OnNext(4);
            subject.OnCompleted();
        }

        public static void TakeLast()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new Subject<int>();
            subject
            .TakeLast(2)
            .Subscribe(Console.WriteLine, () => Console.WriteLine("Completed"));
            Console.WriteLine("Pushing 1");
            subject.OnNext(1);
            Console.WriteLine("Pushing 2");
            subject.OnNext(2);
            Console.WriteLine("Pushing 3");
            subject.OnNext(3);
            Console.WriteLine("Pushing 4");
            subject.OnNext(4);
            subject.OnCompleted();
        }

        public static void SkipUntil()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new Subject<int>();
            var otherSubject = new Subject<Unit>();
            subject
            .SkipUntil(otherSubject)
            .Subscribe(Console.WriteLine, () => Console.WriteLine("Completed"));
            subject.OnNext(1);
            subject.OnNext(2);
            subject.OnNext(3);
            otherSubject.OnNext(Unit.Default);
            subject.OnNext(4);
            subject.OnNext(5);
            subject.OnNext(6);
            subject.OnNext(7);
            subject.OnNext(8);
            subject.OnCompleted();
        }

        public static void TakeUntil()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new Subject<int>();
            var otherSubject = new Subject<Unit>();
            subject
            .TakeUntil(otherSubject)
            .Subscribe(Console.WriteLine, () => Console.WriteLine("Completed"));
            subject.OnNext(1);
            subject.OnNext(2);
            subject.OnNext(3);
            otherSubject.OnNext(Unit.Default);
            subject.OnNext(4);
            subject.OnNext(5);
            subject.OnNext(6);
            subject.OnNext(7);
            subject.OnNext(8);
            subject.OnCompleted();
        }
    }
}
