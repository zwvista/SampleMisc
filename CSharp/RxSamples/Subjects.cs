using System;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Reflection;
using System.Threading;

namespace RxSamples
{
    public class Subjects
    {
        public static void Test()
        {
            Subject1();
            Subject2();
            ReplaySubject1();
            ReplaySubjectBufferExample();
            ReplaySubjectWindowExample();
            BehaviorSubjectExample();
            BehaviorSubjectExample2();
            BehaviorSubjectExample3();
            BehaviorSubjectCompletedExample();
            AsyncSubject1();
            AsyncSubject2();
            SubjectInvalidUsageExample();
        }

        //Takes an IObservable<string> as its parameter. 
        //Subject<string> implements this interface.
        private static void WriteSequenceToConsole(IObservable<string> sequence)
        {
            //The next two lines are equivalent.
            //sequence.Subscribe(value=>Console.WriteLine(value));
            sequence.Subscribe(Console.WriteLine);
        }

        private static void Subject1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new Subject<string>();
            WriteSequenceToConsole(subject);
            subject.OnNext("a");
            subject.OnNext("b");
            subject.OnNext("c");
        }

        private static void Subject2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new Subject<string>();
            subject.OnNext("a");
            WriteSequenceToConsole(subject);
            subject.OnNext("b");
            subject.OnNext("c");
        }

        private static void ReplaySubject1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new ReplaySubject<string>();
            subject.OnNext("a");
            WriteSequenceToConsole(subject);
            subject.OnNext("b");
            subject.OnNext("c");
        }

        private static void ReplaySubjectBufferExample()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var bufferSize = 2;
            var subject = new ReplaySubject<string>(bufferSize);
            subject.OnNext("a");
            subject.OnNext("b");
            subject.OnNext("c");
            subject.Subscribe(Console.WriteLine);
            subject.OnNext("d");
        }

        private static void ReplaySubjectWindowExample()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var window = TimeSpan.FromMilliseconds(150);
            var subject = new ReplaySubject<string>(window);
            subject.OnNext("w");
            Thread.Sleep(TimeSpan.FromMilliseconds(100));
            subject.OnNext("x");
            Thread.Sleep(TimeSpan.FromMilliseconds(100));
            subject.OnNext("y");
            subject.Subscribe(Console.WriteLine);
            subject.OnNext("z");
        }

        private static void BehaviorSubjectExample()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            //Need to provide a default value.
            var subject = new BehaviorSubject<string>("a");
            subject.Subscribe(Console.WriteLine);
        }

        private static void BehaviorSubjectExample2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new BehaviorSubject<string>("a");
            subject.OnNext("b");
            subject.Subscribe(Console.WriteLine);
        }

        private static void BehaviorSubjectExample3()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new BehaviorSubject<string>("a");
            subject.OnNext("b");
            subject.Subscribe(Console.WriteLine);
            subject.OnNext("c");
            subject.OnNext("d");
        }

        private static void BehaviorSubjectCompletedExample()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new BehaviorSubject<string>("a");
            subject.OnNext("b");
            subject.OnNext("c");
            subject.OnCompleted();
            subject.Subscribe(Console.WriteLine);
        }

        private static void AsyncSubject1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new AsyncSubject<string>();
            subject.OnNext("a");
            WriteSequenceToConsole(subject);
            subject.OnNext("b");
            subject.OnNext("c");
        }

        private static void AsyncSubject2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new AsyncSubject<string>();
            subject.OnNext("a");
            WriteSequenceToConsole(subject);
            subject.OnNext("b");
            subject.OnNext("c");
            subject.OnCompleted();
        }

        private static void SubjectInvalidUsageExample()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var subject = new Subject<string>();
            subject.Subscribe(Console.WriteLine);
            subject.OnNext("a");
            subject.OnNext("b");
            subject.OnCompleted();
            subject.OnNext("c");
        }
    }
}
