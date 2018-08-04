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

            // OnError1();
            OnError2();
            Unsubscribe1();
            Dispose1();
            Dispose2();
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

        private static void OnError1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var values = new Subject<int>();
            try
            {
                values.Subscribe(value => Console.WriteLine("1st subscription received {0}", value));
            }
            catch (Exception)
            {
                Console.WriteLine("Won't catch anything here!");
            }
            values.OnNext(0);
            //Exception will be thrown here causing the app to fail.
            values.OnError(new Exception("Dummy exception"));
        }

        private static void OnError2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var values = new Subject<int>();
            values.Subscribe(
            value => Console.WriteLine("1st subscription received {0}", value),
            ex => Console.WriteLine("Caught an exception : {0}", ex));
            values.OnNext(0);
            values.OnError(new Exception("Dummy exception"));
        }

        private static void Unsubscribe1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var values = new Subject<int>();
            var firstSubscription = values.Subscribe(value =>
            Console.WriteLine("1st subscription received {0}", value));
            var secondSubscription = values.Subscribe(value =>
            Console.WriteLine("2nd subscription received {0}", value));
            values.OnNext(0);
            values.OnNext(1);
            values.OnNext(2);
            values.OnNext(3);
            firstSubscription.Dispose();
            Console.WriteLine("Disposed of 1st subscription");
            values.OnNext(4);
            values.OnNext(5);
        }

        private static void Dispose1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Action<string> DoSomeWork = s =>
            {
                Thread.Sleep(TimeSpan.FromSeconds(.5));
                Console.WriteLine(s);
            };
            using (new TimeIt("Outer scope"))
            {
                using (new TimeIt("Inner scope A"))
                {
                    DoSomeWork("A");
                }
                using (new TimeIt("Inner scope B"))
                {
                    DoSomeWork("B");
                }
            }
        }


        private static void Dispose2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Console.WriteLine("Normal color");
            using (new ConsoleColor(System.ConsoleColor.Red))
            {
                Console.WriteLine("Now I am Red");
                using (new ConsoleColor(System.ConsoleColor.Green))
                {
                    Console.WriteLine("Now I am Green");
                }
                Console.WriteLine("and back to Red");
            }
        }

    }
}
