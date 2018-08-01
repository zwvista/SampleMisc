using Microsoft.Reactive.Testing;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive;
using System.Reactive.Concurrency;
using System.Reactive.Linq;
using System.Reflection;

namespace RxSamples
{
    public class Testing
    {
        public static void Test()
        {
            Test1();
            AdvanceTo();
            AdvanceBy();
            Start1();
            Start2();
            Stop();
            Collisions();
            Interval();
            Timeout();
            Interval2();
            Interval3();
            CreateColdObservable();
            CreateHotObservable();
        }

        private static void Test1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = new TestScheduler();
            var wasExecuted = false;
            scheduler.Schedule(() => wasExecuted = true);
            Console.WriteLine(wasExecuted); // False
            scheduler.AdvanceBy(1); //execute 1 tick of queued actions
            Console.WriteLine(wasExecuted); // True
        }

        private static void AdvanceTo()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = new TestScheduler();
            scheduler.Schedule(() => Console.WriteLine("A")); //Schedule immediately
            scheduler.Schedule(TimeSpan.FromTicks(10), () => Console.WriteLine("B"));
            scheduler.Schedule(TimeSpan.FromTicks(20), () => Console.WriteLine("C"));
            Console.WriteLine("scheduler.AdvanceTo(1);");
            scheduler.AdvanceTo(1);
            Console.WriteLine("scheduler.AdvanceTo(10);");
            scheduler.AdvanceTo(10);
            Console.WriteLine("scheduler.AdvanceTo(15);");
            scheduler.AdvanceTo(15);
            Console.WriteLine("scheduler.AdvanceTo(20);");
            scheduler.AdvanceTo(20);
        }

        private static void AdvanceBy()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = new TestScheduler();
            scheduler.Schedule(() => Console.WriteLine("A")); //Schedule immediately
            scheduler.Schedule(TimeSpan.FromTicks(10), () => Console.WriteLine("B"));
            scheduler.Schedule(TimeSpan.FromTicks(20), () => Console.WriteLine("C"));
            Console.WriteLine("scheduler.AdvanceBy(1);");
            scheduler.AdvanceBy(1);
            Console.WriteLine("scheduler.AdvanceBy(9);");
            scheduler.AdvanceBy(9);
            Console.WriteLine("scheduler.AdvanceBy(5);");
            scheduler.AdvanceBy(5);
            Console.WriteLine("scheduler.AdvanceBy(5);");
            scheduler.AdvanceBy(5);
        }

        private static void Start1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = new TestScheduler();
            scheduler.Schedule(() => Console.WriteLine("A")); //Schedule immediately
            scheduler.Schedule(TimeSpan.FromTicks(10), () => Console.WriteLine("B"));
            scheduler.Schedule(TimeSpan.FromTicks(20), () => Console.WriteLine("C"));
            Console.WriteLine("scheduler.Start();");
            scheduler.Start();
            Console.WriteLine("scheduler.Clock:{0}", scheduler.Clock);
        }

        private static void Start2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = new TestScheduler();
            scheduler.Schedule(() => Console.WriteLine("A"));
            scheduler.Schedule(TimeSpan.FromTicks(10), () => Console.WriteLine("B"));
            scheduler.Schedule(TimeSpan.FromTicks(20), () => Console.WriteLine("C"));
            Console.WriteLine("scheduler.Start();");
            scheduler.Start();
            Console.WriteLine("scheduler.Clock:{0}", scheduler.Clock);
            scheduler.Schedule(() => Console.WriteLine("D"));
            scheduler.Start();
            Console.WriteLine("scheduler.Clock:{0}", scheduler.Clock);
        }

        private static void Stop()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = new TestScheduler();
            scheduler.Schedule(() => Console.WriteLine("A"));
            scheduler.Schedule(TimeSpan.FromTicks(10), () => Console.WriteLine("B"));
            scheduler.Schedule(TimeSpan.FromTicks(15), scheduler.Stop);
            scheduler.Schedule(TimeSpan.FromTicks(20), () => Console.WriteLine("C"));
            Console.WriteLine("scheduler.Start();");
            scheduler.Start();
            Console.WriteLine("scheduler.Clock:{0}", scheduler.Clock);
        }

        private static void Collisions()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = new TestScheduler();
            scheduler.Schedule(TimeSpan.FromTicks(10), () => Console.WriteLine("A"));
            scheduler.Schedule(TimeSpan.FromTicks(10), () => Console.WriteLine("B"));
            scheduler.Schedule(TimeSpan.FromTicks(10), () => Console.WriteLine("C"));
            Console.WriteLine("scheduler.Start();");
            scheduler.Start();
            Console.WriteLine("scheduler.Clock:{0}", scheduler.Clock);
        }

        private static void Interval()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var expectedValues = new long[] { 0, 1, 2, 3, 4 };
            var actualValues = new List<long>();
            var scheduler = new TestScheduler();
            var interval = Observable
            .Interval(TimeSpan.FromSeconds(1), scheduler)
            .Take(5);
            interval.Subscribe(actualValues.Add);
            scheduler.Start();
            Console.WriteLine(Enumerable.SequenceEqual(expectedValues, actualValues));
            //Executes in less than 0.01s "on my machine"
        }

        private static void Timeout()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = new TestScheduler();
            var never = Observable.Never<int>();
            var exceptionThrown = false;
            never.Timeout(TimeSpan.FromMinutes(1), scheduler)
            .Subscribe(
            i => Console.WriteLine("This will never run."),
            ex => exceptionThrown = true);
            scheduler.Start();
            Console.WriteLine(exceptionThrown);
        }

        private static void Interval2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = new TestScheduler();
            var source = Observable.Interval(TimeSpan.FromSeconds(1), scheduler)
            .Take(4);
            var testObserver = scheduler.Start(
            () => source,
            0,
            0,
            TimeSpan.FromSeconds(5).Ticks);
            Console.WriteLine("Time is {0} ticks", scheduler.Clock);
            Console.WriteLine("Received {0} notifications", testObserver.Messages.Count);
            foreach (Recorded<Notification<long>> message in testObserver.Messages)
            {
                Console.WriteLine("{0} @ {1}", message.Value, message.Time);
            }
        }

        private static void Interval3()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = new TestScheduler();
            var source = Observable.Interval(TimeSpan.FromSeconds(1), scheduler)
            .Take(4);
            var testObserver = scheduler.Start(
            () => Observable.Interval(TimeSpan.FromSeconds(1), scheduler).Take(4),
            0,
            TimeSpan.FromSeconds(2).Ticks,
            TimeSpan.FromSeconds(5).Ticks);
            Console.WriteLine("Time is {0} ticks", scheduler.Clock);
            Console.WriteLine("Received {0} notifications", testObserver.Messages.Count);
            foreach (Recorded<Notification<long>> message in testObserver.Messages)
            {
                Console.WriteLine("{0} @ {1}", message.Value, message.Time);
            }
        }

        private static void CreateColdObservable()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = new TestScheduler();
            var source = scheduler.CreateColdObservable(
            new Recorded<Notification<long>>(10000000, Notification.CreateOnNext(0L)),
            new Recorded<Notification<long>>(20000000, Notification.CreateOnNext(1L)),
            new Recorded<Notification<long>>(30000000, Notification.CreateOnNext(2L)),
            new Recorded<Notification<long>>(40000000, Notification.CreateOnNext(3L)),
            new Recorded<Notification<long>>(40000000, Notification.CreateOnCompleted<long>())
            );
            var testObserver = scheduler.Start(
            () => source,
            0,
            0,
            TimeSpan.FromSeconds(5).Ticks);
            Console.WriteLine("Time is {0} ticks", scheduler.Clock);
            Console.WriteLine("Received {0} notifications", testObserver.Messages.Count);
            foreach (Recorded<Notification<long>> message in testObserver.Messages)
            {
                Console.WriteLine("  {0} @ {1}", message.Value, message.Time);
            }
        }

        private static void CreateHotObservable()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = new TestScheduler();
            var source = scheduler.CreateHotObservable(
            new Recorded<Notification<long>>(10000000, Notification.CreateOnNext(0L)),
            new Recorded<Notification<long>>(20000000, Notification.CreateOnNext(1L)),
            new Recorded<Notification<long>>(30000000, Notification.CreateOnNext(2L)),
            new Recorded<Notification<long>>(40000000, Notification.CreateOnNext(3L)),
            new Recorded<Notification<long>>(40000000, Notification.CreateOnCompleted<long>())
            );
            var testObserver = scheduler.Start(
            () => source,
            0,
            TimeSpan.FromSeconds(1).Ticks,
            TimeSpan.FromSeconds(5).Ticks);
            Console.WriteLine("Time is {0} ticks", scheduler.Clock);
            Console.WriteLine("Received {0} notifications", testObserver.Messages.Count);
            foreach (Recorded<Notification<long>> message in testObserver.Messages)
            {
                Console.WriteLine("  {0} @ {1}", message.Value, message.Time);
            }
        }

    }
}
