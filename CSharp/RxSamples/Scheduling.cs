using System;
using System.Collections.Generic;
using System.Reactive.Concurrency;
using System.Reactive.Disposables;
using System.Reactive.Linq;
using System.Reflection;
using System.Threading;

namespace RxSamples
{
    public class Scheduling
    {
        public static void Test()
        {
            Subscribe();
            SubscribeOn();
            ObserveOn();
            SubscribeOnObserveOn();

            ScheduleState1();
            ScheduleState2();
            ScheduleState3();
            ScheduleState4();
            ScheduleState5();
            ScheduleState6();

            ScheduleTime();
            ScheduleCancel1();
        }

        public static void Subscribe()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Console.WriteLine("Starting on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
            var source = Observable.Create<int>(
            o =>
            {
                Console.WriteLine("Invoked on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
                o.OnNext(1);
                o.OnNext(2);
                o.OnNext(3);
                o.OnCompleted();
                Console.WriteLine("Finished on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
                return Disposable.Empty;
            });
            source
            .Subscribe(
            o => Console.WriteLine("Received {1} on threadId:{0}", Thread.CurrentThread.ManagedThreadId, o),
            () => Console.WriteLine("OnCompleted on threadId:{0}", Thread.CurrentThread.ManagedThreadId));
            Console.WriteLine("Subscribed on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
        }

        public static void SubscribeOn()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Console.WriteLine("Starting on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
            var source = Observable.Create<int>(
            o =>
            {
                Console.WriteLine("Invoked on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
                o.OnNext(1);
                o.OnNext(2);
                o.OnNext(3);
                o.OnCompleted();
                Console.WriteLine("Finished on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
                return Disposable.Empty;
            });
            source
            .SubscribeOn(Scheduler.Default)
            .Subscribe(
            o => Console.WriteLine("Received {1} on threadId:{0}", Thread.CurrentThread.ManagedThreadId, o),
            () => Console.WriteLine("OnCompleted on threadId:{0}", Thread.CurrentThread.ManagedThreadId));
            Console.WriteLine("Subscribed on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
            Console.ReadKey();
        }

        public static void ObserveOn()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Console.WriteLine("Starting on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
            var source = Observable.Create<int>(
            o =>
            {
                Console.WriteLine("Invoked on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
                o.OnNext(1);
                o.OnNext(2);
                o.OnNext(3);
                o.OnCompleted();
                Console.WriteLine("Finished on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
                return Disposable.Empty;
            });
            source
            .ObserveOn(Scheduler.Default)
            .Subscribe(
            o => Console.WriteLine("Received {1} on threadId:{0}", Thread.CurrentThread.ManagedThreadId, o),
            () => Console.WriteLine("OnCompleted on threadId:{0}", Thread.CurrentThread.ManagedThreadId));
            Console.WriteLine("Subscribed on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
            Console.ReadKey();
        }

        public static void SubscribeOnObserveOn()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Console.WriteLine("Starting on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
            var source = Observable.Create<int>(
            o =>
            {
                Console.WriteLine("Invoked on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
                o.OnNext(1);
                o.OnNext(2);
                o.OnNext(3);
                o.OnCompleted();
                Console.WriteLine("Finished on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
                return Disposable.Empty;
            });
            source
            .SubscribeOn(Scheduler.Default)
            .ObserveOn(Scheduler.Default)
            .Subscribe(
            o => Console.WriteLine("Received {1} on threadId:{0}", Thread.CurrentThread.ManagedThreadId, o),
            () => Console.WriteLine("OnCompleted on threadId:{0}", Thread.CurrentThread.ManagedThreadId));
            Console.WriteLine("Subscribed on threadId:{0}", Thread.CurrentThread.ManagedThreadId);
            Console.ReadKey();
        }

        public static void ScheduleState1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = NewThreadScheduler.Default;
            var myName = "Lee";
            scheduler.Schedule(
            () => Console.WriteLine("myName = {0}", myName));
            myName = "John";
            Console.ReadKey();
        }

        public static void ScheduleState2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = ImmediateScheduler.Instance;
            var myName = "Lee";
            scheduler.Schedule(
            () => Console.WriteLine("myName = {0}", myName));
            myName = "John";
            Console.ReadKey();
        }

        public static void ScheduleState3()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = NewThreadScheduler.Default;
            var myName = "Lee";
            scheduler.Schedule(myName,
            (_, state) =>
            {
                Console.WriteLine("myName = {0}", state);
                return Disposable.Empty;
            });
            myName = "John";
            Console.ReadKey();
        }

        public static void ScheduleState4()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = ImmediateScheduler.Instance;
            var myName = "Lee";
            scheduler.Schedule(myName,
            (_, state) =>
            {
                Console.WriteLine("myName = {0}", state);
                return Disposable.Empty;
            });
            myName = "John";
            Console.ReadKey();
        }

        public static void ScheduleState5()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = NewThreadScheduler.Default;
            var list = new List<int>();
            scheduler.Schedule(list,
            (innerScheduler, state) =>
            {
                Console.WriteLine(state.Count);
                return Disposable.Empty;
            });
            list.Add(1);
            Console.ReadKey();
        }

        public static void ScheduleState6()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = ImmediateScheduler.Instance;
            var list = new List<int>();
            scheduler.Schedule(list,
            (innerScheduler, state) =>
            {
                Console.WriteLine(state.Count);
                return Disposable.Empty;
            });
            list.Add(1);
            Console.ReadKey();
        }

        public static void ScheduleTime()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = ImmediateScheduler.Instance;
            var delay = TimeSpan.FromSeconds(1);
            Console.WriteLine("Before schedule at {0:o}", DateTime.Now);
            scheduler.Schedule(delay,
            () => Console.WriteLine("Inside schedule at {0:o}", DateTime.Now));
            Console.WriteLine("After schedule at  {0:o}", DateTime.Now);
            Console.ReadKey();
        }

        public static void ScheduleCancel1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = ImmediateScheduler.Instance;
            var delay = TimeSpan.FromSeconds(1);
            Console.WriteLine("Before schedule at {0:o}", DateTime.Now);
            var token = scheduler.Schedule(delay,
            () => Console.WriteLine("Inside schedule at {0:o}", DateTime.Now));
            Console.WriteLine("After schedule at  {0:o}", DateTime.Now);
            token.Dispose();
            Console.ReadKey();
        }
    }
}
