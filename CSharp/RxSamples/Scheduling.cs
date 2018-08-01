using System;
using System.Collections.Generic;
using System.Reactive.Concurrency;
using System.Reactive.Disposables;
using System.Reactive.Linq;
using System.Reflection;
using System.Threading;
using System.Threading.Tasks;

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
            ScheduleCancel2();

            // Recursion();

            CurrentThreadExample();
            ImmediateExample();
            NewThreadExample();
            ThreadPoolExample();
            TaskPoolExample();
        }

        private static void Subscribe()
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

        private static void SubscribeOn()
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

        private static void ObserveOn()
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

        private static void SubscribeOnObserveOn()
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

        private static void ScheduleState1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = NewThreadScheduler.Default;
            var myName = "Lee";
            scheduler.Schedule(
            () => Console.WriteLine("myName = {0}", myName));
            myName = "John";
            Console.ReadKey();
        }

        private static void ScheduleState2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = Scheduler.Immediate;
            var myName = "Lee";
            scheduler.Schedule(
            () => Console.WriteLine("myName = {0}", myName));
            myName = "John";
            Console.ReadKey();
        }

        private static void ScheduleState3()
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

        private static void ScheduleState4()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = Scheduler.Immediate;
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

        private static void ScheduleState5()
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

        private static void ScheduleState6()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = Scheduler.Immediate;
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

        private static void ScheduleTime()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = Scheduler.Immediate;
            var delay = TimeSpan.FromSeconds(1);
            Console.WriteLine("Before schedule at {0:o}", DateTime.Now);
            scheduler.Schedule(delay,
            () => Console.WriteLine("Inside schedule at {0:o}", DateTime.Now));
            Console.WriteLine("After schedule at  {0:o}", DateTime.Now);
            Console.ReadKey();
        }

        private static void ScheduleCancel1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = NewThreadScheduler.Default;
            var delay = TimeSpan.FromSeconds(1);
            Console.WriteLine("Before schedule at {0:o}", DateTime.Now);
            var token = scheduler.Schedule(delay,
            () => Console.WriteLine("Inside schedule at {0:o}", DateTime.Now));
            Console.WriteLine("After schedule at  {0:o}", DateTime.Now);
            token.Dispose();
            Console.ReadKey();
        }

        public static IDisposable Work(IScheduler scheduler, List<int> list)
        {
            var tokenSource = new CancellationTokenSource();
            var cancelToken = tokenSource.Token;
            var task = new Task(() =>
            {
                Console.WriteLine();
                for (int i = 0; i < 1000; i++)
                {
                    var sw = new SpinWait();
                    for (int j = 0; j < 3000; j++) sw.SpinOnce();
                    Console.Write(".");
                    list.Add(i);
                    if (cancelToken.IsCancellationRequested)
                    {
                        Console.WriteLine("Cancelation requested");
                        //cancelToken.ThrowIfCancellationRequested();
                        return;
                    }
                }
            }, cancelToken);
            task.Start();
            return Disposable.Create(tokenSource.Cancel);
        }

        private static void ScheduleCancel2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = NewThreadScheduler.Default;
            var list = new List<int>();
            Console.WriteLine("Enter to quit:");
            var token = scheduler.Schedule(list, Work);
            Console.ReadLine();
            Console.WriteLine("Cancelling...");
            token.Dispose();
            Console.WriteLine("Cancelled");
            Console.ReadLine();
        }

        private static void Recursion()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            var scheduler = NewThreadScheduler.Default;
            Action<Action> work = (Action self)
            =>
            {
                Console.WriteLine("Running");
                self();
            };
            Console.WriteLine("Enter to quit:");
            var token = scheduler.Schedule(work);
            Console.ReadLine();
            Console.WriteLine("Cancelling");
            token.Dispose();
            Console.WriteLine("Cancelled");
        }

        private static void ScheduleTasks(IScheduler scheduler)
        {
            Action leafAction = () => Console.WriteLine("----leafAction.");
            Action innerAction = () =>
            {
                Console.WriteLine("--innerAction start.");
                scheduler.Schedule(leafAction);
                Console.WriteLine("--innerAction end.");
            };
            Action outerAction = () =>
            {
                Console.WriteLine("outer start.");
                scheduler.Schedule(innerAction);
                Console.WriteLine("outer end.");
            };
            scheduler.Schedule(outerAction);
        }
        private static void CurrentThreadExample()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            ScheduleTasks(Scheduler.CurrentThread);
            /*Output: 
            outer start. 
            outer end. 
            --innerAction start. 
            --innerAction end. 
            ----leafAction. 
            */
        }
        private static void ImmediateExample()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            ScheduleTasks(Scheduler.Immediate);
            /*Output: 
            outer start. 
            --innerAction start. 
            ----leafAction. 
            --innerAction end. 
            outer end. 
            */
        }

        private static IDisposable OuterAction(IScheduler scheduler, string state)
        {
            Console.WriteLine("{0} start. ThreadId:{1}",
            state,
            Thread.CurrentThread.ManagedThreadId);
            scheduler.Schedule(state + ".inner", InnerAction);
            Console.WriteLine("{0} end. ThreadId:{1}",
            state,
            Thread.CurrentThread.ManagedThreadId);
            return Disposable.Empty;
        }
        private static IDisposable InnerAction(IScheduler scheduler, string state)
        {
            Console.WriteLine("{0} start. ThreadId:{1}",
            state,
            Thread.CurrentThread.ManagedThreadId);
            scheduler.Schedule(state + ".Leaf", LeafAction);
            Console.WriteLine("{0} end. ThreadId:{1}",
            state,
            Thread.CurrentThread.ManagedThreadId);
            return Disposable.Empty;
        }
        private static IDisposable LeafAction(IScheduler scheduler, string state)
        {
            Console.WriteLine("{0}. ThreadId:{1}",
            state,
            Thread.CurrentThread.ManagedThreadId);
            return Disposable.Empty;
        }

        private static void NewThreadExample()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Console.WriteLine("Starting on thread :{0}", Thread.CurrentThread.ManagedThreadId);
            NewThreadScheduler.Default.Schedule("A", OuterAction);
            NewThreadScheduler.Default.Schedule("B", OuterAction);
            Console.ReadKey();
        }

        private static void ThreadPoolExample()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Console.WriteLine("Starting on thread :{0}", Thread.CurrentThread.ManagedThreadId);
            ThreadPoolScheduler.Instance.Schedule("A", OuterAction);
            ThreadPoolScheduler.Instance.Schedule("B", OuterAction);
            Console.ReadKey();
        }

        private static void TaskPoolExample()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod().Name);
            Console.WriteLine("Starting on thread :{0}", Thread.CurrentThread.ManagedThreadId);
            TaskPoolScheduler.Default.Schedule("A", OuterAction);
            TaskPoolScheduler.Default.Schedule("B", OuterAction);
            Console.ReadKey();
        }

    }
}
