﻿using System;
using System.Concurrency;
using System.Diagnostics;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace RxSamples.Testing
{
    [TestClass]
    public class TestingSheduling
    {
        [TestMethod]
        public void Testing_with_real_scheduler()
        {
            Console.WriteLine("This test will take 5 seconds to run.");
            var timer = Stopwatch.StartNew();
            var interval = Observable
              .Interval(TimeSpan.FromSeconds(1))
              .Take(5);

            bool isComplete = false;
            interval
                .Timeout(TimeSpan.FromSeconds(3))
                .Subscribe(Console.WriteLine, () => isComplete = true);

            while (!isComplete)
            {

            }
            Console.WriteLine("Test completed in {0}.", timer.Elapsed);
        }

        [TestMethod]
        public void Testing_with_real_scheduler2()
        {
            Console.WriteLine("This test will take 5 seconds to run.");
            var timer = Stopwatch.StartNew();
            var interval = Observable
                .Interval(TimeSpan.FromSeconds(1))
                .Take(5);

            interval
                .Timeout(TimeSpan.FromSeconds(3))
                .Run(Console.WriteLine);

            Console.WriteLine("Test completed in {0}.", timer.Elapsed);
        }

        [TestMethod]
        public void Scheduling_with_the_TestScheduler()
        {
            var scheduler = new TestScheduler();
            var wasExecuted = false;
            scheduler.Schedule(() => wasExecuted = true);   //no offset or duetime specified so it should be marshalled for immediate.
            Assert.IsFalse(wasExecuted);
            scheduler.RunTo(1);         //executing one tick of queued actions is effectively the same as executing the first action on the scheduler.
            Assert.IsTrue(wasExecuted);
        }

        [TestMethod]
        public void Scheduling_at_duplicate_points_in_time_with_the_TestScheduler()
        {
            var scheduler = new TestScheduler();
            long dueTime = 4L;
            scheduler.Schedule(() => Console.WriteLine("1"), dueTime);
            scheduler.Schedule(() => Console.WriteLine("2"), dueTime);
            scheduler.Schedule(() => Console.WriteLine("3"), dueTime + 1);
            scheduler.Schedule(() => Console.WriteLine("4"), dueTime + 1);
            Console.WriteLine("RunTo(dueTime)");
            scheduler.RunTo(dueTime);
            Console.WriteLine("Run()");
            scheduler.Run();
            /* Output:
            RunTo(dueTime)
            1
            2
            Run()
            3
            4
            */
        }

        [TestMethod]
        public void Testing_with_test_scheduler()
        {
            Console.WriteLine("This test should take less than a second to run.");
            var timer = Stopwatch.StartNew();
            var scheduler = new TestScheduler();
            var interval = Observable
                .Interval(TimeSpan.FromSeconds(1), scheduler)
                .Take(5);

            bool isComplete = false;
            interval.Subscribe(Console.WriteLine, () => isComplete = true);

            scheduler.Run();

            Assert.IsTrue(isComplete);
            Console.WriteLine("Test completed in {0}.", timer.Elapsed);
        }

        [TestMethod]
        public void Testing_timeouts_badly()
        {
            var stream = Observable.Never<int>();
            var exceptionThrown = false;
            stream.Timeout(TimeSpan.FromSeconds(5))
                .Run(
                    i => Console.WriteLine("This will never run."),
                    ex => exceptionThrown = true);
            Assert.IsTrue(exceptionThrown);
        }
        [TestMethod]
        public void Testing_timeouts_with_the_TestScheduler()
        {
            var scheduler = new TestScheduler();
            var stream = Observable.Never<int>();

            var exceptionThrown = false;
            //stream.Timeout(TimeSpan.FromMinutes(1))//If no scheduler is passed then Scheduler.ThreadPool is used as the default
            stream.Timeout(TimeSpan.FromMinutes(1), scheduler)
                .Subscribe(
                    i => Console.WriteLine("This will never run."),
                    ex => exceptionThrown = true);
            scheduler.Run();
            Assert.IsTrue(exceptionThrown);
        }
    }
}
