using System;

namespace RxSamples
{
    class MainClass
    {
        public static void Main(string[] args)
        {
            Console.WriteLine("a: Aggregation");
            Console.WriteLine("c1: Coincidence");
            Console.WriteLine("c2: Combining");
            Console.WriteLine("c3: Creation");
            Console.WriteLine("e: Error Handling");
            Console.WriteLine("i: Inspection");
            Console.WriteLine("l: Leaving");
            Console.WriteLine("r: Reduction");
            Console.WriteLine("s1: Scheduling");
            Console.WriteLine("s2: Side Effects");
            Console.WriteLine("t1: TimeShifted");
            Console.WriteLine("t2: Transformation");
            switch (Console.ReadLine())
            {
                case "a": Aggregation.Test(); break;
                case "c1": Coincidence.Test(); break;
                case "c2": Combining.Test(); break;
                case "c3": Creation.Test(); break;
                case "e": ErrorHandling.Test(); break;
                case "i": Inspection.Test(); break;
                case "l": Leaving.Test(); break;
                case "r": Reduction.Test(); break;
                case "s1": Scheduling.Test(); break;
                case "s2": SideEffects.Test(); break;
                case "t1": TimeShifted.Test(); break;
                case "t2": Transformation.Test(); break;
                default: SideEffects.Test(); break;
            }
        }

        public static void TestAggregation()
        {
            Aggregation.Count();
            Aggregation.MinMaxSumAvg();
            Aggregation.Scan();
            Aggregation.GroupBy1();
            Aggregation.GroupBy2();
        }

        public static void TestCoincidence()
        {
            Coincidence.Window1();
            Coincidence.Window2();
        }

        public static void TestCombining()
        {
            Combining.Concat1();
            Combining.Concat2();
            Combining.Repeat();
            Combining.StartWith();
            Combining.Amb1();
            Combining.Amb2();
            Combining.Amb3();
            Combining.Merge1();
            Combining.Merge2();
            Combining.Zip1();
            Combining.Zip2();
            Combining.Zip3();
            Combining.AndThenWhen();
        }

        public static void TestCreation()
        {
            Creation.ReturnEmptyNeverThrow();
            Creation.Create();
            Creation.Range();
            Creation.Interval();
            Creation.Timer();
        }

        public static void TestErrorHandling()
        {
            ErrorHandling.Catch1();
            ErrorHandling.Catch2();
            ErrorHandling.Catch3();
            ErrorHandling.Finally1();
            ErrorHandling.Finally2();
            // ErrorHandling.Finally3();
            ErrorHandling.Using();
        }

        public static void TestInspection()
        {
            Inspection.Any1();
            Inspection.Any2();
            Inspection.All();
            Inspection.Contains();
            Inspection.DefaultIfEmpty1();
            Inspection.DefaultIfEmpty2();
            Inspection.ElementAt();
            Inspection.SequenceEqual();
        }

        public static void TestLeaving()
        {
            Leaving.ToEnumerable();
            Leaving.ToArray();
        }

        public static void TestReduction() {
            Reduction.Where();
            Reduction.Distinct();
            Reduction.DistinctUntilChanged();
            Reduction.IgnoreElements();
            Reduction.SkipAndTake();
            Reduction.SkipWhile();
            Reduction.TakeWhile();
            Reduction.SkipLast();
            Reduction.TakeLast();
            Reduction.SkipUntil();
            Reduction.TakeUntil();
        }

        public static void TestScheduling()
        {
            Scheduling.Subscribe();
            Scheduling.SubscribeOn();
        }

        public static void TestTimeShifted()
        {
            TimeShifted.Buffer1();
            TimeShifted.Buffer2();
            TimeShifted.Buffer3();
            TimeShifted.Buffer4();
            TimeShifted.Delay();
            TimeShifted.Sample();
            TimeShifted.Timeout1();
            TimeShifted.Timeout2();
        }

        public static void TestTransformation()
        {
            Transformation.Select();
            Transformation.Cast1();
            Transformation.Cast2();
            Transformation.OfType();
            Transformation.Timestamp();
            Transformation.TimeInterval();
            Transformation.SelectMany();
        }
    }
}
