using System;

namespace RxSamples
{
    class MainClass
    {
        public static void Main(string[] args)
        {
            Console.WriteLine("0: Creation");
            Console.WriteLine("1: Reduction");
            Console.WriteLine("2: Inspection");
            Console.WriteLine("3: Aggregation");
            Console.WriteLine("4: Transformation");
            Console.WriteLine("5: Combining");
            Console.WriteLine("6: TimeShifted");
            Console.WriteLine("7: Leaving");
            Console.WriteLine("8: Scheduling");
            switch (Console.ReadLine())
            {
                case "0": TestCreation(); break;
                case "1": TestReduction(); break;
                case "2": TestInspection(); break;
                case "3": TestAggregation(); break;
                case "4": TestTransformation(); break;
                case "5": TestCombining(); break;
                case "6": TestTimeShifted(); break;
                case "7": TestLeaving(); break;
                case "8": TestScheduling(); break;
                default: TestScheduling(); break;
            }
        }

        public static void TestCreation()
        {
            Creation.ReturnEmptyNeverThrow();
            Creation.Create();
            Creation.Range();
            Creation.Interval();
            Creation.Timer();
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

        public static void TestInspection() {
            Inspection.Any1();
            Inspection.Any2();
            Inspection.All();
            Inspection.Contains();
            Inspection.DefaultIfEmpty1();
            Inspection.DefaultIfEmpty2();
            Inspection.ElementAt();
            Inspection.SequenceEqual();
        }

        public static void TestAggregation()
        {
            Aggregation.Count();
            Aggregation.MinMaxSumAvg();
            Aggregation.Scan();
            Aggregation.GroupBy1();
            Aggregation.GroupBy2();
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

        public static void TestLeaving()
        {
            Leaving.ToEnumerable();
            Leaving.ToArray();
        }

        public static void TestScheduling()
        {
            Scheduling.Subscribe();
            Scheduling.SubscribeOn();
        }
    }
}
