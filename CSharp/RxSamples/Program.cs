using System;

namespace RxSamples
{
    class MainClass
    {
        public static void Main(string[] args)
        {
            Console.WriteLine("0: Aggregation");
            Console.WriteLine("1: Coincidence");
            Console.WriteLine("2: Combining");
            Console.WriteLine("3: Creation");
            Console.WriteLine("4: Inspection");
            Console.WriteLine("5: Leaving");
            Console.WriteLine("6: Reduction");
            Console.WriteLine("7: Scheduling");
            Console.WriteLine("8: TimeShifted");
            Console.WriteLine("9: Transformation");
            switch (Console.ReadLine())
            {
                case "0": TestAggregation(); break;
                case "1": TestCoincidence(); break;
                case "2": TestCombining(); break;
                case "3": TestCreation(); break;
                case "4": TestInspection(); break;
                case "5": TestLeaving(); break;
                case "6": TestReduction(); break;
                case "7": TestTimeShifted(); break;
                case "8": TestTransformation(); break;
                case "9": TestScheduling(); break;
                default: TestCoincidence(); break;
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

        public static void TestCoincidence()
        {
            Coincidence.Window1();
            Coincidence.Window2();
        }
    }
}
