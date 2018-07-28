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
                case "0": Aggregation.Test(); break;
                case "1": Coincidence.Test(); break;
                case "2": Combining.Test(); break;
                case "3": Creation.Test(); break;
                case "4": Inspection.Test(); break;
                case "5": Leaving.Test(); break;
                case "6": Reduction.Test(); break;
                case "7": Scheduling.Test(); break;
                case "8": TimeShifted.Test(); break;
                case "9": Transformation.Test(); break;
                default: Coincidence.Test(); break;
            }
        }
    }
}
