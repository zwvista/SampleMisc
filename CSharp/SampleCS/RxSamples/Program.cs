using RxSamples;

Console.WriteLine("a: Aggregation");
Console.WriteLine("c1: Coincidence");
Console.WriteLine("c2: Combining");
Console.WriteLine("c3: Creation");
Console.WriteLine("e: Error Handling");
Console.WriteLine("h: Hot & Cold");
Console.WriteLine("i: Inspection");
Console.WriteLine("l: Leaving");
Console.WriteLine("m: Misc");
Console.WriteLine("r1: Reduction");
Console.WriteLine("r2: REST API");
Console.WriteLine("s1: Scheduling");
Console.WriteLine("s2: Side Effects");
Console.WriteLine("s3: Subjects");
Console.WriteLine("t1: Testing");
Console.WriteLine("t2: TimeShifted");
Console.WriteLine("t3: Transformation");
switch (Console.ReadLine())
{
    case "a": Aggregation.Test(); break;
    case "c1": Coincidence.Test(); break;
    case "c2": Combining.Test(); break;
    case "c3": Creating.Test(); break;
    case "e": ErrorHandling.Test(); break;
    case "h": HotAndCold.Test(); break;
    case "i": Inspection.Test(); break;
    case "l": Leaving.Test(); break;
    case "m": Misc.Test(); break;
    case "r1": Reducing.Test(); break;
    case "r2": RestExample.Test(); break;
    case "s1": Scheduling.Test(); break;
    case "s2": SideEffects.Test(); break;
    case "s3": Subjects.Test(); break;
    case "t1": Testing.Test(); break;
    case "t2": TimeShifted.Test(); break;
    case "t3": Transformation.Test(); break;
    default: RestExample.Test(); break;
}
