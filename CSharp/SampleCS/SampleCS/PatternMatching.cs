using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SampleCS
{
    internal class PatternMatching
    {
        // constant pattern (number)
        // relational pattern
        // logical pattern
        public static void Match1()
        {
            var x = new Random().Next(10);
            var y = x switch
            {
                5 => "constant",
                < 7 and > 3 => "and",
                not (> 8 or < 2) => "or",
                _ => "else"
            };
            Console.WriteLine($"{x},{y}");
        }
        // constant pattern(string, null)
        public static void Match2(string? x)
        {
            var y = x switch
            {
                "a" => "a",
                null => "null",
                _ => "else"
            };
            Console.WriteLine($"{x},{y}");
        }
    }
}
