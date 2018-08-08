using System;
using System.Diagnostics;

namespace RxSamples
{
    public static class SampleExtentions
    {
        public static void Dump<T>(this IObservable<T> source, string name)
        {
            source.Subscribe(
            i => Console.WriteLine("{0}-->{1}", name, i),
            ex => Console.WriteLine("{0} failed-->{1}", name, ex.Message),
            () => Console.WriteLine("{0} completed", name));
        }
    }
    public class Coord
    {
        public int X { get; set; }
        public int Y { get; set; }
        public override string ToString()
        {
            return string.Format("{0},{1}", X, Y);
        }
    }
    public class TimeIt : IDisposable
    {
        private readonly string _name;
        private readonly Stopwatch _watch;
        public TimeIt(string name)
        {
            _name = name;
            _watch = Stopwatch.StartNew();
        }
        public void Dispose()
        {
            _watch.Stop();
            Console.WriteLine("{0} took {1}", _name, _watch.Elapsed);
        }
    }
    //Creates a scope for a console foreground color. When disposed, will return to 
    //  the previous Console.ForegroundColor
    public class ConsoleColor : IDisposable
    {
        private readonly System.ConsoleColor _previousColor;
        public ConsoleColor(System.ConsoleColor color)
        {
            _previousColor = Console.ForegroundColor;
            Console.ForegroundColor = color;
        }
        public void Dispose()
        {
            Console.ForegroundColor = _previousColor;
        }
    }
}
