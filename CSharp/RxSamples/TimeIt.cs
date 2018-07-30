using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RxSamples
{
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
}
