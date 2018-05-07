using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using SamplePad;

namespace SampleCS
{
    [Title("ZWSample")]
    [Prefix("Linq")]
    class ZWSample : SampleHarness
    {
        private void Print(string text, int offset)
        {
            text
                .Select((c, i) => new { Char = c, Index = i })
                .GroupBy(p => p.Index % offset, p => p.Char.ToString())
                .ForEach(g => Console.WriteLine(string.Join("|", g.Reverse())));
        }

        [Category("趣味编程")]
        [Title("静夜思")]
        [Description("静夜思")]
        [LinkedMethod("Print")]
        public void Linq1()
        {
            Print("床前明月光疑是地上霜举头望明月低头思故乡", 5);
        }
    }
}
