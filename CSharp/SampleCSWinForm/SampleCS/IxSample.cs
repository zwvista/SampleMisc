using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using SamplePad;

namespace SampleCS
{
    [Title("IxSample")]
    [Prefix("Linq")]
    class IxSample : SampleHarness
    {
        [Category("IxSample")]
        [Title("Generate")]
        [Description("Linq1")]
        public void Linq1()
        {
            // 生成斐波那契数列中前10个数：[0,1,1,2,3,5,8,13,21,34]
            EnumerableEx.Generate(
                new { v1 = 0, v2 = 1 },                     // initial
                _ => true,                                  // condition
                a => new { v1 = a.v2, v2 = a.v1 + a.v2 },   // iterate
                a => a.v1                                   // resultSelector
            )
            .Take(10)
            .ForEach(Console.WriteLine);
        }
        [Category("IxSample")]
        [Title("MaxBy,MinBy,Distinct")]
        [Description("Linq2")]
        public void Linq2()
        {
            // 返回绝对值最大的元素序列[4,-4]
            new[] { 1, -2, 3, 4, -4 }.MaxBy(Math.Abs)
                .ForEach(Console.WriteLine);
            // 返回除以4余数最小的元素序列[1,5]
            new[] { 1, 2, 3, 5 }.MinBy(x => x % 4)
                .ForEach(Console.WriteLine);
            // 返回除以5余数首次出现时的元素序列[0,1,2,3,4]
            Enumerable.Range(0, 10).Distinct(x => x % 5)
                .ForEach(Console.WriteLine);
        }
        [Category("IxSample")]
        [Title("IgnoreElements,Return,IsEmpty")]
        [Description("Linq3")]
        public void Linq3()
        {
            // 序列[1,2,3]所对应的空序列为空,返回True
            Console.WriteLine(new[] { 1, 2, 3 }.IgnoreElements().IsEmpty());
            // 序列[3]不为空,返回False
            Console.WriteLine(EnumerableEx.Return(3).IsEmpty());
        }
        [Category("IxSample")]
        [Title("DistinctUntilChanged")]
        [Description("Linq4")]
        public void Linq4()
        {
            // 返回序列[1,2,3,4,3,4]
            new[] { 1, 1, 2, 2, 2, 3, 4, 3, 4 }.DistinctUntilChanged()
                .ForEach(Console.WriteLine);
        }
        [Category("IxSample")]
        [Title("Scan")]
        [Description("Linq5")]
        public void Linq5()
        {
            // 返回序列[3,6,10]
            new[] { 1, 2, 3, 4 }.Scan((acc, x) => acc + x)
                .ForEach(Console.WriteLine);
        }
    }
}
