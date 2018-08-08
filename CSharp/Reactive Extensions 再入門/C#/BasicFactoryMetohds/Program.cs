using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Linq;
using System.Threading;

namespace BasicFactoryMetohds
{
    class Program
    {
        static void Main(string[] args)
        {
            // 動作を確認したいメソッド呼び出しのコメントを外してください
            //ColdSamples.ReturnSample();
            //ColdSamples.RepeatSample();
            //ColdSamples.RangeSample();
            //ColdSamples.RepeatExSample();
            //ColdSamples.GenerateSample();
            //ColdSamples.DeferSample();
            //ColdSamples.CreateSample();
            //ColdSamples.ThrowSample();
            //ColdSamples.TimerSample();
            //ColdSamples.IntervalSample();
            //ColdSamples.GenerateSampleWithTimespan();
            //ColdSamples.UsingSample();

            //HotSamples.FromEventSample();
            //HotSamples.StartSample();
            HotSamples.ToAsyncSample();
            //HotSamples.FromAsyncSample();
        }

    }
}
