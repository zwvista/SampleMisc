using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Linq;

namespace TimespanAndIntervalSample
{
    class Program
    {
        static void Main(string[] args)
        {
            TimestampSample();
            TimeIntervalSample();
        }

        static void TimestampSample()
        {
            Console.WriteLine("#TimestampSample");
            var subscriber = Observable
                // 1秒間隔で0～4の値を発行
                .Generate(0, i => i < 5, i => i + 1, i => i, _ => TimeSpan.FromSeconds(1))
                // タイムスタンプをつける(Timestamped<T>型になる)
                .Timestamp()
                // 購読
                .Subscribe(
                    i => Console.WriteLine("Timestamp: {0}, Value: {1}", i.Timestamp, i.Value));

            // 終了待ち
            Console.WriteLine("Please enter key.");
            Console.ReadLine();
            subscriber.Dispose();
            Console.WriteLine("---------------------------------");
        }

        static void TimeIntervalSample()
        {
            Console.WriteLine("#TimeIntervalSample");
            var subscriber = Observable
                // 1秒間隔で0～4の値を発行
                .Generate(0, i => i < 5, i => i + 1, i => i, _ => TimeSpan.FromSeconds(1))
                // 値の発行間隔をつける(TimeInterval<T>型になる)
                .TimeInterval()
                // 購読
                .Subscribe(
                    i => Console.WriteLine("Interval: {0}, Value: {1}", i.Interval, i.Value));

            // 終了待ち
            Console.WriteLine("Please enter key.");
            Console.ReadLine();
            subscriber.Dispose();
            Console.WriteLine("---------------------------------");
        }
    }
}
