using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Linq;

namespace SensorSample
{
    class Program
    {
        static void Main(string[] args)
        {
            // 10個のセンサーを作成
            var sensors = Enumerable.Range(1, 10).Select(i => new Sensor("Sensor#" + i)).ToArray();
            // 10個のセンサーの値発行イベントをマージ
            var subscription = Observable.Merge(
                sensors.Select(sensor => Observable.FromEvent<EventHandler<SensorEventArgs>, SensorEventArgs>(
                    h => (s, e) => h(e),
                    h => sensor.Publish += h,
                    h => sensor.Publish -= h)))
                // 10秒ためて
                .Buffer(TimeSpan.FromSeconds(10))
                // その中から最大のものを探して
                .Select(values => values.Aggregate((x, y) => x.Value > y.Value ? x : y))
                // 表示する
                .Subscribe(e => Console.WriteLine("{0}: {1}", e.Name, e.Value));

            // センサースタート
            foreach (var sensor in sensors)
            {
                sensor.Start();
            }

            Console.WriteLine("Sensor started.");
            Console.ReadLine();
            // 最後にセンサーのPublishイベントの購読解除
            subscription.Dispose();
        }
    }
}
