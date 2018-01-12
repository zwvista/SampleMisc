using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Linq;
using System.Reactive.Disposables;

namespace TimeoutSample
{
    class Program
    {
        static void Main(string[] args)
        {
            TimeoutSample();
            TimeoutErrorTimeValueProvideSample();
        }

        static void TimeoutSample()
        {
            Console.WriteLine("#TimeoutSample");
            var subscriber = Observable
                // 0～4の値をi秒間隔で発行する
                .Generate(0, i => i < 5, i => i + 1, i => i, i => TimeSpan.FromSeconds(i))
                // 3500ms以上間隔があくとタイムアウト
                .Timeout(TimeSpan.FromMilliseconds(3500))
                // 購読
                .Subscribe(
                    i => Console.WriteLine("{0:HH:mm:ss} OnNext({1})", DateTime.Now, i),
                    ex => Console.WriteLine("{0:HH:mm:ss} OnError({1})", DateTime.Now, ex),
                    () => Console.WriteLine("{0:HH:mm:ss} OnCompleted()", DateTime.Now));
            // Enterを押すと購読終了
            Console.ReadLine();
            subscriber.Dispose();
            Console.WriteLine("------------------------------");
        }

        static void TimeoutErrorTimeValueProvideSample()
        {
            Console.WriteLine("#TimeoutErrorTimeValueProvideSample");
            var subscriber = Observable
                // 0～4の値をi秒間隔で発行する
                .Generate(0, i => i < 5, i => i + 1, i => i, i => TimeSpan.FromSeconds(i))
                // 3500ms以上間隔があくとタイムアウト
                .Timeout(
                    TimeSpan.FromMilliseconds(3500), 
                    // タイムアウトの時に流す値を指定
                    Observable.Create<int>(o =>
                    {
                        Console.WriteLine("{0:HH:mm:ss} Error Action", DateTime.Now);
                        // -1を流して完了
                        o.OnNext(-1);
                        o.OnCompleted();
                        return Disposable.Empty;
                    }))
                // 購読
                .Subscribe(
                    i => Console.WriteLine("{0:HH:mm:ss} OnNext({1})", DateTime.Now, i),
                    ex => Console.WriteLine("{0:HH:mm:ss} OnError({1})", DateTime.Now, ex),
                    () => Console.WriteLine("{0:HH:mm:ss} OnCompleted()", DateTime.Now));
            
            // Enterを押すと購読終了
            Console.ReadLine();
            subscriber.Dispose();
            Console.WriteLine("------------------------------");
        }
    }
}
