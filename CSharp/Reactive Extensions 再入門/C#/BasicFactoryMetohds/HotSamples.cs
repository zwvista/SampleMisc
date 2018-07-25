using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace BasicFactoryMetohds
{
    static class HotSamples
    {
        public static void StartSample()
        {
            Console.WriteLine("## StartSample");
            // バックグラウンドで処理を開始
            var source = Observable.Start(() =>
            {
                Console.WriteLine("background task start.");
                Thread.Sleep(2000);
                Console.WriteLine("background task end.");
                return 1;
            });
            // 購読
            Console.WriteLine("subscribe1");
            var subscription1 = source.Subscribe(
                i => Console.WriteLine("1##OnNext({0})", i),
                ex => Console.WriteLine("1##OnError({0})", ex.Message),
                () => Console.WriteLine("1##Completed()"));

            // 処理が確実に終わるように5秒待つ
            Console.WriteLine("sleep 5sec.");
            Thread.Sleep(5000);

            // Observableが発行する値の購読を停止
            Console.WriteLine("dispose method call.");
            subscription1.Dispose();

            // 購読
            Console.WriteLine("subscribe2");
            var subscription2 = source.Subscribe(
                i => Console.WriteLine("2##OnNext({0})", i),
                ex => Console.WriteLine("2##OnError({0})", ex.Message),
                () => Console.WriteLine("2##Completed()"));
            subscription2.Dispose();
            Console.WriteLine("-----------------------------------");
        }

        public static void ToAsyncSample()
        {
            Console.WriteLine("## ToAsyncSample");
            // 戻り値はFunc<IObservable<T>>
            var source = Observable.ToAsync(() =>
            {
                Console.WriteLine("background task start.");
                Thread.Sleep(2000);
                Console.WriteLine("background task end.");
                return 1;
            });

            // ToAsyncはデリゲートを返すのでInvoke() or ()をしないと処理が開始されない
            Console.WriteLine("source() call.");
            var invokedSource = source.Invoke();
            var subscription1 = invokedSource.Subscribe(
                i => Console.WriteLine("1##OnNext({0})", i),
                ex => Console.WriteLine("1##OnError({0})", ex.Message),
                () => Console.WriteLine("1##Completed()"));
            // 処理が確実に終わるように5秒待つ
            Console.WriteLine("sleep 5sec.");
            Thread.Sleep(5000);

            // Observableが発行する値の購読を停止
            Console.WriteLine("dispose method call.");
            subscription1.Dispose();

            // 購読
            Console.WriteLine("subscribe2");
            var subscription2 = invokedSource.Subscribe(
                i => Console.WriteLine("2##OnNext({0})", i),
                ex => Console.WriteLine("2##OnError({0})", ex.Message),
                () => Console.WriteLine("2##Completed()"));
            subscription2.Dispose();
            Console.WriteLine("-----------------------------------");
        }

        public static void FromEventSample()
        {
            Console.WriteLine("## FromEventSample");
            // イベントを発行するクラス
            var eventSource = new EventSource();
            var source = Observable.FromEvent<EventHandler, EventArgs>(
                h => (s, e) => h(e),
                // 普通は h => eventSource.Raised += h だけでいい
                h => 
                {
                    Console.WriteLine("add handler");
                    eventSource.Raised += h;
                },
                // 普通は h => eventSource.Raised -= h だけでいい
                h => 
                {
                    Console.WriteLine("remove handler");
                    eventSource.Raised -= h;
                });

            // 2回購読
            var subscription1 = source.Subscribe(
                i => Console.WriteLine("1##OnNext({0})", i),
                ex => Console.WriteLine("1##OnError({0})", ex.Message),
                () => Console.WriteLine("1##Completed()"));
            var subscription2 = source.Subscribe(
                i => Console.WriteLine("2##OnNext({0})", i),
                ex => Console.WriteLine("2##OnError({0})", ex.Message),
                () => Console.WriteLine("2##Completed()"));

            // 2回呼び出してみる
            // 合計4回のOnNextが呼ばれるはず
            eventSource.OnRaised();
            eventSource.OnRaised();

            // Observableが発行する値の購読を停止
            Console.WriteLine("dispose method call.");
            subscription1.Dispose();
            subscription2.Dispose();
            Console.WriteLine("-----------------------------------");
        }

        // イベント発行クラス
        class EventSource
        {
            public event EventHandler Raised;
            public void OnRaised()
            {
                var h = this.Raised;
                if (h != null)
                {
                    h(this, EventArgs.Empty);
                }
            }
        }

        public static void FromAsyncPatternSample()
        {
            Console.WriteLine("## FromEventSample");
            // 重たい処理
            Func<int, int, int> asyncProcess = (x, y) =>
            {
                Console.WriteLine("process start.");
                Thread.Sleep(2000);
                Console.WriteLine("process end.");
                return x + y;
            };

            // 因みに非同期呼び出しは普通に書くとこんな感じ
            // asyncProcess.BeginInvoke(
            //     10, 2,
            //     ar =>
            //     {
            //         var ret = asyncProcess.EndInvoke(ar);
            //         // do something
            //     },
            //     null);
            //var asyncPattern = Observable.FromAsyncPattern<int, int, int>(
            //    asyncProcess.BeginInvoke,
            //    asyncProcess.EndInvoke);

            //var source = asyncPattern(10, 2);
            var source = Observable.FromAsync(() => Task.FromResult(asyncProcess(10, 2)));

            // 処理中に購読開始
            Console.WriteLine("subscribe2");
            var subscription1 = source.Subscribe(
                i => Console.WriteLine("1##OnNext({0})", i),
                ex => Console.WriteLine("1##OnError({0})", ex.Message),
                () => Console.WriteLine("1##Completed()"));

            // 確実に処理が終わるように5秒待つ
            Console.WriteLine("sleep 5sec");
            Thread.Sleep(5000);
            // Observableが発行する値の購読を停止
            Console.WriteLine("dispose method call.");
            subscription1.Dispose();

            // 処理が完了したあとに購読
            Console.WriteLine("subscribe2");
            var subscription2 = source.Subscribe(
                i => Console.WriteLine("2##OnNext({0})", i),
                ex => Console.WriteLine("2##OnError({0})", ex.Message),
                () => Console.WriteLine("2##Completed()"));

            // 購読解除
            Console.WriteLine("dispose method call.");
            subscription2.Dispose();
            Console.WriteLine("-----------------------------------");
        }
    }

}
