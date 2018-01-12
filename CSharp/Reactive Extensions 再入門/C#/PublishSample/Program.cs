using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Linq;
using System.Threading;
using System.Reactive.Subjects;

namespace PublishSample
{
    class Program
    {
        static void Main(string[] args)
        {
            //PublishSample();
            //PublishIntArgSample();
            //PublishLastSample();
            //ReplaySample();
            MulticastSample();
        }

        static void PublishSample()
        {
            Console.WriteLine("## PublishSample");
            {
                // coldなObservableの作成
                var source = Observable.Range(1, 3);

                // 2回購読
                Console.WriteLine("# Subscribe1");
                source.Subscribe(
                    i => Console.WriteLine("Subscribe1#OnNext: {0}", i),
                    () => Console.WriteLine("Subscribe1#OnCompleted"));
                Console.WriteLine("# Subscribe2");
                source.Subscribe(
                    i => Console.WriteLine("Subscribe2#OnNext: {0}", i),
                    () => Console.WriteLine("Subscribe2#OnCompleted"));

                Console.WriteLine("--");

                // PublishでHotなObservableに
                Console.WriteLine("# Publish");
                var connectableSource = source.Publish();

                // 2回購読
                Console.WriteLine("# Subscribe1");
                connectableSource.Subscribe(
                    i => Console.WriteLine("Subscribe1#OnNext: {0}", i),
                    () => Console.WriteLine("Subscribe1#OnCompleted"));
                Console.WriteLine("# Subscribe2");
                connectableSource.Subscribe(
                    i => Console.WriteLine("Subscribe2#OnNext: {0}", i),
                    () => Console.WriteLine("Subscribe2#OnCompleted"));

                // Connectで購読しているObserverに値を流す
                Console.WriteLine("# Connect");
                connectableSource.Connect();

                // Connect後に購読したときの動作確認
                Console.WriteLine("# Subscribe3");
                connectableSource.Subscribe(
                    i => Console.WriteLine("Subscribe3#OnNext: {0}", i),
                    () => Console.WriteLine("Subscribe3#OnCompleted"));
            }
            Console.WriteLine("-------------------------------------");
            {
                // 500ms間隔で0からカウントアップしていく値を発行するIObservableを作成
                Console.WriteLine("# Create source(ConnectableObservable)");
                var source = Observable
                    .Interval(TimeSpan.FromMilliseconds(500))
                    .Publish();

                // Connectで値の放流開始
                Console.WriteLine("# Connect");
                using (source.Connect())
                {
                    // 2秒間何もせずに待つ
                    Console.WriteLine("Sleep 2sec...");
                    Thread.Sleep(2000);

                    // 2秒間購読
                    Console.WriteLine("# Subscribe1");
                    using (
                        source.Subscribe(
                            i => Console.WriteLine("Subscribe1#OnNext: {0}", i),
                            () => Console.WriteLine("Subscribe1#OnCompleted")))
                    {
                        Console.WriteLine("Sleep 2sec...");
                        Thread.Sleep(2000);
                    }
                    Console.WriteLine("# UnSubscribe1");
                }
                // Disposeが呼ばれるのでConnectが解除される
                Console.WriteLine("# DisConnect");

                // Connectが解除された状態で2秒間購読する
                Console.WriteLine("# Subscribe2");
                using (
                    source.Subscribe(
                        i => Console.WriteLine("Subscribe2#OnNext: {0}", i),
                        () => Console.WriteLine("Subscribe2#OnCompleted")))
                {
                    Console.WriteLine("Sleep 2sec...");
                    Thread.Sleep(2000);
                }
                Console.WriteLine("# UnSubscribe2");

                // 再接続
                Console.WriteLine("# Connect");
                using (source.Connect())
                {
                    // 再接続で発行される値を確認するため2秒間購読する
                    Console.WriteLine("# Subscribe3");
                    using (
                        source.Subscribe(
                            i => Console.WriteLine("Subscribe3#OnNext: {0}", i),
                            () => Console.WriteLine("Subscribe3#OnCompleted")))
                    {
                        Console.WriteLine("Sleep 2sec...");
                        Thread.Sleep(2000);
                    }
                    Console.WriteLine("# UnSubscribe3");
                }
                // Disposeが呼ばれるので接続解除
                Console.WriteLine("# DisConnect");

                // 接続解除状態で2秒間待機して様子を見る
                Console.WriteLine("Sleep 2sec...");
                Thread.Sleep(2000);
            }
            Console.WriteLine("-------------------------------------");
            {
                // 500ms間隔で0から値をカウントアップしていくIObservableを作成
                Console.WriteLine("# Create source(RefCount)");
                var source = Observable
                    .Interval(TimeSpan.FromMilliseconds(500))
                    // Hot
                    .Publish()
                    // 購読者がいる間Connect状態を保つ
                    .RefCount();

                // 2秒待機して様子身
                Console.WriteLine("Sleep 2sec...");
                Thread.Sleep(2000);

                // 購読開始
                Console.WriteLine("# Subscribe1");
                using (
                    source.Subscribe(
                        i => Console.WriteLine("Subscribe1#OnNext: {0}", i),
                        () => Console.WriteLine("Subscribe1#OnCompleted")))
                {
                    // 購読状態で2秒待機
                    Console.WriteLine("Sleep 2sec...");
                    Thread.Sleep(2000);

                    // 追加で1秒間購読
                    Console.WriteLine("# Subscribe2");
                    using (source.Subscribe(
                         i => Console.WriteLine("Subscribe2#OnNext: {0}", i),
                         () => Console.WriteLine("Subscribe2#OnCompleted")))
                    {
                        Console.WriteLine("Sleep 1sec...");
                        Thread.Sleep(1000);
                    }
                    // 1つ購読解除
                    Console.WriteLine("# UnSubscribe2");

                    // 1つ購読解除した状態で2秒待機
                    Console.WriteLine("Sleep 2sec...");
                    Thread.Sleep(2000);
                }
                // 購読解除(ここで購読者数が0になるのでConnectが解除される)
                Console.WriteLine("# UnSubscribe1");

                // 2秒待機
                Console.WriteLine("Sleep 2sec...");
                Thread.Sleep(2000);

                // 新たに購読開始(Connect状態になる)
                Console.WriteLine("# Subscribe3");
                using (source.Subscribe(
                     i => Console.WriteLine("Subscribe3#OnNext: {0}", i),
                     () => Console.WriteLine("Subscribe3#OnCompleted")))
                {
                    // 2秒待機
                    Console.WriteLine("Sleep 2sec...");
                    Thread.Sleep(2000);
                }
                // 購読解除
                Console.WriteLine("# UnSubscribe3");
            }
            Console.WriteLine("-------------------------------------");
        }

        static void PublishIntArgSample()
        {
            Console.WriteLine("## PublishIntArgSample");
            {
                // Publishのintの引数を受け取るオーバーロード
                var source = Observable
                    // 1～3の値を発行するColdなObservable
                    .Range(1, 3)
                    // initialValueに100を指定してPublishを呼ぶ
                    .Publish(100);

                // 購読
                Console.WriteLine("# Subscribe");
                source.Subscribe(
                    i => Console.WriteLine("OnNext: {0}", i),
                    () => Console.WriteLine("OnCompleted"));

                // 接続
                Console.WriteLine("# Connect");
                source.Connect();
            }
            Console.WriteLine("-------------------------------------");
        }

        static void PublishLastSample()
        {
            Console.WriteLine("## PublishLastSample");
            {
                // PublishLast
                var source = Observable
                    // 1～3の値を発行するColdなObservable
                    .Range(1, 3)
                    // PublishLastでHot化
                    .PublishLast();

                // 接続
                Console.WriteLine("# Connect");
                source.Connect();
                
                // 購読
                Console.WriteLine("# Subscribe1");
                source.Subscribe(
                    i => Console.WriteLine("Subscribe1#OnNext: {0}", i),
                    () => Console.WriteLine("Subscribe1#OnCompleted"));


                // 接続後にもう一度購読
                Console.WriteLine("# Subscribe2");
                source.Subscribe(
                    i => Console.WriteLine("Subscribe2#OnNext: {0}", i),
                    () => Console.WriteLine("Subscribe2#OnCompleted"));
            }
            Console.WriteLine("-------------------------------------");
        }

        static void ReplaySample()
        {
            Console.WriteLine("## ReplaySample");
            {
                // 500ms間隔で0から値をカウントアップしていく
                var source = Observable
                    .Interval(TimeSpan.FromMilliseconds(500))
                    // ReplayでHotに変換
                    .Replay();

                // 購読
                Console.WriteLine("# Subscribe1");
                using (
                    source.Subscribe(
                        i => Console.WriteLine("Subscribe1#OnNext: {0}", i)))
                {
                    // 接続
                    Console.WriteLine("# Connect");
                    source.Connect();

                    // 2秒待機
                    Console.WriteLine("Sleep 2sec...");
                    Thread.Sleep(2000);
                    
                    // 購読２
                    Console.WriteLine("# Subscribe2");
                    using (
                        source.Subscribe(
                            i => Console.WriteLine("Subscribe1#OnNext: {0}", i)))
                    {
                        // 1秒待機
                        Console.WriteLine("Sleep 1sec...");
                        Thread.Sleep(1000);
                    }
                    // 購読２解除
                    Console.WriteLine("# UnSubscribe2");

                    // 2秒待機
                    Console.WriteLine("Sleep 2sec...");
                    Thread.Sleep(2000);
                }
                // 購読解除
                Console.WriteLine("# UnSubscribe1");
            }
            Console.WriteLine("-------------------------------------");
        }

        static void MulticastSample()
        {
            Console.WriteLine("## MulticastSample");
            {
                Console.WriteLine("# Multicast(new Subject<int>())");
                var source = Observable
                    .Range(1, 3)
                    // Publishと一緒
                    .Multicast(new Subject<int>());

                // 購読
                Console.WriteLine("# Subscribe");
                source.Subscribe(
                    i => Console.WriteLine("OnNext: {0}", i),
                    () => Console.WriteLine("OnCompleted"));

                // 接続
                Console.WriteLine("# Connect");
                source.Connect();
            }
            Console.WriteLine("-------------------------------------");
            {
                Console.WriteLine("# Multicast(new BehaviorSubject<int>(100))");
                var source = Observable
                    .Range(1, 3)
                    // Publish(100)と一緒
                    .Multicast(new BehaviorSubject<int>(100));

                // 購読
                Console.WriteLine("# Subscribe");
                source.Subscribe(
                    i => Console.WriteLine("OnNext: {0}", i),
                    () => Console.WriteLine("OnCompleted"));

                // 接続
                Console.WriteLine("# Connect");
                source.Connect();
            }
            Console.WriteLine("-------------------------------------");
            {
                Console.WriteLine("# Multicast(new AsyncSubject<int>())");
                var source = Observable
                    .Range(1, 3)
                    // PublishLastと一緒
                    .Multicast(new AsyncSubject<int>());

                // 購読
                Console.WriteLine("# Subscribe");
                source.Subscribe(
                    i => Console.WriteLine("OnNext: {0}", i),
                    () => Console.WriteLine("OnCompleted"));

                // 接続
                Console.WriteLine("# Connect");
                source.Connect();
            }
            Console.WriteLine("-------------------------------------");
            {
                Console.WriteLine("# Multicast(new ReplaySubject<long>())");
                var source = Observable
                    .Interval(TimeSpan.FromMilliseconds(500))
                    // Replayと一緒
                    .Multicast(new ReplaySubject<long>());

                // 接続
                Console.WriteLine("# Connect");
                source.Connect();

                // 2秒待機
                Console.WriteLine("Sleep 2sec...");
                Thread.Sleep(2000);

                // 購読
                Console.WriteLine("# Subscribe");
                source.Subscribe(
                    i => Console.WriteLine("OnNext: {0}", i),
                    () => Console.WriteLine("OnCompleted"));

            }
            Console.WriteLine("-------------------------------------");
        }

    }
}
