using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Linq;
using System.Threading;
using System.Reactive.Subjects;
using System.Reactive;

namespace BufferAndWindowSample
{
    class Program
    {
        static void Main(string[] args)
        {
            //BufferCountSample();
            //BufferTimeSample();
            //BufferObservableSample();
            //BufferCountAndTimeSample();

            WindowSample();
        }

        static void BufferCountSample()
        {
            Console.WriteLine("#BufferCountSample");
            {
                // 1～10の値を発行するIObservable<int>のシーケンス
                Observable.Range(1, 10)
                    // 3つずつの値に分ける
                    .Buffer(3)
                    .Subscribe(
                        l =>
                        {
                            // IList<int>の内容を出力
                            Console.WriteLine("-- Buffer start");
                            foreach (var i in l)
                            {
                                Console.WriteLine(i);
                            }
                        },
                    // 完了
                        () => Console.WriteLine("OnCompleted"));
            }
            Console.WriteLine("-------------------------------");
            {
                // 1～10の値を発行するIObservable<int>のシーケンス
                Observable.Range(1, 10)
                    // 3つずつの値に分けて、値は2つ飛ばし
                    .Buffer(3, 2)
                    .Subscribe(
                        l =>
                        {
                            // IList<int>の内容を出力
                            Console.WriteLine("-- Buffer start");
                            foreach (var i in l)
                            {
                                Console.WriteLine(i);
                            }
                        },
                        // 完了
                        () => Console.WriteLine("OnCompleted"));
                Console.WriteLine("-------------------------------");
                {
                    // 1～10の値を発行するIObservable<int>のシーケンス
                    Observable.Range(1, 10)
                        // 3つずつの値に分けて、値は5つ飛ばし
                        .Buffer(3, 5)
                        .Subscribe(
                            l =>
                            {
                                // IList<int>の内容を出力
                                Console.WriteLine("-- Buffer start");
                                foreach (var i in l)
                                {
                                    Console.WriteLine(i);
                                }
                            },
                            // 完了
                            () => Console.WriteLine("OnCompleted"));
                }
            }
            Console.WriteLine("-------------------------------");
        }

        static void BufferTimeSample()
        {
            Console.WriteLine("#BufferTimeSample");
            {
                var gate = new EventWaitHandle(false, EventResetMode.AutoReset);
                Observable
                    // 500msごとに値を発行する
                    .Interval(TimeSpan.FromMilliseconds(500))
                    // 3秒間値を溜める
                    .Buffer(TimeSpan.FromSeconds(3))
                    // 最初の3つを後続に流す
                    .Take(3)
                    .Subscribe(
                        l =>
                        {
                            // 値を表示
                            Console.WriteLine("--Buffer {0:HH:mm:ss}", DateTime.Now);
                            foreach (var i in l)
                            {
                                Console.WriteLine(i);
                            }
                        },
                        () =>
                        {
                            // 完了
                            Console.WriteLine("OnCompleted");
                            gate.Set();
                        });

                // OnCompleted待ち
                Console.WriteLine("WaitOne");
                gate.WaitOne();
                Console.WriteLine("WaitOne Completed");
            }
            Console.WriteLine("-------------------------------");
            {
                var gate = new EventWaitHandle(false, EventResetMode.AutoReset);
                Observable
                    // 500msごとに値を発行する
                    .Interval(TimeSpan.FromMilliseconds(500))
                    // 3秒間値を溜める。次の値をためるための待機時間は2秒
                    .Buffer(TimeSpan.FromSeconds(3), TimeSpan.FromSeconds(2))
                    // 最初の3つを後続に流す
                    .Take(3)
                    .Subscribe(
                        l =>
                        {
                            // 値を表示
                            Console.WriteLine("--Buffer {0:HH:mm:ss}", DateTime.Now);
                            foreach (var i in l)
                            {
                                Console.WriteLine(i);
                            }
                        },
                        () =>
                        {
                            // 完了
                            Console.WriteLine("OnCompleted");
                            gate.Set();
                        });

                // OnCompleted待ち
                Console.WriteLine("WaitOne");
                gate.WaitOne();
                Console.WriteLine("WaitOne Completed");
            }
            Console.WriteLine("-------------------------------");
        }

        static void BufferObservableSample()
        {
            Console.WriteLine("#BufferObservableSample");
            {
                var gate = new EventWaitHandle(false, EventResetMode.AutoReset);
                Observable
                    // 500ms間隔で値を発行
                    .Interval(TimeSpan.FromMilliseconds(500))
                    // 任意の値で値をまとめるのを辞める(この場合3秒間隔)
                    .Buffer(() => Observable.Interval(TimeSpan.FromSeconds(3)))
                    // 最初の3つだけ後続に流す
                    .Take(3)
                    .Subscribe(
                         l =>
                         {
                             // 値を表示
                             Console.WriteLine("--Buffer {0:HH:mm:ss}", DateTime.Now);
                             foreach (var i in l)
                             {
                                 Console.WriteLine(i);
                             }
                         },
                         () =>
                         {
                             // 完了
                             Console.WriteLine("OnCompleted");
                             gate.Set();
                         });
                // OnCompleted待ち
                Console.WriteLine("WaitOne");
                gate.WaitOne();
                Console.WriteLine("WaitOne Completed");
            }
            Console.WriteLine("-------------------------------");
            {
                var gate = new EventWaitHandle(false, EventResetMode.AutoReset);
                // クリックをエミュレート
                var clickEmuration = new Subject<Unit>();
                Observable
                    // 500ms間隔で値を発行
                    .Interval(TimeSpan.FromMilliseconds(500))
                    // 任意の値で値をまとめるのを辞める(この場合3秒間隔)
                    .Buffer(
                        // clickEmurationから通知がきたら
                        clickEmuration.AsObservable(),
                        // 2秒間値を集める
                        _ => Observable.Interval(TimeSpan.FromSeconds(2)))
                    // 最初の2つだけ後続に流す
                    .Take(2)
                    .Subscribe(
                         l =>
                         {
                             // 値を表示
                             Console.WriteLine("--Buffer {0:HH:mm:ss}", DateTime.Now);
                             foreach (var i in l)
                             {
                                 Console.WriteLine(i);
                             }
                         },
                         () =>
                         {
                             // 完了
                             Console.WriteLine("OnCompleted");
                             gate.Set();
                         });

                // Enterを押すとクリックを模したSubjectから通知を上げる
                Console.ReadLine();
                Console.WriteLine("{0:HH:mm:ss} Click emurate", DateTime.Now);
                clickEmuration.OnNext(Unit.Default);

                // Enterを押すとクリックを模したSubjectから通知を上げる
                Console.ReadLine();
                Console.WriteLine("{0:HH:mm:ss} Click emurate", DateTime.Now);
                clickEmuration.OnNext(Unit.Default);

                // OnCompleted待ち
                gate.WaitOne();
                Console.WriteLine("WaitOne Completed");
            }
            Console.WriteLine("-------------------------------");
        }

        static void BufferCountAndTimeSample()
        {
            Console.WriteLine("#BufferCountAndTimeSample");
            {
                var gate = new EventWaitHandle(false, EventResetMode.AutoReset);
                Observable
                    // 0-9の値をi * 200 ms間隔で発行する
                    .Generate(
                        0,
                        i => i < 10,
                        i => ++i,
                        i => i,
                        i => TimeSpan.FromMilliseconds(i * 200))
                    // 2秒間隔か2つ値がくるまでまとめる
                    .Buffer(TimeSpan.FromSeconds(2), 2)
                    .Subscribe(
                         l =>
                         {
                             // 値を表示
                             Console.WriteLine("--Buffer {0:HH:mm:ss}", DateTime.Now);
                             foreach (var i in l)
                             {
                                 Console.WriteLine(i);
                             }
                         },
                         () =>
                         {
                             // 完了
                             Console.WriteLine("OnCompleted");
                             gate.Set();
                         });
                gate.WaitOne();
            }
            Console.WriteLine("-------------------------------");
        }

        static void WindowSample()
        {
            Console.WriteLine("#WindowSample");
            {
                Console.WriteLine("Buffer behavior");
                // 値の発行元
                var source = new Subject<int>();
                source
                    // 3つずつに纏める
                    .Buffer(3)
                    .Subscribe(
                        l =>
                        {
                            // 値を表示
                            Console.WriteLine("--Buffer");
                            // IList<T>なのでループを使って値を出力
                            foreach (var i in l)
                            {
                                Console.WriteLine(i);
                            }
                        },
                        () =>
                        {
                            // 完了通知
                            Console.WriteLine("Buffer Completed");
                        });

                // 1～4の値を発行して終了
                Console.WriteLine("OnNext(1)");
                source.OnNext(1);
                Console.WriteLine("OnNext(2)");
                source.OnNext(2);
                Console.WriteLine("OnNext(3)");
                source.OnNext(3);
                Console.WriteLine("OnNext(4)");
                source.OnNext(4);
                Console.WriteLine("OnCompleted()");
                source.OnCompleted();
            }
            Console.WriteLine("-------------------------------");
            {
                Console.WriteLine("Window behavior");
                // 値の発行元
                var source = new Subject<int>();
                source
                    // 3つずつに纏める
                    .Window(3)
                    .Subscribe(
                        o =>
                        {
                            // 値を表示
                            Console.WriteLine("--Window");
                            // IO<T>なのでSubscribeでOnNextを使って出力する
                            o.Subscribe(Console.WriteLine);
                        },
                        () =>
                        {
                            // 完了通知
                            Console.WriteLine("Window Completed");
                        });

                // 1～4の値を発行して終了
                Console.WriteLine("OnNext(1)");
                source.OnNext(1);
                Console.WriteLine("OnNext(2)");
                source.OnNext(2);
                Console.WriteLine("OnNext(3)");
                source.OnNext(3);
                Console.WriteLine("OnNext(4)");
                source.OnNext(4);
                Console.WriteLine("OnCompleted()");
                source.OnCompleted();
            }
            Console.WriteLine("-------------------------------");
        }
    }
}
