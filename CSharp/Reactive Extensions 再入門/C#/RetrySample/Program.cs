using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Linq;
using System.Reactive.Disposables;

namespace RetrySample
{
    class Program
    {
        static void Main(string[] args)
        {
            RetrySample();
        }

        static void RetrySample()
        {
            Console.WriteLine("## RetrySample");
            {
                var retryCount = 0;
                Observable
                    // retryCountが3になるまでエラーになる
                    .Create<string>(o =>
                    {
                        Console.WriteLine("Create method called: {0}", retryCount);
                        if (retryCount == 3)
                        {
                            o.OnNext(retryCount.ToString());
                            o.OnCompleted();
                            return Disposable.Empty;
                        }

                        retryCount++;
                        o.OnError(new InvalidOperationException(retryCount.ToString()));
                        return Disposable.Empty;
                    })
                    // 成功するまでリトライする
                    .Retry()
                    // 購読
                    .Subscribe(
                        s => Console.WriteLine("OnNext: {0}", s),
                        ex => Console.WriteLine("OnError: {0}", ex),
                        () => Console.WriteLine("OnCompleted"));
            }
            Console.WriteLine("---------------------------------------");
            {
                var retryCount = 0;
                Observable
                    // retryCountが3になるまでエラーになる
                    .Create<string>(o =>
                    {
                        Console.WriteLine("Create method called: {0}", retryCount);
                        if (retryCount == 3)
                        {
                            o.OnNext(retryCount.ToString());
                            o.OnCompleted();
                            return Disposable.Empty;
                        }

                        retryCount++;
                        o.OnError(new InvalidOperationException(retryCount.ToString()));
                        return Disposable.Empty;
                    })
                    // 2回リトライする
                    .Retry(2)
                    // 購読
                    .Subscribe(
                        s => Console.WriteLine("OnNext: {0}", s),
                        ex => Console.WriteLine("OnError: {0}", ex),
                        () => Console.WriteLine("OnCompleted"));
            }
            Console.WriteLine("---------------------------------------");
        }
    }
}
