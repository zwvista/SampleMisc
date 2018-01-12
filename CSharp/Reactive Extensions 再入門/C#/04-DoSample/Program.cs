using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Subjects;
using System.Reactive.Linq;

namespace _04_DoSample
{
    class Program
    {
        static void Main(string[] args)
        {
            // 普通のDo
            {
                Console.WriteLine("-------------------------");
                var subject = new Subject<int>();
                subject
                    // 途中に処理を挟む
                    .Do(i => Console.WriteLine("Do : {0}", i))
                    // 購読(購読しないとOnNextをしても値が流れないね)
                    .Subscribe(i => Console.WriteLine("OnNext : {0}", i));

                // 値の発行
                subject.OnNext(1);
                subject.OnNext(2);
                subject.OnNext(3);
            }

            // OnErrorも指定可能
            {
                Console.WriteLine("-------------------------");
                var subject = new Subject<int>();
                subject
                    // 途中に処理を挟む
                    .Do(
                        i => Console.WriteLine("Do : OnNext : {0}", i),
                        ex => Console.WriteLine("Do : OnError : {0}", ex),
                        () => Console.WriteLine("Do : OnCompleted"))
                    // 購読(購読しないとOnNextをしても値が流れないね)
                    .Subscribe(
                        i => Console.WriteLine("OnNext : {0}", i),
                        ex => Console.WriteLine("OnError : {0}", ex),
                        () => Console.WriteLine("OnCompleted"));
                // 値の発行
                subject.OnNext(1);
                subject.OnError(new Exception());
            }

            // OnCompletedも指定可能
            {
                Console.WriteLine("-------------------------");
                var subject = new Subject<int>();
                subject
                    .Do(
                        i => Console.WriteLine("Do : OnNext : {0}", i),
                        ex => Console.WriteLine("Do : OnError : {0}", ex),
                        () => Console.WriteLine("Do : OnCompleted"))
                    // 購読(購読しないとOnNextをしても値が流れないね)
                    .Subscribe(
                        i => Console.WriteLine("OnNext : {0}", i),
                        ex => Console.WriteLine("OnError : {0}", ex),
                        () => Console.WriteLine("OnCompleted"));

                // 値の発行
                subject.OnNext(1);
                subject.OnCompleted();
            }

        }
    }
}
