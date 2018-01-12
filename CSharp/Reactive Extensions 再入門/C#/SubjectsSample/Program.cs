using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Subjects;

namespace SubjectsSample
{
    class Program
    {
        static void Main(string[] args)
        {
            SubjectSample();
            BehaviorSubjectSample();
            BehaviorSubjectCacheSample();
            AsyncSubjectSample();
            ReplaySubjectSample();
        }

        static void SubjectSample()
        {
            Console.WriteLine("## SubjectSample");
            {
                var source = new Subject<int>();
                // 購読1
                Console.WriteLine("# Subscribe1");
                source.Subscribe(
                    i => Console.WriteLine("Subscribe1#OnNext: {0}", i),
                    ex => Console.WriteLine("Subscribe1#OnError: {0}", ex),
                    () => Console.WriteLine("Subscribe1#OnCompleted"));

                // 購読2
                Console.WriteLine("# Subscribe2");
                source.Subscribe(
                    i => Console.WriteLine("Subscribe2#OnNext: {0}", i),
                    ex => Console.WriteLine("Subscribe2#OnError: {0}", ex),
                    () => Console.WriteLine("Subscribe2#OnCompleted"));

                // 値の発行～完了
                Console.WriteLine("OnNext(1)");
                source.OnNext(1);
                Console.WriteLine("OnNext(2)");
                source.OnNext(2);
                Console.WriteLine("OnNext(3)");
                source.OnNext(3);
                Console.WriteLine("OnCompleted()");
                source.OnCompleted();
            }
            Console.WriteLine("-------------------------");
            {
                var source = new Subject<int>();

                // 購読
                Console.WriteLine("# Subscribe");
                source.Subscribe(
                    i => Console.WriteLine("Subscribe1#OnNext: {0}", i),
                    ex => Console.WriteLine("Subscribe1#OnError: {0}", ex),
                    () => Console.WriteLine("Subscribe1#OnCompleted"));

                // 例外
                source.OnError(new Exception("Error!!"));
            }
            Console.WriteLine("-------------------------");
        }

        static void BehaviorSubjectSample()
        {
            Console.WriteLine("# BehaviorSubjectSample");
            // 初期値0のBehaviorSubject
            var behaviorSubject = new BehaviorSubject<int>(0);

            // 購読
            Console.WriteLine("## Subscribe call");
            behaviorSubject.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                ex => Console.WriteLine("OnError({0})", ex.Message),
                () => Console.WriteLine("OnCompleted()"));

            // 順に値を発行して終了
            Console.WriteLine("## OnNext(1) call");
            behaviorSubject.OnNext(1);
            Console.WriteLine("## OnNext(2) call");
            behaviorSubject.OnNext(2);
            Console.WriteLine("## OnNext(3) call");
            behaviorSubject.OnNext(3);
            Console.WriteLine("## OnCompleted() call");
            behaviorSubject.OnCompleted();
            Console.WriteLine("-------------------------");
        }

        static void BehaviorSubjectCacheSample()
        {
            Console.WriteLine("# BehaviorSubjectCacheSample");
            // 初期値0のBehaviorSubject
            var behaviorSubject = new BehaviorSubject<int>(0);

            // 値を発行
            Console.WriteLine("## OnNext(1) call");
            behaviorSubject.OnNext(1);

            Console.WriteLine("Subscribe");
            // 値を発行した後に購読開始
            behaviorSubject.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                () => Console.WriteLine("OnCompleted()"));

            behaviorSubject.OnCompleted();

            // 終了した後に購読開始
            Console.WriteLine("Subscribe");
            behaviorSubject.Subscribe(
                i => Console.WriteLine("OnNext({0})", i),
                () => Console.WriteLine("OnCompleted()"));
            Console.WriteLine("-------------------------");
        }

        static void AsyncSubjectSample()
        {
            Console.WriteLine("# AsyncSubjectSample");
            {
                var source = new AsyncSubject<int>();

                // 購読
                Console.WriteLine("# Subscribe1");
                source.Subscribe(
                    i => Console.WriteLine("Subscribe1#OnNext: {0}", i),
                    ex => Console.WriteLine("Subscribe1#OnError: {0}", ex),
                    () => Console.WriteLine("Subscribe1#OnCompleted"));

                // 値の発行～完了
                Console.WriteLine("OnNext(1)");
                source.OnNext(1);
                Console.WriteLine("OnNext(2)");
                source.OnNext(2);
                Console.WriteLine("OnNext(3)");
                source.OnNext(3);
                Console.WriteLine("OnCompleted()");
                source.OnCompleted();

                // OnCompleted後の購読
                Console.WriteLine("# Subscribe2");
                source.Subscribe(
                    i => Console.WriteLine("Subscribe2#OnNext: {0}", i),
                    ex => Console.WriteLine("Subscribe2#OnError: {0}", ex),
                    () => Console.WriteLine("Subscribe2#OnCompleted"));
            }
            Console.WriteLine("-------------------------");
        }

        static void ReplaySubjectSample()
        {
            Console.WriteLine("# ReplaySubjectSample");
            {
                var source = new ReplaySubject<int>();

                // 購読
                Console.WriteLine("# Subscribe1");
                source.Subscribe(
                    i => Console.WriteLine("Subscribe1#OnNext: {0}", i),
                    ex => Console.WriteLine("Subscribe1#OnError: {0}", ex),
                    () => Console.WriteLine("Subscribe1#OnCompleted"));

                // 値の発行～完了
                Console.WriteLine("OnNext(1)");
                source.OnNext(1);
                Console.WriteLine("OnNext(2)");
                source.OnNext(2);
                Console.WriteLine("OnNext(3)");
                source.OnNext(3);
                Console.WriteLine("OnCompleted()");
                source.OnCompleted();

                // OnCompleted後の購読
                Console.WriteLine("# Subscribe2");
                source.Subscribe(
                    i => Console.WriteLine("Subscribe2#OnNext: {0}", i),
                    ex => Console.WriteLine("Subscribe2#OnError: {0}", ex),
                    () => Console.WriteLine("Subscribe2#OnCompleted"));
            }
            Console.WriteLine("-------------------------");
        }
        
    }
}
