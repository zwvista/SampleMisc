namespace CatchFinallySample
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Reactive.Subjects;
    using System.Reactive.Linq;

    class Program
    {
        static void Main(string[] args)
        {
            CatchSampleSimpleCase();
            CatchSampleEmptyCase();
            CatchSampleOtherCase();
            FinallySample();
            FinallySampleErrorCase();
            FinallySampleErrorCase2();
            FinallySampleErrorCase3();
        }

        private static void CatchSampleSimpleCase()
        {
            Console.WriteLine("# CatchSampleSimpleCase");
            var source = new Subject<string>();
            source
                // sourceから例外が発生したらErrorという文字列を後続へ流す
                .Catch(Observable.Return("Error"))
                // 購読
                .Subscribe(
                    s => Console.WriteLine("OnNext {0}", s),
                    ex => Console.WriteLine("OnError {0}", ex),
                    () => Console.WriteLine("OnCompleted"));

            // 2つ値を発行したあと、例外を流して、その後にもう１つ値を発行する。
            source.OnNext("A");
            source.OnNext("B");
            source.OnError(new Exception("例外"));
            source.OnNext("C");
            Console.WriteLine("-------------------------");
        }

        private static void CatchSampleEmptyCase()
        {
            Console.WriteLine("# CatchSampleEmptyCase");
            var source = new Subject<string>();
            source
                // エラーが起きたら終了する
                .Catch(Observable.Empty<string>())
                // 購読
                .Subscribe(
                    s => Console.WriteLine("OnNext {0}", s),
                    ex => Console.WriteLine("OnError {0}", ex),
                    () => Console.WriteLine("OnCompleted"));

            // 2つ値を発行したあと、例外を流して、その後にもう１つ値を発行する。
            source.OnNext("A");
            source.OnNext("B");
            source.OnError(new Exception("例外"));
            source.OnNext("C");
            Console.WriteLine("-------------------------");
        }

        private static void CatchSampleOtherCase()
        {
            Console.WriteLine("# CatchSampleOtherCase");
            var source = new Subject<string>();
            source
                // ArgumentException発生時にはArgumentExceptionからの復帰という文字列を後続に流す
                .Catch((ArgumentException ex) => Observable.Return("ArgumentExceptionから復帰"))
                // NullReferenceException発生時には何もせず終了
                .Catch((NullReferenceException ex) => Observable.Empty<string>())
                // 購読
                .Subscribe(
                    s => Console.WriteLine("OnNext {0}", s),
                    ex => Console.WriteLine("OnError {0}", ex),
                    () => Console.WriteLine("OnCompleted"));

            // 2つ値を発行したあと、例外を流して、その後にもう１つ値を発行する。
            source.OnNext("A");
            source.OnNext("B");
            // ## 1, ## 2, ## 3のいずれかの行のコメントを外して実行してください
            //source.OnError(new ArgumentException("例外")); // ## 1
            //source.OnError(new NullReferenceException("例外")); // ## 2
            source.OnError(new Exception("例外")); // ## 3
            source.OnNext("C");
            Console.WriteLine("-------------------------");
        }

        private static void FinallySample()
        {
            Console.WriteLine("# FinallySample");
            var source = new Subject<string>();
            source
                // 終了時に必ず呼ばれる処理
                .Finally(() => Console.WriteLine("Finally"))
                // 購読
                .Subscribe(
                    s => Console.WriteLine("OnNext {0}", s),
                    ex => Console.WriteLine("OnError {0}", ex),
                    () => Console.WriteLine("OnCompleted"));

            // 2つ値を発行したあと、終了する。
            source.OnNext("A");
            source.OnNext("B");
            source.OnCompleted();
            Console.WriteLine("-------------------------");
        }

        private static void FinallySampleErrorCase()
        {
            Console.WriteLine("# FinallySampleErrorCase");
            var source = new Subject<string>();
            source
                // sourceから例外が発生したらErrorという文字列を後続へ流す
                .Catch(Observable.Return("Error"))
                // 終了時に必ず呼ばれる処理
                .Finally(() => Console.WriteLine("Finally"))
                // 購読
                .Subscribe(
                    s => Console.WriteLine("OnNext {0}", s),
                    ex => Console.WriteLine("OnError {0}", ex),
                    () => Console.WriteLine("OnCompleted"));

            // 2つ値を発行したあと、例外を発生させる。
            source.OnNext("A");
            source.OnNext("B");
            source.OnError(new Exception("例外"));
            Console.WriteLine("-------------------------");
        }

        private static void FinallySampleErrorCase2()
        {
            Console.WriteLine("# FinallySampleErrorCase2");
            var source = new Subject<string>();
            source
                // 終了時に必ず呼ばれる処理
                .Finally(() => Console.WriteLine("Finally"))
                // 購読
                .Subscribe(
                    s => Console.WriteLine("OnNext {0}", s),
                    ex => Console.WriteLine("OnError {0}", ex),
                    () => Console.WriteLine("OnCompleted"));

            // 2つ値を発行したあと、例外を発生させる。
            source.OnNext("A");
            source.OnNext("B");
            source.OnError(new Exception("例外"));
            Console.WriteLine("-------------------------");
        }

        private static void FinallySampleErrorCase3()
        {
            Console.WriteLine("# FinallySampleErrorCase2");
            var source = new Subject<string>();
            var subscriber = source
                // 終了時に必ず呼ばれる処理
                .Finally(() => Console.WriteLine("Finally"))
                // 購読(エラー処理無し)
                .Subscribe(
                    s => Console.WriteLine("OnNext {0}", s));

            // 2つ値を発行したあと、例外を発生させる。
            source.OnNext("A");
            source.OnNext("B");
            try
            {
                // Reactive Extensionsのシーケンスの処理内で例外が処理されないため例外がスローされる
                source.OnError(new Exception("例外"));
            }
            catch
            {
                // 例外が発生した場合はログを出して握りつぶす
                Console.WriteLine("catch句");
            }
            // 購読を解除する
            Console.WriteLine("subscriber.Dispose()");
            subscriber.Dispose();
            Console.WriteLine("-------------------------");
        }
    }
}
