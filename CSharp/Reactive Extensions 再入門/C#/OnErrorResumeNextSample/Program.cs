using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Linq;

namespace OnErrorResumeNextSample
{
    class Program
    {
        static void Main(string[] args)
        {
            OnErrorResumeNextSample();
        }

        static void OnErrorResumeNextSample()
        {
            Console.WriteLine("## OnErrorResumeNextSample");
            {
                Observable
                    // 例外を出す
                    .Throw<string>(new Exception())
                    // OnErrorResumeNextでエラーになったときの代わりを指定しておく
                    .OnErrorResumeNext(Observable.Return("OK"))
                    // 購読
                    .Subscribe(
                        s => Console.WriteLine("OnNext: {0}", s),
                        ex => Console.WriteLine("OnError: {0}", ex),
                        () => Console.WriteLine("OnCompleted"));
            }
            Console.WriteLine("---------------------------------------");
            {
                // 4番目にOK
                new[] { "NG", "Error", "Abort", "OK" }
                    // インデックスと値のペアに変換
                    .Select((s, i) => new { index = i, value = s })
                    // OK以外は例外を飛ばすIO<string>を返す(IEnumerable<IObservable<T>>へ変換)
                    .Select(s => s.value != "OK" ?
                        Observable.Throw<string>(new Exception(s.ToString())) :
                        Observable.Return(s.ToString()))
                    .OnErrorResumeNext()
                    // 購読
                    .Subscribe(
                        s => Console.WriteLine("OnNext: {0}", s),
                        ex => Console.WriteLine("OnError: {0}", ex),
                        () => Console.WriteLine("OnCompleted"));
            }
            Console.WriteLine("---------------------------------------");
            {
                // 2番目にOK
                new[] { "NG", "OK", "Abort", "Error" }
                    // インデックスと値のペアに変換
                    .Select((s, i) => new { index = i, value = s })
                    // OK以外は例外を飛ばすIO<string>を返す(IEnumerable<IObservable<T>>へ変換)
                    .Select(s => s.value != "OK" ?
                        Observable.Throw<string>(new Exception(s.ToString())) :
                        Observable.Return(s.ToString()))
                    .OnErrorResumeNext()
                    // 購読
                    .Subscribe(
                        s => Console.WriteLine("OnNext: {0}", s),
                        ex => Console.WriteLine("OnError: {0}", ex),
                        () => Console.WriteLine("OnCompleted"));
            }
            Console.WriteLine("---------------------------------------");
            {
                // OKが無い
                new[] { "NG", "Exception", "Abort", "Error" }
                    // インデックスと値のペアに変換
                    .Select((s, i) => new { index = i, value = s })
                    // OK以外は例外を飛ばすIO<string>を返す(IEnumerable<IObservable<T>>へ変換)
                    .Select(s => s.value != "OK" ?
                        Observable.Throw<string>(new Exception(s.ToString())) :
                        Observable.Return(s.ToString()))
                    .OnErrorResumeNext()
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
