using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Subjects;
using System.Reactive.Linq;

namespace TypeConvertSample
{
    class Program
    {
        static void Main(string[] args)
        {
            CastSample();
            OfTypeSample();
        }

        static void CastSample()
        {
            Console.WriteLine("#CastSample");
            var source = new Subject<object>();
            source
                // 値をint型にキャスト
                .Cast<int>()
                // 購読
                .Subscribe(
                    i => Console.WriteLine("OnNext({0})", i),
                    ex => Console.WriteLine("OnError({0})", ex),
                    () => Console.WriteLine("OnCompleted()"));

            // int型の値を発行
            source.OnNext(1);
            source.OnNext(2);
            source.OnNext(3);

            // 文字列型を発行してみる
            source.OnNext("abc");

            Console.WriteLine("-----------------------------");
        }

        static void OfTypeSample()
        {
            Console.WriteLine("#OfTypeSample");
            var source = new Subject<object>();
            source
                // int型のみを通過させる
                .OfType<int>()
                // 購読
                .Subscribe(
                    i => Console.WriteLine("OnNext({0})", i),
                    ex => Console.WriteLine("OnError({0})", ex),
                    () => Console.WriteLine("OnCompleted()"));

            // int型の値を発行
            source.OnNext(1);
            source.OnNext(2);
            source.OnNext(3);

            // 文字列型を発行してみる
            source.OnNext("abc");
            source.OnNext("4");

            // もう一度int型の値を発行して終了
            source.OnNext(100);
            source.OnCompleted();
            Console.WriteLine("-----------------------------");
        }
    }
}
