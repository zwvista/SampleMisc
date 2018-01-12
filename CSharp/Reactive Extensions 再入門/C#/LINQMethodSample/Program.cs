using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Subjects;
using System.Reactive.Linq;

namespace LINQMethodSample
{
    class Program
    {
        static void Main(string[] args)
        {
            WhereAndSelectSample();
        }

        public static void WhereAndSelectSample()
        {
            Console.WriteLine("## WhereAndSelectSample");
            // 値を発行するためのSubject
            var subject = new Subject<int>();
            // AsObservableでIObservable<T>に変換(ダウンキャストでSubject<T>に戻せない
            var source = subject.AsObservable();

            // 普通にSubscribe
            source.Subscribe(
                value => Console.WriteLine("1##OnNext({0})", value),
                ex => Console.WriteLine(ex.Message),
                () => Console.WriteLine("1##OnCompleted()"));

            // 奇数のみ通すようにフィルタリングして
            source.Where(i => i % 2 == 1)
                // 文字列に加工して
                .Select(i => i + "は奇数です")
                // 表示する
                .Subscribe(
                    value => Console.WriteLine("2##OnNext({0})", value),
                    ex => Console.WriteLine(ex.Message),
                    () => Console.WriteLine("2##OnCompleted()"));

            // 1～10の値をsubjectに対して発行する
            Observable.Range(1, 10).ForEach(i => subject.OnNext(i));
            // 完了通知を行う
            subject.OnCompleted();
            Console.WriteLine("-------------------------------------------");
        }
    }
}
