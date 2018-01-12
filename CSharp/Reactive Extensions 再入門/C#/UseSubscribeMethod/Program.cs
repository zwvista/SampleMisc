namespace UseSubscribeMethod
{
    using System;

    class Program
    {
        static void Main(string[] args)
        {
            // 監視される人を作成
            var source = new NumberObservable();
            // 2つ監視役を登録
            var subscriber1 = source.Subscribe(
                // OnNext
                value => Console.WriteLine("OnNext({0}) called.", value),
                // OnError
                ex => Console.WriteLine("OnError({0}) called.", ex.Message),
                // OnCompleted
                () => Console.WriteLine("OnCompleted() called."));
            var subscriber2 = source.Subscribe(
                // OnNext
                value => Console.WriteLine("OnNext({0}) called.", value),
                // OnError
                ex => Console.WriteLine("OnError({0}) called.", ex.Message),
                // OnCompleted
                () => Console.WriteLine("OnCompleted() called."));

            // 監視される人の処理を実行
            Console.WriteLine("## Execute(1)");
            source.Execute(1);
            // 1つを監視する人から解雇
            Console.WriteLine("## Dispose");
            subscriber2.Dispose();
            // 再度処理を実行
            Console.WriteLine("## Execute(2)");
            source.Execute(2);
            // エラーを起こしてみる
            Console.WriteLine("## Execute(0)");
            source.Execute(0);
            // もう1つ監視役を追加して完了通知を行う
            var sbscriber3 = source.Subscribe(
                // OnNext
                value => Console.WriteLine("OnNext({0}) called.", value),
                // OnError
                ex => Console.WriteLine("OnError({0}) called.", ex.Message),
                // OnCompleted
                () => Console.WriteLine("OnCompleted() called."));
            Console.WriteLine("## Completed");
            source.Completed();
        }
    }
}
