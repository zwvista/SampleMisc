namespace IObservableIObserverImpl
{
    using System;

    // 監視する人
    class PrintObserver : IObserver<int>
    {
        // 監視対象から通知が来たときの処理
        public void OnNext(int value)
        {
            Console.WriteLine("OnNext({0}) called.", value);
        }

        // 完了通知が来たときの処理
        public void OnCompleted()
        {
            Console.WriteLine("OnCompleted called.");
        }

        // エラー通知が来たときの処理
        public void OnError(Exception error)
        {
            Console.WriteLine("OnError({0}) called.", error.Message);
        }
    }
}
