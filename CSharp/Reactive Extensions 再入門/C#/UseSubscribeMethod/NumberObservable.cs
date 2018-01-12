namespace UseSubscribeMethod
{
    using System;
    using System.Reactive.Subjects;

    class NumberObservable : IObservable<int>
    {
        // IObservable<T>とIObserver<T>の両方を兼ねるクラス
        private Subject<int> source = new Subject<int>();

        // 自分を監視してる人に通知を行う
        // 0を渡したらエラー通知
        public void Execute(int value)
        {
            if (value == 0)
            {
                this.source.OnError(new Exception("value is 0"));
                // エラー状態じゃないまっさらなSubjectを再作成
                this.source = new Subject<int>();
                return;
            }

            this.source.OnNext(value);
        }

        // 完了通知
        public void Completed()
        {
            this.source.OnCompleted();
        }

        // 監視してる人を追加する。
        // 戻り値のIDisposableをDisposeすると監視から外れる。
        public IDisposable Subscribe(IObserver<int> observer)
        {
            return this.source.Subscribe(observer);
        }
    }
}
