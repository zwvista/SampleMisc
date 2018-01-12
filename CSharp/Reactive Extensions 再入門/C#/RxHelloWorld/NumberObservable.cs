namespace IObservableIObserverImpl
{
    using System;
    using System.Collections.Generic;

    /// <summary>
    /// 監視されるクラス
    /// </summary>
    class NumberObservable : IObservable<int>
    {
        // 自分を監視してる人を管理するリスト
        private List<IObserver<int>> observers = new List<IObserver<int>>();

        // 自分を監視してる人に通知を行う
        // 0を渡したらエラー通知
        public void Execute(int value)
        {
            if (value == 0)
            {
                foreach (var obs in observers)
                {
                    obs.OnError(new Exception("value is 0"));
                }

                // エラーが起きたので処理は終了
                this.observers.Clear();
                return;
            }

            foreach (var obs in observers)
            {
                obs.OnNext(value);
            }
        }

        // 完了通知
        public void Completed()
        {
            foreach (var obs in observers)
            {
                obs.OnCompleted();
            }
            // 完了したので監視してる人たちをクリア
            this.observers.Clear();
        }

        // 監視してる人を追加する。
        // 戻り値のIDisposableをDisposeすると監視から外れる。
        public IDisposable Subscribe(IObserver<int> observer)
        {
            this.observers.Add(observer);
            return new RemoveListDisposable(observers, observer);
        }

        // Disposeが呼ばれたらobserverを監視対象から削除する
        private class RemoveListDisposable : IDisposable
        {
            private List<IObserver<int>> observers = new List<IObserver<int>>();
            private IObserver<int> observer;

            public RemoveListDisposable(List<IObserver<int>> observers, IObserver<int> observer)
            {
                this.observers = observers;
                this.observer = observer;
            }

            public void Dispose()
            {
                if (this.observers == null)
                {
                    return;
                }

                if (observers.IndexOf(observer) != -1)
                {
                    this.observers.Remove(observer);
                }

                this.observers = null;
                this.observer = null;
            }
        }
    }
}
