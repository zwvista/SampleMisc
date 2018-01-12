using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reactive.Linq;
using System.Reactive.Subjects;

namespace MulticastSample
{
    class Program
    {
        static void Main(string[] args)
        {
            var s = Observable.Range(1, 10);
            var o = s.Multicast(new SampleSubject());

            Console.WriteLine("Subscribe");
            o.Subscribe(Console.WriteLine);

            Console.WriteLine("Connect");
            o.Connect();
        }
    }

    class SampleSubject : ISubject<int, string>
    {
        private Subject<string> source = new Subject<string>();

        #region IObserver<int> メンバー

        public void OnCompleted()
        {
            source.OnCompleted();
        }

        public void OnError(Exception error)
        {
            source.OnError(error);
        }

        public void OnNext(int value)
        {
            source.OnNext(value + " converted");
        }

        #endregion

        #region IObservable<string> メンバー

        public IDisposable Subscribe(IObserver<string> observer)
        {
            return source.Subscribe(observer);
        }

        #endregion
    }

}
