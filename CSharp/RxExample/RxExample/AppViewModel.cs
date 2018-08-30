using ReactiveUI;
using System;

namespace RxExample
{
    public class AppViewModel : ReactiveObject
    {
        private string _number1;
        public string Number1
        {
            get { return _number1; }
            set { this.RaiseAndSetIfChanged(ref _number1, value); }
        }

        private string _number2;
        public string Number2
        {
            get { return _number2; }
            set { this.RaiseAndSetIfChanged(ref _number2, value); }
        }

        private string _number3;
        public string Number3
        {
            get { return _number3; }
            set { this.RaiseAndSetIfChanged(ref _number3, value); }
        }

        private readonly ObservableAsPropertyHelper<string> _result;
        public string Result => _result.Value;

        public AppViewModel()
        {
            _number1 = "1"; _number2 = "2"; _number3 = "3";

            int f(string s) =>
                int.TryParse(s, out var o) ? o : 0;

            _result = this.WhenAnyValue(x => x.Number1, x => x.Number2, x => x.Number3,
                (s1, s2, s3) => (f(s1) + f(s2) + f(s3)).ToString())
                .ToProperty(this, x => x.Result, "");
        }

    }

}