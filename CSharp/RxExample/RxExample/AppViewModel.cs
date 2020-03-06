using ReactiveUI;
using ReactiveUI.Fody.Helpers;

namespace RxExample
{
    public class AppViewModel : ReactiveObject
    {
        [Reactive]
        public string Number1 { get; set; } = "1";

        [Reactive]
        public string Number2 { get; set; } = "2";

        [Reactive]
        public string Number3 { get; set; } = "3";

        public string Result { [ObservableAsProperty] get; }

        public AppViewModel()
        {
            int f(string s) =>
                int.TryParse(s, out var o) ? o : 0;

            this.WhenAnyValue(x => x.Number1, x => x.Number2, x => x.Number3,
                (s1, s2, s3) => (f(s1) + f(s2) + f(s3)).ToString())
                .ToPropertyEx(this, x => x.Result);
        }
    }
}
