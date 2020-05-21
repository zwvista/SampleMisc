using ReactiveUI;
using ReactiveUI.Fody.Helpers;
using System;
using System.Reactive.Linq;
using System.Windows;
using System.Windows.Controls;

namespace RxExample
{
    /// <summary>
    /// TimeSpanUpDown.xaml の相互作用ロジック
    /// </summary>
    public partial class TimeSpanUpDown : UserControl
    {
        public TimeSpanUpDownViewModel VM { get; } = new TimeSpanUpDownViewModel();

        public int Value
        {
            get => (int)GetValue(ValueProperty);
            set => SetValue(ValueProperty, value);
        }

        public static readonly DependencyProperty ValueProperty =
            DependencyProperty.Register("Value", typeof(int), typeof(TimeSpanUpDown),
                new FrameworkPropertyMetadata(0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, ValuePropertyChangedCallback));

        private static void ValuePropertyChangedCallback(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var control = d as TimeSpanUpDown;
            control.VM.SetValue(control.Value);
        }

        public TimeSpanUpDown()
        {
            InitializeComponent();
            grid.DataContext = VM;
            VM.ValueChanged.Subscribe(value => SetValue(ValueProperty, value));
        }
    }
    public class TimeSpanUpDownViewModel : ReactiveObject
    {
        [Reactive]
        public int Hour { get; set; }
        [Reactive]
        public int Minute { get; set; }
        [Reactive]
        public int Second { get; set; }
        public IObservable<int> ValueChanged;
        public void SetValue(int value)
        {
            Hour = value / 3600;
            Minute = value / 60 % 60;
            Second = value % 60;
        }
        public TimeSpanUpDownViewModel()
        {
            ValueChanged = this.WhenAnyValue(x => x.Hour, x => x.Minute, x => x.Second, (h, m, s) => h * 3600 + m * 60 + s);
        }
    }
}
