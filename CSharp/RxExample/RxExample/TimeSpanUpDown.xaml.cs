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
        public DateTime Value { get; set; } = new DateTime(2000, 1, 1);
        public IObservable<int> ValueChanged;
        public void SetValue(int value)
        {
            value = Math.Max(0, Math.Min(86399, value));
            var h = value / 3600;
            var m = value / 60 % 60;
            var s = value % 60;
            Value = new DateTime(2000, 1, 1, h, m, s);
        }
        public TimeSpanUpDownViewModel()
        {
            ValueChanged = this.WhenAnyValue(x => x.Value, (DateTime v) => v.Hour * 3600 + v.Minute * 60 + v.Second);
        }
    }
}
