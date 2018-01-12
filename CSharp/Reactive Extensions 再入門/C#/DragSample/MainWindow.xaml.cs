namespace DragSample
{
    using System;
    using System.Reactive.Linq;
    using System.Windows;
    using System.Windows.Controls;
    using System.Windows.Input;
    using System.Windows.Media;
    using System.Windows.Shapes;

    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            // マウスダウン、マウスアップ、マウスムーブのIObservableを作る
            var mouseDown = Observable.FromEvent<MouseButtonEventHandler, MouseButtonEventArgs>(
                h => (s, e) => h(e),
                h => this.MouseDown += h,
                h => this.MouseDown -= h);

            var mouseMove = Observable.FromEvent<MouseEventHandler, MouseEventArgs>(
                h => (s, e) => h(e),
                h => this.MouseMove += h,
                h => this.MouseMove -= h);

            var mouseUp = Observable.FromEvent<MouseButtonEventHandler, MouseButtonEventArgs>(
                h => (s, e) => h(e),
                h => this.MouseUp += h,
                h => this.MouseUp -= h);

            var drag = mouseMove
                // マウスムーブをマウスダウンまでスキップ。マウスダウン時にマウスをキャプチャ
                .SkipUntil(mouseDown.Do(_ => this.CaptureMouse()))
                // マウスアップが行われるまでTake。マウスアップでマウスのキャプチャをリリース
                .TakeUntil(mouseUp.Do(_ => this.ReleaseMouseCapture()))
                // ドラッグが終了したタイミングでCompletedを表示
                .Finally(() => textBlock.Text = "Completed")
                // これを繰り返す
                .Repeat();

            // ドラッグ中は、イベント引数から座標を取り出して表示用に整えてTextBlockに設定
            drag.Select(e => e.GetPosition(null))
                .Select(p => string.Format("X: {0}, Y:{1}", p.X, p.Y))
                .Subscribe(s => textBlock.Text = s);
                    
        }

    }
}
