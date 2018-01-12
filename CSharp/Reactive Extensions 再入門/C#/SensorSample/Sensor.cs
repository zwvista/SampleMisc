namespace SensorSample
{
    using System;
    using System.Threading;

    // センサーを表すクラス
    class Sensor
    {
        // センサーの値はとりあえず乱数で発行する
        private static Random random = new Random();
        // センサーの値を発行する間隔を制御するタイマー
        private Timer timer;

        // センサー名
        public string Name { get; private set; }

        // 名前をつけてセンサー作成
        public Sensor(string name)
        {
            this.Name = name;
        }

        // センサーの値の発行開始
        public void Start()
        {
            this.timer = new Timer(_ =>
            {
                this.OnPublish(random.Next(1001));
            },
            null,
            0,
            1000);
        }

        // センサーの値発行イベント
        public EventHandler<SensorEventArgs> Publish;

        // センサーから値を発行する
        private void OnPublish(int value)
        {
            var h = this.Publish;
            if (h != null)
            {
                h(this, new SensorEventArgs(this.Name, value));
            }
        }
    }

    // センサーが値を発行したときのイベント引数
    class SensorEventArgs : EventArgs
    {
        // 値を発行したセンサー名
        public string Name { get; private set; }
        // 発行した値
        public int Value { get; private set; }

        public SensorEventArgs(string name, int value)
        {
            this.Name = name;
            this.Value = value;
        }
    }
}
