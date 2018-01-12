namespace SingleValueSample
{
    using System;
    using System.Reactive.Linq;
    using System.Reactive.Subjects;
    using System.Threading;

    class Program
    {
        static void Main(string[] args)
        {
            //FirstSample();
            //FirstNoElementSample();
            //FirstOrDefaultSample();
            //FirstOrDefaultNoElementSample();

            //LastSample();
            //LastNoElementSample();
            //LastOrDefaultSample();
            //LastOrDefaultNoElementSample();

            //ElementAtSample();
            //ElementAtNoElementSample();
            //ElementAtOrDefaultSample();
            //ElementAtOrDefaultNoElementSample();

            //SingleSample();
            //SinglePredicateSample();
            SingleOrDefaultSample();
        }

        static void FirstSample()
        {
            Console.WriteLine("#FirstSample");
            // Observableを作成前のタイムスタンプを表示
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            var firstResult = Observable
                // 5秒間隔で値を発行する
                .Interval(TimeSpan.FromSeconds(5))
                .Select(i => "value is " + i)
                // 最初の値を取得
                .First();
            // Firstの実行が終わった後のタイムスタンプを表示
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            // 取得した値を表示
            Console.WriteLine("firstResult: {0}", firstResult);
            Console.WriteLine("-------------------------------");
        }

        static void FirstNoElementSample()
        {
            Console.WriteLine("#FirstNoElementsSample");
            // 1つも要素の無いIObservable
            var noElementsSequence = new Subject<string>();
            noElementsSequence.OnCompleted();

            try
            {
                // 要素が無いものに対して最初の要素を要求
                var firstResult = noElementsSequence.First();
            }
            catch (InvalidOperationException e)
            {
                // 何もないので例外が発生する
                Console.WriteLine("Exception: {0}", e.Message);
            }
            Console.WriteLine("-------------------------------");
        }

        static void FirstOrDefaultSample()
        {
            Console.WriteLine("#FirstOrDefaultSample");
            // Observableを作成前のタイムスタンプを表示
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            var firstResult = Observable
                // 5秒間隔で値を発行する
                .Interval(TimeSpan.FromSeconds(5))
                .Select(i => "value is " + i)
                // 最初の値を取得
                .FirstOrDefault();
            // Firstの実行が終わった後のタイムスタンプを表示
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            // 取得した値を表示
            Console.WriteLine("firstResult: {0}", firstResult);
            Console.WriteLine("-------------------------------");
        }

        static void FirstOrDefaultNoElementSample()
        {
            Console.WriteLine("#FirstOrDefaultNoElementSample");
            // 1つも要素の無いIObservable
            var noElementsSequence = new Subject<string>();
            noElementsSequence.OnCompleted();

            // 最初の値 or デフォルト値を取得
            var firstResult = noElementsSequence.FirstOrDefault();
            // 結果を出力。この場合はnullが表示される。
            Console.WriteLine("firstResult: {0}", firstResult ?? "null");
            Console.WriteLine("-------------------------------");
        }

        static void LastSample()
        {
            Console.WriteLine("#LastSample");
            // Observableを作成前のタイムスタンプを表示
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            var lastResult = Observable
                // 1秒間隔で値を5つ発行するIObservable
                .Generate(0, i => i < 5, i => ++i, i => "value is " + i, i => TimeSpan.FromSeconds(1))
                // 最後の値を取得
                .Last();
            // Lastの実行が終わった後のタイムスタンプを表示
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            // Lastの実行結果を取得
            Console.WriteLine("lastResult: {0}", lastResult);
            Console.WriteLine("-------------------------------");
        }

        static void LastNoElementSample()
        {
            Console.WriteLine("#LastNoElementSample");
            // 1つも要素の無いIObservable
            var noElementsSequence = new Subject<string>();
            noElementsSequence.OnCompleted();

            try
            {
                // 最後の要素の取得
                var lastResult = noElementsSequence.Last();
            }
            catch (InvalidOperationException e)
            {
                // 何もないので例外が発生する
                Console.WriteLine("Exception: {0}", e.Message);
            }
            Console.WriteLine("-------------------------------");
        }

        static void LastOrDefaultSample()
        {
            Console.WriteLine("#LastOrDefaultSample");
            // Observableを作成前のタイムスタンプを表示
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            var lastResult = Observable
                // 1秒間隔で値を5つ発行するIObservable
                .Generate(0, i => i < 5, i => ++i, i => "value is " + i, i => TimeSpan.FromSeconds(1))
                // 最後の値を取得
                .LastOrDefault();
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            // LastOrDefaultの実行結果を取得
            Console.WriteLine("lastResult: {0}", lastResult);
            Console.WriteLine("-------------------------------");
        }

        static void LastOrDefaultNoElementSample()
        {
            Console.WriteLine("#LastOrDefaultNoElementSample");
            // 1つも要素の無いIObservable
            var noElementsSequence = new Subject<string>();
            noElementsSequence.OnCompleted();

            // 最後の値 or デフォルト値を取得
            var lastResult = noElementsSequence.LastOrDefault();
            // 結果を出力。この場合はnullが表示される。
            Console.WriteLine("lastResult: {0}", lastResult ?? "null");
            Console.WriteLine("-------------------------------");
        }

        static void ElementAtSample()
        {
            Console.WriteLine("#ElementAtSample");
            // 待機用のWaitHandle
            var gate = new EventWaitHandle(false, EventResetMode.AutoReset);
            // ElementAt前のタイムスタンプを表示
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            Observable
                // 1秒間隔で5回値を発行する
                .Generate(0, i => i < 5, i => ++i, i => "value is " + i, i => TimeSpan.FromSeconds(1))
                // 3番目の要素を取得する
                .ElementAt(3)
                // この一連のシーケンスの最後で待機しているスレッドを解放する
                .Finally(() => gate.Set())
                // 購読
                .Subscribe(
                    // 値を表示する（nullの場合はnullと表示する）
                    i => Console.WriteLine("elementAt3: {0}", i ?? "null"),
                    // 例外が発生した場合は例外のメッセージを表示する
                    ex => Console.WriteLine("Exception: {0}, {1}", ex.GetType().Name, ex.Message),
                    // 完了を示すメッセージを表示する
                    () => Console.WriteLine("OnCompleted"));
            // 一連のメソッドチェインが終わった時のタイムスタンプを表示する
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            // gate.Set()が呼ばれるまで停止
            gate.WaitOne();
            // gate.Setが呼ばれた後のタイムスタンプを表示する
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            Console.WriteLine("-------------------------------");
        }

        static void ElementAtNoElementSample()
        {
            Console.WriteLine("#ElementAtNoElementSample");
            // 待機用のWaitHandle
            var gate = new EventWaitHandle(false, EventResetMode.AutoReset);
            // ElementAt前のタイムスタンプを表示
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            Observable
                // 3要素しか発行しない
                .Generate(0, i => i < 2, i => ++i, i => "value is " + i, i => TimeSpan.FromSeconds(1))
                // 4番目の要素を取得する
                .ElementAt(3)
                // この一連のシーケンスの最後で待機しているスレッドを解放する
                .Finally(() => gate.Set())
                // 購読
                .Subscribe(
                    // 値を表示する（nullの場合はnullと表示する）
                    i => Console.WriteLine("elementAt3: {0}", i ?? "null"),
                    // 例外が発生した場合は例外のメッセージを表示する
                    ex => Console.WriteLine("Exception: {0}, {1}", ex.GetType().Name, ex.Message),
                    // 完了を示すメッセージを表示する
                    () => Console.WriteLine("OnCompleted"));
            // 一連のメソッドチェインが終わった時のタイムスタンプを表示する
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            // gate.Set()が呼ばれるまで停止
            gate.WaitOne();
            // gate.Setが呼ばれた後のタイムスタンプを表示する
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            Console.WriteLine("-------------------------------");
        }

        static void ElementAtOrDefaultSample()
        {
            Console.WriteLine("#ElementAtOrDefaultSample");
            // 待機用のWaitHandle
            var gate = new EventWaitHandle(false, EventResetMode.AutoReset);
            // ElementAt前のタイムスタンプを表示
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            Observable
                // 1秒間隔で5回値を発行する
                .Generate(0, i => i < 5, i => ++i, i => "value is " + i, i => TimeSpan.FromSeconds(1))
                // 3番目の要素 or デフォルト値を取得する
                .ElementAtOrDefault(3)
                // この一連のシーケンスの最後で待機しているスレッドを解放する
                .Finally(() => gate.Set())
                // 購読
                .Subscribe(
                    // 値を表示する（nullの場合はnullと表示する）
                    i => Console.WriteLine("elementAt3: {0}", i ?? "null"),
                        // 例外が発生した場合は例外のメッセージを表示する
                    ex => Console.WriteLine("Exception: {0}, {1}", ex.GetType().Name, ex.Message),
                // 完了を示すメッセージを表示する
                    () => Console.WriteLine("OnCompleted"));
            // 一連のメソッドチェインが終わった時のタイムスタンプを表示する
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            // gate.Set()が呼ばれるまで停止
            gate.WaitOne();
            // gate.Setが呼ばれた後のタイムスタンプを表示する
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            Console.WriteLine("-------------------------------");
        }

        static void ElementAtOrDefaultNoElementSample()
        {
            Console.WriteLine("#ElementAtOrDefaultNoElementSample");
            // 待機用のWaitHandle
            var gate = new EventWaitHandle(false, EventResetMode.AutoReset);
            // ElementAt前のタイムスタンプを表示
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            Observable
                // 3要素しか発行しない
                .Generate(0, i => i < 2, i => ++i, i => "value is " + i, i => TimeSpan.FromSeconds(1))
                // 4番目の要素を取得する
                .ElementAtOrDefault(3)
                // この一連のシーケンスの最後で待機しているスレッドを解放する
                .Finally(() => gate.Set())
                // 購読
                .Subscribe(
                    // 値を表示する（nullの場合はnullと表示する）
                    i => Console.WriteLine("elementAt3: {0}", i ?? "null"),
                    // 例外が発生した場合は例外のメッセージを表示する
                    ex => Console.WriteLine("Exception: {0}, {1}", ex.GetType().Name, ex.Message),
                    // 完了を示すメッセージを表示する
                    () => Console.WriteLine("OnCompleted"));
            // 一連のメソッドチェインが終わった時のタイムスタンプを表示する
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            // gate.Set()が呼ばれるまで停止
            gate.WaitOne();
            // gate.Setが呼ばれた後のタイムスタンプを表示する
            Console.WriteLine("Timestamp {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            Console.WriteLine("-------------------------------");
        }

        static void SingleSample()
        {
            Console.WriteLine("#SingleSample");
            {
                // 実行開始時間を出力
                Console.WriteLine("Start {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
                var singleResult = Observable
                    // 1秒後に1つだけ値を発行する
                    .Generate(1, i => i == 1, i => ++i, i => i, _ => TimeSpan.FromSeconds(1))
                    // 発行された値をダンプ
                    .Do(i => Console.WriteLine("Dump {0:yyyy/MM/dd HH:mm:ss.FFF}, Value = {1}", DateTime.Now, i))
                    // 単一の値を取得する
                    .Single();
                // 結果の出力
                Console.WriteLine("singleResult: {0}", singleResult);
                // 終了時の時間を出力
                Console.WriteLine("End {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            }
            Console.WriteLine("-------------------------------");
            {
                // 実行開始時間を出力
                Console.WriteLine("Start {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
                try
                {
                    var singleResult = Observable
                        // 1秒間隔で2つの値を出力
                        .Generate(0, i => i < 2, i => ++i, i => i, i => TimeSpan.FromSeconds(1))
                        // 発行された値を出力
                        .Do(i => Console.WriteLine("Dump {0:yyyy/MM/dd HH:mm:ss.FFF}, Value = {1}", DateTime.Now, i))
                        // 単一の値を取得する
                        .Single();
                    // 結果を出力
                    Console.WriteLine("singleResult: {0}", singleResult);
                }
                catch (InvalidOperationException ex)
                {
                    // 単一の値を取得しようとしたら2つ以上値が流れてきたので例外になる
                    Console.WriteLine("{0}: {1}", ex.GetType().Name, ex.Message);
                }
                // 終了時の時間を出力
                Console.WriteLine("End {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            }
            Console.WriteLine("-------------------------------");
        }

        static void SinglePredicateSample()
        {
            Console.WriteLine("#SinglePredicateSample");
            {
                // 実行開始時間を出力
                Console.WriteLine("Start {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
                var singleResult = Observable
                    // 1秒間隔で0～4の値を発行する
                    .Generate(
                        0, i => i < 5, i => ++i, i => i, i => TimeSpan.FromSeconds(1))
                    // 発行された値を出力
                    .Do(i => Console.WriteLine("Dump {0:yyyy/MM/dd HH:mm:ss.FFF}, Value = {1}", DateTime.Now, i))
                    // 値が3のものを1つだけ取得したい
                    .Single(i => i == 3);
                // 結果を出力
                Console.WriteLine("singleResult: {0}", singleResult);
                // 終了時の時間を出力
                Console.WriteLine("End {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            }
            Console.WriteLine("-------------------------------");
            {
                // 実行開始時間を出力
                Console.WriteLine("Start {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
                try
                {
                    var singleResult = Observable
                        // 1秒間隔で0～4の値を発行する
                        .Generate(
                            0, i => i < 5, i => ++i, i => i, i => TimeSpan.FromSeconds(1))
                        // 発行された値を出力
                        .Do(i => Console.WriteLine("Dump {0:yyyy/MM/dd HH:mm:ss.FFF}, Value = {1}", DateTime.Now, i))
                        // 値が10より大きいものを1つだけ取得したい
                       .Single(i => i > 10);
                    // 結果を出力
                    Console.WriteLine("singleResult: {0}", singleResult);
                }
                catch (InvalidOperationException ex)
                {
                    // 単一の値を取得しようとしたら1つも値が無かったのでエラー
                    Console.WriteLine("{0}: {1}", ex.GetType().Name, ex.Message);
                }
                // 終了時の時間を出力
                Console.WriteLine("End {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            }
            Console.WriteLine("-------------------------------");
        }

        static void SingleOrDefaultSample()
        {
            Console.WriteLine("#SingleOrDefaultSample");
            {
                // 実行開始時間を出力
                Console.WriteLine("Start {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
                var singleResult = Observable
                    // 1秒間隔で0～4の値を発行する
                    .Generate(
                        0, i => i < 5, i => ++i, i => i, i => TimeSpan.FromSeconds(1))
                    // デフォルト値をnullにしたいのでstring型に変換
                    .Select(i => i.ToString())
                    // 発行された値を出力
                    .Do(i => Console.WriteLine("Dump {0:yyyy/MM/dd HH:mm:ss.FFF}, Value = {1}", DateTime.Now, i))
                    // 値が”3"のものを1つだけ取得したい
                    .SingleOrDefault(i => i == "3");
                // 結果を出力
                Console.WriteLine("singleResult: {0}", singleResult ?? "null");
                // 終了時の時間を出力
                Console.WriteLine("End {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            }
            Console.WriteLine("-------------------------------");
            {
                // 空のIObservableシーケンス
                var s = new Subject<string>();
                s.OnCompleted();

                // 1つも値が取得できない場合はnullが返る
                var singleResult = s.SingleOrDefault();
                // 結果を出力
                Console.WriteLine("singleResult: {0}", singleResult ?? "null");
            }
            Console.WriteLine("-------------------------------");
            {
                // 実行開始時間を出力
                Console.WriteLine("Start {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
                try
                {
                    var singleResult = Observable
                        // 1秒間隔で0～4の値を発行する
                        .Generate(
                            0, i => i < 5, i => ++i, i => i, i => TimeSpan.FromSeconds(1))
                        // デフォルト値をnullにしたいのでstring型に変換
                        .Select(i => i.ToString())
                        // 発行された値を出力
                        .Do(i => Console.WriteLine("Dump {0:yyyy/MM/dd HH:mm:ss.FFF}, Value = {1}", DateTime.Now, i))
                        // 値を1つだけ取得したい
                        .SingleOrDefault();
                    // 結果を出力
                    Console.WriteLine("singleResult: {0}", singleResult ?? "null");
                }
                catch (InvalidOperationException ex)
                {
                    // SingleOrDefaultメソッドを使っても複数の値が取得できてしまうケースでは例外になる
                    Console.WriteLine("{0}: {1}", ex.GetType().Name, ex.Message);
                }
                // 終了時の時間を出力
                Console.WriteLine("End {0:yyyy/MM/dd HH:mm:ss.FFF}", DateTime.Now);
            }
            Console.WriteLine("-------------------------------");
        }

    }
}
