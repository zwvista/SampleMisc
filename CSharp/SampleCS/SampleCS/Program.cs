using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using System.Windows.Forms;

using SamplePad;

namespace SampleCS
{
    static class Program
    {
        /// <summary>
        /// アプリケーションのメイン エントリ ポイントです。
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);

            List<SampleHarness> harnesses = new List<SampleHarness>();

            // LinqSamples:
            var ixsample = new IxSample();
            harnesses.Add(ixsample);

            // LinqSamples:
            var zwsample = new ZWSample();
            harnesses.Add(zwsample);

            using (SampleForm form = new SampleForm("SampleCS", harnesses))
            {
                form.ShowDialog();
            }
        }
    }
}
