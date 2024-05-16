using System;
using System.Threading.Tasks;

namespace cc
{
    class MainClass
    {
        public static async Task Main(string[] args)
        {
            Console.WriteLine("main start");
            await AsyncMethod();
            Console.WriteLine("main finish!");
        }

        static async Task AsyncMethod() {
            Console.WriteLine("async start");
            var result = await MyMethod();
            Console.WriteLine("async finish");
        }

        static async Task<int> MyMethod() {
            for (int i = 0; i < 5; i++) {
                Console.WriteLine(i.ToString());
                await Task.Delay(1000);
            }
            return 0;
        }
    }
}

/*
main start
async start
0
1
2
3
4
async finish
main finish!
*/

