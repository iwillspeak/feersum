using System;

namespace EnvironmentTest
{
    class Environment
    {
        public readonly Environment _parent;
        public readonly int[] _slots;

        public Environment(Environment parent, int size)
        {
            _parent = parent;
            _slots = new int[size];
        }

        public int Lambda()
        {
            _slots[0] = _slots[0] + 1;
            return _slots[0];
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var counter = new Func<int, Func<int>>(Counter);
            
            var ten = counter(10);
            var ton = counter(100);

            Console.WriteLine(ten());
            Console.WriteLine(ton());
        }
        
        public static Func<int> Counter(int n)
        {
            var env = new Environment(null, 1);
            env._slots[0] = n;
            return new Func<int>(env.Lambda);
        }
    }
}
