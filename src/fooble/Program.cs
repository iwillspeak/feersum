using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace fooble
{
    class Env
    {
        public Env _outer;
        public object[] _slots;

        public Env(Env outer, int slots)
        {
            _outer = outer;
            _slots = new object[slots];
        }

        public object IterfactAcc(object n, object acc)
        {
			return (n is 0.0d) ? acc : ((Func<object[], object>)_slots[0]).Invoke(new object[2]
			{
				new Func<object[], object>(Program.arithsub).Invoke(new object[2]
				{
					n,
					1.0d
				}),
				new Func<object[], object>(Program.arithmul).Invoke(new object[2]
				{
					acc,
					n
				})
			});
            
        }

        [CompilerGenerated]
        [DebuggerNonUserCode]
        public object IterfactAcc_t(object[] args)
        {
            if (args.LongLength == 2)
            {
                return IterfactAcc(args[0], args[1]);
            }
            throw new Exception("DSFSDF");
        }
    }

    class Program
    {
        static object iterfact;

        private static object Iterfact(object n)
        {
            var env = new Env(null, 1);
            env._slots[0] = new Func<object[], object>(env.IterfactAcc_t);
            return ((Func<object[], object>)env._slots[0]).Invoke(new object[]
            {
                n,
                1.0d,
            });
        }

        [CompilerGenerated]
        [DebuggerNonUserCode]
        private static object Iterfact_t(object[] args)
        {
            if (args.LongLength == 1)
            {
                return Iterfact(args[0]);
            }
            throw new Exception("OH DSD");
        }


		public static object arithmul(object[] P_0)
		{
			double num;
			int i;
			if ((nint)P_0.LongLength > 1)
			{
				num = (double)P_0[0];
				i = 1;
			}
			else
			{
				num = 1.0;
				i = 0;
			}
			for (; i < (nint)P_0.LongLength; i++)
			{
				double num2 = (double)P_0[i];
				num *= num2;
			}
			return num;
		}

		public static object arithsub(object[] P_0)
		{
			double num;
			int i;
			if ((nint)P_0.LongLength > 1)
			{
				num = (double)P_0[0];
				i = 1;
			}
			else
			{
				num = 0.0;
				i = 0;
			}
			for (; i < (nint)P_0.LongLength; i++)
			{
				double num2 = (double)P_0[i];
				num -= num2;
			}
			return num;
		}

        private static object ScriptBody()
        {
            iterfact = new Func<object[], object>(Iterfact_t);
            return ((Func<object[], object>)iterfact).Invoke(new object[] {
                5.0
            });
        }

        [CompilerGenerated]
        [DebuggerNonUserCode]
        static void Main(string[] args)
        {
            Console.WriteLine("{0}", ScriptBody());
        }
    }
}
