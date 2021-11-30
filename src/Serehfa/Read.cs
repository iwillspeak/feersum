using System;
using System.Text;
using Serehfa.Attributes;
using System.Runtime.InteropServices;

namespace Serehfa
{
    using static ArgHelpers;

    [LispLibrary("feersum", "serehfa", "read")]
    public static class Read
    {
        [LispExport("read-char")]
        public static object ReadChar(object[] args)
        {
            CheckNoArgs(args);

            var read = Console.In.Read();
            if (read < 0)
            {
                return Eof.Instance;
            }

            return (char)read;
        }

        [LispExport("peek-char")]
        public static object PeekChar(object[] args)
        {
            CheckNoArgs(args);

            var read = Console.In.Peek();
            if (read < 0)
            {
                return Eof.Instance;
            }

            return (char)read;
        }

        [LispExport("read-line")]
        public static object ReadLine(object[] args)
        {
            CheckNoArgs(args);

            var read = Console.In.ReadLine();
            if (read == null)
            {
                return Eof.Instance;
            }

            return read;
        }

        [LispExport("eof-object")]
        public static object MakeEofObject(object[] args)
        {
            CheckNoArgs(args);
            return Eof.Instance;
        }

        [LispExport("eof-object?")]
        public static object IsEofObject(object[] args)
        {
            var toCheck = UnpackArgs<object>(args);
            return Eof.Instance == toCheck;
        }

        [LispExport("char-ready?")]
        public static object CharReady(object[] args)
        {
            CheckNoArgs(args);

            return Console.KeyAvailable;
        }

        [LispExport("read-string")]
        public static object ReadString(object[] args)
        {
            var len = (int)UnpackArgs<double>(args);
            var buff = new char[len];
            var read = Console.In.Read(buff, 0, len);

            if (read == 0)
            {
                return Eof.Instance;
            }

            return new String(buff, 0, read);
        }

        /// <summary>Marker type to rerepresent the end of file</summary>
        private class Eof
        {
            private Eof() { }

            /// <inheritdoc />
            public override string ToString() =>
                "#<eof>";

            /// <summary>Singleton instance of the EOF.</summary>
            public static Eof Instance = new();
        }
    }
}
