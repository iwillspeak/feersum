using System;

namespace Serehfa
{
    /// <summary>
    ///  The unspecified / undefined value. Instances of this type are returned
    ///  from scheme functions where no specific return value is specified in
    ///  the standard.
    /// </summary>
    public class Undefined
    {
        private static Lazy<Undefined> s_Instance = new Lazy<Undefined>();
        public static Undefined Instance => s_Instance.Value;

        public override string ToString() => "; Unspecified value";

        /// <summary>
        ///  Instances of these values are never equal to anything else.
        /// </summary>
        public override bool Equals(object obj) => false;

        public override int GetHashCode() => base.GetHashCode();
    }
}