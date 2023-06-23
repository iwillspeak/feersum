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
        private static readonly Lazy<Undefined> s_Instance = new();

        /// <summary>Shared instance of the undefined type. References to this
        /// are emitted by the compiler when an undefined value is required.
        /// </summary>
        public static Undefined Instance => s_Instance.Value;

        /// <inheritdoc />
        public override string ToString() => "; Unspecified value";

        /// <summary>
        ///  Instances of these values are never equal to anything else.
        /// </summary>
        public override bool Equals(object obj) => false;

        /// <inheritdoc />
        public override int GetHashCode() => base.GetHashCode();
    }
}
