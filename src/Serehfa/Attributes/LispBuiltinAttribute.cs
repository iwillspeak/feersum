using System;

namespace Serehfa.Attributes
{
    /// <summary>Attribute to mark a lisp built-in element</summary>
    [AttributeUsage(AttributeTargets.Method)]
    public class LispBuiltinAttribute : Attribute
    {
        /// <summary>
        /// Initialise an instance of <see cref="LispBuiltinAttribute" />
        /// with the given external <paramref param="name" />.
        /// </summary>
        public LispBuiltinAttribute(string name)
        {
            Name = name;
        }

        /// <summary>Gets the exported name of this builtin.</summary>
        public string Name { get; }
    }
}
