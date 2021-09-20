using System;

namespace Serehfa.Attributes
{
    /// <summary>Attribute to mark an exported lisp element</summary>
    [AttributeUsage(AttributeTargets.Method)]
    public class LispExportAttribute : Attribute
    {
        /// <summary>
        /// Initialise an instance of <see cref="LispExportAttribute" />
        /// with the given external <paramref param="name" />.
        /// </summary>
        public LispExportAttribute(string name)
        {
            Name = name;
        }

        /// <summary>Gets the exported name of this builtin.</summary>
        public string Name { get; }
    }
}
