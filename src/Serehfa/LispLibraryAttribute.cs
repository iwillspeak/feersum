using System;
using System.Collections.Generic;

namespace Serehfa
{
    /// <summary>Attribute to mark scheme libraries</summary>
    [AttributeUsage(AttributeTargets.Class)]
    public sealed class LispLibraryAttribute : Attribute
    {
        /// <summary>Initialise an instance of <see cref="LispLibraryAttribute" /></summary>
        /// <param name="name">The library name.</param>
        public LispLibraryAttribute(string[] name)
        {
            Name = name;
        }

        /// <summary>The libraries name</summary>

        public IReadOnlyCollection<string> Name { get; }
    }
}