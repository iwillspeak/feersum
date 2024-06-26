using System;
using System.Collections.Generic;

namespace Serehfa.Attributes;

/// <summary>Attribute to mark scheme libraries.</summary>
[AttributeUsage(AttributeTargets.Class)]
public sealed class LispLibraryAttribute : Attribute
{
    /// <summary>
    /// Initialise an instance of <see cref="LispLibraryAttribute" />
    /// </summary>
    /// <param name="name">The library name.</param>
    public LispLibraryAttribute(params string[] name)
    {
        Name = name;
    }

    /// <summary>The library's name</summary>
    public IReadOnlyCollection<string> Name { get; }
}
