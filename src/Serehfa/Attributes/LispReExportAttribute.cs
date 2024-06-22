using System;

namespace Serehfa.Attributes;

/// <summary>Attribute to mark a re-export exported lisp element</summary>
[AttributeUsage(AttributeTargets.Class)]
public class LispReExportAttribute : Attribute
{
    /// <summary>
    /// Initialise an instance of <see cref="LispReExportAttribute" />
    /// with the given external <paramref param="name" />.
    /// </summary>
    public LispReExportAttribute(string externName, Type libraryType, string libraryItem, bool isMethod)
    {
        ExportedName = externName;
        LibraryType = libraryType;
        LibraryItem = libraryItem;
        IsMethod = isMethod;
    }

    /// <summary>Gets the exported name of this builtin.</summary>
    public string ExportedName { get; }

    /// <summary>Gets the library the item is exported from</summary>
    public Type LibraryType { get; }

    /// <summary>Gets the name of the item to re-export</summary>
    public string LibraryItem { get; }
    public bool IsMethod { get; }
}
