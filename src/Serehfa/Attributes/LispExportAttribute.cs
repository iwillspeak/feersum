using System;

namespace Serehfa.Attributes
{
    [AttributeUsage(AttributeTargets.Method)]
    public class LispExportAttribute : Attribute
    {
        public LispExportAttribute(string name)
        {
            Name = name;
        }

        public string Name { get; }
    }
}
