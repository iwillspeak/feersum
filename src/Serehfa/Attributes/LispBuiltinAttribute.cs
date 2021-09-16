using System;

namespace Serehfa.Attributes
{
    [AttributeUsage(AttributeTargets.Method)]
    public class LispBuiltinAttribute : Attribute
    {
        public LispBuiltinAttribute(string name)
        {
            Name = name;
        }

        public string Name { get; }
    }
}
