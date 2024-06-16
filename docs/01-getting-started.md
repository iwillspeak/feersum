# Getting Started with Feersum Scheme

The Feersum Scheme compiler is available as a standalone .NET Tool for batch
compilation and REPL support; and a .NET SDK to integrate with the main .NET
toolchain.

## .NET Tool  - `feersum-scheme`

Install the tool globally with `dotnet tool install -g Feersum`. Once
installed Feersum will be available on the command line as `feersum-scheme`. You
can alais this to `scheme` if you are brave enough to use Feersum as your
_default_ scheme implementation.

Once you have the tool installed you can access a Scheme Read, Eval, Print, Loop
(REPL) by running `feersum-scheme`. Type in scheme expressions to be evaluated
and see their representation printed back to you:

```
Feersum Scheme Compiler - 0.2.6
§> "Hello, Schemer!"
}= "Hello, Schemer!"
§> (+ 1 9)  
}= 10
§> 
```

> note, current limitations prevent REPL expressions from accessing the results
> of previous evaluations. Each expression must be a complete Scheme form on a
> single line.
>
> Check out <https://github.com/iwillspeak/feersum/issues/35> to track progress
> on interactive scripting.

## SDK Projects

Feersum scheme is also available as a .NET SDK to build console tools or class
libraries. The Feersum .NET SDK is distributed as a NuGet package. The easiest
way to get started is with the Feersum template pack:

```
$ dotnet new install Feersum.Templates::*
```

With this installed you can use use the following templates:

 * `dotnet new scm-console` - Create a .NET Console application project.
 * `dotnet new scm-classlib` - Create a .NET Class library project.
 * `dotnet new scmlib` - Create a Scheme library definition.

Check out [the `examples/` folder][example-projects] for more information.

[example-projects]: https://github.com/iwillspeak/feersum/blob/main/examples/

The `scm-console` and `scm-classib` templates create `.scmproj` MSBuild projects
which reference the shared `Feersum.SDK` package:

```xml
<Project Sdk="Feersum.Sdk/0.2.6">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
  </PropertyGroup>

  <ItemGroup>
    <Compile Include="program.scm" />
  </ItemGroup>
  
</Project>
```

Things to note here:

 * The version of Feersum used in the SDK must be pinned in the `Sdk=".."`
   attribute. To upgrade the version of the compiler and toolchain in use bump
   this.
 * Scheme files must be added manually as `<Compile Include=".."/>` items. The
   order of these entries is important. A file can only rely on library
   declarations from an earlier file.
