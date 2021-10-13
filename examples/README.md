# Example Projects

This folder contains a series of example projects that demonstrate using the
`Feersum.Sdk` MSBuild SDK to compile Scheme code. Each project is an SDK-style
project using `Sdk="Feersum.Sdk"`.

Each project uses the extension `.scmproj`. The SDK then allows scheme code to
be compiled by registering `<Compile Include="..." />` items. Support for
`ProjectReference`, `PackageReference` and other .NET SDK items is provided by
the base `Microsoft.NET.Sdk`. Projects can be built, run, and packed with the
`dontet` CLI as usual.

## Hello World

The hello world project is a stand-alone .NET application that can be built into
a .NET tool package.

## Multi File

The multi file project shows how library exports from one file in a project can
be consumed by another file. In this example the ordering of the `<Compile ..>`
items in the project matters:

```xml
  <ItemGroup>
    <Compile Include="lib.scm" />
    <Compile Include="main.scm" />
  </ItemGroup>
```

This ensures that the library defined in `lib.scm` is exposed to `main.scm`.

## Multi Project

This project demonstrates a cross-project dependency. The output of this program
depends on the library defined in **Multi File**.
