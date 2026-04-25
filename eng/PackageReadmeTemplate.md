# __TITLE__

## About

__DESCRIPTION__

## Installation

### As a .NET Tool

```bash
dotnet tool install -g Feersum
```

You can then start the REPL with `feersum-scheme`.

### As an MSBuild SDK

Set the SDK in your `Project`:

```xml
<!-- hello.scmproj -->
<Project Sdk="Feersum.Sdk/__VERSION__">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
  </PropertyGroup>

  <ItemGroup>
    <Compile Include="program.scm" />
  </ItemGroup>
  
</Project>
```

Or drop the `/__VERSION__` and add the following to your `global.json`:

```json
{
  "msbuild-sdks": {
    "Feersum.Sdk": "__VERSION__"
  }
}
```

### As a Library

Add a package reference to your project:

```bash
dotnet add package Feersum.CompilerServices
```

Or include in your `.csproj` or `.fsproj`:

```xml
<PackageReference Include="__PACKAGEID__" Version="__VERSION__" />
```

## Documentation

- [Getting Started Guide](https://docs.feersum-scheme.net/01-getting-started.html)
- [Language Reference](https://docs.feersum-scheme.net/10-language-reference/)
- [Compiler Reference](https://docs.feersum-scheme.net/20-compiler-reference/)

## Links

- **GitHub**: https://github.com/Feersum/feersum
- **NuGet**: https://www.nuget.org/packages/__PACKAGEID__/
- **Documentation**: https://docs.feersum-scheme.net/

## License

MIT License - see LICENSE file in the repository for details.
