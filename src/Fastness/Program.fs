// Learn more about F# at http://fsharp.org

open System
open Mono.Cecil
open Mono.Cecil.Cil
open System.IO

[<EntryPoint>]
let main argv =
    let name = AssemblyNameDefinition("test", Version(0, 1, 0))
    let assm = AssemblyDefinition.CreateAssembly(name, "test.exe", ModuleKind.Console)

    let strTy = assm.MainModule.ImportReference(typeof<string>)
    let voidTy = assm.MainModule.ImportReference(typeof<Void>)
    let consoleWrite = assm.MainModule.ImportReference(typeof<Console>.GetMethod("WriteLine", [| typeof<string> |]))

    let mainMethod = MethodDefinition("Main", MethodAttributes.Public ||| MethodAttributes.Static, assm.MainModule.TypeSystem.Void)
    let il = mainMethod.Body.GetILProcessor()
    il.Emit(OpCodes.Ldstr, "Hello World!")
    il.Emit(OpCodes.Call, consoleWrite)
    il.Emit(OpCodes.Ret)

    let progTy = TypeDefinition("testing", "Program", TypeAttributes.Class ||| TypeAttributes.Public ||| TypeAttributes.AnsiClass, assm.MainModule.TypeSystem.Object)
    assm.MainModule.Types.Add progTy
    progTy.Methods.Add mainMethod
    assm.EntryPoint <- mainMethod

    assm.Write "test.exe"
    File.WriteAllText("test.runtimeconfig.json", """
    {
      "runtimeOptions": {
        "tfm": "netcoreapp3.0",
        "framework": {
          "name": "Microsoft.NETCore.App",
          "version": "3.0.0"
        }
      }
    }
    """)

    0 // return an integer exit code
