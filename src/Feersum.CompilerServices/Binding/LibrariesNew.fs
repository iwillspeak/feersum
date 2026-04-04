namespace Feersum.CompilerServices.Binding.New

open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Binding

module private LibraryDiagnostics =

    let improperLibraryName =
        DiagnosticKind.Create DiagnosticLevel.Warning 20 "Improper library name"

    let invalidLibraryName =
        DiagnosticKind.Create DiagnosticLevel.Error 21 "Invalid library name"

    let malformedLibraryDecl =
        DiagnosticKind.Create DiagnosticLevel.Error 22 "Malformed library declaration"


/// The rename of a single element exported or imported from a library
type SymbolRename = { From: string; To: string }

/// A single export from a library declaration
type ExportSet =
    | Plain of string
    | Renamed of SymbolRename

/// A single import set from a library declaration
type ImportSet =
    | Plain of string list
    | Only of ImportSet * string list
    | Except of ImportSet * string list
    | Prefix of ImportSet * string
    | Renamed of ImportSet * SymbolRename list
    | Error

/// Library Declarations.
///
/// Unlike the CST-based type, `Begin` carries `Stx` nodes instead of
/// `Expression` nodes.
type LibraryDeclaration =
    | Export of ExportSet list
    | Import of ImportSet list
    | Begin of Stx list
    | Error

/// Library Definition
///
/// Represents the parsed body of a `(define-library)` form.
type LibraryDefinition =
    { LibraryName: string list
      Declarations: LibraryDeclaration list }

/// Exported API signature of a given library
type LibrarySignature<'a> =
    { LibraryName: string list
      Exports: (string * 'a) list }

[<AutoOpen>]
module Utils =

    // ── Private helpers ───────────────────────────────────────────────────

    let private isInvalidChar =
        function
        | '|'
        | '\\'
        | '?'
        | '*'
        | '<'
        | '"'
        | ':'
        | '>'
        | '+'
        | '['
        | ']'
        | '/'
        | '\'' -> true
        | _ -> false

    let rec private parseLibraryName (diags: DiagnosticBag) (stx: Stx) : string list option =
        let parseElement =
            function
            | Stx.Id(name, loc) ->
                if Seq.exists isInvalidChar name then
                    diags.Emit LibraryDiagnostics.improperLibraryName
                        loc
                        "Library names should not contain complex characters"

                Some name
            | Stx.Datum(StxDatum.Number n, _) -> Some(string (int n))
            | other ->
                diags.Emit LibraryDiagnostics.invalidLibraryName other.Loc "Invalid library name part"
                None

        match stx with
        | Stx.Closure(inner, _, _) -> parseLibraryName diags inner
        | Stx.List(parts, _, _) ->
            let parsed = List.map parseElement parts

            if List.forall Option.isSome parsed then
                List.choose id parsed |> Some
            else
                None
        | other ->
            diags.Emit LibraryDiagnostics.invalidLibraryName other.Loc "Expected library name"
            None

    let private tryParseRename =
        function
        | Stx.List([ Stx.Id(from_, _); Stx.Id(to_, _) ], _, _) ->
            Some { From = from_; To = to_ }
        | _ -> None

    let rec private parseExportDeclaration (diags: DiagnosticBag) (export: Stx) : ExportSet option =
        match export with
        | Stx.Id(name, _) -> Some(ExportSet.Plain name)
        | Stx.Closure(inner, _, _) -> parseExportDeclaration diags inner
        | Stx.List([ Stx.Id("rename", _); Stx.Id(from_, _); Stx.Id(to_, _) ], _, _) ->
            Some(ExportSet.Renamed { From = from_; To = to_ })
        | other ->
            diags.Emit LibraryDiagnostics.malformedLibraryDecl other.Loc "Invalid export element"
            None

    let rec private parseImportSet (diags: DiagnosticBag) (stx: Stx) : ImportSet =
        let getName =
            function
            | Stx.Id(n, _) -> Some n
            | _ -> None

        let getNames items = List.choose getName items

        match stx with
        | Stx.Closure(inner, _, _) -> parseImportSet diags inner
        | Stx.List(Stx.Id("only", _) :: fromSet :: filters, _, _) ->
            ImportSet.Only(parseImportSet diags fromSet, getNames filters)
        | Stx.List(Stx.Id("except", _) :: fromSet :: filters, _, _) ->
            ImportSet.Except(parseImportSet diags fromSet, getNames filters)
        | Stx.List([ Stx.Id("prefix", _); fromSet; Stx.Id(prefix, _) ], _, _) ->
            ImportSet.Prefix(parseImportSet diags fromSet, prefix)
        | Stx.List(Stx.Id("rename", _) :: fromSet :: renames, _, _) ->
            let parsed =
                renames
                |> List.choose (fun r ->
                    match tryParseRename r with
                    | Some rename -> Some rename
                    | None ->
                        diags.Emit LibraryDiagnostics.malformedLibraryDecl r.Loc "Invalid rename"
                        None)

            ImportSet.Renamed(parseImportSet diags fromSet, parsed)
        | _ ->
            match parseLibraryName diags stx with
            | Some name -> ImportSet.Plain name
            | None -> ImportSet.Error

    and private parseLibraryDeclaration (diags: DiagnosticBag) (decl: Stx) : LibraryDeclaration =
        match decl with
        | Stx.Closure(inner, _, _) -> parseLibraryDeclaration diags inner
        | Stx.List(Stx.Id("export", _) :: exports, _, _) ->
            exports |> List.choose (parseExportDeclaration diags) |> LibraryDeclaration.Export
        | Stx.List(Stx.Id("import", _) :: imports, _, _) ->
            imports |> List.map (parseImportSet diags) |> LibraryDeclaration.Import
        | Stx.List(Stx.Id("begin", _) :: body, _, _) -> LibraryDeclaration.Begin body
        | Stx.List(Stx.Id(kw, _) :: _, _, loc) ->
            diags.Emit
                LibraryDiagnostics.malformedLibraryDecl
                loc
                (sprintf "Unrecognised library declaration %s" kw)

            LibraryDeclaration.Error
        | other ->
            diags.Emit
                LibraryDiagnostics.malformedLibraryDecl
                other.Loc
                "Expected library declaration"

            LibraryDeclaration.Error

    // ── Public API ────────────────────────────────────────────────────────

    /// Prettify a library name for printing
    let prettifyLibraryName name =
        String.concat " " name |> sprintf "(%s)"

    /// Recursively check a library name matches
    let rec matchLibraryName left right =
        match left, right with
        | l :: lrest, r :: rrest -> if l = r then matchLibraryName lrest rrest else false
        | [], [] -> true
        | _ -> false

    let private mapExports mapper (signature: LibrarySignature<'a>) =
        { signature with Exports = signature.Exports |> mapper }

    /// Resolve a library import
    let rec resolveImport libraries =
        function
        | ImportSet.Error -> Result.Error "invalid import set"
        | ImportSet.Except(inner, except) ->
            resolveImport libraries inner
            |> Result.map (mapExports (List.filter (fun (id, _) -> not (List.contains id except))))
        | ImportSet.Only(inner, only) ->
            resolveImport libraries inner
            |> Result.map (mapExports (List.filter (fun (id, _) -> List.contains id only)))
        | ImportSet.Plain lib ->
            match Seq.tryFind (fun signature -> matchLibraryName lib signature.LibraryName) libraries with
            | Some signature -> Ok signature
            | None ->
                lib
                |> prettifyLibraryName
                |> sprintf "Could not find library %s"
                |> Result.Error
        | ImportSet.Prefix(inner, prefix) ->
            resolveImport libraries inner
            |> Result.map (mapExports (List.map (fun (name, storage) -> (prefix + name, storage))))
        | ImportSet.Renamed(inner, renames) ->
            let processRenames (name, storage) =
                match List.tryFind (fun (r: SymbolRename) -> r.From = name) renames with
                | Some rename -> (rename.To, storage)
                | _ -> (name, storage)

            resolveImport libraries inner
            |> Result.map (mapExports (List.map processRenames))

    /// Parse the body of an import form
    let parseImport = parseImportSet

    /// Parse a `(define-library ...)` form
    let parseLibraryDefinition (name: Stx) (body: Stx list) =
        let diags = DiagnosticBag.Empty

        match parseLibraryName diags name with
        | None -> Result.Error(diags.Take)
        | Some libraryName ->
            match libraryName with
            | start :: _ when start = "scheme" || start = "srfi" ->
                diags.Emit
                    LibraryDiagnostics.improperLibraryName
                    name.Loc
                    (sprintf "The name prefix %s is reserved" start)
            | _ -> ()

            let decls = body |> List.map (parseLibraryDeclaration diags)
            Ok({ LibraryName = libraryName; Declarations = decls }, diags.Take)
