namespace Feersum.CompilerServices.Binding

open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Binding.Stx
open Feersum.CompilerServices.Utils

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
/// A library is made up of a collection of library declarations
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
module private Utils =

    // Map the exports in a given library signature
    let mapExports mapper (signature: LibrarySignature<'a>) =
        { signature with
            Exports = signature.Exports |> mapper }

    // Recognise a list of strings or numbers as a library name
    let parseLibraryName (diags: DiagnosticBag) (stx: Stx) : Result<string list, unit> =
        let isInvalidChar =
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

        let parseElement stx =
            match stx with
            | StxDatum(StxDatum.Number n, _) -> Ok(sprintf "%g" n)
            | StxId(name, loc, _) ->
                if Seq.exists isInvalidChar name then
                    diags.Emit
                        LibraryDiagnostics.improperLibraryName
                        loc.Location
                        "Library names should not contain complex characters"

                Ok name
            | other ->
                diags.Emit LibraryDiagnostics.invalidLibraryName other.Pos.Location "Invalid library name part"
                Result.Error()

        match stx with
        | StxList(parts, None, _, _) -> parts |> List.map parseElement |> Result.collect
        | other ->
            diags.Emit LibraryDiagnostics.invalidLibraryName other.Pos.Location "Expected library name"
            Result.Error()

    let private tryParseRename (stx: Stx list) =
        match stx with
        | [ Stx.Id(interior, _); Stx.Id(exterior, _) ] -> Some { From = interior; To = exterior }
        | _ -> None

    let private parseExportDeclaration (diags: DiagnosticBag) (export: Stx) : ExportSet option =
        match export with
        | StxId(name, _, _) -> Some(ExportSet.Plain name)
        | StxList(Stx.Id("rename", _) :: rename, None, _, _) ->
            match tryParseRename rename with
            | Some renamed -> Some(ExportSet.Renamed renamed)
            | None ->
                diags.Emit LibraryDiagnostics.malformedLibraryDecl export.Pos.Location "Invalid rename"
                None
        | other ->
            diags.Emit LibraryDiagnostics.malformedLibraryDecl other.Pos.Location "Invalid export element"
            None

    let rec parseImportSet (diags: DiagnosticBag) (stx: Stx) : ImportSet =
        let getName =
            function
            | Stx.Id(n, _) -> Some n
            | _ -> None

        let getNames items = List.choose getName items

        match stx with
        | StxList(Stx.Id("only", _) :: fromSet :: filters, _, _, _) ->
            ImportSet.Only(parseImportSet diags fromSet, getNames filters)
        | StxList(Stx.Id("except", _) :: fromSet :: filters, _, _, _) ->
            ImportSet.Except(parseImportSet diags fromSet, getNames filters)
        | StxList(Stx.Id("rename", _) :: fromSet :: renames, _, _, _) ->
            let parseRenames (renames: Stx list) =
                let parseRename (node: Stx) =
                    match node with
                    | StxList(x, None, _, _) ->
                        match tryParseRename x with
                        | Some rename -> Some rename
                        | None ->
                            diags.Emit LibraryDiagnostics.malformedLibraryDecl node.Pos.Location "Invalid rename"
                            None
                    | _ ->
                        diags.Emit LibraryDiagnostics.malformedLibraryDecl node.Pos.Location "Invalid rename"
                        None

                renames |> List.choose parseRename

            ImportSet.Renamed(parseImportSet diags fromSet, parseRenames renames)
        | StxList([ Stx.Id("prefix", _); fromSet; Stx.Id(prefix, _) ], _, _, _) ->
            ImportSet.Prefix(parseImportSet diags fromSet, prefix)
        | _ ->
            match parseLibraryName diags stx with
            | Ok name -> ImportSet.Plain name
            | Result.Error _ -> ImportSet.Error

    and parseLibraryDeclaration (diags: DiagnosticBag) (decl: Stx) : LibraryDeclaration =
        match decl with
        | StxList(Stx.Id("export", _) :: exports, _, _, _) ->
            exports
            |> List.choose (parseExportDeclaration diags)
            |> LibraryDeclaration.Export
        | StxList(Stx.Id("import", _) :: imports, _, _, _) ->
            imports |> List.map (parseImportSet diags) |> LibraryDeclaration.Import
        | StxList(Stx.Id("begin", _) :: body, _, _, _) -> LibraryDeclaration.Begin body
        | StxList(Stx.Id(kw, _) :: _, _, loc, _) ->
            diags.Emit
                LibraryDiagnostics.malformedLibraryDecl
                loc.Location
                (sprintf "Unrecognised library declaration %s" kw)

            LibraryDeclaration.Error
        | other ->
            diags.Emit LibraryDiagnostics.malformedLibraryDecl other.Pos.Location "Expected library declaration"

            LibraryDeclaration.Error

    let parseLibraryBody (diags: DiagnosticBag) name body =
        { LibraryName = name
          Declarations = body |> List.map (parseLibraryDeclaration diags) }

// -- Public API ---------------------------------------------------------------

module Libraries =

    /// Prettify a library name for printing
    let prettifyLibraryName name =
        String.concat " " name |> sprintf "(%s)"

    /// Recursively check a library name matches
    let rec matchLibraryName left right =
        match left, right with
        | l :: lrest, r :: rrest -> if l = r then matchLibraryName lrest rrest else false
        | [], [] -> true
        | _ -> false

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

        let checkReservedNames =
            function
            | start :: rest ->
                if start = "scheme" || start = "srfi" then
                    start
                    |> sprintf "The name prefix %s is reserved"
                    |> Diagnostic.Create LibraryDiagnostics.improperLibraryName name.Pos.Location
                    |> diags.Add

                start :: rest
            | [] -> []

        parseLibraryName diags name
        |> Result.map checkReservedNames
        |> Result.map (fun boundName -> parseLibraryBody diags boundName body)
        |> Result.map (fun lib -> (lib, diags.Take))
        |> Result.mapError (fun _ -> diags.Take)
