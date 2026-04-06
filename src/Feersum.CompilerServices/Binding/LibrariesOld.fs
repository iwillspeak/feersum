namespace Feersum.CompilerServices.Binding

open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax.Tree
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

/// A single export from a library declration
type ExportSet =
    | Plain of string
    | Renamed of SymbolRename

/// A single import from a library declration
type ImportSet =
    | Plain of string list
    | Only of ImportSet * string list
    | Except of ImportSet * string list
    | Prefix of ImportSet * string
    | Renamed of ImportSet * SymbolRename list
    | Error

/// Library Declarations
///
/// A library is made up of a collection of library declarations
type LibraryDeclaration =
    | Export of ExportSet list
    | Import of ImportSet list
    | Begin of Expression list
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
    /// Map the exports in a given library signature
    let mapExports mapper signature =
        { signature with
            Exports = signature.Exports |> mapper }

    /// Resolve a node location using the source registry
    let private nodeLocation (registry: SourceRegistry) (expr: Expression) =
        SourceRegistry.resolveLocation registry expr.DocId expr.SyntaxRange

    /// Recognise a list of strings as a library name
    let parseLibraryName (diags: DiagnosticBag) (registry: SourceRegistry) (name: Expression) =
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

        let parseNameElement (element: Expression) =
            let loc () = nodeLocation registry element

            match element with
            | Constant(Some(NumVal n)) -> Ok(n |> sprintf "%g")
            | Symbol namePart ->
                if Seq.exists (isInvalidChar) (namePart.ToCharArray()) then
                    "Library names should not contain complex characters"
                    |> Diagnostic.Create LibraryDiagnostics.improperLibraryName (loc ())
                    |> diags.Add

                Ok(namePart)
            | _ ->
                diags.Emit LibraryDiagnostics.invalidLibraryName (loc ()) "Invalid library name part"
                Result.Error(())

        match name with
        | Form x -> x |> List.map (parseNameElement) |> Result.collect
        | _ ->
            diags.Emit LibraryDiagnostics.invalidLibraryName (nodeLocation registry name) "Expected library name"
            Result.Error(())

    /// Try and parse a node as an identifier
    let parseIdentifier (registry: SourceRegistry) (node: Expression) =
        match node with
        | Symbol id -> Ok id
        | _ ->
            Result.Error(
                Diagnostic.Create
                    LibraryDiagnostics.invalidLibraryName
                    (nodeLocation registry node)
                    "Expected identifier"
            )

    /// Parse a list of identifiers into a list of strings
    let parseIdentifierList registry idents =
        idents |> List.map (parseIdentifier registry) |> Result.collect

    /// Parse a library declaration form
    let rec parseLibraryDeclaration (diags: DiagnosticBag) (registry: SourceRegistry) (declaration: Expression) =
        match declaration with
        | Form(Symbol special :: body) ->
            parseLibraryDeclarationForm diags registry (nodeLocation registry declaration) special body
        | _ ->
            diags.Emit
                LibraryDiagnostics.malformedLibraryDecl
                (nodeLocation registry declaration)
                "Expected library declaration"

            LibraryDeclaration.Error

    and parseLibraryDeclarationForm diags registry position special body =
        match special with
        | "export" ->
            body
            |> List.choose (parseExportDeclaration diags registry)
            |> LibraryDeclaration.Export
        | "import" ->
            body
            |> List.map (parseImportDeclaration diags registry)
            |> LibraryDeclaration.Import
        | "begin" -> LibraryDeclaration.Begin body
        | s ->
            sprintf "Unrecognised library declaration %s" s
            |> diags.Emit LibraryDiagnostics.malformedLibraryDecl position

            LibraryDeclaration.Error

    and tryParseRename (rename: Expression list) =
        match rename with
        | [ Symbol int; Symbol ext ] -> Ok({ From = int; To = ext })
        | _ -> Result.Error("invalid rename")

    and parseExportDeclaration (diags: DiagnosticBag) (registry: SourceRegistry) (export: Expression) =
        let loc () = nodeLocation registry export

        match export with
        | Symbol plain -> Some(ExportSet.Plain plain)
        | Form(Symbol "rename" :: rename) ->
            match tryParseRename rename with
            | Ok renamed -> Some(ExportSet.Renamed renamed)
            | Result.Error e ->
                diags.Emit LibraryDiagnostics.malformedLibraryDecl (loc ()) e
                None
        | _ ->
            diags.Emit LibraryDiagnostics.malformedLibraryDecl (loc ()) "Invalid export element"
            None

    and parseImportDeclaration (diags: DiagnosticBag) (registry: SourceRegistry) (import: Expression) =
        let parseImportForm parser fromSet body resultCollector =
            match parser body with
            | Ok(bound) -> resultCollector (parseImportDeclaration diags registry fromSet, bound)
            | Result.Error(diag) ->
                diags.Add(diag)
                ImportSet.Error

        match import with
        | Form(Symbol "only" :: fromSet :: filters) ->
            parseImportForm (parseIdentifierList registry) fromSet filters ImportSet.Only
        | Form(Symbol "except" :: fromSet :: filters) ->
            parseImportForm (parseIdentifierList registry) fromSet filters ImportSet.Except
        | Form(Symbol "rename" :: fromSet :: renames) ->
            let parseRenames (renames: Expression list) =
                let parseRename (node: Expression) =
                    let loc () = nodeLocation registry node

                    match node with
                    | Form f ->
                        tryParseRename f
                        |> Result.mapError (fun e ->
                            Diagnostic.Create LibraryDiagnostics.malformedLibraryDecl (loc ()) e)
                    | _ ->
                        Result.Error(
                            Diagnostic.Create LibraryDiagnostics.malformedLibraryDecl (loc ()) "Expected rename"
                        )

                renames |> List.map parseRename |> Result.collect

            parseImportForm (parseRenames) fromSet renames ImportSet.Renamed
        | Form [ Symbol "prefix"; fromSet; prefix ] ->
            parseImportForm (parseIdentifier registry) fromSet prefix ImportSet.Prefix
        | _ ->
            match parseLibraryName diags registry import with
            | Ok(name) -> ImportSet.Plain name
            | Result.Error _ -> ImportSet.Error


    let parseLibraryBody (diags: DiagnosticBag) (registry: SourceRegistry) name body =
        { LibraryName = name
          Declarations = body |> List.map (parseLibraryDeclaration diags registry) }

// -------------------- Public Libraries API ---------------------

module Libraries =

    /// Prettify a library name for printing
    let prettifyLibraryName name =
        String.concat " " name |> sprintf "(%s)"

    /// Recursively check a library name matches
    let rec matchLibraryName left right =
        match (left, right) with
        | (l :: lrest, r :: rrest) -> if l = r then matchLibraryName lrest rrest else false
        | ([], []) -> true
        | _ -> false

    /// Resolve a library import
    let rec resolveImport libraries =
        function
        | ImportSet.Error -> Result.Error "invalid import set"
        | ImportSet.Except(inner, except) ->
            resolveImport libraries inner
            |> Result.map (mapExports (List.filter (fun (id, _) -> List.contains id except |> not)))
        | ImportSet.Only(inner, only) ->
            resolveImport libraries inner
            |> Result.map (mapExports (List.filter (fun (id, _) -> List.contains id only)))
        | ImportSet.Plain lib ->
            match Seq.tryFind (fun signature -> matchLibraryName lib signature.LibraryName) libraries with
            | Some(signature) -> Ok(signature)
            | _ ->
                lib
                |> prettifyLibraryName
                |> sprintf "Could not find library %s"
                |> Result.Error
        | ImportSet.Prefix(inner, prefix) ->
            resolveImport libraries inner
            |> Result.map (mapExports (List.map (fun (name, storage) -> (prefix + name, storage))))
        | ImportSet.Renamed(inner, renames) ->
            let processRenames (name, storage) =
                match List.tryFind (fun (x: SymbolRename) -> x.From = name) renames with
                | Some rename -> (rename.To, storage)
                | _ -> (name, storage)

            resolveImport libraries inner
            |> Result.map (mapExports (List.map (processRenames)))

    /// Parse the body of an import form
    let parseImport = parseImportDeclaration

    /// Parse a `(define-library ...)` form
    let parseLibraryDefinition (registry: SourceRegistry) (name: Expression) (body: Expression list) =
        let diags = DiagnosticBag.Empty

        let nameLoc = SourceRegistry.resolveLocation registry name.DocId name.SyntaxRange

        let checkReservedNames =
            function
            | start :: rest ->
                if start = "scheme" || start = "srfi" then
                    start
                    |> sprintf "The name prefix %s is reserved"
                    |> Diagnostic.Create LibraryDiagnostics.improperLibraryName nameLoc
                    |> diags.Add

                start :: rest
            | [] -> []

        parseLibraryName diags registry name
        |> Result.map checkReservedNames
        |> Result.map (fun boundName -> parseLibraryBody diags registry boundName body)
        |> Result.map (fun lib -> (lib, diags.Take))
        |> Result.mapError (fun _ -> diags.Take)
