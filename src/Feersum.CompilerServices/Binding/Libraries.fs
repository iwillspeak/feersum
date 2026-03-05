namespace Feersum.CompilerServices.Binding

open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Utils

module private LibraryDiagnostics =

    let improperLibraryName = DiagnosticKind.Create Warning 20 "Improper library name"

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

    /// Resolve a node location using an optional TextDocument
    let private nodeLocation (doc: TextDocument option) (expr: Expression) =
        match doc with
        | Some d -> TextDocument.rangeToLocation d expr.SyntaxRange
        | None -> TextLocation.Missing

    /// Recognise a list of strings as a library name
    let parseLibraryName (diags: DiagnosticBag) (doc: TextDocument option) (name: Expression) =
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
            let loc () = nodeLocation doc element

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
            diags.Emit LibraryDiagnostics.invalidLibraryName (nodeLocation doc name) "Expected library name"
            Result.Error(())

    /// Try and parse a node as an identifier
    let parseIdentifier (doc: TextDocument option) (node: Expression) =
        match node with
        | Symbol id -> Ok id
        | _ ->
            Result.Error(
                Diagnostic.Create LibraryDiagnostics.invalidLibraryName (nodeLocation doc node) "Expected identifier"
            )

    /// Parse a list of identifiers into a list of strings
    let parseIdentifierList doc idents =
        idents |> List.map (parseIdentifier doc) |> Result.collect

    /// Parse a library declaration form
    let rec parseLibraryDeclaration (diags: DiagnosticBag) (doc: TextDocument option) (declaration: Expression) =
        match declaration with
        | Form(Symbol special :: body) ->
            parseLibraryDeclarationForm diags doc (nodeLocation doc declaration) special body
        | _ ->
            diags.Emit
                LibraryDiagnostics.malformedLibraryDecl
                (nodeLocation doc declaration)
                "Expected library declaration"

            LibraryDeclaration.Error

    and parseLibraryDeclarationForm diags doc position special body =
        match special with
        | "export" ->
            body
            |> List.choose (parseExportDeclaration diags doc)
            |> LibraryDeclaration.Export
        | "import" -> body |> List.map (parseImportDeclaration diags doc) |> LibraryDeclaration.Import
        | "begin" -> LibraryDeclaration.Begin body
        | s ->
            sprintf "Unrecognised library declaration %s" s
            |> diags.Emit LibraryDiagnostics.malformedLibraryDecl position

            LibraryDeclaration.Error

    and tryParseRename (doc: TextDocument option) (rename: Expression list) =
        match rename with
        | [ Symbol int; Symbol ext ] -> Ok({ From = int; To = ext })
        | _ -> Result.Error("invalid rename")

    and parseExportDeclaration (diags: DiagnosticBag) (doc: TextDocument option) (export: Expression) =
        let loc () = nodeLocation doc export

        match export with
        | Symbol plain -> Some(ExportSet.Plain plain)
        | Form(Symbol "rename" :: rename) ->
            match tryParseRename doc rename with
            | Ok renamed -> Some(ExportSet.Renamed renamed)
            | Result.Error e ->
                diags.Emit LibraryDiagnostics.malformedLibraryDecl (loc ()) e
                None
        | _ ->
            diags.Emit LibraryDiagnostics.malformedLibraryDecl (loc ()) "Invalid export element"
            None

    and parseImportDeclaration (diags: DiagnosticBag) (doc: TextDocument option) (import: Expression) =
        let parseImportForm parser fromSet body resultCollector =
            match parser body with
            | Ok(bound) -> resultCollector (parseImportDeclaration diags doc fromSet, bound)
            | Result.Error(diag) ->
                diags.Add(diag)
                ImportSet.Error

        match import with
        | Form(Symbol "only" :: fromSet :: filters) ->
            parseImportForm (parseIdentifierList doc) fromSet filters ImportSet.Only
        | Form(Symbol "except" :: fromSet :: filters) ->
            parseImportForm (parseIdentifierList doc) fromSet filters ImportSet.Except
        | Form(Symbol "rename" :: fromSet :: renames) ->
            let parseRenames (renames: Expression list) =
                let parseRename (node: Expression) =
                    let loc () = nodeLocation doc node

                    match node with
                    | Form f ->
                        tryParseRename doc f
                        |> Result.mapError (fun e ->
                            Diagnostic.Create LibraryDiagnostics.malformedLibraryDecl (loc ()) e)
                    | _ ->
                        Result.Error(
                            Diagnostic.Create
                                LibraryDiagnostics.malformedLibraryDecl
                                (loc ())
                                "Expected rename"
                        )

                renames |> List.map parseRename |> Result.collect

            parseImportForm (parseRenames) fromSet renames ImportSet.Renamed
        | Form [ Symbol "prefix"; fromSet; prefix ] ->
            parseImportForm (parseIdentifier doc) fromSet prefix ImportSet.Prefix
        | _ ->
            match parseLibraryName diags doc import with
            | Ok(name) -> ImportSet.Plain name
            | Result.Error _ -> ImportSet.Error


    let parseLibraryBody (diags: DiagnosticBag) (doc: TextDocument option) name body =
        { LibraryName = name
          Declarations = body |> List.map (parseLibraryDeclaration diags doc) }

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
    let parseLibraryDefinition (doc: TextDocument option) (name: Expression) (body: Expression list) =
        let diags = DiagnosticBag.Empty

        let nameLoc =
            match doc with
            | Some d -> TextDocument.rangeToLocation d name.SyntaxRange
            | None -> TextLocation.Missing

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

        parseLibraryName diags doc name
        |> Result.map checkReservedNames
        |> Result.map (fun boundName -> parseLibraryBody diags doc boundName body)
        |> Result.map (fun lib -> (lib, diags.Take))
        |> Result.mapError (fun _ -> diags.Take)
