module Libraries

open Diagnostics
open Syntax
open Utils

/// The rename of a single element exported or imported from a library
type SymbolRename =
    { From: string
    ; To: string }

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
    | Begin of AstNode list
    | Error

/// Library Definition
/// 
/// Represents the parsed body of a `(define-library)` form.
type LibraryDefinition = 
    { LibraryName: string list
    ; Declarations: LibraryDeclaration list }

/// Exported API signature of a given library
type LibrarySignature<'a> =
    { LibraryName: string list
    ; Exports: (string * 'a) list}

/// Map the exports in a given library signature
let private mapExports mapper signature =
    { signature with Exports = signature.Exports |> mapper }

/// Recognise a list of strings as a library name
let private parseLibraryName (diags: DiagnosticBag) name =
    let isInvalidChar = function
        | '|' | '\\' | '?' | '*'
        | '<' | '"' | ':' | '>'
        | '+' | '[' | ']' | '/' | '\'' -> true
        | _ -> false
    let parseNameElement element =
        match element.Kind with
        | AstNodeKind.Constant(SyntaxConstant.Number(namePart)) ->
            Ok(namePart |> sprintf "%g")
        | AstNodeKind.Ident(namePart) ->
            if Seq.exists (isInvalidChar) (namePart.ToCharArray()) then
                "Library names should not contain complex characters"
                |> Diagnostic.CreateWarning element.Location
                |> diags.Add
            Ok(namePart)
        | _ ->
            diags.Emit element.Location "Invalid library name part"
            Result.Error(())
    match name.Kind with
    | AstNodeKind.Form x ->
        x
        |> List.map (parseNameElement)
        |> ResultEx.collect
    | _ -> 
        diags.Emit name.Location "Expected library name"
        Result.Error(())

/// Try and parse a node as an identifier
let private parseIdentifier = function
    | { Kind = AstNodeKind.Ident(id) } -> Ok(id)
    | node -> Result.Error(Diagnostic.Create node.Location "Expected identifier")

/// Parse a list of identifiers into a list of strings
let private parseIdentifierList idents =
    idents
    |> List.map parseIdentifier
    |> ResultEx.collect

/// Parse a library declaration form
let rec private parseLibraryDeclaration (diags: DiagnosticBag) declaration =
    match declaration.Kind with
    | AstNodeKind.Form({ Kind = AstNodeKind.Ident(special)}::body) ->
        parseLibraryDeclarationForm diags declaration.Location special body
    | _ ->
        diags.Emit declaration.Location "Expected library declaration"
        LibraryDeclaration.Error

and private parseLibraryDeclarationForm diags position special body =
    match special with
    | "export"  ->
        body
        |> List.choose (parseExportDeclaration diags)
        |> LibraryDeclaration.Export
    | "import" ->
        body
        |> List.map (parseImportDeclaration diags)
        |> LibraryDeclaration.Import
    | "begin" ->
        LibraryDeclaration.Begin body
    | s ->
        sprintf "Unrecognised library declaration %s" s
        |> diags.Emit position
        LibraryDeclaration.Error

and private tryParseRename rename =
    match rename with
    | [{ Kind = AstNodeKind.Ident(int) };{ Kind = AstNodeKind.Ident(ext) }] ->
        Ok({From = int; To = ext})
    | _ -> Result.Error("invalid rename")

and private parseExportDeclaration diags export =
    match export.Kind with
    | AstNodeKind.Ident(plain) -> Some(ExportSet.Plain plain)
    | AstNodeKind.Form({ Kind = AstNodeKind.Ident("rename")}::rename) ->
        match rename |> tryParseRename with
        | Ok renamed -> Some(ExportSet.Renamed renamed)
        | Result.Error e ->
            diags.Emit export.Location e
            None
    | _ ->
        diags.Emit export.Location "Invalid export element"
        None

and private parseImportDeclaration diags import =
    let parseImportForm parser fromSet body resultCollector =
        match parser body with
        | Ok(bound) ->
            resultCollector(parseImportDeclaration diags fromSet, bound)
        | Result.Error(diag) ->
            diags.Add(diag)
            ImportSet.Error

    match import.Kind with
    | AstNodeKind.Form({ Kind = AstNodeKind.Ident("only")}::(fromSet::filters)) ->
        parseImportForm (parseIdentifierList) fromSet filters ImportSet.Only
    | AstNodeKind.Form({ Kind = AstNodeKind.Ident("except")}::(fromSet::filters)) ->
        parseImportForm (parseIdentifierList) fromSet filters ImportSet.Except
    | AstNodeKind.Form({ Kind = AstNodeKind.Ident("rename")}::(fromSet::renames)) ->
        let parseRenames renames =
            let parseRename node =
                match node with
                | { Kind = AstNodeKind.Form(f) } ->
                    tryParseRename(f)
                    |> Result.mapError (Diagnostic.Create node.Location)
                | _ -> Result.Error(Diagnostic.Create node.Location "Expected rename")
            renames
            |> List.map parseRename
            |> ResultEx.collect
        
        parseImportForm (parseRenames) fromSet renames ImportSet.Renamed
    | AstNodeKind.Form([{ Kind = AstNodeKind.Ident("prefix")};fromSet;prefix]) ->
        parseImportForm (parseIdentifier) fromSet prefix ImportSet.Prefix
    | _ -> 
        match parseLibraryName diags import with
        | Ok(name) -> ImportSet.Plain name
        | Result.Error _ -> ImportSet.Error


let private parseLibraryBody ctx name body =
    { LibraryName = name
    ; Declarations = body |> List.map (parseLibraryDeclaration ctx) }

// -------------------- Public Libraries API ---------------------

/// Prettify a library name for printing
let public prettifyLibraryName name =
    String.concat " " name |> sprintf "(%s)"

/// Recursively check a library name matches
let rec public matchLibraryName left right =
    match (left, right) with
    | (l::lrest, r::rrest) ->
        if l = r then
            matchLibraryName lrest rrest
        else
            false
    | ([], []) -> true
    | _ -> false

/// Resolve a library import
let rec public resolveImport libraries = function
    | ImportSet.Error -> Result.Error "invalid import set"
    | ImportSet.Except(inner, except) ->
        resolveImport libraries inner
        |> Result.map (mapExports (List.filter (fun (id, _) -> List.contains id except |> not)))
    | ImportSet.Only(inner, only) ->
        resolveImport libraries inner
        |> Result.map (mapExports (List.filter (fun (id, _) -> List.contains id only)))
    | ImportSet.Plain lib ->
        match Seq.tryFind (fun signature -> matchLibraryName lib signature.LibraryName) libraries with
        | Some (signature) -> Ok(signature)
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
let public parseImport = parseImportDeclaration

/// Parse a `(define-library ...)` form
let public parseLibraryDefinition name body =
    let diags = DiagnosticBag.Empty
    let checkReservedNames = function
        | start::rest ->
            if start = "scheme" || start = "srfi" then
                start
                |> sprintf "The name prefix %s is reserved"
                |> Diagnostic.CreateWarning name.Location
                |> diags.Add
            start::rest
        | [] -> []
    parseLibraryName diags name
    |> Result.map checkReservedNames
    |> Result.map (fun boundName -> parseLibraryBody diags boundName body)
    |> Result.map (fun lib -> (lib, diags.Take))
    |> Result.mapError (fun _ -> diags.Take)