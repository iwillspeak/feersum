module Libraries

open Diagnostics
open Syntax
open Utils

/// The rename of a single element exported or imported from a library
type SymbolRename = Rename of string * string

/// A single export from a library declration
type ExportSet =
    | Plain of string
    | Renamed of SymbolRename
    | Error

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
        |> List.map (parseExportDeclaration diags)
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
        Ok(SymbolRename.Rename(int, ext))
    | _ -> Result.Error("invalid rename")

and private parseExportDeclaration diags export =
    match export.Kind with
    | AstNodeKind.Ident(plain) -> ExportSet.Plain plain
    | AstNodeKind.Form({ Kind = AstNodeKind.Ident("rename")}::rename) ->
        match rename |> tryParseRename with
        | Result.Ok(renamed) -> ExportSet.Renamed renamed
        | Result.Error(e) ->
            diags.Emit export.Location e
            ExportSet.Error
    | _ ->
        diags.Emit export.Location "Invalid export element" 
        ExportSet.Error

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
    let parser = parseLibraryDeclaration ctx
    let parsedBodies =
        body
        |> List.map parser

    { LibraryName = name; Declarations = parsedBodies }

// -------------------- Public Libraries API ---------------------

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