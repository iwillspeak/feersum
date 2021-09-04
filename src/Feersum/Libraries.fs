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
    | Error

/// Library Definition
/// 
/// Represents the parsed body of a `(define-library)` form.
type LibraryDefinition = Library of string list * LibraryDeclaration list

/// Recognise a list of strings as a library name
let private parseLibraryName (diags: DiagnosticBag) name =
    let isInvalidChar = function
        | '|' | '\\' | '?' | '*'
        | '<' | '"' | ':' | '>'
        | '+' | '[' | ']' | '/' | '\'' -> true
        | _ -> false
    let checkReservedNames = function
        | start::rest ->
            if start = "scheme" || start = "srfi" then
                start
                |> sprintf "The name prefix %s is reserved"
                |> Diagnostic.CreateWarning name.Location
                |> diags.Add
            start::rest
        | [] -> []
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
        |> Result.map (checkReservedNames)
    | _ -> 
        diags.Emit name.Location "Expected library name"
        Result.Error(())

/// Try and parse a node as an identifier
let private parseIdentifier = function
    | { Kind = AstNodeKind.Ident(id) } -> Ok(id)
    | node -> Result.Error(Diagnostic.Create node.Location "Expected identifier")

/// Bind a list of identifiers into a list of strings
let private parseIdentifierList idents =
    idents
    |> List.map parseIdentifier
    |> ResultEx.collect

/// Bind a library declaration form
let rec private bindLibraryDeclaration (diags: DiagnosticBag) declaration =
    match declaration.Kind with
    | AstNodeKind.Form({ Kind = AstNodeKind.Ident(special)}::body) ->
        bindLibraryDeclarationForm diags declaration.Location special body
    | _ ->
        diags.Emit declaration.Location "Expected library declaration"
        LibraryDeclaration.Error

and private bindLibraryDeclarationForm diags position special body =
    match special with
    | "export"  ->
        body
        |> List.map (bindExportDeclaration diags)
        |> LibraryDeclaration.Export
    | "import" ->
        body
        |> List.map (bindImportDeclration diags)
        |> LibraryDeclaration.Import
    | "begin" ->
        // TODO: Handle library bodies
        LibraryDeclaration.Export []
    | s ->
        sprintf "Unrecognised library declaration %s" s
        |> diags.Emit position
        LibraryDeclaration.Error

and private tryParseRename rename =
    match rename with
    | [{ Kind = AstNodeKind.Ident(int) };{ Kind = AstNodeKind.Ident(ext) }] ->
        Ok(SymbolRename.Rename(int, ext))
    | _ -> Result.Error("invalid rename")

and private bindExportDeclaration diags export =
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

and private bindImportDeclration diags import =
    let bindImportForm binder fromSet body resultCollector =
        match binder body with
        | Ok(bound) ->
            resultCollector(bindImportDeclration diags fromSet, bound)
        | Result.Error(diag) ->
            diags.Add(diag)
            ImportSet.Error

    match import.Kind with
    | AstNodeKind.Form({ Kind = AstNodeKind.Ident("only")}::(fromSet::filters)) ->
        bindImportForm (parseIdentifierList) fromSet filters ImportSet.Only
    | AstNodeKind.Form({ Kind = AstNodeKind.Ident("except")}::(fromSet::filters)) ->
        bindImportForm (parseIdentifierList) fromSet filters ImportSet.Except
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
        
        bindImportForm (parseRenames) fromSet renames ImportSet.Renamed
    | AstNodeKind.Form([{ Kind = AstNodeKind.Ident("prefix")};fromSet;prefix]) ->
        bindImportForm (parseIdentifier) fromSet prefix ImportSet.Prefix
    | _ -> 
        match parseLibraryName diags import with
        | Ok(name) -> ImportSet.Plain name
        | Result.Error _ -> ImportSet.Error


let private bindLibraryBody ctx name body =
    let binder = bindLibraryDeclaration ctx
    let boundBody =
        body
        |> List.map binder

    LibraryDefinition.Library(name, boundBody)

// -------------------- Public Libraries API ---------------------

let public parseLibraryDeclaration name body =
    let diags = DiagnosticBag.Empty
    parseLibraryName diags name
    |> Result.map (fun boundName -> bindLibraryBody diags boundName body)
    |> Result.map (fun lib -> (lib, diags.Take))
    |> Result.mapError (fun _ -> diags.Take)