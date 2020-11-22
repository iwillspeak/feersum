module Macros
open Syntax
open Diagnostics
open Utils

/// The macro pattern type. Used in syntax cases to define the form that a
/// macro should match.
type MacroPattern =
    | Constant of SyntaxConstant
    | Underscore
    | Literal of string
    | Variable of string
    | Repeat of MacroPattern
    | Form of MacroPattern list
    | DottedForm of MacroPattern list * MacroPattern

/// Macro template specify how to convert a match into new syntax. They are
/// effecively patterns for the syntax, with holes to be filled in by matches.
type MacroTemplate =
    | Quoted of AstNode
    | Subst of string
    | Form of MacroTemplateElement list
    | DottedForm of MacroTemplateElement list * MacroTemplate
and MacroTemplateElement =
    | Template of MacroTemplate
    | Repeated of MacroTemplate

/// The binding of a macro variable to syntax.
type MacroBinding = (string * AstNode)

/// Collection of bindings produced by a macro match
type MacroBindings =
    { Bindings: MacroBinding list
    ; Repeated: MacroBindings list }

with

    /// Empty set of bindings
    static member public Empty = { Bindings = []; Repeated = [] }

    /// Create a set of bindings with a single bound variable
    static member public FromVariable name node =
        { Bindings = [(name, node)]; Repeated = [] }

    static member public Union left right =
        let rec f left right = 
            { Bindings = List.append left.Bindings right.Bindings
            ; Repeated = g left.Repeated right.Repeated }
        and g left right =
            match left, right with
            | [],a -> a
            | a,[] -> a
            | lh::lt,rh::rt -> (f lh rh)::(g lt rt)
        f left right

/// Create an error result with a diagnostic at `location`
let private errAt location message =
    Diagnostic(location, message)
    |> Result.Error

/// Parse a (a ... . b) or (a ...) form. This is used to parse both patterns and
/// templates for transformers.
let private parseMacroForm form elipsis recurse onRepeated onSingle onDotted onForm =
    let rec parseForm maybeDot nodes =
        match maybeDot with
        | Some loc ->
            ([],match nodes with
                | [n] -> recurse n
                | _ ->
                    errAt loc "Only expected a single element after dot"
            |> Some)
        | None ->
            match nodes with
            | { Kind = AstNodeKind.Dot; Location = l }::rest ->
                parseForm (Some(l)) rest
            | node::rest ->
                let element = recurse node
                let (element, rest) = 
                    match rest with
                    | { Kind = AstNodeKind.Ident(id) }::rest when id = elipsis ->
                        (element |> Result.map onRepeated, rest)
                    | _ -> (element |> Result.map onSingle, rest)
                let (templates, maybeDotElement) = parseForm None rest
                (element::templates, maybeDotElement)
            | [] -> ([], None)

    let (elements, maybeDotElement) = parseForm None form
    match maybeDotElement with
    | Some dot ->
        elements
        |> ResultEx.collect
        |> Result.bind (fun elements ->
            match dot with
            | Ok d -> Ok(onDotted(elements, d))
            | _ -> dot)
    | None ->
        elements |> ResultEx.collect |> Result.map onForm

/// Attempt to match a pattern against a syntax tree. Returns `Ok` if the
/// pattern matches. Returns `Err` if the pattern does not match the given node.
let rec macroMatch (pat: MacroPattern) (ast: AstNode): Result<MacroBindings,unit> =
    match pat with
    | Constant c ->
        match ast.Kind with
        | AstNodeKind.Constant k ->
            if k = c then
                Result.Ok MacroBindings.Empty
            else
                Result.Error ()
        | _ -> Result.Error ()
    | Variable v ->
        Result.Ok (MacroBindings.FromVariable v ast)
    | MacroPattern.Form patterns ->
        match ast.Kind with
        | AstNodeKind.Form g ->
            matchForm patterns None g
        | _ -> Result.Error ()
    | MacroPattern.DottedForm(patterns, tail) ->
        match ast.Kind with
        | AstNodeKind.Form g ->
            matchForm patterns (Some(tail)) g
        | _ -> Result.Error ()
    | Underscore -> Result.Ok MacroBindings.Empty
    | Literal literal ->
        match ast.Kind with
        | AstNodeKind.Ident id ->
            if id = literal then
                Result.Ok MacroBindings.Empty
            else
                Result.Error ()
        | _ -> Result.Error ()
    | Repeat _ ->
        // We _could_ try and 'gracefully' fail here by just matching the inner
        // pattern. That would make finding any bugs in our pattern parsing more
        // difficult to track down though.
        failwith "Repeat at top level"

/// Try to match a list of patterns and, optionally, a tail form against the
/// contents of a form. If the pattern list contains any elipsis the heavy
/// is forwarded to `matchRepeated`.
and private matchForm patterns maybeTail syntax =
    match patterns with
    | MacroPattern.Repeat(repeat)::pats ->
        matchRepeated repeat pats maybeTail syntax []
    | headPat::patterns ->
        match syntax with
        | head::rest ->
            macroMatch headPat head
            |> Result.bind (fun vars ->
                matchForm patterns maybeTail rest
                |> Result.map (MacroBindings.Union vars))
        | [] -> Result.Error ()
    | [] ->
        match maybeTail with
        | Some tailPattern ->
            match syntax with
            | [single] -> 
                macroMatch tailPattern single
            | other ->
                match tailPattern with
                | Underscore -> Result.Ok MacroBindings.Empty
                | Variable v -> 
                    Result.Ok (MacroBindings.FromVariable v { Kind = AstNodeKind.Form(other); Location = Missing })
                | _ -> Result.Error ()
        | None ->
            if List.isEmpty syntax then
                Ok MacroBindings.Empty
            else
                Result.Error ()

/// Try and match a repeated pattern. This effectively re-matches the tail
/// patterns until no further matches of repeat are required in a manner similar
/// to backtracking.
and private matchRepeated repeat patterns maybeTail syntax repeatedBindings =
    match matchForm patterns maybeTail syntax with
    | Ok vars -> Ok { vars with Repeated = repeatedBindings |> List.rev } 
    | _ ->
        match syntax with
        | head::syntax ->
            macroMatch repeat head
            |> Result.bind (fun x ->
                matchRepeated repeat patterns maybeTail syntax (x::repeatedBindings))
        | _ ->
            // Ran out of repeats and tail never matched
            Result.Error ()

/// Expand a macro template with the given bindings.
let rec public macroExpand template (bindings: MacroBindings) =
    match template with
    | Quoted q -> Result.Ok q
    | Subst v ->
        match List.tryFind (fun (id, _) -> id = v) bindings.Bindings with
        | Some(id, syntax) -> Result.Ok syntax
        | None -> Result.Error (sprintf "Reference to unbound substitution %s" v)
    | Form templateElements ->
        List.collect (fun t -> macroExpandElement t bindings) templateElements
        |> ResultEx.collect
        |> Result.map(fun expanded ->
            { Kind = AstNodeKind.Form(expanded)
            // TODO: Syntax locations from macro elements?
            ; Location = TextLocation.Missing })
    | DottedForm _ -> failwith "Not supported"
and private macroExpandElement templateElement bindings: Result<AstNode,string> list =
    match templateElement with
    | Template t -> [ macroExpand t bindings ]
    | Repeated t ->
        List.map (macroExpand t) bindings.Repeated

/// Try to parse a pattern from the given syntax.
let rec public parsePattern elipsis literals syntax =
    let recurse = parsePattern elipsis literals
    let e = errAt syntax.Location
    match syntax.Kind with
    | AstNodeKind.Constant c -> Ok(MacroPattern.Constant c)
    | AstNodeKind.Dot -> e "Unexpected dot"
    | AstNodeKind.Ident id ->
        match id with
        | "_" -> MacroPattern.Underscore
        | l when List.contains l literals ->
            MacroPattern.Literal l
        | v -> MacroPattern.Variable v
        |> Ok
    | AstNodeKind.Form f ->
        parseMacroForm f elipsis (recurse) (MacroPattern.Repeat) (id) (MacroPattern.DottedForm) (MacroPattern.Form)
    | AstNodeKind.Vector _ | AstNodeKind.ByteVector _ | AstNodeKind.Quoted _ ->
        e "Unsupported pattern element"
    | AstNodeKind.Seq _ | AstNodeKind.Error ->
        e "Invalid macro pattern"

/// Parse a macro template specification from a syntax tree.
let rec public parseTemplate elipsis syntax =
    let recurse = parseTemplate elipsis
    match syntax.Kind with
    | AstNodeKind.Form f ->
        parseMacroForm f elipsis (recurse) (MacroTemplateElement.Repeated) (MacroTemplateElement.Template) (MacroTemplate.DottedForm) (MacroTemplate.Form)
    | AstNodeKind.Ident id ->
        // FIXME: Not all ids are substs, need a way to make bound variables
        //        available to `parseTemplate`
        Ok(MacroTemplate.Subst id)
    | _ -> Ok(MacroTemplate.Quoted syntax)
