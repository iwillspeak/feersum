namespace Feersum.CompilerServices.Binding

open Feersum.CompilerServices.Ice
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Utils

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
    | Form of TextLocation * MacroTemplateElement list
    | DottedForm of MacroTemplateElement list * MacroTemplate

and MacroTemplateElement =
    | Template of MacroTemplate
    | Repeated of MacroTemplate

/// Macro transformer is a pair of macro pattern, and macro template
type MacroTransformer = (MacroPattern * MacroTemplate)

/// Macro is a list of transformers
type Macro =
    { Name: string
      Transformers: MacroTransformer list }

/// The binding of a macro variable to syntax.
type MacroBinding = (string * AstNode)

/// Collection of bindings produced by a macro match
type MacroBindings =
    { Bindings: MacroBinding list
      Repeated: MacroBindings list }


    /// Empty set of bindings
    static member public Empty = { Bindings = []; Repeated = [] }

    /// Create a set of bindings with a single bound variable
    static member public FromVariable name node =
        { Bindings = [ (name, node) ]
          Repeated = [] }

    static member public Union left right =
        let rec f left right =
            { Bindings = List.append left.Bindings right.Bindings
              Repeated = g left.Repeated right.Repeated }

        and g left right =
            match left, right with
            | [], a -> a
            | a, [] -> a
            | lh :: lt, rh :: rt -> (f lh rh) :: (g lt rt)

        f left right

module Macros =

    /// Create an error result with a diagnostic at `location`
    let private errAt location message =
        Diagnostic.Create location message |> Result.Error

    /// Parse a (a ... . b) or (a ...) form. This is used to parse both patterns and
    /// templates for transformers.
    let private parseMacroForm form elipsis recurse onRepeated onSingle onDotted onForm =
        let rec parseForm maybeDot nodes =
            match maybeDot with
            | Some loc ->
                ([],
                 match nodes with
                 | [ n ] -> recurse n
                 | _ -> errAt loc "Only expected a single element after dot"
                 |> Some)
            | None ->
                match nodes with
                | { Kind = AstNodeKind.Dot; Location = l } :: _ -> ([], errAt l "Invalid dotted form" |> Some)
                | node :: rest ->
                    let element = recurse node

                    let (maybeDot, rest) =
                        match rest with
                        | { Kind = AstNodeKind.Dot; Location = l } :: rest -> (Some(l), rest)
                        | _ -> (None, rest)

                    let (element, rest) =
                        match rest with
                        | { Kind = AstNodeKind.Ident (id) } :: rest when id = elipsis ->
                            (element |> Result.map onRepeated, rest)
                        | _ -> (element |> Result.map onSingle, rest)

                    let (templates, maybeDotElement) = parseForm maybeDot rest
                    (element :: templates, maybeDotElement)
                | [] -> ([], None)

        let (elements, maybeDotElement) = parseForm None form

        match maybeDotElement with
        | Some dot ->
            elements
            |> ResultEx.collect
            |> Result.bind
                (fun elements ->
                    match dot with
                    | Ok d -> Ok(onDotted (elements, d))
                    | _ -> dot)
        | None -> elements |> ResultEx.collect |> Result.map onForm

    /// Attempt to match a pattern against a syntax tree. Returns `Ok` if the
    /// pattern matches. Returns `Err` if the pattern does not match the given node.
    let rec macroMatch (pat: MacroPattern) (ast: AstNode) : Result<MacroBindings, unit> =
        match pat with
        | Constant c ->
            match ast.Kind with
            | AstNodeKind.Constant k ->
                if k = c then
                    Result.Ok MacroBindings.Empty
                else
                    Result.Error()
            | _ -> Result.Error()
        | Variable v -> Result.Ok(MacroBindings.FromVariable v ast)
        | MacroPattern.Form patterns ->
            match ast.Kind with
            | AstNodeKind.Form g -> matchForm patterns None g
            | _ -> Result.Error()
        | MacroPattern.DottedForm (patterns, tail) ->
            match ast.Kind with
            | AstNodeKind.Form g -> matchForm patterns (Some(tail)) g
            | _ -> Result.Error()
        | Underscore -> Result.Ok MacroBindings.Empty
        | Literal literal ->
            match ast.Kind with
            | AstNodeKind.Ident id ->
                if id = literal then
                    Result.Ok MacroBindings.Empty
                else
                    Result.Error()
            | _ -> Result.Error()
        | Repeat _ ->
            // We _could_ try and 'gracefully' fail here by just matching the inner
            // pattern. That would make finding any bugs in our pattern parsing more
            // difficult to track down though.
            ice "Repeat at top level"

    /// Try to match a list of patterns and, optionally, a tail form against the
    /// contents of a form. If the pattern list contains any elipsis the heavy
    /// is forwarded to `matchRepeated`.
    and private matchForm patterns maybeTail syntax =
        match patterns with
        | MacroPattern.Repeat (repeat) :: pats -> matchRepeated repeat pats maybeTail syntax []
        | headPat :: patterns ->
            match syntax with
            | head :: rest ->
                macroMatch headPat head
                |> Result.bind
                    (fun vars ->
                        matchForm patterns maybeTail rest
                        |> Result.map (MacroBindings.Union vars))
            | [] -> Result.Error()
        | [] ->
            match maybeTail with
            | Some tailPattern ->
                match syntax with
                | [ single ] -> macroMatch tailPattern single
                | other ->
                    match tailPattern with
                    | Underscore -> Result.Ok MacroBindings.Empty
                    | Variable v ->
                        Result.Ok(
                            MacroBindings.FromVariable
                                v
                                { Kind = AstNodeKind.Form(other)
                                  Location = Missing }
                        )
                    | _ -> Result.Error()
            | None ->
                if List.isEmpty syntax then
                    Ok MacroBindings.Empty
                else
                    Result.Error()

    /// Try and match a repeated pattern. This effectively re-matches the tail
    /// patterns until no further matches of repeat are required in a manner similar
    /// to backtracking.
    and private matchRepeated repeat patterns maybeTail syntax repeatedBindings =
        match matchForm patterns maybeTail syntax with
        | Ok vars ->
            Ok
                { vars with
                      Repeated = repeatedBindings |> List.rev }
        | _ ->
            match syntax with
            | head :: syntax ->
                macroMatch repeat head
                |> Result.bind (fun x -> matchRepeated repeat patterns maybeTail syntax (x :: repeatedBindings))
            | _ ->
                // Ran out of repeats and tail never matched
                Result.Error()

    let private getNode (node, _) = node
    let private getCount (_, count) = count

    /// Expand a macro template with the given bindings.
    let rec public macroExpand template (bindings: MacroBindings) =
        let result, _ = macroExpandTemplate template bindings
        result

    and macroExpandTemplate template (bindings: MacroBindings) =
        match template with
        | Quoted q -> (Result.Ok q, 0)
        | Subst v ->
            match List.tryFind (fun (id, _) -> id = v) bindings.Bindings with
            | Some (_, syntax) -> (Result.Ok syntax, 1)
            | None -> (Result.Error(sprintf "Reference to unbound substitution %s" v), 0)
        | Form (location, templateElements) ->
            let elements =
                List.map (fun t -> macroExpandElement t bindings) templateElements

            let substs = List.sumBy (getCount) elements

            let elements =
                elements
                |> List.map (getNode)
                |> ResultEx.collect
                |> Result.map
                    (fun expanded ->
                        { Kind = AstNodeKind.Form(expanded |> List.concat)
                          Location = location })

            (elements, substs)
        | DottedForm _ -> unimpl "Dotted forms in macro expansion are not yet supported"

    and private macroExpandElement templateElement bindings =
        match templateElement with
        | Template t ->
            let result, substs = macroExpandTemplate t bindings
            (result |> Result.map (fun n -> [ n ]), substs)
        | Repeated t ->
            let rec collectRepeat =
                function
                | (Result.Error _, 0) :: rest -> collectRepeat rest
                | (Result.Error e, n) :: _ -> Result.Error e
                | (Result.Ok node, _) :: rest ->
                    collectRepeat rest
                    |> Result.map (fun rest -> node :: rest)
                | [] -> Result.Ok []

            let repeated =
                bindings.Repeated
                |> List.map (macroExpandTemplate t)
                |> collectRepeat

            (repeated, 0)

    /// Apply a macro to a given syntax node
    let macroApply (macro: Macro) syntax =
        let rec macroTryApply transfomers =
            match transfomers with
            | (p, t) :: rest ->
                match macroMatch p syntax with
                | Ok binds -> macroExpand t binds
                | Result.Error _ -> macroTryApply rest
            | [] -> Result.Error "No pattern matched the syntax"

        macroTryApply macro.Transformers
        |> Result.mapError (Diagnostic.Create syntax.Location)

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
            | l when List.contains l literals -> MacroPattern.Literal l
            | v -> MacroPattern.Variable v
            |> Ok
        | AstNodeKind.Form f ->
            parseMacroForm f elipsis (recurse) (MacroPattern.Repeat) (id) (MacroPattern.DottedForm) (MacroPattern.Form)
        | AstNodeKind.Vector _
        | AstNodeKind.ByteVector _
        | AstNodeKind.Quoted _ -> e "Unsupported pattern element"
        | AstNodeKind.Seq _
        | AstNodeKind.Error -> e "Invalid macro pattern"

    /// Parse a macro template specification from a syntax tree.
    let rec public parseTemplate elipsis bound syntax =
        let recurse = parseTemplate elipsis bound

        match syntax.Kind with
        | AstNodeKind.Form f ->
            parseMacroForm
                f
                elipsis
                (recurse)
                (MacroTemplateElement.Repeated)
                (MacroTemplateElement.Template)
                (MacroTemplate.DottedForm)
                (fun x -> MacroTemplate.Form(syntax.Location, x))
        | AstNodeKind.Ident id ->
            if List.contains id bound then
                MacroTemplate.Subst id
            else
                MacroTemplate.Quoted syntax
            |> Ok
        | _ -> Ok(MacroTemplate.Quoted syntax)

    let rec public findBound =
        function
        | MacroPattern.Variable v -> [ v ]
        | MacroPattern.Repeat r -> findBound r
        | MacroPattern.Form body -> Seq.map (findBound) body |> List.concat
        | MacroPattern.DottedForm (body, extra) -> List.append (Seq.map (findBound) body |> List.concat) (findBound extra)
        | _ -> []

    /// Parse a single macro transformer from a syntax node
    let private parseTransformer id elip literals =
        function
        | { Kind = AstNodeKind.Form ([ pat; template ]) } ->
            parsePattern elip literals pat
            |> Result.bind
                (fun pat ->
                    let bound = findBound pat

                    parseTemplate elip bound template
                    |> Result.map (fun template -> (pat, template)))
        | n -> errAt n.Location "Ill-formed syntax case"

    // Parse a list of syntax transformers
    let private parseTransformers id elip literals body =
        List.map
            (function
            | { Kind = AstNodeKind.Ident (id) } -> Ok(id)
            | n -> errAt n.Location "Expected an identifier in macro literals")
            literals
        |> ResultEx.collect
        |> Result.bind
            (fun literals ->
                List.map (fun case -> parseTransformer id elip literals case) body
                |> ResultEx.collect)

    /// Parse the body of a syntax rules form.
    let private parseSyntaxRulesBody id loc syntax =
        match syntax with
        | { Kind = AstNodeKind.Ident (elip) } :: { Kind = AstNodeKind.Form (literals) } :: body ->
            parseTransformers id elip literals body
        | { Kind = AstNodeKind.Form (literals) } :: body -> parseTransformers id "..." literals body
        | _ -> errAt loc "Ill-formed syntax rules."
        |> Result.map
            (fun transformers ->
                { Name = id
                  Transformers = transformers })

    /// Parse a syntax rules expression into a macro definition.
    let public parseSyntaxRules id syntaxRulesSyn =
        match syntaxRulesSyn with
        | { Kind = AstNodeKind.Form ({ Kind = AstNodeKind.Ident ("syntax-rules") } :: body) } ->
            parseSyntaxRulesBody id syntaxRulesSyn.Location body
        | _ -> errAt syntaxRulesSyn.Location "Expected `syntax-rules` special form"
