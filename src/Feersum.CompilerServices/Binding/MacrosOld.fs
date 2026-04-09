namespace Feersum.CompilerServices.Binding

open Feersum.CompilerServices.Ice
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Syntax
open Feersum.CompilerServices.Syntax.Tree
open Feersum.CompilerServices.Syntax.Factories
open Feersum.CompilerServices.Utils

/// The macro pattern type. Used in syntax cases to define the form that a
/// macro should match.
type MacroPattern =
    | Constant of ConstantValue
    | Underscore
    | Literal of string
    | Variable of string
    | Repeat of MacroPattern
    | Form of MacroPattern list
    | DottedForm of MacroPattern list * MacroPattern

/// Macro template specify how to convert a match into new syntax. They are
/// effecively patterns for the syntax, with holes to be filled in by matches.
type MacroTemplate =
    | Quoted of Expression
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
type MacroBinding = (string * Expression)

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

module MacrosOld =

    let private macroExpansionError =
        DiagnosticKind.Create DiagnosticLevel.Error 40 "Macro expansion error"

    /// Create an error result with a diagnostic at `location`
    let private errAt location message =
        Diagnostic.Create macroExpansionError location message |> Result.Error

    /// Parse a (a ... . b) or (a ...) form body. Handles elipsis detection.
    /// `body` is the list of elements before any dotted tail; `tailOpt` is the
    /// optional tail element (from a DottedForm).
    let private parseMacroFormBody
        (body: Expression list)
        (tailOpt: Expression option)
        elipsis
        recurse
        onRepeated
        onSingle
        onDotted
        onForm
        =
        let rec parseBody nodes =
            match nodes with
            | node :: rest ->
                let element = recurse node

                let (element, rest) =
                    match rest with
                    | (Symbol id) :: rest when id = elipsis -> (element |> Result.map onRepeated, rest)
                    | _ -> (element |> Result.map onSingle, rest)

                let (templates, maybeDotElement) = parseBody rest
                (element :: templates, maybeDotElement)
            | [] ->
                match tailOpt with
                | Some tailNode -> ([], recurse tailNode |> Some)
                | None -> ([], None)

        let (elements, maybeDotElement) = parseBody body

        match maybeDotElement with
        | Some dot ->
            elements
            |> Result.collect
            |> Result.bind (fun elements ->
                match dot with
                | Ok d -> Ok(onDotted (elements, d))
                | _ -> dot)
        | None -> elements |> Result.collect |> Result.map onForm

    /// Compare two ConstantValues by their extracted values
    let private constantsMatch (pat: ConstantValue) (cv: ConstantValue) =
        match pat, cv with
        | NumVal p, NumVal n -> p = n
        | StrVal p, StrVal s -> p = s
        | BoolVal p, BoolVal b -> p = b
        | CharVal p, CharVal c -> p = c
        | _ -> false

    /// Attempt to match a pattern against a syntax tree. Returns `Ok` if the
    /// pattern matches. Returns `Err` if the pattern does not match the given node.
    let rec macroMatch (pat: MacroPattern) (ast: Expression) : Result<MacroBindings, unit> =
        match pat with
        | Constant c ->
            match ast with
            | ConstantNode cn ->
                match cn.Value with
                | Some k when constantsMatch c k -> Result.Ok MacroBindings.Empty
                | _ -> Result.Error()
            | _ -> Result.Error()
        | Variable v -> Result.Ok(MacroBindings.FromVariable v ast)
        | MacroPattern.Form patterns ->
            match ast with
            | FormNode f -> matchForm patterns None (f.Body |> List.ofSeq) None
            | _ -> Result.Error()
        | MacroPattern.DottedForm(patterns, tail) ->
            match ast with
            | FormNode f ->
                let body = f.Body |> List.ofSeq
                let tailExpr = f.DottedTail |> Option.bind (_.Body)
                matchForm patterns (Some tail) body tailExpr
            | _ -> Result.Error()
        | Underscore -> Result.Ok MacroBindings.Empty
        | Literal literal ->
            match ast with
            | Symbol id ->
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
    /// contents of a form body. `body` is the list of body elements,
    /// `tailExpr` is the optional parsed tail expression from a DottedForm.
    and private matchForm patterns maybeTailPat body tailExpr =
        match patterns with
        | MacroPattern.Repeat(repeat) :: pats -> matchRepeated repeat pats maybeTailPat body tailExpr []
        | headPat :: patterns ->
            match body with
            | head :: rest ->
                macroMatch headPat head
                |> Result.bind (fun vars ->
                    matchForm patterns maybeTailPat rest tailExpr
                    |> Result.map (MacroBindings.Union vars))
            | [] -> Result.Error()
        | [] ->
            match maybeTailPat with
            | Some tailPattern ->
                match tailExpr with
                | Some single -> macroMatch tailPattern single
                | None ->
                    // No tail expression, but pattern expects one.
                    // Match remaining body elements as a list.
                    match body with
                    | [ single ] -> macroMatch tailPattern single
                    | other ->
                        match tailPattern with
                        | Underscore -> Result.Ok MacroBindings.Empty
                        | Variable v ->
                            Result.Ok(MacroBindings.FromVariable v (Factories.form DocId.Synthetic other :> Expression))
                        | _ -> Result.Error()
            | None ->
                if List.isEmpty body && tailExpr.IsNone then
                    Ok MacroBindings.Empty
                else
                    Result.Error()

    /// Try and match a repeated pattern. This effectively re-matches the tail
    /// patterns until no further matches of repeat are required in a manner similar
    /// to backtracking.
    and private matchRepeated repeat patterns maybeTailPat body tailExpr repeatedBindings =
        match matchForm patterns maybeTailPat body tailExpr with
        | Ok vars ->
            Ok
                { vars with
                    Repeated = repeatedBindings |> List.rev }
        | _ ->
            match body with
            | head :: rest ->
                macroMatch repeat head
                |> Result.bind (fun x ->
                    matchRepeated repeat patterns maybeTailPat rest tailExpr (x :: repeatedBindings))
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
            | Some(_, syntax) -> (Result.Ok syntax, 1)
            | None -> (Result.Error(sprintf "Reference to unbound substitution %s" v), 0)
        | MacroTemplate.Form(location, templateElements) ->
            let elements = List.map (fun t -> macroExpandElement t bindings) templateElements

            let substs = List.sumBy (getCount) elements

            let elements =
                elements
                |> List.map (getNode)
                |> Result.collect
                |> Result.map (fun expanded ->
                    let concatted: Expression list = expanded |> List.concat
                    Factories.form DocId.Synthetic concatted :> Expression)

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
                | (Result.Ok node, _) :: rest -> collectRepeat rest |> Result.map (fun rest -> node :: rest)
                | [] -> Result.Ok []

            let repeated =
                bindings.Repeated |> List.map (macroExpandTemplate t) |> collectRepeat

            (repeated, 0)

    /// Apply a macro to a given syntax node
    let macroApply (macro: Macro) (syntax: Expression) (loc: TextLocation) =
        let rec macroTryApply transfomers =
            match transfomers with
            | (p, t) :: rest ->
                match macroMatch p syntax with
                | Ok binds -> macroExpand t binds
                | Result.Error _ -> macroTryApply rest
            | [] -> Result.Error "No pattern matched the syntax"

        macroTryApply macro.Transformers
        |> Result.mapError (Diagnostic.Create macroExpansionError loc)

    /// Try to parse a pattern from the given syntax.
    let rec public parsePattern elipsis literals (syntax: Expression) =
        let recurse = parsePattern elipsis literals
        let e = errAt TextLocation.Missing

        match syntax with
        | ConstantNode c ->
            match c.Value with
            | Some cv -> Ok(MacroPattern.Constant cv)
            | None -> e "Invalid constant in macro pattern"
        | SymbolNode s ->
            match s.CookedValue with
            | "_" -> MacroPattern.Underscore
            | l when List.contains l literals -> MacroPattern.Literal l
            | v -> MacroPattern.Variable v
            |> Ok
        | FormNode f ->
            let body = f.Body |> List.ofSeq
            let tailOpt = f.DottedTail |> Option.bind (_.Body)

            parseMacroFormBody
                body
                tailOpt
                elipsis
                (recurse)
                (MacroPattern.Repeat)
                (id)
                (MacroPattern.DottedForm)
                (MacroPattern.Form)
        | VecNode _
        | ByteVecNode _
        | QuotedNode _ -> e "Unsupported pattern element"

    /// Parse a macro template specification from a syntax tree.
    let rec public parseTemplate elipsis bound (syntax: Expression) =
        let recurse = parseTemplate elipsis bound

        match syntax with
        | FormNode f ->
            let body = f.Body |> List.ofSeq
            let tailOpt = f.DottedTail |> Option.bind (_.Body)

            parseMacroFormBody
                body
                tailOpt
                elipsis
                (recurse)
                (MacroTemplateElement.Repeated)
                (MacroTemplateElement.Template)
                (MacroTemplate.DottedForm)
                (fun x -> MacroTemplate.Form(TextLocation.Missing, x))
        | Symbol id ->
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
        | MacroPattern.DottedForm(body, extra) ->
            List.append (Seq.map (findBound) body |> List.concat) (findBound extra)
        | _ -> []

    /// Parse a single macro transformer from a syntax node
    let private parseTransformer id elip literals (syntax: Expression) =
        match syntax with
        | FormNode f when f.DottedTail.IsNone ->
            let body = f.Body |> List.ofSeq

            match body with
            | [ pat; template ] ->
                parsePattern elip literals pat
                |> Result.bind (fun pat ->
                    let bound = findBound pat

                    parseTemplate elip bound template |> Result.map (fun tmpl -> (pat, tmpl)))
            | _ -> errAt TextLocation.Missing "Ill-formed syntax case"
        | _ -> errAt TextLocation.Missing "Ill-formed syntax case"

    // Parse a list of syntax transformers
    let private parseTransformers id elip (lits: Expression list) (body: Expression list) =
        List.map
            (function
            | SymbolNode s -> Ok(s.CookedValue)
            | _ -> errAt TextLocation.Missing "Expected an identifier in macro literals")
            lits
        |> Result.collect
        |> Result.bind (fun literals ->
            List.map (fun case -> parseTransformer id elip literals case) body
            |> Result.collect)

    /// Parse the body of a syntax rules form.
    let private parseSyntaxRulesBody id loc (syntax: Expression list) =
        match syntax with
        | SymbolNode elipNode :: FormNode litsNode :: body ->
            parseTransformers id elipNode.CookedValue (litsNode.Body |> List.ofSeq) body
        | FormNode litsNode :: body -> parseTransformers id "..." (litsNode.Body |> List.ofSeq) body
        | _ -> errAt loc "Ill-formed syntax rules."
        |> Result.map (fun transformers ->
            { Name = id
              Transformers = transformers })

    /// Parse a syntax rules expression into a macro definition.
    let public parseSyntaxRules id (syntaxRulesSyn: Expression) =
        match syntaxRulesSyn with
        | FormNode f when f.DottedTail.IsNone ->
            let body = f.Body |> List.ofSeq

            match body with
            | SymbolNode s :: body when s.CookedValue = "syntax-rules" ->
                parseSyntaxRulesBody id TextLocation.Missing body
            | _ -> errAt TextLocation.Missing "Expected `syntax-rules` special form"
        | _ -> errAt TextLocation.Missing "Expected `syntax-rules` special form"
