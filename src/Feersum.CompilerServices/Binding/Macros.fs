namespace Feersum.CompilerServices.Binding.New

open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Binding.Stx
open Feersum.CompilerServices.Ice
open Feersum.CompilerServices.Utils

// -- Pattern types ------------------------------------------------------------

/// The macro pattern type. Used in syntax cases to define the form that a
/// macro should match.
[<RequireQualifiedAccess>]
type MacroPattern =
    /// Match a self-evaluating literal exactly.
    | Constant of StxDatum
    /// Match anything, without binding (`_`).
    | Underscore
    /// Match a literal keyword (must appear in the `syntax-rules` literals list).
    /// The `StxBinding option` is the definition-site resolution of this identifier,
    /// stored at parse time so that hygienic literal matching can compare both
    /// sides by binding identity rather than by name alone.
    | Literal of name: string * defBinding: StxBinding option
    /// Capture a single sub-form into a pattern variable.
    | Variable of string
    /// Match zero-or-more repetitions of the sub-pattern (the `...` operator).
    | Repeat of MacroPattern
    /// Match a proper list.
    | Form of MacroPattern list
    /// Match an improper list (dotted pair form).
    | DottedForm of MacroPattern list * MacroPattern
    /// Match a vector literal `#(...)`.
    | Vec of MacroPattern list

// -- Template types -----------------------------------------------------------

/// Macro templates specify how to convert a match into new syntax. They are
/// effectively patterns for the output, with holes filled in by pattern variable matches.
[<RequireQualifiedAccess>]
type MacroTemplate =
    /// A literal node from the macro definition site (quoted, not substituted).
    | Quoted of stx: Stx
    /// Substitute the sub-form captured by a pattern variable.
    | Subst of name: string
    /// Produce a proper list.
    | Form of loc: TextLocation * elements: MacroTemplateElement list
    /// Produce an improper list.
    | DottedForm of loc: TextLocation * elements: MacroTemplateElement list * tail: MacroTemplate
    /// Produce a vector literal.
    | Vec of loc: TextLocation * elements: MacroTemplateElement list

/// A macro template element — either a single sub-template or a repeated one.
and [<RequireQualifiedAccess>] MacroTemplateElement =
    | Template of MacroTemplate
    | Repeated of MacroTemplate

// -- Capture types ------------------------------------------------------------

/// A captured pattern variable: the matched syntax node paired with the scope at capture time.
type MacroCapture = Stx * StxEnvironment

/// The binding of a macro variable to captured syntax.
type MacroBinding = string * MacroCapture

/// Collection of bindings produced by a macro match. `Bindings` holds the
/// flat variable captures at this level; `Repeated` holds one `MacroBindings`
/// per iteration of an ellipsis sub-pattern.
type MacroBindings =
    { Bindings: MacroBinding list
      Repeated: MacroBindings list }

    /// Empty set of bindings.
    static member Empty = { Bindings = []; Repeated = [] }

    /// Create a set of bindings with a single bound variable.
    static member FromVariable name (capture: MacroCapture) =
        { Bindings = [ name, capture ]
          Repeated = [] }

    /// Merge two sibling binding sets, appending flat bindings and zipping
    /// repeated binding lists element-by-element.
    static member Union left right =
        let rec f left right =
            { Bindings = List.append left.Bindings right.Bindings
              Repeated = g left.Repeated right.Repeated }

        and g left right =
            match left, right with
            | [], a -> a
            | a, [] -> a
            | lh :: lt, rh :: rt -> (f lh rh) :: (g lt rt)

        f left right

// -- Macro type ---------------------------------------------------------------

/// A macro transformer: a single `(pattern template)` rule pair.
type MacroRule = MacroPattern * MacroTemplate

/// A macro: the set of transformer rules together with the definition-site
/// scope and source location.
type Macro =
    { Transformers: MacroRule list
      DefScope: StxEnvironment
      DefLoc: TextLocation }

// -- Diagnostics --------------------------------------------------------------

module private MacroDiagnostics =

    let macroExpansionError =
        DiagnosticKind.Create DiagnosticLevel.Error 40 "Macro expansion error"

    let invalidMacro =
        DiagnosticKind.Create DiagnosticLevel.Error 41 "Invalid macro definition"

    let invalidLiteralList =
        DiagnosticKind.Create DiagnosticLevel.Error 42 "Invalid literal list"

    let malformedEllipsisEscape =
        DiagnosticKind.Create DiagnosticLevel.Warning 43 "Malformed ellipsis escape"

    let invalidPatternHead =
        DiagnosticKind.Create DiagnosticLevel.Warning 44 "Invalid pattern head"

// -- Macros ----------------------------------------------------------------

type private PatternCtx =
    { Diags: DiagnosticBag
      Ellipsis: string option
      Literals: (string * StxBinding option) list }

    static member Create diags ellipsis literals =
        // R7RS 4.3.2: if the ellipsis identifier appears in the literals list it
        // is treated as a literal keyword and loses its special "..." meaning.
        let activeEllipsis =
            if List.exists (fun (n, _) -> n = ellipsis) literals then
                None
            else
                Some ellipsis

        { Diags = diags
          Ellipsis = activeEllipsis
          Literals = literals }

module MacroParse =

    /// Parse a literal list `(lit1 lit2 ...)` from the syntax of a `syntax-rules` form.
    /// Each identifier is resolved in its home environment so that hygienic literal matching
    /// can compare by binding identity rather than name alone.
    /// Emits diagnostics and returns an empty list on failure.
    let parseLiteralList
        (diags: DiagnosticBag)
        (defScope: StxEnvironment)
        (stx: Stx)
        : (string * StxBinding option) list =
        let unwrapId s =
            match s with
            | StxId(name, _, envOpt) ->
                let activeEnv = envOpt |> Option.defaultValue defScope
                Some(name, Map.tryFind name activeEnv)
            | _ ->
                diags.Emit MacroDiagnostics.invalidLiteralList s.Loc "literal list must contain identifiers"
                None

        match stx with
        | StxList(items, _, _, _) -> items |> List.choose unwrapId
        | _ ->
            diags.Emit MacroDiagnostics.invalidLiteralList stx.Loc "Expected list of literals"
            []

    /// Parse a single pattern element. This is used to recrusively build up the `MacroPattern` item that appears on
    /// the left hand side of a syntax rule.
    ///
    /// We return not only the pattern but also a `scope` mapping captured pattern variables to their `...` depth, which
    /// is used to detect invalid template references later on.
    let rec private parsePattern
        (ctx: PatternCtx)
        (depth: int)
        (scope: Map<string, StxBinding option * int>)
        (ambientEnv: StxEnvironment)
        (stx: Stx)
        : MacroPattern * Map<string, StxBinding option * int> =

        match stx with
        | StxDatum(d, _) -> MacroPattern.Constant d, scope
        | StxId(name, _, envOpt) ->
            let activeEnv = envOpt |> Option.defaultValue ambientEnv
            let resolvedBinding = Map.tryFind name activeEnv
            // Literals take priority over the `_` wildcard and plain variables.
            // R7RS §4.3.2: if `_` appears in the literals list it is a literal keyword, not a wildcard.
            // Comparing binding identity (not just name) handles identifiers injected by outer macro
            // expansions whose closure-env binding differs from any same-named local variable.
            if
                List.exists (fun (litName, litBinding) -> litName = name && litBinding = resolvedBinding) ctx.Literals
            then
                MacroPattern.Literal(name, resolvedBinding), scope
            else if name = "_" then
                MacroPattern.Underscore, scope
            else
                // Record the resolved binding so that `parseTemplate` can match template
                // references to this exact binding rather than just comparing name strings.
                MacroPattern.Variable name, scope.Add(name, (resolvedBinding, depth))
        | StxList(items, tail, _, envOpt) ->
            let ambientEnv = envOpt |> Option.defaultValue ambientEnv
            let patterns, scope = parsePatternList ctx depth scope ambientEnv items

            match tail with
            | Some tailStx ->
                let tailPat, scope = parsePattern ctx depth scope ambientEnv tailStx
                MacroPattern.DottedForm(patterns, tailPat), scope
            | None -> MacroPattern.Form patterns, scope
        | StxVec(items, _, envOpt) ->
            let ambientEnv = envOpt |> Option.defaultValue ambientEnv
            let patterns, scope = parsePatternList ctx depth scope ambientEnv items
            MacroPattern.Vec patterns, scope
        | StxError _ ->
            ctx.Diags.Emit MacroDiagnostics.invalidMacro stx.Loc "malformed syntax node in pattern"
            MacroPattern.Underscore, scope

    /// Recognise a list of patterns. This is slightly more complex than a simple map / fold over the list because we
    /// need to detect the `...` operator and parse the repeated pattern at the correct depth.
    and private parsePatternList
        (ctx: PatternCtx)
        (depth: int)
        (scope: Map<string, StxBinding option * int>)
        (ambientEnv: StxEnvironment)
        (items: Stx list)
        : MacroPattern list * Map<string, StxBinding option * int> =
        let rec loop acc scope remaining =
            match remaining with
            | [] -> List.rev acc, scope
            | current :: (StxId(next, _, _) :: rest) when Some next = ctx.Ellipsis ->
                let innerPat, scope' = parsePattern ctx (depth + 1) scope ambientEnv current
                loop (MacroPattern.Repeat innerPat :: acc) scope' rest
            | current :: rest ->
                let pat, scope' = parsePattern ctx depth scope ambientEnv current
                loop (pat :: acc) scope' rest

        loop [] scope items

    /// Parse the top-level pattern of a single syntax rule.
    ///
    /// The first element of the pattern form is the macro name placeholder. It is
    /// always replaced with `MacroPattern.Underscore` regardless of what identifier
    /// it is or whether that identifier appears in the literals list.  This matches
    /// R7RS §4.3.2, which states the first sub-form of a pattern names the keyword
    /// being defined and is not used for matching.
    ///
    /// This is necessary because `_` can legally appear in the literals list, in
    /// which case normal `parsePattern` would wrongly classify the head position as
    /// a literal keyword rather than the always-wildcard macro-name placeholder.
    let private parsePatternTopLevel
        (ctx: PatternCtx)
        (ambientEnv: StxEnvironment)
        (stx: Stx)
        : MacroPattern * Map<string, StxBinding option * int> =
        match stx with
        | StxList(head :: bodyItems, tail, _, envOpt) ->
            match head with
            | StxId(_, _, _) -> ()
            | _ ->
                ctx.Diags.Emit MacroDiagnostics.invalidPatternHead head.Loc "syntax-rules pattern should start with an identifier"

            let ambientEnv = envOpt |> Option.defaultValue ambientEnv
            let patterns, scope = parsePatternList ctx 0 Map.empty ambientEnv bodyItems

            match tail with
            | Some tailStx ->
                let tailPat, scope = parsePattern ctx 0 scope ambientEnv tailStx
                MacroPattern.DottedForm(MacroPattern.Underscore :: patterns, tailPat), scope
            | None -> MacroPattern.Form(MacroPattern.Underscore :: patterns), scope
        | _ ->
            ctx.Diags.Emit MacroDiagnostics.invalidMacro stx.Loc "syntax-rules pattern must be a (_ ....) form"
            MacroPattern.Underscore, Map.empty

    let rec private parseTemplate
        (ctx: PatternCtx)
        (scope: Map<string, StxBinding option * int>)
        (depth: int)
        (ambientEnv: StxEnvironment)
        (stx: Stx)
        : MacroTemplate =
        match stx with
        | StxId(name, _, envOpt) ->
            // Resolve the identifier in its home environment, then look for a pattern variable
            // with the same name *and* the same resolved binding.  This handles the common case
            // (bare identifier in both pattern and template, both unbound in defScope → None = None)
            // and the hygienic case (an identifier injected from an outer expansion carries a
            // different StxBinding than a locally-introduced pattern variable with the same name).
            let activeEnv = envOpt |> Option.defaultValue ambientEnv
            let resolvedId = Map.tryFind name activeEnv

            match scope.TryGetValue name with
            | true, (storedId, boundDepth) when resolvedId = storedId ->
                if boundDepth <> depth then
                    ctx.Diags.Emit
                        MacroDiagnostics.invalidMacro
                        stx.Loc
                        $"template variable '{name}' is not in scope at this ellipsis level"
                    // We quote the variable here to suppress any expander errors about the same name error
                    MacroTemplate.Quoted stx
                else
                    MacroTemplate.Subst name
            | _ -> MacroTemplate.Quoted stx
        | StxList(items, tail, loc, envOpt) ->
            let ambientEnv = envOpt |> Option.defaultValue ambientEnv

            match ctx.Ellipsis, items, tail with
            | Some ell, StxId(name, _, _) :: inner, None when name = ell ->
                // R7RS 4.3.2: (〈ellipsis〉 〈template〉) is an ellipsis-escape form.
                // The single inner template is parsed with no active ellipsis so that
                // occurrences of the ellipsis identifier are treated as plain identifiers.
                match inner with
                | [ innerStx ] -> parseTemplate { ctx with Ellipsis = None } scope depth ambientEnv innerStx
                | _ ->
                    ctx.Diags.Emit
                        MacroDiagnostics.malformedEllipsisEscape
                        loc
                        "ellipsis escape requires exactly one sub-template: (... <template>)"

                    MacroTemplate.Quoted stx
            | _ ->
                let elements = parseTemplateElementList ctx scope depth ambientEnv items

                match tail with
                | Some tailStx ->
                    let tailTmpl = parseTemplate ctx scope depth ambientEnv tailStx
                    MacroTemplate.DottedForm(loc, elements, tailTmpl)
                | None -> MacroTemplate.Form(loc, elements)
        | StxVec(items, loc, envOpt) ->
            let ambientEnv = envOpt |> Option.defaultValue ambientEnv
            let elements = parseTemplateElementList ctx scope depth ambientEnv items
            MacroTemplate.Vec(loc, elements)
        | other -> MacroTemplate.Quoted other

    and private parseTemplateElementList
        (ctx: PatternCtx)
        (scope: Map<string, StxBinding option * int>)
        (depth: int)
        (ambientEnv: StxEnvironment)
        (items: Stx list)
        : MacroTemplateElement list =
        let rec loop acc remaining =
            match remaining with
            | [] -> List.rev acc
            | current :: (StxId(next, _, _) :: rest) when Some next = ctx.Ellipsis ->
                let innerTmpl = parseTemplate ctx scope (depth + 1) ambientEnv current
                loop (MacroTemplateElement.Repeated innerTmpl :: acc) rest
            | current :: rest ->
                let tmpl = parseTemplate ctx scope depth ambientEnv current
                loop (MacroTemplateElement.Template tmpl :: acc) rest

        loop [] items

    /// Parse a single `(pattern template)` rule from the body of a `syntax-rules` form.
    let parseSyntaxRule
        (diags: DiagnosticBag)
        (ellipsis: string)
        (lits: (string * StxBinding option) list)
        (defScope: StxEnvironment)
        (stx: Stx)
        : MacroRule =
        match stx with
        | StxList([ pat; tmpl ], _, _, maybeDefEnv) ->
            let defScope = maybeDefEnv |> Option.defaultValue defScope
            let patternCtx = PatternCtx.Create diags ellipsis lits
            let pattern, patternScope = parsePatternTopLevel patternCtx defScope pat
            let template = parseTemplate patternCtx patternScope 0 defScope tmpl
            (pattern, template)
        | _ ->
            diags.Emit MacroDiagnostics.invalidMacro stx.Loc "each syntax-rules rule must be (pattern template)"
            (MacroPattern.Underscore, MacroTemplate.Quoted stx)

    /// Parse a list of `(pattern template)` rules from the body of a `syntax-rules` form.
    let parseSyntaxRulesBody
        (diags: DiagnosticBag)
        (id: string)
        (ellipsis: string)
        (literals: (string * StxBinding option) list)
        (rules: Stx list)
        (defScope: StxEnvironment)
        : MacroRule list =
        rules |> List.map (parseSyntaxRule diags ellipsis literals defScope)

    /// Parse the body of a `(syntax-rules ...)` form,
    let public parseSyntaxRulesStx
        (diags: DiagnosticBag)
        (id: string)
        (syntaxRulesSyn: Stx)
        (defScope: StxEnvironment)
        : Macro option =
        let parseBody ellipsis lits rules maybeDefEnv =
            let defScope = maybeDefEnv |> Option.defaultValue defScope
            let lits = parseLiteralList diags defScope lits
            let rules = parseSyntaxRulesBody diags id ellipsis lits rules defScope

            { Transformers = rules
              DefScope = defScope
              DefLoc = syntaxRulesSyn.Loc }

        match syntaxRulesSyn with
        | StxList(StxId("syntax-rules", _, _) :: StxId(ellipsis, _, _) :: lits :: rules, _, _, maybeDefEnv) ->
            parseBody ellipsis lits rules maybeDefEnv |> Some
        | StxList(StxId("syntax-rules", _, _) :: lits :: rules, _, _, maybeDefEnv) ->
            parseBody "..." lits rules maybeDefEnv |> Some
        | _ ->
            diags.Emit MacroDiagnostics.invalidMacro syntaxRulesSyn.Loc "expected (syntax-rules (literals...) rules...)"
            None

/// Parse `(syntax-rules ...)` forms into `SyntaxTransformer` values.
/// Only the functions consumed by `Expander` are public.
module Macros =

    // -- Pattern matching -----------------------------------------------------

    /// Attempt to match a MacroPattern against a syntax node.
    /// Returns Some MacroBindings on success, None on mismatch.
    let rec matchPattern (pat: MacroPattern) (stx: Stx) (scope: StxEnvironment) : MacroBindings option =
        let stx', envOpt = Stx.peel stx
        let scope' = envOpt |> Option.defaultValue scope

        match pat, stx' with
        | MacroPattern.Underscore, _ -> Some MacroBindings.Empty

        | MacroPattern.Variable v, _ -> Some(MacroBindings.FromVariable v (stx, scope))

        | MacroPattern.Literal(lit, defBinding), Stx.Id(name, _) ->
            // R7RS §4.3.2: a literal keyword matches only if both name and binding identity agree.
            // Comparing `StxBinding option` values handles the common case (both unbound → None = None)
            // and the hygienic case (a captured identifier with a closure-injected binding won't
            // accidentally match a same-named but differently-bound identifier at the call site).
            if name = lit && Map.tryFind name scope' = defBinding then
                Some MacroBindings.Empty
            else
                None

        | MacroPattern.Constant patLit, Stx.Datum(stxLit, _) ->
            if patLit = stxLit then Some MacroBindings.Empty else None

        | MacroPattern.Form pats, Stx.List(items, None, _) -> matchPatternList pats None items None scope'

        | MacroPattern.DottedForm(pats, tailPat), Stx.List(items, tail, _) ->
            matchPatternList pats (Some tailPat) items tail scope'

        | MacroPattern.Vec pats, Stx.Vec(items, _) -> matchPatternList pats None items None scope'

        | _ -> None

    /// Try to match a list of patterns and, optionally, a tail pattern against the
    /// contents of a form body or vec. `body` is the list of body elements,
    /// `tailItem` is the optional parsed tail expression from a DottedForm.
    and matchPatternList
        (pats: MacroPattern list)
        (tailPat: MacroPattern option)
        (body: Stx list)
        (tailItem: Stx option)
        (scope: StxEnvironment)
        : MacroBindings option =
        match pats with
        | MacroPattern.Repeat inner :: restPats -> matchRepeated inner restPats tailPat body tailItem scope []

        | headPat :: patterns ->
            match body with
            | head :: rest ->
                matchPattern headPat head scope
                |> Option.bind (fun vars ->
                    matchPatternList patterns tailPat rest tailItem scope
                    |> Option.map (MacroBindings.Union vars))
            | [] -> None

        | [] ->
            match tailPat with
            | Some tp ->
                match tailItem with
                | Some t -> matchPattern tp t scope
                | None ->
                    // No explicit tail in input — the remaining items form the tail list.
                    // Always construct a list, even for a single remaining item: the tail
                    // of `(_ a b . c)` matching `(m 1 2 3)` is `(3)`, not `3`.
                    matchPattern tp (Stx.List(body, None, TextLocation.Missing)) scope
            | None ->
                if List.isEmpty body && tailItem.IsNone then
                    Some MacroBindings.Empty
                else
                    None

    /// Try and match a repeated pattern. This effectively re-matches the tail
    /// patterns until no further matches of repeat are required in a manner similar
    /// to backtracking.
    and private matchRepeated
        (repeat: MacroPattern)
        (patterns: MacroPattern list)
        (maybeTailPat: MacroPattern option)
        (body: Stx list)
        (maybeTailItem: Stx option)
        (scope: StxEnvironment)
        (accumulated: MacroBindings list)
        : MacroBindings option =
        match matchPatternList patterns maybeTailPat body maybeTailItem scope with
        | Some tailBindings ->
            let repeatedPart =
                { MacroBindings.Empty with
                    Repeated = List.rev accumulated }

            Some(MacroBindings.Union tailBindings repeatedPart)
        | None ->
            match body with
            | head :: rest ->
                matchPattern repeat head scope
                |> Option.bind (fun b ->
                    matchRepeated repeat patterns maybeTailPat rest maybeTailItem scope (b :: accumulated))
            | [] ->
                // Ran out of repeats and tail never matched
                None

    // -- Transcription --------------------------------------------------------

    /// Expand a template with the given bindings. Returns an error if the template
    /// references a variable that is not bound at the current ellipsis level.
    ///
    /// This is the main entry point for macro expansion: `macroApply` calls
    /// this after matching a rule.
    ///
    /// The second element of the resturn represents the number of substitutions
    /// that were made. This can be used to gracefully handle template expansions
    /// in ellipsis contexts when _no_ substitutions were made.
    let rec macroExpandTemplate (template: MacroTemplate) (bindings: MacroBindings) : Result<Stx, string> * int =
        let xpandList elements =
            let elements = List.map (macroExpandElement bindings) elements
            let totalSubsts = elements |> List.sumBy snd

            elements |> List.map fst |> Result.collect |> Result.map List.concat, totalSubsts

        match template with
        | MacroTemplate.Quoted q -> (Result.Ok q, 0)
        | MacroTemplate.Subst v ->
            match List.tryFind (fun (id, _) -> id = v) bindings.Bindings with
            | Some(_, (stx, capturedScope)) -> Result.Ok(Stx.Closure(stx, capturedScope)), 1
            | None -> Result.Error $"Reference to '{v}' is not bound at this repetition level", 0
        | MacroTemplate.Form(location, templateElements) ->
            let children, totalSubsts = xpandList templateElements
            Result.map (fun children -> Stx.List(children, None, location)) children, totalSubsts
        | MacroTemplate.DottedForm(location, templateElements, tail) ->
            let children, totalSubsts = xpandList templateElements
            let tailResult, tailSubsts = macroExpandTemplate tail bindings

            match children, tailResult with
            | Result.Error e, _
            | _, Result.Error e -> Result.Error e, totalSubsts + tailSubsts
            | Result.Ok children, Result.Ok tail ->
                let totalSubsts = totalSubsts + tailSubsts
                Result.Ok(Stx.List(children, Some tail, location)), totalSubsts
        | MacroTemplate.Vec(location, templateElements) ->
            let children, totalSubsts = xpandList templateElements
            Result.map (fun children -> Stx.Vec(children, location)) children, totalSubsts

    and macroExpandElement
        (bindings: MacroBindings)
        (templateElement: MacroTemplateElement)
        : Result<Stx list, string> * int =
        match templateElement with
        | MacroTemplateElement.Template t ->
            let result, substs = macroExpandTemplate t bindings
            (result |> Result.map List.singleton, substs)
        | MacroTemplateElement.Repeated t ->
            let rec collectRepeat m acc =
                function
                | (Result.Error _, 0) :: rest -> collectRepeat m acc rest
                | (Result.Error e, n) :: _ -> Result.Error e, (n + m)
                | (Result.Ok node, n) :: rest -> collectRepeat (m + n) (node :: acc) rest
                | [] -> Result.Ok(List.rev acc), m

            bindings.Repeated |> List.map (macroExpandTemplate t) |> collectRepeat 0 []


    /// Apply a Macro to a given syntax node
    let macroApply (macro: Macro) (form: Stx) (callScope: StxEnvironment) : Result<Stx, string> =
        let rec macroTryApply transformers =
            match transformers with
            | (p, t) :: rest ->
                match matchPattern p form callScope with
                | Some binds -> macroExpandTemplate t binds |> fst
                | _ -> macroTryApply rest
            | [] -> Result.Error "no macro rule matched the form"

        macroTryApply macro.Transformers
        |> Result.map (fun xpanded ->
            // Re-attach the macro definition scope to the expanded form to ensure that any identifiers
            // introduced by the macro are resolved in the correct environment.
            Stx.Closure(xpanded, macro.DefScope))


    /// Public entry point: parse a `(syntax-rules ...)` form into a `SyntaxTransformer`.
    /// Returns a ready-to-call function, or `None` if the form is malformed.
    let makeSyntaxTransformer
        (name: string)
        (stx: Stx)
        (defScope: StxEnvironment)
        (diag: DiagnosticBag)
        : SyntaxTransformer option =
        MacroParse.parseSyntaxRulesStx diag name stx defScope
        |> Option.map (fun rules -> macroApply rules)
