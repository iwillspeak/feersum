namespace Feersum.CompilerServices.Binding.New

open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Binding
open Feersum.CompilerServices.Binding.Stx

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
    | Literal of string
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
    | DottedForm of elements: MacroTemplateElement list * tail: MacroTemplate
    /// Produce a vector literal.
    | Vec of elements: MacroTemplateElement list

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
        let rec f l r =
            { Bindings = List.append l.Bindings r.Bindings
              Repeated = g l.Repeated r.Repeated }

        and g l r =
            match l, r with
            | [], a -> a
            | a, [] -> a
            | lh :: lt, rh :: rt -> f lh rh :: g lt rt

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

// -- Macros ----------------------------------------------------------------

type private PatternCtx =
    { Diags: DiagnosticBag
      Ellipsis: string option
      Literals: string list }

    static member Create diags ellipsis literals =
        // R7RS 4.3.2: if the ellipsis identifier appears in the literals list it
        // is treated as a literal keyword and loses its special "..." meaning.
        let activeEllipsis =
            if List.contains ellipsis literals then
                None
            else
                Some ellipsis

        { Diags = diags
          Ellipsis = activeEllipsis
          Literals = literals }

module MacroParse =

    /// Parse a literal list `(lit1 lit2 ...)` from the syntax of a `syntax-rules` form.
    /// Emits diagnostics and returns an empty list on failure.
    let parseLiteralList (diags: DiagnosticBag) (stx: Stx) : string list =
        let unwrapId s =
            match s with
            | StxId(name, _, _) -> Some name
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
        let recurse = (parsePattern ctx depth scope)

        match stx with
        | StxDatum(d, _) -> MacroPattern.Constant d, scope
        | StxId("_", _, _) -> MacroPattern.Underscore, scope
        | StxId(name, _, envOpt) ->
            if List.contains name ctx.Literals then
                MacroPattern.Literal name, scope
            else
                // Resolve the identifier's binding in its home environment (the definition-site scope,
                // or the injected closure scope for identifiers injected by an outer macro expansion).
                // Storing the resolved binding alongside the depth lets `parseTemplate` match a template
                // reference to this exact binding, rather than just comparing name strings.
                let activeEnv = envOpt |> Option.defaultValue ambientEnv
                let resolvedId = Map.tryFind name activeEnv
                MacroPattern.Variable name, scope.Add(name, (resolvedId, depth))
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

        // TODO: If the _first_ iem in this list is an ellipsis it is supposed to 'quote' the ellipsis identifier for
        //       all patterns beneath it. Properly handling this needs a little thought.
        loop [] scope items

    let rec private parseTemplate
        (ctx: PatternCtx)
        (scope: Map<string, StxBinding option * int>)
        (depth: int)
        (ambientEnv: StxEnvironment)
        (stx: Stx)
        : MacroTemplate =
        let recurse = (parseTemplate ctx scope depth ambientEnv)

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

                MacroTemplate.Subst name
            | _ -> MacroTemplate.Quoted stx
        | StxList(items, tail, loc, envOpt) ->
            let ambientEnv = envOpt |> Option.defaultValue ambientEnv
            // R7RS 4.3.2: (〈ellipsis〉 〈template〉) is an ellipsis-escape form.
            // The single inner template is parsed with no active ellipsis so that
            // occurrences of the ellipsis identifier are treated as plain identifiers.
            match ctx.Ellipsis, items, tail with
            | Some ell, [ StxId(name, _, _); inner ], None when name = ell ->
                parseTemplate { ctx with Ellipsis = None } scope depth ambientEnv inner
            | Some ell, (StxId(name, _, _) :: _), _ when name = ell ->
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
                    MacroTemplate.DottedForm(elements, tailTmpl)
                | None -> MacroTemplate.Form(loc, elements)
        | StxVec(items, loc, envOpt) ->
            let ambientEnv = envOpt |> Option.defaultValue ambientEnv
            let elements = parseTemplateElementList ctx scope depth ambientEnv items
            MacroTemplate.Vec elements
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
        (lits: string list)
        (defScope: StxEnvironment)
        (stx: Stx)
        : MacroRule =
        match stx with
        | StxList([ pat; tmpl ], _, _, maybeDefEnv) ->
            let defScope = maybeDefEnv |> Option.defaultValue defScope
            let patternCtx = PatternCtx.Create diags ellipsis lits
            let pattern, patternScope = parsePattern patternCtx 0 Map.empty defScope pat
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
        (literals: string list)
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
            let lits = parseLiteralList diags lits
            let rules = parseSyntaxRulesBody diags id ellipsis lits rules defScope

            { Transformers = rules
              DefScope = defScope
              DefLoc = syntaxRulesSyn.Loc }

        match syntaxRulesSyn with
        | StxList(_ :: StxId(ellipsis, _, _) :: lits :: rules, _, _, maybeDefEnv) ->
            parseBody ellipsis lits rules maybeDefEnv |> Some
        | StxList(_ :: lits :: rules, _, _, maybeDefEnv) -> parseBody "..." lits rules maybeDefEnv |> Some
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

        | MacroPattern.Literal lit, Stx.Id(name, _) -> if name = lit then Some MacroBindings.Empty else None

        | MacroPattern.Constant patLit, Stx.Datum(stxLit, _) ->
            if patLit = stxLit then Some MacroBindings.Empty else None

        | MacroPattern.Form pats, Stx.List(items, None, _) -> matchPatternList pats None items None scope'

        | MacroPattern.DottedForm(pats, tailPat), Stx.List(items, Some tail, _) ->
            matchPatternList pats (Some tailPat) items (Some tail) scope'

        | MacroPattern.DottedForm(pats, tailPat), Stx.List(items, None, _) ->
            matchPatternList pats (Some tailPat) items None scope'

        | MacroPattern.Vec pats, Stx.Vec(items, _) -> matchPatternList pats None items None scope'

        | _ -> None

    and matchPatternList
        (pats: MacroPattern list)
        (tailPat: MacroPattern option)
        (items: Stx list)
        (tailItem: Stx option)
        (scope: StxEnvironment)
        : MacroBindings option =
        match pats with
        | MacroPattern.Repeat inner :: restPats -> matchRepeat inner restPats tailPat items tailItem scope []

        | headPat :: restPats ->
            match items with
            | headItem :: restItems ->
                matchPattern headPat headItem scope
                |> Option.bind (fun b1 ->
                    matchPatternList restPats tailPat restItems tailItem scope
                    |> Option.map (MacroBindings.Union b1))
            | [] -> None

        | [] ->
            match tailPat with
            | Some tp ->
                match tailItem with
                | Some t -> matchPattern tp t scope
                | None ->
                    // No explicit tail in input - construct tail from remaining items
                    let tailStx =
                        match items with
                        | [ single ] -> single
                        | _ -> Stx.List(items, None, TextLocation.Missing)

                    matchPattern tp tailStx scope
            | None ->
                if List.isEmpty items && tailItem.IsNone then
                    Some MacroBindings.Empty
                else
                    None

    and matchRepeat
        (inner: MacroPattern)
        (restPats: MacroPattern list)
        (tailPat: MacroPattern option)
        (items: Stx list)
        (tailItem: Stx option)
        (scope: StxEnvironment)
        (accumulated: MacroBindings list)
        : MacroBindings option =
        match matchPatternList restPats tailPat items tailItem scope with
        | Some tailBindings ->
            let repeatedPart =
                { MacroBindings.Empty with
                    Repeated = List.rev accumulated }

            Some(MacroBindings.Union tailBindings repeatedPart)
        | None ->
            match items with
            | head :: rest ->
                matchPattern inner head scope
                |> Option.bind (fun b -> matchRepeat inner restPats tailPat rest tailItem scope (b :: accumulated))
            | [] -> None

    // -- Transcription --------------------------------------------------------

    and transcribe
        (template: MacroTemplate)
        (bindings: MacroBindings)
        (defScope: StxEnvironment)
        (loc: TextLocation)
        (diag: DiagnosticBag)
        : Stx =
        match template with
        | MacroTemplate.Subst name ->
            match List.tryFind (fun (n, _) -> n = name) bindings.Bindings with
            | Some(_, (stx, capturedScope)) -> Stx.Closure(stx, capturedScope, loc)
            | None ->
                diag.Emit
                    MacroDiagnostics.invalidMacro
                    loc
                    $"template variable '{name}' is not bound at this repetition level"

                Stx.List([], None, loc)

        | MacroTemplate.Quoted stx -> stx

        | MacroTemplate.Form(_, elements) ->
            let children =
                elements |> List.collect (transcribeElement bindings defScope loc diag)

            Stx.List(children, None, loc)

        | MacroTemplate.DottedForm(elems, tail) ->
            let children = elems |> List.collect (transcribeElement bindings defScope loc diag)
            Stx.List(children, Some(transcribe tail bindings defScope loc diag), loc)

        | MacroTemplate.Vec elements ->
            let children =
                elements |> List.collect (transcribeElement bindings defScope loc diag)

            Stx.Vec(children, loc)

    and transcribeElement
        (bindings: MacroBindings)
        (defScope: StxEnvironment)
        (loc: TextLocation)
        (diag: DiagnosticBag)
        (elem: MacroTemplateElement)
        : Stx list =
        match elem with
        | MacroTemplateElement.Template t -> [ transcribe t bindings defScope loc diag ]
        | MacroTemplateElement.Repeated t -> transcribeRepeated t bindings defScope loc diag

    and transcribeRepeated
        (template: MacroTemplate)
        (bindings: MacroBindings)
        (defScope: StxEnvironment)
        (loc: TextLocation)
        (diag: DiagnosticBag)
        : Stx list =
        // Collect all substitution variable names referenced anywhere in the template.
        // Used to detect whether a given per-repetition binding is the one that
        // owns this ellipsis expansion, or belongs to a sibling `...` at a different
        // nesting level — matching the old "skip if count = 0" behaviour.
        let rec templateVars tmpl =
            match tmpl with
            | MacroTemplate.Subst name -> Set.singleton name
            | MacroTemplate.Form(_, elems)
            | MacroTemplate.Vec elems -> elems |> List.map elemVars |> Set.unionMany
            | MacroTemplate.DottedForm(elems, tail) ->
                Set.union (elems |> List.map elemVars |> Set.unionMany) (templateVars tail)
            | MacroTemplate.Quoted _ -> Set.empty

        and elemVars elem =
            match elem with
            | MacroTemplateElement.Template t
            | MacroTemplateElement.Repeated t -> templateVars t

        let vars = templateVars template

        // Recursively check if a variable is present anywhere in a bindings tree,
        // including nested Repeated lists.
        let rec hasVarRecursive var bindings =
            // Check in flat bindings
            if List.exists (fun (n, _) -> n = var) bindings.Bindings then
                true
            else
                // Check in nested repetitions
                bindings.Repeated |> List.exists (hasVarRecursive var)

        bindings.Repeated
        |> List.choose (fun perRepBindings ->
            // FIXME: Transcription should use the same count-based approach as the old one. Previously we ignored
            //        recursive expansion failures where _no_ transcriptions occurred.

            // Skip iterations where none of the template's variables are present.
            // This handles nested ellipsis levels: e.g. `e1 ...` should produce []
            // when the current Repeated list belongs to an outer `c ...` expansion.
            let hasVar = vars |> Set.exists (fun v -> hasVarRecursive v perRepBindings)

            if hasVar || Set.isEmpty vars then
                Some(transcribe template perRepBindings defScope loc diag)
            else
                None)

    /// Public entry point: parse a `(syntax-rules ...)` form into a `SyntaxTransformer`.
    /// Returns a ready-to-call function, or `None` if the form is malformed.
    let makeSyntaxTransformer
        (name: string)
        (stx: Stx)
        (defScope: StxEnvironment)
        (diag: DiagnosticBag)
        : SyntaxTransformer option =
        MacroParse.parseSyntaxRulesStx diag name stx defScope
        |> Option.map (fun rules ->
            fun (form: Stx) (callScope: StxEnvironment) ->
                let result =
                    rules.Transformers
                    |> List.tryPick (fun (pat, tmpl) ->
                        matchPattern pat form callScope |> Option.map (fun b -> tmpl, b))

                match result with
                | None -> Result.Error "no macro rule matched the form"
                | Some(template, bindings) ->
                    let transcribed =
                        transcribe template bindings rules.DefScope form.Loc diag
                        |> (fun stx -> Stx.Closure(stx, rules.DefScope, form.Loc))


                    Result.Ok transcribed)
