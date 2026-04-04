namespace Feersum.CompilerServices.Binding.New

open Feersum.CompilerServices.Text
open Feersum.CompilerServices.Diagnostics
open Feersum.CompilerServices.Binding

// ──────────────────────────────────── Pattern types

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

// ──────────────────────────────────── Template types

/// A macro template element — either a single sub-template or a repeated one.
[<RequireQualifiedAccess>]
type MacroTemplateElement =
    | Template of MacroTemplate
    | Repeated of MacroTemplate

/// Macro templates specify how to convert a match into new syntax. They are
/// effectively patterns for the output, with holes filled in by pattern variable matches.
and [<RequireQualifiedAccess>] MacroTemplate =
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

// ──────────────────────────────────── Capture types

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
        { Bindings = [ name, capture ]; Repeated = [] }

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

// ──────────────────────────────────── Macro type

/// A macro transformer: a single `(pattern template)` rule pair.
type MacroTransformer = MacroPattern * MacroTemplate

/// A macro: the set of transformer rules together with the definition-site
/// scope and source location.
type Macro =
    { Transformers: MacroTransformer list
      DefScope: StxEnvironment
      DefLoc: TextLocation }

// ──────────────────────────────────── Diagnostics

module private MacroDiagnostics =
    let invalidMacro =
        DiagnosticKind.Create DiagnosticLevel.Error 56 "Invalid macro definition"

// ─────────────────────────────────────────────────────────────── MacrosNew

/// Parse `(syntax-rules ...)` forms into `SyntaxTransformer` values.
/// Only the functions consumed by `Expander` are public.
module MacrosNew =

    // ── Helpers ──────────────────────────────────────────────────────────

    /// Collect all bound pattern variable names from a pattern.
    let rec findBound (pat: MacroPattern) : string list =
        match pat with
        | MacroPattern.Variable name -> [ name ]
        | MacroPattern.Form pats -> pats |> List.collect findBound
        | MacroPattern.DottedForm(pats, tail) -> (pats |> List.collect findBound) @ findBound tail
        | MacroPattern.Vec pats -> pats |> List.collect findBound
        | MacroPattern.Repeat inner -> findBound inner
        | _ -> []

    /// Parse a pattern literal list `(lit1 lit2 ...)`.
    let parseLiteralList (stx: Stx) : Result<string list, string> =
        let rec unwrapId s =
            match s with
            | Stx.Id(name, _) -> Result.Ok name
            | Stx.Closure(inner, _, _) -> unwrapId inner
            | _ -> Result.Error "literal list must contain identifiers"

        let rec unwrapList s =
            match s with
            | Stx.List(items, _, _) ->
                items
                |> List.map unwrapId
                |> List.fold
                    (fun acc r ->
                        match acc, r with
                        | Result.Ok xs, Result.Ok x -> Result.Ok(xs @ [ x ])
                        | Result.Error e, _ -> Result.Error e
                        | _, Result.Error e -> Result.Error e)
                    (Result.Ok [])
            | Stx.Closure(inner, _, _) -> unwrapList inner
            | _ -> Result.Error "expected list of literals"

        unwrapList stx

    /// Returns true if `stx` is the ellipsis identifier `ell`, peeling any
    /// `Stx.Closure` wrappers.
    let rec private isEllipsisNode (ell: string) (stx: Stx) : bool =
        match stx with
        | Stx.Id(name, _) -> name = ell
        | Stx.Closure(inner, _, _) -> isEllipsisNode ell inner
        | _ -> false

    /// Parse `(pats...)` or `(pats... . tail)` form body into a list of
    /// patterns, detecting ellipsis.
    let rec parseFormBody
        (kw: string)
        (ellipsis: string)
        (lits: string list)
        (items: Stx list)
        (tail: Stx option)
        : Result<MacroPattern list * MacroPattern option, string> =
        let rec loop acc remaining =
            match remaining with
            | [] ->
                let tailPat =
                    tail |> Option.map (fun t -> parsePattern kw ellipsis lits t)

                match tailPat with
                | None -> Result.Ok(List.rev acc, None)
                | Some(Result.Ok tp) -> Result.Ok(List.rev acc, Some tp)
                | Some(Result.Error e) -> Result.Error e
            | [ single ] ->
                match parsePattern kw ellipsis lits single with
                | Result.Error e -> Result.Error e
                | Result.Ok pat -> loop (pat :: acc) []
            | current :: (next :: rest) when isEllipsisNode ellipsis next ->
                match parsePattern kw ellipsis lits current with
                | Result.Error e -> Result.Error e
                | Result.Ok inner -> loop (MacroPattern.Repeat inner :: acc) rest
            | current :: rest ->
                match parsePattern kw ellipsis lits current with
                | Result.Error e -> Result.Error e
                | Result.Ok pat -> loop (pat :: acc) rest

        loop [] items

    /// Parse a single syntax node as a macro pattern.
    and parsePattern (kw: string) (ellipsis: string) (lits: string list) (stx: Stx) : Result<MacroPattern, string> =
        match stx with
        | Stx.Closure(inner, _, _) -> parsePattern kw ellipsis lits inner
        | Stx.Datum(d, _) -> Result.Ok(MacroPattern.Constant d)
        | Stx.Id("_", _) -> Result.Ok MacroPattern.Underscore
        | Stx.Id(name, _) when name = ellipsis && not (List.contains name lits) ->
            Result.Error "unexpected ellipsis in pattern"
        | Stx.Id(name, _) ->
            if List.contains name lits then
                Result.Ok(MacroPattern.Literal name)
            else
                Result.Ok(MacroPattern.Variable name)
        | Stx.List(items, None, _) ->
            match items with
            | Stx.Id(head, _) :: rest when head = kw ->
                match parseFormBody kw ellipsis lits rest None with
                | Result.Error e -> Result.Error e
                | Result.Ok(pats, None) -> Result.Ok(MacroPattern.Form(MacroPattern.Variable kw :: pats))
                | Result.Ok(pats, Some tail) ->
                    Result.Ok(MacroPattern.DottedForm(MacroPattern.Variable kw :: pats, tail))
            | _ ->
                match parseFormBody kw ellipsis lits items None with
                | Result.Error e -> Result.Error e
                | Result.Ok(pats, None) -> Result.Ok(MacroPattern.Form pats)
                | Result.Ok(pats, Some tail) -> Result.Ok(MacroPattern.DottedForm(pats, tail))
        | Stx.List(items, Some tail, _) ->
            match parseFormBody kw ellipsis lits items (Some tail) with
            | Result.Error e -> Result.Error e
            | Result.Ok(pats, tailOpt) ->
                let tailPat = tailOpt |> Option.defaultValue MacroPattern.Underscore
                Result.Ok(MacroPattern.DottedForm(pats, tailPat))
        | Stx.Vec(items, _) ->
            let results = items |> List.map (parsePattern kw ellipsis lits)

            results
            |> List.fold
                (fun acc r ->
                    match acc, r with
                    | Result.Ok xs, Result.Ok x -> Result.Ok(xs @ [ x ])
                    | Result.Error e, _ -> Result.Error e
                    | _, Result.Error e -> Result.Error e)
                (Result.Ok [])
            |> Result.map MacroPattern.Vec
        | Stx.Error _ -> Result.Error "malformed syntax node"

    /// Parse `(tmpl...)` or `(tmpl... . tail)` template body, detecting ellipsis.
    let rec parseTemplateFormBody
        (ellipsis: string)
        (bound: string list)
        (items: Stx list)
        (tail: Stx option)
        : Result<MacroTemplateElement list * MacroTemplate option, string> =
        let rec loop acc remaining =
            match remaining with
            | [] ->
                let tailTmpl = tail |> Option.map (fun t -> parseTemplate ellipsis bound t)

                match tailTmpl with
                | None -> Result.Ok(List.rev acc, None)
                | Some(Result.Ok tp) -> Result.Ok(List.rev acc, Some tp)
                | Some(Result.Error e) -> Result.Error e
            | [ single ] ->
                match parseTemplate ellipsis bound single with
                | Result.Error e -> Result.Error e
                | Result.Ok tmpl -> loop (MacroTemplateElement.Template tmpl :: acc) []
            | current :: (next :: rest) when isEllipsisNode ellipsis next ->
                match parseTemplate ellipsis bound current with
                | Result.Error e -> Result.Error e
                | Result.Ok tmpl -> loop (MacroTemplateElement.Repeated tmpl :: acc) rest
            | current :: rest ->
                match parseTemplate ellipsis bound current with
                | Result.Error e -> Result.Error e
                | Result.Ok tmpl -> loop (MacroTemplateElement.Template tmpl :: acc) rest

        loop [] items

    /// Parse a single syntax node as a macro template.
    and parseTemplate (ellipsis: string) (bound: string list) (stx: Stx) : Result<MacroTemplate, string> =
        match stx with
        | Stx.Closure(inner, _, _) -> parseTemplate ellipsis bound inner
        | Stx.Id(name, _) ->
            if List.contains name bound then
                Result.Ok(MacroTemplate.Subst name)
            else
                Result.Ok(MacroTemplate.Quoted stx)
        | Stx.List(items, None, loc) ->
            match parseTemplateFormBody ellipsis bound items None with
            | Result.Error e -> Result.Error e
            | Result.Ok(elems, None) -> Result.Ok(MacroTemplate.Form(loc, elems))
            | Result.Ok(elems, Some tail) -> Result.Ok(MacroTemplate.DottedForm(elems, tail))
        | Stx.List(items, Some tail, loc) ->
            match parseTemplateFormBody ellipsis bound items (Some tail) with
            | Result.Error e -> Result.Error e
            | Result.Ok(elems, tailOpt) ->
                let tailTmpl = tailOpt |> Option.defaultValue (MacroTemplate.Quoted stx)
                Result.Ok(MacroTemplate.DottedForm(elems, tailTmpl))
        | Stx.Vec(items, _) ->
            let results = items |> List.map (parseTemplate ellipsis bound)

            results
            |> List.fold
                (fun acc r ->
                    match acc, r with
                    | Result.Ok xs, Result.Ok x -> Result.Ok(xs @ [ MacroTemplateElement.Template x ])
                    | Result.Error e, _ -> Result.Error e
                    | _, Result.Error e -> Result.Error e)
                (Result.Ok [])
            |> Result.map MacroTemplate.Vec
        | other ->
            Result.Ok(MacroTemplate.Quoted other)

    /// Parse one `(pattern template)` transformer arm.
    let parseTransformer
        (kw: string)
        (ellipsis: string)
        (lits: string list)
        (stx: Stx)
        : Result<MacroTransformer, string> =
        match stx with
        | Stx.List([ pat; tmpl ], _, _) ->
            match parsePattern kw ellipsis lits pat with
            | Result.Error e -> Result.Error e
            | Result.Ok patParsed ->
                let bound = findBound patParsed

                match parseTemplate ellipsis bound tmpl with
                | Result.Error e -> Result.Error e
                | Result.Ok tmplParsed -> Result.Ok(patParsed, tmplParsed)
        | _ -> Result.Error "each syntax-rules rule must be (pattern template)"

    /// Parse the body of `(syntax-rules (lits...) rule...)`.
    let parseSyntaxRulesBody
        (kw: string)
        (ellipsis: string)
        (litsStx: Stx)
        (rules: Stx list)
        (defScope: StxEnvironment)
        (diag: DiagnosticBag)
        (loc: TextLocation)
        : Macro option =
        match parseLiteralList litsStx with
        | Result.Error e ->
            diag.Emit MacroDiagnostics.invalidMacro loc e
            None
        | Result.Ok lits ->
            let results = rules |> List.map (parseTransformer kw ellipsis lits)

            let transformers =
                results
                |> List.choose (function
                    | Result.Ok pair -> Some pair
                    | Result.Error e ->
                        diag.Emit MacroDiagnostics.invalidMacro loc e
                        None)

            Some
                { Transformers = transformers
                  DefScope = defScope
                  DefLoc = loc }

    let rec private stxIdName (stx: Stx) : string option =
        match stx with
        | Stx.Id(name, _) -> Some name
        | Stx.Closure(inner, _, _) -> stxIdName inner
        | _ -> None

    let rec private stxUnwrapList (stx: Stx) : Stx option =
        match stx with
        | Stx.List _ -> Some stx
        | Stx.Closure(inner, _, _) -> stxUnwrapList inner
        | _ -> None

    /// Public entry point: parse a `(syntax-rules ...)` form into a `Macro`.
    /// Supports the R7RS extended form `(syntax-rules <ellipsis> (lits...) rules...)`
    /// where an optional identifier before the literal list names the ellipsis.
    let parseSyntaxRulesStx
        (name: string)
        (stx: Stx)
        (defScope: StxEnvironment)
        (diag: DiagnosticBag)
        (loc: TextLocation)
        : Macro option =
        let rec unwrap s =
            match s with
            | Stx.Closure(inner, _, _) -> unwrap inner
            | _ -> s

        match unwrap stx with
        | Stx.List(_ :: second :: rest, _, _) ->
            match stxIdName second, rest with
            | Some ellipsis, litsStx :: rules when stxUnwrapList litsStx |> Option.isSome ->
                parseSyntaxRulesBody name ellipsis litsStx rules defScope diag loc
            | None, rules when stxUnwrapList second |> Option.isSome ->
                parseSyntaxRulesBody name "..." second rules defScope diag loc
            | _ ->
                diag.Emit MacroDiagnostics.invalidMacro loc "expected (syntax-rules (literals...) rules...)"
                None
        | _ ->
            diag.Emit MacroDiagnostics.invalidMacro loc "expected (syntax-rules (literals...) rules...)"
            None

    // ── Pattern matching ──────────────────────────────────────────────────

    /// Attempt to match a MacroPattern against a syntax node.
    /// Returns Some MacroBindings on success, None on mismatch.
    let rec matchPattern (pat: MacroPattern) (stx: Stx) (scope: StxEnvironment) : MacroBindings option =
        match pat, stx with
        | MacroPattern.Underscore, _ -> Some MacroBindings.Empty

        | MacroPattern.Variable v, s -> Some(MacroBindings.FromVariable v (s, scope))

        | MacroPattern.Literal lit, Stx.Id(name, _) ->
            if name = lit then Some MacroBindings.Empty else None

        | MacroPattern.Literal lit, Stx.Closure(inner, closedScope, _) ->
            matchPattern (MacroPattern.Literal lit) inner closedScope

        | MacroPattern.Constant patLit, Stx.Datum(stxLit, _) ->
            if patLit = stxLit then Some MacroBindings.Empty else None

        | MacroPattern.Form pats, Stx.List(items, None, _) ->
            matchPatternList pats None items None scope

        | MacroPattern.DottedForm(pats, tailPat), Stx.List(items, Some tail, _) ->
            matchPatternList pats (Some tailPat) items (Some tail) scope

        | MacroPattern.DottedForm(pats, tailPat), Stx.List(items, None, _) ->
            matchPatternList pats (Some tailPat) items None scope

        | MacroPattern.Form pats, Stx.Closure(inner, closedScope, _) ->
            matchPattern (MacroPattern.Form pats) inner closedScope

        | MacroPattern.DottedForm(pats, tailPat), Stx.Closure(inner, closedScope, _) ->
            matchPattern (MacroPattern.DottedForm(pats, tailPat)) inner closedScope

        | MacroPattern.Vec pats, Stx.Vec(items, _) ->
            matchPatternList pats None items None scope

        | _ -> None

    and matchPatternList
        (pats: MacroPattern list)
        (tailPat: MacroPattern option)
        (items: Stx list)
        (tailItem: Stx option)
        (scope: StxEnvironment)
        : MacroBindings option =
        match pats with
        | MacroPattern.Repeat inner :: restPats ->
            matchRepeat inner restPats tailPat items tailItem scope []

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
                    match items with
                    | [ single ] -> matchPattern tp single scope
                    | _ when not (List.isEmpty items) ->
                        match tp with
                        | MacroPattern.Underscore -> Some MacroBindings.Empty
                        | MacroPattern.Variable v ->
                            Some(
                                MacroBindings.FromVariable
                                    v
                                    (Stx.List(items, None, TextLocation.Missing), scope)
                            )
                        | _ -> None
                    | _ -> None
            | None -> if List.isEmpty items && tailItem.IsNone then Some MacroBindings.Empty else None

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
            let repeatedPart = { MacroBindings.Empty with Repeated = List.rev accumulated }
            Some(MacroBindings.Union tailBindings repeatedPart)
        | None ->
            match items with
            | head :: rest ->
                matchPattern inner head scope
                |> Option.bind (fun b ->
                    matchRepeat inner restPats tailPat rest tailItem scope (b :: accumulated))
            | [] -> None

    // ── Transcription ─────────────────────────────────────────────────────

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
                diag.Emit MacroDiagnostics.invalidMacro loc $"template variable '{name}' is not bound at this repetition level"
                Stx.List([], None, loc)

        | MacroTemplate.Quoted stx ->
            Stx.Closure(stx, defScope, loc)

        | MacroTemplate.Form(_, elements) ->
            let children = elements |> List.collect (transcribeElement bindings defScope loc diag)
            Stx.List(children, None, loc)

        | MacroTemplate.DottedForm(elems, tail) ->
            let children = elems |> List.collect (transcribeElement bindings defScope loc diag)
            Stx.List(children, Some(transcribe tail bindings defScope loc diag), loc)

        | MacroTemplate.Vec elements ->
            let children = elements |> List.collect (transcribeElement bindings defScope loc diag)
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
            | MacroTemplate.Form(_, elems) | MacroTemplate.Vec elems ->
                elems |> List.map elemVars |> Set.unionMany
            | MacroTemplate.DottedForm(elems, tail) ->
                Set.union (elems |> List.map elemVars |> Set.unionMany) (templateVars tail)
            | MacroTemplate.Quoted _ -> Set.empty

        and elemVars elem =
            match elem with
            | MacroTemplateElement.Template t | MacroTemplateElement.Repeated t -> templateVars t

        let vars = templateVars template

        bindings.Repeated
        |> List.choose (fun perRepBindings ->
            // Skip iterations where none of the template's variables are present.
            // This handles nested ellipsis levels: e.g. `e1 ...` should produce []
            // when the current Repeated list belongs to an outer `c ...` expansion.
            let hasVar = vars |> Set.exists (fun v -> perRepBindings.Bindings |> List.exists (fun (n, _) -> n = v))

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
        (loc: TextLocation)
        : SyntaxTransformer option =
        parseSyntaxRulesStx name stx defScope diag loc
        |> Option.map (fun rules ->
            fun (form: Stx) (callScope: StxEnvironment) ->
                let result =
                    rules.Transformers
                    |> List.tryPick (fun (pat, tmpl) ->
                        matchPattern pat form callScope |> Option.map (fun b -> tmpl, b))

                match result with
                | None -> Result.Error "no macro rule matched the form"
                | Some(template, bindings) ->
                    let transcribed = transcribe template bindings rules.DefScope form.Loc diag
                    Result.Ok transcribed)
