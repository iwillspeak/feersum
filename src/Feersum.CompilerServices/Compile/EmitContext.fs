namespace Feersum.CompilerServices.Compile

open Mono.Cecil
open Mono.Cecil.Cil
open System.Collections.Generic

open Feersum.CompilerServices.Text

/// Capture Environment Kind
///
/// Capture environments are either standard environments containing fields used
/// to store captured values; or link environments.
type EnvInfo =
    /// A standard environment represents a closure type containing each
    /// captured value as a field. If the environment has a parent then that is
    /// stored as the final field.
    | Standard of local: VariableDefinition * ty: TypeDefinition * parent: EnvInfo option
    /// Link environments represent the case when only the parent environment
    /// is captured. In this case we can re-use the parent type and pointer at
    /// runtime. This saves the cost of a new type just for this closure's
    /// methods, and saves indirection on lookups for deeply nested captures.
    | Link of EnvInfo

module private EnvUtils =
    /// Get the local variable used to store the current method's environment
    let getLocal =
        function
        | Standard(local, _, _) -> local |> Some
        | _ -> None

    /// Get the type part of the environment info.
    let rec getType =
        function
        | Standard(_, ty, _) -> ty
        | Link inner -> getType inner

    /// Get the parent of the given environment.
    let rec getParent =
        function
        | Standard(_, _, parent) -> parent
        | Link inner -> Some(inner)

/// Type to Hold Context While Emitting IL
type EmitCtx =
    {
        Assm: AssemblyDefinition
        IL: ILProcessor
        Locals: VariableDefinition list
        Parameters: ParameterDefinition list
        DebugDocuments: Dictionary<string, Document>
        /// Stub document used as the document argument for PDB hidden sequence
        /// points (i.e. those arising from synthetic/macro-expanded nodes that
        /// have no real source location). Mono.Cecil requires a non-null Document
        /// even for hidden sequence points.
        MissingDocument: Document
        mutable NextLambda: int
        ScopePrefix: string
        EmitSymbols: bool
        Exports: Map<string, string>
        Externs: Map<string, TypeDefinition>
        Environment: EnvInfo option
        ProgramTy: TypeDefinition
        mutable Libraries: Map<string, TypeDefinition>
        Core: CoreTypes
        Registry: SourceRegistry
    }
