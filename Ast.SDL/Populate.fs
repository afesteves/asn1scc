module PopulateAST

open System.Linq
open System.Numerics
open Ast.SDL
open Antlr.SDL
open Antlr.Runtime.Tree
open Antlr.Runtime

let inline fromAntlr (antlr: ^a when ^a: (static member toAst: ^a -> ^a')) : ^a' =
    (^a: (static member toAst: ^a -> ^a') (antlr))

let inline fromAntlrs antlrs = Seq.map fromAntlr antlrs

type Antlr = sdlParser

type Antlr.pr_file_return with
  static member toAst(): PRFile =
      printfn "PARSING MA FILES"
      {
        clauses   = []
        systems   = []
        processes = []
      }

(*
and Terminator =
    | NextState of state: State option
    | Join of connector: ID
    | Stop
    | ReturnStatement of Expr option
and TerminatorStatement = {
    label: Label option
    cif: CIFCoordinates option
    hyperlink: Hyperlink option
    terminator: Terminator
    cifEnd: CIFEnd
}
*)
