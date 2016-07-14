module PopulateSDL

open System.Linq
open System.Numerics
open Ast
open AstSDL
open Antlr.SDL
open Antlr.Runtime.Tree
open Antlr.Runtime
open FsUtils

let inline fromAntlr (antlr: ^a when ^a: (static member toAst: ^a -> ^a')) : ^a' =
    (^a: (static member toAst: ^a -> ^a') (antlr))

let inline fromAntlrs antlrs = Seq.map fromAntlr antlrs

type Antlr = sdlParser
type PRParse = {
  tree: ITree
  filename: string
  tokens: IToken[]
}

type Antlr.pr_file_return with
  static member toAst(): PRFile =
      printfn "PARSING MA FILES"
      {
        clauses   = []
        systems   = []
        processes = []
      }

let buildFile (parse: PRParse) = 
    printfn "WHYY"
    Antlr.pr_file_return.toAst()

let buildAst (files:(ITree*string*array<IToken>) seq) =
    printfn "STUPID XAMARIN"
    printfn "%A" <| Seq.length files
    Seq.map (fun (t,s,ts) -> buildFile { tree = t; filename = s; tokens = ts }) files |> Seq.toList



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
