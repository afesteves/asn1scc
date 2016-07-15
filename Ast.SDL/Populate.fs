module PopulateSDL

open System.Linq
open System.Numerics
open Ast
open AstSDL
open Antlr.SDL
open Antlr.Runtime.Tree
open Antlr.Runtime
open FsUtils
open FSharpx

type P = sdlParser
let print x = printfn "%A" x
(*
let validator pred error value =
    if pred value
    then Choice1Of2 value
    else Choice2Of2 (Collections.NonEmptyList

type PRFileEntity = Choice<System, Process, UseClause>

let fileAst (tree: ITree, filename: string, tokens: IToken[]) = 
  let system  (t: ITree) = Success 1
  let clause  (t: ITree) = Success 2
  let procezz (t: ITree) = Failure ["a"]
                                                     
  let entity(t: ITree) = 
    match tree.Type with
    | P.USE     -> clause(t)
    | P.SYSTEM  -> system(t)
    | P.PROCESS -> procezz(t) 
    | _ -> err(SyntaxErr)

  printfn "%A" filename
  match tree.Type with
    | P.PR_FILE -> Seq.map entity tree.Children
    | _ -> err(SyntaxErr)

*)
let buildAst (files: (ITree * string * IToken[]) seq) =
    printfn "Building %A files" <| Seq.length files
    //Seq.map fileAst files |> Seq.toList |> print

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
