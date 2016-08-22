module Gateway

open Antlr.Runtime.Tree
open Antlr.Runtime
open AstSDL
open TreeParser
open Parsers
open Utils

let attemptFile (file: ITree * string * IToken[]) =
  let (t, _, _) = file
  if t.Type = P.PR_FILE then run attemptPRFile (t, 0) |> fst else None
    
let modulesAst (files: (ITree * string * IToken[]) seq): PRFile option seq =
  Seq.length files |> printfn "Building %A files"
  let res = Seq.map attemptFile files
  print res
  res
