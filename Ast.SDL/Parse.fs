module Parse

open Antlr.Runtime.Tree
open FsUtils
open FSharpx
open FSharpx.Collections
open FSharpx.Option
open Utils

#nowarn "0046"
#nowarn "1189"

let castTree (x:ITree) =
  match x with
  | :? CommonTree as x' -> Some x'
  | _ -> None

type CurrentChild = int
type Parse<'a> = Parse of (ITree * CurrentChild -> 'a option * CurrentChild)

let inline pure x = Parse (fun (t,c) -> (Some x, c))

let log = printfn "%s: %s"

let warn = log "Warning"

let run (Parse f) (tree, child) : ('a option * int) = 
  match f (tree, child) with
  | (Some a, c) -> 
    printfn "PARSED %A" a
    (Some a, c)
  | (None, c) -> 
    let funcType = typeof<Microsoft.FSharp.Core.FSharpFunc<_,_>>
    let parsingType = typeof<'a>

    if funcType.Name = parsingType.Name 
    then (None, c)
    else
      let name = parsingType.ToString()
      let parentToken = castTree tree |> Option.map (fun t' -> t'.Token.Text) |> Option.getOrElse "|UNKNOWN|"
      printfn "Error at line %A, inside %A while building %A" tree.Line parentToken name
      (None, c)

let map f (Parse g) = Parse (g >> fun (x,c) -> (Option.map f x, c))

let (|>>) p f = map f p

let (<*>) pf px =
  Parse (fun (t,c) ->
    match (run pf (t,c)) with
    | (Some f, c') -> run px (t,c') |> fun (x,c) -> (Option.map f x, c)
    | (None, c') -> (None, c')
  )

let (>>=) p f =
  Parse (fun (t,c) ->
    match (run p (t,c)) with
    | (Some x, c') -> run (f x) (t, c')
    | (None, c') -> (None, c')
  )


let lift2 f x y = pure f <*> x <*> y

let rec sequence (parsers: 'a Parse list) : 'a list Parse = 
  List.foldBack (lift2 cons) parsers (pure []) 

let traverse f = List.map f >> sequence

let (<|>) pa pb =
  Parse (fun (t,c) -> 
    match (run pa (t,c)) with
    | (Some a, c') -> (Some a, c')
    | (None, c')   -> run pb (t,c)
  )

let one token (Parse f) = 
    Parse (fun (t,c) -> 
      let consumed = List.skip c (getTreeChildren t)
                  |> List.tryFind (fun t' -> t'.Type = token)
      
      match consumed with
      | Some t' -> f (t', 0) |> fun (x, _) -> (x, t'.ChildIndex + 1)
      | None -> (None, c)
    )

let opt token parser = (one token parser |>> Some) <|> pure None

let rec many  token parser = (one token parser >>= (fun h -> many token parser |>> (fun t -> cons h t))) <|> pure [] 

let rec many1 token parser = lift2 NonEmptyList.create (one token parser) (many token parser)

let recursive fp = Parse (fun t -> run (fp()) t)

let fail = Parse (fun (t,c) -> (None, c))

let choice ps = Seq.fold (<|>) fail ps
