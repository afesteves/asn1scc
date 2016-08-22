module Parse

open Antlr.Runtime.Tree
open FsUtils
open FSharpx
open FSharpx.Collections
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

let one w =
    let (token, Parse f) = w()
    Parse (fun (t,c) -> 
      let consumed =
        List.skip c (getTreeChildren t)
        |> head
        |> Option.filter (fun t' -> t'.Type = token)
      
      match consumed with
      | Some t' -> f (t', 0) |> fun (x, _) -> (x, t'.ChildIndex + 1)
      | None -> (None, c)
    )

let recursive fp = Parse (fun t -> run (fp()) t)

let fail = Parse (fun (t,c) -> (None, c))

let opt w = (one w |>> Some) <|> pure None
let rec many w = (one w >>= (fun h -> many w |>> (fun t -> cons h t))) <|> pure []
let rec many1 w = lift2 NonEmptyList.create (one w) (many w)

/// Need better name
let rec _many parser = (parser >>= (fun h -> _many parser |>> (fun t -> cons h t))) <|> pure []

let groups2 p = _many p |>> partitions2
let groups3 p = _many p |>> partitions3
let groups4 p = _many p |>> partitions4
let groups5 p = _many p |>> partitions5
let groups6 p = _many p |>> partitions6
let groups7 p = _many p |>> partitions7

/// Need better name
let rec many' parser = (parser >>= (fun h -> many' parser |>> (fun t -> cons h t))) <|> pure []

let choice ps = Seq.fold (<|>) fail ps

let choice2 (a, b) =
        (a |>> Choice1Of2)
    <|> (b |>> Choice2Of2)

let choice3 (a, b, c) =
        (a |>> Choice1Of3)
    <|> (b |>> Choice2Of3)
    <|> (c |>> Choice3Of3)

let choice4 (a, b, c, d) =
        (a |>> Choice1Of4)
    <|> (b |>> Choice2Of4)
    <|> (c |>> Choice3Of4)
    <|> (d |>> Choice4Of4)

let choice5 (a, b, c, d, e) =
        (a |>> Choice1Of5)
    <|> (b |>> Choice2Of5)
    <|> (c |>> Choice3Of5)
    <|> (d |>> Choice4Of5)
    <|> (e |>> Choice5Of5)

let choice6 (a, b, c, d, e, f) =
        (a |>> Choice1Of6)
    <|> (b |>> Choice2Of6)
    <|> (c |>> Choice3Of6)
    <|> (d |>> Choice4Of6)
    <|> (e |>> Choice5Of6)
    <|> (f |>> Choice6Of6)

let choice7 (a, b, c, d, e, f, g) =
        (a |>> Choice1Of7)
    <|> (b |>> Choice2Of7)
    <|> (c |>> Choice3Of7)
    <|> (d |>> Choice4Of7)
    <|> (e |>> Choice5Of7)
    <|> (f |>> Choice6Of7)
    <|> (g |>> Choice7Of7)
