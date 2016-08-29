module TreeParser

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

type ChildIdentifier = int
type ParserError = string list
type ParserInput = ITree
type ParserState = ChildIdentifier
type ParserResult <'a> = Choice<'a, ParserError>
type Parser <'a> = Parser of (ParserInput * ParserState -> 'a ParserResult * ParserState)

let (|Output|Error|) (r: 'a ParserResult) =
    match r with
    | Choice1Of2 o -> Output o
    | Choice2Of2 e -> Error e

let Output = Choice1Of2
let Error = Choice2Of2

let mapResult (f: 'a -> 'b) (r: 'a ParserResult) : 'b ParserResult =
  match r with
  | Output o -> Output (f o)
  | Error e -> Error e

let inline pure x = Parser (fun (t,c) -> (Output x, c))

let run (Parser f) (tree, child) = f (tree, child) 

let map f (Parser g) = Parser (g >> fun (x,c) -> (mapResult f x, c))

let (|>>) p f = map f p

let (<*>) (pf: ('a -> 'b) Parser)  (px: 'a Parser) : 'b Parser =
  Parser (fun (t,c) ->
    let (f, c' ) = run pf (t,c)
    let (x, c'') = run px (t, c')

    let r =
      match (f,x) with
      | (Output f', Output x') -> Output (f' x')
      | (Output _, Error e) -> Error e
      | (Error e, Output _) -> Error e
      | (Error e, Error e') -> e @ e' |> Error

    (r, c'')
  )

let (>>=) p f =
  Parser (fun (t,c) ->
    match (run p (t,c)) with
    | (Output x, c') -> run (f x) (t, c')
    | (Error e, c') -> (Error e, c')
  )

let lift2 f x y = pure f <*> x <*> y

let rec sequence (parsers: 'a Parser list) : 'a list Parser =
  List.foldBack (lift2 cons) parsers (pure []) 

let traverse f = List.map f >> sequence

let (<|>) pa pb =
  Parser (fun (t,c) ->
    match (run pa (t,c)) with
    | (Output a, c') -> (Output a, c')
    | (Error _, _) -> run pb (t, c)
  )

let tokenOf tree =
  castTree tree |> Option.map (fun t' -> t'.Token.Text) |> Option.getOrElse "|UNKNOWN|"

let warn tree msg =
  printfn "Warning in %A, LINE %A: %s" (tokenOf tree) tree.Line msg

let err tree msg : 'a ParserResult =
  let parsingType = typeof<'a>
  let name = parsingType.ToString()
  let token = tokenOf tree
  let e = sprintf "Error at line %A, inside %A while building %A: %A" tree.Line token name msg
  Error [e]

let checkFullMatch (t': ITree) (c': ChildIdentifier) =
  let unmatched = t'.Children |> List.skip c'
  if unmatched = []
  then ()
  else
    let children = unmatched |> List.map (fun t -> sprintf "L%d/%s" t.Line (tokenOf t))
    "unmatched children: " + String.concat " " children |> warn t'

let one w =
    let (token, Parser f) = w()
    Parser (fun (t,c) -> 
      let consumed =
        List.skip c (getTreeChildren t)
        |> head
        |> Option.filter (fun t' -> t'.Type = token)
      
      match consumed with
      | Some t' -> f (t', 0) |> fun (r, c') ->
        checkFullMatch t' c'
        (r, t'.ChildIndex + 1)
      | None -> (err t "No matching token found" , c)
    )

//Anchor for breakpoints
let debug p = Parser (fun (t,c) ->
  run p (t,c)
)

let recursive fp = Parser (fun t -> run (fp()) t)

let fail = Parser (fun (t,c) -> (err t "HARDCODED FAIL", c))

let opt w = (one w |>> Some) <|> pure None

let exists token = opt (fun _ -> (token, pure ())) |>> Option.isSome

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
