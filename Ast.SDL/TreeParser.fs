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

/// Active pattern for ParserResult
let (|Output|Error|) (r: 'a ParserResult) =
    match r with
    | Choice1Of2 o -> Output o
    | Choice2Of2 e -> Error e

/// Synonym for Choice1Of2
let Output = Choice1Of2

/// Synonym for Choice2Of2
let Error = Choice2Of2

let mapResult (f: 'a -> 'b) (r: 'a ParserResult) : 'b ParserResult =
  match r with
  | Output o -> Output (f o)
  | Error e -> Error e

/// A parser that consumes no input and yields the specified value
let inline pure x = Parser (fun (t,c) -> (Output x, c))

/// Unwrap the parser function 
let run (Parser f) (tree, child) = f (tree, child)

/// Returns a parser that fails when the provided one fails, and outputs a function of the original output
let map f (Parser g) = Parser (g >> fun (x,c) -> (mapResult f x, c))

/// Operator form of map (same as FParsec)
let (|>>) p f = map f p

/// Apply a function inside the Parser context
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

/// Operator form of bind
let (>>=) p f =
  Parser (fun (t,c) ->
    match (run p (t,c)) with
    | (Output x, c') -> run (f x) (t, c')
    | (Error e, c') -> (Error e, c')
  )

/// Turn a function over two arguments into one over two parsers
let lift2 h a b = pure h <*> a <*> b

/// Turn a function over three arguments into one over three parsers
let lift3 h a b c = pure h <*> a <*> b <*> c

/// Turn a function over four arguments into one over four parsers
let lift4 h a b c d = pure h <*> a <*> b <*> c <*> d

/// Turn a function over five arguments into one over five parsers
let lift5 h a b c d e = pure h <*> a <*> b <*> c <*> d <*> e

/// Turn a function over six arguments into one over six parsers
let lift6 h a b c d e f = pure h <*> a <*> b <*> c <*> d <*> e <*> f

/// Turn a function over seven arguments into one over seven parsers
let lift7 h a b c d e f g = pure h <*> a <*> b <*> c <*> d <*> e <*> f <*> g


/// Run two parsers in sequence, keep the first's output
let (.>>) pa pb = lift2 (fun a _ -> a) pa pb

/// Run two parsers in sequence, keep the second's output
let (>>.) pa pb = lift2 (fun _ b -> b) pa pb

/// Run all parsers in sequence and yield a list of their results
let rec sequence (parsers: 'a Parser list) : 'a list Parser =
  List.foldBack (lift2 cons) parsers (pure []) 

let traverse f = List.map f >> sequence

/// If the first parser suceeds return its result else run the second on the original input
let (<|>) pa pb =
  Parser (fun (t,c) ->
    match (run pa (t,c)) with
    | (Output a, c') -> (Output a, c')
    | (Error _, _) -> run pb (t, c)
  )

/// Extract the token text from the specified tree
let tokenOf tree =
  castTree tree |> Option.map (fun t' -> t'.Token.Text) |> Option.getOrElse "|UNKNOWN|"

/// Print a warning - includes the current token and current line
let warn tree msg =
  printfn "Warning in %A, LINE %A: %s" (tokenOf tree) tree.Line msg

/// Parser error - includes the current token, line and output type
let err tree msg : 'a ParserResult =
  let parsingType = typeof<'a>
  let name = parsingType.ToString()
  let token = tokenOf tree
  let e = sprintf "Error at line %A, token %A while building %A: %A" tree.Line token name msg
  Error [e]

/// Check all children of a tree were consumed
let checkFullMatch (t': ITree) (c': ChildIdentifier) =
  let unmatched = t'.Children |> List.skip c'
  if unmatched = []
  then ()
  else
    let children = unmatched |> List.map (fun t -> sprintf "L%d/%s" t.Line (tokenOf t))
    "unmatched children: " + String.concat " " children |> warn t'

/// Run a parser over the next child if it matches the specified token
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

/// Anchor for breakpoints
let debug p = Parser (fun (t,c) ->
  run p (t,c)
)

/// Returns an equivalent parser - prevents infinite loops
let recursive fp = Parser (fun (t,c) -> run (fp()) (t,c))

/// Hardcoded error function
let fail = Parser (fun (t,c) ->
    let (Error e) = err t "HARDCODED FAIL"
    print e
    (Error e, c))

/// Optionally match the next child (same as FParsec)
let opt w = (one w |>> Some) <|> pure None

/// Check if the next child has the specified token
let exists token = opt (fun _ -> (token, pure ())) |>> Option.isSome

/// Match zero or more of the next children (same as FParsec)
let rec many w = (one w >>= (fun h -> many w |>> (fun t -> cons h t))) <|> pure []

/// Match one or more of the next children (same as FParsec)
let rec many1 w = lift2 NonEmptyList.create (one w) (many w)

// TODO: Need better name
let rec _many parser = (parser >>= (fun h -> _many parser |>> (fun t -> cons h t))) <|> pure []

let groups2 p = _many p |>> partitions2
let groups3 p = _many p |>> partitions3
let groups4 p = _many p |>> partitions4
let groups5 p = _many p |>> partitions5
let groups6 p = _many p |>> partitions6
let groups7 p = _many p |>> partitions7

/// Use the first parser that matches
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
