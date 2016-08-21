module Parse

open Antlr.Runtime.Tree
open FsUtils
open FSharpx
open FSharpx.Collections
open FSharpx.Option

#nowarn "0046"
#nowarn "1189"

let print x = printfn "%A" x

let castTree (x:ITree) =
  match x with
  | :? CommonTree as x' -> Some x'
  | _ -> None

let asNonEmpty xs =
  match xs with
  | [] -> None
  | y :: ys -> NonEmptyList.create y ys |> Some

type Parse<'a> = Parse of 'a option

let wrap x = Parse x
let unwrap (Parse x) = x

let log = printfn "%s: %s"

let warn = log "Warning"

let error x =
    printfn "%s" x
    Parse None

let fail' <'a> e : 'a Parse =
    let name = typeof<'a>.ToString()
    log ("Error building " + name) e
    Parse None


let fails t = fail' "FAIL"
let fail () = fail' "FAIL"

let check (tree: ITree) (Parse x) : 'a Parse =
  if x.IsSome
  then Parse x
  else
   let name = typeof<'a>.ToString()
   let parentToken: string = castTree tree |> Option.map (fun t -> t.Token.Text) |> Option.getOrElse "|UNKNOWN|"
   sprintf "Error at line %A, inside %A while building %A"  tree.Line parentToken name |> error

let inline pure x = Parse (Some x)

let map f (Parse x) = Parse (Option.map f x)

let (<*>) (Parse f) (Parse x) : Parse<'b> =
  match (f, x) with
  | Some(f'), Some(x') -> f' x' |> pure 
  | _,_ -> Parse None

let sequence (op : 'a Parse option) : 'a option Parse =
    match op with 
    | Some p -> map Some p
    | None -> pure None

let traverseOption  (f: 'a -> 'b Parse) (op: 'a option) : 'b option Parse = Option.map f op |> sequence 
let traverseSeq     (f: 'a -> 'b Parse) (xs: 'a seq)    : 'b list Parse   = mapM (f >> unwrap) (Seq.toList xs) |> Parse

let bind (f: 'a -> 'b Parse) (Parse x: 'a Parse) : 'b Parse = 
  match x with
  | Some(x') -> f x'
  | None -> Parse None

let exactlyOne <'a> (tree: ITree) (label: int) (builder: ITree -> 'a Parse): 'a Parse              = getOptionalChildByType(tree, label) |> wrap |> bind builder |> check tree
let zeroOrOne  <'a> (tree: ITree) (label: int) (builder: ITree -> 'a Parse): 'a option Parse       = getOptionalChildByType(tree, label) |> traverseOption builder |> check tree
let zeroOrMore <'a> (tree: ITree) (label: int) (builder: ITree -> 'a Parse): 'a list Parse         = getChildrenByType(tree, label) |> traverseSeq builder |> check tree
let oneOrMore  <'a> (tree: ITree) (label: int) (builder: ITree -> 'a Parse): 'a NonEmptyList Parse = zeroOrMore tree label builder |> bind (asNonEmpty >> wrap) |> check tree

(*
type Case<'a> = int * (ITree -> 'a Parse)
let oneOf (t: ITree) (cases: 'a Case list) : 'a Parse = 
    cases
    |> Seq.filter (fun (label, _) -> t.Type = label)
    |> Seq.tryHead 
    |> Option.bind (fun (_ , builder) -> builder t)
*)