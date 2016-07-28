module PopulateSDL

#nowarn "1189"

open System.Linq
open System.Numerics
open Ast
open AstSDL
open Antlr.SDL
open Antlr.Runtime.Tree
open Antlr.Runtime
open FsUtils
open FSharpx
open FSharpx.Validation
open FSharpx.Collections
open FSharpx.Option

type P  = sdlParser
let print x = printfn "%A" x
let traverse f xs = mapM f (Seq.toList xs)

let asNonEmpty xs =
  match xs with
  | [] -> None
  | y :: ys -> NonEmptyList.create y ys |> Some

//TODO: proper polymorphic implementations
//type Parse<'a> = Parse of 'a option
type Parse<'a> = 'a option

let log = printfn "%s: %s"

let warn = log "Warning"

let err<'a> e : 'a Parse =
    let wtf = (typedefof<'a>).Name
    log ("Error building " + wtf) e
    None

let retrieve = Some

//TODO: Remove
let fail() = err "FAIL"
let fails t = err "FAILS"

type ParserBuilder() =
  member this.Return x = Some x
  member this.ReturnFrom px = px
  member this.Bind(x: 'a Parse, f: 'a -> 'b Parse): 'b Parse =
      let res = Option.bind f x
      if res.IsNone then fail() else res

let parse = ParserBuilder()

let sequenceOption oox =
    match oox with 
    | Some ox -> Option.map Some ox
    | None -> Some None 

let exactlyOne <'a> (tree: ITree) (label: int) (builder: ITree -> 'a Parse): 'a Parse              = getOptionalChildByType(tree, label) |> Option.bind builder
let zeroOrOne  <'a> (tree: ITree) (label: int) (builder: ITree -> 'a Parse): 'a option Parse       = getOptionalChildByType(tree, label) |> Option.map builder |> sequenceOption
let zeroOrMore <'a> (tree: ITree) (label: int) (builder: ITree -> 'a Parse): 'a list Parse         = getChildrenByType(tree, label) |> traverse builder
let oneOrMore  <'a> (tree: ITree) (label: int) (builder: ITree -> 'a Parse): 'a NonEmptyList Parse = zeroOrMore tree label builder |> Option.bind asNonEmpty

type Case<'a> = int * (ITree -> 'a Parse)
let oneOf (t: ITree) (cases: 'a Case list) : 'a Parse = 
    cases
    |> Seq.filter (fun (label, _) -> t.Type = label)
    |> Seq.tryHead 
    |> Option.bind (fun (_ , builder) -> builder t)


let attemptExpr = fails
let attemptCIFEnd = fails
let attemptChannel = fails
let attemptContent = fails
let attemptPriority = fails
let attemptProcedure  = fails
let attemptTerminator = fails
let attemptSignalRoute = fails

let attemptASN1 (t: ITree): string Parse = 
    t.Children.FirstOrNone() |> Option.map (fun c -> c.Text) 

let attemptInt (t: ITree): int Parse = fail()

let attemptString (label: int) (t: ITree) : string Parse =
    if t.Type = label then Some t.Text else 
      match t.Children with
          | (x::xs) when x.Type = label -> Some x.Text
          | _ -> fail()

let attemptID = attemptString P.ID
let attemptSort = attemptString P.SORT
let attemptHyperlink = attemptString P.HYPERLINK

let attemptPassBy (t:ITree) : PassBy Parse = 
    match t.Text with
    | "In" -> Some In
    | "Out" -> Some Out
    | "InOut" -> Some InOut
    | _ -> err "INVALID"

let attemptResult (t: ITree) : Result Parse =
    parse {
        let! id = zeroOrOne  t P.ID attemptID
        let! st = exactlyOne t P.SORT attemptSort
        return { id = id; sort = st }
    }

let attemptVariable t : Variable Parse = 
    parse {
        let! id = exactlyOne t P.ID attemptID
        let! st = exactlyOne t P.SORT attemptSort
        return! fail()
    }

let attemptVarParameter t : VarParameter Parse =
    parse {
        let! id = exactlyOne t P.ID attemptID
        let! st = exactlyOne t P.SORT attemptSort
        let! pb = fail()
        return { id = id; sort = st; passBy = pb }
    }

let attemptVarDecl t : VarDecl Parse =
    parse {
        let! id = exactlyOne t P.ID attemptID
        let! st = exactlyOne t P.SORT attemptSort
        let! init = fail()
        return { id = id; sort = st; init = init }
    }

let attemptCIFCoords (t: ITree): CIFCoordinates Parse =
    parse {
        let! x = exactlyOne t P.INT attemptInt
        return! fail()
    }

let attemptTextArea t : TextArea Parse =
    parse {
        let! cif = exactlyOne t P.CIF attemptCIFCoords
        let! ctn = zeroOrOne  t P.TEXTAREA_CONTENT attemptContent
        return { cif = cif; content = ctn }
    }
     
let attemptClause t : UseClause Parse =
    parse {
        let! asn = zeroOrOne t P.ASN1 attemptASN1
        let! ids = oneOrMore t P.ID attemptID
        return { asn1 = asn; package = ids.Head; uses = ids.Tail; cifEnd = fail() }
    }

let attemptLabel t : Label Parse =
    parse {
        let! cif = exactlyOne t P.CIF attemptCIFCoords
        let! cnt = exactlyOne t P.ID attemptID
        return { cif = cif; connector = cnt }
    }

let attemptTask = fails
let attemptTaskBody = fails
let attemptOutput = fails
let attemptCreateRequest = fails
let attemptDecision = fails
let attemptTransitionOption = fails
let attemptExport = fails
let attemptTimer = fails
let attemptProcedureCall = fails

let attemptAction t : Action Parse =
    parse {
        let! label = zeroOrOne  t P.LABEL attemptLabel
        let! body  = oneOf t [
            P.TASK,  attemptTask;
            P.TIMER, attemptTimer
        ]
        return { label = label; body = body }
    }

let attemptTerminatorStatement t : TerminatorStatement Parse =
    parse {
        let! label  = zeroOrOne  t P.LABEL attemptLabel
        let! coords = zeroOrOne  t P.CIF attemptCIFCoords
        let! link   = zeroOrOne  t P.HYPERLINK attemptHyperlink
        let! term   = exactlyOne t P.TERMINATOR attemptTerminator
        let! cifEnd = exactlyOne t P.END attemptCIFEnd
        return { label = label; cif = coords; hyperlink = link; terminator = term; cifEnd = cifEnd }
    }

let attemptTransition t : Transition Parse =
    parse {
        let! label   = zeroOrOne  t P.LABEL attemptLabel
        let! actions = oneOrMore  t P.ACTION attemptAction
        let! stmt    = exactlyOne t P.TERMINATOR attemptTerminatorStatement
        return { label = label; actions = actions; statement = stmt }
    }

let attemptStart t : Start Parse =
    parse {
        let! coords = zeroOrOne  t P.CIF attemptCIFCoords
        let! link   = zeroOrOne  t P.HYPERLINK attemptHyperlink
        let! cifEnd = zeroOrOne  t P.END attemptCIFEnd
        let! entry  = exactlyOne t P.ID attemptID
        let! trans  = exactlyOne t P.TRANSITION attemptTransition
        return { cif = coords; hyperlink = link; cifEnd = cifEnd; entryState = entry; transition = trans }
    }


let attemptFreeAction t : FreeAction Parse = 
   parse {
       let! coords  = zeroOrOne  t P.CIF attemptCIFCoords
       let! link    = zeroOrOne  t P.HYPERLINK attemptHyperlink
       let! conn    = zeroOrOne  t P.ID attemptID
       let! trans   = exactlyOne t P.TRANSITION attemptTransition
       return { cif = coords; hyperlink = link; connector = conn; transition = trans }
   }

let attemptState t : State Parse =
    parse {
       let! coords  = zeroOrOne  t P.CIF attemptCIFCoords
       let! link    = zeroOrOne  t P.HYPERLINK attemptHyperlink
       //body
       //parts
       let! name    = zeroOrOne  t P.ID attemptID
       //ends
       return! fail()
    }

let attemptSpontaneous t : SpontaneousTransition Parse =
    parse {
        let! coords = zeroOrOne  t P.CIF attemptCIFCoords
        let! link   = zeroOrOne  t P.HYPERLINK attemptHyperlink
        let! cifEnd = zeroOrOne  t P.END attemptCIFEnd
        let! trans  = exactlyOne t P.TRANSITION attemptTransition
        let! guard  = exactlyOne t P.PROVIDED attemptExpr
        return { cif = coords; hyperlink = link; cifEnd = cifEnd; guard = guard; transition = trans }
    }

let attemptConnectPart t : ConnectPart Parse =
    parse {
        let! coords = zeroOrOne  t P.CIF attemptCIFCoords
        let! link   = zeroOrOne  t P.HYPERLINK attemptHyperlink
        let! cifEnd = zeroOrOne  t P.END attemptCIFEnd
        let! trans  = zeroOrOne  t P.TRANSITION attemptTransition
        return { cif = coords; hyperlink = link; cifEnd = cifEnd; transition = trans }
    }

let attemptContinuous t : ContinuousSignal Parse = 
    parse {
        let! coords = zeroOrOne  t P.CIF attemptCIFCoords
        let! link   = zeroOrOne  t P.HYPERLINK attemptHyperlink
        let! cifEnd = exactlyOne t P.END attemptCIFEnd
        let! trans  = zeroOrOne  t P.TRANSITION attemptTransition
        let! guard  = exactlyOne t P.PROVIDED attemptExpr
        let! prty   = zeroOrOne  t P.PRIORITY attemptPriority
        return { cif = coords; hyperlink = link; cifEnd = cifEnd; transition = trans; guard = guard; priority = prty }
    }

let attemptStimulus t : Stimulus Parse =
    parse {
        let! id = exactlyOne t P.ID attemptID
        let! vs = zeroOrMore t P.ID attemptID
        return { id = id; vars = vs }
    }

let attemptInputPart t : InputPart Parse =
    parse {
        let! coords = zeroOrOne t P.CIF attemptCIFCoords
        let! link   = zeroOrOne t P.HYPERLINK attemptHyperlink
        let! stim   = oneOrMore t P.STIMULUS attemptStimulus
        let! cifEnd = fail()
        let! guard  = fail()
        let! trans  = zeroOrOne t P.TRANSITION attemptTransition
        return { cif = coords; hyperlink = link; stimuli = stim; cifEnd = cifEnd; guard = guard; transition = trans }
    }

let attemptProcess (t: ITree) = err "hopeless"

let attemptSignal (t: ITree): Signal Parse =
    parse {
        let! id = exactlyOne t P.ID attemptID
        let! ps = zeroOrMore t P.ID attemptID
        let! vs = zeroOrMore t P.ID attemptID
        let! ce = zeroOrOne t P.END attemptCIFEnd
        return { id = id; parameterNames = ps; vars = vs; cifEnd = ce }
    }

let attemptConnection (t: ITree): Connection Parse =
    parse {
        let! cs = oneOrMore t P.ID attemptID
        let! rs = oneOrMore t P.ID attemptID
        return { channels = cs; routes = rs }
    }

let rec attemptBlock (t:ITree): Block Parse =
    parse {
        let! id = exactlyOne t P.ID attemptID
        let! ss = zeroOrMore t P.SIGNAL attemptSignal
        let! bs = zeroOrMore t P.BLOCK attemptBlock
        let! rs = zeroOrMore t P.SIGNALROUTE attemptSignalRoute
        let! cs = zeroOrMore t P.CONNECTION attemptConnection
        let! ps = zeroOrMore t P.PROCESS attemptProcess
        return { id = id; signals = ss; blocks = bs; signalRoutes = rs; connections = cs; processes = ps }
    }

let attemptSystem  (t: ITree): System Parse = 
    parse {
        let! id = exactlyOne t P.ID attemptID
        let! ss = zeroOrMore t P.SIGNAL attemptSignal
        let! bs = zeroOrMore t P.BLOCK attemptBlock
        let! ts = zeroOrMore t P.TEXTAREA attemptTextArea
        let! ps = zeroOrMore t P.PROCESS attemptProcedure
        let! cs = zeroOrMore t P.CHANNEL attemptChannel
        warn "yolo"
        return { id = id; signals = ss; blocks = bs; textAreas = ts; procedures = ps; channels = cs } 
    }

let fileAst (tree: ITree, filename: string, tokens: IToken[]): PRFile Parse =  
  printfn "%A" filename
  match tree.Type with
    | P.PR_FILE ->
        parse {
            let! ss = zeroOrMore tree P.SYSTEM attemptSystem
            let! ps = zeroOrMore tree P.PROCESS attemptProcess
            let! cs = zeroOrMore tree P.USE attemptClause
            return { clauses = cs; processes = ps; systems = ss }
        }
    | _ -> err "SYNTAX"

let modulesAst (files: (ITree * string * IToken[]) seq): PRModules Parse =
    Seq.length files |> printfn "Building %A files"
    let attempt = traverse fileAst files
    print attempt
    attempt
    //traverse fileAst files
