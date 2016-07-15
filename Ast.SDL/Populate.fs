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

type P = sdlParser
let print x = printfn "%A" x
let traverse f xs = mapM f (Seq.toList xs)
let ast = MaybeBuilder()

let proveNonEmpty xs =
  match xs with
  | [] -> None
  | y :: ys -> NonEmptyList.create y ys |> Some

type Attempt<'a> = 'a option
  
let exactlyOne <'a> (tree: ITree) (label: int) (builder: ITree -> 'a option): 'a option              = getOptionalChildByType(tree, label) |> Option.bind builder
let zeroOrOne  <'a> (tree: ITree) (label: int) (builder: ITree -> 'a option): 'a option option       = getOptionalChildByType(tree, label) |> Option.map builder //|> sequence
let zeroOrMore <'a> (tree: ITree) (label: int) (builder: ITree -> 'a option): 'a list option         = getChildrenByType(tree, label) |> traverse builder
let oneOrMore  <'a> (tree: ITree) (label: int) (builder: ITree -> 'a option): 'a NonEmptyList option = zeroOrMore tree label builder |> Option.bind proveNonEmpty

let oneOf (t: ITree) (cases: (int * (ITree -> 'a Attempt)) list) : 'a Attempt = 
    cases
    |> Seq.filter (fun (label, _) -> t.Type = label)
    |> Seq.tryHead 
    |> Option.bind (fun (_ , builder) -> builder t)

let log = printfn "%s: %s"

let warn = log "Warning"

let err e =
  log "Error" e
  None

let retrieve = Some


//TODO: Remove
let fails t = None
let fail = None

let attemptExpr = fails
let attemptCIFEnd = fails
let attemptChannel = fails
let attemptContent = fails
let attemptPriority = fails
let attemptProcedure  = fails
let attemptTerminator = fails
let attemptSignalRoute = fails

let attemptASN1 (t: ITree): string Attempt = 
    t.Children.FirstOrNone() |> Option.map (fun c -> c.Text) 

let attemptInt (t: ITree): int Attempt = fail

let attemptString (label: int) (t: ITree) : string Attempt =
    if t.Type = label then Some t.Text else 
      match t.Children with
          | (x::xs) when x.Type = label -> Some x.Text
          | _ -> None

let attemptID = attemptString P.ID
let attemptSort = attemptString P.SORT
let attemptHyperlink = attemptString P.HYPERLINK

let attemptPassBy (t:ITree) : PassBy Attempt = 
    match t.Text with
    | "In" -> Some In
    | "Out" -> Some Out
    | "InOut" -> Some InOut
    | _ -> err "INVALID"

let attemptResult (t: ITree) : Result Attempt =
    ast {
        let! id = zeroOrOne  t P.ID attemptID
        let! st = exactlyOne t P.SORT attemptSort
        return { id = id; sort = st }
    }

let attemptVariable t : Variable Attempt = 
    ast {
        let! id = exactlyOne t P.ID attemptID
        let! st = exactlyOne t P.SORT attemptSort
        return! fail
    }

let attemptVarParameter t : VarParameter Attempt =
    ast {
        let! id = exactlyOne t P.ID attemptID
        let! st = exactlyOne t P.SORT attemptSort
        let! pb = fail
        return { id = id; sort = st; passBy = pb }
    }

let attemptVarDecl t : VarDecl Attempt =
    ast {
        let! id = exactlyOne t P.ID attemptID
        let! st = exactlyOne t P.SORT attemptSort
        let! init = fail
        return { id = id; sort = st; init = init }
    }

let attemptCIFCoords (t: ITree): CIFCoordinates Attempt =
    ast {
        let! x = exactlyOne t P.INT attemptInt
        return! fail
    }

let attemptTextArea t : TextArea Attempt =
    ast {
        let! cif = exactlyOne t P.CIF attemptCIFCoords
        let! ctn = zeroOrOne  t P.TEXTAREA_CONTENT attemptContent
        return { cif = cif; content = ctn }
    }
     
let attemptClause t : UseClause Attempt =
    ast {
        let! asn = zeroOrOne t P.ASN1 attemptASN1
        let! ids = oneOrMore t P.ID attemptID
        return { asn1 = asn; package = ids.Head; uses = ids.Tail; cifEnd = fail }
    }

let attemptLabel t : Label Attempt =
    ast {
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

let attemptAction t : Action Attempt =
    ast {
        let! label = zeroOrOne  t P.LABEL attemptLabel
        let! body  = oneOf t [
            P.TASK,  attemptTask;
            P.TIMER, attemptTimer
        ]
        return { label = label; body = body }
    }

let attemptTerminatorStatement t : TerminatorStatement Attempt =
    ast {
        let! label  = zeroOrOne  t P.LABEL attemptLabel
        let! coords = zeroOrOne  t P.CIF attemptCIFCoords
        let! link   = zeroOrOne  t P.HYPERLINK attemptHyperlink
        let! term   = exactlyOne t P.TERMINATOR attemptTerminator
        let! cifEnd = exactlyOne t P.END attemptCIFEnd
        return { label = label; cif = coords; hyperlink = link; terminator = term; cifEnd = cifEnd }
    }

let attemptTransition t : Transition Attempt =
    ast {
        let! label   = zeroOrOne  t P.LABEL attemptLabel
        let! actions = oneOrMore  t P.ACTION attemptAction
        let! stmt    = exactlyOne t P.TERMINATOR attemptTerminatorStatement
        return { label = label; actions = actions; statement = stmt }
    }

let attemptStart t : Start Attempt =
    ast {
        let! coords = zeroOrOne  t P.CIF attemptCIFCoords
        let! link   = zeroOrOne  t P.HYPERLINK attemptHyperlink
        let! cifEnd = zeroOrOne  t P.END attemptCIFEnd
        let! entry  = exactlyOne t P.ID attemptID
        let! trans  = exactlyOne t P.TRANSITION attemptTransition
        return { cif = coords; hyperlink = link; cifEnd = cifEnd; entryState = entry; transition = trans }
    }


let attemptFreeAction t : FreeAction Attempt = 
   ast {
       let! coords  = zeroOrOne  t P.CIF attemptCIFCoords
       let! link    = zeroOrOne  t P.HYPERLINK attemptHyperlink
       let! conn    = zeroOrOne  t P.ID attemptID
       let! trans   = exactlyOne t P.TRANSITION attemptTransition
       return { cif = coords; hyperlink = link; connector = conn; transition = trans }
   }

let attemptState t : State Attempt =
    ast {
       let! coords  = zeroOrOne  t P.CIF attemptCIFCoords
       let! link    = zeroOrOne  t P.HYPERLINK attemptHyperlink
       //body
       //parts
       let! name    = zeroOrOne  t P.ID attemptID
       //ends
       return! fail
    }

let attemptSpontaneous t : SpontaneousTransition Attempt =
    ast {
        let! coords = zeroOrOne  t P.CIF attemptCIFCoords
        let! link   = zeroOrOne  t P.HYPERLINK attemptHyperlink
        let! cifEnd = zeroOrOne  t P.END attemptCIFEnd
        let! trans  = exactlyOne t P.TRANSITION attemptTransition
        let! guard  = exactlyOne t P.PROVIDED attemptExpr
        return { cif = coords; hyperlink = link; cifEnd = cifEnd; guard = guard; transition = trans }
    }

let attemptConnectPart t : ConnectPart Attempt =
    ast {
        let! coords = zeroOrOne  t P.CIF attemptCIFCoords
        let! link   = zeroOrOne  t P.HYPERLINK attemptHyperlink
        let! cifEnd = zeroOrOne  t P.END attemptCIFEnd
        let! trans  = zeroOrOne  t P.TRANSITION attemptTransition
        return { cif = coords; hyperlink = link; cifEnd = cifEnd; transition = trans }
    }

let attemptContinuous t : ContinuousSignal Attempt = 
    ast {
        let! coords = zeroOrOne  t P.CIF attemptCIFCoords
        let! link   = zeroOrOne  t P.HYPERLINK attemptHyperlink
        let! cifEnd = exactlyOne t P.END attemptCIFEnd
        let! trans  = zeroOrOne  t P.TRANSITION attemptTransition
        let! guard  = exactlyOne t P.PROVIDED attemptExpr
        let! prty   = zeroOrOne  t P.PRIORITY attemptPriority
        return { cif = coords; hyperlink = link; cifEnd = cifEnd; transition = trans; guard = guard; priority = prty }
    }

let attemptStimulus t : Stimulus Attempt =
    ast {
        let! id = exactlyOne t P.ID attemptID
        let! vs = zeroOrMore t P.ID attemptID
        return { id = id; vars = vs }
    }

let attemptInputPart t : InputPart Attempt =
    ast {
        let! coords = zeroOrOne t P.CIF attemptCIFCoords
        let! link   = zeroOrOne t P.HYPERLINK attemptHyperlink
        let! stim   = oneOrMore t P.STIMULUS attemptStimulus
        let! cifEnd = fail
        let! guard  = fail
        let! trans  = zeroOrOne t P.TRANSITION attemptTransition
        return { cif = coords; hyperlink = link; stimuli = stim; cifEnd = cifEnd; guard = guard; transition = trans }
    }

let attemptProcess (t: ITree) = err "hopeless"

let attemptSignal (t: ITree): Signal Attempt =
    ast {
        let! id = exactlyOne t P.ID attemptID
        let! ps = zeroOrMore t P.ID attemptID
        let! vs = zeroOrMore t P.ID attemptID
        let! ce = exactlyOne t P.END attemptCIFEnd
        return { id = id; parameterNames = ps; vars = vs; cifEnd = ce }
    }

let attemptConnection (t: ITree): Connection Attempt =
    ast {
        let! cs = oneOrMore t P.ID attemptID
        let! rs = oneOrMore t P.ID attemptID
        return { channels = cs; routes = rs }
    }

let rec attemptBlock (t:ITree): Block Attempt =
    ast {
        let! id = exactlyOne t P.ID attemptID
        let! ss = zeroOrMore t P.SIGNAL attemptSignal
        let! bs = zeroOrMore t P.BLOCK attemptBlock
        let! rs = zeroOrMore t P.SIGNALROUTE attemptSignalRoute
        let! cs = zeroOrMore t P.CONNECTION attemptConnection
        let! ps = zeroOrMore t P.PROCESS attemptProcess
        return { id = id; signals = ss; blocks = bs; signalRoutes = rs; connections = cs; processes = ps }
    }

let attemptSystem  (t: ITree): System Attempt = 
    ast {
        let! id = exactlyOne t P.ID attemptID
        let! ss = zeroOrMore t P.SIGNAL attemptSignal
        let! bs = zeroOrMore t P.BLOCK attemptBlock
        let! ts = zeroOrMore t P.TEXTAREA attemptTextArea
        let! ps = zeroOrMore t P.PROCESS attemptProcedure
        let! cs = zeroOrMore t P.CHANNEL attemptChannel
        warn "yolo"
        return { id = id; signals = ss; blocks = bs; textAreas = ts; procedures = ps; channels = cs } 
    }

let fileAst (tree: ITree, filename: string, tokens: IToken[]): PRFile Attempt =  
  printfn "%A" filename
  match tree.Type with
    | P.PR_FILE ->
        ast {
            let! ss = zeroOrMore tree P.SYSTEM attemptSystem
            let! ps = zeroOrMore tree P.PROCESS attemptProcess
            let! cs = zeroOrMore tree P.USE attemptClause
            return { clauses = cs; processes = ps; systems = ss }
        }
    | _ -> err "SYNTAX"

let modulesAst (files: (ITree * string * IToken[]) seq): PRModules Attempt =
    Seq.length files |> printfn "Building %A files"
    let attempt = traverse fileAst files
    print attempt
    attempt
    //traverse fileAst files
