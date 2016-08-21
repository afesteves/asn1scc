module PopulateSDL
open System
open System.Linq
open System.Numerics
open Ast
open Constructors
open Parse
open Utils
open AstSDL
open Antlr.SDL
open Antlr.Runtime.Tree
open Antlr.Runtime
open FsUtils
open FSharpx

#nowarn "0046"
#nowarn "1189"

type P = sdlParser

let attemptExpr = fail
let attemptPassBy = fail
let attemptCIFEnd = fail
let attemptChannel: Channel Parse = fail
let attemptContent = fail
let attemptPriority = fail
let attemptProcedure: Procedure Parse = fail
let attemptTerminator = fail
let attemptSignalRoute = fail

let attemptASN1 = Parse(fun (t,s) -> 
  (t.Children.FirstOrNone() |> Option.map (fun c -> c.Text), s))

let attemptInt = fail

let attemptString (label: int) =
  Parse (fun (t,s) -> 
    if t.Type = label then (Some t.Text, s) else 
      match t.Children with
      | (x::xs) when x.Type = label -> (Some x.Text, s)
      | _ -> (None, s)
   )

let attemptID = attemptString P.ID
let attemptSort = attemptString P.SORT
let attemptHyperlink = attemptString P.HYPERLINK
    
let attemptResult =
    pure Result 
      <*> opt P.ID attemptID
      <*> one P.SORT attemptSort

let attemptVariable = 
    pure Variable
      <*> one P.ID attemptID
      <*> one P.SORT attemptSort

let attemptVarParameter =
    pure VarParameter
      <*> one P.ID attemptID
      <*> one P.SORT attemptSort
      <*> fail

let attemptVarDecl =
    pure VarDecl 
      <*> one P.ID attemptID
      <*> one P.SORT attemptSort
      <*> fail

let attemptCIFCoords = 
    pure CIFCoordinates 
      <*> one P.INT attemptInt
      <*> fail
      <*> fail
      <*> fail

let attemptTextArea =
    pure TextArea
      <*> one P.CIF attemptCIFCoords
      <*> opt P.TEXTAREA_CONTENT attemptContent
     
let attemptClause =
    pure UseClause
      <*> opt P.ASN1 attemptASN1
      <*> opt P.END attemptCIFEnd
      <*> one P.ID attemptID
      <*> many P.ID attemptID
    
let attemptLabel =
    pure Label
      <*> one P.CIF attemptCIFCoords
      <*> one P.ID attemptID

(*
let attemptTask = fail
let attemptTaskBody = fail
let attemptOutput = fail
let attemptCreateRequest = fail
let attemptDecision = fail
let attemptTransitionOption = fail
let attemptExport = fail
let attemptTimer = fail
let attemptProcedureCall = fail
*)
let attemptAction = fail
(*
let attemptAction t : Action Parse =
    pure Action
      <*> opt  t P.LABEL attemptLabel
      <*> oneOf t [
            P.TASK,  attemptTask;
            P.TIMER, attemptTimer
        ]
*)      

let attemptTerminatorStatement =
    pure TerminatorStatement 
      <*> opt P.LABEL attemptLabel
      <*> opt P.CIF attemptCIFCoords
      <*> opt P.HYPERLINK attemptHyperlink
      <*> one P.TERMINATOR attemptTerminator
      <*> opt P.END attemptCIFEnd

let attemptTransition =
    pure Transition
      <*> opt P.LABEL attemptLabel
      <*> many1 P.ACTION attemptAction
      <*> one P.TERMINATOR attemptTerminatorStatement

let attemptStart =
    pure Start
      <*> opt P.CIF attemptCIFCoords
      <*> opt P.HYPERLINK attemptHyperlink
      <*> opt P.END attemptCIFEnd
      <*> one P.ID attemptID
      <*> one P.TRANSITION attemptTransition

let attemptFreeAction =
    pure FreeAction
      <*> opt P.CIF attemptCIFCoords
      <*> opt P.HYPERLINK attemptHyperlink
      <*> opt P.ID attemptID
      <*> one P.TRANSITION attemptTransition

let attemptState =
    pure State
      <*> opt P.CIF attemptCIFCoords
      <*> opt P.HYPERLINK attemptHyperlink
      <*> fail
      <*> fail
      <*> opt P.ID attemptID
      <*> fail
      <*> fail

let attemptSpontaneous =
    pure SpontaneousTransition
      <*> opt P.CIF attemptCIFCoords
      <*> opt P.HYPERLINK attemptHyperlink
      <*> opt P.END attemptCIFEnd
      <*> one P.PROVIDED attemptExpr
      <*> one P.TRANSITION attemptTransition
      
let attemptConnectPart = 
    pure ConnectPart 
      <*> opt P.CIF attemptCIFCoords
      <*> opt P.HYPERLINK attemptHyperlink
      <*> opt P.END attemptCIFEnd
      <*> opt P.TRANSITION attemptTransition

let attemptContinuous =
    pure ContinuousSignal 
      <*> opt P.CIF attemptCIFCoords
      <*> opt P.HYPERLINK attemptHyperlink
      <*> one P.PROVIDED attemptExpr
      <*> opt P.PRIORITY attemptPriority
      <*> opt P.END attemptCIFEnd      
      <*> opt P.TRANSITION attemptTransition
           
let attemptStimulus =
    pure Stimulus
      <*> one P.ID attemptID
      <*> many P.ID attemptID

let attemptInputPart = 
    pure InputPart 
      <*> opt P.CIF attemptCIFCoords
      <*> opt P.HYPERLINK attemptHyperlink
      <*> many1 P.STIMULUS attemptStimulus
      <*> fail
      <*> fail
      <*> opt P.TRANSITION attemptTransition

let attemptProcess = fail
  
let attemptSignal =
    pure Signal 
      <*> (many P.PARAMNAMES (many P.ID attemptID) |>> List.concat)
      <*> one P.ID attemptID
      <*> (many P.PARAMS (many P.ID attemptID) |>> List.concat)
      <*> opt P.END attemptCIFEnd

let attemptConnection =
    pure Connection
      <*> many1 P.ID attemptID
      <*> many1 P.ID attemptID


let rec attemptBlock' () =
    pure Block
      <*> one P.ID attemptID
      <*> many P.SIGNAL attemptSignal
      <*> many P.BLOCK (recursive attemptBlock')
      <*> many P.SIGNALROUTE attemptSignalRoute
      <*> many P.CONNECTION attemptConnection
      <*> many P.PROCESS attemptProcess

let attemptBlock = attemptBlock'()

let attemptSystemEntity =
  choice5
    ( one P.SIGNAL attemptSignal
    , one P.BLOCK attemptBlock
    , one P.TEXTAREA attemptTextArea
    , one P.PROCEDURE attemptProcedure
    , one P.CHANNEL attemptChannel)

let attemptSystem =
    pure (fun i (ss, bs, ts, ps, cs) -> System i ss bs ts ps cs)
      <*> one P.ID attemptID
      <*> (many' attemptSystemEntity |>> partitions5)

let attemptPRFile =
  pure PRFile
    <*> many P.USE attemptClause
    <*> many P.SYSTEM attemptSystem
    <*> many P.PROCESS attemptProcess

let attemptFile (file: ITree * string * IToken[]) =
  let (t, _, _) = file
  if t.Type = P.PR_FILE then run attemptPRFile (t, 0) |> fst else None
    
let modulesAst (files: (ITree * string * IToken[]) seq): PRFile option seq =
  Seq.length files |> printfn "Building %A files"
  let res = Seq.map attemptFile files
  print res
  res
