module PopulateSDL

open System.Linq
open System.Numerics
open Ast
open Constructors
open Parse
open AstSDL
open Antlr.SDL
open Antlr.Runtime.Tree
open Antlr.Runtime
open FsUtils
open FSharpx

#nowarn "0046"
#nowarn "1189"

type P = sdlParser

let attemptExpr = fails
let attemptCIFEnd = fails
let attemptChannel = fails
let attemptContent = fails
let attemptPriority = fails
let attemptProcedure  = fails
let attemptTerminator = fails
let attemptSignalRoute = fails

let attemptASN1 (t: ITree): string Parse = 
    t.Children.FirstOrNone() |> wrap |> Parse.map (fun c -> c.Text) 

let attemptInt (t: ITree): int Parse = fail()

let attemptString (label: int) (t: ITree) : string Parse =
    if t.Type = label then pure t.Text else 
      match t.Children with
          | (x::xs) when x.Type = label -> pure x.Text
          | _ -> fail()

let attemptID = attemptString P.ID
let attemptSort = attemptString P.SORT
let attemptHyperlink = attemptString P.HYPERLINK

let attemptPassBy (t:ITree) : PassBy Parse = 
    match t.Text with
    | "In" -> pure In
    | "Out" -> pure Out
    | "InOut" -> pure InOut
    | _ -> err "INVALID"
    
let attemptResult t =
    pure Result 
      <*> zeroOrOne  t P.ID attemptID
      <*> exactlyOne t P.SORT attemptSort

let attemptVariable t = 
    pure Variable
      <*> exactlyOne t P.ID attemptID
      <*> exactlyOne t P.SORT attemptSort

let attemptVarParameter t =
    pure VarParameter
      <*> exactlyOne t P.ID attemptID
      <*> exactlyOne t P.SORT attemptSort
      <*> fail()

let attemptVarDecl t =
    pure VarDecl 
      <*> exactlyOne t P.ID attemptID
      <*> exactlyOne t P.SORT attemptSort
      <*> fail()

let attemptCIFCoords t = 
    pure CIFCoordinates 
      <*> exactlyOne t P.INT attemptInt
      <*> fail()
      <*> fail()
      <*> fail()

let attemptTextArea t =
    pure TextArea
      <*> exactlyOne t P.CIF attemptCIFCoords
      <*> zeroOrOne  t P.TEXTAREA_CONTENT attemptContent
     
let attemptClause t =
    pure UseClause
      <*> zeroOrOne t P.ASN1 attemptASN1
      <*> fail()
      <*> exactlyOne t P.ID attemptID
      <*> zeroOrMore t P.ID attemptID
    
let attemptLabel t =
    pure Label
      <*> exactlyOne t P.CIF attemptCIFCoords
      <*> exactlyOne t P.ID attemptID

let attemptTask = fails
let attemptTaskBody = fails
let attemptOutput = fails
let attemptCreateRequest = fails
let attemptDecision = fails
let attemptTransitionOption = fails
let attemptExport = fails
let attemptTimer = fails
let attemptProcedureCall = fails

let attemptAction = fails
(*
let attemptAction t : Action Parse =
    pure Action
      <*> zeroOrOne  t P.LABEL attemptLabel
      <*> oneOf t [
            P.TASK,  attemptTask;
            P.TIMER, attemptTimer
        ]
*)      

let attemptTerminatorStatement t =
    pure TerminatorStatement 
      <*> zeroOrOne  t P.LABEL attemptLabel
      <*> zeroOrOne  t P.CIF attemptCIFCoords
      <*> zeroOrOne  t P.HYPERLINK attemptHyperlink
      <*> exactlyOne t P.TERMINATOR attemptTerminator
      <*> exactlyOne t P.END attemptCIFEnd

let attemptTransition t =
    pure Transition
      <*> zeroOrOne  t P.LABEL attemptLabel
      <*> oneOrMore  t P.ACTION attemptAction
      <*> exactlyOne t P.TERMINATOR attemptTerminatorStatement

let attemptStart t =
    pure Start
      <*> zeroOrOne  t P.CIF attemptCIFCoords
      <*> zeroOrOne  t P.HYPERLINK attemptHyperlink
      <*> zeroOrOne  t P.END attemptCIFEnd
      <*> exactlyOne t P.ID attemptID
      <*> exactlyOne t P.TRANSITION attemptTransition

let attemptFreeAction t =
    pure FreeAction
      <*> zeroOrOne  t P.CIF attemptCIFCoords
      <*> zeroOrOne  t P.HYPERLINK attemptHyperlink
      <*> zeroOrOne  t P.ID attemptID
      <*> exactlyOne t P.TRANSITION attemptTransition

let attemptState t =
    pure State
      <*> zeroOrOne  t P.CIF attemptCIFCoords
      <*> zeroOrOne  t P.HYPERLINK attemptHyperlink
      <*> fails()
      <*> fails()
      <*> zeroOrOne  t P.ID attemptID
      <*> fails()
      <*> fails()

let attemptSpontaneous t =
    pure SpontaneousTransition
      <*> zeroOrOne  t P.CIF attemptCIFCoords
      <*> zeroOrOne  t P.HYPERLINK attemptHyperlink
      <*> zeroOrOne  t P.END attemptCIFEnd
      <*> exactlyOne t P.PROVIDED attemptExpr
      <*> exactlyOne t P.TRANSITION attemptTransition
      
let attemptConnectPart t = 
    pure ConnectPart 
      <*> zeroOrOne  t P.CIF attemptCIFCoords
      <*> zeroOrOne  t P.HYPERLINK attemptHyperlink
      <*> zeroOrOne  t P.END attemptCIFEnd
      <*> zeroOrOne  t P.TRANSITION attemptTransition

let attemptContinuous t =
    pure ContinuousSignal 
      <*> zeroOrOne  t P.CIF attemptCIFCoords
      <*> zeroOrOne  t P.HYPERLINK attemptHyperlink
      <*> exactlyOne t P.PROVIDED attemptExpr
      <*> zeroOrOne  t P.PRIORITY attemptPriority
      <*> exactlyOne t P.END attemptCIFEnd      
      <*> zeroOrOne  t P.TRANSITION attemptTransition
           
let attemptStimulus t =
    pure Stimulus
      <*> exactlyOne t P.ID attemptID
      <*> zeroOrMore t P.ID attemptID

let attemptInputPart t = 
    pure InputPart 
      <*> zeroOrOne t P.CIF attemptCIFCoords
      <*> zeroOrOne t P.HYPERLINK attemptHyperlink
      <*> oneOrMore t P.STIMULUS attemptStimulus
      <*> fail()
      <*> fail()
      <*> zeroOrOne t P.TRANSITION attemptTransition

let attemptProcess t = err "hopeless"

let attemptSignal t =
    pure Signal 
      <*> exactlyOne t P.ID attemptID
      <*> zeroOrMore t P.ID attemptID
      <*> zeroOrMore t P.ID attemptID
      <*> zeroOrOne t P.END attemptCIFEnd

let attemptConnection t =
    pure Connection
      <*> oneOrMore t P.ID attemptID
      <*> oneOrMore t P.ID attemptID

let rec attemptBlock t =
    pure Block
      <*> exactlyOne t P.ID attemptID
      <*> zeroOrMore t P.SIGNAL attemptSignal
      <*> zeroOrMore t P.BLOCK attemptBlock
      <*> zeroOrMore t P.SIGNALROUTE attemptSignalRoute
      <*> zeroOrMore t P.CONNECTION attemptConnection
      <*> zeroOrMore t P.PROCESS attemptProcess

let attemptSystem t =
    warn "yolo"
    pure System
      <*> exactlyOne t P.ID attemptID
      <*> zeroOrMore t P.SIGNAL attemptSignal
      <*> zeroOrMore t P.BLOCK attemptBlock
      <*> zeroOrMore t P.TEXTAREA attemptTextArea
      <*> zeroOrMore t P.PROCESS attemptProcedure
      <*> zeroOrMore t P.CHANNEL attemptChannel

let fileAst (tree: ITree, filename: string, tokens: IToken[]): PRFile Parse =  
  printfn "%A" filename
  match tree.Type with
    | P.PR_FILE ->
        pure PRFile 
          <*> zeroOrMore tree P.USE attemptClause
          <*> zeroOrMore tree P.SYSTEM attemptSystem
          <*> zeroOrMore tree P.PROCESS attemptProcess
          
    | _ -> err "SYNTAX"

let modulesAst (files: (ITree * string * IToken[]) seq): PRModules Parse =
    Seq.length files |> printfn "Building %A files"
    let attempt = traverseSeq fileAst files
    print attempt
    attempt
    //traverse fileAst files
