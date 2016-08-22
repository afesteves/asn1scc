module Parsers

open Ast
open Constructors
open TreeParser
open Utils
open AstSDL
open Antlr.SDL
open FsUtils

#nowarn "0046"
#nowarn "1189"

type P = sdlParser

let attemptExpr = fail
let attemptPassBy = fail
let attemptCIFEnd = fail
let attemptChannel: Channel Parser = fail
let attemptContent = fail
let attemptPriority = fail
let attemptProcedure: Procedure Parser = fail
let attemptTerminator = fail
let attemptSignalRoute = fail

let attemptASN1 = Parser(fun (t,s) ->
  (head t.Children |> Option.map (fun c -> c.Text), s))

let attemptInt = fail

let rec attemptString (label: int) =
  Parser (fun (t,s) ->
    if t.Type = label then (Some t.Text, s) else 
      match t.Children with
      | (x::xs) when x.Type = label -> (Some x.Text, s)
      | _ -> (None, s)
   )

and attemptID = attemptString P.ID
and attemptSort = attemptString P.SORT
and attemptHyperlink = attemptString P.HYPERLINK
    
and attemptResult =
    pure Result
      <*> opt ID 
      <*> one SORT

and attemptVariable = 
    pure Variable
      <*> one ID
      <*> one SORT

and attemptVarParameter =
    pure VarParameter
      <*> one ID
      <*> one SORT
      <*> fail

and attemptVarDecl =
    pure VarDecl 
      <*> one ID
      <*> one SORT
      <*> fail

and attemptCIFCoords =
    pure CIFCoordinates 
      <*> one INT
      <*> fail
      <*> fail
      <*> fail

and attemptTextArea =
    pure TextArea
      <*> one CIF
      <*> opt TEXTAREA_CONTENT
     
and attemptClause =
    pure UseClause
      <*> opt ASN1
      <*> opt END
      <*> one ID
      <*> many ID
    
and attemptLabel =
    pure Label
      <*> one CIF
      <*> one ID

(*
and attemptTask = fail
and attemptTaskBody = fail
and attemptOutput = fail
and attemptCreateRequest = fail
and attemptDecision = fail
and attemptTransitionOption = fail
and attemptExport = fail
and attemptTimer = fail
and attemptProcedureCall = fail
*)
and attemptAction = fail
(*
and attemptAction t : Action Parse =
    pure Action
      <*> opt  t P.LABEL attemptLabel
      <*> oneOf t [
            P.TASK,  attemptTask;
            P.TIMER, attemptTimer
        ]
*)      

and attemptTerminatorStatement =
    pure TerminatorStatement
      <*> opt LABEL
      <*> opt CIF
      <*> opt HYPERLINK
      <*> fail
      <*> opt END

and attemptTransition =
    pure Transition
      <*> opt LABEL
      <*> many1 ACTION
      <*> one TERMINATOR_STATEMENT

and attemptStart =
    pure Start
      <*> opt CIF
      <*> opt HYPERLINK
      <*> opt END
      <*> one ID
      <*> one TRANSITION

and attemptFreeAction =
    pure FreeAction
      <*> opt CIF
      <*> opt HYPERLINK
      <*> opt ID
      <*> one TRANSITION

and attemptState =
    pure State
      <*> opt CIF
      <*> opt HYPERLINK
      <*> fail
      <*> fail
      <*> opt ID
      <*> fail
      <*> fail

and attemptSpontaneous =
    pure SpontaneousTransition
      <*> opt CIF
      <*> opt HYPERLINK
      <*> opt END
      <*> one PROVIDED
      <*> one TRANSITION
      
and attemptConnectPart = 
    pure ConnectPart 
      <*> opt CIF
      <*> opt HYPERLINK
      <*> opt END
      <*> opt TRANSITION

and attemptContinuous =
    pure ContinuousSignal
      <*> opt CIF
      <*> opt HYPERLINK
      <*> one PROVIDED
      <*> opt PRIORITY
      <*> opt END
      <*> opt TRANSITION
           
and attemptStimulus =
    pure Stimulus
      <*> one  ID
      <*> many ID

and attemptInputPart =
    pure InputPart
      <*> opt CIF
      <*> opt HYPERLINK
      <*> many1 STIMULUS
      <*> fail
      <*> fail
      <*> opt TRANSITION

and attemptProcess = fail
  
and attemptSignal =
    pure Signal 
      <*> (many PARAMNAMES |>> List.concat)
      <*> one ID
      <*> (many PARAMS |>> List.concat)
      <*> opt END

and attemptConnection =
    pure Connection
      <*> many1 ID
      <*> many1 ID

and attemptBlock' () =
    pure Block
      <*> one ID
      <*> many SIGNAL
      <*> many BLOCK
      <*> many SIGNALROUTE
      <*> many CONNECTION
      <*> many PROCESS

and attemptBlock = attemptBlock'()

and attemptSystemEntity =
  choice5
    ( one SIGNAL
    , one BLOCK
    , one TEXTAREA
    , one PROCEDURE
    , one CHANNEL)

and attemptSystem =
    pure (fun i (ss, bs, ts, ps, cs) -> System i ss bs ts ps cs)
      <*> one ID
      <*> groups5 attemptSystemEntity

and attemptPRFile =
  pure PRFile
    <*> many USE
    <*> many SYSTEM
    <*> many PROCESS

and ID _ = (P.ID, attemptID)
and SIGNAL _ = (P.SIGNAL, attemptSignal)
and BLOCK _ = (P.BLOCK, recursive attemptBlock')
and SIGNALROUTE _ = (P.SIGNALROUTE, attemptSignalRoute)
and CONNECTION _ = (P.CONNECTION, attemptConnection)
and PROCESS _ = (P.PROCESS, attemptProcess)
and TEXTAREA _ = (P.TEXTAREA, attemptTextArea)
and PROCEDURE _ = (P.PROCEDURE, attemptProcedure)
and CHANNEL _ = (P.CHANNEL, attemptChannel)
and SORT _ = (P.SORT, attemptSort)
and HYPERLINK _ = (P.HYPERLINK, attemptHyperlink)
and INT _ = (P.INT, attemptInt)
and ASN1 _ = (P.ASN1, attemptASN1)
and END _ = (P.END, attemptCIFEnd)
and CIF _ = (P.CIF, attemptCIFCoords)
and TEXTAREA_CONTENT _ = (P.TEXTAREA_CONTENT, attemptContent)
and LABEL _ = (P.LABEL, attemptLabel)
and TERMINATOR_STATEMENT _ = (P.TERMINATOR, attemptTerminatorStatement)
and ACTION _ = (P.ACTION, attemptAction)
and TRANSITION _ = (P.TRANSITION, attemptTransition)
and PROVIDED _ = (P.PROVIDED, attemptExpr)
and PRIORITY _ = (P.PRIORITY, attemptPriority)
and STIMULUS _ = (P.STIMULUS, attemptStimulus)
and PARAMNAMES _ = (P.PARAMNAMES, many ID)
and PARAMS _ = (P.PARAMS, many ID)
and USE _ = (P.USE, attemptClause)
and SYSTEM _ = (P.SYSTEM, attemptSystem)
