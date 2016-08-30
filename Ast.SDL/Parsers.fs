module Parsers

open Ast
open Constructors
open TreeParser
open Utils
open AstSDL
open Antlr.SDL
open FsUtils
open FSharpx.Collections

#nowarn "0046"
#nowarn "1189"

type P = sdlParser

let attemptExpr = fail
let attemptPassBy = fail

let attemptPriority = fail
let attemptTerminator = fail


let rec attemptStr = Parser(fun (t,s) -> (Output t.Text, s))

and attemptID = attemptStr

and attemptASN1 = one STRING

and attemptHyperlink = one STRING

and attemptSort = one ID

and attemptInt = attemptStr |>> int

and attemptRoute =
    pure Route
      <*> one ID
      <*> one ID
      <*> many1 ID

and attemptChannel =
    pure Channel
      <*> one ID
      <*> many1 ROUTE

and attemptSignalRoute =
    pure SignalRoute
      <*> one ID
      <*> many1 ROUTE

and attemptResult =
    pure Result
      <*> opt ID 
      <*> one SORT

and attemptVariable = 
    pure Parameter
      <*> one ID
      <*> one SORT

and attemptCIFCoords =
    pure CIFCoordinates 
      <*> one INT
      <*> one INT
      <*> one INT
      <*> one INT

and attemptContent =
    pure Content
      <*> (one PROCEDURE_SIGNATURE <|> pure [])
      <*> opt RESULT
      <*> many PROCEDURE
      <*> (many DCL |>> List.concat)
      <*> pure []
      <*> many SIGNAL
      <*> many USE

and attemptTextArea =
    pure TextArea
      <*> one CIF
      <*> opt TEXTAREA_CONTENT
      .>> exists P.ENDTEXT
     
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

and attemptComment =
    pure CIFEnd
      <*> opt CIF
      <*> opt HYPERLINK
      <*> one STRING

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

and attemptFloatingLabel =
    pure FloatingLabel
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

and attemptInOut =
    pure (fun i o io -> (i || io, o || io))
      <*> exists P.IN
      <*> exists P.OUT
      <*> exists P.INOUT

and attemptProcedureParameterGroup =
    pure (fun (i,o) ids sort -> ids |> List.map (fun id -> ProcedureParameter id sort i o))
      <*> attemptInOut
      <*> many ID
      <*> one SORT

and attemptProcessParameterGroup =
    pure (fun ids sort -> ids |> List.map (fun id -> Parameter id sort))
      <*> many ID
      <*> one SORT

and attemptDeclarationGroup =
    pure (fun ids s e -> ids |> List.map (fun id -> VarDecl id s e))
      <*> many ID
      <*> one SORT
      <*> pure None

and attemptDcl = many1 DECLARATION_GROUP |>> NonEmptyList.toList
and attemptProcessSignature = many1 PROCESS_PARAMETER_GROUP |>> NonEmptyList.toList
and attemptProcedureSignature = many1 PROCEDURE_PARAMETER_GROUP |>> NonEmptyList.toList

and attemptProcess =
    pure Process
      <*> debug(opt CIF)
      <*> one ID
      <*> exists P.REFERENCED
      <*> opt END
      <*> (one PROCESS_SIGNATURE <|> pure [])
      <*> many TEXT_AREA
      <*> many PROCEDURE
      <*> pure []
      <*> pure None

and attemptProcessBody =
    pure ProcessBody
      <*> pure None
      <*> pure []
      <*> pure []

and attemptProcedure' () =
    pure (fun c i e1 e2 fp r (tx, pr) b e -> Procedure c i e1 e2 fp r tx pr b e)
      <*> opt CIF
      <*> one ID
      <*> opt END
      <*> opt END
      <*> (one PROCEDURE_SIGNATURE <|> pure [])
      <*> opt RESULT
      <*> groups2 (choice2 (one TEXT_AREA, one PROCEDURE))
      <*> pure None
      <*> exists P.EXTERNAL

and attemptProcedure = attemptProcedure' ()

and attemptSignal =
    pure Signal 
      <*> (many PARAMNAMES |>> List.concat)
      <*> one ID
      <*> (many PARAMS |>> List.concat)
      <*> opt END

and attemptConnection =
    pure Connection
      <*> one ID
      <*> one ID

and attemptBlockEntity =
  choice5
    ( one SIGNAL
    , one BLOCK
    , one SIGNALROUTE
    , one CONNECTION
    , one PROCESS)

and attemptBlock' () =
    pure (fun i (ss, bs, rs, cs, ps) -> Block i ss bs rs cs ps)
      <*> one ID
      <*> groups5 attemptBlockEntity

and attemptBlock = attemptBlock'()

and attemptSystemEntity =
  choice5
    ( one SIGNAL
    , one BLOCK
    , one TEXT_AREA
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

and STRING _ = (P.STRING, attemptStr)
and ID _ = (P.ID, attemptID)
and SIGNAL _ = (P.SIGNAL, attemptSignal)
and BLOCK _ = (P.BLOCK, recursive attemptBlock')
and ROUTE _ = (P.ROUTE, attemptRoute)
and SIGNALROUTE _ = (P.SIGNALROUTE, attemptSignalRoute)
and CONNECTION _ = (P.CONNECTION, attemptConnection)
and PROCESS _ = (P.PROCESS, attemptProcess)
and TEXT_AREA _ = (P.TEXTAREA, attemptTextArea)
and PROCEDURE _ = (P.PROCEDURE, recursive attemptProcedure')
and CHANNEL _ = (P.CHANNEL, attemptChannel)
and SORT _ = (P.SORT, attemptSort)
and HYPERLINK _ = (P.HYPERLINK, attemptHyperlink)
and INT _ = (P.INT, attemptInt)
and ASN1 _ = (P.ASN1, attemptASN1)
and END _ = (P.COMMENT, attemptComment)
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
and RESULT _ = (P.RETURNS, attemptResult)
and PROCEDURE_PARAMETER_GROUP _ = (P.PARAM, attemptProcedureParameterGroup)
and PROCESS_PARAMETER_GROUP _ = (P.PARAM, attemptProcessParameterGroup)
and DECLARATION_GROUP _ = (P.VARIABLES, attemptDeclarationGroup)
and PROCESS_SIGNATURE _ = (P.PFPAR, attemptProcessSignature |>> List.concat)
and PROCEDURE_SIGNATURE _ = (P.FPAR, attemptProcedureSignature |>> List.concat)
and DCL _ = (P.DCL, attemptDcl |>> List.concat)
//and PROCESS_BODY _ = (P.)
and TYPE_INSTANCE _ = (P.TYPE_INSTANCE, attemptSort)
