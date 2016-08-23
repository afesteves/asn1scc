module Constructors

open AstSDL

let PRFile clauses systems processes : PRFile = { clauses=clauses; systems=systems; processes=processes }
let UseClause asn1 cifEnd package uses : UseClause = { asn1=asn1; cifEnd=cifEnd; package=package; uses=uses }
let System id signals blocks textAreas procedures channels : System = { id=id; signals=signals; blocks=blocks; textAreas=textAreas; procedures=procedures; channels=channels }
let Block id signals blocks signalRoutes connections processes : Block = { id=id; signals=signals; blocks=blocks; signalRoutes=signalRoutes; connections=connections; processes=processes }
let Route source dest signals : Route = { source=source; dest=dest; signals=signals }
let Channel id routes : Channel = { id=id; routes=routes }
let SignalRoute id routes : SignalRoute = { id=id; routes=routes }
let Connection channels routes : Connection = { channels=channels; routes=routes }
let Signal parameterNames id vars cifEnd : Signal = { parameterNames=parameterNames; id=id; vars=vars; cifEnd=cifEnd }
let CIFCoordinates x width y height : CIFCoordinates = { x=x; width=width; y=y; height=height }
let Variable id sort : Variable = { id=id; sort=sort }
let VarParameter id sort passBy : VarParameter = { id=id; sort=sort; passBy=passBy }
let VarDecl id sort init : VarDecl = { id=id; sort=sort; init=init }
let Result id sort : Result = { id=id; sort=sort }
let Procedure cif id end1 end2 parameters result body external : Procedure = { cif=cif; id=id; end1=end1; end2=end2; parameters=parameters; result=result; body=body; external=external }
let TextArea cif content : TextArea = { cif=cif; content=content }
let Process cif id isReferenced cifEnd1 cifEnd2 parameters start body : Process = { cif=cif; id=id; isReferenced=isReferenced; cifEnd1=cifEnd1; cifEnd2=cifEnd2; parameters=parameters; start=start; body=body }
let Start cif hyperlink cifEnd entryState transition : Start = { cif=cif; hyperlink=hyperlink; cifEnd=cifEnd; entryState=entryState; transition=transition }
let FreeAction cif hyperlink connector transition : FreeAction = { cif=cif; hyperlink=hyperlink; connector=connector; transition=transition }
let State cif hyperlink body parts name cifEnd1 cifEnd2 : State = { cif=cif; hyperlink=hyperlink; body=body; parts=parts; name=name; cifEnd1=cifEnd1; cifEnd2=cifEnd2 }
let SpontaneousTransition cif hyperlink cifEnd guard transition : SpontaneousTransition = { cif=cif; hyperlink=hyperlink; cifEnd=cifEnd; guard=guard; transition=transition }
let ConnectPart cif hyperlink cifEnd transition : ConnectPart = { cif=cif; hyperlink=hyperlink; cifEnd=cifEnd; transition=transition }
let ContinuousSignal cif hyperlink guard priority cifEnd transition : ContinuousSignal = { cif=cif; hyperlink=hyperlink; guard=guard; priority=priority; cifEnd=cifEnd; transition=transition }
let Priority value cifEnd : Priority = { value=value; cifEnd=cifEnd }
let CompositeStateGraph name cifEnd1 cifEnd2 points body : CompositeStateGraph = { name=name; cifEnd1=cifEnd1; cifEnd2=cifEnd2; points=points; body=body }
let StateAggregation name cifEnd1 cifEnd2 entities body : StateAggregation = { name=name; cifEnd1=cifEnd1; cifEnd2=cifEnd2; entities=entities; body=body }
let CompositeStateBody entities start parts : CompositeStateBody = { entities=entities; start=start; parts=parts }
let StateAggregationBody entities states : StateAggregationBody = { entities=entities; states=states }
let StatePartitionConnection inner outer cifEnd : StatePartitionConnection = { inner=inner; outer=outer; cifEnd=cifEnd }
let EntryPoint id via : EntryPoint = { id=id; via=via }
let ConnectionPoint direction states cifEnd : ConnectionPoint = { direction=direction; states=states; cifEnd=cifEnd }
let InputPart cif hyperlink stimuli cifEnd guard transition : InputPart = { cif=cif; hyperlink=hyperlink; stimuli=stimuli; cifEnd=cifEnd; guard=guard; transition=transition }
let Stimulus id vars : Stimulus = { id=id; vars=vars }
let Transition label actions statement : Transition = { label=label; actions=actions; statement=statement }
let Action label body : Action = { label=label; body=body }
let TimerSetPart expr access : TimerSetPart = { expr=expr; access=access }
let TimerAccess id expressions : TimerAccess = { id=id; expressions=expressions }
let ProcedureCall cif hyperlink cifEnd id args : ProcedureCall = { cif=cif; hyperlink=hyperlink; cifEnd=cifEnd; id=id; args=args }
let Decision cif hyperlink question answers cifEnd1 cifEnd2 : Decision = { cif=cif; hyperlink=hyperlink; question=question; answers=answers; cifEnd1=cifEnd1; cifEnd2=cifEnd2 }
let TransitionOption question answers cifEnd1 cifEnd2 : TransitionOption = { question=question; answers=answers; cifEnd1=cifEnd1; cifEnd2=cifEnd2 }
let AnswerPart cif hyperlink transition answer : AnswerPart = { cif=cif; hyperlink=hyperlink; transition=transition; answer=answer }
let ElsePart cif hyperlink transition : ElsePart = { cif=cif; hyperlink=hyperlink; transition=transition }
let Interval start stop : Interval = { start=start; stop=stop }
let CreateRequest pid args : CreateRequest = { pid=pid; args=args }
let Output cif hyperlink endCIF items destination : Output = { cif=cif; hyperlink=hyperlink; endCIF=endCIF; items=items; destination=destination }
let OutputItem signal args : OutputItem = { signal=signal; args=args }
let Task cif link body endCIF : Task = { cif=cif; link=link; body=body; endCIF=endCIF }
let ForLoop var over transition : ForLoop = { var=var; over=over; transition=transition }
let Range start stop step : Range = { start=start; stop=stop; step=step }
let Assignment lhs value : Assignment = { lhs=lhs; value=value }
let FieldAccess value field : FieldAccess = { value=value; field=field }
let Label cif connector : Label = { cif=cif; connector=connector }
let TerminatorStatement label cif hyperlink terminator cifEnd : TerminatorStatement = { label=label; cif=cif; hyperlink=hyperlink; terminator=terminator; cifEnd=cifEnd }
let CIFEnd coords hyperlink text : CIFEnd = { coords=coords; hyperlink=hyperlink; text=text }
