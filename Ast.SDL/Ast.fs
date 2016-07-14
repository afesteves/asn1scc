﻿module Ast.SDL

//type PLACEHOLDER = int

type NonEmpty<'a>   = { first: 'a; rest: 'a list}

type ID = string
type Filename = string
type Sort = string
type InformalText = String

type PRFile = {
    clauses: UseClause list
    systems: System list
    processes: Process list
}

and UseClause    = { asn1: Filename Option; package: ID; uses: ID list }
and System       = { id: ID; entities: SystemEntity list }
and Channel      = { id: ID; routes: Block   Route NonEmpty }
and SignalRoute  = { id: ID; routes: Process Route NonEmpty }
and Block        = { id: ID; entities: BlockEntity list }

and SystemEntity = Choice<Signal, Block, TextArea, Procedure, Channel>
and BlockEntity  = Choice<Signal, Block, SignalRoute, Connection, Process>

and Connection = { channels: ID NonEmpty; routes: ID NonEmpty }
and Signal = { 
    id: ID
    parameterNames: ID list
    vars: ID list
    cifEnd: CIFEnd
}

and Route<'a>    = { source: 'a; dest: 'a; signals: Signal NonEmpty }

and CIFCoordinates = { 
    x: int; width: int
    y: int; height: int
}

and PassBy = In | Out | InOut
and Variable     = { id: ID; sort: Sort}
and VarParameter = { id: ID; sort: Sort; passBy: PassBy } 
and VarDecl      = { id: ID; sort: Sort; init: Expr option }
and Result       = { id: ID option; sort: Sort  }

and Variables = Variable list
and VarParameters = VarParameter list

and Procedure = {
    cif: CIFCoordinates option
    id: ID
    end1: CIFEnd
    end2: CIFEnd
    parameters: VarParameters
    result: Result option
//  something:
    body: ProcedureBody
    external: bool
}
and ProcedureBody = 
    | PB_Start of start: Start * parts: BodyPart list
    | PB_Parts of parts: BodyPart NonEmpty

and TextArea = {
    cif: CIFCoordinates
    content: Content option
}

and Content = ContentEntity list

and ContentEntity = 
    | C_Procedure    of Procedure
    | C_UseClause    of UseClause
    | C_Signal       of Signal
    | C_Result       of Result 
    | C_Parameters   of VarParameters
    | C_Timer        of ID list
    | C_Synonym      of Sort * ID * Expr
    | C_Refinement   of Sort * Sort * RangeCondition list
    | C_Declarations of VarDecl list
    | C_Newtype      of Newtype

and Newtype = 
    | N_Dictionary of id: ID * key: Sort * value: Sort
    | N_Struct of id: ID * vars: Variables


// and Instances = { initial: int; max: int}
and Process = {
    cif: CIFCoordinates option
    id: ID
//  instances: Instances
//  instanceType:
    isReferenced: bool
    cifEnd1: CIFEnd
    cifEnd2: CIFEnd option
    parameters: Variables
//  something:
    start: Start
    body: BodyPart list
}

and BodyPart = Choice<State, FreeAction>

and Start = { 
    cif: CIFCoordinates option 
    hyperlink: Hyperlink option
    cifEnd: CIFEnd option
    entryState: State
    transition: Transition
}

and FreeAction = {
    cif: CIFCoordinates option
    hyperlink: Hyperlink option
    connector: ID option
    transition: Transition
}

and State = {
    cif: CIFCoordinates option
    hyperlink: Hyperlink option
    body: StateBody
    parts: StatePart list
    name: ID option
    cifEnd1: CIFEnd
    cifEnd2: CIFEnd
}

and StateBody =
    | SB_States of ID NonEmpty
    | SB_Exception of ID list

and StatePart = 
    | SP_Input of InputPart
    | SP_Save of body: SaveBody * cifEnd: CIFEnd
    | SP_Spontaneous of SpontaneousTransition
    | SP_ConnectPart of ConnectPart
    | SP_Continuous of ContinuousSignal
//  | SP_Priority of PriorityInput // Not supported

and SaveBody = 
    | SB_All
    | SB_List of ID NonEmpty

and SpontaneousTransition = {
    cif: CIFCoordinates option
    hyperlink: Hyperlink option
    cifEnd: CIFEnd option
    guard: Expr
    transition: Transition
}

and ConnectPart = {
    cif: CIFCoordinates option
    hyperlink: Hyperlink option
    cifEnd: CIFEnd option
    transition: Transition option
    //connections: 
}

and ContinuousSignal = {
    cif: CIFCoordinates option
    hyperlink: Hyperlink option
    expr: Expr
    priority: Priority option
    cifEnd: CIFEnd
    transition: Transition option
}

and Priority = {
    value: Expr
    cifEnd: CIFEnd
}

and CompositeState = Choice<CompositeStateGraph, StateAggregation>

and CompositeStateGraph = {
    name: ID
    cifEnd1: CIFEnd
    cifEnd2: CIFEnd
    points: ConnectionPoint list
    body: CompositeStateBody
} 
and StateAggregation = {
    name: ID
    cifEnd1: CIFEnd
    cifEnd2: CIFEnd
    entities: Choice<TextArea, Procedure> list
    body: StateAggregationBody
}

and CompositeStateBody = {  
    entities: Choice<TextArea, Procedure, CompositeState> list
    start: Start list
    parts: BodyPart list

}

and StateAggregationBody = {
    entities: Choice<CompositeState, StatePartitionConnection> list
    states: State list
}
and StatePartitionConnection = {
    inner: EntryPoint
    outer: EntryPoint
    cifEnd: CIFEnd option
}
and EntryPoint = {
    id: ID
    via: Point
}


and Point = 
    | DEFAULT
    | StatePoint of id: ID

and Direction = IN | OUT

and ConnectionPoint = {
    direction: Direction
    states: ID NonEmpty
    cifEnd: CIFEnd
}

// this is only the "basic input part" from SDL92
and InputPart = {
    cif: CIFCoordinates
    hyperlink: Hyperlink
    stimuli: Stimulus NonEmpty
    cifEnd: CIFEnd
    guard: Expr
    transition: Transition option
}

and Stimulus = {
    id: ID
    vars: ID list
}

and Transition = {
    label: Label option
    actions: Action NonEmpty
    statement: TerminatorStatement
}

and Action = { label: Label option; body: ActionBody }

and ActionBody = 
    | A_Task of Task
    | A_TaskBody of TaskBody
    | A_Output of Output
    | A_CreateRequest of CreateRequest
    | A_Decision of Decision
    | A_TransitionOption of TransitionOption
    | A_Export of exports: ID list
    | A_Timer of TimerOperation
    | A_ProcedureCall of ProcedureCall
  
and TimerOperation =
    | Set of TimerSetPart NonEmpty
    | Reset of TimerAccess NonEmpty

and TimerSetPart = { expr: Expr option; access: TimerAccess }
and TimerAccess = { id: ID; expressions: Expr NonEmpty }

and ProcedureCall = {
    cif: CIFCoordinates option
    hyperlink: Hyperlink option
    cifEnd: CIFEnd
    id: ID
    args: Arguments
}

and Decision = {
    cif: CIFCoordinates option
    hyperlink: Hyperlink option
    question: Question option
    answers: Answers
    cifEnd1: CIFEnd
    cifEnd2: CIFEnd
}

and TransitionOption = {
    question: Question
    answers: Answers
    cifEnd1: CIFEnd option
    cifEnd2: CIFEnd option
}

and Answers =
    | A_Simple of answerPart: AnswerPart * elsePart: ElsePart
    | A_Complex of mainAnswer: AnswerPart * otherAnswers: AnswerPart NonEmpty * elsePart: ElsePart option

and Answer = Choice<RangeCondition, InformalText>
and AnswerPart = {
    cif: CIFCoordinates option
    hyperlink: Hyperlink option
    transition: Transition option
    answer: Answer option
}
and Question = Choice<InformalText, Expr>

and ElsePart = {
    cif: CIFCoordinates option
    hyperlink: Hyperlink option
    transition: Transition option
}

//and RangeCondition = Range NonEmpty

and RangeCondition =
    | Continuous of Interval
    | Union of Interval * Interval

and Interval = { start: int option; stop: int option}

and CreateRequest = { pid: PID; args: Arguments }

and PID =
    | THIS
    | SELF
    | PARENT
    | OFFSPRING
    | SENDER
    | PID of ID

and Output = {
    cif: CIFCoordinates option
    hyperlink: Hyperlink option
    endCIF: CIFEnd option
    items: OutputItem NonEmpty
    destination: PID
    //via: Via
}

and OutputItem = { signal: Signal; args: Arguments }

(*
and Via = 
    | All
    | items of ViaItem NonEmpty

and ViaItem = 
    | O_SignalRoute of SignalRoute
    | O_Channel of Channel
    | O_Gate of 
*)

and Arguments = Expr NonEmpty


and Task = {
    cif: CIFCoordinates option
    link: Hyperlink
    body: TaskBody
    endCIF: CIFEnd
}

and TaskBody =
    | TAssign of Assignment NonEmpty
    | TInformal of InformalText NonEmpty
    | TForLoop of ForLoop NonEmpty

and ForLoop = {
    var: ID
    over: ForValues
    transition: Transition option 
}
and ForValues = Choice<Range, ID>

and Range = {start: int; stop: int option; step: int option}

and Assignment = { lhs: LHS; value: Expr }

and LHS = Choice<ID, FieldAccess>

and FieldAccess = { value: Expr; field: ID}

and Op1 =
    | Neg of Expr
    | Not of Expr

and Op2 = 
    | Implies
    | Or
    | Xor
    | And
    | EQ
    | NE
    | GT
    | GE
    | LT
    | LE
//    | IN what? for each?
    | Add
    | Sub
    | Append
    | Times
    | Division
    | Modulo
    | Remainder

and Expr =
    | ExprNull
    | ExprBool of bool
    | ExprInt of int
    | ExprStr of string
    | ExprFloat of float
    | ExprVar of ID
    | Expr1 of Op1 * Expr
    | Expr2 of Op2 * Expr * Expr
    | ExprIFE of Expr * Expr * Expr
    | ExprCall of func: Expr * args: Expr NonEmpty
    | ExprField of FieldAccess

(*
unary_expression
        :       primary
        ;

primary:
        |       ID ':' expression           -> ^(CHOICE ID expression)
        |       ID                          -> ^(VARIABLE ID)
        |       '{'
                named_value (COMMA named_value)*
                '}'                         -> ^(SEQUENCE named_value+)
        |       '{'
                primary (COMMA primary)*
                '}'                         -> ^(SEQOF primary+)
        |       STATE^
        ;


// { a 5 } (SEQUENCE field value)
named_value
        :       ID expression
        ;

sort    :       sort_id
        ->      ^(SORT sort_id)
        ;


type_inst
        :       type_id
        ->      ^(TYPE_INSTANCE type_id)
        ;


syntype :       syntype_id
        ;


variable_access
        :       variable_id
        ;


operator_application
        :       operator_id '('active_expression_list ')'
        ;





external_synonym
        :       external_synonym_id
        ;

expression_list
        :       expression (',' expression)*
        ->      expression+
        ;

*)

and Label = {
    cif: CIFCoordinates
    connector: ID
}

and Terminator =
    | NextState of state: State option
    | Join of connector: ID
    | Stop
    | ReturnStatement of Expr option

and TerminatorStatement = {
    label: Label option
    cif: CIFCoordinates option
    hyperlink: Hyperlink option
    terminator: Terminator
    cifEnd: CIFEnd
}

and CIFEnd = {
    coords: CIFCoordinates option
    hyperlink: Hyperlink option
    text: string
}

and Hyperlink = String