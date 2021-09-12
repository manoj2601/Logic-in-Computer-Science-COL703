functor LcsLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Lcs_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* Standard yacc file format, again borrowed 
	from ml-yacc/examples/calc/calc.grm and 
	modified as per requirement*)

open AST


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\014\000\002\000\013\000\006\000\012\000\014\000\011\000\000\000\
\\001\000\001\000\014\000\002\000\013\000\006\000\012\000\014\000\024\000\000\000\
\\001\000\001\000\014\000\002\000\013\000\006\000\044\000\014\000\043\000\000\000\
\\001\000\001\000\014\000\002\000\013\000\014\000\026\000\000\000\
\\001\000\001\000\014\000\002\000\013\000\014\000\028\000\000\000\
\\001\000\001\000\014\000\002\000\013\000\014\000\030\000\000\000\
\\001\000\004\000\079\000\005\000\082\000\006\000\070\000\007\000\070\000\
\\008\000\070\000\009\000\073\000\012\000\070\000\015\000\070\000\000\000\
\\001\000\004\000\079\000\005\000\082\000\006\000\070\000\007\000\070\000\
\\008\000\070\000\012\000\070\000\015\000\070\000\000\000\
\\001\000\004\000\079\000\005\000\082\000\006\000\070\000\008\000\070\000\
\\012\000\064\000\015\000\064\000\000\000\
\\001\000\004\000\079\000\005\000\082\000\006\000\076\000\007\000\076\000\
\\008\000\076\000\009\000\076\000\012\000\076\000\015\000\076\000\000\000\
\\001\000\004\000\079\000\005\000\082\000\006\000\079\000\007\000\079\000\
\\008\000\079\000\009\000\079\000\012\000\079\000\015\000\079\000\000\000\
\\001\000\006\000\069\000\007\000\069\000\008\000\069\000\009\000\072\000\
\\012\000\069\000\015\000\069\000\000\000\
\\001\000\006\000\018\000\007\000\035\000\000\000\
\\001\000\006\000\018\000\007\000\054\000\000\000\
\\001\000\009\000\049\000\000\000\
\\001\000\009\000\056\000\000\000\
\\001\000\010\000\021\000\000\000\
\\001\000\012\000\019\000\000\000\
\\001\000\013\000\000\000\000\000\
\\001\000\015\000\034\000\000\000\
\\001\000\015\000\045\000\000\000\
\\001\000\015\000\046\000\000\000\
\\001\000\015\000\047\000\000\000\
\\001\000\015\000\048\000\000\000\
\\001\000\015\000\053\000\000\000\
\\059\000\000\000\
\\060\000\000\000\
\\061\000\001\000\014\000\002\000\013\000\006\000\012\000\014\000\011\000\000\000\
\\062\000\000\000\
\\063\000\000\000\
\\065\000\006\000\018\000\008\000\017\000\000\000\
\\066\000\006\000\018\000\000\000\
\\067\000\006\000\018\000\000\000\
\\068\000\006\000\018\000\000\000\
\\069\000\000\000\
\\071\000\000\000\
\\074\000\000\000\
\\075\000\004\000\016\000\000\000\
\\077\000\000\000\
\\078\000\005\000\015\000\000\000\
\\080\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\"
val actionRowNumbers =
"\000\000\041\000\039\000\037\000\
\\034\000\030\000\017\000\027\000\
\\016\000\000\000\001\000\003\000\
\\043\000\004\000\005\000\000\000\
\\001\000\028\000\026\000\000\000\
\\019\000\012\000\000\000\040\000\
\\000\000\038\000\000\000\036\000\
\\000\000\029\000\032\000\025\000\
\\008\000\002\000\020\000\021\000\
\\022\000\023\000\011\000\014\000\
\\031\000\000\000\001\000\007\000\
\\042\000\010\000\009\000\001\000\
\\024\000\013\000\033\000\006\000\
\\002\000\015\000\002\000\035\000\
\\018\000"
val gotoT =
"\
\\001\000\056\000\002\000\008\000\003\000\007\000\004\000\006\000\
\\005\000\005\000\007\000\004\000\008\000\003\000\009\000\002\000\
\\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\018\000\003\000\007\000\004\000\006\000\005\000\005\000\
\\007\000\004\000\008\000\003\000\009\000\002\000\010\000\001\000\000\000\
\\000\000\
\\004\000\020\000\005\000\005\000\007\000\004\000\008\000\003\000\
\\009\000\002\000\010\000\001\000\000\000\
\\005\000\021\000\007\000\004\000\008\000\003\000\009\000\002\000\
\\010\000\001\000\000\000\
\\009\000\023\000\010\000\001\000\000\000\
\\000\000\
\\008\000\025\000\009\000\002\000\010\000\001\000\000\000\
\\007\000\027\000\008\000\003\000\009\000\002\000\010\000\001\000\000\000\
\\004\000\029\000\005\000\005\000\007\000\004\000\008\000\003\000\
\\009\000\002\000\010\000\001\000\000\000\
\\005\000\030\000\007\000\004\000\008\000\003\000\009\000\002\000\
\\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\003\000\031\000\004\000\006\000\005\000\005\000\007\000\004\000\
\\008\000\003\000\009\000\002\000\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\004\000\034\000\005\000\005\000\007\000\004\000\008\000\003\000\
\\009\000\002\000\010\000\001\000\000\000\
\\000\000\
\\004\000\035\000\005\000\005\000\007\000\004\000\008\000\003\000\
\\009\000\002\000\010\000\001\000\000\000\
\\000\000\
\\004\000\036\000\005\000\005\000\007\000\004\000\008\000\003\000\
\\009\000\002\000\010\000\001\000\000\000\
\\000\000\
\\004\000\037\000\005\000\005\000\007\000\004\000\008\000\003\000\
\\009\000\002\000\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\040\000\006\000\039\000\007\000\038\000\008\000\003\000\
\\009\000\002\000\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\048\000\005\000\005\000\007\000\004\000\008\000\003\000\
\\009\000\002\000\010\000\001\000\000\000\
\\005\000\049\000\007\000\004\000\008\000\003\000\009\000\002\000\
\\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\050\000\007\000\004\000\008\000\003\000\009\000\002\000\
\\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\040\000\006\000\053\000\007\000\038\000\008\000\003\000\
\\009\000\002\000\010\000\001\000\000\000\
\\000\000\
\\005\000\050\000\006\000\055\000\007\000\038\000\008\000\003\000\
\\009\000\002\000\010\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 57
val numrules = 25
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | ATOMV of unit ->  (string) | stmtH of unit ->  (Prop)
 | stmtG of unit ->  (Prop) | stmtF of unit ->  (Prop)
 | stmtE of unit ->  (Prop) | stmtD of unit ->  (Prop)
 | stmtC of unit ->  (Prop) | stmtB of unit ->  (Prop)
 | stmtA of unit ->  (Prop) | stmt of unit ->  (Prop list)
 | start of unit ->  (Argument)
end
type svalue = MlyValue.svalue
type result = Argument
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 12) => true | _ => false
val showTerminal =
fn (T 0) => "ATOMV"
  | (T 1) => "NOTV"
  | (T 2) => "SPACEV"
  | (T 3) => "ANDV"
  | (T 4) => "ORV"
  | (T 5) => "IFV"
  | (T 6) => "THENV"
  | (T 7) => "IFFV"
  | (T 8) => "ELSEV"
  | (T 9) => "THEREFOREV"
  | (T 10) => "COMMAV"
  | (T 11) => "DOTV"
  | (T 12) => "EOF"
  | (T 13) => "LEFTP"
  | (T 14) => "RIGHTP"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.stmtA stmtA1, _, stmtA1right)) :: _ :: ( _,
 ( MlyValue.stmt stmt1, stmt1left, _)) :: rest671)) => let val  result
 = MlyValue.start (fn _ => let val  (stmt as stmt1) = stmt1 ()
 val  (stmtA as stmtA1) = stmtA1 ()
 in (HENCE(stmt, stmtA))
end)
 in ( LrTable.NT 0, ( result, stmt1left, stmtA1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.stmt stmt1, _, stmt1right)) :: ( _, ( 
MlyValue.stmtA stmtA1, stmtA1left, _)) :: rest671)) => let val  result
 = MlyValue.stmt (fn _ => let val  (stmtA as stmtA1) = stmtA1 ()
 val  (stmt as stmt1) = stmt1 ()
 in (stmtA :: stmt)
end)
 in ( LrTable.NT 1, ( result, stmtA1left, stmt1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.stmtA stmtA1, stmtA1left, stmtA1right)) :: 
rest671)) => let val  result = MlyValue.stmt (fn _ => let val  (stmtA
 as stmtA1) = stmtA1 ()
 in (stmtA :: [])
end)
 in ( LrTable.NT 1, ( result, stmtA1left, stmtA1right), rest671)
end
|  ( 3, ( ( _, ( _, _, DOTV1right)) :: ( _, ( MlyValue.stmtB stmtB1, 
stmtB1left, _)) :: rest671)) => let val  result = MlyValue.stmtA (fn _
 => let val  (stmtB as stmtB1) = stmtB1 ()
 in (stmtB)
end)
 in ( LrTable.NT 2, ( result, stmtB1left, DOTV1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.stmtB stmtB1, _, stmtB1right)) :: _ :: ( _, 
( MlyValue.stmtC stmtC1, stmtC1left, _)) :: rest671)) => let val  
result = MlyValue.stmtB (fn _ => let val  (stmtC as stmtC1) = stmtC1
 ()
 val  (stmtB as stmtB1) = stmtB1 ()
 in (BIC(stmtC, stmtB))
end)
 in ( LrTable.NT 3, ( result, stmtC1left, stmtB1right), rest671)
end
|  ( 5, ( ( _, ( _, _, RIGHTP1right)) :: ( _, ( MlyValue.stmtB stmtB1,
 _, _)) :: ( _, ( _, LEFTP1left, _)) :: rest671)) => let val  result =
 MlyValue.stmtB (fn _ => let val  (stmtB as stmtB1) = stmtB1 ()
 in (stmtB)
end)
 in ( LrTable.NT 3, ( result, LEFTP1left, RIGHTP1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.stmtC stmtC1, stmtC1left, stmtC1right)) :: 
rest671)) => let val  result = MlyValue.stmtB (fn _ => let val  (stmtC
 as stmtC1) = stmtC1 ()
 in (stmtC)
end)
 in ( LrTable.NT 3, ( result, stmtC1left, stmtC1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.stmtC stmtC2, _, stmtC2right)) :: _ :: ( _, 
( MlyValue.stmtC stmtC1, _, _)) :: ( _, ( _, IFV1left, _)) :: rest671)
) => let val  result = MlyValue.stmtC (fn _ => let val  stmtC1 = 
stmtC1 ()
 val  stmtC2 = stmtC2 ()
 in (COND(stmtC1, stmtC2))
end)
 in ( LrTable.NT 4, ( result, IFV1left, stmtC2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.stmtC stmtC2, _, stmtC2right)) :: _ :: ( _, 
( MlyValue.stmtC stmtC1, stmtC1left, _)) :: rest671)) => let val  
result = MlyValue.stmtC (fn _ => let val  stmtC1 = stmtC1 ()
 val  stmtC2 = stmtC2 ()
 in (COND(stmtC1, stmtC2))
end)
 in ( LrTable.NT 4, ( result, stmtC1left, stmtC2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.stmtC stmtC2, _, stmtC2right)) :: _ :: ( _, 
( MlyValue.stmtD stmtD1, _, _)) :: _ :: ( _, ( MlyValue.stmtC stmtC1,
 _, _)) :: ( _, ( _, IFV1left, _)) :: rest671)) => let val  result = 
MlyValue.stmtC (fn _ => let val  stmtC1 = stmtC1 ()
 val  (stmtD as stmtD1) = stmtD1 ()
 val  stmtC2 = stmtC2 ()
 in (ITE(stmtC1, stmtD, stmtC2))
end)
 in ( LrTable.NT 4, ( result, IFV1left, stmtC2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.stmtE stmtE1, stmtE1left, stmtE1right)) :: 
rest671)) => let val  result = MlyValue.stmtC (fn _ => let val  (stmtE
 as stmtE1) = stmtE1 ()
 in (stmtE)
end)
 in ( LrTable.NT 4, ( result, stmtE1left, stmtE1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RIGHTP1right)) :: ( _, ( MlyValue.stmtB stmtB1
, _, _)) :: ( _, ( _, LEFTP1left, _)) :: rest671)) => let val  result
 = MlyValue.stmtC (fn _ => let val  (stmtB as stmtB1) = stmtB1 ()
 in (stmtB)
end)
 in ( LrTable.NT 4, ( result, LEFTP1left, RIGHTP1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.stmtD stmtD2, _, stmtD2right)) :: _ :: ( _,
 ( MlyValue.stmtD stmtD1, _, _)) :: _ :: ( _, ( MlyValue.stmtC stmtC1,
 _, _)) :: ( _, ( _, IFV1left, _)) :: rest671)) => let val  result = 
MlyValue.stmtD (fn _ => let val  (stmtC as stmtC1) = stmtC1 ()
 val  stmtD1 = stmtD1 ()
 val  stmtD2 = stmtD2 ()
 in (ITE(stmtC, stmtD1, stmtD2))
end)
 in ( LrTable.NT 5, ( result, IFV1left, stmtD2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.stmtE stmtE1, stmtE1left, stmtE1right)) :: 
rest671)) => let val  result = MlyValue.stmtD (fn _ => let val  (stmtE
 as stmtE1) = stmtE1 ()
 in (stmtE)
end)
 in ( LrTable.NT 5, ( result, stmtE1left, stmtE1right), rest671)
end
|  ( 14, ( ( _, ( _, _, RIGHTP1right)) :: ( _, ( MlyValue.stmtB stmtB1
, _, _)) :: ( _, ( _, LEFTP1left, _)) :: rest671)) => let val  result
 = MlyValue.stmtD (fn _ => let val  (stmtB as stmtB1) = stmtB1 ()
 in (stmtB)
end)
 in ( LrTable.NT 5, ( result, LEFTP1left, RIGHTP1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.stmtE stmtE1, _, stmtE1right)) :: _ :: ( _,
 ( MlyValue.stmtF stmtF1, stmtF1left, _)) :: rest671)) => let val  
result = MlyValue.stmtE (fn _ => let val  (stmtF as stmtF1) = stmtF1
 ()
 val  (stmtE as stmtE1) = stmtE1 ()
 in (AND(stmtF, stmtE))
end)
 in ( LrTable.NT 6, ( result, stmtF1left, stmtE1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.stmtF stmtF1, stmtF1left, stmtF1right)) :: 
rest671)) => let val  result = MlyValue.stmtE (fn _ => let val  (stmtF
 as stmtF1) = stmtF1 ()
 in (stmtF)
end)
 in ( LrTable.NT 6, ( result, stmtF1left, stmtF1right), rest671)
end
|  ( 17, ( ( _, ( _, _, RIGHTP1right)) :: ( _, ( MlyValue.stmtB stmtB1
, _, _)) :: ( _, ( _, LEFTP1left, _)) :: rest671)) => let val  result
 = MlyValue.stmtE (fn _ => let val  (stmtB as stmtB1) = stmtB1 ()
 in (stmtB)
end)
 in ( LrTable.NT 6, ( result, LEFTP1left, RIGHTP1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.stmtF stmtF1, _, stmtF1right)) :: _ :: ( _,
 ( MlyValue.stmtG stmtG1, stmtG1left, _)) :: rest671)) => let val  
result = MlyValue.stmtF (fn _ => let val  (stmtG as stmtG1) = stmtG1
 ()
 val  (stmtF as stmtF1) = stmtF1 ()
 in (OR(stmtG, stmtF))
end)
 in ( LrTable.NT 7, ( result, stmtG1left, stmtF1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.stmtG stmtG1, stmtG1left, stmtG1right)) :: 
rest671)) => let val  result = MlyValue.stmtF (fn _ => let val  (stmtG
 as stmtG1) = stmtG1 ()
 in (stmtG)
end)
 in ( LrTable.NT 7, ( result, stmtG1left, stmtG1right), rest671)
end
|  ( 20, ( ( _, ( _, _, RIGHTP1right)) :: ( _, ( MlyValue.stmtB stmtB1
, _, _)) :: ( _, ( _, LEFTP1left, _)) :: rest671)) => let val  result
 = MlyValue.stmtF (fn _ => let val  (stmtB as stmtB1) = stmtB1 ()
 in (stmtB)
end)
 in ( LrTable.NT 7, ( result, LEFTP1left, RIGHTP1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.stmtG stmtG1, _, stmtG1right)) :: ( _, ( _,
 NOTV1left, _)) :: rest671)) => let val  result = MlyValue.stmtG (fn _
 => let val  (stmtG as stmtG1) = stmtG1 ()
 in (NOT(stmtG))
end)
 in ( LrTable.NT 8, ( result, NOTV1left, stmtG1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.stmtH stmtH1, stmtH1left, stmtH1right)) :: 
rest671)) => let val  result = MlyValue.stmtG (fn _ => let val  (stmtH
 as stmtH1) = stmtH1 ()
 in (stmtH)
end)
 in ( LrTable.NT 8, ( result, stmtH1left, stmtH1right), rest671)
end
|  ( 23, ( ( _, ( _, _, RIGHTP1right)) :: ( _, ( MlyValue.stmtB stmtB1
, _, _)) :: ( _, ( _, LEFTP1left, _)) :: rest671)) => let val  result
 = MlyValue.stmtG (fn _ => let val  (stmtB as stmtB1) = stmtB1 ()
 in (stmtB)
end)
 in ( LrTable.NT 8, ( result, LEFTP1left, RIGHTP1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ATOMV ATOMV1, ATOMV1left, ATOMV1right)) :: 
rest671)) => let val  result = MlyValue.stmtH (fn _ => let val  (ATOMV
 as ATOMV1) = ATOMV1 ()
 in (ATOM(ATOMV))
end)
 in ( LrTable.NT 9, ( result, ATOMV1left, ATOMV1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Lcs_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ATOMV (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ATOMV (fn () => i),p1,p2))
fun NOTV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun SPACEV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun ANDV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun ORV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun IFV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun THENV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun IFFV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSEV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun THEREFOREV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMAV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun DOTV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LEFTP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun RIGHTP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
end
end
