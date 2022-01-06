use "FOL.sml";
use "struct.sml";
open fol

fun checkArity a blist =
  case blist of
  [] => ~1
  |(hName,hValue)::xs => if(hName = a) then hValue
           else checkArity a xs

fun checkVarIfBound a blist =
  case blist of
  [] => false
  |x::xs => if (x = a) then true
           else checkVarIfBound a xs

fun update_termList_with_bound tl boundVarList =
  case tl of
  [] => []
  | x::xs => [(x,boundVarList)]@(update_termList_with_bound xs boundVarList)

fun isValid_Term termList funArity atomArity =
  case termList of
  [] => true
  |(h,hbound)::xs => (case h of
                     CONST(s) => isValid_Term xs funArity atomArity
                     |VAR(s) => (if (checkVarIfBound s hbound)
                                 then isValid_Term xs funArity atomArity
                                 else raise NotClosed
                                )
                     |FUN(s,tl) => (
                                      let val arity = length tl in
                                        if( (checkArity s funArity) = ~1)
                                        then  isValid_Term ((update_termList_with_bound tl hbound)@xs)  (funArity@[(s,arity)])  (atomArity)
                                        else(
                                              if (arity = (checkArity s funArity))
                                              then isValid_Term ((update_termList_with_bound tl hbound)@xs)  (funArity)  (atomArity)
                                              else raise NotWFT
                                            )
                                      end
                                    )
                    )


fun isValid_pred predList funArity atomArity =
  case predList of
  [] => true
  |(h,hbound)::xs => (case h of
                     FF => raise NotWFA
                     |ATOM(s,tl) => (let val arity = length tl in
                                        if( (checkArity s atomArity) = ~1)
                                        then (
                                               if( (isValid_Term (update_termList_with_bound tl hbound) (funArity) ((s,arity)::atomArity) ) = true )
                                               then isValid_pred xs funArity ((s, arity)::atomArity)
                                               else false
                                             )
                                        else(
                                             if (arity = (checkArity s atomArity))
                                             then (
                                                   if(isValid_Term (update_termList_with_bound tl hbound) funArity atomArity = true)
                                                   then isValid_pred xs funArity atomArity
                                                   else false
                                                  )
                                             else raise NotWFP
                                        )
                                     end
                                    )

                     |NOT(pred) => isValid_pred ((pred,hbound)::xs) (funArity) (atomArity)

                     |AND(pred1,pred2) => isValid_pred ((pred1,hbound)::((pred2,hbound)::xs)) (funArity) (atomArity)

                     |OR(pred1,pred2) => isValid_pred ((pred1,hbound)::((pred2,hbound)::xs)) (funArity) (atomArity)

                     |COND(pred1,pred2) => isValid_pred ((pred1,hbound)::((pred2,hbound)::xs)) (funArity) (atomArity)

                     |BIC(pred1,pred2) => isValid_pred ((pred1,hbound)::((pred2,hbound)::xs)) (funArity) (atomArity)

                     |ITE(pred1,pred2,pred3) => isValid_pred ((pred1,hbound)::((pred2,hbound)::((pred3, hbound)::xs))) (funArity) (atomArity)

                     |ALL(te,pred) => (case te of
                                      VAR(s) => isValid_pred ((pred,s::hbound)::xs) (funArity) (atomArity)
                                      |_ => raise NotVAR
                                    )

                     |EX(te,pred) => (case te of
                                      VAR(s) => isValid_pred ((pred,s::hbound)::xs) (funArity) (atomArity)
                                      |_ => raise NotVAR
                                   )
                    )


fun isValid_Arg (arg:Argument) =
  case arg of
  HENCE(predList,p) => (
    let val funArity = []
    val atomArity = []
    val boundVar = []
    in 
    isValid_pred (update_termList_with_bound (predList@[NOT(p)]) boundVar) funArity atomArity
    end
  )
(*  EXCEPTIONS DONE   *)



val dir : (int * int) list ref = ref []
val blueDir : (int * int) list ref = ref []
val count : int ref = ref 1

fun printFile (str:string, filename) =
			let val f = TextIO.openOut filename
			in 	(TextIO.output (f, str); TextIO.closeOut f) 
			end

fun termToString(t) =
  case t of 
  VAR(s) => s
  | FUN(s, tl) => s ^ "(" ^ tlToString(tl) ^ ")"
  | CONST(s) => s
and
 tlToString(tl) =
  case tl of
   [] => ""
    | x::[] => termToString(x)
    | x::xs => (termToString(x) ^ ", " ^ tlToString(xs))

fun predToString(pred) =
  case pred of
  FF => "final"
  | ATOM(str, tl) => str ^ "(" ^ tlToString(tl) ^ ")"
  | NOT(pred) => "\\neg (" ^ predToString(pred)^")"
  | AND(pred1, pred2) => predToString(pred1) ^ " \\land " ^ predToString(pred2)
  | OR(pred1, pred2) => predToString(pred1) ^ " \\lor " ^ predToString(pred2)
  | COND(pred1, pred2) => predToString(pred1) ^ " \to " ^ predToString(pred2)
  | BIC(pred1, pred2) => predToString(pred1) ^ " <-> " ^ predToString(pred2)
  | ITE(pred1, pred2, pred3) => " IF " ^ predToString(pred1) ^ " THEN " ^ predToString(pred2) ^ " ELSE " ^ predToString(pred3)
  | _ => "unknown"




fun getDelta(arg) =
  case arg of
  HENCE(pl, p) => pl@[NOT(p)]


fun make_node(str) =
  let val a = 1 in
  count := (!count+1);
  (Int.toString(!count-1) ^ " [texlbl=\"\\underline{ $" ^ Int.toString(!count-1) ^". " ^ str ^ "$ }\"];")
  end

fun make_tree(del, parent) =
  case del of 
  x::xs => (
    case x of 
    ATOM(s, tl) => (
      dir := (parent, ((!count)))::(!dir);
      make_node(predToString(x))::make_tree(xs, (!count-1))
    )
    
    | NOT(ATOM(s, tl)) => (
      dir := (parent, ((!count)))::(!dir);
      make_node(predToString(x))::make_tree(xs, (!count-1))
    )
    | NOT(NOT(pred)) => (
      dir := (parent, ((!count)))::(!dir);
      make_node(predToString(x))::make_tree(pred::xs, (!count-1))
    )
    | AND(pred1, pred2) => (
      dir := (parent, ((!count)))::(!dir);
      blueDir := (((!count)), (!count+2))::(!blueDir);
      make_node(predToString(x))::make_tree(pred1::(pred2::xs), (!count-1))
    )
    | OR(pred1, pred2) => (
      dir := (parent, ((!count)))::(!dir);
      let val par = !count in
        make_node(predToString(x))::(make_tree(pred1::xs, par)@make_tree(pred2::xs, par))
      end
    )
    | NOT(AND(pred1, pred2)) => (
      dir := (parent, ((!count)))::(!dir);
      let val par = !count in
        make_node(predToString(x))::(make_tree(NOT(pred1)::xs, par)@make_tree(NOT(pred2)::xs, par))
      end
    )
    | NOT(OR(pred1, pred2)) => (
      dir := (parent, ((!count)))::(!dir);
      blueDir := (((!count)), (!count+2))::(!blueDir);
      make_node(predToString(x))::make_tree(NOT(pred1)::(NOT(pred2)::xs), (!count-1))
    )
    | COND(pred1, pred2) => (
      dir := (parent, ((!count)))::(!dir);
      let val par = !count in
        make_node(predToString(x))::(make_tree(NOT(pred1)::xs, par)@make_tree(pred2::xs, par))
      end
    )
    | NOT(COND(pred1, pred2)) => (
      dir := (parent, ((!count)))::(!dir);
      blueDir := (((!count)), (!count+2))::(!blueDir);
      make_node(predToString(x))::make_tree(pred1::(NOT(pred2)::xs), (!count-1))
    )
    | BIC(pred1, pred2) => (
      dir := (parent, ((!count)))::(!dir);
      let val par = !count in
        make_node(predToString(x))::(make_tree(AND(pred1, pred2)::xs, par)@make_tree(AND(NOT(pred1), NOT(pred2))::xs, par))
      end
    )
    | NOT(BIC(pred1, pred2)) => (
      dir := (parent, ((!count)))::(!dir);
      let val par = !count in
        make_node(predToString(x))::(make_tree(AND(pred1, NOT(pred2))::xs, par)@make_tree(AND(NOT(pred1), pred2)::xs, par))
      end
    )
    | ITE(pred1, pred2, pred3) => (
      dir := (parent, ((!count)))::(!dir);
      blueDir := (((!count)), (!count+2))::(!blueDir);
      make_node(predToString(x))::make_tree(COND(pred1,pred2)::(COND(NOT(pred1), pred3)::xs), (!count-1))
    )
  )
  | _ => [""]


(* printFile(start_message, "sample.dot") *)

fun getString(ls) =
  case ls of
  x::xs => "\t" ^ x ^ "\n" ^ getString(xs)
  | _ => "\n"

fun getSubgraph_directory(l) =
  case l of 
  (a1, a2)::xs => ("\t" ^ Int.toString(a1) ^ " -> " ^ Int.toString(a2) ^ ";\n" ^ getSubgraph_directory(xs))
  | _ => ""


val arg = HENCE([AND( ATOM("k",[VAR("a"),VAR("b")]) , ATOM("j",[VAR("a")])  ), ATOM("a2", [])], ATOM("a3", []));
(* val t = isValid_Arg(arg); *)
(* val arg = HENCE([OR(ATOM("a1", []), ATOM("a2", []))], NOT(ATOM("a3", []))) *)
val del = getDelta(arg)

val filename = "mygraph.dot";
val start_message = "digraph{\n\tnodesep = 0.5;\n\tranksep = 0.35;\n\tnode [shape=plaintext];\n";
val nodes = "\t0 [texlbl=\"\\underline{0. $root$ }\"];\n" ^ getString(make_tree(del, 0));
val subgraph_dir = "subgraph dir\n{\n" ^ getSubgraph_directory(!dir) ^ "}\n";
val ancestor = "subgraph ancestor {\n\tedge [dir=back, color=blue, style=dashed]\n" ^ getSubgraph_directory(!blueDir) ^ "}\n";
val undir = "";
val end_message = "}";
val content = start_message ^ nodes ^ subgraph_dir ^ ancestor ^ undir ^ end_message;
printFile(content, "sample.dot");


fun mktableau(predList, pred) =
let 
  val del = predList@[NOT(pred)]
  val filename = "mygraph.dot"
  in 
  count := 1;
  dir := [];
  blueDir := [];
  let val start_message = "digraph{\n\tnodesep = 0.5;\n\tranksep = 0.35;\n\tnode [shape=plaintext];\n"
  val nodes = "\t0 [texlbl=\"\\underline{0. $root$ }\"];\n" ^ getString(make_tree(del, 0))
  val subgraph_dir = "subgraph dir\n{\n" ^ getSubgraph_directory(!dir) ^ "}\n"
  val ancestor = "subgraph ancestor {\n\tedge [dir=back, color=blue, style=dashed]\n" ^ getSubgraph_directory(!blueDir) ^ "}\n"
  val undir = ""
  val end_message = "}"
  in 
  printFile(start_message ^ nodes ^ subgraph_dir ^ ancestor ^ undir ^ end_message, "sample.dot");
  ()
  end
  end
