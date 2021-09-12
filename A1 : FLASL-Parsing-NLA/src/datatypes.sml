(* datatypes.sml *)
(* As specified in the statement *)
signature AST = sig
	datatype Prop = 
				ATOM of string
				|	NOT of Prop
				|	AND of Prop * Prop
				|	OR of Prop * Prop
				|	COND of Prop * Prop
				|	BIC of Prop * Prop
				|	ITE of Prop * Prop * Prop
	
	datatype Argument = HENCE of Prop list * Prop
	val ArgToString : Argument -> string
	val PropListToString : Prop list -> string
	val PropToString : Prop -> string
end

structure AST : AST =
	struct
		datatype Prop = 
				ATOM of string
				|	NOT of Prop
				|	AND of Prop * Prop
				|	OR of Prop * Prop
				|	COND of Prop * Prop
				|	BIC of Prop * Prop
				|	ITE of Prop * Prop * Prop
	
	datatype Argument = HENCE of Prop list * Prop

	fun PropToString (ATOM(str))			= (str)
	|	PropToString (NOT(p))			= ("NOT " ^ PropToString(p))
	|	PropToString (AND(p1, p2))			= (PropToString(p1) ^ " AND " ^ PropToString(p2))
	|	PropToString (OR(p1, p2))			= (PropToString(p1) ^ " OR " ^ PropToString(p2))
	|	PropToString (COND(p1, p2))			= ("IF " ^ PropToString(p1) ^ " THEN " ^ PropToString(p2))
	|	PropToString (BIC(p1, p2))			= (PropToString(p1) ^ " IFF " ^ PropToString(p2))
	|	PropToString (ITE(p1, p2, p3))			= ("IF " ^ PropToString(p1) ^ " THEN " ^ PropToString(p2) ^ " ELSE " ^ PropToString(p3))

	fun PropListToString (x :: xs)			= (PropToString(x) ^ ". " ^ PropListToString(xs))
	|	PropListToString ([])			= ("")

	fun ArgToString (HENCE(x, y))			= (PropListToString(x) ^ "THEREFORE " ^ PropToString(y))


	val ArgToString = ArgToString;
	val PropListToString = PropListToString;
	val PropToString = PropToString;
	
end;