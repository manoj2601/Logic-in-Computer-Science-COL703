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
	
	val ArgToFlasl : Argument -> string
	val PropListToFlasl : Prop list -> string
	val PropToFlasl : Prop -> string
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

	fun PropToFlasl (ATOM(str))			= (str)
	|	PropToFlasl (NOT(p))			= ("NOT (" ^ PropToFlasl(p) ^ ")")
	|	PropToFlasl (AND(p1, p2))			= ("(" ^ PropToFlasl(p1) ^ ") AND (" ^ PropToFlasl(p2) ^ ")")
	|	PropToFlasl (OR(p1, p2))			= ("(" ^PropToFlasl(p1) ^ ") OR (" ^ PropToFlasl(p2) ^ ")")
	|	PropToFlasl (COND(p1, p2))			= ("IF (" ^ PropToFlasl(p1) ^ ") THEN (" ^ PropToFlasl(p2) ^ ")")
	|	PropToFlasl (BIC(p1, p2))			= ("(" ^ PropToFlasl(p1) ^ ") IFF (" ^ PropToFlasl(p2) ^ ")")
	|	PropToFlasl (ITE(p1, p2, p3))			= ("IF (" ^ PropToFlasl(p1) ^ ") THEN (" ^ PropToFlasl(p2) ^ ") ELSE (" ^ PropToFlasl(p3) ^ ")")

	fun PropListToFlasl (x :: xs)			= (PropToFlasl(x) ^ ". " ^ PropListToFlasl(xs))
	|	PropListToFlasl ([])			= ("")

	fun ArgToFlasl (HENCE(x, y))			= (PropListToFlasl(x) ^ "THEREFORE (" ^ PropToFlasl(y)^ ").")


	val ArgToFlasl = ArgToFlasl;
	val PropListToFlasl = PropListToFlasl;
	val PropToFlasl = PropToFlasl;
	
end;