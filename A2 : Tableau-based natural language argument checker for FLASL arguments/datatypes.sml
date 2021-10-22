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
	
	val ArgToSml : Argument -> string
	val PropListToSml : Prop list -> string
	val PropToSml : Prop -> string
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

	fun PropToSml (ATOM(str))			= ("AST.ATOM("^str^")")
	|	PropToSml (NOT(p))			= ("AST.NOT(" ^ PropToSml(p) ^ ")")
	|	PropToSml (AND(p1, p2))			= ("AST.AND ((" ^ PropToSml(p1) ^ "), (" ^ PropToSml(p2) ^ "))")
	|	PropToSml (OR(p1, p2))			= (" AST.OR ((" ^PropToSml(p1) ^ "), (" ^ PropToSml(p2) ^ "))")
	|	PropToSml (COND(p1, p2))			= ("AST.COND((" ^ PropToSml(p1) ^ "), (" ^ PropToSml(p2) ^ "))")
	|	PropToSml (BIC(p1, p2))			= ("AST.BIC((" ^ PropToSml(p1) ^ "), (" ^ PropToSml(p2) ^ "))")
	|	PropToSml (ITE(p1, p2, p3))			= ("AST.ITE ((" ^ PropToSml(p1) ^ "), (" ^ PropToSml(p2) ^ "), (" ^ PropToSml(p3) ^ "))")

	fun PropListToSml (x::[])			= (PropToSml(x))
	|	PropListToSml (x :: xs)			= (PropToSml(x) ^ ", " ^ PropListToSml(xs))	
	|	PropListToSml ([])			= ("")

	fun ArgToSml (HENCE(x, y))			= ("use \"datatypes.sml\";\n\n" ^ "val ast = AST.HENCE([" ^ PropListToSml(x) ^ "], " ^ PropToSml(y)^ ")")

	val ArgToSml = ArgToSml;
	val PropListToSml = PropListToSml;
	val PropToSml = PropToSml;

end;