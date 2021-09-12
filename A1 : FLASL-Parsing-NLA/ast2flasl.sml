use "datatypes.sml";

val list_arg = CommandLine.arguments();
val v = hd(list_arg);
val v2 = hd (tl(list_arg));
use v;

open AST;

fun PropToFlasl (ATOM(str))			= ("\"" ^ str ^ "\"")
	|	PropToFlasl (NOT(p))			= ("NOT (" ^ PropToFlasl(p) ^ ")")
	|	PropToFlasl (AND(p1, p2))			= ("(" ^ PropToFlasl(p1) ^ ") AND (" ^ PropToFlasl(p2) ^ ")")
	|	PropToFlasl (OR(p1, p2))			= ("(" ^PropToFlasl(p1) ^ ") OR (" ^ PropToFlasl(p2) ^ ")")
	|	PropToFlasl (COND(p1, p2))			= ("IF (" ^ PropToFlasl(p1) ^ ") THEN (" ^ PropToFlasl(p2) ^ ")")
	|	PropToFlasl (BIC(p1, p2))			= ("(" ^ PropToFlasl(p1) ^ ") IFF (" ^ PropToFlasl(p2) ^ ")")
	|	PropToFlasl (ITE(p1, p2, p3))			= ("IF (" ^ PropToFlasl(p1) ^ ") THEN (" ^ PropToFlasl(p2) ^ ") ELSE (" ^ PropToFlasl(p3) ^ ")")

	fun PropListToFlasl (x :: xs)			= (PropToFlasl(x) ^ ". " ^ PropListToFlasl(xs))
	|	PropListToFlasl ([])			= ("")

	fun ast2flasl (HENCE(x, y))			= (PropListToFlasl(x) ^ "THEREFORE (" ^ PropToFlasl(y)^ ").")


fun printFile (str:string, v2) =
			let val f = TextIO.openOut v2
			in 	(TextIO.output (f, str); TextIO.closeOut f) 
			end

val flaslOutput = ast2flasl ast;
val a = printFile(flaslOutput, v2);
OS.Process.exit(OS.Process.success);