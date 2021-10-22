Control.Print.printDepth := 1000;

CM.make "lcs.cm";
open AST;

val list_arg = CommandLine.arguments();

fun getList(arg) = case arg of
		HENCE(propList, prop) => NOT(prop):: propList

fun checkInDelNOT(str, del) =
	case del of 
		NOT(ATOM(str2))::xs => (
			if (str = str2) then true
			else checkInDelNOT(str, xs)
		)
		| ATOM(sdf)::xs => checkInDelNOT(str, xs)
		| _ => false

fun checkInDel(str, del) =
	case del of 
		ATOM(str2)::xs => (
			if (str = str2) then true
			else checkInDel(str, xs)
		)
		| NOT(ATOM(sdf))::xs => checkInDel(str, xs)
		| _ => false

fun getOutputFile(del) =
	case del of 
		ATOM(str)::xs => str^" : True \n"^getOutputFile(xs)
	| NOT(ATOM(str))::xs => str^" : False \n"^getOutputFile(xs)
	| _ => ""

fun printFile(content, outname) =
	let val f = TextIO.openOut (outname^".out") in
	(
		TextIO.output (f, content); 
		TextIO.closeOut f) 
	end

fun getRet(l, del, outname) =
	case l of 
		x::xs => (
			case x of 
			ATOM(str) => if(not(checkInDelNOT(str, del))) then 
							(
							if(checkInDel(str, del)) then getRet(xs, del, outname)
								else getRet(xs, x::del, outname)
							)
						else (TextIO.output(TextIO.stdOut,"*** THIS PATH IS CLOSED ***\n");
						true)
			| NOT(ATOM(str)) => 
							if (not(checkInDel(str, del))) then
							(
								if(checkInDelNOT(str, del)) then getRet(xs, del, outname)
								else getRet(xs, x::del, outname)
							)
								else (TextIO.output(TextIO.stdOut,"*** THIS PATH IS CLOSED not atom ***\n");
								true)
			| NOT(NOT(prop)) => getRet(prop::xs, del, outname)
			| AND(prop1, prop2) => getRet(prop1::prop2::xs, del, outname)
			| NOT(AND(prop1, prop2)) => if((getRet(NOT(prop1)::xs, del, outname))) then
											getRet(NOT(prop2)::xs, del, outname)
										else false
			| OR(prop1, prop2) => if((getRet(prop1::xs, del, outname))) then
									getRet(prop2::xs, del, outname)
								else false
			| NOT(OR(prop1, prop2)) => getRet(NOT(prop1)::NOT(prop2)::xs, del, outname)
			| COND(prop1, prop2) => if((getRet(NOT(prop1)::xs, del, outname))) then
										getRet(prop2::xs, del, outname)
									else false
			| NOT(COND(prop1, prop2)) => getRet(prop1::NOT(prop2)::xs, del, outname)
			| BIC(prop1, prop2) => if((getRet(AND(prop1, prop2)::xs, del, outname))) then
										getRet(AND(NOT(prop1), NOT(prop2))::xs, del, outname)
									else false
			| NOT(BIC(prop1, prop2)) => if((getRet(AND(prop1, NOT(prop2))::xs, del, outname))) then
										getRet(AND(NOT(prop1), prop2)::xs, del, outname)
									else false
			| ITE(prop1, prop2, prop3) => getRet(COND(prop1, prop2)::COND(NOT(prop1), prop3)::xs, del, outname)
			| NOT(ITE(prop1, prop2, prop3)) => getRet(OR(NOT(COND(prop1, prop2)), NOT(COND(NOT(prop1), prop3)))::xs, del, outname)
		)
		| _ => (
				printFile("Not a valid argument, A falsifying assignment is given below.\n\n"^getOutputFile(del), outname);
				false)

fun getTableau(arg, outname) = 
	let val l = getList(arg)
		val del = [] in
	if(getRet(l, del, outname)) then (
		TextIO.output(TextIO.stdOut,"*** IT IS A VALID PROPOSITIONAL ARGUMENT ***\n");
		printFile("*** IT IS A VALID PROPOSITIONAL ARGUMENT ***\n", outname);
		true)
	else (TextIO.output(TextIO.stdOut,"*** IT IS NOT A VALID PROPOSITIONAL ARGUMENT (ATLEAST ONE PATH IS OPEN)***\n");
		false)
	end

fun tableAUValid (infile:string) = 
		let val arg = Lcs.parser infile
		val outname = substring(infile, 0, size infile -6) in
		getTableau(arg, outname)
	end

val out = tableAUValid(hd (list_arg));
OS.Process.exit(OS.Process.success);