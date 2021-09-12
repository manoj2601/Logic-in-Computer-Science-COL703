Control.Print.printDepth := 1000;

CM.make "lcs.cm";
open AST;

val list_arg = CommandLine.arguments();

fun flasl2ast (infile:string, outfile:string) = 
	let fun printFile (str:string) =
			let val f = TextIO.openOut outfile
			in 	(TextIO.output (f, str); TextIO.closeOut f) 
			end
		val parsetrees = Lcs.parser infile
		val smlOutput = Lcs.smlOutput parsetrees
	in printFile (smlOutput)
	end

val out = flasl2ast(hd (list_arg), hd (tl list_arg));
OS.Process.exit(OS.Process.success);