(* Base format borrowed from http://cs.wellesley.edu/~cs235/fall08/lectures/35_YACC_revised.pdf 
	and modified as per requirement.
	A few more functions have been added to process multiple ASTs with error handling and
	processing the prefix and postfix to get the file output *)

structure Lcs  :
	sig
		val parser : string -> AST.Argument
		val smlOutput : AST.Argument -> string
	end
= struct 
	exception LcsError;
	structure LcsLrVals = LcsLrValsFun(
		structure Token = LrParser.Token)
	structure LcsLex = LcsLexFun(
		structure Tokens = LcsLrVals.Tokens);
	structure LcsParser = Join(
		structure LrParser = LrParser
		structure ParserData = LcsLrVals.ParserData
		structure Lex = LcsLex)

	val invoke = fn lexstream =>
		let val print_error = fn (str,pos,_) =>
			TextIO.output(TextIO.stdOut,
				"***Error Occured : Lcs Parser Error at " ^ (Int.toString pos) ^ "*** " ^ str ^ "\n")
		in 
		LcsParser.parse(0,lexstream,print_error,())
		end
		
	fun getLexer str = (* creates a lexer from a string *)
		let val strDone = ref false
		in 
		LcsParser.makeLexer (fn n => if (!strDone) then "" else (strDone := true; str))
		end

	fun getParserFromLexer (lexer) = (* creates a parser from a lexer *)
		let val EOFToken = LcsLrVals.Tokens.EOF(0,0)
			val (result,lexer) = invoke lexer
			val (nextToken,lexer) = LcsParser.Stream.get lexer
		in 
			if LcsParser.sameToken(nextToken,EOFToken) then
				result
			else (TextIO.output(TextIO.stdOut,"*** Warning: LCS PARSER WARNING -- unconsumed input ***\n");
				result)
		end

	fun parseString str = 
		let val a = getLexer str in
		getParserFromLexer a 
		end

	fun parser (filename : string) =
		let val ins = TextIO.openIn filename
			fun takeInput ins = 
							let 
								fun loop ins = String.implode(String.explode(TextIO.inputAll ins))
							in
								loop ins before TextIO.closeIn ins
			end in
			parseString (takeInput ins)
	end

	fun smlOutput (arg) = AST.ArgToSml(arg)

end (* struct *)