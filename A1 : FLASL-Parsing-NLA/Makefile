flasl2ast:
	sml flasl2ast.sml arg-inp.flasl arg.sml

ast2flasl:
	sml ast2flasl.sml arg.sml arg-out.flasl

validation:
	sml flasl2ast.sml arg-out.flasl arg2.sml
	diff arg.sml arg2.sml

clean:
	rm -f lcs.grm.* lcs.lex.*

clean-output:
	rm -f arg.sml arg-out.flasl arg2.sml