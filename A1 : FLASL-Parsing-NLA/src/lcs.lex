(* Standard lex file format, again borrowed 
	from ml-yacc/examples/calc/calc.lex and 
	modified as per requirement*)

structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue, pos) token

val pos = ref 0
fun init () = ()
fun eof() = Tokens.EOF(!pos, !pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
	"line ", (Int.toString l), ": ", e, "\n"])

%%
%header (functor LcsLexFun(structure Tokens: Lcs_TOKENS));
ws  = [\ \t];



%%
\n 		=> (pos := (!pos) + 1; lex());
{ws}+	=> (lex());

"."{ws}* 	=> (TextIO.output(TextIO.stdOut, "\n\n\nDOT AAYA\n\n\n");Tokens.DOTV(!pos,!pos));
{ws}*"NOT"{ws}*		=> (Tokens.NOTV(!pos,!pos));
{ws}*"AND"{ws}*		=> (Tokens.ANDV(!pos,!pos));
{ws}*"OR"{ws}*		=> (Tokens.ORV(!pos,!pos));
{ws}*"IF"{ws}*		=> (TextIO.output(TextIO.stdOut, "\n\n\nManoj is boss\n\n\n");Tokens.IFV(!pos,!pos));
{ws}*"THEN"{ws}*		=> (TextIO.output(TextIO.stdOut, "\n\n\nManoj is boss2\n\n\n");Tokens.THENV(!pos,!pos));
{ws}*"IFF"{ws}*		=> (Tokens.IFFV(!pos,!pos));
{ws}*"ELSE"{ws}*		=> (Tokens.ELSEV(!pos,!pos));
{ws}*"THEREFORE"{ws}*	=> (TextIO.output(TextIO.stdOut, "\n\n\nTHEREFORE AAYA\n\n\n");Tokens.THEREFOREV(!pos,!pos));
"\""[^\"]*"\""	=> (TextIO.output(TextIO.stdOut, "\n\n\nManoj is boss3\n\n\n"^yytext^"\n");Tokens.ATOMV(yytext, !pos, !pos));
"("				=> (TextIO.output(TextIO.stdOut, "\n\n\nHurrey LEFTP\n\n\n");Tokens.LEFTP(!pos, !pos));
")"				=> (TextIO.output(TextIO.stdOut, "\n\n\nHurrey RIGHTP\n\n\n");Tokens.RIGHTP(!pos, !pos));