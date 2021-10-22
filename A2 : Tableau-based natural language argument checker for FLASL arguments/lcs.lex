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

"."{ws}* 	=> (Tokens.DOTV(!pos,!pos));
{ws}*"NOT"{ws}*		=> (Tokens.NOTV(!pos,!pos));
{ws}*"AND"{ws}*		=> (Tokens.ANDV(!pos,!pos));
{ws}*"OR"{ws}*		=> (Tokens.ORV(!pos,!pos));
{ws}*"IF"{ws}*		=> (Tokens.IFV(!pos,!pos));
{ws}*"THEN"{ws}*		=> (Tokens.THENV(!pos,!pos));
{ws}*"IFF"{ws}*		=> (Tokens.IFFV(!pos,!pos));
{ws}*"ELSE"{ws}*		=> (Tokens.ELSEV(!pos,!pos));
{ws}*"THEREFORE"{ws}*	=> (Tokens.THEREFOREV(!pos,!pos));
"\""[^\"]*"\""	=> (Tokens.ATOMV(yytext, !pos, !pos));
"("				=> (Tokens.LEFTP(!pos, !pos));
")"				=> (Tokens.RIGHTP(!pos, !pos));