signature Lcs_TOKENS =
sig
type ('a,'b) token
type svalue
val RIGHTP:  'a * 'a -> (svalue,'a) token
val LEFTP:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val DOTV:  'a * 'a -> (svalue,'a) token
val COMMAV:  'a * 'a -> (svalue,'a) token
val THEREFOREV:  'a * 'a -> (svalue,'a) token
val ELSEV:  'a * 'a -> (svalue,'a) token
val IFFV:  'a * 'a -> (svalue,'a) token
val THENV:  'a * 'a -> (svalue,'a) token
val IFV:  'a * 'a -> (svalue,'a) token
val ORV:  'a * 'a -> (svalue,'a) token
val ANDV:  'a * 'a -> (svalue,'a) token
val SPACEV:  'a * 'a -> (svalue,'a) token
val NOTV:  'a * 'a -> (svalue,'a) token
val ATOMV: (string) *  'a * 'a -> (svalue,'a) token
end
signature Lcs_LRVALS=
sig
structure Tokens : Lcs_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
