(* 教科書のparserの記述に基づいて変更 *)
type svalue = Tokens.svalue
  type pos = int
  type ('a, 'b) token = ('a, 'b) Tokens.token
  type lexresult = (svalue, pos) token

  val lineNum = ErrorMsg.lineNum
  val linePos = ErrorMsg.linePos
  val commentCounter = ref 0
  fun err(p1,p2) = ErrorMsg.error p1

  fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

  exception NonNum
  fun str2int str = case Int.fromString str of
  SOME i => i
  | NONE   => raise NonNum

  %%
  (* パーサの記述に基づいて追加 *)
  %header (functor TigerLexFun (structure Tokens : Tiger_TOKENS));

DIGITS = [0-9]+;
IDENTIFIER = [a-zA-Z] [\_a-zA-Z0-9]*;
WHITE = [\ \t]+;
LCOMMENT = \/\*;
RCOMMENT = \*\/;
%s COMMENT STR;

%%
{LCOMMENT}            => (commentCounter := !commentCounter + 1; YYBEGIN COMMENT; continue());
<COMMENT> {RCOMMENT}  => (commentCounter := !commentCounter - 1;if (!commentCounter <= 0) then (YYBEGIN INITIAL; continue()) else continue());
<COMMENT> .           => (continue());

<INITIAL>while        => (Tokens.WHILE (yypos, yypos + 5));
<INITIAL>for          => (Tokens.FOR (yypos, yypos + 3));
<INITIAL>to           => (Tokens.TO (yypos, yypos + 2));
<INITIAL>break        => (Tokens.BREAK (yypos, yypos + 5));
<INITIAL>let          => (Tokens.LET (yypos, yypos + 3));
<INITIAL>in           => (Tokens.IN (yypos, yypos + 2));
<INITIAL>end          => (Tokens.END (yypos, yypos + 3));
<INITIAL>function     => (Tokens.FUNCTION (yypos, yypos + 7));
<INITIAL>var          => (Tokens.VAR (yypos, yypos + 3));
<INITIAL>type         => (Tokens.TYPE (yypos, yypos + 4));
<INITIAL>array        => (Tokens.ARRAY (yypos, yypos + 5));
<INITIAL>if           => (Tokens.IF (yypos, yypos + 2));
<INITIAL>then         => (Tokens.THEN (yypos, yypos + 4));
<INITIAL>else         => (Tokens.ELSE (yypos, yypos + 4));
<INITIAL>do           => (Tokens.DO (yypos, yypos + 2));
<INITIAL>of           => (Tokens.OF (yypos, yypos + 2));
<INITIAL>nil          => (Tokens.NIL(yypos, yypos + 3));

<INITIAL>"+"          => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"-"          => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"*"          => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"/"          => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"," 	        => (Tokens.COMMA(yypos,yypos+1));
<INITIAL>"&"		      => (Tokens.AND(yypos,yypos+1));
<INITIAL>"|"          => (Tokens.OR(yypos,yypos+1));
<INITIAL>"="          => (Tokens.EQ(yypos,yypos+1));
<INITIAL>">="       	=> (Tokens.GE(yypos,yypos+2));
<INITIAL>">"		      => (Tokens.GT(yypos,yypos+1));
<INITIAL>"<="	        => (Tokens.LE(yypos,yypos+2));
<INITIAL>"<"      		=> (Tokens.LT(yypos,yypos+1));
<INITIAL>"<>"	        => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"."	      	=> (Tokens.DOT(yypos,yypos+1));
<INITIAL>":"          => (Tokens.COLON(yypos,yypos+1));
<INITIAL>";"          => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>":="	        => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>"{"          => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"          => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"("          => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"          => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"["          => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]"          => (Tokens.RBRACK(yypos,yypos+1));

\n	                  => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>{WHITE}      => (continue());

<INITIAL>{DIGITS}     => (Tokens.INT(str2int yytext, yypos, yypos + size yytext));
<INITIAL>{IDENTIFIER} => (Tokens.ID(yytext, yypos, yypos + size yytext));

<INITIAL>\"           => (YYBEGIN STR; continue());
<STR>\"               => (YYBEGIN INITIAL; continue());
<STR>[^\"]*           => (Tokens.STRING(yytext, yypos, yypos + size yytext));

.                     => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

