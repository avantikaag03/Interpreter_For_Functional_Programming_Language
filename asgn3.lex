structure Tokens = Tokens

	type pos = int * int
	type svalue = Tokens.svalue
	type ('a, 'b) token = ('a, 'b) Tokens.token
	type lexresult = (svalue, pos) token

	val pos = ref((1, 0))
	val eof = fn () => (print("]\n"); Tokens.EOF(!pos, !pos))
	val prev = ref(false)

	val error = fn (value, pos, _) =>
		let
			val (l, c) = pos
		in
	 		TextIO.output(TextIO.stdOut, "Unknown Token:" ^ (Int.toString l) ^ ":" ^ (Int.toString (c) ^ ":" ^ value ^ "\n"))
	 	end

	fun revfold _ nil b = b
    | revfold f (hd::tl) b = revfold f tl (f(hd,b))

	fun lineinc(pos, inc) =
		let
			val (l, r) = pos
		in
			(l+inc, 1)
		end

	fun colinc(pos, inc) =
		let
			val (l, r) = pos
		in
			(l, r+inc)
		end


%%
%header (functor Asgn3LexFun(structure Tokens:Asgn3_TOKENS));

alpha = [A-Za-z];
ws = [\ ];
tabs = [\t];
num = [0-9];
alphanum = [A-Za-z0-9];
%%

\n       => (pos := lineinc(!pos, 1); lex());

{ws}+    => (pos := colinc(!pos, 1); lex());

{tabs}+  => (pos := colinc(!pos, 4); lex());

"TRUE"   => (pos := colinc(!pos, 4); Tokens.TRUE(!pos, !pos));

"FALSE"	 => (pos := colinc(!pos, 5); Tokens.FALSE(!pos, !pos));

"NOT"	 => (pos := colinc(!pos, 3); Tokens.NOT(!pos, !pos));

"AND"	 => (pos := colinc(!pos, 3); Tokens.AND(!pos, !pos));

"OR"	 => (pos := colinc(!pos, 2); Tokens.OR(!pos, !pos));

"XOR"	 => (pos := colinc(!pos, 3); Tokens.XOR(!pos, !pos));

"EQUALS" => (pos := colinc(!pos, 6); Tokens.EQUALS(!pos, !pos));

"IMPLIES"=> (pos := colinc(!pos, 7); Tokens.IMPLIES(!pos, !pos));

"if"	 => (pos := colinc(!pos, 2); Tokens.IF(!pos, !pos));

"then"	 => (pos := colinc(!pos, 4); Tokens.THEN(!pos, !pos));

"else"	 => (pos := colinc(!pos, 4); Tokens.ELSE(!pos, !pos));

"fi"     => (pos := colinc(!pos, 2); Tokens.FI(!pos, !pos));

"let"    => (pos := colinc(!pos, 3); Tokens.LET(!pos, !pos));

"in"     => (pos := colinc(!pos, 2); Tokens.IN(!pos, !pos));

"end"    => (pos := colinc(!pos, 3); Tokens.END(!pos, !pos));

"("		 => (pos := colinc(!pos, 1); Tokens.LPAREN(!pos, !pos));

")"		 => (pos := colinc(!pos, 1); Tokens.RPAREN(!pos, !pos));

"PLUS"   => (pos := colinc(!pos, 4); Tokens.PLUS(!pos, !pos));

"MINUS"  => (pos := colinc(!pos, 5); Tokens.MINUS(!pos, !pos));

"TIMES"  => (pos := colinc(!pos, 5); Tokens.TIMES(!pos, !pos));

"LESSTHAN"    => (pos := colinc(!pos, 8); Tokens.LESSTHAN(!pos, !pos));

"GREATERTHAN" => (pos := colinc(!pos, 11); Tokens.GREATERTHAN(!pos, !pos));

"NEGATE" => (pos := colinc(!pos, 6); Tokens.NEGATE(!pos, !pos));

"fn"     => (pos := colinc(!pos, 2); Tokens.FN(!pos, !pos));

"fun"    => (pos := colinc(!pos, 3); Tokens.FUN(!pos, !pos));

"int"    => (pos := colinc(!pos, 3); Tokens.INT(!pos, !pos));

"bool"   => (pos := colinc(!pos, 4); Tokens.BOOL(!pos, !pos));

"->"     => (pos := colinc(!pos, 2); Tokens.ARROW(!pos, !pos));

"=>"     => (pos := colinc(!pos, 2); Tokens.DARROW(!pos, !pos));

"="      => (pos := colinc(!pos, 1); Tokens.ASSIGN(!pos, !pos));

":"      => (pos := colinc(!pos, 1); Tokens.COLON(!pos, !pos));

";"		 => (pos := colinc(!pos, 1); Tokens.TERM(!pos, !pos));

{alpha}+ => (pos := colinc(!pos, size(yytext)); Tokens.ID(yytext, !pos, !pos));

{num}+   => (pos := colinc(!pos, size(yytext)); Tokens.NUM(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !pos, !pos));

{alpha}{alphanum}+ => (pos := colinc(!pos, size(yytext)); Tokens.ID(yytext, !pos, !pos));

.        => (pos := colinc(!pos, 1); error (yytext, !pos, !pos); lex());