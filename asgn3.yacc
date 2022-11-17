(* User declarations *)

%%
(* required declarations *)
%name Asgn3

%term ID of string | TRUE | FALSE | NOT | AND | OR | XOR | EQUALS | IMPLIES | IF | THEN | ELSE | FI | LPAREN | RPAREN | EOF | TERM | PLUS | MINUS | TIMES | NEGATE | LESSTHAN | GREATERTHAN | NUM of int | LET | IN | END | ASSIGN | FN | FUN | ARROW | DARROW | COLON | INT | BOOL

%nonterm program of AST.exp | statement of AST.exp | formula of AST.exp | types of AST.types | decl of AST.decl

%pos int * int


(* Optional declarations *)
%eop EOF
%noshift EOF

(* header *)

%right ARROW
%right DARROW
%right LET IN END
%right IF THEN ELSE FI
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT
%nonassoc LESSTHAN GREATERTHAN
%left PLUS MINUS
%left TIMES
%right NEGATE


%start program

%verbose

%%
program: formula statement (AST.MultExp(formula, statement))

statement: TERM formula statement (AST.MultExp(formula, statement)) 
		  | (AST.NoneExp)

decl: ID ASSIGN formula (AST.ValDecl(ID, formula))

formula: TRUE (AST.BoolExp(true))
		| FALSE (AST.BoolExp(false)) 
		| NOT formula (AST.UnExp(AST.Not, formula1)) 
		| formula AND formula (AST.BinExp(AST.And, formula1, formula2)) 
		| formula OR formula (AST.BinExp(AST.Or, formula1, formula2)) 
		| formula XOR formula (AST.BinExp(AST.Xor, formula1, formula2)) 
		| formula EQUALS formula (AST.BinExp(AST.Eq, formula1, formula2)) 
		| formula IMPLIES formula (AST.BinExp(AST.Implies, formula1, formula2)) 
		| IF formula THEN formula ELSE formula FI (AST.ITE(formula1, formula2, formula3)) 
		| LPAREN formula RPAREN (formula1) 
		| ID (AST.VarExp(ID)) 
		| NUM (AST.NumExp(NUM)) 
		| LET decl IN formula END (AST.LetExp(decl, formula1)) 
		| formula PLUS formula (AST.BinExp(AST.Add, formula1, formula2)) 
		| formula MINUS formula (AST.BinExp(AST.Sub, formula1, formula2)) 
		| formula TIMES formula (AST.BinExp(AST.Mul, formula1, formula2)) 
		| NEGATE formula (AST.UnExp(AST.Negate, formula1)) 
		| formula LESSTHAN formula (AST.BinExp(AST.LessThan, formula1, formula2)) 
		| formula GREATERTHAN formula (AST.BinExp(AST.GreaterThan, formula1, formula2)) 
		| LPAREN formula formula RPAREN (AST.AppExp(formula1, formula2)) 
		| FN LPAREN ID COLON types RPAREN COLON types DARROW formula (AST.Fn(ID1, types1, types2, formula1))
		| FUN ID LPAREN ID COLON types RPAREN COLON types DARROW formula (AST.Fun(ID1, ID2, types1, types2, formula1))

types: INT (AST.Int) 
	  | BOOL (AST.Bool)
	  | types ARROW types (AST.Arrow(types1, types2))
	  | LPAREN types RPAREN (types1)

