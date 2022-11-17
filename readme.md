1. Run the following commands:\
	ml-lex asgn3.lex\
	ml-yacc asgn3.yacc\
	sml  (to open sml interactive mode)\
	use "loader-asgn3.sml";\
	open EVALUATOR;\
	val a = parseFile "filename.txt";\
	getType(a, []); 			(This command is used to type-check the expression. If type checking goes through without any hiccup, then 								call the next command for evaluation)\
	evalExp(a, [], []);\
