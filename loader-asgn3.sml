CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "ast_trial.sml";
use "evaluator_trial.sml";
use "asgn3.yacc.sig";
use "asgn3.yacc.sml";
use "asgn3.lex.sml";
use "load-asgn3.sml";
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)
