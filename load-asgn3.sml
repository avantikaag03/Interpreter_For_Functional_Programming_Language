exception SyntaxError;

structure Asgn3LrVals = Asgn3LrValsFun(structure Token = LrParser.Token)
structure Asgn3Lex = Asgn3LexFun(structure Tokens = Asgn3LrVals.Tokens);
structure Asgn3Parser =
      Join(structure LrParser = LrParser
               structure ParserData = Asgn3LrVals.ParserData
               structure Lex = Asgn3Lex)
     
fun invoke lexstream =
        let
            fun error(value, pos, _) =
                let
                    val (l, c) = pos
                in
                    TextIO.output(TextIO.stdOut,"Syntax Error:" ^ (Int.toString l) ^ ":" ^ (Int.toString c) ^ ":" ^ value ^ "\n")
                end
        in
            Asgn3Parser.parse(0, lexstream, error, ()) handle Asgn3Parser.ParseError => raise SyntaxError
        end

fun fileToLexer infilename =
    let val instream = TextIO.openIn infilename;
        val lexer = Asgn3Parser.makeLexer (fn _ => TextIO.input instream);
    in
        print("[");
        lexer
    end 
        
fun parse (lexer) =
    let
        val (result, lexer) = invoke lexer
        val (nextToken, lexer) = Asgn3Parser.Stream.get lexer
    in
        result
    end

val parseFile = parse o fileToLexer

