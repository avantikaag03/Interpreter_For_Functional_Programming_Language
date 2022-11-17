structure AST =
struct

type id = string

datatype binop = Add | Sub | Mul | Eq | And | Or | Xor | Implies | LessThan | GreaterThan

datatype unop = Not | Negate

datatype types = Int | Bool | Arrow of types * types

datatype decl = ValDecl of id * exp

and exp = NumExp of int
		| BoolExp of bool
    	| VarExp of id
    	| MultExp of exp * exp
		| BinExp of binop * exp * exp
		| LetExp of decl * exp
		| UnExp of unop * exp
        | ITE of exp * exp * exp
        | Fun of id * id * types * types * exp
        | Fn of id * types * types * exp
        | AppExp of exp * exp
        | NoneExp
				
type environment = (id * exp) list

type typEnv = (id * types) list

fun envAdd (var:id, e:exp, env:environment) =
    (var,e)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => VarExp var


fun envRem (nil, var:id) = nil
	| envRem (hd::tl, var:id) =
		let
			val (x, y) = hd
		in
			if x = var then (envRem(tl, var)) else hd::(envRem(tl, var))
		end


fun typAdd (var:id, v:types, typeEnvs:typEnv) =
    (var,v)::typeEnvs

fun typLookup (var:id, typeEnvs:typEnv) =
    case List.find(fn (x, _) => x = var) typeEnvs of
				       SOME (x, v)   => v
				    |   NONE => raise Fail ("Type not defined for " ^ var)

end