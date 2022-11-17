structure EVALUATOR  =
struct
open AST

val brokenTypes = Fail "Error in evaluation!"

fun sameType2(t1:types, t2:types) =
  case (t1, t2) of
  (Int, Int) => true
  | (Bool, Bool) => true
  | (Arrow(a1, a2), Arrow(a3, a4)) => (sameType2(a1, a3) andalso sameType2(a2, a4))
  | _ => false

fun typeToString(t:types) =
  case t of
  Int => "Int"
  | Bool => "Bool"
  | Arrow(t1, t2) => "Arrow(" ^ typeToString(t1) ^ ", " ^ typeToString(t2) ^ ")"

fun output(e:exp) =
  case e of
  NumExp n => print("NumExp: " ^ Int.toString(n) ^ "\n")
  | BoolExp b => if b then print("BoolExp: true\n") else print("BoolExp: false\n")
  | Fun (id1, id2, t1, t2, e1) => print("val " ^ id1 ^ " = fn : " ^ typeToString(Arrow(t1, t2)) ^ "\n")
  | Fn (id1, t1, t2, e1) => print("val fn : " ^ typeToString(Arrow(t1, t2)) ^ "\n")
  | _ => print("Not Int or Bool")

fun binopToString(b:binop) =
  case b of
  Add => "Add"
  | Sub => "Sub" 
  | Mul => "Mul"
  | Eq => "Eq"
  | And => "And"
  | Or => "Or"
  | Xor => "Xor"
  | Implies => "Implies"
  | LessThan => "LessThan"
  | GreaterThan => "GreaterThan"

fun unopToString(u:unop) =
  case u of
  Not => "Not"
  | Negate => "Negate"

fun getType(e:exp, typeEnvs:typEnv):types =
  case e of
    NumExp i => Int
    | BoolExp b => Bool
    | VarExp v => typLookup(v, typeEnvs)
    | MultExp(e1, e2) => (getMultType(e1, e2, typeEnvs))
    | BinExp(b, e1, e2) => getBinType(b, e1, e2, typeEnvs)
    | LetExp(ValDecl(x, e1), e2) =>
    let
      val v1 = getType(e1, typeEnvs)
    in
      getType(e2, typAdd(x, v1, typeEnvs))
    end
    | UnExp(u, e1) => getUnType(u, e1, typeEnvs)
    | ITE(e1, e2, e3) => getITEType(e1, e2, e3, typeEnvs)
    | Fun(id1, id2, t1, t2, e1) =>
    let
      val a = getType(e1, typAdd(id2, t1, typAdd(id1, Arrow(t1, t2), typeEnvs)))
    in
      if sameType2(a, t2) then Arrow(t1, t2) else raise Fail ("Function " ^ id1 ^ " expression type does not match with return type, Expected type: " ^ typeToString(t2) ^ ", Expression type: " ^ typeToString(a))
    end
    | Fn(id1, t1, t2, e1) =>
    let
      val a = getType(e1, typAdd(id1, t1, typeEnvs))
    in
      if sameType2(a, t2) then Arrow(t1, t2) else raise Fail ("Function expression type does not match with return type, Expected type: " ^ typeToString(t2) ^ ", Expression type: " ^ typeToString(a))
    end
    | AppExp(e1, e2) => 
      let
        val t1 = getType(e1, typeEnvs)
      in
        case t1 of
          Arrow(v1, v2) => if sameType2(getType(e2, typeEnvs), v1) then v2 else raise Fail ("Argument type does not match with function argument type, Expected argument type: " ^ typeToString(v1) ^ ", Expression type: " ^ typeToString(getType(e2, typeEnvs)))
          | _ => raise Fail ("First expression is not of function type, Expression type: " ^ typeToString(t1))
      end
    | NoneExp => Int

and getMultType(e1:exp, e2:exp, typeEnvs:typEnv):types =
  case (e1, e2) of
  (Fun(id1, id2, t1, t2, e3), _) => getType(e2, typAdd(id1, getType(e1, typeEnvs), typeEnvs))
  | (_, NoneExp) => getType(e1, typeEnvs)
  | _ => (getType(e1, typeEnvs); getType(e2, typeEnvs))

and getBinType(b:binop, e1:exp, e2:exp, typeEnvs:typEnv):types =
  case (b, getType(e1, typeEnvs), getType(e2, typeEnvs)) of
      (Add, Int, Int) => Int
  |   (Sub, Int, Int) => Int
  |   (Mul, Int, Int) => Int
  |   (Eq, Int, Int)  => Bool
  |   (Eq, Bool, Bool) => Bool
  |   (And, Bool, Bool) => Bool
  |   (Or, Bool, Bool) => Bool
  |   (Xor, Bool, Bool) => Bool
  |   (Implies, Bool, Bool) => Bool
  |   (LessThan, Int, Int) => Bool
  |   (GreaterThan, Int, Int) => Bool
  |   _  => raise Fail ("Invalid argument types for Binary Operator " ^ binopToString(b))

and getUnType(u:unop, e:exp, typeEnvs:typEnv):types =
  case (u, getType(e, typeEnvs)) of
        (Not, Bool) => Bool
        | (Negate, Int) => Int
        | _ => raise Fail ("Invalid argument type for Unary Operator " ^ unopToString(u))

and getITEType(e1:exp, e2:exp, e3:exp, typeEnvs:typEnv):types =
  case (getType(e1, typeEnvs), getType(e2, typeEnvs), getType(e3, typeEnvs)) of
          (Bool, Int, Int) => Int
        | (Bool, Bool, Bool) => Bool
        | _ => raise Fail "ITE expression is not well typed"


fun evalImplies (b1:exp, b2:exp):exp =
  case (b1, b2) of
          (BoolExp (false), _) => BoolExp (true)
          | (BoolExp (true), BoolExp (true)) => BoolExp (true)
          | (BoolExp (true), BoolExp (false)) => BoolExp (false)
          | _ => raise brokenTypes

fun evalExp(e:exp, env:environment, typeEnvs:typEnv):exp =
    case e of
	      NumExp i            => NumExp i
      | BoolExp b           => BoolExp b
      | VarExp x            => envLookup (x, env)
      | MultExp (e1, e2)    => evalMultExp(e1, e2, env, typeEnvs)			  
      | BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env, typeEnvs)
      | LetExp(ValDecl(x, e1), e2)  =>
  	  let
	       val v1 = evalExp (e1, env, typeEnvs)
	    in
	       evalExp(e2, envAdd (x, v1, env), typAdd(x, getType(e1, typeEnvs), typeEnvs))
      end
      | UnExp (u, e1)       => evalUnExp(u, e1, env, typeEnvs)
      | ITE (e1, e2, e3)    => evalIteExp(e1, e2, e3, env, typeEnvs)
      | Fun (id1, id2, t1, t2, e1) => Fn(id2, t1, t2, evalExp(e1, env, typeEnvs))
      | Fn (id1, t1, t2, e1) => Fn(id1, t1, t2, evalExp(e1, env, typeEnvs))
      | AppExp (e1, e2)     => evalApply(e1, e2, env, typeEnvs)
      | NoneExp             => NumExp 0

and

evalBinExp(b:binop, e1:exp, e2:exp, env:environment, typeEnvs:typEnv):exp =
case (b, evalExp(e1, env, typeEnvs), evalExp(e2, env, typeEnvs)) of
      (Add, NumExp i1, NumExp i2) => NumExp(i1+i2)
  |   (Sub, NumExp i1, NumExp i2) => NumExp(i1-i2)
  |   (Mul, NumExp i1, NumExp i2) => NumExp(i1*i2)
  |   (Eq, NumExp i1, NumExp i2)  => BoolExp(i1 = i2)
  |   (Eq, BoolExp b1, BoolExp b2) => BoolExp(b1 = b2)
  |   (And, BoolExp b1, BoolExp b2) => BoolExp(b1 andalso b2)
  |   (Or, BoolExp b1, BoolExp b2) => BoolExp(b1 orelse b2)
  |   (Xor, BoolExp b1, BoolExp b2) => if (b1 = b2) then BoolExp (false) else BoolExp (true)
  |   (Implies, BoolExp b1, BoolExp b2) => evalImplies(BoolExp b1, BoolExp b2)
  |   (LessThan, NumExp i1, NumExp i2) => BoolExp(i1 < i2)
  |   (GreaterThan, NumExp i1, NumExp i2) => BoolExp(i1 > i2)
  |   _  => BinExp(b, evalExp(e1, env, typeEnvs), evalExp(e2, env, typeEnvs))		

and

evalUnExp(u:unop, e1:exp, env:environment, typeEnvs:typEnv):exp	= 
  case (u, evalExp(e1, env, typeEnvs)) of
        (Not, BoolExp b1) => BoolExp(not b1)
        | (Negate, NumExp i1) => NumExp (i1*(~1))
        | _ => UnExp(u, evalExp(e1, env, typeEnvs))

and

evalIteExp(e1:exp, e2: exp, e3: exp, env: environment, typeEnvs:typEnv):exp =
  case evalExp(e1, env, typeEnvs) of
  BoolExp b1 => if b1 then evalExp(e2, env, typeEnvs) else evalExp(e3, env, typeEnvs)
  | _ => ITE(evalExp(e1, env, typeEnvs), evalExp(e2, env, typeEnvs), evalExp(e3, env, typeEnvs))

and

evalMultExp(e1:exp, e2:exp, env:environment, typeEnvs:typEnv):exp =
  case (e1, e2) of
  (Fun(id1, id2, t1, t2, e3), _) => (output(e1); evalExp(e2, envAdd(id1, Fn(id2, t1, t2, e3), env), typAdd(id1, getType(e1, typeEnvs), typeEnvs)))
  | (_, NoneExp) => evalExp(e1, env, typeEnvs)
  | _ => (output(evalExp(e1, env, typeEnvs)); evalExp(e2, env, typeEnvs))

and

evalApply(x:exp, e1:exp, env:environment, typeEnvs:typEnv):exp =
      let
        val t1 = getType(x, typeEnvs)
      in
        case t1 of
          Arrow(v1, v2) => if sameType2(getType(e1, typeEnvs), v1) then
            let
              val Fn(e, f, g, h) = evalExp(x, env, typeEnvs)
              val a2 = evalExp(e1, env, typeEnvs)
            in
              evalExp(h, envAdd(e, a2, env), typAdd(e, getType(e1, typeEnvs), typeEnvs))
            end
            else raise Fail "Argument type does not match function argument type"
          | _ => raise Fail "First expression is not of function type"
      end

end
