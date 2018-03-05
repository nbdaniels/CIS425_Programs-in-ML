use "parser.sml";

(* Here is a result datatype *)
datatype result =
    RES_ERROR of string
  | RES_NUM   of int
  | RES_BOOL  of bool
  | RES_SUCC
  | RES_PRED
  | RES_ISZERO
  | RES_FUN   of (string * term);

(* Here is a basic environment implementation *)
exception not_found;
datatype env = Env of (string * result) list

fun new_env() = Env(nil);
fun extend_env (Env(oldenv), id, value) = Env( (id, value):: oldenv);
fun extend_env_all (Env(oldenv), id_value_list) = Env(id_value_list @ oldenv);
fun lookup_env (Env(nil), id) = (print("Free Var!! "^id); raise not_found)
   |lookup_env (Env((id1,value1)::b), id) =  
        if (id1 = id) 
        then value1
	    else lookup_env(Env(b), id) ;

fun interp (exp, env) = 

  case exp of
    AST_ERROR s                 => RES_ERROR s
  | AST_NUM  x                  => RES_NUM x
  | AST_BOOL b                  => if b then RES_BOOL true
  									else RES_BOOL false
  | AST_SUCC                    => RES_SUCC
  | AST_PRED                    => RES_PRED
  | AST_ISZERO                  => RES_ISZERO
  | AST_IF (exp1, exp2, exp3)   => if (interp(exp1, env) = RES_BOOL true)
  									then interp(exp2, env)
  									else interp(exp3, env)
  | AST_APP (exp1, exp2)        => let val app1 = interp(exp1, env)
  									   val app2 = interp(exp2, env)
  								   in
  								   	   case app1 of
  								   	   	  RES_ISZERO => let val num = app2
  								   	   					in
  								   	   					case num of
  								   	   					  RES_NUM 0 => RES_BOOL true
  								   	   					| RES_NUM x => RES_BOOL false
  								   	   					| _			=> RES_ERROR "Cannot apply iszero to second term"
  								   	   					end
  								   		| RES_SUCC	 => let val num = app2
  														in
  														case num of
  														  RES_NUM x	=> RES_NUM (x + 1)
  														| _			=> RES_ERROR "Cannot apply succ to second term"
  														end
  					   					| RES_PRED	 => let val num = app2
  									   					in
  									   					case num of
  														  RES_NUM x	=> RES_NUM (x - 1)
  														| _			=> RES_ERROR "Cannot apply pred to second term"
  														end
  										| RES_FUN(var, exp)	 => interp (exp, extend_env (env, var, app2))
  										| _			 => RES_ERROR "Application Error"
  								   end
  | AST_ID name                 => lookup_env (env, name)
  | AST_FUN  (var, exp)         => RES_FUN(var, exp)
