exception OddNum;
fun g y = let fun f(0, count) = count
	  		  | f(1, count) = raise OddNum
	  		  | f(x, count) = f((x-2), (count+1)) handle OddNum => (~1)
		  in
			  f(16,0)
		  end;

fun h y = let fun f(x, count) = if x = 0 then count
							  else if x = 1 then raise OddNum
							  else f(x-2, count+1) handle OddNum => ~1
		  in
		  	  f(16,0)
		  end;
