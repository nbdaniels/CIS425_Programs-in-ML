fun mult(a,b) =
	let fun mult1(a,b,result) = 
		if a=0 then 0
		else if a = 1 then b + result
		else mult1(a-1,b,result+b)
	in
		mult1(a,b,0)
	end;
	
fun myMult(a,b) = 
	let val result = ref 0
		val x = ref a
	in
		while (!x > 1) do
		(result := !result + b;
		 x := !x - 1);
		if !x = 0 then 0
		else !result + b
	end;

val x = ref 0;
fun p(y' : int ref) =
	y' := ref 1;
	x := 0;
	x := y';
p(x);
