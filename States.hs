data State a = State (Int -> (a, Int))

instance Monad State where
	return x = State $ \s -> (x, s)
	(State f) >>= k = State $ \s ->
		let
			(x, s') = f s
			State f' = k x
		in f' s'

get :: State Int
get = State $ \s -> (s, s)

put :: Int -> State ()
put s = State $ \_ -> ((), s)

increment :: State()
increment = do
	x <- get
	put (x+1)
	
run :: State() -> Int
run (State f) = do
	f
	let x = get
	x