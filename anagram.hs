dictfile = "swedish.dict"

anagram _ "" = [""]
anagram d x = 
    concatMap (\w -> maybe [] (map (join w) . anagram d) (x `minus` w)) d

join xs "" = xs
join xs ys = xs ++ " " ++ ys

minus xs [] = Just xs
minus xs (y:ys) = xs `remove` y >>= flip minus ys

remove [] y = Nothing
remove (x:xs) y | x == y = Just xs
		| otherwise = fmap (x:) (remove xs y)

readdict f = fmap lines (readFile f)

test x = do
	 d <- readdict dictfile
	 mapM_ putStrLn (anagram d x)