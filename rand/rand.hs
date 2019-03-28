import System.Random

threeToss :: StdGen -> (Bool, Bool, Bool)
threeToss gen = 
	let 
		(toss1, gen1) = random gen
		(toss2, gen2) = random gen1
		(toss3, _) = random gen2
	in 
		(toss1, toss2, toss3)

randomsM :: (RandomGen g, Random a) => g -> [a]
randomsM gen =
	let (v,genNew) = random gen
	in v: randomsM  genNew

-- finish this
finiteRandom :: (RandomGen g, Random a, Num n) => g -> n -> (g, [a])
finiteRandom gen 0 = (gen, []) 
finiteRandom gen n =
	let 