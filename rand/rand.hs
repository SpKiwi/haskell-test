import System.Random
import Control.Monad

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


finiteRandom :: (RandomGen g, Random a, Num n, Eq n) => g -> n -> ([a], g)
finiteRandom gen 0 = ([], gen) 
finiteRandom gen n =
	let 
		(value, newGen) = random gen
		(values, newerGen) = finiteRandom newGen (n - 1)
	in
		(value:values, newerGen)

-- main = do
-- 	gen <- getStdGen
-- 	let x = 1 :: Int
-- 	let y = 2 :: Int
-- 	putStrLn $ show $ take 10 $ randomRs (x,y) $ gen

-- main = do
-- 	gen <- newStdGen
-- 	let (x, newGen) = random $ gen :: (Int, StdGen)
-- 	putStrLn $ show x

main = do  
	gen <- getStdGen
	askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
	let (randomNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
	putStrLn "Which number is correct?"
	input <- getLine
	let userNumber = read input :: Int
	let result = if (randomNumber == userNumber)
		then "You guessed, yey"
		else "The correct number was " ++ show randomNumber
	putStrLn result
	askForNumber newGen

-- Bytestrings










