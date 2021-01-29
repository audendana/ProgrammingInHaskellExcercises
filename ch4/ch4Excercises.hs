--1.
halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
            where n = length xs `div` 2

halve2 :: [a] -> ([a], [a])
halve2 xs = splitAt ( length xs `div` 2) xs

--2.
third :: [a] -> a 
third xs = head ( tail ( tail xs ) )

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (_:_:x:_) = x

--3. 
safetail :: [a] -> [a]
safetail xs = if null xs then []
              else tail xs

safetailGuarded :: [a] -> [a]
safetailGuarded xs | null xs = []
                   | otherwise = tail xs 

safetailPatternMatching :: [a] -> [a]
safetailPatternMatching [] = []
safetailPatternMatching (_:xs) = xs

--4.
(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

--5.
(/\) :: Bool -> Bool -> Bool
a /\ b = if a then 
            if b then True else False
         else 
             False
--6.
(//) :: Bool -> Bool -> Bool
a // b = if a then b else False

--7.
multiplyLambda :: Int -> Int -> Int -> Int
multiplyLambda = \x -> (\y -> (\z -> x * y * z))

--8.
lunhDouble :: Int -> Int 
lunhDouble x | x*2 > 9 = (x*2) -9
             | otherwise = x * 2

--9.
lunh :: Int -> Int -> Int -> Int -> Bool
lunh a b c d = ((lunhDouble a) + b + (lunhDouble c) + d) `mod` 10 == 0

main :: IO ()
main = return()
