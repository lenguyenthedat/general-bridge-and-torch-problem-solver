 -- A torch problem solver
 -- Copyright (C) 2008-2009 Le Nguyen The Dat
 -- University of Nottingham 
 -- School of Computer Science      
 -- Contact email: thedat.lenguyen@gmail.com

module Main where

--import Graphics.UI.WX
import Data.List
import Data.Maybe

main :: IO ()
main = do 
    putStrLn "Input Total Number of People:"
    inpN   <- getLine
    putStrLn "Input Bridge capacity:"
    inpC   <- getLine
    putStrLn "Input time taken for each person, seperated by 1 space:"
    times  <- getLine
    putStrLn $ showResult $ getFirstResults (read inpN) (read inpC) (readTimes times)

type Trip            =  [Person]
type PureTrip        =  Trip
type NomadicTrip     =  Trip
type MixedTrip       =  Trip
type N               =  Int
type C               =  Int

type Person          =  Int
type Setler          =  Person
type Nomad           =  Person
type ForwardBag      =  [Trip]
type Sequence        =  [Trip]
type Time            =  Int

readTimes :: Read a => String -> [a]
readTimes s = map read (words s) 

showSequence ::Show a => [a] -> String
showSequence [] = ""
showSequence (x:[]) = show x ++ " go(es) across..." 
showSequence (x:y:[]) = show x ++ " go(es) across..." ++ "\n" ++ show y ++ " return(s)..."
showSequence xs = (showSequence (take 2 xs)) ++ "\n" ++ (showSequence (drop 2 xs))

showResult :: Show a => ([a],Int) -> String
showResult (xs,t) = "Best Sequences: \n \n" ++ showSequence xs ++ "\n \n" ++ "Crossing Time: " ++ show t ++ " minutes.\n" 

----------------------------- SCHEDULERS -----------------------------------------------------------------------

--------- Pure Trips Scheduler -------------- 

possiblePureTrips :: N -> C -> [PureTrip]
possiblePureTrips n c | n < c+2 = []
                      | otherwise = ([(n-c+1)..n]) : (possiblePureTrips (n-c) c)


pureTripsIterate:: [PureTrip] -> N -> C -> [([PureTrip],Int,Int,Int,Int,Bool)]
pureTripsIterate [] n c = []
pureTripsIterate xs n c | (n - (length xs) * c) > c =  ((xs, n - (length xs) * c ,0,0,length xs, True) : (pureTripsIterate (init xs) n c))   
                        | otherwise =  ((xs, n - (length xs) * c ,0,0,length xs, False) : (pureTripsIterate (init xs) n c))   
-- type of the result: [puretrip list, m,  nc , nc', exceed p (pure trips need to be served), (m > C ?)]

schedulePureTrips :: N -> C -> [(ForwardBag,Int,Int,Int,Int,Bool)]
schedulePureTrips n c = ([[]],n,0,0,0,True) : (pureTripsIterate (possiblePureTrips n c) n c) 
-- type of the result: [bag, m,  nc , nc', exceed p, (m > C ?)]


-------- Mixed Trips Scheduler --------------

scheduleFullMixedXNomadTrips :: N -> C -> Int -> [(ForwardBag, Int, Int, Int, Int, Bool)] -> [(ForwardBag, Int, Int, Int, Int, Bool)]
scheduleFullMixedXNomadTrips n c i []      = []
scheduleFullMixedXNomadTrips n c i ((fb,m,nc, nc',p,b):xs)  
  | (b == False || p < 0)  = ((fb,m,nc, nc',p,b):(scheduleFullMixedXNomadTrips n c i xs)) 
  | ( m - c + i > c)       = ((fb,m,nc, nc',p,b):(scheduleFullMixedXNomadTrips n c i (( ( ([1..i]++[(m-c+i+1)..m]) : fb ),m-c+i,nc',i,p - (max nc' 1) +1, True):xs)))
  | otherwise              = ((fb,m,nc, nc',p,b):(scheduleFullMixedXNomadTrips n c i (( ( ([1..i]++[(m-c+i+1)..m]) : fb ),m-c+i,nc',i,p - (max nc' 1) +1, False):xs)))
-- type of the 4th argument and the result: [bag, m,  nc , nc', exceed p, (m > C ?)]

scheduleFullMixedTrips :: N -> C -> [(ForwardBag, Int, Int, Int, Int, Bool)]
scheduleFullMixedTrips n c = map deleteEmptyTrips (pruneMixedBags ( foldl (\xs i -> scheduleFullMixedXNomadTrips n c i xs ) (schedulePureTrips n c) [1..(c-1)] ))
-- i from 1 to c-1: maximum c-1 nomad at this time.
-- type of the result: [bag, m,  nc , nc', exceed p, (m > C ?)]

 
pruneMixedBags :: [(ForwardBag, Int, Int, Int, Int, Bool)] -> [(ForwardBag, Int, Int, Int, Int, Bool)]
pruneMixedBags [] = []
pruneMixedBags ((fb,m, nc,nc',p,b):xs)     | (b == True || p < 0) = pruneMixedBags xs
                                           | otherwise = (fb,m, nc,nc',p,b): (pruneMixedBags xs)
-- m have to < or = C at this time, and p have to > or = 0, otherwise -> prune
-- type of the argument and result: [bag, m,  nc , nc', exceed p, (m > C ?)]

deleteEmptyTrips :: (ForwardBag, Int, Int, Int, Int, Bool) -> (ForwardBag, Int, Int, Int, Int, Bool)
deleteEmptyTrips (fb,m, nc,nc',p,b) = (deleteEmpty fb,m,nc,nc',p,b)
-- type of the argument and result: [bag, m,  nc , nc', exceed p, (m > C ?)]

deleteEmpty :: Eq a => [[a]] -> [[a]]
deleteEmpty [] = []
deleteEmpty (x:xs) | (x == []) = deleteEmpty xs
                   | otherwise = x:(deleteEmpty xs)

makeLastMixedTrip :: (ForwardBag,Int,Int,Int,Int,Bool) -> (ForwardBag,Int,Int)
makeLastMixedTrip (fb,m,nc, nc',p,b) = (([1..m]:fb),nc',p - (max nc' 1) +1)
-- type of the argument :[bag, m,  nc , nc', exceed p (pure trips need to be served), (m > C ?)]
-- type of the result: [bag, n, exceed p]


scheduleMixedTrips :: N -> C -> [(ForwardBag,Int,Int)]
scheduleMixedTrips n c = filter (\(fb,n,p) -> p >=0) (map makeLastMixedTrip (scheduleFullMixedTrips n c))
-- type of the result: [bag, n, exceed p]

    
scheduleMixedFinalizedNomadTrips :: N -> C -> [(ForwardBag,Int,Int,Int)]
scheduleMixedFinalizedNomadTrips n c = concat (map (finalizeNomadCount) (scheduleMixedTrips n c))
-- type of the result: [bag, n, exceed p, n'']
-- (if p = 0 then finalizedNomad = 0)
    
finalizeNomadCount :: (ForwardBag,Int,Int) -> [(ForwardBag,Int,Int,Int)]
finalizeNomadCount (fb,n,p) | (p > 0) = map (\i -> (fb,n,p,i)) [(max n 2)..(min (p+1) (length (head fb))) ]
                            | (p == 0) = [(fb,n,p,0)]
-- type of the result: [bag, n, exceed p, n'']

----------------- Nomadic Trips Scheduler --------------------

scheduleNomadicTrips :: N -> C -> [(ForwardBag,Int,Int)]
scheduleNomadicTrips n c = concat (map (\fb -> scheduleTNT fb ) (map (scheduleNT) (scheduleMixedFinalizedNomadTrips n c)))
-- type of the result: [bag, n, exceed p]

scheduleTNT :: (ForwardBag,Int,Int) -> [(ForwardBag,Int,Int)]
scheduleTNT (fb,n,p) | (p < 0)   = []
                     | (p == 0)  = [(fb,n,p)]
                     | otherwise =  concat (map (\(fbx,nx,px) -> scheduleTNT (fbx,nx,px) )  (map (\i -> (([1..i]:fb),i,p-i+1) ) [2..(min (p+1) n)]))
-- type of the argument and result: [bag, n, exceed p]

scheduleNT :: (ForwardBag,Int,Int,Int) -> (ForwardBag,Int,Int)
scheduleNT (fb,n,p,n'') | (p == 0) || (n == n'') = (fb,n,p)
                        | otherwise             = (([1..n'']:fb),n'',p-n''+1)  
-- type of the result: [bag, n, exceed p]

------------------------------- GET THE BEST BAG -----------------------------------------------------------------

finishedBags :: N -> C -> [Time] -> [(ForwardBag,Time)]
finishedBags n c ts = map (\fb -> (fb,bagTimeCost fb (sort ts))) (pruneFinishedBags (map (\(fb,n,p)->fb) (scheduleNomadicTrips n c)))


pruneFinishedBags :: [ForwardBag] -> [ForwardBag]
pruneFinishedBags [] = []
pruneFinishedBags (xs:xss) | (length (bagToPureTrips xs) <= (countPureTripsServed xs)) = (xs:(pruneFinishedBags xss)) 
                           | otherwise = pruneFinishedBags xss


seqWithTimes :: Sequence -> [Time] -> Sequence
seqWithTimes [] ts = []
seqWithTimes (xs:xss) ts = ((map (\x -> ( ts !!(x-1)) ) xs) : (seqWithTimes xss ts))

tripsTimeCost :: [Trip] -> [Time] -> Int
tripsTimeCost [] ts = 0
tripsTimeCost (xs:xss) (ts) = ( ts !!(maximum xs -1)) + (tripsTimeCost xss ts)  

bagTimeCost :: ForwardBag -> [Time] -> Int
bagTimeCost fb ts = (tripsTimeCost fb ts) + sum (map (\(n,i) -> (ts !! (n -1)) * (i-1)  ) (bagToNomads fb))

findBagsByTime :: ([(ForwardBag,Time)],Time) -> [(ForwardBag,Time)] 
findBagsByTime ([],t)               = []
findBagsByTime (((fb,t):fbts),tMin) | (t == tMin) = ((fb,t):(findBagsByTime (fbts,tMin)))  
                                    | otherwise = findBagsByTime (fbts,tMin)

findBestBags :: [(ForwardBag,Time)] -> [(ForwardBag,Time)] 
findBestBags fbts = findBagsByTime (fbts,minimum (map (\(fb,t) -> t) fbts))



------------ BAG to SEQUENCE CONVERTER -------------------------------------------------------------------------------
bagToNomads :: ForwardBag -> [(Nomad,Int)] 
bagToNomads xs = filter (\x -> snd x > 1) (zip ( map head (group (sort (concat xs))) ) ( map length (group (sort (concat xs)))))
-- Int : Occurence

bagToNomadList :: ForwardBag -> [Nomad]
bagToNomadList xs = map fst (bagToNomads xs)

bagToPureTrips :: ForwardBag -> [PureTrip]
bagToPureTrips xs = filter (all (\y -> notElem y (bagToNomadList xs))) xs

bagToNomadicTrips :: ForwardBag -> [NomadicTrip]
bagToNomadicTrips xs = filter (all (\y -> elem y (bagToNomadList xs))) xs

countNomads :: Trip -> [Nomad] -> Int
countNomads xs ns = length (filter (\x -> elem x ns) xs) 
-- ns : list of all nomads

bagToXNomadsTrips :: ForwardBag -> Int -> [Trip]
bagToXNomadsTrips xss n = filter (\xs -> (countNomads xs (bagToNomadList xss) == n) ) xss
-- n: number of nomad in the request bags

bagToLessThanXNomadsTrips :: ForwardBag -> Int -> [Trip]
bagToLessThanXNomadsTrips xss n = filter (\xs -> (countNomads xs (bagToNomadList xss) < n) ) xss
-- n: number of nomad that the request bag must not reach

bagToSequence :: ForwardBag -> C -> Sequence
bagToSequence xss c = init (concat (map (\nc -> bagToSequenceForXNomadsTrip xss nc ) [1..c]))
-- c: maximum number of nomads that any trip can have, also, the capacity of the bridge

bagToSequenceForXNomadsTrip :: ForwardBag -> Int -> Sequence
bagToSequenceForXNomadsTrip xss nc 
   = concat (map (\(i,t) -> xNomadsTripToSequence (i,t) nc (countPureTripsServedByLessThanXNomadsTrips xss nc ) (bagToPureTrips xss) ) (zip [0..] (bagToXNomadsTrips xss nc)) )
-- deal with trips with nc nomad (also with pure trips when nc = 0)
-- a trip with exactly nc nomad will serve (nc -1) pure trips
-- zip [0..]: we're doing a *for loop* here, (i,t): a trip *t* and its position *i*
 
xNomadsTripToSequence :: (Int,Trip) -> Int -> Int ->  [PureTrip] -> [Trip]
xNomadsTripToSequence (i,t) n ps pts = (t:( tupleToList (zip [1..(n-1)] (drop (ps+i) pts)))) ++ [[n]]
-- i: position of a trip with n nomad in its list of n-nomad-trip
-- ps: number of pure trips served with *trips with less than n nomad*
-- pts: list of pure trips in a forward bag 
-- ++ [[n]] is to ensure that person n will bring the torch back 
-- but if there are no people left (last trip)-> do not need to go back, then we delete this [by the *init* function in bagToSequence]

tupleToList :: [(a,[a])] -> [[a]]
tupleToList [] =  []
tupleToList ((x,xs):xss) = [x]:xs:(tupleToList xss)

countPureTripsServedByLessThanXNomadsTrips :: ForwardBag -> Int -> Int
countPureTripsServedByLessThanXNomadsTrips xss nc = length (filter (\x -> (elem x (bagToNomadList xss)) && (x/=1)) (concat (bagToLessThanXNomadsTrips xss nc)))
-- number of pure trips served with *trips with less than nc nomad*

countPureTripsServed :: ForwardBag -> Int
countPureTripsServed xss = notNegative (length (filter (\x -> (elem x (bagToNomadList xss)) && (x/=1)) (concat xss)) - 1)

notNegative :: Int -> Int
notNegative i | (i < 0) = 0
              | otherwise = i


------------------------------- GET RESULT ----------------------------------------------------------------------
getAllResults :: N -> C -> [Time] -> [(Sequence,Time)]
getAllResults n c ts | (n <= c)  = [([ts],sum ts)]
                     | otherwise = map (\(fb,t) -> (seqWithTimes (bagToSequence fb c) (sort ts),t) ) (findBestBags (finishedBags n c ts))

getFirstResults :: N -> C -> [Time] -> (Sequence,Time)
getFirstResults n c ts = head (getAllResults n c ts )
