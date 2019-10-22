module Musarithmetic where

data Pitch = Pitch {
    pnum :: Int,
    oct :: Int,
    accid :: Int 
} deriving (Show)

pitchStr :: Pitch -> String
pitchStr (Pitch pnum oct accid) = pname : show oct ++ accidName
    where
        pname = "cdefgab" !! pnum
        accidName = ["bb", "b", "", "#", "x"] !! (accid + 2)


pitchClassDia :: Pitch -> Int
pitchClassDia (Pitch pnum oct accid) = 7 * oct + pnum

pitchIncDia :: Pitch -> Int -> Pitch
pitchIncDia p n = Pitch pnum oct (accid p)
    where
        new = pitchClassDia p + n
        converted = (quotRem new 7)
        oct = (fst converted)
        pnum = (snd converted)

pnumChrom :: Int -> Int
pnumChrom pnumDia = [0, 2, 4, 5, 7, 9, 11] !! pnumDia

pitchClassChrom :: Pitch -> Int
pitchClassChrom (Pitch pnum oct accid) = 12 * oct + (pnumChrom pnum) + accid

data Interval = Interval {
    quality :: String,
    degree :: Int
} deriving (Show)

-- XXX "Maybe" problem with elem
intervalPerfect :: Interval -> Maybe
intervalPerfect (Interval quality degree) = Just (snd (elem degree [1, 4, 5]))

intervalChrom :: Interval -> Int
intervalChrom (Interval quality degree) = steps + adjust
    where
        steps = pitchClassChrom degree
        adjust = lookup adjustMap quality
        
        adjustMap
            | intervalPerfect = perfectAdjustment 
            | otherwise = imperfectAdjustment 
        
        imperfectAdjustment = 
            [("dd", 0 - 3), 
             ("d",  0 - 2),
             ("m",  0 - 1),
             ("M",  0),
             ("a",  1),
             ("aa", 2)] 

        perfectAdjustment = 
            [("dd", 0 - 2), 
             ("d",  0 - 1), 
             ("P",  0),
             ("a",  1),
             ("aa", 2)] 

pitchDiffChrom :: Pitch -> Pitch -> Pitch
pitchDiffChrom p1 p2 = pitchClassChrom p1 - pitchClassChrom p2

pitchInc :: Pitch -> Interval -> Pitch
pitchInc p i = 
    Pitch { 
        pnum = pnum newPitchDia, 
        oct = oct newPitchDia, 
        accid = pitchDiffChrom newPitchDia newPitchChrom
    } where
        newPitchDia = pitchIncDia p (degree i)
        newPitchChrom = pitchIncChrom p (intervalChrom i)









