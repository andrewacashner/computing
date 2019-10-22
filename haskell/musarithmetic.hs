-- musarithmetic
-- Andrew A. Cashner
-- 2019/10/22
-- First program in Haskell
-- Do arithmetic with pitches: e.g., g4 + P5 = d5, eb3 - M3 = c3, g4 - c4 = P5

-- TODO
-- results are off by one in pc2pitchChrom, 
-- possibly need to convert dia pcnums to chrom pcnums?

module Musarithmetic where

-- PITCH
-- Pitch contains 0-indexed number of a diatonic pitch class (c = 0)
-- Octave is Helmholtz octave number
-- Accid is integer in range [(-2)..2]
data Pitch = Pitch {
    pnum :: Int,
    oct :: Int,
    accid :: Int 
} deriving (Show)

pitchStr :: Pitch -> String
pitchStr (Pitch pnum oct accid) = pname : accidName ++ show oct 
    where
        pname = "cdefgab" !! pnum
        accidName = ["bb", "b", "", "#", "x"] !! (accid + 2)

-- CONVERSION
-- convert between Pitch and absolute pitch number in a given base
-- (diatonic/chromatic)
pitch2pcBase :: Int -> Pitch -> Int
pitch2pcBase base (Pitch pnum oct accid) = base * oct + pnum + accid

pc2pitchBase :: Int -> Int -> Int -> Pitch
pc2pitchBase base pnum accid = 
    Pitch {
        oct = (fst converted),
        pnum = (snd converted),
        accid = accid
    } where
        converted = quotRem (pnum - accid) base

-- diatonic (ignore accidental)
pitch2pcDia :: Pitch -> Int
pitch2pcDia (Pitch pnum oct accid) = pitch2pcBase 7 (Pitch pnum oct 0) 

pc2pitchDia :: Int -> Int -> Pitch
pc2pitchDia pnum accid = pc2pitchBase 7 pnum accid

-- chromatic
-- get chromatic pitch-class number (e.g., D = 2)
pnumDia2Chrom :: Int -> Int
pnumDia2Chrom n = [0, 2, 4, 5, 7, 9, 11] !! n

pitch2pcChrom :: Pitch -> Int
pitch2pcChrom = pitch2pcBase 12 

pc2pitchChrom :: Int -> Pitch
pc2pitchChrom pnum = pc2pitchBase 12 pnum 0 

-- INTERVAL
-- quality is e.g.,"P" or "m"
-- degree is the diatonic steps (P5 = "P" 5)
data Interval = Interval {
    quality :: String,
    degree :: Int
} deriving (Show)

-- Is the interval in the list of perfect intervals?
intervalPerfect :: Interval -> Bool
intervalPerfect (Interval quality degree) = elem degree [1, 4, 5]

-- Convert the interval to chromatic steps
intervalChrom :: Interval -> Int
intervalChrom i = steps + adjust
    where
        steps = pnumDia2Chrom (degree i - 1)
        adjust = case (lookup (quality i) adjustMap) of
            Just x -> x
            Nothing -> error "Unknown interval quality"

        adjustMap
            | intervalPerfect i = perfectAdjustment 
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

-- difference of two absolute chromatic pitch numbers
pitchDiffChrom :: Pitch -> Pitch -> Int
pitchDiffChrom p1 p2 = pitch2pcChrom p1 - pitch2pcChrom p2

-- PITCH ARITHMETIC

-- increase pitch by diatonic steps
pitchIncDia :: Pitch -> Int -> Pitch
pitchIncDia p n = pc2pitchDia (pitch2pcDia p + n) (accid p)

-- increase pitch by Interval
    -- accid is the difference between the diatonic sum with
    --   accidental and the absolute chromatic sum 
pitchInc :: Pitch -> Interval -> Pitch
pitchInc p i = 
    Pitch { 
        pnum = pnum newPitchDia, 
        oct = oct newPitchDia, 
        accid = pitchDiffChrom newPitchDia newPitchChrom
    } where
        newPitchDia = pitchIncDia p (degree i)
        newPitchChrom = pc2pitchChrom (pitch2pcChrom p + (intervalChrom i))





