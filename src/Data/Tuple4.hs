-- | Tuples with four elements
module Data.Tuple4 where

-- | Returns the first element of the tuple
fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a

-- | Returns the second element of the tuple
snd4 :: (a,b,c,d) -> b
snd4 (_,b,_,_) = b

-- | Returns the third element of the tuple
trd4 :: (a,b,c,d) -> c
trd4 (_,_,c,_) = c

-- | Returns the fourth element of the tuple
fth4 :: (a,b,c,d) -> d
fth4 (_,_,_,d) = d
