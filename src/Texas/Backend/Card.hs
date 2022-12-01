{-# LANGUAGE InstanceSigs #-}
module Texas.Backend.Card where


-- | Suit of a card
data Suit = Heart | Spade | Diamond | Club
    deriving Eq

instance Show Suit where
  show :: Suit -> String
  show Heart = "♥"
  show Spade = "♠"
  show Diamond = "♦"
  show Club = "♣"

data RankHelper = Rk Int | J | Q | K | A

instance Num RankHelper where
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger i | 1 <= i && i <= 13 = Rk $ fromInteger i
                | otherwise    = undefined
  negate = undefined

data Rank = R02 | R03 | R04 | R05 | R06 | R07 | R08 | R09 | R10 | R0j | R0q | R0k | R0a
    deriving (Eq, Ord, Enum, Bounded)

rkToStr :: Rank -> String
rkToStr R0a = "A"
rkToStr R02 = "2"
rkToStr R03 = "3"
rkToStr R04 = "4"
rkToStr R05 = "5"
rkToStr R06 = "6"
rkToStr R07 = "7"
rkToStr R08 = "8"
rkToStr R09 = "9"
rkToStr R10 = "10"
rkToStr R0j = "J"
rkToStr R0q = "Q"
rkToStr R0k = "K"

instance Show Rank where
  show :: Rank -> String
  show r = "<rk " ++ rkToStr r ++ ">"

-- |Smart constructor for Rank. 
--  Converts number or J/Q/K/A to Rank.
rk :: RankHelper -> Rank
rk (Rk 2) = R02
rk (Rk 3) = R03
rk (Rk 4) = R04
rk (Rk 5) = R05
rk (Rk 6) = R06
rk (Rk 7) = R07
rk (Rk 8) = R08
rk (Rk 9) = R09
rk (Rk 10) = R10
rk (Rk 11) = R0j
rk (Rk 12) = R0q
rk (Rk 13) = R0k
rk (Rk 1)  = R0a
rk J       = R0j
rk Q       = R0q
rk K       = R0k
rk A       = R0a
rk _       = undefined

data Card = C {
    suit :: Suit,
    rank :: Rank
}
    deriving (Eq)

instance Ord Card where
  compare :: Card -> Card -> Ordering
  compare a b = compare (rank a) (rank b)

instance Show Card where
  show :: Card -> String
  show (C s r) = "<" ++ show s ++ rkToStr r ++ ">"

allCards :: [Card]
allCards = bdc Heart ++ bdc Spade ++ bdc Diamond ++ bdc Club
    where bdc st = [C st i | i <- [R02 .. R0a]]

incRank :: Int -> Rank -> Rank
incRank x r = toEnum $ fromEnum r + x

-- >>> allCards
-- [<♥2>,<♥3>,<♥4>,<♥5>,<♥6>,<♥7>,<♥8>,<♥9>,<♥10>,<♥J>,<♥Q>,<♥K>,<♥A>,<♠2>,<♠3>,<♠4>,<♠5>,<♠6>,<♠7>,<♠8>,<♠9>,<♠10>,<♠J>,<♠Q>,<♠K>,<♠A>,<♦2>,<♦3>,<♦4>,<♦5>,<♦6>,<♦7>,<♦8>,<♦9>,<♦10>,<♦J>,<♦Q>,<♦K>,<♦A>,<♣2>,<♣3>,<♣4>,<♣5>,<♣6>,<♣7>,<♣8>,<♣9>,<♣10>,<♣J>,<♣Q>,<♣K>,<♣A>]
