{-# LANGUAGE InstanceSigs #-}
module Texas.Backend.Combination where
import Texas.Backend.Card 
import Data.List (sort)

data CombType = THigh | TPair | TPairs | TThree | TStraight | TFlush | THouse | TFour | TStraightFlush | TRoyalFlush
    deriving (Eq, Bounded, Enum, Ord, Show)

data Combination = RoyalFlush Suit
                 | StraightFlush Suit Rank -- Rank is the highest rank
                 | Four Rank Card -- Card is the remaining Card
                 | House [Card] [Card] -- three and two
                 | Flush [Card]
                 | Straight [Card]
                 | Three [Card] [Card] -- three and rem
                 | Pairs [Card] [Card] Card -- 2 2 1
                 | Pair [Card] [Card] -- Pair and rem
                 | High [Card]
    deriving (Eq,Show)

combType :: Combination -> CombType
combType (RoyalFlush _)         = TRoyalFlush
combType (StraightFlush _ _)    = TStraightFlush
combType (Four _ _)             = TFour
combType (House _ _)            = THouse
combType (Flush _)              = TFlush
combType (Straight _)           = TStraight
combType (Three _ _)            = TThree
combType (Pairs _ _ _)          = TPairs
combType (Pair _ _)             = TPair
combType (High _)               = THigh


instance Ord Combination where
  compare :: Combination -> Combination -> Ordering
  compare (RoyalFlush _) (RoyalFlush _)             = EQ
  compare (StraightFlush _ r1) (StraightFlush _ r2) = compare r1 r2
  compare (Four r1 c1) (Four r2 c2)                 = let cr = compare r1 r2 in
                                                        if cr == EQ then
                                                            compare c1 c2 
                                                        else cr
  compare (House l3 l2) (House r3 r2)               = let cr = compare (rank $ head l3) (rank $ head r3) in
                                                        if cr == EQ then
                                                            compare (rank $ head l2) (rank $ head r2)
                                                        else cr
  compare (Flush lhs) (Flush rhs)                   = compare (_sorted lhs) (_sorted rhs)
  compare (Straight lhs) (Straight rhs)             = compare (maximum lhs) (maximum rhs)
  compare (Three l3 lr) (Three r3 rr)               = let cr = compare (rank $ head l3) (rank $ head r3) in
                                                        if cr == EQ then
                                                            compare (_sorted lr) (_sorted rr)
                                                        else cr
  compare (Pairs lp1 lp2 lc) (Pairs rp1 rp2 rc)     = let cr = compare (_sorted [head lp1,head lp2]) (_sorted [head rp1, head rp2]) in
                                                        if cr == EQ then
                                                            compare lc rc 
                                                        else cr
  compare (Pair lp lr) (Pair rp rr)                 = let cr = compare (head lp) (head rp) in
                                                        if cr == EQ then
                                                            compare (_sorted lr) (_sorted rr)
                                                        else cr
  compare (High lhs) (High rhs)                     = compare (_sorted lhs) (_sorted rhs)
  compare lhs rhs                                   = compare (combType lhs) (combType rhs)

_sorted :: [Card] -> [Card]
_sorted = reverse . sort

-- >>> compare (Flush [C Heart R08, C Heart R07, C Heart R0k, C Heart R02, C Heart R03]) (Flush [C Spade R0k, C Spade R07, C Spade R09, C Spade R02, C Spade R03])
-- LT


-- Return the 5 cards of a combination. Return value is undefined if the combination is invalid
cardsOf :: Combination -> [Card]
cardsOf (RoyalFlush s)      = [C s i | i <- [R10 .. R0a]]
cardsOf (StraightFlush s r) = [C s i | i <- [incRank (-4) r .. r]]
cardsOf (Four r c)          = c:[C s r | s <- [Heart,Spade,Diamond,Club]]
cardsOf (House three two)   = three ++ two
cardsOf (Flush l)           = l 
cardsOf (Straight l)        = l 
cardsOf (Three three rm)    = three ++ rm
cardsOf (Pairs p1 p2 rm)    = rm:p1 ++ p2 
cardsOf (Pair p rm)         = p ++ rm 
cardsOf (High l)            = l 


-- >>> cardsOf $ RoyalFlush Spade
-- [<♠10>,<♠J>,<♠Q>,<♠K>,<♠A>]
-- >>> cardsOf $ StraightFlush Heart $ rk 7
-- [<♥3>,<♥4>,<♥5>,<♥6>,<♥7>]
-- >>> cardsOf $ Four (rk A) $ C Heart $ rk Q
-- [<♥Q>,<♥A>,<♠A>,<♦A>,<♣A>]
