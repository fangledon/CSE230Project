{-# LANGUAGE InstanceSigs #-}
module Texas.Backend.Combination where
import Texas.Backend.Card 
import Data.List (sortBy, foldl')
import Control.Monad.Except

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

badComb = High []
-- >>> compare badComb $ High []
-- GT

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

combFirst :: Combination -> [Card]
combFirst (House f _) = f 
combFirst (Three f _) = f 
combFirst (Pairs f _ _) = f 
combFirst (Pair f _) = f 
combFirst (High f) = f 
combFirst _ = []

combSecond :: Combination -> [Card]
combSecond (House _ s) = s 
combSecond (Three _ s) = s 
combSecond (Pairs _ s _) = s  
combSecond (Pair _ s) = s  
combSecond _ = []


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
_sorted = sortBy $ flip compare

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

-- | Return the best combination achieved by 5 cards
cardsToComb :: [Card] -> Combination
cardsToComb cards = case res of
                        Just c -> c 
                        Nothing -> High cards
    where   scs = _sorted cards
            res = _ctc scs

_ctc :: [Card] -> Maybe Combination
_ctc scs =  catchError (matchRoyalFlush scs) (\_ ->
            catchError (matchStraightFlush scs) (\_ ->
            catchError (matchFour scs) (\_ ->
            catchError (matchHouse scs) (\_ ->
            catchError (matchFlush scs) (\_ ->
            catchError (matchStraight scs) (\_ ->
            catchError (matchThree scs) (\_ ->
            catchError (matchPairs scs) (\_ ->
            matchPair scs))))))))

matchRoyalFlush :: [Card] -> Maybe Combination
matchRoyalFlush scs@(f:_) = do
                                matchStraightFlush scs
                                if rank f == R0a then
                                    return $ RoyalFlush $ suit f 
                                else
                                    throwError ()
matchRoyalFlush _ = throwError ()

matchStraightFlush :: [Card] -> Maybe Combination
matchStraightFlush scs@(f:_) = do 
                                matchStraight scs
                                matchFlush scs
                                return $ StraightFlush (suit f) (rank f)
matchStraightFlush _ = throwError ()

matchStraight :: [Card] -> Maybe Combination
matchStraight [x]       = Just $ Straight [x]
matchStraight (x:y:xs)  | ry /= R0a && rx == succ ry    = do {rest <- matchStraight (y:xs); return  $ Straight $ x:cardsOf rest}
                        | otherwise                     = throwError ()
                    where   rx = rank x
                            ry = rank y
matchStraight []        = throwError ()

matchFlush :: [Card] -> Maybe Combination
matchFlush cs@(c:r) | all (\x -> suit x == suit c) r    = return $ Flush cs 
                    | otherwise                         = throwError ()
matchFlush [] = throwError ()

rkArray :: [Card] -> [(Rank, Int)]
rkArray = foldl' rAi []
    where   rAi :: [(Rank, Int)] -> Card -> [(Rank, Int)]
            rAi [] C{rank = r} = [(r, 1)]
            rAi rs@((r1, cnt):xs) C{rank = r}   | r == r1   = (r1, cnt+1):xs
                                                | otherwise = (r, 1):rs

-- >>> rkArray [C Spade R08, C Heart R08, C Spade R07, C Diamond R07, C Club R07]
-- [(<rk 7>,3),(<rk 8>,2)]

matchFour :: [Card] -> Maybe Combination
matchFour scs = case ftd of 
                    [] -> throwError ()
                    ((r,_):_) -> return $ Four r $ head $ filter (\x -> rank x /= r) scs
    where   ra      = rkArray scs
            ftd     = filter (\(_, c) -> c == 4) ra 


-- >>> matchFour [C Heart R04, C Spade R04, C Club R04, C Diamond R04, C Spade R03]
-- Just (Four <rk 4> <♠3>)


matchHouse :: [Card] -> Maybe Combination
matchHouse scs = do
                    c3 <- matchThree scs
                    if length ra == 2 then
                        return $ House (combFirst c3) (combSecond c3)
                    else 
                        throwError ()
    where   ra  = rkArray scs

matchThree :: [Card] -> Maybe Combination
matchThree scs = case ftd of
                    [] -> throwError ()
                    ((r,_):_) -> return $ Three (filter (\x -> rank x == r) scs) (filter (\x -> rank x /= r) scs)
    where   ra  = rkArray scs
            ftd = filter (\(_,c) -> c == 3) ra

-- >>> matchHouse [C Heart R0k, C Spade R0k, C Diamond R0k, C Spade R04, C Diamond R04]
-- Just (House [<♥K>,<♠K>,<♦K>] [<♠4>,<♦4>])

matchPairs :: [Card] -> Maybe Combination
matchPairs scs = do
                    p1 <- matchPair scs  
                    p2 <- matchPair $ combSecond p1 
                    return $ Pairs (combFirst p1) (combFirst p2) $ head $ combSecond p2


matchPair :: [Card] -> Maybe Combination
matchPair scs = case ftd of
                    [] -> throwError ()
                    (r,_) : _ -> return $ Pair (filter (\x -> rank x == r) scs) (filter (\x -> rank x /= r) scs)
    where   ra  = rkArray scs
            ftd = filter (\(_,c) -> c == 2) ra

-- >>> matchPairs [C Heart R0k, C Spade R0k, C Diamond R05, C Spade R04, C Diamond R04]
-- Just (Pairs [<♠4>,<♦4>] [<♥K>,<♠K>] <♦5>)

-- >>> matchPairs [C Heart R0k, C Spade R0k, C Diamond R05, C Spade R04, C Diamond R04]
-- Just (Pairs [<♠4>,<♦4>] [<♥K>,<♠K>] <♦5>)
