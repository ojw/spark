module Option where

import Maybe

-- Choice must be in [0..1).
type Choice = Float

type Option a = [(Float, a)]

normalizeOption : [(Float, a)] -> Option a
normalizeOption options = let totalWeight = sum (map fst options) in map (\pair -> (fst pair / totalWeight, snd pair)) options

intOption : [(Int, a)] -> Option a
intOption ops = normalizeOption (map (\pair -> (toFloat (fst pair), snd pair)) ops)

-- Given options and a choice, returns the options chosen, if any.
-- I don't feel good about this style at all.
pick : Option a -> Choice -> Maybe a
pick options choice = if isEmpty options then Nothing else
                          let winner = sortBy fst . 
                                       filter (\pair -> fst pair >= choice) .
                                       scanl1 (\pair accum -> (fst pair + fst accum, snd pair)) <|
                                       options
                          in
                            if isEmpty winner then Nothing else (Just . snd . head) winner

pickWithDefault : a -> Option a -> Choice -> a
pickWithDefault default option choice = Maybe.maybe default id (pick option choice)

