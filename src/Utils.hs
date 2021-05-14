module Utils where


import qualified Data.Maybe

-- | Try to update an element at a given position in a list.
updateListAt :: Int -> (a -> a) -> [a] -> [a]
updateListAt index changeTo list = leftPart ++ newA ++ rightPart
    where
        (leftPart, leftTail) = splitAt index list
        newA = if index < 0 then [] else map changeTo (take 1 leftTail)
        rightPart = if index < 0 then leftTail else drop 1 leftTail

-- | Get element of the list by index
getListElemAt :: Int -> [a] -> Maybe a
getListElemAt _ [] = Nothing
getListElemAt 0 (a : _rest) = Just a
getListElemAt index (_a : rest)
  = if index < 0 then Nothing else getListElemAt (index - 1) rest

-- | Updates only one element under index "ind" using function "f"
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt ind f list = if ind > 0 then newList else list
  where
    (begin, end) = splitAt (ind-1) list
    newList = begin ++ newElem ++ drop 1 end
    maybeElem = Data.Maybe.listToMaybe end
    newElem = case maybeElem of
      Nothing      -> []
      Just oldElem -> [f oldElem]
