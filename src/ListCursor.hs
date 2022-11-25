{-# LANGUAGE ImportQualifiedPost #-}

module ListCursor
  ( ListCursor (..),
    prev,
    next,
    tryApply,
    mapToList,
  )
where

data ListCursor a = ListCursor
  { lcSelected :: a,
    lcBefore :: [a],
    lsAfter :: [a]
  }
  deriving (Show)

prev :: ListCursor a -> Maybe (ListCursor a)
prev ListCursor {lcSelected = curr, lcBefore = (p : ps), lsAfter = ns} =
  Just ListCursor {lcSelected = p, lcBefore = ps, lsAfter = (curr : ns)}
prev ListCursor {lcBefore = []} = Nothing

next :: ListCursor a -> Maybe (ListCursor a)
next ListCursor {lcSelected = curr, lcBefore = ps, lsAfter = (n : ns)} =
  Just ListCursor {lcSelected = n, lcBefore = (curr : ps), lsAfter = ns}
next ListCursor {lsAfter = []} = Nothing

tryApply :: (a -> Maybe a) -> Maybe a -> Maybe a
tryApply f x =
  let result = f =<< x
   in maybe x return result

mapToList :: (Bool -> a -> b) -> ListCursor a -> [b]
mapToList f ListCursor {lcSelected = s, lcBefore = ps, lsAfter = ns} =
  reverse (map (f False) ps) ++ [f True s] ++ map (f False) ns