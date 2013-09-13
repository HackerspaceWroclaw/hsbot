module MyUtils
( forever
, when
, readInt
) where

import qualified Data.Text as T(Text)

forever :: (Monad m) => m a -> m a
forever a = a >> forever a

when :: Monad m => Bool -> m [T.Text] -> m [T.Text]
when cond res = if cond then res else return []

readInt :: String -> Int
readInt = read