{-# LANGUAGE PackageImports #-}
module Git.Pile.ReposToWorkOn.MaybeUtils
 (mapMaybeM) where

import "base" Data.Maybe
 (catMaybes)

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f as = mapM f as >>= (return . catMaybes)