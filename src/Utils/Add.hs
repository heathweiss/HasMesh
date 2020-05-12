{-# LANGUAGE NoImplicitPrelude #-}
module Utils.Add(Add(..)) where

-- | Add various HasMesh types.
class Add a where
  (+++) :: a -> a -> a 
--todo -- needs to return an Either wrapper? Or some way to keep the result with unique values in case of vertexes.
