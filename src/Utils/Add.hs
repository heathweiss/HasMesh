{-# LANGUAGE NoImplicitPrelude #-}
module Utils.Add(Add(..)) where

-- | Add various HasMesh types.
class Add a where
  (+++) :: a -> a -> a 
