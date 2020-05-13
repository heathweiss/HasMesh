{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Utils.Add() where

import RIO
import RIO.Text as T
import qualified Utils.Exceptions as Hex

-- | Add various HasMesh types.
class Add a where
  -- (+++) :: a -> a -> a
  
  (+>+) :: Either Hex.HasMeshException a -> a -> Either Hex.HasMeshException a
  
--todo -- needs to return an Either wrapper? Or some way to keep the result with unique values in case of vertexes.

-- | A simple instance for testing. Sums >= 10 return an exception.
instance Add Int where
  (Right int) +>+ int' =
    let
      sum = int + int'
    in
      if sum < 10 then
        Right sum
      else
        Left $ Hex.GeneralException $ T.pack $ show sum <> " exceeds the limit" 
  (Left (Hex.GeneralException msg)) +>+ _ = Left $ Hex.GeneralException msg
