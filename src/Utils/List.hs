{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}

{- |
Supply specialized lists.
-}
module Utils.List(SafeList()) where

import RIO

data Empty
data NonEmptyID

data SafeList a b where
     Nil :: SafeList a Empty
     Cons:: a -> a -> [a] -> SafeList a b -> SafeList a NonEmptyID

     

safeHead :: SafeList a NonEmptyID -> a
safeHead (Cons x y ys _) = x


runSafeHead = safeHead $ Cons "hi" "there" ["how is your", "pie there" ] Nil
runSafeHead2 = safeHead $ Cons "hi" "there" [] Nil


