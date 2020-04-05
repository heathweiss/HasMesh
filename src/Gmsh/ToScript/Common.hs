{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{- |
Generate Gmsh script that is common to the builtin cad engine and open cascade, such as writing comments.

import qualified Gmsh.ToScript.Common as ScrC
or as part of the Gmsh import module
import qualified Gmsh.Gmsh as Gmsh
-}
module Gmsh.ToScript.Common(writeSeparator, writeComment) where

import RIO
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import Data.String.Interpolate ( i )


-- | Output a separator to help format the .geo file into sections.
writeSeparator :: B.ByteString
writeSeparator = "///////////////////////////////////////////////////////////////"

-- | Output a gmsh comment.
writeComment :: T.Text -> B.ByteString
writeComment text =
  [i|\n//#{text}|] :: B.ByteString

