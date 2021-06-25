{-# LANGUAGE RecordWildCards #-}    -- per utilitzar fields

import Data.Char (isUpper)
import Data.List (nub, isInfixOf)
import Data.List.Split (splitOn)    -- cal instal·lar: cabal install split
import Data.String.Utils (strip)    -- cal instal·lar: cabal install MissingH
import Data.Maybe (mapMaybe, fromMaybe)

type Programa = [ Regla ]

data Regla = Regla { _cap::Atom, _cos::[ Atom ] }

data Atom = Atom { _nomPredicat::String, _termes::[ Term ] }
    deriving (Eq, Show)

data Term = Var String | Sym String
   deriving (Eq, Show)