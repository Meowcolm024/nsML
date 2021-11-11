{-# LANGUAGE TemplateHaskell #-}

module SymbolTable where

import           Control.Lens.TH                ( makeLenses )
import qualified Data.Map                      as Map
import           Data.STRef
import           Lens.Micro
import           Types

-- ! dummy
data Signature = TypeSignature | ConstrSignature | VarSignature

-- functions will be transformed to be lambdas 

data SymbolTable = SymbolTable
    { _types        :: Map.Map Idx Signature     -- ^ types
    , _constructors :: Map.Map Idx Signature     -- ^ constructors
    , _variables    :: Map.Map Idx Signature     -- ^ variable / function
    }

makeLenses ''SymbolTable

type Table s = STRef s (SymbolTable, Int)
