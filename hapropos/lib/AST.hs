module AST where

import Prop

type Name = String
data AST = Bind Name Prop | Pexp Prop deriving (Read, Show, Eq) -- Variable binding / proposition.
