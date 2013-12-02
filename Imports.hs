module Imports
(
    Imports,
    Import(Lib, Only, Except, Prefix, Rename),
    LibName
) where

import Identifier

type LibName = [String]

data Import
    = Lib LibName
    | Only Import [Identifier]
    | Except Import [Identifier]
    | Prefix Import [Identifier]
    | Rename Import [(Identifier, Identifier)]

type Imports = [Import]
