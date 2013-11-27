module Imports
(
    Imports,
    Import(Lib, Only, Except, Prefix, Rename)
) where

import Identifier
import Library

data Import
    = Lib LibName
    | Only Import [Identifier]
    | Except Import [Identifier]
    | Prefix Import [Identifier]
    | Rename Import [(Identifier, Identifier)]

type Imports = [Import]
