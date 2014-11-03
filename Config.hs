module Config
(
    Config,
    ConfigKey
    (
        AssembleOnly,
        OutFile,
        InFile,
        LogVerbose
    ),
    Config.lookup,
    lookupStr,
    lookupDefaultStr,
    set,
    unset,
    setStr
) where

import Data.Map
import Data.Maybe

-- Configuration is stored in an hashmap whose keys are all here :

data ConfigKey
    = AssembleOnly
    | OutFile
    | InFile
    | LogVerbose

data ConfigValue
     = ConfigString String
     | ConfigInt Int
     | ConfigEmpty

type Config = Map ConfigKey ConfigValue

-- adds an arbitrary int to a ConfigKey to store it in the map
-- FIXME: use an hashmap instead

--instance Data.Hashable.Hashable ConfigKey where
hashWithSalt :: Int -> ConfigKey -> Int
hashWithSalt _ AssembleOnly = 0
hashWithSalt _ OutFile = 1
hashWithSalt _ InFile = 2
hashWithSalt _ LogVerbose = 3

instance Eq ConfigKey where
    key1 == key2 = ((hashWithSalt 0 key1) == (hashWithSalt 0 key2))

instance Ord ConfigKey where
    key1 <= key2 = ((hashWithSalt 0 key1) <= (hashWithSalt 0 key2))

-- Lookup functions

lookup :: Config -> ConfigKey -> Bool
lookup cnf key = case Data.Map.lookup key cnf of
    Just _ -> True
    Nothing -> False

lookupDefault :: Config -> ConfigKey -> ConfigValue -> ConfigValue
lookupDefault cnf key def = fromMaybe def (Data.Map.lookup key cnf)

lookupStr :: Config -> ConfigKey -> Maybe String
lookupStr cnf key = case Data.Map.lookup key cnf of
    Just (ConfigString str) -> Just str
    _ -> Nothing

lookupDefaultStr :: Config -> ConfigKey -> String -> String
lookupDefaultStr cnf key def = fromMaybe def (lookupStr cnf key)

-- Set an option in the map (boolean)

set :: ConfigKey -> Config -> Config
set key = insert key ConfigEmpty

unset :: ConfigKey -> Config -> Config
unset = delete

setStr :: ConfigKey -> String -> Config -> Config
setStr key val = insert key (ConfigString val)
