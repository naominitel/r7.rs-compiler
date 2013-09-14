module Config
(
    Config,
    ConfigKey(AssembleOnly, OutFile, InFile),
    configLookup,
    configLookupString,
    configLookupDefaultString,
    configSet,
    configUnset,
    configSetString
) where

import Data.Map

-- Configuration is stored in an hashmap whose keys are all here :

data ConfigKey
    = AssembleOnly
    | OutFile
    | InFile

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

instance Eq ConfigKey where
    key1 == key2 = ((hashWithSalt 0 key1) == (hashWithSalt 0 key2))

instance Ord ConfigKey where
    key1 <= key2 = ((hashWithSalt 0 key1) <= (hashWithSalt 0 key2))

-- Lookup functions

configLookup :: Config -> ConfigKey -> Bool
configLookup cnf key = case Data.Map.lookup key cnf of
    Just _ -> True
    Nothing -> False

configLookupDefault :: Config -> ConfigKey -> ConfigValue -> ConfigValue
configLookupDefault cnf key def = case Data.Map.lookup key cnf of
    Just val -> val
    Nothing -> def

configLookupString :: Config -> ConfigKey -> Maybe String
configLookupString cnf key = case Data.Map.lookup key cnf of
    Just (ConfigString str) -> Just str
    _ -> Nothing

configLookupDefaultString :: Config -> ConfigKey -> String -> String
configLookupDefaultString cnf key def = case configLookupString cnf key of
    Just str -> str
    Nothing -> def

-- Set an option in the map (boolean)

configSet :: ConfigKey -> Config -> Config
configSet key cnf = insert key (ConfigEmpty) cnf 

configUnset :: ConfigKey -> Config -> Config
configUnset key cnf = delete key cnf

configSetString :: ConfigKey -> String -> Config -> Config
configSetString key val cnf = insert key (ConfigString val) cnf
