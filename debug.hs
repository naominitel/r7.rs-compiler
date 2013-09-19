module Debug 
(
    LogLevel,
    Debug.log,
    logW,
    logE
) where

import Config

data LogLevel
    = Warning
    | Error

log :: Config -> String -> LogLevel -> IO ()
log cnf msg lvl = do
    case lvl of
        Error -> putStrLn msg
        Warning -> case Config.lookup cnf Config.LogVerbose of
            True -> putStrLn msg
            False -> return ()

logW :: Config -> String -> IO ()
logW cnf msg = Debug.log cnf msg Warning

logE :: Config -> String -> IO ()
logE cnf msg = Debug.log cnf msg Error
