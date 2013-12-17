import Data.Map as Map
import System.Environment
import System.IO

import AST
import Begin
import Bytecode
import Compiler
import Config
import Debug
import Error
import Lexer
import Module
import Parser
import Serialize

-- default config values

defaultOutfile = "out.bin"

-- printUsage: print help about program options

printUsage :: IO ()
printUsage = do
    progName <- getProgName
    putStrLn "Usage: "
    putStrLn ""
    putStrLn $ "    " ++ progName ++ " [option|filename]..."
    putStrLn ""
    putStrLn "Common options are: "
    putStrLn "    -s: Assemble only. Don't generate bytecode"
    putStrLn "    -o file: Write output to file instead of out.bin"
    putStrLn "    -h: Display this message"
    putStrLn ""

-- parse command line arguments and store them in an HashMap

parseCommandArgs :: [String] -> Either String Config
parseCommandArgs [] = return $ Map.empty
parseCommandArgs (arg:rest) = 
    case arg of
        "-h" -> Left ""
        "-o" -> case rest of
            (val:r) -> parseCommandArgs r >>= return . setStr OutFile val
            _       -> Left $ "Missing parameter to -o"
        "-s" -> parseCommandArgs rest >>= return . set AssembleOnly
        "-v" -> parseCommandArgs rest >>= return . set LogVerbose
        _    -> parseCommandArgs rest >>= return . setStr InFile arg

-- writeAssembly: write assembly program (for use with -s)

writeAssembly :: [Instr] -> Handle -> IO ()
writeAssembly [] _ = do
    putStrLn "file successfully written: "
    return ()                            
writeAssembly (i:rest) h = do
    hPutStrLn h (show i)
    writeAssembly rest h

-- writeProgram: write ouput to file (assembly or bin, depending on config)

writeProgram :: Config -> CompiledModule -> Handle -> IO ()
writeProgram cnf = case Config.lookup cnf AssembleOnly of
    True -> \m -> let (Mod _ _ instrs) = m in writeAssembly instrs
    False -> Serialize.writeProgram

-- main: read command line arguments, and run compiler

main = do

    -- get command line arguments
    args <- getArgs
    case parseCommandArgs args of

        -- error in argument parsing
        Left err -> do
            printUsage 
            putStrLn err

        -- get input file
        Right cnf -> case lookupStr cnf InFile of

            -- no input file
            Nothing -> do
                printUsage
                putStrLn "Error: no input file specified"

            -- run compiler
            Just fp -> do

                -- open input file
                logW cnf $ "Compiling file " ++ fp
                h <- openFile fp ReadMode
                contents <- hGetContents h
                let prog = program contents fp

                -- check syntax
                case lexer prog of

                    -- run parser
                    Right toks -> case parser toks of

                        -- parsing failed
                        Left err -> do
                            reportError err
                            fail $ "Aborting due to previous error"

                        -- generate assembly
                        Right mod ->
                            case compileModule mod of

                            -- compile-time error
                            Left errs -> reportErrors errs

                            -- write output to file
                            Right mod -> do
                                let outfp = lookupDefaultStr cnf OutFile defaultOutfile
                                outf <- openFile outfp WriteMode
                                Main.writeProgram cnf mod outf
                                hClose outf

                    -- syntax verification failed
                    Left errs -> reportErrors errs

