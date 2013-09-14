import Data.Map as Map
import System.Environment
import System.IO

import AST
import Bytecode
import Compiler
import Lexer
import Parser
import Serialize

-- store informations parsed from command line

type Config = Map String String

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
        "-h" -> fail ""
        "-o" -> case rest of
            (val:r) -> parseCommandArgs r >>= return . insert "OutFile" val
            _       -> fail $ "Missing parameter to -o"
        "-s" -> parseCommandArgs rest >>= return . insert "AssembleOnly" "True"
        _    -> parseCommandArgs rest >>= return . insert "InFile" arg

-- writeProgram: write assembly program (for use with -s)

writeProgram :: [Instr] -> Handle -> IO ()
writeProgram [] _ = return ()
writeProgram (i:rest) h = do
    hPutStrLn h (show i)
    Main.writeProgram rest h

-- main: read command line arguments, and run compiler

main = do
    args <- getArgs
    let config = parseCommandArgs args
    case config of
        Left err -> do
            printUsage 
            print err
        Right config ->
            case Map.lookup "InFile" config of
                Nothing -> do
                    printUsage
                    print "Error: no input file specified"
                Just fp -> do
                    print $ "Compiling file " ++ fp
                    outf <- openFile (case Map.lookup "OutFile" config of
                        Just fp -> fp
                        Nothing -> "out.bin") WriteMode
                    h <- openFile fp ReadMode
                    contents <- hGetContents h 
                    let prog = program contents
                    let toks = lexer prog
                    writef <- return $ case Map.lookup "AssembleOnly" config of
                        Just _ -> Main.writeProgram
                        Nothing -> Serialize.writeProgram
                    case parser toks of
                        Left err -> print err
                        Right (AST ast) -> case codegen ast ([], 0) of
                            Left err -> print err
                            Right (i, st) -> do
                                writef (i ++ [(Pop)]) outf
                                print "success"
                                hClose outf
