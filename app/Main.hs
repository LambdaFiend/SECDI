module Main where

import           Control.Exception
import           Control.Monad.IO.Class   (liftIO)
import           Data.Char
import           Data.List
import           Display
import           Evaluation
import           GPL
import           Lexer
import           Parser
import           Syntax
import           System.Console.ANSI
import           System.Console.Haskeline
import           System.Directory
import           System.IO

type EnvVarName = String

type MainEnvironment = [(EnvVarName, SECD)]

type CommandList = [String]

getHelp :: String
getHelp =
  (\s -> s ++ "\n") $
    intercalate "\n" $
      "Command names (the first token of the command) are not case sensitive.\n"
        : "[:var, :v, :assign and :a assign a written term to <var_name>]\n"
        : ":v <var_name>\n"
        : "[:eval, :ev and :e fully evaluate the term from <var_name>]\n"
        : ":e <var_name>\n"
        : "[:evaln, :evn and :en evaluate (<number_of_steps>) n-steps the term from <var_name>]\n"
        : ":en <number_of_steps> <var_name>\n"
        : "[:help, :h and :? display information regarding the commands]\n"
        : ":h\n"
        : "[:show, :sh and :s show the term assigned to <var_name>]\n"
        : ":s <var_name>\n"
        : ( "[Additionally, for command :var, :v, :assign and :a, a third argument may be added, "
              ++ "which is meant to be either :eval, :ev, :e, :evaln, :evn and :en, in which case "
              ++ "it evaluates from the current environment (given a <var_name2>) and then stores it into <var_name1>]\n"
          )
        : ":v <var_name1> :ev <var_name2>\n"
        : ":v <var_name1> :evn <number_of_steps> <var_name2>\n"
        : "[:load and :l load terms from file at <file_path>, which are assigned inside the file as <var_name> := <expression>, and then loaded into the environment correspondingly]\n"
        : ":l <file_path>\n"
        : "[:v? and :vars show the first page (10 environment variables) of the environment, if a number is not specified]\n"
        : ":v?\n"
        : "[:v? and :vars will show the <number>'th page (containing 10 environment variables' names)]\n"
        : ":v? <number>\n"
        : "[:m, :mv and :move will store the contents of <var_name2> into <var_name1>]\n"
        : ":mv <var_name1> <var_name2>\n"
        : "[:q and :quit close the REPL]\n"
        : ":q\n"
        : "[:ee, :eenv and :evalenv attempt to evaluate all variables in the environment]\n"
        : ":evalenv\n"
        : "[:c, :ce, :cenv, :clear and :clearenv clear the environment, which means there will be no variables accessible until new ones are added]\n"
        : ":c\n"
        : "[:av? and :allvars show all variables in the environment]\n"
        : ":av?\n"
        : "[:showenv, :showe, :senv and :se]\n"
        : ":se\n"
        : "[The commands for showing, typing and evaluating the environment can also be used for environment pages, as follows]\n"
        : ":se <page_number>\n"
        : ":ee <page_number>\n"
        : "[Page numbers start at 1]\n"
        : "[Programs may be executed directly in the command line; SECDI will show and then evaluate it (and show the result of the evaluation)]\n"
        : "<program>"
        : []

customHaskelineSettings :: Settings IO
customHaskelineSettings = defaultSettings {historyFile = Just "command_history.txt"}

main :: IO ()
main = do
  comml <- handleCommHistFile "command_history.txt"
  evaluate (length comml)
  putStrLn "\nSECDI  Copyright (C) 2026  Lambda Fiend\nThis program comes with ABSOLUTELY NO WARRANTY; for details type `:gplw'.\nThis is free software, and you are welcome to redistribute it\nunder certain conditions; type `:gplc' for details.\n"
  putStrLn "Welcome to SECD Interpreter, SECDI for short"
  putStrLn "Enter :? for help with commands"
  putStrLn ""
  runInputT customHaskelineSettings (main' [] (reverse (lines comml)))
  return ()

main' :: MainEnvironment -> CommandList -> InputT IO ()
main' env comml = do
  maybeCommand <- getInputLine "secdi> "
  let command = case maybeCommand of Just c -> c; Nothing -> ""
  let commToks = (\comm -> case comm of (x : xs) -> map toLower x : xs; [] -> []) $ words command
  env' <- case (commToks) of
    [] -> return env
    [":gplw"] -> do
      liftIO $ putStrLn gnuWarranty
      return env
    [":gplc"] -> do
      liftIO $ putStrLn gnuComplete
      return env
    [eenv, k] | and (map isDigit k) && elem eenv [":ee", ":eenv", ":evalenv"] -> do
      let k' = read k
          env' = drop ((k' - 1) * 10) $ take (k' * 10) env
      errs <- evalMainEnvironment env'
      if errs /= [] then liftIO $ putStrLn "" else return ()
      liftIO $ putStrLn $ intercalate "\n" errs
      if errs /= [] then liftIO $ putStrLn "" else return ()
      liftIO $ putStr $ "There was a total of "
      liftIO $ setSGR [SetColor Foreground Vivid Red]
      liftIO $ putStr $ show (length errs)
      liftIO $ setSGR [Reset]
      liftIO $ putStrLn $ " evaluation errors " ++ "on page " ++ k
      return env
    [senv, k] | and (map isDigit k) && elem senv [":showenv", ":showe", ":senv", ":se"] -> do
      let k' = read k
          env' = drop ((k' - 1) * 10) $ take (k' * 10) env
      errs <- showMainEnvironment env'
      if errs /= [] then liftIO $ putStrLn "" else return ()
      liftIO $ putStrLn $ intercalate "\n" errs
      if errs /= [] then liftIO $ putStrLn "" else return ()
      liftIO $ putStr $ "There was a total of "
      liftIO $ setSGR [SetColor Foreground Vivid Red]
      liftIO $ putStr $ show (length errs)
      liftIO $ setSGR [Reset]
      liftIO $ putStrLn $ " display errors " ++ "on page " ++ k
      return env
    [senv] | elem senv [":showenv", ":showe", ":senv", ":se"] -> do
      errs <- showMainEnvironment env
      if errs /= [] then liftIO $ putStrLn "" else return ()
      liftIO $ putStrLn $ intercalate "\n" errs
      if errs /= [] then liftIO $ putStrLn "" else return ()
      liftIO $ putStr $ "There was a total of "
      liftIO $ setSGR [SetColor Foreground Vivid Red]
      liftIO $ putStr $ show (length errs)
      liftIO $ setSGR [Reset]
      liftIO $ putStrLn " display errors in the environment"
      return env
    [cenv] | elem cenv [":clearenv", ":clear", ":cenv", ":ce", ":c"] -> do
      liftIO $ setSGR [SetColor Foreground Vivid Green]
      liftIO $ putStrLn "MainEnvironment cleared"
      liftIO $ setSGR [Reset]
      return []
    [eenv] | elem eenv [":ee", ":eenv", ":evalenv"] -> do
      errs <- evalMainEnvironment env
      if errs /= [] then liftIO $ putStrLn "" else return ()
      liftIO $ putStrLn $ intercalate "\n" errs
      if errs /= [] then liftIO $ putStrLn "" else return ()
      liftIO $ putStr $ "There was a total of "
      liftIO $ setSGR [SetColor Foreground Vivid Red]
      liftIO $ putStr $ show (length errs)
      liftIO $ setSGR [Reset]
      liftIO $ putStrLn " evaluation errors in the environment"
      return env
    [quit] | elem quit [":q", ":quit"] -> do
      liftIO $ setSGR [SetColor Foreground Vivid Yellow]
      liftIO $ putStrLn "Leaving secdi."
      liftIO $ setSGR [Reset]
      return [("", emptySECD)]
    [move, name1, name2] | elem move [":move", ":mv", ":m"] -> do
      let x = lookup name2 env
      if x == Nothing
        then do
          liftIO $ setSGR [SetColor Foreground Vivid Red]
          liftIO $ putStrLn "Variable not in scope"
          liftIO $ setSGR [Reset]
          return env
        else do
          let env' = deleteByFstEnv name1 env
          liftIO $ setSGR [SetColor Foreground Vivid Green]
          liftIO $ putStrLn "Variable moved"
          liftIO $ setSGR [Reset]
          return ((name1, fromMaybe x) : env')
    [avars] | elem avars [":av?", ":allvars"] -> do
      liftIO $ setSGR [SetColor Foreground Vivid Yellow]
      liftIO $ putStrLn $ "All variables in the environment are:"
      liftIO $ setSGR [Reset]
      liftIO $ putStrLn $ (intercalate "\n") $ map fst env
      return env
    [vars] | elem vars [":v?", ":vars"] -> do
      liftIO $ setSGR [SetColor Foreground Vivid Yellow]
      liftIO $ putStrLn $ "The variables for page number 1 (10 vars per page) are:"
      liftIO $ setSGR [Reset]
      liftIO $ putStrLn $ (intercalate "\n") $ (take 10) $ map fst env
      return env
    [vars, k]
      | and (map isDigit k) && elem vars [":v?", ":vars"] ->
          let k' = read k
           in do
                liftIO $ setSGR [SetColor Foreground Vivid Yellow]
                liftIO $ putStrLn $ "The variables for page number " ++ k ++ " (10 vars per page) are:"
                liftIO $ setSGR [Reset]
                liftIO $ putStrLn $ (intercalate "\n") $ (drop (10 * (k' - 1))) $ (take (10 * k')) $ map fst env
                return env
    [load, file] | elem load [":load", ":l"] -> do
      fileExists <- liftIO $ doesFileExist ("programs/" ++ file)
      if fileExists
        then do
          txt <- liftIO $ readFile ("programs/" ++ file)
          let comms = simplyParseCommands' $ words txt
          asts <- getMultipleASTsFromTerms comms
          let env' = foldr deleteByFstEnv env (map fst asts)
          liftIO $ setSGR [SetColor Foreground Vivid Green]
          liftIO $ putStrLn $ "Loaded a total of " ++ (show $ length comms) ++ " environment variables"
          liftIO $ setSGR [Reset]
          return $ asts ++ env'
        else do
          liftIO $ setSGR [SetColor Foreground Vivid Red]
          liftIO $ putStrLn "Path leads to nowhere"
          liftIO $ setSGR [Reset]
          return env
    [var, name] | elem var [":var", ":v", ":assign", ":a"] -> do
      txt <- liftIO getTxtFromInput
      term <- getTermFromAST txt
      case term of
        Left e -> return env
        Right term' -> do
          let env' = deleteByFstEnv name env
          liftIO $ setSGR [SetColor Foreground Vivid Green]
          liftIO $ putStrLn "Variable assigned"
          liftIO $ setSGR [Reset]
          return ((name, term') : env')
    [ev, name] | elem ev [":eval", ":ev", ":e"] -> do
      let x = lookup name env
      if x == Nothing
        then do
          liftIO $ setSGR [SetColor Foreground Vivid Red]
          liftIO $ putStrLn "Variable not in scope"
          liftIO $ setSGR [Reset]
          return env
        else do
          printEval $ fromMaybe x
          return env
    [ev, k, name] | and (map isDigit k) && elem ev [":eval", ":ev", ":e"] -> do
      let x = lookup name env
      if x == Nothing
        then do
          liftIO $ setSGR [SetColor Foreground Vivid Red]
          liftIO $ putStrLn "Variable not in scope"
          liftIO $ setSGR [Reset]
          return env
        else do
          printEvalN (read k) $ fromMaybe x
          return env
    [help] | elem help [":help", ":h", ":?"] -> do
      liftIO $ putStrLn getHelp
      return env
    [s, name] | elem s [":show", ":sh", ":s"] -> do
      let x = lookup name env
      if x == Nothing
        then do
          liftIO $ setSGR [SetColor Foreground Vivid Red]
          liftIO $ putStrLn "Variable not in scope"
          liftIO $ setSGR [Reset]
          return env
        else do
          printTerm $ fromMaybe x
          return env
    [var, name1, ev, name2]
      | elem var [":var", ":v", ":assign", ":a"]
          && elem ev [":eval", ":ev", ":e"] -> do
          let x = lookup name2 env
          if x == Nothing
            then do
              liftIO $ setSGR [SetColor Foreground Vivid Red]
              liftIO $ putStrLn "Variable not in scope"
              liftIO $ setSGR [Reset]
              return env
            else do
              let term = fromMaybe x
              term' <- printEval term
              let env' = deleteByFstEnv name1 env
              case term' of
                Left _  -> return ((name1, emptySECD) : env')
                Right m -> return ((name1, m) : env')
    [var, name1, ev, k, name2]
      | and (map isDigit k)
          && elem var [":var", ":v", ":assign", ":a"]
          && elem ev [":eval", ":ev", ":e"] -> do
          let x = lookup name2 env
          if x == Nothing
            then do
              liftIO $ setSGR [SetColor Foreground Vivid Red]
              liftIO $ putStrLn "Variable not in scope"
              liftIO $ setSGR [Reset]
              return env
            else do
              let term = fromMaybe x
              term' <- printEvalN (read k) term
              let env' = deleteByFstEnv name1 env
              case term' of
                Left _  -> return ((name1, emptySECD) : env')
                Right m -> return ((name1, m) : env')
    ((':' : _) : _) -> do
      liftIO $ setSGR [SetColor Foreground Vivid Red]
      liftIO $ putStrLn "Unknown command"
      liftIO $ setSGR [Reset]
      return env
    ws -> do
      term <- getTermFromAST command
      case term of
        Left e -> return env
        Right term' -> do
          liftIO $ setSGR [SetColor Foreground Vivid Yellow]
          liftIO $ putStrLn "Showing:"
          liftIO $ setSGR [Reset]
          printTerm term'
          liftIO $ putStrLn ""
          liftIO $ setSGR [SetColor Foreground Vivid Yellow]
          liftIO $ putStrLn "Evaluation:"
          liftIO $ setSGR [Reset]
          printEval term'
          return env
  if env' == [("", emptySECD)]
    then return ()
    else do
      let comml' = if (filter (/= ' ') command) == "" then comml else (command : comml)
      liftIO $ appendFile "command_history.txt" (command ++ "\n")
      main' env' comml'

deleteByFstEnv :: String -> MainEnvironment -> MainEnvironment
deleteByFstEnv x xs =
  case break (\(x', _) -> x == x') xs of
    (prev, _ : next) -> prev ++ next
    _                -> xs

showMainEnvironment :: MainEnvironment -> InputT IO [String]
showMainEnvironment [] = return []
showMainEnvironment (e : env) = do
  liftIO $ setSGR [SetColor Foreground Vivid Green]
  liftIO $ putStrLn $ fst e ++ ":"
  liftIO $ setSGR [Reset]
  first <- printTerm $ snd e
  case first of
    Left _ -> do
      rest <- showMainEnvironment env
      return ((fst e ++ " had a display error") : rest)
    Right _ -> do
      rest <- showMainEnvironment env
      return rest

evalMainEnvironment :: MainEnvironment -> InputT IO [String]
evalMainEnvironment [] = return []
evalMainEnvironment (e : env) = do
  liftIO $ setSGR [SetColor Foreground Vivid Green]
  liftIO $ putStrLn $ fst e ++ ":"
  liftIO $ setSGR [Reset]
  first <- printEval $ snd e
  case first of
    Left _ -> do
      rest <- evalMainEnvironment env
      return ((fst e ++ " had an evaluation error") : rest)
    _ -> do
      rest <- evalMainEnvironment env
      return rest

simplyParseCommands' :: [String] -> [(String, String)]
simplyParseCommands' s = map (\(x, y) -> (x, intercalate " " y)) $ simplyParseCommands s

simplyParseCommands :: [String] -> [(String, [String])]
simplyParseCommands [] = []
simplyParseCommands (name : ":=" : xs) = let expr = getExpression xs in (name, fst expr) : (simplyParseCommands $ snd expr)
  where
    getExpression :: [String] -> ([String], [String])
    getExpression [] = ([], [])
    getExpression s@(name : ":=" : xs) = ([], s)
    getExpression (x : xs) = let next = getExpression xs in (x : (fst next), snd next)
simplyParseCommands xs = []

handleCommHistFile :: FilePath -> IO String
handleCommHistFile file = do
  fileExists <- doesFileExist file
  if fileExists
    then readFile file
    else do
      writeFile file ""
      return ""

getTxtFromFile :: FilePath -> InputT IO String
getTxtFromFile file = do
  txt <- liftIO $ readFile file
  return txt

getTxtFromInput :: IO String
getTxtFromInput = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  putStrLn "Press Ctrl-D to submit the program"
  txt <- readUntil' '\4'
  putStrLn ""
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetEcho stdin True
  return txt

updateNextLines :: [String] -> Int -> IO ()
updateNextLines [x] n = do
  putStr "\r"
  putStr $ replicate (n + 1) ' '
  putStr "\r"
  putStr x
  putStr "\r"
updateNextLines (x : xs) n = do
  putStr "\r"
  putStr $ replicate (n + 1) ' '
  putStr "\r"
  putStr x
  putStr "\r"
  putStr $ "\ESC[1A"
  updateNextLines xs $ length x

readUntil' :: Char -> IO String
readUntil' finalChar = do
  (leftRight, prevLines, nextLines) <- readUntil finalChar ([], []) [] [] 0 0
  putStrLn ""
  return $ concat $ (reverse $ prevLines) ++ leftRight ++ nextLines

readUntil :: Char -> (String, String) -> [String] -> [String] -> Int -> Int -> IO ([String], [String], [String])
readUntil finalChar (left, right) prevLines nextLines currH maxH = do
  currChar <- getChar
  case currChar of
    '\n' -> do
      let lenNextLines = length nextLines
      if (lenNextLines > 0)
        then putStr $ "\ESC[" ++ show lenNextLines ++ "B" ++ ""
        else return ()
      putStr "\n"
      updateNextLines (reverse (right : nextLines)) 0
      putStr "\ESC[1A"
      putStr "\r"
      putStr $ replicate (length left + length right) ' '
      putStr "\r"
      putStr left
      putStr "\r"
      putStr "\ESC[1B"
      readUntil finalChar ([], right) (left : prevLines) nextLines (currH + 1) (maxH + 1)
    '\DEL' -> case reverse left of
      (_ : ls) -> do
        let left' = reverse ls
            lenLeft = length left
            lenRight = length right
        if right == ""
          then putStr $ "\ESC[1D \ESC[1D" ++ replicate lenRight ' '
          else do
            putStr "\r"
            putStr $ replicate (lenLeft + lenRight) ' '
            putStr "\r"
            putStr $ left' ++ right
            putStr $ "\ESC[" ++ show lenLeft ++ "G"
        readUntil finalChar (left', right) prevLines nextLines currH maxH
      [] -> readUntil finalChar (left, right) prevLines nextLines currH maxH
    '\ESC' -> do
      n1 <- getChar
      n2 <- getChar
      case n2 of
        'D' -> do
          putStr "\r"
          putStr $ replicate (length right + length left + 10) ' '
          putStr "\r"
          putStr $ left ++ right
          putStr $ replicate (length right) '\b'
          case reverse left of
            (l : ls) -> do
              putStr "\ESC[1D"
              readUntil finalChar (reverse ls, l : right) prevLines nextLines currH maxH
            [] ->
              if currH > 0
                then do
                  let left' = getHead prevLines
                  putStr "\ESC[1A"
                  putStr "\r"
                  putStr $ left'
                  readUntil finalChar (left', []) (getTail prevLines) ((left ++ right) : nextLines) (currH - 1) maxH
                else readUntil finalChar (left, right) prevLines nextLines currH maxH
        'C' -> do
          putStr "\r"
          putStr $ replicate (length right + length left + 10) ' '
          putStr "\r"
          putStr $ left ++ right
          putStr $ replicate (length right) '\b'
          case right of
            (r : rs) -> do
              putChar r
              readUntil finalChar (left ++ [r], rs) prevLines nextLines currH maxH
            [] ->
              if currH < maxH
                then do
                  let right' = getHead nextLines
                  putStr "\ESC[1B"
                  putStr "\r"
                  putStr $ right'
                  putStr "\r"
                  readUntil finalChar ([], right') ((left ++ right) : prevLines) (getTail nextLines) (currH + 1) maxH
                else readUntil finalChar (left, right) prevLines nextLines currH maxH
        'A' -> do
          if currH == 0
            then do
              putStr "\r"
              readUntil finalChar ([], left ++ right) prevLines nextLines currH maxH
            else do
              putStr "\ESC[1A"
              let left' = take (length left) $ getHead prevLines
                  right' = drop (length left) $ getHead prevLines
              putStr "\r"
              putStr $ replicate (length right' + length left' + 10) ' '
              putStr "\r"
              putStr $ left' ++ right'
              putStr $ "\ESC[" ++ show (length left' + 1) ++ "G"
              readUntil finalChar (left', right') (getTail prevLines) ((left ++ right) : nextLines) (currH - 1) maxH
        'B' -> do
          if currH == maxH
            then do
              putStr "\r"
              putStr $ left ++ right
              readUntil finalChar (left ++ right, []) prevLines nextLines currH maxH
            else do
              putStr "\ESC[1B"
              let left' = take (length left) $ getHead nextLines
                  right' = drop (length left) $ getHead nextLines
              putStr "\r"
              putStr $ replicate (length right' + length left' + 10) ' '
              putStr "\r"
              putStr $ left' ++ right'
              putStr $ "\ESC[" ++ show (length left' + 1) ++ "G"
              readUntil finalChar (left', right') ((left ++ right) : prevLines) (getTail nextLines) (currH + 1) maxH
        key | key /= '\^C' -> readUntil finalChar (left, right) prevLines nextLines currH maxH
    _ -> do
      if currChar == finalChar
        then return $ ([left ++ right], prevLines, nextLines)
        else do
          putStr (currChar : right)
          putStr $ replicate (length right) '\b'
          readUntil finalChar (left ++ [currChar], right) prevLines nextLines currH maxH
  where
    getHead = \ls -> case ls of [] -> []; (x : xs) -> x
    getTail = \ls -> case ls of [] -> []; (x : xs) -> xs

getTokens :: String -> InputT IO [Token]
getTokens txt = return $ alexScanTokens txt

getAST :: String -> InputT IO (Either String TermNode)
getAST txt = do
  tok <- getTokens txt
  let tokErr = filter (\x -> case x of Token _ (ERROR e) -> True; _ -> False) tok
  case tokErr of
    (x : xs) -> return $ Left $ (\(Token fi (ERROR e)) -> e ++ showFileInfo fi) $ x
    [] -> return $ parser tok

getTermFromAST :: String -> InputT IO (Either String SECD)
getTermFromAST txt = do
  ast <- getAST txt
  case ast of
    Left e -> do
      liftIO $ setSGR [SetColor Foreground Vivid Red]
      liftIO $ putStrLn e
      liftIO $ setSGR [Reset]
      return (Left "")
    Right (TermNode _ (TmError e)) -> do
      liftIO $ setSGR [SetColor Foreground Vivid Red]
      liftIO $ putStrLn e
      liftIO $ setSGR [Reset]
      return (Left "")
    Right t ->
      case findFreeVars [] t of
        Just xs -> do
          liftIO $ setSGR [SetColor Foreground Vivid Red]
          liftIO $ putStrLn ("Free variables found in the given term:\n" ++ show xs)
          liftIO $ setSGR [Reset]
          return (Left "")
        Nothing -> return (Right $ createSECD t)

getMultipleASTsFromTerms :: [(String, String)] -> InputT IO [(String, SECD)]
getMultipleASTsFromTerms [] = return []
getMultipleASTsFromTerms (x : xs) = do
  term <- getTermFromAST $ snd x
  next <- getMultipleASTsFromTerms xs
  case term of
    Left e -> do
      liftIO $ putStr "for environment variable: "
      liftIO $ setSGR [SetColor Foreground Vivid Red]
      liftIO $ putStrLn $ fst x
      liftIO $ setSGR [Reset]
      return next
    Right term' -> return ((fst x, term') : next)

printEval :: SECD -> InputT IO (Either String SECD)
printEval ast = do
  ast' <-
    if isDone ast
      then do
        liftIO $ putStrLn "The given term is already a value"
        return $ Right ast
      else do
        case eval' ast of
          (_, Left e) -> do
            liftIO $ putStrLn e
            liftIO $ setSGR [SetColor Foreground Vivid Red]
            liftIO $ putStrLn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
            liftIO $ setSGR [Reset]
            return (Left "")
          (n', Right m) -> do
            liftIO $ setSGR [SetColor Foreground Vivid Green]
            liftIO $ putStrLn $ "The given term evaluated a total of " ++ (show n') ++ " times: "
            liftIO $ setSGR [Reset]
            printTerm m
            return (Right m)
  return ast'

printEvalN :: Counter -> SECD -> InputT IO (Either String SECD)
printEvalN n ast = do
  ast' <-
    if isDone ast
      then do
        liftIO $ putStrLn "The given term is already a value"
        return (Right ast)
      else do
        case evalN' n ast of
          (_, Left e) -> do
            liftIO $ putStrLn e
            liftIO $ setSGR [SetColor Foreground Vivid Red]
            liftIO $ putStrLn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
            liftIO $ setSGR [Reset]
            return (Left "")
          (n', Right m) -> do
            liftIO $ setSGR [SetColor Foreground Vivid Green]
            liftIO $ putStrLn $ "The given term evaluated a total of " ++ (show (n - n')) ++ " times: "
            liftIO $ setSGR [Reset]
            printTerm m
            return (Right m)
  return ast'

printTerm :: SECD -> InputT IO (Either String String)
printTerm m = do
  case m of
    (ValueStack v EmptyStack, [], EmptyControl, EmptyDump) -> do
      liftIO $ putStrLn $ showTerm' v
      return $ Right ""
    (ClosureStack x t1 e' EmptyStack, [], EmptyControl, EmptyDump) -> do
      liftIO $ putStrLn $ showTerm'' e' (TermNode noPos (TmAbs x t1))
      return $ Right ""
    (EmptyStack, [], TermControl t EmptyControl, EmptyDump) -> do
      liftIO $ putStrLn $ showTerm' t
      return $ Right ""
    _ -> do
      printSECD (Right m)
      return $ Right ""

printSECD :: Either String SECD -> InputT IO (Either String String)
printSECD m = do
  case m of
    Left e -> do
      liftIO $ putStrLn e
      liftIO $ setSGR [SetColor Foreground Vivid Red]
      liftIO $ putStrLn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
      liftIO $ setSGR [Reset]
      return $ Left ""
    Right m' -> do
      liftIO $ putStrLn $ showSECD m'
      return $ Right ""
