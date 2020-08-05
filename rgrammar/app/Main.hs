{-# OPTIONS -XRecordWildCards #-}
module Main where

  import Control.Exception (catch,IOException)
  import Data.Char
  import Data.List
  import Data.Maybe
  import Prelude hiding (print)
  import System.Console.Readline (readline,addHistory)
  import System.Environment
  import System.IO hiding (print)
  import Text.PrettyPrint.HughesPJ (render)

  import Common
  import Grammar
  import Eval
  import Parse
  import PrettyPrinter
  
---------------------
--- Interpreter
---------------------

  main :: IO ()
  main = do args <- getArgs 
            readevalprint args (S [])

  ioExceptionCatcher :: IOException -> IO (Maybe a)
  ioExceptionCatcher _ = return Nothing

  iname, iprompt :: String
  iname = "regular grammar"
  iprompt = "RG> "


  data St = S { env :: Env } -- Entorno con las gramáticas cargadas, se guardan como DFAs
                 

  --  read-eval-print loop
  readevalprint :: [String] -> St -> IO ()
  readevalprint args state@(S {..}) =
    let rec st =
          do
            mx <- catch
                   (readline iprompt)
                    ioExceptionCatcher
            case mx of
              Nothing   ->  return ()
              Just ""   ->  rec st
              Just x    ->
                do
                  addHistory x
                  c   <- interpretCommand x
                  st' <- handleCommand st c
                  maybe (return ()) rec st'
    in
      do
        putStrLn ("Intérprete de " ++ iname ++ ".\n" ++
                  "Escriba :? para recibir ayuda.")
        rec state 

  data Command = RCompile String
               | LCompile String
               | RPrint String
               | LPrint String
               | Browse
               | Quit
               | Help
               | Noop
               | InteractiveStmt String

  
  interpretCommand :: String -> IO Command
  interpretCommand x
    =  if isPrefixOf ":" x then
         do  let  (cmd,t')  =  break isSpace x
                  t         =  dropWhile isSpace t'
             --  find matching commands
             let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
             case matching of
               []  ->  do  putStrLn ("Comando desconocido `" ++ 
                                     cmd                     ++ 
                                     "'. Escriba :? para recibir ayuda.")
                           return Noop
               [Cmd _ _ f _]
                   ->  do  return (f t)
               _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++ 
                                     concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                           return Noop
                                                        
       else
         return (InteractiveStmt x)

-- toma un string (nombre del archivo), si es del tipo "x.gram" devuelve (Just x), sino Nothing
  getName :: String -> Maybe String
  getName s = let r = reverse s
              in if isPrefixOf "marg." r then (Just (reverse (drop 5 r)))
                                         else Nothing

-- toma un string y devuelve el dfa asociado a ese nombre
  lookforDFA :: Name -> Env -> Maybe GDFA 
  lookforDFA n [] = Nothing
  lookforDFA n ((name, dfa):xs) | n == name = Just dfa
                                | n /= name = lookforDFA n xs

  handleCommand :: St -> Command -> IO (Maybe St)
  handleCommand state@(S {..}) cmd
    =  case cmd of
         Quit       ->  return Nothing
         Noop       ->  return (Just state)
         Help       ->  putStr (helpTxt commands) >> return (Just state)
         Browse     ->  do  putStr (unlines [ s | s <- reverse (nub (map fst env)) ])
                            return (Just state)
         RCompile c  ->  let name = getName c
                         in case name of
                           Nothing -> putStrLn "El nombre del archivo no es valido" >> return (Just state)
                           Just s -> do state' <- compileFile state c s True
                                        return (Just state')
         LCompile c  ->  let name = getName c
                         in case name of
                           Nothing -> putStrLn "El nombre del archivo no es valido" >> return (Just state)
                           Just s -> do state' <- compileFile state c s False
                                        return (Just state')
         RPrint s    ->  let g = lookforDFA s env
                         in case g of
                              Nothing -> putStr "La gramática no esta cargada\n" >> return (Just state)
                              Just g' -> printGrammar g' True >> return (Just state)
         LPrint s    ->  let g = lookforDFA s env
                         in case g of
                              Nothing -> putStr "La gramática no esta cargada\n" >> return (Just state)
                              Just g' -> printGrammar g' False >> return (Just state)
         InteractiveStmt s -> do state' <- compilePhrase state s
                                 return (Just state')
               

  data InteractiveCommand = Cmd [String] String (String -> Command) String

  commands :: [InteractiveCommand]
  commands
    =  [ Cmd [":browse"]      ""        (const Browse) "Ver los nombres en scope",
         Cmd [":rload"]       "<file>"  (RCompile)     "Cargar una gramática derecha desde un archivo. Si ya existe, lo reemplaza",
         Cmd [":lload"]       "<file>"  (LCompile)     "Cargar una gramática izquierda desde un archivo. Si ya existe, lo reemplaza",
         Cmd [":rprint"]      "<gram>"  (RPrint)       "Imprime una gramática derecha",
         Cmd [":lprint"]      "<gram>"  (LPrint)       "Imprime una gramática izquierda",
         Cmd [":quit"]        ""        (const Quit)   "Salir del intérprete",
         Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos" ]
  
  helpTxt :: [InteractiveCommand] -> String
  helpTxt cs
    =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
       "c es el primer caracter del nombre completo.\n\n" ++
       unlines (map (\ (Cmd c a _ d) -> 
                     let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                     in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)


  compileFile :: St -> String -> String -> Bool -> IO St
  compileFile state@(S {..}) f name right = do
      putStrLn ("Abriendo "++f++"...")
      let f'= reverse(dropWhile isSpace (reverse f))
      x     <- catch (readFile f')
                 (\e -> do let err = show (e :: IOException)
                           hPutStr stderr ("No se pudo abrir el archivo " ++ f' ++ ": " ++ err ++"\n")
                           return "")
      gram <- do if right then do rg <- parseIO f' (rgram_parse) x
                                  return (maybe Nothing (\x -> Just (Right x)) rg)
                 else do lg <- parseIO f' (lgram_parse) x
                         return (maybe Nothing (\x -> Just (Left x)) lg) 
      maybe (return state) (addGram state name) gram



  printGrammar ::  GDFA -> Bool -> IO ()
  printGrammar dfa right =  
    do
      let outtext = if right then printGram (dfaToRGram dfa)
                             else printGram (dfaToLGram dfa)
      putStrLn (render outtext)

  parseIO :: String -> (String -> ParseResult a) -> String -> IO (Maybe a)
  parseIO f p x = case p x of
                       Failed e  -> do putStrLn (f++": "++e) 
                                       return Nothing
                       Ok r      -> return (Just r)

  addGram :: St -> String -> GramTerm -> IO St
  addGram state name gram =
    do let gram' = gramTermToDFA gram
        in return (S (replace name gram' (env state)))

  replace :: String -> GDFA -> Env -> Env
  replace name gram [] = [(name, gram)]
  replace name gram ((n,g):xs) = if name==n then ((n,gram):xs)
                                            else ((n,g):(replace name gram xs))

  compilePhrase :: St -> String -> IO St
  compilePhrase state x = do stmt <- parseIO "<interactive>" stmt_parse x
                             maybe (return state) (handleStmt state) stmt

  handleStmt :: St -> Stmt -> IO St
  handleStmt state stmt = do case stmt of 
                                SDef n g -> addDef n g
                                SEq g0 g1 -> evalIfEq 
      where 
        addDef n g = let res = eval (env state) g
                     in case res of
                          Left e -> putStrLn e >> return state
                          Right dfa -> return (S (replace n dfa (env state)))
        evalIfEq = let res = checkEq (env state) stmt
                   in case res of 
                         Left e -> putStrLn e >> return state
                         Right b -> putStrLn (show b) >> return state 

 
