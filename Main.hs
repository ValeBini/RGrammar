{-# OPTIONS -XRecordWildCards #-}
module Main where

  import Control.Exception (catch,IOException)
  import Control.Monad.Except
--  import Control.Monad.Error 
  import Data.Char
  import Data.List
  import Data.Maybe
  import Prelude hiding (print)
  import System.Console.Readline
  import System.Environment
  import System.IO hiding (print)
  import Text.PrettyPrint.HughesPJ (render,text)

  import Common
  import Grammar
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


  data St = S { env :: NameEnv  -- Entorno con las gramáticas cargadas
                 }

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
        state' <- compileFiles args state 
        putStrLn ("Intérprete de " ++ iname ++ ".\n" ++
                  "Escriba :? para recibir ayuda.")
        --  enter loop
        rec state' 

  data Command = Compile String
               | Print String
               | Recompile String 
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

  getName :: String -> Maybe String
  getName s = let r = reverse s
              in if isPrefixOf "marg." r then (Just (reverse (drop 5 r)))
                                         else Nothing

  lookfor :: String -> St -> Maybe Gram
  lookfor s (S []) = Nothing 
  lookfor s (S ((name,gram):xs)) = if name == s then Just gram
                                                else lookfor s (S xs)

  handleCommand :: St -> Command -> IO (Maybe St)
  handleCommand state@(S {..}) cmd
    =  case cmd of
         Quit       ->  return Nothing
         Noop       ->  return (Just state)
         Help       ->  putStr (helpTxt commands) >> return (Just state)
         Browse     ->  do  putStr (unlines [ s | s <- reverse (nub (map fst env)) ])
                            return (Just state)
         Compile c  ->  let name = getName c
                        in case name of
                          Nothing -> putStrLn "El nombre del archivo no es valido" >> return (Just state)
                          Just s -> let g = lookfor s state
                                    in case g of
                                         Nothing -> do state' <- compileFile state c s
                                                       return (Just state')
                                         Just g' -> putStrLn "La gramatica ya existe" >> return (Just state)
         Print s    ->  let g = lookfor s state
                          in case g of
                              Nothing -> putStr "La gramática no esta cargada\n" >> return (Just state)
                              Just g' -> printGrammar g' >> return (Just state)
         Recompile c  -> let name = getName c
                         in case name of
                             Nothing -> putStrLn "El nombre del archivo no es valido" >> return (Just state)
                             Just s -> do state' <- recompileFile state c s
                                          return (Just state') 
         InteractiveStmt s -> compilePhrase state s
               

  data InteractiveCommand = Cmd [String] String (String -> Command) String

  commands :: [InteractiveCommand]
  commands
    =  [ Cmd [":browse"]      ""        (const Browse) "Ver los nombres en scope",
         Cmd [":load"]        "<file>"  (Compile)      "Cargar una gramática desde un archivo",
         Cmd [":print"]       "<gram>"  Print          "Imprime una gramática",    
         Cmd [":reload"]      "<file>"  (Recompile)    "Volver a cargar un archivo. Si no estaba cargado, cargarlo",
         Cmd [":quit"]        ""        (const Quit)   "Salir del intérprete",
         Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos" ]
  
  helpTxt :: [InteractiveCommand] -> String
  helpTxt cs
    =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
       "c es el primer caracter del nombre completo.\n\n" ++
       unlines (map (\ (Cmd c a _ d) -> 
                     let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                     in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)


  compileFiles :: [String] -> St -> IO St
  compileFiles [] s      = return s
  compileFiles (x:xs) s  = let name = getName x
                           in case name of
                                Nothing -> putStrLn ("El nombre del archivo " ++ x ++ " no es valido") >> compileFiles xs s
                                Just n -> do s' <- compileFile s x n
                                             compileFiles xs s'


  compileFile :: St -> String -> String -> IO St
  compileFile state@(S {..}) f name = do
      putStrLn ("Abriendo "++f++"...")
      let f'= reverse(dropWhile isSpace (reverse f))
      x     <- catch (readFile f')
                 (\e -> do let err = show (e :: IOException)
                           hPutStr stderr ("No se pudo abrir el archivo " ++ f' ++ ": " ++ err ++"\n")
                           return "")
      gram <- parseIO f' (gram_parse) x
      maybe (return state) (addGram state name) gram 

  recompileFile :: St -> String -> String -> IO St
  recompileFile state@(S {..}) f name = do
      putStrLn ("Abriendo "++f++"...")
      let f'= reverse(dropWhile isSpace (reverse f))
      x     <- catch (readFile f')
                 (\e -> do let err = show (e :: IOException)
                           hPutStr stderr ("No se pudo abrir el archivo " ++ f' ++ ": " ++ err ++"\n")
                           return "")
      gram <- parseIO f' (gram_parse) x
      maybe (return state) (changeGram state name) gram

  printGrammar ::  Gram -> IO ()
  printGrammar gram =  
    do
      let outtext = printGram gram
      putStrLn (render outtext)

  parseIO :: String -> (String -> ParseResult a) -> String -> IO (Maybe a)
  parseIO f p x = case p x of
                       Failed e  -> do putStrLn (f++": "++e) 
                                       return Nothing
                       Ok r      -> return (Just r)



  addGram :: St -> String -> GramTerm -> IO St
  addGram state name gram =
    do let gram' = gramTermToGram gram
        in return (S ((name, gram'):(env state)))

  changeGram :: St -> String -> GramTerm -> IO St
  changeGram state name gram =
    do let gram' = gramTermToGram gram
        in return (S (replace name gram' (env state)))

  replace :: String -> Gram -> NameEnv -> NameEnv
  replace name gram [] = [(name, gram)]
  replace name gram ((n,g):xs) = if name==n then ((n,gram):xs)
                                            else ((n,g):(replace name gram xs))

  compilePhrase :: St -> String -> IO St
  compilePhrase s x = do x' <- parseIO "<interactive>" stmt_parse x
                         maybe (return state) (handleStmt state) x'

  

  prelude :: String
  prelude = "Prelude.lam"

  it :: String          
  it = "it"
 
 
