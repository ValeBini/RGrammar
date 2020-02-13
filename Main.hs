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
        state' <- compileFiles (prelude:args) state 
        putStrLn ("Intérprete de " ++ iname ++ ".\n" ++
                  "Escriba :? para recibir ayuda.")
        --  enter loop
        rec state' 

  data Command = Compile String
               | Print String
              -- | Recompile  
               | Browse
               | Quit
               | Help
               | Noop
               | FindType String

  
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
         return (Compile x)

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
         Compile c  ->  do  state' <- compileFile state c
                            return (Just state')
         Print s    ->  let g = lookfor s state
                          in case g of
                              Nothing -> putStr "La gramática no esta cargada\n" >> return (Just state)
                              Just g' -> printGrammar g' >> return (Just state)
         {-}
         Recompile  -> if null lfile 
                        then putStrLn "No hay un archivo cargado.\n" >> 
                             return (Just state) 
                        else handleCommand state (Compile (CompileFile lfile))
         -}
               

  data InteractiveCommand = Cmd [String] String (String -> Command) String

  commands :: [InteractiveCommand]
  commands
    =  [ Cmd [":browse"]      ""        (const Browse) "Ver los nombres en scope",
         Cmd [":load"]        "<file>"  (Compile)      "Cargar una gramática desde un archivo",
         Cmd [":print"]       "<gram>"  Print "Imprime una gramática", --preguntar
         --Cmd [":reload"]      "<file>"  (const Recompile) "Volver a cargar el último archivo",
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
  compileFiles (x:xs) s  = do s' <- compileFile s x
                              compileFiles xs s'

  compileFile :: St -> String -> IO St
  compileFile state@(S {..}) f = do
      putStrLn ("Abriendo "++f++"...")
      let f'= reverse(dropWhile isSpace (reverse f))
      x     <- catch (readFile f')
                 (\e -> do let err = show (e :: IOException)
                           hPutStr stderr ("No se pudo abrir el archivo " ++ f' ++ ": " ++ err ++"\n")
                           return "")
      gram <- parseIO f' (gram_parse) x
      maybe (return state) (addGram state f) gram --revisar

{-}  
  compilePhrase :: St -> String -> IO St
  compilePhrase state x =
    do
      x' <- parseIO "<interactive>" stmt_parse x
      maybe (return state) (handleStmt state) x'
-}

{-}
  printPhrase   :: String -> IO ()
  printPhrase x =
    do
      x' <- parseIO "<interactive>" stmt_parse x
      maybe (return ()) (printStmt . fmap (\y -> (y, conversion y)) ) x'
-}

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


{-}
  handleStmt :: St -> Stmt LamTerm -> IO St
  handleStmt state stmt =
    do
      case stmt of
          Def x e    -> checkType x (conversion e)
          Eval e     -> checkType it (conversion e)
    where
      checkType i t = do
        case infer (ve state) t of
          Left err -> putStrLn ("Error de tipos: " ++ err) >> return state
          Right ty -> checkEval i t ty
      checkEval i t ty = do
         let v = eval (ve state) t
         _ <- when (inter state) $ do
                  let outtext = if i == it then render (printTerm (quote v))
                                           else render (text i)
                  putStrLn outtext
         return (state { ve = (Global i, (v, ty)) : ve state})
-}

  addGram :: St -> String -> GramTerm -> IO St
  addGram state name gram =
    do let gram' = gramTermToGram gram
        in return (S ((name, gram'):(env state)))

  prelude :: String
  prelude = "Prelude.lam"

  it :: String          
  it = "it"
 
 
