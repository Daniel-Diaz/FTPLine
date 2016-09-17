
{-# LANGUAGE FlexibleInstances, CPP #-}

module Main (main) where

import Data.Maybe
import Data.Char (ord, chr)
import qualified Data.Strict.Maybe as Strict
import Control.Monad.State.Strict
import System.Console.Haskeline hiding (getInputLine)
import qualified System.Console.Haskeline as Haskeline
       (getInputLine)
import Network.Socket hiding (connect)
import Network.FTP.Client
import Network.FTP.Client.Parser
import System.Directory
import Control.Applicative ((<$>))
import Data.List (intercalate,isPrefixOf)
import System.Console.ANSI
import System.IO
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Paths_FTPLine (version)
import Data.Version (showVersion)
 
ftperror :: String -> FTPLine a
ftperror = liftIO . fail
 
getInputLine :: String -> FTPLine String
getInputLine str
  = do withColor Yellow $ outputStr str
       ms <- Haskeline.getInputLine "\n"
       if ms == Nothing then
         do withColor Green $ outputStrLn "Switching local/remote..."
            return "switch"
         else return $ fromJust ms
 
getInputLine_ :: String -> String -> FTPLine ()
getInputLine_ str com
  = do withColor Yellow $ outputStr str
       outputStrLn $ processInteractiveOutput com
 
processInteractiveOutput :: String -> String
processInteractiveOutput str
  | "login" `isPrefixOf` str
              = let ws = words str
                in  if length ws == 3
                       then unwords $ take 2 ws ++ [fmap (\_ -> '*') (ws !! 2)]
                       else str
  | otherwise = str

data File = File FilePath !B.ByteString
 
type SMaybe = Strict.Maybe
 
data FTPState = FTPState{conn :: Maybe FTPConnection,
                         file :: !(SMaybe File), host :: Maybe String, mode :: Bool,
                         interactive :: Bool}
 
boolToMode :: Bool -> String
boolToMode b = if b then "Remote" else "Local"
 
initialFTPState :: FTPState
initialFTPState
  = FTPState{conn = Nothing, file = Strict.Nothing, host = Nothing,
             mode = False, interactive = False}
 
type FTPLine a = InputT (StateT FTPState IO) a
 
getting ::
          (MonadState st m, MonadTrans t, Functor (t m)) =>
          (st -> x) -> t m x
getting f = f <$> lift get
 
modif :: (MonadTrans t, MonadState s m) => (s -> s) -> t m ()
modif = lift . modify
 
getConnection :: FTPLine (Maybe FTPConnection)
getConnection = getting conn
 
newConnection :: String -> FTPConnection -> FTPLine ()
newConnection hn c
  = modif $
      \ (FTPState _ mf _ b i) -> FTPState (Just c) mf (Just hn) b i
 
removeConnection :: FTPLine ()
removeConnection
  = modif $
      \ (FTPState _ mf _ b i) -> FTPState Nothing mf Nothing b i
 
getFile :: FTPLine (SMaybe File)
getFile = getting file
 
newFile :: File -> FTPLine ()
newFile f
  = modif $!
      \ (FTPState mc _ md b i) -> FTPState mc (Strict.Just f) md b i
 
cleanFile :: FTPLine ()
cleanFile
  = modif $!
      \ (FTPState mc _ md b i) -> FTPState mc Strict.Nothing md b i
 
getHost :: FTPLine String
getHost
  = return "Local" <-> const (getting $ maybe "Local" id . host)
 
getMode :: FTPLine Bool
getMode = getting mode
 
turnMode :: FTPLine ()
turnMode = modif $ \ s -> s{mode = not $ mode s}
 
isInteractive :: FTPLine Bool
isInteractive = getting interactive
 
setInteractive :: Bool -> FTPLine ()
setInteractive b = modif $ \ s -> s{interactive = b}
 
connect :: HostName -> PortNumber -> FTPLine FTPResult
connect hn pn
  = do mc <- getConnection
       if isNothing mc then return () else
         (liftIO $ quit $ fromJust mc) >> return ()
       (c, r) <- liftIO $ connectFTP hn pn
       newConnection hn c
       saveConnection hn pn
       return r
 
saveConnection :: HostName -> PortNumber -> FTPLine ()
saveConnection hn pn
  = do appDir <- liftIO ftpLineDir
       liftIO $ writeFile (appDir ++ "/LastConnection") $ show (hn, pn)
 
withConnection :: (FTPConnection -> FTPLine a) -> FTPLine a
withConnection f
  = do mc <- getConnection
       if isNothing mc then ftperror "Connection not established." else
         f $ fromJust mc
 
saveLogin :: String -> String -> FTPLine ()
saveLogin name pass
  = do appDir <- liftIO ftpLineDir
       liftIO $ writeFile (appDir ++ "/LastLogin") $ show (name, pass)
 
ftplogin :: Maybe (String, String) -> FTPLine FTPResult
ftplogin Nothing = withConnection $ liftIO . loginAnon
ftplogin (Just (name, pass))
  = do r <- withConnection $
              \ c -> liftIO $ login c name (Just pass) Nothing
       saveLogin name pass
       return r
 
lastConn :: FTPLine ()
lastConn
  = hand
      (const $
         withColor Red $ outputStrLn "Last connection doesn't exist.")
      $
      do appDir <- liftIO ftpLineDir
         lc <- liftIO $ readFile $ appDir ++ "/LastConnection"
         _ <- (liftIO $ readIO lc) >>= uncurry connect
         outputStrLn "Last connection reestablished."
         hand
           (const $ withColor Red $ outputStrLn "Last login doesn't exist.")
           $
           do ll <- liftIO $ readFile $ appDir ++ "/LastLogin"
              _ <- (liftIO $ readIO ll) >>= ftplogin . Just
              outputStrLn "Last login reestablished."
 
disconnect :: FTPLine FTPResult
disconnect
  = withConnection $
      \ c ->
        do removeConnection
           liftIO $ quit c
 
ftpdebug :: FTPLine ()
ftpdebug = liftIO enableFTPDebugging
 
success :: FTPResult
success = (1, ["Success"])
 
failure :: FTPResult
failure = (0, ["Failure"])
 
showFTPResult :: FTPResult -> String
showFTPResult (n, xs) = unwords $ [show n, ":"] ++ xs
 
outputFTPResult :: FTPResult -> FTPLine ()
outputFTPResult = withColor Magenta . outputStrLn . showFTPResult
 
discardConnection :: FTPLine Bool
discardConnection
  = do mc <- getConnection
       b <- getMode
       return $ not b || isNothing mc
 
(<->) :: FTPLine a -> (FTPConnection -> FTPLine a) -> FTPLine a
la <-> ra
  = do b <- discardConnection
       if b then la else
         do mc <- getConnection
            ra $ fromJust mc
 
localdir :: Maybe String -> FTPLine ()
localdir fp
  = if isNothing fp then
      liftIO (getDirectoryContents ".") >>= mapM_ outputStrLn . reverse
      else
      liftIO (getDirectoryContents $ fromJust fp) >>= mapM_ outputStrLn
 
pdir :: Maybe String -> FTPLine ()
pdir fp
  = localdir fp <-> \ c -> liftIO (dir c fp) >>= mapM_ outputStrLn
 
getfile :: String -> FTPLine FTPResult
getfile fp
  = (do x <- liftIO $ B.readFile fp
        newFile $ File fp x
        return success)
      <->
      \ c ->
        do (x, r) <- liftIO $ getbinary c fp
           newFile $ File fp $ B.pack $ fmap (fromIntegral . ord) x
           return r
 
download :: String -> FTPLine FTPResult
download fp = withConnection $ \ c -> liftIO $ downloadbinary c fp
 
putfile :: String -> FTPLine FTPResult
putfile fp
  = (do x <- getFile
        if Strict.isNothing x then ftperror "File memory empty." else
          let (File _ cnt) = Strict.fromJust x in
            do liftIO $ B.writeFile fp cnt
               return success)
      <->
      \ c ->
        do x <- getFile
           if Strict.isNothing x then ftperror "File memory empty." else
             let (File _ cnt) = Strict.fromJust x in
               liftIO $ putbinary c fp $ fmap (chr . fromIntegral) $ B.unpack cnt
 
upload :: String -> FTPLine FTPResult
upload fp = withConnection $ \ c -> liftIO $ uploadbinary c fp
 
renamefile :: String -> String -> FTPLine FTPResult
renamefile fp1 fp2
  = (do liftIO $ renameFile fp1 fp2
        return success)
      <-> \ c -> liftIO $ rename c fp1 fp2
 
deletefile :: String -> FTPLine FTPResult
deletefile fp
  = (do liftIO $ removeFile fp
        return success)
      <-> \ c -> liftIO $ delete c fp
 
sizefile :: String -> FTPLine FTPResult
sizefile fp
  = do n <- (liftIO $ B.length <$> B.readFile fp) <->
              \ c -> liftIO $ size c fp
       outputStrLn $ show n
       return success
 
ccd :: String -> FTPLine FTPResult
ccd fp
  = (do liftIO $ setCurrentDirectory fp
        return success)
      <-> \ c -> liftIO $ cwd c fp
 
newdir :: String -> FTPLine FTPResult
newdir fp
  = (do liftIO $ createDirectory fp
        return success)
      <-> \ c -> liftIO $ snd <$> mkdir c fp
 
remdir :: String -> FTPLine FTPResult
remdir fp
  = (do liftIO $ removeDirectory fp
        return success)
      <-> \ c -> liftIO $ rmdir c fp
 
curdir :: FTPLine (Maybe String, FTPResult)
curdir
  = (do x <- liftIO getCurrentDirectory
        return (Just x, success))
      <-> (liftIO . pwd)
 
class (Read a) => Arg a where
   parse :: String -> FTPLine a
   parse = liftIO . readIO
 
output :: (Show a) => a -> FTPLine ()
output = outputStrLn . show
 
command :: String -> FTPLine ()
command str
  = if null xs then outputStrLn "No command introduced." else
      withColor Green $ runCom (head xs) (tail xs)
  where xs = separgs str
 
printHelp :: String -> FTPLine ()
printHelp str
  = withColor Cyan $
      mapM_
        (\ ln ->
           if head ln == '*' then outputStrLn ln else
             italized $ outputStrLn ln)
        $ commandDesc str
 
hand :: (MonadException m) => (IOException -> m a) -> m a -> m a
hand = handle
 
handres :: FTPLine FTPResult -> FTPLine ()
handres c
  = hand ((>> return failure) . withColor Red . output) c >>=
      outputFTPResult
 
hand_ :: FTPLine () -> FTPLine ()
hand_ = hand (withColor Red . output)
 
inEchoOff, inEchoOn :: FTPLine ()
inEchoOff = liftIO $ hSetEcho stdin False
inEchoOn = liftIO $ hSetEcho stdin True
 
runCom :: String -> [String] -> FTPLine ()
runCom str ["help"] = printHelp str
runCom "help" _
  = do withColor Cyan $ outputStrLn helpHead
       mapM_ printHelp commands
runCom "connect" args = handres $ mkCom args $ uncurry connect
runCom "login" args
  = do isInt <- isInteractive
       if isInt then
         do let isAnon
                  = do withColor Yellow $ outputStr "Login as anonymous? (y/n) "
                       c <- getInputChar ""
                       case c of
                           Just 'y' -> return True
                           Just 'n' -> return False
                           _ -> do liftIO $
                                     do cursorUpLine 1
                                        clearFromCursorToScreenEnd
                                   isAnon
            b <- isAnon
            if b then handres $ withColor Green $ ftplogin Nothing else
              do withColor Yellow $ outputStr "User name: "
                 name <- Haskeline.getInputLine ""
                 withColor Yellow $ outputStr "Password : "
                 pass <- getPassword (Just '*') ""
                 if isNothing name || isNothing pass then
                   outputStrLn "Incorrect login." else
                   handres $
                     withColor Green $ ftplogin $ Just (fromJust name, fromJust pass)
         else
         if null args then handres $ ftplogin Nothing else
           handres $ mkCom args $ ftplogin . Just
runCom "disconnect" _ = handres disconnect
runCom "ftpdebug" _ = ftpdebug
runCom "dir" args
  = if null args then hand_ $ pdir Nothing else
      hand_ $ mkCom args $ pdir . Just
runCom "getfile" args = handres $ mkCom args $ getfile
runCom "download" args = handres $ mkCom args $ download
runCom "putfile" args = handres $ mkCom args $ putfile
runCom "upload" args = handres $ mkCom args $ upload
runCom "rename" args = handres $ mkCom args $ uncurry renamefile
runCom "delete" args = handres $ mkCom args $ deletefile
runCom "size" args = handres $ mkCom args $ sizefile
runCom "cd" args = handres $ mkCom args $ ccd
runCom "md" args = handres $ mkCom args $ newdir
runCom "rd" args = handres $ mkCom args $ remdir
runCom "clear" _
  = do hand_ cleanFile
       outputStrLn "Memory Cleared."
runCom "pause" _
  = do outputStr "PAUSE. Press ENTER to continue..."
       inEchoOff
       _ <- liftIO $ getLine
       inEchoOn
       outputStrLn ""
runCom "switch" _
  = do turnMode
       b <- getMode
       outputStrLn $ "Current mode: " ++ boolToMode b
runCom "last" _ = lastConn
runCom x _
  = withColor Cyan $ outputStrLn $ "Command doesn't exist: " ++ x
 
commands :: [String]
commands
  = ["connect", "disconnect", "login", "ftpdebug", "last", "dir",
     "cd", "md", "rd", "getfile", "putfile", "clear", "download",
     "upload", "rename", "delete", "size", "switch", "pause", "exit"]
 
helpHead :: String
helpHead
  = unlines
      ["", "Below is a list of available commands.",
       "Arguments in brackets are optional.",
       "Arguments in braces are only used in batch mode."]
 
commandDesc :: String -> [String]
commandDesc "connect"
  = ["connect HostName Port", "* Connect to remote FTP server."]
commandDesc "login"
  = ["login {[UserName Password]}", "* Login to the FTP server."]
commandDesc "disconnect"
  = ["disconnect", "* Close the current conection."]
commandDesc "ftpdebug"
  = ["ftpdebug", "* Enable logging of FTP messages."]
commandDesc "dir"
  = ["dir [FilePath]", "* Show a directory content."]
commandDesc "getfile"
  = ["getfile FilePath",
     "* Load specified file and save its content in a temporal memory."]
commandDesc "download"
  = ["download FilePath", "* Download a remote file to your system."]
commandDesc "putfile"
  = ["putfile FilePath",
     "* Write a file with the temporal memory's content."]
commandDesc "upload"
  = ["upload FilePath", "* Upload a file from disk."]
commandDesc "rename"
  = ["rename FilePath1 FilePath2", "* Rename a file."]
commandDesc "delete" = ["delete FilePath", "* Delete a file."]
commandDesc "size"
  = ["size FilePath", "* Return the size of a file."]
commandDesc "cd"
  = ["cd FilePath", "* Change the current directory."]
commandDesc "md" = ["md FilePath", "* Create a new directory."]
commandDesc "rd" = ["rd FilePath", "* Remove a directory"]
commandDesc "exit" = ["exit", "* Close the client."]
commandDesc "clear" = ["clear", "* Clear the temporal memory."]
commandDesc "pause"
  = ["pause", "* Stop the program until pressing ENTER.",
     "* Useful for FTP batch programs."]
commandDesc "switch"
  = ["switch", "* Switch between local and remote mode.",
     "* CTRL+D invokes this action."]
commandDesc "last"
  = ["last",
     "* Try to connect and login as it was done the last time."]
commandDesc _ = ["* Invalid command."]
 
mkCom :: (Arg a) => [String] -> (a -> FTPLine b) -> FTPLine b
mkCom [] _ = ftperror "Need more arguments."
mkCom args f
  = if length args > 1 then
      parse (concat ["(", intercalate "," args, ")"]) >>= f else
      parse (head args) >>= f
 
instance Arg [Char] where
        parse = return

#if !MIN_VERSION_network(2,6,3)
instance Read PortNumber where
  readsPrec n str = (\(x,r) -> ((fromIntegral :: Int -> PortNumber) x , r)) <$> readsPrec n str
#endif
 
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

instance (Arg a, Arg b) => Arg (a, b) where
        parse str
          = let str' = init . tail $ str
                xs = takeWhile (/= ',') str'
                ys = safeTail $ dropWhile (/= ',') str'
              in
              do x <- parse xs
                 y <- parse ys
                 return (x, y)
 
instance Arg PortNumber where
  parse str = liftIO $ readIO str
 
addChar :: Char -> [String] -> [String]
addChar c [] = [[c]]
addChar c (x : xs) = (c : x) : xs
 
args' :: String -> [String] -> Bool -> [String]
args' [] xs _ = xs
args' (x : xs) ys b
  | x == ' ' =
    if b then args' xs (addChar x ys) b else args' xs ([] : ys) b
  | x == '"' = args' xs ys $ not b
  | otherwise = args' xs (addChar x ys) b
 
separgs :: String -> [String]
separgs str = reverse $ reverse <$> args' str [] False
 
withSGR :: (MonadIO m) => [SGR] -> [SGR] -> m a -> m a
withSGR xs ys comp
  = do liftIO $ setSGR xs
       x <- comp
       liftIO $ setSGR ys
       return x
 
withColor :: (MonadIO m) => Color -> m a -> m a
withColor col
  = withSGR [SetColor Foreground Vivid col]
      [SetColor Foreground Vivid White]
 
italized :: (MonadIO m) => m a -> m a
italized
  = withSGR [SetSwapForegroundBackground True]
      [SetSwapForegroundBackground False]
 
ftpLineDir :: IO FilePath
ftpLineDir = getAppUserDataDirectory "FTPLine"
 
main :: IO ()
main
  = do setSGR [SetColor Foreground Vivid White]
       args <- getArgs
       appDir <- ftpLineDir
       createDirectoryIfMissing True appDir
       putStrLn $ "*** Welcome to FTPLine " ++ showVersion version ++ " ***"
       if null args then
         withColor Cyan $
           do putStr "Type "
              italized $ putStr "help"
              putStrLn " for a list of commands."
              let sets
                    = defaultSettings{historyFile = Just $ appDir ++ "/History"}
              evalStateT (runInputT sets mainInteractive) initialFTPState
         else
         do let arg = head args
            exst <- doesFileExist arg
            if exst then
              do txt <- readFile arg
                 evalStateT
                   (runInputT defaultSettings $
                      mapM_
                        (\ str ->
                           do (mcd, hn) <- withColor Green $
                                             do mcd <- hand
                                                         ((>> (return $ Just "?")) .
                                                            withColor Red . output)
                                                         $ fst <$> curdir
                                                hn <- getHost
                                                return (mcd, hn)
                              let cd = if isNothing mcd then "? " else fromJust mcd
                              getInputLine_ (unwords [cd, "@", hn ++ ">>\n"]) str
                              hand_ $ command str)
                        $ lines txt ++ ["disconnect"])
                   initialFTPState
                 putStrLn "Closing FTPLine."
              else withColor Red $ putStrLn $ "File " ++ arg ++ " doesn't exist."
       setSGR [Reset]
 
mainInteractive :: FTPLine ()
mainInteractive
  = do setInteractive True
       runCicle
 
runCicle :: FTPLine ()
runCicle
  = hand
      (\ e ->
         do withColor Red $ output e
            ln <- getInputLine "? >>"
            if ln == "exit" then exit else
              do hand_ $ command ln
                 runCicle)
      mainCicle
 
mainCicle :: FTPLine ()
mainCicle
  = do (mcd, hn) <- withColor Green $
                      do mcd <- hand
                                  (\ e ->
                                     do withColor Red $ output e
                                        return Nothing)
                                  $ fst <$> curdir
                         hn <- getHost
                         return (mcd, hn)
       let cd = if isNothing mcd then "? " else fromJust mcd
       ln <- getInputLine $ unwords [cd, "@", hn ++ ">>"]
       if ln == "exit" then exit else
         do hand_ $ command ln
            mainCicle
 
exit :: FTPLine ()
exit
  = do _ <- hand (\ _ -> return failure) $ withColor Green disconnect
       outputStrLn "Closing FTPLine."
