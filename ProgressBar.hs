import Prelude hiding (catch)
import System.Environment
import System.IO
import System.Directory
import System.Posix.Files
import Control.Monad
import Control.Concurrent
import Control.Exception
import System.Console.Terminfo
import Text.Printf
import Data.Char (isDigit)

readcontent :: String -> String
readcontent x =
    let corr = head $ lines x
    in  corr

drawbar :: String -> Double -> Double -> IO ()
drawbar path msize dw = do
    curpos <- fmap readcontent $ readFile path
    let (_, posinfo) = break (== '\t') curpos
        dp = read posinfo :: Double
        w = round ((dw-10)*dp/msize) :: Int
        line = replicate w '*'
        spaces = replicate ( (round dw) - w - 10 ) ' '
        pct = 100*dp/msize :: Double
    putStr $ "\r" ++ line ++ spaces
    void $ printf "%.2f" pct
    hFlush stdout
    threadDelay 500

goodlink :: FilePath -> FilePath -> IO Bool
goodlink filename fd = do
    sl <- fmap isSymbolicLink (getSymbolicLinkStatus fd)
    if sl
        then fmap (== filename) (readSymbolicLink fd)
        else return False

getFilesInDir :: String -> IO [String]
getFilesInDir dirname = fmap (map ((++) dirname)) $ getDirectoryContents dirname

readCmdLine :: String -> IO (Maybe String)
readCmdLine cmdfile = do
    filecontent <- fmap Just (readFile cmdfile) `catch` (const $ return Nothing :: IOException -> IO (Maybe String))
    case fmap (reverse . takeWhile (/= '/') . reverse . takeWhile (\x -> x /= '\NUL' && x /= ' ')) filecontent of
        Just "" -> return Nothing
        x       -> return x

getPid :: String -> IO Int
getPid programname = do
    allpids <- fmap (filter (all isDigit . drop 6)) (getFilesInDir "/proc/")
    let cmdlinefiles = map (++ "/cmdline") allpids
    cmdlines <- fmap (zip allpids) $ mapM readCmdLine cmdlinefiles
    let validcmdlines = filter (\(_,x) -> Just programname == x) cmdlines
        toPid = read . drop 6 . fst
    case validcmdlines of
        []  -> error $ "Could not find binary " ++ programname
        [x] -> return $ toPid x
        xs  -> do
            hPutStrLn stderr "Warning, more than one process found, using the first one"
            return $ toPid $ head xs

main :: IO ()
main = do
    (programname,filename) <- getArgs >>=
        \x -> case x of
                  [p,f] -> return (p,f)
                  _     -> error "Usage: ProgressBar programname filename"
    pid <- getPid programname
    let dirname = "/proc/" ++ show pid ++ "/fd/"
    links <- getFilesInDir dirname
    rlinks <- filterM (goodlink filename) links
    if null rlinks
        then error $ "Can't find " ++ filename
        else do
            let fd = (reverse . fst . break (=='/') . reverse . head) rlinks
                fdinfo = "/proc/" ++ show pid ++ "/fdinfo/" ++ fd
            fsize <- fmap (toInteger . fileSize) (getFileStatus filename)
            dwm <- fmap (\x -> getCapability x termColumns) (setupTerm "vt100")
            case dwm of
                Just dw -> forever (drawbar fdinfo (fromIntegral fsize) (fromIntegral dw))
                Nothing -> error "Can't get term width"
