
import System.Directory
import System.FilePath
import Data.List
import System.Environment
--import Data.String.Utils

data DirInfo = DirInfo { diName :: String
                       , diExts :: [String]
		       }
		       deriving(Show, Read)

data FSConfig = FSConfig { cfgDirs :: [DirInfo]
                         , cfgUnknownFolder :: String
			 , cfgIgnoreExt :: [String]
		         }
		         deriving(Read)


getDirInfoForFile :: [DirInfo] -> FilePath -> Maybe DirInfo
getDirInfoForFile [] _ = Nothing
getDirInfoForFile (x:xs) path = 
  if elem extension exts
  then Just x
  else getDirInfoForFile xs path
  where
  extension = drop 1 $ takeExtension path
  exts = diExts x


moveFileInto :: FilePath -> String -> IO ()
moveFileInto path dst = do
  fe <- doesDirectoryExist dst
  moveFileInto' fe
  where 
    moveFileInto' :: Bool -> IO ()
    moveFileInto' True = renameFile path $ dst </> path
    moveFileInto' False = do
      createDirectory dst
      moveFileInto' True

 


categorizeDirectory :: FSConfig -> String -> IO ()
categorizeDirectory cfg executableName = do
  curDir <- getCurrentDirectory
  dirContent <- getDirectoryContents curDir
  catFiles [d | d <- dirContent, not (isPrefixOf "." d), d /= executableName]
  where
    catFiles :: [FilePath] -> IO ()
    catFiles [] = return ()
    catFiles (x:xs) = do
      isFile <- doesFileExist x
      if isFile
        then do
          catFile x
          catFiles xs
        else
          catFiles xs

    catFile :: FilePath -> IO ()
    catFile path = 
      if elem extension (cfgIgnoreExt cfg)
      then return ()
      else
        case getDirInfoForFile (cfgDirs cfg) path of
          Just dirInfo -> moveFileInto path (diName dirInfo)
          Nothing -> moveFileInto path (cfgUnknownFolder cfg)
      where
        extension = drop 1 $ takeExtension path

     


main = do
  configFile <- readFile ".fsconfig.hs"
  executableName <- getProgName
  categorizeDirectory (read configFile) executableName


