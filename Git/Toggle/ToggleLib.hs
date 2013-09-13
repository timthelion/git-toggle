{-GPLv3.0 Timothy Hobbs timothyhobbs@seznam.cz

Copyright 2012.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.-}

module Git.Toggle.ToggleLib where

import System.Directory
import System.FilePath
import Control.Monad
import System.PosixCompat.Files
import Data.Maybe

data Action
 = Above
    {startPath :: FilePath
    ,goalState :: State}
 | Bellow
    {startPath :: FilePath
    ,goalState :: State} deriving (Show)

data State = On | Off deriving (Show,Eq)

otherState :: State -> State
otherState On = Off
otherState Off = On

onDirectory = ".git"
offDirectory = "git-off"
onIgnore = ".gitignore"
offIgnore = "gitignore-off"

getIgnore :: State -> FilePath
getIgnore On  = onIgnore
getIgnore Off = offIgnore

getDirectory :: State -> FilePath
getDirectory On = onDirectory
getDirectory Off = offDirectory

data RepoState = RepoState
 {repoPath :: FilePath
 ,repoState :: State
 ,ignoreExists :: Bool}

instance Show RepoState where
 show state = unlines
  ["-------------------------"
  ,repoPath state
  ,unwords $ [" Repo is turned",show (repoState state)]
  ,unwords $ [" Git ignore",
    case ignoreExists state of
      True -> "exists."
      False -> "does not exist."]]

fromActionGetRepoStates :: Action -> IO [RepoState]
fromActionGetRepoStates (Above p s) =
 do
  state <- getRepoState p
  return [state]
fromActionGetRepoStates (Bellow p s) =
 do
  let
  paths <- getSubDirectoryContentsRecursive p
  let repoPaths
       = clipPaths
       $ filter
          (\path ->
             isPathTo path (getDirectory s))
          paths
  mapM getRepoState repoPaths

isPathTo :: FilePath -> FilePath -> Bool
isPathTo path destination = destination == (last $ splitPath path)

clipPaths :: [FilePath] -> [FilePath]
clipPaths = map (\p->joinPath $ init $ splitPath p)

getRepoState :: FilePath -> IO RepoState
getRepoState dir
 =   findPathContainingFilesAbove [(onDirectory,On),(offDirectory,Off)] dir
 >>= (\(fp,s) ->
       doesFileExist (getIgnore s) >>= (\i->return $ RepoState fp s i))

findPathContainingFilesAbove :: [(FilePath,a)] -> FilePath -> IO (FilePath,a)
findPathContainingFilesAbove files path
 = do
 contents <- getDirectoryContents path
 let matches
      = mapMaybe
         (\(fp,a)->
             case elem fp contents of
               True -> Just a
               False -> Nothing)
         files
 case matches of
  [a] -> return $ (path,a)
  []  -> let up = takeDirectory path in
          case up == path of
           True -> error "No repo found."
           False -> findPathContainingFilesAbove files up
  _  -> error $ "Repo "++path++" in ambigous ON/OFF state. Both .git and git-off directories exist."

actionStatesToRenames :: Action -> [RepoState] -> IO [(FilePath,FilePath)]
actionStatesToRenames action rss = do
 translationss <- mapM (makeTranslation action) rss
 return $ concat translationss

-- | Returns the file paths to be changed.  [(from,to)]
makeTranslation :: Action -> RepoState -> IO [(FilePath,FilePath)]
makeTranslation action state = do
  let s = goalState action
  case repoState state == s of
   True -> do
    putStrLn $ "Repo "++(repoPath state)++" already "++show s++"."
    return []
   False -> do
    let i = case ignoreExists state of
             True -> [(repoPath state </> (getIgnore $ otherState s)
                      ,repoPath state </> (getIgnore s))]
             False -> []
    return $ (repoPath state </> (getDirectory $ otherState s)
             ,repoPath state </> (getDirectory s)) : i

-- | List all the files and directories in the subdirectories of a directory.
getSubDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getSubDirectoryContentsRecursive topDir = do
 subDirs <- getSubDirectories topDir
 contents <- mapM getDirectoryContentsRecursive $ map (\dir -> topDir </> dir) subDirs
 return $ concat contents

-- Shamelessly stolen (AND FIXED!) from the Extra package(due to dependencies)
-- | Return the list of subdirectories, omitting . and ..
getSubDirectories :: FilePath -> IO [String]
getSubDirectories path =
 getDirectoryContents path >>=
 return . filter (not . (flip elem) [".", ".."]) >>=
 filterM isRealDirectory
 where
   isRealDirectory name = do
    status <- getSymbolicLinkStatus (path </> name)
    isDirectory <- doesDirectoryExist (path </> name)
    return $ (not $ isSymbolicLink status) && isDirectory

-- | Returns paths to all files and directories which are bellow the current directory.  i.e. the contents of this directory, all of it's subdirectories and their subdirectories and so on. 
-- NOTE: does not return the contents of subdirectories which are symlinks.
getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir = do
 contents       <- getDirectoryContents dir
 subDirs        <- getSubDirectories    dir
 contentsOfSubDirs <-
  mapM
     getDirectoryContentsRecursive
   $ map
      (\subDir->dir </> subDir)
      subDirs
 return
  $ concat
  $ (map (\file-> dir </> file) contents)
    : contentsOfSubDirs

printPair :: (FilePath, FilePath) -> IO ()
printPair (from,to) =
 putStrLn $ unwords ["Renaming",from,"to",to,"..."]

renameFiles :: [(FilePath,FilePath)] -> IO ()
renameFiles = renameFiles' renameFileOrDirectory

renameFileOrDirectory :: FilePath -> FilePath -> IO ()
renameFileOrDirectory from to = do
 fromDir <- doesDirectoryExist from
 toDir <- doesDirectoryExist to
 fromFile <- doesFileExist from
 toFile <- doesFileExist to
 case (fromDir, toDir, fromFile, toFile) of
  (True,False,False,False) -> renameDirectoryButDon'tBreakSymlinks from to
  (False,False,True,False) -> renameFile from to
  _ -> putStrLn $ "Something whent wrong renaming "++from++" to "++to++"."

-- | If renaming the directory would cause symlink breakage, then display an error, otherwise rename it.
renameDirectoryButDon'tBreakSymlinks :: FilePath -> FilePath -> IO ()
renameDirectoryButDon'tBreakSymlinks from to = do
 don'tContinue <- wouldRenamingDirectoryBreakSymlinks from to
 if don'tContinue
 then
  putStrLn "It looks like you have some relative symlinks in your git dir(Perhaps these files are locked by git-annex?).  I cannot turn git on or off."
 renameDirectory from to

wouldRenamingDirectoryBreakSymlinks :: FilePath -> FilePath -> IO Bool
wouldRenamingDirectoryBreakSymlinks from to = do
 filesInGitDir <- getDirectoryContentsRecursive from
 symlinkDestinations <- mapMaybeM filesInGitDir

 where
  maybeDestination = do
   sl <- isSymbolicLink
   case sl of
    True -> Just `fmap` readSymbolicLink
    False -> return Nothing

mapMaybeM :: (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f as = mapM f as >>= (\mbs -> return $ catMaybes mbs)

dryRenameFiles :: [(FilePath,FilePath)] -> IO ()
dryRenameFiles = renameFiles' (\_ _->return ())

renameFiles' :: (FilePath -> FilePath -> IO ()) -> [(FilePath,FilePath)] -> IO ()
renameFiles' renameFile' []   = putStrLn "Nothing to do, exiting."
renameFiles' renameFile' dirs = mapM_ (\(s,d)-> printPair (s,d) >> renameFile' s d) dirs
