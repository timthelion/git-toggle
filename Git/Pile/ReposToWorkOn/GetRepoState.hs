{-# LANGUAGE PackageImports #-}
{-
This module is used for querying git repositories for info like:
 Where is their top directory?
 Are they pilelable?
 Is the repo currently being used by a git command?
-}
module Git.Pile.ReposToWorkOn.GetRepoState
 (getRepoState
 ,getRepoStateStrict) where

import Git.Pile.Types
import Git.Pile.PathConstants
import Git.Pile.ReposToWorkOn.MaybeUtils

import "directory" System.Directory
 (doesFileExist
 ,getDirectoryContents)

import "filepath" System.FilePath
 ((</>)
 ,takeDirectory)

import "base" Data.Maybe
 (mapMaybe)

import "small-print" Control.Exception.SmallPrint
 ((*@)
 ,(*@@)
 ,exception)

getRepoState :: FilePath -> IO RepoState
getRepoState dir = getRepoState' dir goUp

getRepoStateStrict :: FilePath -> IO RepoState
getRepoStateStrict dir = getRepoState' dir (FindPath don'tGoUp)

-- | TODO canonicalize paths!
getRepoState' :: FilePath -> FindPath State -> IO RepoState
getRepoState' dir goUp' = do
 let (FindPath fpcfa) = findPathContainingFilesAbove
 (fp,s) <- fpcfa [(pileablePackage,Pileable),(gitDirectory,NonPileable)] dir goUp'
 mounted <- doesFileExist (fp </> mountPoint)
 let locked = case mounted of
               True  -> Locked
               False -> Unlocked
 return $ RepoState fp s locked

newtype FindPath a =
 FindPath
  (  [(FilePath,a)]
  -> FilePath
  -> FindPath a
  -> IO (FilePath,a) )

extractFindPathFunction
 :: FindPath a
 ->
  (  [(FilePath,a)]
  -> FilePath
  -> FindPath a
  -> IO (FilePath,a) )
extractFindPathFunction (FindPath fpf) = fpf

findPathContainingFilesAbove :: FindPath a
findPathContainingFilesAbove = FindPath findPathContainingFilesAbove'
findPathContainingFilesAbove' filesToSearchFor path goUpOrNot
 = do
 contents <- getDirectoryContents path
 let
  matches = mapMaybe containsFile filesToSearchFor
  containsFile (fp,a) =
   case elem fp contents of
    True -> Just a
    False -> Nothing
 case matches of
  [a] -> return (path,a)
  []  -> (extractFindPathFunction goUpOrNot) filesToSearchFor path goUpOrNot
  _  -> error $ "Repo "++path++" in ambigous state. Both a .git dir and a git-is-pileable file exist."

goUp = FindPath (\files path ->
 fpcfa files (up path) *@ (atTop path))
 where
 fpcfa = extractFindPathFunction findPathContainingFilesAbove
 up path = takeDirectory path
 atTop path =
  exception
   ((up path) == path)
   (error "Not a git repo.")

don'tGoUp _ _ = error "Not a git repo."