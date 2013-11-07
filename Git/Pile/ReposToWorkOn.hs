{-# LANGUAGE PackageImports #-}
module Git.Pile.ReposToWorkOn
 (reposToWorkOn) where

import Git.Pile.Types
import Git.Pile.ReposToWorkOn.GetRepoState
import Git.Pile.PathConstants

import "base" Data.Functor
 ((<$>))

import "base" Data.Maybe
 (isNothing
 ,fromJust)

import "filepath" System.FilePath
 (pathSeparator)

import "filepath-extrautils" System.FilePath.ExtraUtils
 (clipPaths
 ,getDirectoryContentsRecursive
 ,getParentNamedMaybe
 ,isPathTo)

import "small-print" Control.Exception.SmallPrint
 ((*@)
 ,(*@@)
 ,exception)

-- | What repos are we going to work on?
reposToWorkOn :: Action -> IO [RepoState]
reposToWorkOn
 MakePileable
  {startPath=p
  ,recursiveness=NonRecursive} =
 do
  state <- getRepoState p
  return [state]

reposToWorkOn
 MakePileable
  {startPath=p
  ,recursiveness=Recursive}=
 do
  paths <- getDirectoryContentsRecursive p
  let
   repoPaths = clipPaths pathsToGitDirs
   pathsToGitDirs =
    filter
     (\path -> isPathTo path gitDirectory)
     paths
  mapM getRepoState repoPaths

reposToWorkOn
 RunGitCommand
  {startPath=p
  ,goalRepoName=grn} =
 (\x->[x]) <$> (getRepoState p *@@ notAtGoalRepo *@@ goalRepoIsPath)
 where
  goalRepoIsPath =
   exception
    (return $ elem pathSeparator grn)
    (getRepoStateStrict grn)
  notAtGoalRepo =
   exception
    (return $ p/=grn)
    (getRepoStateStrict parent)
      where
       parent =
        case getParentNamedMaybe p grn of
         Just parent -> parent
         Nothing -> error ("Didn't find any parent repo named "++grn++".")