{-# LANGUAGE PackageImports #-}
{-
This module applies commands.
-}
module Git.Pile.DoWork
 (doWork)
 where

import Git.Pile.Types
import Git.Pile.PathConstants
import Git.Pile.DoWork.MountGit

import "process-shortversions" System.Process.ShortVersions
 (runCommandInDir)

import "base" Control.Concurrent
 (threadDelay)

import "archivemount-hs" System.Directory.Archivemount
 (MountStatus(Mounted,CouldNotMount)
 ,UnmountStatus(Unmounted,CouldNotUnmount,CouldNotUnmountDeviceOrResourceBusy))

import qualified "tar" Codec.Archive.Tar as Tar
 (create)

import "small-print" Control.Exception.SmallPrint
 ((*@)
 ,(*@@)
 ,exception)

import "filepath" System.FilePath
 ((</>))

import "directory" System.Directory
 (doesFileExist
 ,doesDirectoryExist)

import "base" Data.Functor
 ((<$>))

doWork MakePileable{} repoStates = mapM_ makePileable repoStates
doWork RunGitCommand{command=command} repoStates =
 mapM_ (runGitCommand command) repoStates

makePileable :: RepoState -> IO ()
makePileable RepoState{repoPath=repoPath,repoState=NonPileable} = do
 gitDirectory' <- return [gitDirectory] *@@ gitDirectoryDoesNotExist
 gitIgnore' <- return [gitIgnore] *@@ gitIgnoreDoesNotExist
 gitAttributes' <- return [gitAttributes] *@@ gitAttributesDoesNotExist
 let toBePiled = gitDirectory' ++ gitIgnore' ++ gitAttributes'
 --Tar.create pileablePackage repoPath toBePiled -- TODO file ownership!
 runCommandInDir repoPath "tar" ("-cf":toBePiled)
 runCommandInDir repoPath "rm" ("-rf":toBePiled)
 where
 gitDirectoryDoesNotExist = directoryDoesNotExistException (repoPath </> gitDirectory)
 gitIgnoreDoesNotExist = fileDoesNotExistException (repoPath </> gitIgnore)
 gitAttributesDoesNotExist = fileDoesNotExistException (repoPath </> gitAttributes)
 fileDoesNotExistException file =
  exception
   (not <$> doesFileExist file)
   (return [])
 directoryDoesNotExistException dir =
  exception
   (not <$> doesDirectoryExist dir)
   (return [])

makePileable RepoState{repoPath=repoPath,repoState=Pileable} = putStrLn $ unwords ["Repo",repoPath,"is already pileable, skipping."]

runGitCommand :: [String] -> RepoState -> IO ()
runGitCommand _ RepoState{lockStatus=Locked} =
 putStrLn "Repo is locked(in use). Cannot perform actions here."
runGitCommand command RepoState{repoPath=repoPath, repoState=Pileable, lockStatus=Unlocked} = do
 (success,beforeMetaDataM) <- mountGit repoPath
 case (success,beforeMetaDataM) of
  (Mounted, Just beforeMetaData) -> do
   runCommandInDir repoPath "git" command
   threadDelay 1000000
   unmountStatus <- unmountGit beforeMetaData repoPath
   case unmountStatus of
    CouldNotUnmount err -> do
     putStrLn err
    CouldNotUnmountDeviceOrResourceBusy -> do
     putStrLn "Could not unmount, device or resource busy."
    Unmounted -> return ()
   return ()
  (CouldNotMount error,Nothing) -> do
   putStrLn error
   putStrLn
    $ unwords
     [ "Skipping repo",repoPath,"cannot mount."
     , "Will not run commands here." ]

runGitCommand command RepoState{repoPath=repoPath, repoState=NonPileable,lockStatus=Unlocked} =
 runCommandInDir repoPath "git" command