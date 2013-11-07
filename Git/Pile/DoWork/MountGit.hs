{-# LANGUAGE PackageImports #-}

module Git.Pile.DoWork.MountGit
 (mountGit
 ,unmountGit) where

import Git.Pile.PathConstants

import "base" Data.Functor
 ((<$>))

import "archivemount-hs" System.Directory.Archivemount
 (mountTarballForSavingPortable
 ,FileTreeMetaData
 ,saveAndUnmountNoncompressedTarballPortable
 ,MountStatus(Mounted)
 ,UnmountStatus(Unmounted))

import "filepath" System.FilePath
 ((</>))

import "small-print" Control.Exception.SmallPrint
 ((*@)
 ,(*@@)
 ,exception)

import "directory" System.Directory
 (doesFileExist
 ,doesDirectoryExist
 ,removeFile)

import qualified "tar" Codec.Archive.Tar as Tar
 (create)

import "unix-compat" System.PosixCompat.Files
 (createSymbolicLink)

mountGit
 :: FilePath -- ^ top dir of repo to be worked on.
 -> IO (MountStatus,Maybe FileTreeMetaData)
mountGit repo = do
 (status,ftmd) <- mountTarballForSavingPortable
  (repo </> pileablePackage)
  (repo </> mountPoint)
 (do
  makeSymlinkToGitFile repo gitDirectory
  makeSymlinkToGitFile repo gitIgnore
  makeSymlinkToGitFile repo gitAttributes) *@@ mountFailed status
 return (status,ftmd)
 where
 mountFailed status = exception
  (return $ status /= Mounted)
  (return ())

makeSymlinkToGitFile repo file =
 createSymbolicLink (repo </> mountPoint </> file) (repo </> file) *@@ fileDoesNotExist
 where
 fileDoesNotExist = exception
  (do
    fe <- doesFileExist (repo </> mountPoint </> file)
    de <- doesDirectoryExist (repo </> mountPoint </> file)
    return (not fe && not de))
  (return ())
 

unmountGit
 :: FileTreeMetaData
 -> FilePath -- ^ top dir of repo that we just finished working on.
 -> IO UnmountStatus
unmountGit beforeMetaData repo = do
 status <- saveAndUnmountNoncompressedTarballPortable beforeMetaData (repo </> mountPoint) (repo </> pileablePackage)
 (do
  deleteSymlinkToGitFile repo gitDirectory
  deleteSymlinkToGitFile repo gitIgnore
  deleteSymlinkToGitFile repo gitAttributes) *@@ unmountFailed status
 return status
 where
 unmountFailed status = exception
  (return $ status /= Unmounted)
  (return ())

deleteSymlinkToGitFile repo link =
 removeFile (repo </> link) *@@ linkDoesNotExist
 where
 linkDoesNotExist = exception
  (do
   fe <- doesFileExist (repo </> link)
   de <- doesDirectoryExist (repo </> link)
   return (not fe && not de))
  (return ())