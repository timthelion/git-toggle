{-# LANGUAGE PackageImports #-}
{-
This module provides help text and command line option parsing.
The real work is done in Git.Pile.PileLib

See license info at end of file.
-}

module Main where
import Git.Pile.PileLib
import Git.Pile.Types

import "base" System.Environment
 (getArgs)
 
import "directory" System.Directory
 (getCurrentDirectory)

help = unlines
 ["git-pile is a small tool for managing git repos within git-annex repos."
 ,""
 ,"Say you have a git-annex repo called ~/works"
 ,"And in ~/works you have a git repo called ~/works/myProgram"
 ,"And you want to sync ~/works with another computer using git-annex"
 ,"The problem is, you cannot add the .git directory in ~/works/myProgram"
 ,"because git-annex might get confused."
 ,""
 ,"You have to package that .git dir in such a way that git doesn't get confused as to which repository it is working on."
 ,"You also need a way to unpack it temporarily when you want to use it."
 ,""
 ,"git-pile is a fast way to package .git folders as git-is-pileable tarballs so that git-annex can work with them."
 ,"In order to package the .git folder in ~/works/myProgram,"
 ,"you navigate to somewhere within ~/works/myProgram and issue the command:"
 ,"$git-pile make-pileable"
 ,""
 ,"NOTE: If you're thinking\"Hey looks like git-bundle\" you aren't to far off.  git-bundle should work for this purpose. git-pile *should* be more preformant and *should* put less wear on your hard drive.  It doesn't require you to check out then rebundle each time you do something. On linux it uses fuserfs to mount the .git tarball without writing to disk. It saves changes by appending to the tarball."
 ,""
 ,"In order to issue git commands on a pileable git repo, you need a way of temporarily unpacking the git-is-pileable tarball.  To do so, we prefix our normal git commands with the command gere(git here):"
 ,""
 ,"$gere some-git-command"
 ,""
 ,"On linux this should fusermount your .git tarball,"
 ,"run the git command,"
 ,"and then write any changes git made to the fusermounted .git directory to disk."
 ,"On Windows, we have to resort to unpacking the entire tarball and repacking it when we are done."
 ,""
 ,"try $gere status"
 ,""
 ,"Say that in ~/works you have a lot of git repos,"
 ,"and you don't want do make-pileable by hand before adding them to git-annex."
 ,"You can also run make-pileable recursively with:"
 ,""
 ,"$git-pile make-pileable recursive"
 ,""
 ,"If you issue that command in ~/works then ~/works and all subrepos of works will be made pileable."
 ,""
 ,"Since we're piling repo's, it's often useful to run a command on a git repo that is under our current one."
 ,"Say the $PWD is ~/works/myProgram but I want to run a git command on ~/works.  The `gin`(run git in) command is useful here."
 ,""
 ,"$gin works status"
 ,""
 ,"goes up the $PWD looking for a git repo in a directory named works.  It then runs the git command on that directory."
 ,""
 ,"You can also use absolute paths with `gin`:"
 ,""
 ,"$gin /home/timothy/works"
 ,""
 ,"Both gere and gin work on both pileable and normal git repos."
 ,""
 ,"This software was written by Timothy Hobbs and is released under the terms of the GPL-3 licence. See the file COPYING in the source directory."]

data Options
 = Help
 | Do Action
 | Warn String deriving (Show)

main :: IO ()
main = do
 args    <- getArgs
 pwd     <- getCurrentDirectory
 options <- return $ fromArgsGetOptions pwd args
 mapM_
  (\option ->case option of
   Do a -> perform a
   Warn message -> putStrLn $ "Warning: " ++ message
   Help  -> putStrLn help)
  options

fromArgsGetOptions :: FilePath -> [String] -> [Options]
fromArgsGetOptions p ("make-pileable":[]) =
 [Do MakePileable{startPath=p,recursiveness=NonRecursive}]
fromArgsGetOptions p ("make-pileable":"recursive":[]) =
 [Do MakePileable {startPath=p,recursiveness=Recursive}]
fromArgsGetOptions p ("gin":repoName:cs) =
 [Do RunGitCommand{startPath=p,goalRepoName=repoName,command=cs}]
fromArgsGetOptions p ("gere":cs) =
 [Do RunGitCommand{startPath=p,goalRepoName=p,command=cs}]
fromArgsGetOptions _ _ = [Help]

subreposWarning = Warn "Recursive actions must search your directory tree. They may take a long time. Typically about 1 seccond per 2000 files."

{-GPLv3.0 Timothy Hobbs timothyhobbs@seznam.cz

Copyright 2012,2013

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