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

module Main where
import Git.Toggle.ToggleLib
import System.Environment
import System.Directory

help = unlines
 ["git-toggle is a small tool for managing git repos within git-annex repos."
 ,""
 ,"Say you have a git-annex repo called ~/works"
 ,"And in ~/works you have a git repo called ~/works/myProgram"
 ,"And you want to sync ~/works with another computer using git-annex"
 ,"The problem is, you cannot add the .git directory in ~/works/myProgram"
 ,"because git-annex might get confused."
 ,""
 ,"You have to rename that .git directory to something else, add it to git-annex,"
 ,"sync, and then rename it back when you want to use it."
 ,""
 ,"git-toggle is a fast way to rename .git folders to git-off folders so that git-annex can work with them."
 ,"In order to rename the .git folder in ~/works/myProgram,"
 ,"you navigate to somewhere within ~/works/myProgram and issue the command:"
 ,"$git-toggle off"
 ,""
 ,"In order to rename the folder back to .git so it can be used as a normal git repo type:"
 ,"$git-toggle on"
 ,""
 ,"Say that in ~/works you have a lot of git repos,"
 ,"and you don't want to rename them all before adding them to git-annex."
 ,"In order to rename the .git folders to git-off of all sub-directories of a given directory"
 ,"navigate to that directory, and issue the command:"
 ,"$git-toggle subrepos off"
 ,"NOTE: subrepos works on subdirectories of the current directory."
 ,"It does not work on subrepos of the current git-repo!"
 ,""
 ,"Adding the bellow keyword to the end of a command recursively goes down the directory tree turning all git repos off."
 ,""
 ,"In order to rename all the git-off folders that are subdirs of a given dir, use:"
 ,"$git-toggle subrepos on"
 ,""
 ,"In order to see subrepos you can use:"
 ,"$git-toggle subrepos list"
 ,"$git-toggle subrepos list on"
 ,"$git-toggle subrepos list off"
 ,""
 ,"$git-toggle subrepos tracked list"
 ,"$git-toggle subrepos tracked list on"
 ,"$git-toggle subrepos tracked list off"
 ,""
 ,"$git-toggle subrepos untracked list"
 ,"$git-toggle subrepos untracked list on"
 ,"$git-toggle subrepos untracked list off"
 ,""
 ,"NOTE: Remember to navigate to the top-level directory in your main git repo in order to see ALL subrepos, and not just subrepos of the current working directory..."
 ,""
 ,"You can also do a dryrun by prepending any command with dry:"
 ,"$git-toggle dry subrepos on"
 ,""
 ,"git-annex specific commands."
 ,""
 ,"There are two specific commands that are usefull for working with git-annex."
 ,""
 ,"$git-toggle use"
 ,"use is the same as \"git-annex unlock ./\"ing a repo and then turning it on."
 ,""
 ,"$git-toggle subrepos tracked-off-and-add-to-annex"
 ,"This command is to be put at the beginning of your pre-commit hook."
 ,""
 ,"You can see this help message by issuing the command:"
 ,"$git-toggle help"
 ,""
 ,"This software was written by Timothy Hobbs and is released under the terms of the GPL-3 licence. See the file COPYING in the source directory."]

data Options
 = Help
 | Do Action
 | Dry Action
 | List Action
 | Warn String deriving (Show)

main :: IO ()
main = do
 args    <- getArgs
 pwd     <- getCurrentDirectory
 options <- return $ fromArgsGetOptions pwd args
 mapM_
  (\option ->case option of
   Do  a -> fromActionGetRepoStates a
            >>= actionStatesToRenames a
            >>= renameFiles
   Dry a -> fromActionGetRepoStates a
            >>= actionStatesToRenames a
            >>= dryRenameFiles
   List a -> fromActionGetRepoStates a
            >>= listRepos
   Warn message -> putStrLn $ "Warning: " ++ message
   Help  -> putStrLn help)
  options



fromArgsGetOptions :: FilePath -> [String] -> [Options]
fromArgsGetOptions p ("off":[]) = [Do $ Above p Off]
fromArgsGetOptions p ("on":[]) = [Do $ Above p On]
fromArgsGetOptions p ("subrepos":"on":[]) = [subreposWarning,Do $ Bellow p On]
fromArgsGetOptions p ("subrepos":"off":[]) = [subreposWarning,Do $ Bellow p Off]
fromArgsGetOptions p ("subrepos":"list":[]) = [subreposWarning,List $ Bellow p On,List $ Bellow p Off]
fromArgsGetOptions p ("subrepos":"list":"on":[]) = [subreposWarning,List $ Bellow p On]
fromArgsGetOptions p ("subrepos":"list":"off":[]) = [subreposWarning,List $ Bellow p Off]
fromArgsGetOptions p ("dry":args)
 = map
    (\option-> case option of Do a -> Dry a ; _ -> option)
    $ fromArgsGetOptions p args
fromArgsGetOptions _ _ = [Help]

subreposWarning = Warn "Actions on subrepos must search your directory tree. They may take a long time. Typically about 1 seccond per 2000 files."

listRepos :: [RepoState] -> IO ()
listRepos repos = mapM_ (\repo->putStrLn $ show repo) repos
