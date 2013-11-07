{-# LANGUAGE NamedFieldPuns #-}
{-

This module provides the main function perform.

See license info at end of file.
-}

module Git.Pile.PileLib
 (perform
 ,Action(MakePileable,RunGitCommand)
 ,Recursiveness(Recursive,NonRecursive)) where

import Git.Pile.Types
import Git.Pile.PathConstants
import Git.Pile.ReposToWorkOn(reposToWorkOn)
import Git.Pile.DoWork(doWork)

perform :: Action -> IO ()
perform action
 =   reposToWorkOn action
 >>= doWork action

{-GPLv3.0 Timothy Hobbs timothyhobbs@seznam.cz

Copyright 2013.

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