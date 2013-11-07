module Git.Pile.Types
 (Action(MakePileable,startPath,recursiveness,RunGitCommand,startPath,goalRepoName,command)
 ,Recursiveness(Recursive,NonRecursive)
 ,RepoState(RepoState,repoPath,repoState,lockStatus)
 ,LockStatus(Locked,Unlocked)
 ,State(Pileable,NonPileable)) where

data Action
 = MakePileable
    {startPath :: FilePath
    ,recursiveness :: Recursiveness}
 | RunGitCommand
    {startPath     :: FilePath
    ,goalRepoName  :: FilePath
    ,command       :: [String]} deriving (Show)

data Recursiveness = Recursive | NonRecursive deriving (Show,Eq)

data State = Pileable | NonPileable deriving (Show)

data LockStatus = Locked | Unlocked

data RepoState = RepoState
 {repoPath :: FilePath
 ,repoState :: State
 ,lockStatus :: LockStatus}

instance Show RepoState where
 show state = unlines
  ["-------------------------"
  ,repoPath state
  ,unwords [" This repo is ",show (repoState state)]]
