This program is inspired by [this problem](http://git-annex.branchable.com/bugs/Can__39__t_add_a_git_repo_to_git_annex:___34__Invalid_path_repo__47__.git__47__X__34___for_many_X/)

It is literally just a beefed up version of [this script](https://github.com/jaemyeong/dotfiles/blob/master/git-toggle.sh)

Here's how you use it.  Say I have a big `~/works` directory with a whole bunch of `git` subrepos.  I want to use `git-annex` to sync my `~/works` directory between two computers.  `git` and in turn `git-annex` don't handle sub-repos very well.  `git-toggle` allows us to not deal with this problem by being able to "turn a sub repo off".  This literally just means renaming the .git folder and the .gitignore file to something else.  `git-toggle` however provides a nice interface for doing this from anywhere within your git sub-repo and or recursively.

$
