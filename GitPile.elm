module GitPile where
import Window

data OperatingSystem =
   Linux
 | Windows

main = (\os height -> flow right [color blue (spacer 4 height),flow down [
 [markdown|

# git-pile  -  it makes git work with git

<img src="images/git-pile-man.png" width="90%" alt="A man carying a pile of plates."/>

I like to keep all the programs that I'm working on in a big folder called `works`.  I wanted to synchronize this folder between my various computers with `git-annex`, but I ran into a little problem.  I manage all my projects with `git`.  And we all know that `git` doesn't work with `git`.

<img src="images/bad.png" width="90%" alt="A diagram of a non-functioning git setup."/>

That's why I wrote `git-pile`.

## How it works

|]
 ,flow right [On linux, On windows]
 ,operatingSystemSpecificContent os]]) <~ osS Window.width

operatingSystemSpecificContent os = flow down [[markdown|

In order to add the `git-pile` repo to the `works` repo, I have to make it pileable first:

`git-pile make-pileable`

This command transforms a normal git repo into a pileable git repo.

<img src="images/make-pileable.png" width="90%" alt="A diagram showing the git logo turning into a plate(the git-pile logo)."/>

PS: It is also possible to do `git-pile make-pileable recursive` to do that on a lot of repos at once.

Now we can add this repo to our git annex repo above:

<img src="images/git-pile-example-tree.png" width="90%" alt="A diagram of a properly piled git settup using git-annex and git-pile"/>



This command removed the `.git` directory and replaced it with an uncompressed tarbal named `this-git-repo-is-pileable`.

Normal `git` commands will no longer have any affect on this repo of course.  They will fall straight through to the works repo above(if you haven't made it pileable as well).

In order to run git commands on piled git repos you will need two new tools:

<img 

|]
 ,case os of
   Linux -> [markdown|

|]

]
