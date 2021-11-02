**Branch Mover** was built by [Garrick Aden-Buie](https://www.garrickadenbuie.com).
Come say hi to me on Twitter at [&commat;grrrck](https://twitter.com/grrrck).

This app wouldn't be possible without [Jenny Bryan](https://jennybryan.org/)'s thoughtful and detailed work.
In particular the [usethis](https://usethis.r-lib.org) package
and the recently added
[usethis::git_default_branch_rename()](https://usethis.r-lib.org/reference/git-default-branch.html)
function.
Jenny's coordinated efforts to rename the default branches of
about 350 of [RStudio](https://rstudio.com)'s open source repositories
greatly inspired this app.

If you want to learn more about how git's default branch works
and why many are opting to choose a more intentional default,
I highly recommend Jenny's detailed article 
[Renaming the default branch](https://www.tidyverse.org/blog/2021/10/renaming-default-branch/),
which I'll quote directly:

> Technically, Git has no official concept of the default branch. But in practice, most Git repos have an _effective default branch_. If there’s only one branch, this is it! It is the branch that most bug fixes and features get merged in to. It is the branch you see when you first visit a repo on a site such as GitHub. On a Git remote, it is the branch that `HEAD` points to. The default branch may not be precisely defined in Git itself, but most of us know it when we see it.
>
> Historically, `master` has been the most common name for the default branch, but `main` is an increasingly popular choice. There is coordinated change across the Git ecosystem that is making it easier for users to make this switch, for example:
> 
> - [Regarding Git and Branch Naming](https://sfconservancy.org/news/2020/jun/23/gitbranchname/), statement from the Git project and the Software Freedom Conservancy regarding the new `init.defaultBranch` configuration option
> - [Renaming the default branch from`master`](https://github.com/github/renaming#readme), GitHub’s roadmap for supporting the shift away from `master`
> - [The new Git default branch name](https://about.gitlab.com/blog/2021/03/10/new-git-default-branch-name/), same, but for GitLab
>
> <cite>Jenny Bryan, [Renaming the default branch](https://www.tidyverse.org/blog/2021/10/renaming-default-branch/)</cite>
