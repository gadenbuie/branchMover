
<!-- README.md is generated from README.Rmd. Please edit that file -->

# branchMover

<!-- badges: start -->
<!-- badges: end -->

Change the default branch of your GitHub repositories, from inside
RStudio.

## Installation

You can install the development version of branchMover from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gadenbuie/branchMover")
```

The app uses the [usethis](https://usethis.r-lib.org) and
[gh](https://gh.r-lib.org) packages. You need to configure gh with a
Personal Access Token to be able to authenticate with the GitHub API:
read more about [Managing Personal Access
Tokens](https://gh.r-lib.org/articles/managing-personal-access-tokens.html)

Then, in RStudio, run the app with:

``` r
branchMover::app()
```

![](man/figures/app.png)

## Notes

branchMover changes the default branch on GitHub and creates an issue
announcing the change. If everything works, the issue is closed and
instructions on how to update local copies of the repo are added to the
issue.

Unfortunately, branchMover *doesn’t* update the default branch in *your*
local copies of your repos. Thankfully, this is relatively pain-free
with the
[usethis::git_default_branch_rediscover()](https://usethis.r-lib.org/reference/git-default-branch.html)
function, added in usethis version 2.1.2.

The tidyverse article [Renaming the default
branch](https://www.tidyverse.org/blog/2021/10/renaming-default-branch/)
by Jenny Bryan provides a lot more detail about what’s going on behind
the scenes.
