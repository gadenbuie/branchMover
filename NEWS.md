# branchMover 0.1.2

* `app()` now has documentation and explicitly includes the `include_fork` and
  `include_private` arguments that can be used to include forked or private
  repos in the app. Currently, we can only list the private repos for the
  authenticated user. In all cases, the console messages report which repos are
  included in the app (thanks @sellorm, #11).
  
**Full Changelog**: https://github.com/gadenbuie/branchMover/commits/v0.1.1

# branchMover 0.1.1

## What's Changed

* Import shinycssloaders by @jennybc in https://github.com/gadenbuie/branchMover/pull/4
* Add some more resources re: PAT management by @jennybc in https://github.com/gadenbuie/branchMover/pull/3
* Fix running app for users or orgs other than authenticated user by @gadenbuie in https://github.com/gadenbuie/branchMover/pull/7
* Only offer to change branch for repos the user can admin by @gadenbuie in https://github.com/gadenbuie/branchMover/pull/8
* Use GitHub API to move default branch, rather than `usethis::git_default_branch_rename()`. This saves us the trouble of cloning the repo and speeds up the branch moving. @gadenbuie in https://github.com/gadenbuie/branchMover/pull/10
* The app now changes the default branch and opens an issue but leaves the issue open by default. You can follow up in the repo to change any links as needed and can return to the app to finalize the process.
* When we detect that GitHub pages were being served from the default branch of the repo, we now request a pages rebuild. In general, GitHub seems to change the branch for Pages but doesn't rebuild the site, leaving your page in a 404 state until you trigger a new page build.

## New Contributors

* @jennybc made their first contribution in https://github.com/gadenbuie/branchMover/pull/4

**Full Changelog**: https://github.com/gadenbuie/branchMover/commits/v0.1.1
