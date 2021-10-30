
#' @export
move_default_branch <- function(
  repo,
  issue_number = NULL,
  new_default = "main",
  issue_body = NULL,
  issue_close = NULL
) {
  r <- gh::gh("/repos/{repo}", repo = repo)

  if (identical(r$default_branch, new_default)) {
    cli::cli_alert_success("The default branch for {.field {repo}} is already {.value {new_default}}")
    return(list(success = TRUE))
  }

  if (!is.null(issue_number)) {
    issue <- gh::gh("/repos/{repo}/issues/{number}", repo = repo, number = issue_number)
  } else {
    issue_body <- issue_body %||% pkg_read_lines("templates", "issue-body.md")
    issue <- gh::gh(
      "POST /repos/{repo}/issues",
      repo = repo,
      title = jsonlite::unbox(glue::glue(
        "Move default branch from `{old}` to `{new}`",
        old = r$default_branch,
        new = new_default
      )),
      body = jsonlite::unbox(
        glue::glue(
          issue_body,
          repo = repo,
          old_default = r$default_branch,
          new_default = new_default
        )
      )
    )
    cli_alert_success("Created issue {.field #{issue$number}}")
  }
  cli_alert_info("{.url {issue$html_url}}")
  cli_alert_info("Cloning {repo}")

  withr::with_tempdir({
    path <- gert::git_clone(r$ssh_url)
    setwd(path)

    old_proj <- usethis::proj_set(path)

    issue_close <- issue_close %||% pkg_read_lines("templates", "issue-close.md")

    success <- FALSE
    tryCatch({
      usethis::git_default_branch_rename(to = new_default)
      gh::gh(
        "POST /repos/{repo}/issues/{number}/comments",
        repo = repo,
        number = issue$number,
        body = jsonlite::unbox(
          glue::glue(
            issue_close,
            repo = repo,
            old_default = r$default_branch,
            new_default = new_default
          )
        )
      )
      gh::gh(
        "PATCH /repos/{repo}/issues/{number}",
        repo = repo,
        number = issue$number,
        state = jsonlite::unbox("closed")
      )
      cli_alert_success("Moved to branch {.field main} and closed issue #{issue$number}")
      success <- TRUE
    }, error = function(e) {
      cli::cli_alert_danger("Could not move default branch to {.field main}")
      cli::cli_text(e$message)
    })

    usethis::proj_set(old_proj)

    list(
      success = success,
      repo = r,
      issue = issue
    )
  })
}
