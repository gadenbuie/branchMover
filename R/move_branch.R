
#' @export
move_default_branch <- function(
  repo,
  issue_number = NULL,
  new_default = "main",
  issue_body = NULL,
  issue_close = NULL,
  auto_close_issue = TRUE
) {
  r <- gh::gh("/repos/{repo}", repo = repo)

  if (r$archived || r$disabled) {
    cli::cli_alert_info("{.field {repo}} is archived or disabled, cannot change default branch")
    return(list(
      success = FALSE,
      repo = r,
      branch = r$default_branch,
      message = glue::glue("`{repo}` is archived or disabled, cannot change default branch")
    ))
  }

  if (shiny::isTruthy(issue_number)) {
    issue <- gh::gh("/repos/{repo}/issues/{number}", repo = repo, number = issue_number)
    cli_alert_info("{.url {issue$html_url}}")
  } else if (r$has_issues) {
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
          "\n\n<sup>ID: 91e77a361e4e</sup>",
          repo = repo,
          old_default = r$default_branch,
          new_default = new_default,
          .trim = FALSE
        )
      )
    )
    cli_alert_success("Created issue {.field #{issue$number}}")
    cli_alert_info("{.url {issue$html_url}}")
  } else {
    issue <- NULL
    cli::cli_alert_warning("Issues are not enabled for {.field {repo}}, will not create an issue to announce branch change")
  }

  is_finalized <- is.null(issue) || identical(issue$state, "closed")

  if (identical(r$default_branch, new_default) && is_finalized) {
    cli::cli_alert_success("The default branch for {.field {repo}} is already {.value {new_default}}")
    return(list(
      success = TRUE,
      repo = r,
      branch = r$default_branch,
      issue = issue,
      message = glue::glue("The default branch for `{repo}` is already `{new_default}`")
    ))
  }

  issue_close <- issue_close %||% pkg_read_lines("templates", "issue-close.md")

  # Move default branch ----
  if (!identical(r$default_branch, new_default)) {
    success <- FALSE
    tryCatch({
      gh::gh(
        "POST /repos/{repo}/branches/{from}/rename",
        repo = repo,
        from = r$default_branch,
        new_name = new_default
      )
      cli_alert_success("Moved to branch {.field {new_default}}")
      success <- TRUE
    }, error = function(err) {
      cli::cli_alert_danger("Could not move default branch to {.field {new_default}}")
      cli::cli_text(err$message)
    })
  } else {
    success <- TRUE
    cli_alert_success("The default branch of {.field {repo}} is already {.strong {new_default}}")
  }

  # Check in on GitHub Pages ----
  # This seems to be handled automatically by the GitHub branch rename,
  # but on a few of my repos the change wasn't made so I added this check
  pages <- gh_check_pages(r)
  if (!is.null(pages) && pages$has_default_branch_pages) {
    cli::cli_alert_warning("Detected that {.field {repo}} has GitHub Pages served from the current default branch")
    success_pages_branch <- FALSE
    tryCatch({
      pages$source$branch <- new_default
      gh::gh(
        "PUT /repos/{repo}/pages",
        source = pages$source
      )
      success_pages_branch <- TRUE
      cli::cli_alert_success("GitHub Pages are now served from {.field {new_default}}")
    }, error = function(err) {
      cli::cli_alert_danger("Could not move pages branch to {.field {new_default}}")
      cli::cli_text(err$message)
    })

    if (!is.null(issue)) {
      tryCatch({
        gh::gh(
          "POST /repos/{repo}/issues/{number}/comments",
          repo = repo,
          number = issue$number,
          body = jsonlite::unbox(
            pkg_read_lines(
              "templates", "issue-pages.md",
              repo = repo,
              old_default = r$default_branch,
              new_default = new_default,
              action = if (success_pages_branch) "updated" else "was not able to update"
            )
          )
        )
      }, error = function(err) {
        cli::cli_alert_danger("Could not add comment about GitHub pages to issue #{issue$number}")
        cli::cli_text(err$message)
      })
    }
  }

  # Close the issue on success ----
  if (success && isTRUE(auto_close_issue)) {
    tryCatch({
      if (!is.null(issue)) {
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
        issue$state <- "closed"
        cli_alert_success("Closed issue #{issue$number}")
      }
    }, error = function(err) {
      cli::cli_alert_danger("Could not close issue #{issue$number}")
      cli::cli_text(err$message)
    })
  }

  list(
    success = success,
    branch = if (success) new_default else r$default_branch,
    repo = r,
    issue = issue
  )
}
