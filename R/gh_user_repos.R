gh_user_repos <- function(username = NULL, include_fork = FALSE, include_private = TRUE) {
  checkmate::assert_character(username, max.len = 1, null.ok = TRUE)
  username_authenticated <- gh::gh_whoami()[["login"]]
  username <- username %||% username_authenticated
  is_authenticated_user <- identical(username, username_authenticated)

  gh::gh("/users/{username}", username = username) %>%
    ui_report_total_user_repos()

  if (!is_authenticated_user && include_private) {
    cli_alert_danger("branchMover cannot list private repos for users other than the authenticated user ({.field @{username_authenticated}})")
  }
  ui_report_inclusions(include_fork, is_authenticated_user && include_private)

  repos <-
    if (is_authenticated_user) {
      gh::gh("/user/repos", .limit = Inf, type = "owner")
    } else {
      gh::gh("/users/{username}/repos", .limit = Inf, type = "owner", username = username)
    }

  repo_fields <- c(
    full_name = "full_name",
    default_branch = "default_branch",
    language = "language",
    is_private = "private",
    is_fork = "fork",
    count_forks = "forks_count",
    count_stargazers = "stargazers_count",
    url_html = "html_url",
    url_ssh = "ssh_url"
  )

  repos_df <-
    repos %>%
    purrr::map_dfr(`[`, repo_fields) %>%
    dplyr::mutate(
      can_admin = purrr::map_lgl(repos, c("permissions", "admin"), .default = FALSE),
      .repo = repos
    ) %>%
    dplyr::rename(!!!repo_fields) %>%
    filter_rows_if(!include_fork, !.data$is_fork) %>%
    filter_rows_if(!include_private, !.data$is_private) %>%
    dplyr::arrange(
      dplyr::desc(.data$can_admin),
      dplyr::desc(.data$count_forks + .data$count_stargazers)
    ) %>%
    dplyr::select(dplyr::all_of(names(repo_fields)), dplyr::everything())

  issues <- gh_find_branch_mover_issues(username) %>%
    ui_report_branch_mover_issues()

  repos_df <- dplyr::left_join(repos_df, issues, by = "full_name")

  repos_df %>%
    ui_report_default_branch_count()
}

gh_find_branch_mover_issues <- function(username) {
  issues <- gh::gh("/search/issues", q = glue::glue("91e77a361e4e org:{username}"))
  if (!length(issues$items)) {
    return(dplyr::tibble(
      full_name = character(0),
      issue = integer(0),
      state = character(0),
      created_at = character(0),
      issue_url = character(0)
    ))
  }

  issues$items %>%
    purrr::map_dfr(`[`, c("url", "html_url", "number", "state", "created_at")) %>%
    dplyr::mutate(
      full_name = sub("https://.+/repos/", "", .data$url),
      full_name = sub("/issues/.+$", "", .data$full_name)
    ) %>%
    dplyr::group_by(.data$full_name) %>%
    dplyr::slice_max(.data$created_at, n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$full_name, issue = .data$number, .data$state, .data$created_at, issue_url = .data$html_url)
}

get_repos <- memoise::memoise(gh_user_repos, cache = memoise::cache_filesystem(tempdir()))

filter_rows_if <- function(df, cond, expr) {
  if (!cond) return(df)
  expr <- rlang::enexpr(expr)
  dplyr::filter(df, !!expr)
}

ui_report_total_user_repos <- function(user) {
  total_repos <- (user$public_repos %||% 0) + (user$owned_private_repos %||% 0)

  cli_alert_info(
    "{.field @{user$login}} has {.strong {total_repos}} total repositor{?y/ies} (including forks)"
  )
  cli_bullets(c(
    "*" = "{.strong {user$public_repos}} public repo{?s}",
    "*" = "{.strong {user$owned_private_repos %||% 0}} private rep{?s}"
  ))

  user
}

ui_report_inclusions <- function(forks, private) {
  things <- c("{.strong forks}", "{.strong private repos}")
  include <- things[c(forks, private)]
  exclude <- things[!c(forks, private)]
  if (length(include)) {
    cli_alert_info(c("Will include ", glue::glue_collapse(include, sep = " and ")))
  }
  if (length(exclude)) {
    cli_alert_info(c("Will exclude ", glue::glue_collapse(exclude, sep = " and ")))
  }
}

ui_report_default_branch_count <- function(repos_df) {
  repos_default_branch_count <-
    repos_df %>%
    dplyr::mutate(
      default_branch = forcats::fct_expand(.data$default_branch, "main", "master"),
      default_branch = forcats::fct_collapse(
        .data$default_branch,
        master = "master",
        main = "main",
        other_level = "other"
      )
    ) %>%
    dplyr::count(.data$default_branch, sort = TRUE)

  rdbc <-
    repos_default_branch_count %>%
    split(repos_default_branch_count$default_branch) %>%
    purrr::keep(function(x) nrow(x) > 0)

  cli_alert_info(
    "{sum(repos_default_branch_count$n)} non-fork repositor{?y/ies} {?has/have} the following default branch{?es}:"
  )
  cli_bullets(c(
    "x" = if ("master" %in% names(rdbc))
      "{.field master}: {rdbc$master$n} repo{?s}",
    "v" = if ("main" %in% names(rdbc))
      "{.field main}: {rdbc$main$n %||% 0} repo{?s}",
    "!" = if ("other" %in% names(rdbc))
      "{.field something else}: {rdbc$other$n} repo{?s}"
  ))

  cli_alert_info(
    "You have admin rights on {.strong {sum(repos_df$can_admin)}} repo{?s}"
  )

  repos_df
}

ui_report_branch_mover_issues <- function(x) {
  if (is.null(x) || !nrow(x)) {
    return(invisible(x))
  }

  n_issues <- nrow(x)
  n_closed <- sum(x$state == "closed")
  n_unresolved <- n_issues - n_closed

  cli_alert_info("Found {.val {n_issues}} {.emph branch mover} issue{?s}:")
  cli_bullets(
    c(
      "!" = if (n_unresolved > 0) "{.val {n_unresolved}} {?is/are} unresolved",
      "v" = if (n_closed > 0) "{.val {n_closed}} {?is/are} closed"
    )
  )

  invisible(x)
}

gh_repo_pages <- function(repo_spec) {
  repo <- if (is.character(repo_spec)) {
    gh::gh("/repos/{repo_spec}", repo_spec = repo_spec)
  } else {
    stopifnot(inherits(repo_spec, "gh_response"))
    repo_spec
  }

  if (!repo$has_pages) {
    return(NULL)
  }

  pages <- gh::gh("/repos/{repo_spec}/pages", repo_spec = repo$full_name)
  pages
}
