gh_user_repos <- function(user = NULL, include_fork = TRUE, include_private = TRUE) {
  checkmate::assert_character(user, max.len = 1, null.ok = TRUE)
  user <- user %||% gh::gh_whoami()[["login"]]

  user <- gh::gh("/user") %>%
    ui_report_total_user_repos()

  repos <- gh::gh("/user/repos", .limit = Inf, type = "owner")

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

  repos %>%
    purrr::map_dfr(`[`, repo_fields) %>%
    dplyr::mutate(.repo = repos) %>%
    dplyr::rename(!!!repo_fields) %>%
    filter_rows_if(!include_fork, !.data$is_fork) %>%
    filter_rows_if(!include_private, !.data$is_private) %>%
    dplyr::arrange(dplyr::desc(.data$count_stargazers)) %>%
    ui_report_default_branch_count()
}

get_repos <- memoise::memoise(gh_user_repos, cache = memoise::cache_filesystem(tempdir()))

filter_rows_if <- function(df, cond, expr) {
  if (!cond) return(df)
  expr <- rlang::enexpr(expr)
  dplyr::filter(df, !!expr)
}

ui_report_total_user_repos <- function(user) {
  total_repos <- user$public_repos + user$owned_private_repos

  cli_alert_info(
    "{.field @{user$login}} has {.strong {total_repos}} total repositories (including forks)"
  )
  cli_bullets(c(
    "*" = "{.strong {user$public_repos}} public repos",
    "*" = "{.strong {user$owned_private_repos}} private repos"
  ))

  user
}

ui_report_default_branch_count <- function(repos_df) {
  repos_default_branch_count <-
    repos_df %>%
    dplyr::mutate(
      default_branch = forcats::fct_collapse(
        .data$default_branch,
        master = "master",
        main = "main",
        other_level = "other"
      )
    ) %>%
    dplyr::count(.data$default_branch, sort = TRUE)

  rdbc <- repos_default_branch_count %>% split(.$default_branch)

  cli_alert_info(
    "{sum(repos_default_branch_count$n)} non-fork repositories have the following default branches:"
  )
  cli_bullets(c(
    "x" = if ("master" %in% names(rdbc))
      "{.field master}: {rdbc$master$n} repos",
    "v" = if ("main" %in% names(rdbc))
      "{.field main}: {rdbc$main$n} repos",
    "!" = if ("other" %in% names(rdbc))
      "{.field something else}: {rdbc$other$n} repos"
  ))

  repos_df
}
