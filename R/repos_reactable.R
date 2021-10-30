repos_reactable <- function(repos_df, include_buttons = TRUE) {
  repos_df[[".repo"]] <- NULL

  if (include_buttons) {
    repos_df$button <- glue::glue(
      '<button data-repo="{repo}" type="button" class="btn btn-primary btn-sm js-change-branch">Change Default Branch</button>',
      repo = repos_df$full_name
    )
    repos_df$issue <- glue::glue(
      '<span class="js-issue-link" data-repo="{repo}"></span>',
      repo = repos_df$full_name
    )
    repos_df[repos_df$default_branch == "main", "button"] <- ""
  }

  reactable(
    repos_df,
    searchable = TRUE,
    filterable = TRUE,
    borderless = TRUE,
    highlight = TRUE,
    columns = list(
      full_name = colDef(
        name = "Repo",
        minWidth = 150,
        html = TRUE,
        cell = function(value, index) {
          glue::glue(
            '<a href="{href}" style="{style}">{repo}</a>',
            style = "word-wrap: break-word",
            href = repos_df$url_html[[index]],
            repo = shiny::HTML(sub("/", "/<wbr>", value))
          )
        }
      ),
      default_branch = colDef(
        name = "Default Branch",
        html = TRUE,
        align = "center",
        cell = function(value) {
          glue::glue(
            '<span class="{color}">{value}</span>',
            color = switch(
              value,
              master = "badge bg-danger",
              main = "badge bg-success",
              "badge bg-light"
            )
          )
        }
      ),
      language = colDef(name = "Language"),
      is_fork = colDef(show = FALSE),
      is_private = colDef(show = FALSE),
      count_forks = colDef("Forks", filterable = FALSE),
      count_stargazers = colDef("Stars", filterable = FALSE),
      url_html = colDef(show = FALSE),
      url_ssh = colDef(show = FALSE),
      button = colDef(name = "", html = TRUE, filterable = FALSE, minWidth = 120),
      issue = colDef(name = "Issue", html = TRUE, filterable = FALSE)
    )
  )
}
