add_buttons <- function(repos_df) {
  if ("button" %in% names(repos_df)) {
    return(repos_df)
  }

  repos_df$button <- repos_df$full_name
  repos_df[repos_df$default_branch == "main", "button"] <- ""
  repos_df
}

repos_reactable <- function(repos_df, include_buttons = FALSE) {
  repos_df[[".repo"]] <- NULL

  if (isTRUE(include_buttons)) {
    repos_df <- add_buttons(repos_df)
  }

  repos_df$can_admin <- ifelse(repos_df$can_admin, "Yes", "No")

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
        cell = reactable::JS('function(cellInfo) {
          const desiredClass = window.branchMoverNewDefaultBranch || "main"
          let badgeClass = "badge bg-light"
          badgeClass = cellInfo.value === "master" ? "badge bg-danger" : badgeClass
          badgeClass = cellInfo.value === desiredClass ? "badge bg-success" : badgeClass
          return `<span class="${badgeClass}">${cellInfo.value}</span>`
        }')
      ),
      language = colDef(name = "Language"),
      is_fork = colDef(show = FALSE),
      is_private = colDef(show = FALSE),
      count_forks = colDef("Forks", filterable = FALSE),
      count_stargazers = colDef("Stars", filterable = FALSE),
      can_admin = colDef("Admin?", align = "center"),
      url_html = colDef(show = FALSE),
      url_ssh = colDef(show = FALSE),
      button = colDef(
        name = "", html = TRUE, filterable = FALSE, minWidth = 120,
        cell = reactable::JS('function(cellInfo) {
          if (cellInfo.row.can_admin !== "Yes") {
            return `<span class="text-info">Cannot admin</span>`
          }
          const hasIssue = !(cellInfo.row.issue === "NA" || cellInfo.row.issue === "" || cellInfo.row.issue === null)
          const isIssueClosed = cellInfo.row.state === "closed"
          const isDefault = cellInfo.row.default_branch === (window.branchMoverNewDefaultBranch || "main")
          if ((hasIssue && isIssueClosed && isDefault) || (!hasIssue && isDefault)) {
            return ""
          }
          const buttonText = hasIssue && isDefault ? "Finalize Change" : "Change Default Branch"
          const action = hasIssue && isDefault ? "finalize" : "change"
          return `<button data-action="${action}" data-repo="${cellInfo.row.full_name}" type="button" class="btn btn-primary btn-sm js-change-branch">${buttonText}</button>`
        }')
      ),
      issue = colDef(
        name = "Issue", html = TRUE, filterable = FALSE,
        cell = reactable::JS('function(cellInfo) {
          return cellInfo.value
            ? `<a href="${cellInfo.row.issue_url}">#${cellInfo.value}</a>`
            : ""
        }')
      ),
      state = colDef(show = FALSE),
      created_at = colDef(show = FALSE),
      issue_url = colDef(show = FALSE)
    )
  )
}
