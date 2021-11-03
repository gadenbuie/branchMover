
#' @export
app <- function(username = NULL, ...) {
  shiny::shinyApp(ui(), server(username, ...))
}


ui <- function(req) {
  shiny::navbarPage(
    "Branch Mover",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "zephyr",
      primary = "#447099",
      secondary = "#75AADB",
      success = "#A4C689",
      warning = "#fdbe4b",
      info = "#CBD4DD",
      "font-size-base" = "1rem"
    ),
    collapsible = TRUE,
    page_repos(),
    page_issues(),
    page_settings(),
    page_about()
  )
}

server <- function(username, ...) {
  function(input, output, session) {
    repos <- gh_user_repos(username, ...)
    repo_ex <- repos[1, ]

    repos <- shiny::reactiveVal(repos)

    output$repos <- reactable::renderReactable({
      repos_reactable(shiny::isolate(repos()), include_buttons = TRUE)
    })

    # store issue text as an RStudio user preference
    shiny::observeEvent(input$issue_markdown, ignoreInit = TRUE, {
      stored_preference("issue_markdown", input$issue_markdown)
    })

    shiny::observeEvent(input$issue_close_markdown, ignoreInit = TRUE, {
      stored_preference("issue_close_markdown", input$issue_close_markdown)
    })

    output$issue_preview <- shiny::renderUI({
      html <- commonmark::markdown_html(
        glue::glue(
          input$issue_markdown,
          repo = repo_ex$full_name[[1]],
          old_default = repo_ex$default_branch[[1]],
          new_default = input$new_default
        )
      )
      shiny::div(
        class = "border-start border-3 ps-4",
        shiny::HTML(html)
      )
    })

    output$issue_close_preview <- shiny::renderUI({
      html <- commonmark::markdown_html(
        glue::glue(
          input$issue_close_markdown,
          repo = repo_ex$full_name[[1]],
          old_default = repo_ex$default_branch[[1]],
          new_default = input$new_default
        )
      )
      shiny::div(
        class = "border-start border-3 ps-4",
        shiny::HTML(html)
      )
    })

    rv_update <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$change_branch, {
      session$sendCustomMessage("set_change_branch_state", "disable")

      repo <- input$change_branch$repo

      if (input$change_branch$action == "change") {
        cli::cli_process_start("Changing branch of repo: {repo}")
      } else {
        cli::cli_process_start("Finalizing change of default branch in {repo}")
      }
      res <- move_default_branch(
        repo = repo,
        new_default = input$new_default,
        issue_number = repos()[["issue"]][repos()$full_name == repo],
        issue_body = input$issue_markdown,
        issue_close = input$issue_close_markdown,
        auto_close_issue = identical(input$change_branch$action, "finalize") ||
          identical(input$auto_close_issue, "yes")
      )

      cli::cli_process_done()
      session$sendCustomMessage("set_change_branch_state", "enable")
      rv_update(res)
    })

    shiny::observe({
      res <- rv_update()
      shiny::req(res)

      repos <- shiny::isolate(repos())

      if (!is.null(res$issue)) {
        issue <- res$issue[c("number", "html_url", "state", "created_at")]
        names(issue) <- c("issue", "issue_url", "state", "created_at")
        for (field in names(issue)) {
          repos[repos$full_name == res$repo$full_name, field] <- issue[[field]]
        }
      }

      if (!is.null(res$branch)) {
        repos[repos$full_name == res$repo$full_name, "default_branch"] <- res$branch
      }

      if (!is.null(res$message)) {
        toastify(
          title = res$repo$full_name,
          body = res$message,
          state = c("danger", "success")[res$success + 1]
        )
      } else if (res$success) {
        toastify(
          title = res$repo$full_name,
          body = if (!identical(res$branch, res$repos$default_branch)) {
            glue::glue("Default branch moved to `{res$branch}`")
          } else {
            glue::glue("Default branch remained the same: `{res$branch}`")
          },
          state = "success"
        )
      } else {
        toastify(
          title = res$repo$full_name,
          body = "Could not change default branch",
          state = "danger"
        )
      }

      repos(repos %>% add_buttons())
      rct_state <- shiny::isolate(reactable::getReactableState("repos"))
      repos$can_admin <- ifelse(repos$can_admin == TRUE | repos$can_admin == "Yes", "Yes", "No")
      reactable::updateReactable("repos", repos, page = rct_state$page)
    })
  }
}


stored_preference <- function(name, value = NULL) {
  name <- paste0("branchMover.", name)

  if (is.null(value)) {
    if (!rstudioapi::hasFun("readPreference")) {
      return(NULL)
    }
    pref <- rstudioapi::readPreference(name, "")
    return(if (nzchar(pref)) pref)
  }

  if (!rstudioapi::hasFun("writePreference")) {
    return(NULL)
  }

  rstudioapi::writePreference(name, value)
}

stored_preference_clear <- function() {
  rstudioapi::writePreference("branchMover.issue_markdown", "")
  rstudioapi::writePreference("branchMover.issue_close_markdown", "")
}
