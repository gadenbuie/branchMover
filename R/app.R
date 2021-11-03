
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
    shiny::tabPanel(
      title = "Repos",
      id = "repos",
      shiny::div(
        class = "container-sm",
        shinycssloaders::withSpinner(
          reactable::reactableOutput("repos"),
          type = 8,
          color = "#447099",
          color.background = "#CBD4DD"
        ),
        use_toast(),
        shiny::tags$script(shiny::HTML(
          "function disableAllChangeButtons() {
            document
              .querySelectorAll('.js-change-branch')
              .forEach(function(el) {
                el.setAttribute('disabled', true)
                el.classList.add('btn-disabled')
              })
          }

          function enableAllChangeButtons() {
            document
              .querySelectorAll('.js-change-branch')
              .forEach(function(el) {
                el.innerHTML = 'Change Default Branch'
                el.removeAttribute('disabled')
                el.classList.remove('btn-disabled')
              })
          }

          function addSpinner(el) {
            el.innerHTML = '<span class=\"spinner-border spinner-border-sm\"' +
              'role=\"status\" aria-hidden=\"true\"></span> Changing Branch'
          }

          $('#repos').on('click', '.js-change-branch', function(ev) {
            const {repo, action} = ev.target.dataset
            Shiny.setInputValue('change_branch', {repo, action})
            addSpinner(ev.target)
          })

          $(document).on('change', '#new_default', function(ev) {
            window.branchMoverNewDefaultBranch = ev.target.value
          })

          Shiny.addCustomMessageHandler('set_change_branch_state', function(x) {
            x === 'disable' ? disableAllChangeButtons() : enableAllChangeButtons()
          })"
        ))
      )
    ),
    shiny::tabPanel(
      title = "Issue Text",
      shiny::div(
        class = "container-sm",
        shiny::p(
          "An issue with this body will be created when we start the branch change process.",
          "If successful, we'll also close the issue with a comment.",
          "You can use", shiny::code("{repo}", .noWS = "after"), ",",
          shiny::code("{old_default}", .noWS = "after"), ", and",
          shiny::code("{new_default}", .noWS = "after"),
          "in the text for the full repo name, old default branch name and",
          "new default branch name, respectively."
        ),
        shiny::div(
          class = "row",
          shiny::h3("Issue text announcing the branch change"),
          shiny::div(
            class = "col-md-6",
            shiny::textAreaInput(
              "issue_markdown",
              "Issue Body",
              value = pkg_read_lines("templates", "issue-body.md"),
              width = "100%",
              rows = 10
            ),
          ),
          shiny::div(
            class = "col-md-6",
            shiny::p("Preview"),
            shiny::uiOutput("issue_preview")
          )
        ),
        shiny::div(
          class = "row",
          shiny::h3("Issue comment announcing the branch change was completed"),
          shiny::div(
            class = "col-md-6",
            shiny::textAreaInput(
              "issue_close_markdown",
              "Comment Body",
              value = pkg_read_lines("templates", "issue-close.md"),
              width = "100%",
              rows = 10
            )
          ),
          shiny::div(
            class = "col-md-6",
            shiny::p("Preview"),
            shiny::uiOutput("issue_close_preview")
          ),
          shiny::tags$style(shiny::HTML(
            "#issue_markdown, #issue_close_markdown { font-family: monospace; font-size: 1rem; }"
          ))
        )
      )
    ),
    shiny::tabPanel(
      title = "Settings",
      id = "settings",
      shiny::div(
        class = "container",
        shiny::textInput(
          "new_default",
          "New default branch name",
          value = "main"
        ),
        shiny::radioButtons(
          "auto_close_issue",
          "Should we automatically close issue when move is complete?",
          choices = c("Yes" = "yes", "No, leave the issue open" = "no")
        ),
        shiny::helpText(
          "If you leave the issue open, you can return to this app at a later time to finalize the change and close the issue."
        )
      )
    ),
    about_page()
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
