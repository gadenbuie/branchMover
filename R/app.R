
#' @export
app <- function(user = NULL, ...) {
  repos <- get_repos(user, ...)
  # repos <- gh_user_repos(user, ...)

  shiny::shinyApp(ui(), server(repos))
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
        reactable::reactableOutput("repos"),
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
            Shiny.setInputValue('change_branch', ev.target.dataset.repo)
            addSpinner(ev.target)
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
          "If successful, we'll also close the issue with a comment."
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
            "#issue_markdown, #issue_close_markdown { font-family: monospace }"
          ))
        )
      )
    ),
    shiny::tabPanel(
      title = "Branch Name",
      id = "settings",
      shiny::div(
        class = "container",
        shiny::textInput(
          "new_default",
          "New Default Branch Name",
          value = "main"
        )
      )
    )
  )
}

server <- function(repos) {
  function(input, output, session) {
    repo_ex <- repos[1, ]

    repos <- shiny::reactiveVal(repos)

    output$repos <- reactable::renderReactable(repos_reactable(repos(), include_buttons = TRUE))

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
      cli::cli_process_start("Changing branch of repo: {input$change_branch}")
      Sys.sleep(runif(1, 0, 3))
      # mock issue number
      issue <- list(
        number = floor(runif(1, 1, 100)),
        html_url = "https://github.com/gadenbuie/shinyThings/issues/5",
        state = "closed",
        created_at = paste(Sys.time())
      )
      res <- list(
        success = TRUE,
        branch = input$new_default,
        repo = input$change_branch,
        issue = issue
      )
      cli::cli_process_done()
      session$sendCustomMessage("set_change_branch_state", "enable")
      rv_update(res)
    })

    shiny::observe({
      res <- rv_update()
      shiny::req(res)

      repos <- isolate(repos())

      issue <- res$issue[c("number", "html_url", "state", "created_at")]
      names(issue) <- c("issue", "issue_url", "state", "created_at")
      for (field in names(issue)) {
        repos[repos$full_name == res$repo, field] <- issue[[field]]
      }

      if (res$success) {
        repos[repos$full_name == res$repo, "default_branch"] <- res$branch
      }

      repos(repos)
      rct_state <- reactable::getReactableState("repos")
      str(rct_state)
      reactable::updateReactable("repos", repos, page = rct_state$page)
    })
  }
}
