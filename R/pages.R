page_repos <- function() {
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
      shiny::includeScript(system.file("srcjs", "inputs.js", package = "branchMover"))
    )
  )
}

page_issues <- function() {
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
            value = stored_preference("issue_markdown") %||%
              pkg_read_lines("templates", "issue-body.md"),
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
            value = stored_preference("issue_close_markdown") %||%
              pkg_read_lines("templates", "issue-close.md"),
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
  )
}

tab_page_centered <- function(title, id, ...) {
  shiny::tabPanel(
    title = title,
    id = id,
    shiny::div(
      class = "container",
      shiny::div(
        class = "row justify-content-center",
        shiny::div(
          class = "col-md-9 col-lg-6 col-10",
          ...
        )
      )
    )
  )
}

page_about <- function() {
  tab_page_centered(
    title = "About",
    id = "about",
    shiny::HTML(markdown_html(pkg_read_lines("about.md")))
  )
}

page_settings <- function() {
  tab_page_centered(
    title = "Settings",
    id = "settings",
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
}
