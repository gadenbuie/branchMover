about_page <- function() {
  shiny::tabPanel(
    title = "About",
    id = "about",
    shiny::div(
      class = "container",
      shiny::div(
        class = "row justify-content-center",
        shiny::div(
          class = "col-md-9 col-lg-6 col-10",
          shiny::HTML(markdown_html(pkg_read_lines("about.md")))
        )
      )
    )
  )
}
