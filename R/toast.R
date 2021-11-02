use_toast <- function() {
  htmltools::tags$div(
    htmltools::div(
      class = "position-relative",
      `aria-live` = "polite",
      `aria-atomic` = "true",
      htmltools::div(
        id = "toaster",
        class = "toast-container position-fixed bottom-0 end-0 p-3",
        style = "z-index: 11"
      )
    ),
    htmltools::htmlDependency(
      name = "branchMover-toaster",
      version = "0.0.1",
      package = "branchMover",
      src = "srcjs",
      script = "toaster.js",
      all_files = FALSE
    )
  )
}

toastify <- function(
  title,
  body,
  state = c("success", "danger"),
  session = shiny::getDefaultReactiveDomain()
) {
  state <- match.arg(state)

  toast <- list(
    title = markdown_html(title),
    body = markdown_html(body),
    state = state
  )
  session$sendCustomMessage("toastify", toast)
  invisible(toast)
}

markdown_html <- function(x) {
  x <- commonmark::markdown_html(x)
  if (length(gregexpr("</p>", x)[[1]]) == 1) {
    x <- gsub("^<p>|</p>\\s*$", "", x)
  }
  x
}
