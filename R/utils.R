pkg_read_lines <- function(..., values = list()) {
  txt <- readLines(system.file(..., package = "branchMover"))
  txt <- paste0(txt, collapse = "\n")
  if (length(values)) {
    glue::glue_data(values, txt)
  } else {
    txt
  }
}
