#'@export
WINGS <- function() {
  path <- paste0(.libPaths()[1],"/ICARUSviewer")
  shiny::runApp(path)
}