
#' Makes a directory
#'
#' Makes a directory
#'
#' @param path Path to directory
#' @export
make_dir <- function(path) {
    if (!dir.exists(path)) { dir.create(path) }
}

#' Write text to file
#'
#' Write text to file
#'
#' @param text Text to save
#' @param path Path to save to
#' @export
save_raw <- function(text, path) {
    fileConn <- file(path)
    writeLines(text, fileConn)
    close(fileConn)
}
