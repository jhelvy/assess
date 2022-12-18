
#' Get an assessment scale
#'
#' @param n Scale size
#' @export
get_scale <- function(n) {
    if (n == 3) {
        return(data.frame(
            assessment = seq(0, 3),
            score = c(0, 0.5, 0.8, 1)
        ))
    }
    if (n == 4) {
        return(data.frame(
            assessment = seq(0, 4),
            score = c(0, 0.5, 0.75, 0.9, 1)
        ))
    }
    if (n == 5) {
        return(data.frame(
            assessment = seq(0, 5),
            score = c(0, 0.5, 0.7, 0.8, 0.9, 1)
        ))
    }
    if (n == 6) {
        return(data.frame(
            assessment = seq(0, 6),
            score = c(0, 0.5, 0.7, 0.75, 0.8, 0.9, 1)
        ))
    }
}

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
