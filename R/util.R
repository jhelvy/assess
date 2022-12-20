
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

#' Unzip or copy submissions
#'
#' If bb submission is a zip file, it unzips the contents, otherwise it copies
#' the file over
#'
#' @param text Text to save
#' @param path Path to save to
#' @export
unzip_submissions <- function(pars, roster, id, junkpathsSetting = FALSE) {

    # Create unzipped submissions folder if it does't exist
    path <- here::here("assignments", pars$assign, "submissions")
    make_dir(here::here("assignments", pars$assign, "unzipped"))

    # Unzip or copy each file
    ids <- get_enrolled_ids(roster, id)
    files <- file.path(path, list.files(path))
    cat("Missing:\n")
    for (i in 1:length(ids)) {
        id <- ids[i]
        studentFiles <- files[which(str_detect(files, id))]
        if (length(studentFiles) == 0) {
            cat(id, '\n')
            next()
        }
        dest <- here::here("assignments", pars$assign, "unzipped", id)
        for (j in 1:length(studentFiles)) {
            import_file(studentFiles[j], dest, junkpathsSetting)
        }
    }
}

get_enrolled_ids <- function(roster, id) {
    result <- roster %>%
        filter(enrolled == 1) %>%
        pull({{id}})
    return(result)
}

import_file <- function(file, destination, junkpathsSetting) {

    # If it's a zip file, unzip it to the destination

    if (fs::path_ext(str_to_lower(file)) == 'zip') {

        zip::unzip(
            zipfile = file,
            exdir = destination,
            junkpaths = junkpathsSetting
        )

    # Otherwise just copy the file there

    } else {
        file.copy(from = file, to = file.path(destination, basename(file)))
    }

}
