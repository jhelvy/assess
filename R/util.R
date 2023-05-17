
#' Make a directory
#'
#' @param path Path to directory
#' @export
make_dir <- function(path) {
    if (!dir.exists(path)) { dir.create(path) }
}

#' Write plain text to file
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
#' If submission is a zip file, it unzips the contents, otherwise it copies
#' the file over to `path`
#'
#' @param pars List of features defining an assignment
#' @param roster Class roster (data frame)
#' @param junkpathsSetting Ignore the file structure in the zipped file?
#' Defaults to `FALSE`.
#' @export
unzip_submissions <- function(pars, roster, junkpathsSetting = FALSE) {

    # Create unzipped submissions folder if it does't exist
    path <- here::here("assignments", pars$assign, "submissions")
    make_dir(here::here("assignments", pars$assign, "unzipped"))

    # Unzip or copy each file
    ids <- roster |>
        dplyr::filter(enrolled == 1) |>
        dplyr::mutate(gwid = stringr::str_to_lower(gwid)) |>
        dplyr::select(netID, gwid)
    files <- file.path(path, list.files(path))
    cat("Missing:\n")
    for (i in 1:nrow(ids)) {
        id <- ids[i,]
        studentFiles <- c(
            files[which(stringr::str_detect(files, id$netID))],
            files[which(stringr::str_detect(files, id$gwid))]
        )
        if (length(studentFiles) == 0) {
            cat(id$netID, '\n')
            next()
        }
        dest <- here::here("assignments", pars$assign, "unzipped", id$netID)
        make_dir(dest)
        for (j in 1:length(studentFiles)) {
            import_file(studentFiles[j], dest, junkpathsSetting)
        }
    }
}

get_enrolled_ids <- function(roster) {
    result <- roster |>
        dplyr::filter(enrolled == 1) |>
        dplyr::pull(netID)
    return(result)
}

import_file <- function(file, destination, junkpathsSetting) {

    # If it's a zip file, unzip it to the destination

    if (fs::path_ext(stringr::str_to_lower(file)) == 'zip') {

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
