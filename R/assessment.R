#' Make blank assessment.csv file
#'
#' @param pars Parameters defining assignment
#' @param roster Course roster data frame
#' @export
make_assessment <- function(pars, roster) {
    enrolled <- netID <- name <- order <- question <- assessment <- feedback <- NULL

    result <- roster |>
        dplyr::filter(enrolled == 1) |>
        dplyr::select(netID, name)

    temp <- list()
    for (i in 1:length(pars$weights$question)) {
        temp[[i]] <- result |>
            dplyr::mutate(
                question = pars$weights$question[i],
                order = i
            )
    }
    result <- do.call(rbind, temp) |>
        dplyr::arrange(netID, order) |>
        dplyr::mutate(assessment = "", feedback = "") |>
        dplyr::select(-order) |>
        dplyr::select(netID, name, question, assessment, feedback)

    # Save template
    readr::write_csv(result,
        here::here('assignments', pars$assign, 'assessment_temp.csv'))
}

#' Get assessment data frame
#'
#' @param pars List of parameters defining assignment
#' @export
get_assessment <- function(pars) {
    assessment <- readxl::read_excel(
        here::here('data', 'gradebook.xlsx'),
        sheet = pars$assign
    )
    return(assessment)
}
