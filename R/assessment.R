#' Makd blank assessment.csv file
#'
#' @param pars Parameters defining assignment
#' @param roster Course roster data frame
#' @param id Unique identifier for student or group / team
#' @export
make_assessment <- function(pars, roster, id) {

    result <- roster %>%
        filter(enrolled == 1) %>%
        select({{id}}, name)

    temp <- list()
    for (i in 1:length(pars$weights$question)) {
        temp[[i]] <- result %>%
            mutate(
                question = pars$weights$question[i],
                order = i
            )
    }
    result <- do.call(rbind, temp) %>%
        arrange({{id}}, order) %>%
        mutate(assessment = "", feedback = "") %>%
        select(-order) %>%
        select({{id}}, name, question, assessment, feedback)

    # Save template
    write_csv(result, here::here('assignments', pars$assign, 'assessment_temp.csv'))
}
