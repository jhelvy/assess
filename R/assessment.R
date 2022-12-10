#' Title
#'
#' Description
#'
#' @param pars Parameters defining assignment
#' @param roster Course roster
#' @export
make_assessments <- function(pars, roster) {

    # Import submissions
    df <- suppressMessages(
        read_csv(here::here("data", "submissions_indiv.csv")) %>%
        filter(category == pars$category, number == pars$number)
    )

    # Assign 1 if submitted at all and 0 if missing
    result <- roster %>%
        filter(enrolled == 1) %>%
        select(netID, name) %>%
        left_join(df %>% select(netID, days_late), by = 'netID') %>%
        mutate(assessment = ifelse(is.na(days_late), 0, 1), feedback = "") %>%
        arrange(netID)

    # Add rows for each question (if applicable)
    if (pars$category != 'hw') {
        temp <- list()
        for (i in 1:length(pars$weights$question)) {
            temp[[i]] <- result %>%
                mutate(
                    question = pars$weights$question[i],
                    order = i
                )
        }
        result <- do.call(rbind, temp) %>%
            arrange(netID, order) %>%
            mutate(assessment = "") %>%
            select(-order)
        # Print missing submissions to console
        print(paste0("Missing submissions from: ", get_missing(result)))
        result <- result %>%
            select(netID, name, question, assessment, feedback)
    }
    # Save template
    write_csv(result, here::here(pars$category, pars$number, 'assessment_temp.csv'))
}

get_missing <- function(assessment) {
    missing <- assessment %>%
        filter(is.na(days_late)) %>%
        distinct(netID) %>%
        pull(netID)
    return(paste(missing, collapse = ", "))
}
