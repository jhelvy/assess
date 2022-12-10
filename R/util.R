
#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
make_dir <- function(path) {
    if (!dir.exists(path)) { dir.create(path) }
}

#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
save_raw <- function(text, path) {
    fileConn <- file(path)
    writeLines(text, fileConn)
    close(fileConn)
}

#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
get_due_dates <- function() {
    schedule <- get_schedule()
    # get_schedule() From here:
    # https://raw.githubusercontent.com/emse-eda-gwu/2022-Fall/main/R/settings.R
    assign <- schedule %>%
        select(starts_with("assign")) %>%
        filter(!is.na(assign_n)) %>%
        mutate(category = 'hw') %>%
        select(category, assign_n, assign_name, assign_stub, assign_due)
    final <- schedule %>%
        select(starts_with("final")) %>%
        filter(!is.na(final_n)) %>%
        mutate(category = 'project-final') %>%
        select(category, final_n, final_name, final_stub, final_due)
    exam <- schedule %>%
        filter(assign_stub == 'exam') %>%
        mutate(category = 'exam', number = 'exam') %>%
        select(category, number, assign_name, assign_stub, assign_due)
    quiz <- data.frame(
        category = 'quiz',
        assign_n = seq(5)) %>%
        mutate(
            assign_name = paste0('Quiz ', assign_n),
            assign_stub = paste0(assign_n, '-quiz'),
            assign_due = '2022-09-15')
    names(assign) <- c('category', 'number', 'name', 'stub', 'due')
    names(final) <- c('category', 'number', 'name', 'stub', 'due')
    names(quiz) <- c('category', 'number', 'name', 'stub', 'due')
    names(exam) <- c('category', 'number', 'name', 'stub', 'due')
    due <- rbind(assign, final, quiz, exam)
    return(due)
}
