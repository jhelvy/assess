#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
make_assessment_indiv <- function(pars, roster) {

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

#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
get_missing <- function(assessment) {
    missing <- assessment %>%
        filter(is.na(days_late)) %>%
        distinct(netID) %>%
        pull(netID)
    return(paste(missing, collapse = ", "))
}

#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
update_submissions_team <- function(roster, path_box) {

    # Get all the individual assignment due dates
    due <- get_due_dates() %>%
        mutate(
            category = str_replace(category, 'project-', ''),
            assign = str_sub(stub, 3, str_length(stub))
        ) %>%
        # Drop individual assignments
        filter(category == 'final')

    # Create a data frame of all submissions
    df <- roster %>%
        filter(enrolled == 1) %>%
        group_by(team) %>%
        slice(1) %>%
        mutate(path = file.path(
            path_box, '0teams', paste0('madd-f22-', team), 'submissions'))

    submissions <- list()
    index <- 1
    for (i in 1:nrow(df)) {
        row <- df[i,]
        files <- file.path(row$path, list.files(row$path))
        file_names <- unlist(lapply(files, basename))
        if (length(files) == 0) { next }
        temp <- data.frame(
            team = row$team,
            file = files,
            file_name = file_names) %>%
            separate(file_name, into = c('assign', 'drop'), sep = '.zip') %>%
            select(-drop) %>%
            mutate(submitted = as.Date(file.info(file)$mtime))
        submissions[[index]] <- temp
        index <- index + 1
    }
    submissions <- do.call(rbind, submissions)

    # Check if the submissions are late
    result <- submissions %>%
        left_join(due, by = "assign") %>%
        mutate(days_late = as.numeric(submitted - due)) %>%
        arrange(category, number, days_late)

    # Save the result
    write_csv(result, file.path('data', 'submissions_team.csv'))
}

#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
make_assessment_team <- function(pars, roster) {

    # Import submissions
    df <- suppressMessages(
        read_csv(here::here("data", "submissions_team.csv")) %>%
            filter(category == pars$category, assign == pars$number)
    )

    # Assign 1 if submitted at all and 0 if missing
    result <- roster %>%
        filter(!is.na(team)) %>%
        distinct(team) %>%
        left_join(df %>% select(team, days_late), by = 'team') %>%
        mutate(assessment = ifelse(is.na(days_late), 0, 1), feedback = "") %>%
        arrange(team)

    # Add rows for each question
    temp <- list()
    for (i in 1:nrow(result)) {
        temp[[i]] <- result[i,] %>% cbind(pars$weights)
    }
    result <- do.call(rbind, temp) %>%
        arrange(team) %>%
        mutate(assessment = "") %>%
        select(team, days_late, question, maxPoints, assessment, feedback)

    # Save template
    write_csv(result, here::here(pars$category, pars$number, 'assessment_temp.csv'))
}

#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
make_team_indiv_assessment <- function(pars, roster) {

    # Assign 1 if submitted at all and 0 if missing
    result <- roster %>%
        filter(enrolled == 1) %>%
        select(netID, name, team) %>%
        arrange(netID) %>%
        mutate(
            question = "Individual Contributions",
            maxPoints = 8,
            assessment = 8,
            feedback = ""
        ) %>%
        arrange(team, netID)

    # Save template
    write_csv(result, here::here(pars$category, pars$number, 'assessment_indiv.csv'))
}
