#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
make_reports <- function(pars, netID = NULL, template = NULL) {
    if (is.null(template)) {
        template <- here::here('templates', paste0(pars$category, '.Rmd'))
    }
    # Create feedback folder (if doesn't exist)
    make_dir(here::here(pars$category, pars$number, 'feedback'))
    # Get grades
    grades <- get_grades(pars)
    if (!is.null(netID)) {
        # Build only the one report for the provided netID
        build_report(grades, pars, template, netID)
    } else {
        # Build report for each netID
        netIDs <- unique(grades$netID)
        for (i in seq(length(netIDs))) {
            if (! (netIDs[i] %in% roster$netID)) { next }
            build_report(grades, pars, template, netIDs[i])
        }
    }
}

#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
build_report <- function(grades, pars, template, netID_i) {
    df <- filter(grades, netID == netID_i)
    output_file <- get_report_path(pars, netID_i)
    student <- unique(df$name)
    params_temp <- list(
        df      = df,
        title   = pars$title,
        student = student)
    if (pars$feedback == "pdf") {
        rmarkdown::render(
            input       = template,
            output_file = output_file,
            params      = params_temp)
    } else {
        pagedown::chrome_print(rmarkdown::render(
            input       = template,
            output_file = tempfile(fileext = "html"),
            params      = params_temp),
            output_file
        )
    }
}

#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
make_reports_team <- function(pars, roster, netID = NULL, template = NULL) {
    if (is.null(template)) {
        template <- here::here('templates', paste0(pars$category, '.Rmd'))
    }
    # Create feedback folder (if doesn't exist)
    make_dir(here::here(pars$category, pars$number, 'feedback'))
    # Get grades
    grades <- get_grades_team(pars, roster)
    if (!is.null(netID)) {
        # Build only the one report for the provided netID
        build_report(grades, pars, template, netID)
    } else {
        # Build report for each netID
        netIDs <- unique(grades$netID)
        for (i in seq(length(netIDs))) {
            if (! (netIDs[i] %in% roster$netID)) { next }
            build_report(grades, pars, template, netIDs[i])
        }
    }
}

#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
build_report_team <- function(grades, pars, template, team_i) {
    df <- filter(grades, team == team_i)
    output_file <- get_report_path(pars, team_i)
    params_temp <- list(
        df    = df,
        title = pars$title,
        team = team_i)
    if (pars$feedback == "pdf") {
        rmarkdown::render(
            input       = template,
            output_file = output_file,
            params      = params_temp)
    } else {
        pagedown::chrome_print(rmarkdown::render(
            input       = template,
            output_file = tempfile(fileext = "html"),
            params      = params_temp),
            output_file
        )
    }
}

#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
get_report_name <- function(pars, netID) {
    return(paste(netID, pars$category, pars$number, 'feedback.pdf', sep = "_"))
}

#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
get_report_path <- function(pars, netID = NULL) {
    report_name <- get_report_name(pars, netID)
    return(here::here(pars$category, pars$number, 'feedback', report_name))
}

#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
update_feedback <- function(assignments, roster) {
    categories <- unique(assignments$category)
    for (i in 1:nrow(assignments)) {
        assign <- assignments[i,]
        for (j in 1:nrow(roster)) {
            student <- roster[j,]
            report_path <- get_report_path(assign, student$netID)
            if (file.exists(report_path)) {
                print(paste0(
                    assign$category, assign$number, "-", student$netID)
                )
                file.copy(
                    from = report_path,
                    to = file.path(
                        path_box, student$box, 'feedback',
                        get_report_name(assign, student$netID)),
                    overwrite = TRUE
                )
            }
        }
    }
}

#' Title
#'
#' Description
#'
#' @param arg description
#' @return Description
#' @export
update_feedback_team <- function(assignments, roster) {
    teams <- roster %>%
        filter(enrolled == 1) %>%
        distinct(team) %>%
        pull(team)
    assignments_temp <- assignments %>%
        filter(category == 'final')
    for (i in 1:nrow(assignments_temp)) {
        assign <- assignments_temp[i,]
        for (j in 1:length(teams)) {
            team <- teams[j]
            report_path <- get_report_path(assign, team)
            if (file.exists(report_path)) {
                print(paste(assign$category, assign$number, team, sep = "-"))
                file.copy(
                    from = report_path,
                    to = file.path(
                        path_box, '0teams', paste0('madd-f22-', team), 'feedback',
                        get_report_name(assign, team)),
                    overwrite = TRUE
                )
            }
        }
    }
}

