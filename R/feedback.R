#' Make pdf reports
#'
#' Make pdf reports of assignment feedback
#'
#' @param pars Parameters defining assignment
#' @param template qmd template for making report.
#' @param studentID Individual studentID to make report for.
#' Defaults to `NULL` in which case reports are made for all students.
#' @export
make_reports <- function(pars, template, netID = NULL) {
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

get_report_name <- function(pars, netID) {
    return(paste(netID, pars$category, pars$number, 'feedback.pdf', sep = "_"))
}

get_report_path <- function(pars, netID = NULL) {
    report_name <- get_report_name(pars, netID)
    return(here::here(pars$category, pars$number, 'feedback', report_name))
}

#' Update feedback for all assignments
#'
#' Update feedback for all assignments in Box folders
#'
#' @param assignments Assignments data frame
#' @param roster Course roster
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
