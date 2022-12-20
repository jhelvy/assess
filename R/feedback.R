#' Make pdf reports of assignment feedback
#'
#' @param pars Parameters defining assignment
#' @param roster Course roster data frame
#' @param template qmd template for making report.
#' @param id Unique identifier for student or group / team
#' @param indiv One specific id (to build just one report instead of all)
#' Defaults to `NULL` in which case reports are made for all students.
#' @export
make_reports <- function(pars, roster, template, id, indiv = NULL) {
    # Create feedback folder (if doesn't exist)
    make_dir(here::here('assignments', pars$assign, 'feedback'))
    # Get grades
    grades <- get_grades(pars)
    if (!is.null(indiv)) {
        # Build only the one report for the provided id
        build_report(grades, pars, template, id, indiv)
    } else {
        # Build report for each id
        ids <- pull(grades, {{id}})
        for (i in seq(length(ids))) {
            build_report(grades, pars, template, id, indiv = ids[i])
        }
    }
}

build_report <- function(grades, pars, template, id, indiv) {
    df <- filter(grades, {{id}} == indiv)
    output_file <- get_report_path(pars, indiv)
    params_temp <- list(
        df    = df,
        title = pars$title,
        name  = unique(df$name)
    )
    pagedown::chrome_print(rmarkdown::render(
        input       = template,
        output_file = tempfile(fileext = "html"),
        params      = params_temp),
        output_file
    )
}

get_report_path <- function(pars, indiv = NULL) {
    report_name <- paste0(indiv, "-", pars$assign, '.pdf')
    return(here::here('assignments', pars$assign, 'feedback', report_name))
}

#' Update feedback for all assignments
#'
#' Update feedback for all assignments in Box folders
#'
#' @param assignments Assignments data frame
#' @param roster Course roster
#' @param id Unique identifier for student or group / team
#' @export
update_feedback <- function(assignments, roster, id) {
    categories <- unique(assignments$category)
    ids <- get_enrolled_ids(roster, id)
    for (i in 1:nrow(assignments)) {
        assign <- assignments[i,]
        for (j in 1:nrow(roster)) {
            id <- ids[j]
            report_path <- get_report_path(assign, id)
            if (file.exists(report_path)) {
                print(paste0(assign$assign, "-", id))
                file.copy(
                    from = report_path,
                    to = file.path(
                        path_box, student$box,
                        basename(report_path)),
                    overwrite = TRUE
                )
            }
        }
    }
}
