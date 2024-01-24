#' Make pdf reports of assignment feedback
#'
#' @param pars Parameters defining assignment
#' @param roster Course roster data frame
#' @param template qmd template for making report.
#' @param indiv One specific id (to build just one report instead of all)
#' Defaults to `NULL` in which case reports are made for all students.
#' @export
make_reports <- function(pars, roster, template, indiv = NULL) {
    # Create feedback folder (if doesn't exist)
    make_dir(here::here('assignments', pars$assign, 'feedback'))
    # Get grades
    grades <- get_assignment_grades(pars, roster)
    if (!is.null(indiv)) {
        # Build only the one report for the provided netID
        build_report(grades, pars, template, indiv)
    } else {
        # Build report for each id
        ids <- get_enrolled_ids(roster)
        for (i in seq(length(ids))) {
            build_report(grades, pars, template, indiv = ids[i])
        }
    }
}

build_report <- function(grades, pars, template, indiv) {
    netID <- NULL

    df <- dplyr::filter(grades, netID == indiv)
    output_file <- get_report_path(pars$assign, indiv)
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

get_report_path <- function(assign, indiv = NULL) {
    report_name <- paste0(indiv, "-", assign, '.pdf')
    return(here::here('assignments', assign, 'feedback', report_name))
}

#' Update feedback for all assignments
#'
#' Update feedback for all assignments in Box folders
#'
#' @param assignments Assignments data frame
#' @param roster Course roster data frame
#' @param path_box Path to root box folder
#' @export
update_feedback <- function(assignments, roster, path_box) {
    netID <- box_folder <- NULL

    enrolled <- roster |>
        dplyr::filter(enrolled == 1)
    ids <- enrolled |> dplyr::pull(netID)
    box_folders <- enrolled |> dplyr::pull(box_folder)
    for (i in 1:nrow(assignments)) {
        assign <- assignments$assign[i]
        for (j in 1:length(ids)) {
            id <- ids[j]
            box_folder <- box_folders[j]
            report_path <- get_report_path(assign, id)
            dest_path <- file.path(path_box, box_folder, basename(report_path))
            if (file.exists(report_path)) {
                print(paste0(assign, "-", id))
                file.copy(
                    from = report_path,
                    to = dest_path,
                    overwrite = TRUE
                )
            }
        }
    }
}

#' Update grades for all assignments
#'
#' Update grades for all assignments in Box folders
#'
#' @param assignments Assignments data frame
#' @param roster Course roster data frame
#' @param path_box Path to root box folder
#' @param drop Which assignments to drop from grade computation. Should be a
#' named vector defining the category and number to drop.
#' @export
update_grades <- function(
        assignments, roster, path_box, drop = NULL)
{

    netID <- category <- n <- grade <- weight <- score <- letter <- NULL

    grades_final <- readr::read_csv(here::here('grades', 'grades.csv'))
    grades <- get_all_grades(assignments, roster)

    # Drop lowest assignments
    if (!is.null(drop)) {
        drop_df <- as.data.frame(drop)
        drop_df$name <- row.names(drop_df)
        cat_count <- grades |>
            dplyr::count(netID, category) |>
            dplyr::left_join(drop_df, by = c("category" = "name")) |>
            dplyr::mutate(
                drop = ifelse(is.na(drop), 0, drop),
                n = n - drop
            ) |>
            dplyr::distinct(category, n)
    } else {
        cat_count <- grades |>
            dplyr::count(netID, category) |>
            dplyr::distinct(category, n)
    }

    grades_report <- grades |>
        dplyr::left_join(cat_count, by = 'category') |>
        dplyr::mutate(weight = weight / n) |>
        dplyr::select(netID, assignment = assign, score = grade, weight) |>
        dplyr::mutate(weight = round(weight, 3))

    for (i in 1:nrow(roster)) {
        row <- roster[i,]
        if (row$enrolled == 0) { next }

        # Write grades for each assignment
        temp_grades <- grades_report |>
            dplyr::filter(netID == row$netID) |>
            dplyr::select(-netID) |>
            dplyr::mutate(score = round(score, 3))
        readr::write_csv(
            temp_grades,
            file.path(path_box, row$box_folder, "grade_assignments.csv")
        )

        # Write running final grade
        temp <- grades_final |>
            dplyr::filter(netID == row$netID) |>
            dplyr::select(-netID)
        score <- dplyr::select(temp, grade, letter)
        max <- dplyr::select(temp, dplyr::ends_with("max"))
        names(max) <- names(score)
        temp_grade <- rbind(score, max)
        temp_grade$category <- c('Current:', 'Max possible:')
        temp_grade <- temp_grade |> dplyr::select(category, grade, letter)
        readr::write_csv(temp_grade, file.path(path_box, row$box_folder, "grade_course.csv"))
    }
}
