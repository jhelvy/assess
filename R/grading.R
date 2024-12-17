#' Update participation grade
#'
#' @param url url to google sheet gradebook.
#' @param roster Course roster data frame.
#' @param buffer Number of days as a buffer. Defaults to `1`
#' @export
grade_participation <- function(url, roster, buffer = 1) {
    netID <- present <- absent <- excused <- total <- grade <- enrolled <- name <- NULL
    attendance <- get_gradebook_assessment(list(assign = 'attendance'), url) |>
        dplyr::select(netID, present, absent, excused) |>
        dplyr::mutate(
            total = present + absent + excused,
            grade = (present + excused + buffer) / (total + buffer)
        ) |>
        dplyr::select(netID, grade) |>
        dplyr::left_join(roster, by = 'netID') |>
        dplyr::filter(enrolled == 1) |>
        dplyr::select(netID, grade, name)
    readr::write_csv(attendance, file.path('grades', 'participation.csv'))
}

#' Save all grades to file
#'
#' @param pars Parameters defining assignment
#' @param roster Course roster data frame
#' @export
save_assignment_grades <- function(pars, roster) {
    netID <- grade <- name <- NULL

    get_assignment_grades(pars, roster) |>
        dplyr::distinct(name, netID, grade) |>
        readr::write_csv(here::here("grades", paste0(pars$assign, ".csv")))
}

#' Get grades for an assignments
#'
#' @param pars List of parameters defining assignment
#' @param roster Course roster data frame
#' @export
get_assignment_grades <- function(pars, roster) {
    assessment <- question <- weight <- enrolled <- netID <- score <- NULL

    assessment <- get_local_assessment(pars)
    if (is.null(pars$weights)) {
        return(get_grades_unweighted(assessment, pars))
    }
    maxScore <- pars$weights |>
        dplyr::filter(! is_bonus(question)) |>
        dplyr::pull(weight) |>
        sum()
    scores <- assessment |>
        dplyr::mutate(question = as.character(question)) |>
        dplyr::left_join(pars$weights, by = 'question') |>
        dplyr::filter(!is.na(weight)) |>
        dplyr::rename(score = assessment)
    bonus <- scores |>
        dplyr::filter(is_bonus(question))
    grades <- scores |>
        dplyr::filter(! is_bonus(question))
    if (!pars$percent) {
        grades$weight <- 1
        bonus$weight <- 1
    }
    grades <- grades |>
        dplyr::group_by(netID) |>
        dplyr::summarise(score = sum(score*weight)) |>
        add_bonus(bonus) |>
        dplyr::group_by(netID) |>
        dplyr::summarise(grade = sum(score) / maxScore)
    grades$score <- NULL
    grades <- scores |>
        dplyr::left_join(grades, by = 'netID') |>
        dplyr::full_join(dplyr::select(roster, netID, enrolled), by = "netID") |>
        dplyr::filter(enrolled == 1)
    return(grades)
}

get_grades_unweighted <- function(assessment, pars) {
    netID <- NULL

    grades <- assessment |>
        dplyr::group_by(netID) |>
        dplyr::mutate(grade = sum(assessment) / pars$maxScore)
    return(grades)
}

is_bonus <- function(question) {
    return(stringr::str_detect(stringr::str_to_lower(question), 'bonus'))
}

add_bonus <- function(df, bonus) {
    score <- weight <- netID <- grade <- NULL

    if (nrow(bonus) == 0) { return(df) }
    result <- bonus |>
        dplyr::mutate(bonus = ifelse(score == 0, 0, score)) |>
        dplyr::select(netID, bonus, weight) |>
        dplyr::group_by(netID) |>
        dplyr::summarise(bonus = sum(bonus*weight)) |>
        dplyr::right_join(df, by = 'netID') |>
        dplyr::mutate(score = score + bonus) |>
        dplyr::select(netID, score)
    return(result)
}

#' Get letter grade from grade
#'
#' @param x Grade
#' @export
get_letter <- function(x) {
    scale <- tibble::tribble(
        ~letter, ~bound,
        'A',  0.94,
        'A-', 0.90,
        'B+', 0.87,
        'B',  0.84,
        'B-', 0.80,
        'C+', 0.77,
        'C',  0.74,
        'C-', 0.70,
        'D+', 0.67,
        'D',  0.64,
        'D-', 0.60,
        'F',  -1.0)
    letters <- c()
    thresholds <- scale$bound
    for (i in seq(length(x))) {
        mark <- thresholds[which(x[i] >= thresholds)][1]
        letters[i] <- scale[which(thresholds == mark),]$letter
    }
    return(letters)
}

#' Get all grades data frame
#'
#' Data frame of all assignments
#'
#' @param assignments Data frame of all assignments
#' @param roster Course roster data frame
#' @param weights The column in assignments to use for weighting
#' @export
get_all_grades <- function(assignments, roster, weights) {
    netID <- order <- NULL

    ids <- get_enrolled_ids(roster)
    missing_grades <- data.frame(netID = ids, grade = NA)
    grades <- list()
    for (i in 1:nrow(assignments)) {
        row <- assignments[i,]
        if (file.exists(row$path)) {
            temp <- suppressMessages(readr::read_csv(row$path))
            temp$missing <- NULL
            temp$name <- NULL
        } else {
            temp <- missing_grades
        }
        temp$category <- row$category
        temp$assign <- row$assign
        temp$weight <- row[weights] |> dplyr::pull()
        temp$order <- row$order
        grades[[i]] <- temp
    }

    # Merge
    grades <- do.call(rbind, grades) |>
        dplyr::arrange(netID, order)

    return(grades)
}

#' Save final grades to csv
#'
#' Saves final grades to a csv file located at `file`
#'
#' @param assignments Data frame of all assignments
#' @param roster Course roster data frame
#' @param weights The column in assignments to use for weighting
#' @param drop Which assignments to drop from grade computation. Should be a
#' named vector defining the category and number to drop.
#' @param file File name where to save grades as csv
#' @export
save_final_grades <- function(
    assignments, roster, weights = 'weight', drop = NULL, file = 'grades.csv'
) {

    netID <- n <- category <- grade <- grade_max <- grade_category <- weight <- weight_max <- weight_fill <-  NULL

    grades <- get_all_grades(assignments, roster, weights)

    # Drop lowest assignments
    if (!is.null(drop)) {
        for (i in 1:length(drop)) {
            grades <- grades |>
                dplyr::group_by(netID) |>
                drop_lowest(names(drop[i]), drop[i], assignments)
        }
    }

    # Compute grade for each category
    result <- grades |>
        dplyr::group_by(netID, category) |>
        dplyr::mutate(
            # Store which grades are missing
            missing = is.na(grade),
            # Set the weight by assignment
            weight = weight / n(),
            grade = ifelse(missing, 0, grade),
            grade_max = ifelse(missing, 1, grade)
        ) |>
        # If any grades are still missing, distribute their weight
        # proportionally across the other categories that aren't missing
        dplyr::group_by(netID) |>
        dplyr::mutate(
            weight_max = weight,
            weight = ifelse(missing, 0, weight),
            weight = weight / sum(weight)
        )

    # Compute grades
    result <- result |>
        dplyr::group_by(netID) |>
        dplyr::summarise(
            grade = sum(weight*grade),
            grade_max = sum(weight_max*grade_max)
        ) |>
        dplyr::mutate(
            grade_max = ifelse(grade > grade_max, grade, grade_max),
            letter = get_letter(grade),
            grade = round(grade, 3),
            letter_max = get_letter(grade_max),
            grade_max = round(grade_max, 3)
        ) |>
        dplyr::arrange(dplyr::desc(grade))
    readr::write_csv(result, file)
}

drop_lowest <- function(df, cat, number, assignments) {
    category <- netID <- is_cat <- grade <- NULL

    result <- df |>
        dplyr::mutate(is_cat = ifelse(category == cat, 1, 0)) |>
        dplyr::arrange(netID, dplyr::desc(is_cat), grade) |>
        dplyr::slice(-(1:number)) |>
        dplyr::select(-is_cat)
    return(result)
}

#' Update grades for all assignments
#'
#' Update grades for all assignments in Box folders
#'
#' @param assignments Assignments data frame
#' @param roster Course roster data frame
#' @param weights The column in assignments to use for weighting
#' @param drop Which assignments to drop from grade computation. Should be a
#' named vector defining the category and number to drop.
#' @param path_box Path to root box folder
#' @export
update_grades <- function(assignments, roster, weights, path_box, drop = NULL)
{

    netID <- category <- n <- grade <- weight <- score <- letter <- NULL

    grades_final <- readr::read_csv(here::here('grades', 'grades_final.csv'))
    grades <- get_all_grades(assignments, roster, weights)

    # Account for any dropped assignments
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

#' Copy grades from one team member to all others
#'
#' @param df Assessment with one graded team member
#' @param roster Course roster data frame
#' @export
copy_team_grades <- function(df, roster) {

    netID <- team <- enrolled <- NULL

    copy_grades <- function(df) {
        for (q in unique(df$question)) {
            # Find the first non-NA assessment and feedback for each question
            first_assessment <- stats::na.omit(df$assessment[df$question == q])[1]
            first_feedback <- stats::na.omit(df$feedback[df$question == q])[1]

            # If a non-NA value is found, copy it to all rows for this question
            if (!is.na(first_assessment)) {
                df$assessment[df$question == q] <- first_assessment
                df$feedback[df$question == q] <- first_feedback
            }
        }
        return(df)
    }
    df <- df |>
        dplyr::left_join(
            roster |> dplyr::select(netID, team),
            by = 'netID'
        ) |>
        dplyr::group_by(team) |>
        dplyr::group_modify(~copy_grades(.x))

    return(df)
}
