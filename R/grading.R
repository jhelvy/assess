
#' Save all grades to file
#'
#' @param pars Parameters defining assignment
#' @export
save_grades <- function(pars, roster) {
    netID <- grade <- name <- NULL

    get_grades(pars, roster) |>
        dplyr::distinct(name, netID, grade) |>
        readr::write_csv(here::here(
            "assignments", pars$assign, "grades.csv"))
}

#' Get grades for an assignments
#'
#' @param pars List of parameters defining assignment
#' @param roster Course roster data frame
#' @export
get_grades <- function(pars, roster) {
    assessment <- question <- weight <- enrolled <- netID <- score <- NULL

    assessment <- get_assessment(pars)
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
        dplyr::filter(! is_bonus(question)) |>
        dplyr::group_by(netID) |>
        dplyr::summarise(score = sum(score)) |>
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
        dplyr::mutate(bonus = ifelse(score == 0, 0, weight)) |>
        dplyr::select(netID, bonus) |>
        dplyr::group_by(netID) |>
        dplyr::summarise(bonus = sum(bonus)) |>
        dplyr::right_join(df, by = 'netID') |>
        dplyr::mutate(score = score + bonus) |>
        dplyr::select(netID, score)
    return(result)
}

#' Get all grades data frame
#'
#' Data frame of all assignments
#'
#' @param assignments Data frame of all assignments
#' @param roster Course roster data frame
#' @export
get_all_grades <- function(assignments, roster) {
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
        temp$weight <- row$weight
        temp$order <- row$order
        grades[[i]] <- temp
    }
    # Merge
    grades <- do.call(rbind, grades) |>
        dplyr::arrange(netID, order)
    return(grades)
}

#' Get letter grade from grade
#'
#' @param x Grade
#' @export
getLetter <- function(x) {
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

#' Save final grades to csv
#'
#' Saves final grades to a csv file located at `file`
#'
#' @param assignments Data frame of all assignments
#' @param roster Course roster data frame
#' @param drop Which assignments to drop from grade computation. Should be a
#' named vector defining the category and number to drop.
#' @param file File name where to save grades as csv
#' @export
save_final_grades <- function(
    assignments, roster, drop = NULL, file = 'grades.csv'
) {

    netID <- n <- category <- grade <- grade_max <- grade_category <- weight <- weight_max <- weight_fill <-  NULL

    grades <- get_all_grades(assignments, roster)

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
            letter = getLetter(grade),
            grade = round(grade, 3),
            letter_max = getLetter(grade_max),
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
