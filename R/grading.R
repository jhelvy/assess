
#' Save all grades to file
#'
#' @param pars Parameters defining assignment
#' @export
save_grades <- function(pars) {
    get_grades(pars) |>
        distinct(name, netID, grade) |>
        readr::write_csv(here::here(
            "assignments", pars$assign, "grades.csv"))
}

#' Get grades for an assignments
#'
#' @param pars List of parameters defining assignment
#' @param roster Course roster data frame
#' @return Description
#' @export
get_grades <- function(pars, roster) {
    path <- here::here("assignments", pars$assign, "assessment.csv")
    assessment <- readr::read_csv(path)
    if (pars$weighted == FALSE) {
        return(get_grades_unweighted(assessment, pars))
    }
    scores <- assessment |>
        dplyr::mutate(question = as.character(question)) |>
        dplyr::left_join(pars$weights, by = 'question') |>
        dplyr::filter(!is.na(weight)) |>
        rename(score = assessment)
    bonus <- scores |>
        dplyr::filter(stringr::str_detect(question, 'bonus'))
    grades <- scores |>
        dplyr::filter(! stringr::str_detect(question, 'bonus')) |>
        dplyr::group_by(netID) |>
        dplyr::summarise(grade = stats::weighted.mean(score, weight)) |>
        add_bonus(bonus)
    grades$score <- NULL
    grades <- scores |>
        dplyr::left_join(grades, by = 'netID') |>
        dplyr::full_join(
            dplyr::select(roster, netID, enrolled), by = "netID") |>
        dplyr::filter(enrolled == 1)
    return(grades)
}

get_grades_unweighted <- function(assessment, pars) {
    grades <- assessment |>
        dplyr::group_by(netID) |>
        dplyr::mutate(grade = sum(assessment) / pars$maxScore)
    return(grades)
}

add_bonus <- function(df, bonus) {
    if (nrow(bonus) == 0) { return(df) }
    result <- bonus |>
        dplyr::mutate(score = ifelse(score == 1, weight, 0)) |>
        dplyr::select(netID, score) |>
        dplyr::group_by(netID) |>
        dplyr::summarise(score = sum(score)) |>
        dplyr::right_join(df, by = 'netID') |>
        dplyr::mutate(grade = grade + score)
    return(result)
}

#' Get grades for all assignments
#'
#' @param assignments Data frame of all assignments
#' @param roster Course roster data frame
#' @param amg Include an Alternative Minimum Grade (AMG) grade?
#' @export
get_all_grades <- function(assignments, roster) {
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
        temp$weight_amg = row$weight_amg
        temp$order <- row$order
        grades[[i]] <- temp
    }
    # Merge
    grades <- do.call(rbind, grades) |>
        dplyr::arrange(netID, order)
    return(grades)
}

#' Convert numeric grade to letter
#'
#' @param x Numeric grade, 0 to 1.
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

#' Save final grades
#'
#' Writes final grades to disc as grades.csv
#'
#' @param assignments Data frame of all assignments
#' @param roster Course roster data frame
#' @param weights Column name defining weights to use in computing grade.
#' @param drop Which assignments to drop from grade computation. Should be a
#' named vector defining the category and number to drop.
#' @param file Name of file to save. Defaults to `'grades.csv'`.
#' @export
save_final_grades <- function(
    assignments, roster, weights, drop = NULL, file = 'grades.csv'
) {
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
            grade_category = mean(grade, na.rm = TRUE),
            # If no grade yet in category, replace with NA
            grade_category = ifelse(is.nan(grade_category), NA, grade_category)
        )

    # Insert mean category grade for missing assignments
    # (those not yet graded)
    missing <- which(is.na(result$grade))
    result[missing,]$grade <- result[missing,]$grade_category

    # For max score, insert 1 for missing assignments
    result$grade_max <- result$grade
    result[missing,]$grade_max <- 1

    # Set the weights to use
    result$weight <- dplyr::pull(result[weights])
    result <- result |>
        dplyr::group_by(netID, category) |>
        dplyr::mutate(weight = weight / n())
    result$weight_max <- result$weight

    # If any grades are still missing, then distribute their weight
    # across the other categories
    temp <- result |>
        dplyr::mutate(weight_fill = ifelse(is.na(grade), weight, 0)) |>
        dplyr::group_by(netID) |>
        dplyr::summarise(weight_fill = sum(weight_fill))
    result <- result |>
        dplyr::left_join(temp, by = 'netID') |>
        dplyr::group_by(netID) |>
        dplyr::mutate(
            missing = is.na(grade),
            grade = ifelse(missing, 0, grade),
            weight = ifelse(missing, 0, weight),
            weight_fill = ifelse(missing, 0, weight_fill)
        ) |>
        dplyr::group_by(netID, missing) |>
        dplyr::mutate(weight = weight + (weight_fill / n()))

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
        dplyr::arrange(desc(grade))
    readr::write_csv(result, file)
}

drop_lowest <- function(df, cat, number, assignments) {
    result <- df |>
        dplyr::mutate(is_cat = ifelse(category == cat, 1, 0)) |>
        dplyr::arrange(netID, desc(is_cat), grade) |>
        slice(-(1:number)) |>
        dplyr::select(-is_cat)
    return(result)
}

#' Update grades on Box
#'
#' @param assignments Assignments data frame
#' @param roster Course roster data frame
#' @param path_box Path to root box folder
#' @export
update_grades <- function(assignments, roster, path_box) {
    grades_final <- readr::read_csv(here::here('grades', 'grades.csv'))
    grades <- get_all_grades(assignments, roster)

    grades_report <- grades |>
        dplyr::group_by(netID, category) |>
        dplyr::mutate(weight = weight / n()) |>
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
            file.path(path_box, row$box, "grade_assignments.csv")
        )

        # Write running final grade
        temp <- grades_final |>
            dplyr::filter(netID == row$netID) |>
            dplyr::select(-netID)
        score <- dplyr::select(temp, grade, letter)
        amg <- dplyr::select(temp, dplyr::ends_with("amg"))
        max <- dplyr::select(temp, dplyr::ends_with("max"))
        names(amg) <- names(score)
        names(max) <- names(score)
        temp_grade <- rbind(score, amg, max)
        temp_grade$category <- c('Current:', 'AMG:', 'Max possible:')
        temp_grade <- temp_grade |> dplyr::select(category, grade, letter)
        readr::write_csv(
            temp_grade, file.path(path_box, row$box, "grade_course.csv"))
    }
}
