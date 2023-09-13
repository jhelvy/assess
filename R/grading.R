
#' Save all grades to file
#'
#' @param pars Parameters defining assignment
#' @export
save_grades <- function(pars) {
    netID <- grade <- NULL

    get_grades(pars) |>
        distinct(name, netID, grade) |>
        write_csv(here::here(
            "assignments", pars$assign, "grades.csv"))
}

#' Get grades for an assignments
#'
#' @param pars List of parameters defining assignment
#' @param roster Course roster data frame
#' @export
get_grades <- function(pars, roster) {
    assessment <- question <- weight <- enrolled <- NULL

    assessment <- get_assessment(pars)
    if (pars$weighted == FALSE) {
        return(get_grades_unweighted(assessment, pars))
    }
    scores <- assessment |>
        mutate(question = as.character(question)) |>
        left_join(pars$weights, by = 'question') |>
        filter(!is.na(weight)) |>
        rename(score = assessment)
    bonus <- scores |>
        filter(str_detect(question, 'bonus'))
    grades <- scores |>
        filter(! str_detect(question, 'bonus')) |>
        group_by(netID) |>
        summarise(grade = weighted.mean(score, weight)) |>
        add_bonus(bonus)
    grades$score <- NULL
    grades <- scores |>
        left_join(grades, by = 'netID') |>
        full_join(select(roster, netID, enrolled), by = "netID") |>
        filter(enrolled == 1)
    return(grades)
}

#' Get assessment data frame
#'
#' @param pars List of parameters defining assignment
#' @export
get_assessment <- function(pars) {
    assessment <- read_excel(
        here::here('data', 'gradebook.xlsx'),
        sheet = pars$assign
    )
    return(assessment)
}

get_grades_unweighted <- function(assessment, pars) {
    netID <- NULL

    grades <- assessment |>
        group_by(netID) |>
        mutate(grade = sum(assessment) / pars$maxScore)
    return(grades)
}

add_bonus <- function(df, bonus) {
    score <- weight <- netID <- grade <- NULL

    if (nrow(bonus) == 0) { return(df) }
    result <- bonus |>
        mutate(score = ifelse(score == 1, weight, 0)) |>
        select(netID, score) |>
        group_by(netID) |>
        summarise(score = sum(score)) |>
        right_join(df, by = 'netID') |>
        mutate(grade = grade + score)
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
            temp <- suppressMessages(read_csv(row$path))
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
        arrange(netID, order)
    return(grades)
}

#' Get letter grade from grade
#'
#' @param x Grade
#' @export
getLetter <- function(x) {
    scale <- tribble(
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

    netID <- grade <- grade_max <- grade_category <- weight <- weight_max <- weight_fill <-  NULL

    grades <- get_all_grades(assignments, roster)

    # Drop lowest assignments
    if (!is.null(drop)) {
        for (i in 1:length(drop)) {
            grades <- grades |>
                group_by(netID) |>
                drop_lowest(names(drop[i]), drop[i], assignments)
        }
    }

    # Compute grade for each category
    result <- grades |>
        group_by(netID, category) |>
        mutate(
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

    # Set the weight by assignment
    result <- result |>
        group_by(netID, category) |>
        mutate(weight = weight / n())
    result$weight_max <- result$weight

    # If any grades are still missing, then distribute their weight
    # across the other categories
    temp <- result |>
        mutate(weight_fill = ifelse(is.na(grade), weight, 0)) |>
        group_by(netID) |>
        summarise(weight_fill = sum(weight_fill))
    result <- result |>
        left_join(temp, by = 'netID') |>
        group_by(netID) |>
        mutate(
            missing = is.na(grade),
            grade = ifelse(missing, 0, grade),
            weight = ifelse(missing, 0, weight),
            weight_fill = ifelse(missing, 0, weight_fill)
        ) |>
        group_by(netID, missing) |>
        mutate(weight = weight + (weight_fill / n()))

    # Compute grades

    result <- result |>
        group_by(netID) |>
        summarise(
            grade = sum(weight*grade),
            grade_max = sum(weight_max*grade_max)
        ) |>
        mutate(
            grade_max = ifelse(grade > grade_max, grade, grade_max),
            letter = getLetter(grade),
            grade = round(grade, 3),
            letter_max = getLetter(grade_max),
            grade_max = round(grade_max, 3)
        ) |>
        arrange(desc(grade))
    write_csv(result, file)
}

drop_lowest <- function(df, cat, number, assignments) {
    category <- cat <- netID <- is_cat <- grade <- number <- NULL

    result <- df |>
        mutate(is_cat = ifelse(category == cat, 1, 0)) |>
        arrange(netID, desc(is_cat), grade) |>
        slice(-(1:number)) |>
        select(-is_cat)
    return(result)
}
