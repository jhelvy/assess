
#' Title
#'
#' @param pars Parameters defining assignment
#' @export
save_grades <- function(pars) {
    get_grades(pars) %>%
        distinct(name, netID, grade) %>%
        write_csv(here::here(
            pars$category, pars$number, 'grades.csv'))
}

#' Title
#'
#' @param pars Parameters defining assignment
#' @return Description
#' @export
get_grades <- function(pars) {
    path <- here::here(pars$category, pars$number, "assessment.csv")
    assessment <- read_csv(path)
    if (pars$grading == "raw") {
        return(get_grades_raw(assessment, pars))
    }
    scores <- assessment %>%
        mutate(question = as.character(question)) %>%
        left_join(pars$scale, by = 'assessment') %>%
        left_join(pars$weights, by = 'question') %>%
        filter(!is.na(weight))
    if ("graded" %in% names(scores)) {
        scores <- scores %>%
            filter(graded == 1) %>%
            select(-graded)
    }
    bonus <- scores %>%
        filter(str_detect(question, 'bonus'))
    grades <- scores %>%
        filter(! str_detect(question, 'bonus')) %>%
        group_by(netID) %>%
        summarise(grade = weighted.mean(score, weight)) %>%
        add_bonus(bonus) %>%
        select(netID, grade) %>%
        right_join(scores, by = 'netID') %>%
        full_join(select(roster, netID, enrolled), by = "netID") %>%
        filter(enrolled == 1) %>%
        mutate(
            missing = ifelse(is.na(grade), 1, 0),
            grade = ifelse(missing == 1, 0, grade)) %>%
        arrange(desc(missing))
    return(grades)
}

#' Title
#'
#' Description
#'
#' @param assignments Data frame of all assignments
#' @param roster Course roster data frame
#' @param id Unique identifier for student or group / team
#' @param amg Include an Alternative Minimum Grade (AMG) grade?
#' @export
get_all_grades <- function(assignments, roster, id, amg = FALSE) {
    ids <- get_enrolled_ids(roster, id)
    missing_grades <- data.frame(id = ids, grade = NA)
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
        temp$weight_category <- row$weight_category
        if (amg) {
            temp$weight_amg = row$weight_amg,
            temp$weight_category_amg = row$weight_category_amg
        }
        grades[[i]] <- temp
    }
    # Merge
    grades <- do.call(rbind, grades) %>%
        mutate(assignment = paste(category, number, sep = "_")) %>%
        arrange({{id}})
    return(grades)
}

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

#' Title
#'
#' Description
#'
#' @param grades Data frame of all grades for each item
#' @param id Unique identifier for student or group / team
#' @param drop Which assignments to drop from grade computation. Should be a
#' named vector defining the category and number to drop.
#' @export
compute_grade <- function(grades, id, drop = NULL) {
    result <- grades
    if (!is.null(drop)) {
        for (i in 1:length(drop)) {
            result <- result %>%
                group_by({{studentID}}) %>%
                drop_lowest(names(drop[i]), drop[i])
        }
    }

    # Compute grade for each category
    result <- result %>%
        group_by({{studentID}}, category) %>%
        mutate(grade_category = mean(grade, na.rm = TRUE))

    # Insert mean category grade for missing assignments
    # (those not yet graded)
    missing <- which(is.na(result$grade))
    result[missing,]$grade <- result[missing,]$grade_category

    # For max score, insert 1 for missing assignments
    result$grade_max <- result$grade
    result[missing,]$grade_max <- 1

    # Compute scores
    result <- result %>%
        group_by({{studentID}}) %>%
        summarise(
            score = sum(weight_assignment*grade),
            score_max = sum(weight_assignment*grade_max)
        ) %>%
        mutate(
            letter = getLetter(score),
            score = round(score, 3),
            letter_max = getLetter(score_max),
            score_max = round(score_max, 3)
        ) %>%
        arrange(desc(score))
    return(result)
}

drop_lowest <- function(df, cat, number) {
    # Remove lowest row
    result <- df %>%
        mutate(is_cat = ifelse(category == cat, 1, 0)) %>%
        arrange(netID, desc(is_cat), grade) %>%
        slice(-number) %>%
        select(-is_cat)
    # Redistribution assignment weight for category
    cat_ids <- which(result$category == cat)
    weight <- unique(result[cat_ids,]$weight_category)
    n <- result[cat_ids,] %>%
        mutate(n = n()) %>%
        pull(n) %>%
        unique()
    result[cat_ids,]$weight_assignment <- weight / n
    return(result)
}
