
#' Title
#'
#' Description
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
#' Description
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
#' @param arg description
#' @export
update_grades <- function() {
    # Compute all grades & copy results to box folders
    source(here::here("code", "grade.R"))
}

#' Title
#'
#' Description
#'
#' @param arg description
#' @param roster Course roster
#' @export
get_all_grades <- function(assignments, roster) {
    netIDs <- roster %>%
        filter(enrolled == 1) %>%
        pull(netID)
    missing_grades <- data.frame(netID = netIDs, grade = NA)
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
        temp <- temp %>%
            mutate(
                category = row$category,
                number = row$number,
                weight_category = row$weight_category,
                weight_assignment = row$weight_assignment,
                weight_category_amg = row$weight_category_amg,
                weight_assignment_amg = row$weight_assignment_amg
            )
        grades[[i]] <- temp
    }
    # Merge
    grades <- do.call(rbind, grades) %>%
        mutate(assignment = paste(category, number, sep = "_")) %>%
        arrange(netID)
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
#' @param arg description
#' @export
compute_grade <- function(grades) {
    result <- grades %>%
        group_by(netID) %>%
        # Drop lowest quiz
        mutate(is_quiz = ifelse(category == "quiz", 1, 0)) %>%
        arrange(netID, desc(is_quiz), grade) %>%
        slice(-1) %>%
        # Drop lowest hw
        mutate(is_hw = ifelse(category == "hw", 1, 0)) %>%
        arrange(netID, desc(is_hw), grade) %>%
        slice(-1) %>%
        # Compute grade
        filter(!is.na(grade)) %>%
        group_by(netID, category, weight_category, weight_assignment) %>%
        summarise(grade = mean(grade)) %>%
        # Redistribute weight
        group_by(netID) %>%
        mutate(
            weight_missing = 1 - sum(weight_category),
            count = n(),
            weight_category = weight_category + weight_missing / count) %>%
        # Compute score
        summarise(score = sum(weight_category*grade)) %>%
        mutate(
            letter = getLetter(score),
            score = round(score, 3))
    return(result)
}
