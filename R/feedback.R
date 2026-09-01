#' Make reports of assignment feedback
#'
#' Builds one feedback report per student for `pars$assign`, staged in
#' `assignments/<assign>/feedback/`.
#'
#' `type = "md"` writes a markdown file directly from the assessment data. It
#' needs no template and no rendering, so it is effectively instant, and the
#' output is deterministic: re-running it for the whole class rewrites every
#' file byte-for-byte, so only students whose grade or feedback actually
#' changed show up as a diff once the reports are delivered into git repos.
#'
#' `type = "pdf"` renders `template` and prints it to pdf via headless Chrome,
#' one process per student.
#'
#' @param pars Parameters defining assignment
#' @param roster Course roster data frame
#' @param template qmd template for making report. Required when
#' `type = "pdf"`, ignored when `type = "md"`.
#' @param indiv One specific id (to build just one report instead of all)
#' Defaults to `NULL` in which case reports are made for all students.
#' @param type Report file type, either `"pdf"` (the default, for
#' backwards compatibility) or `"md"`.
#' @export
make_reports <- function(
  pars,
  roster,
  template = NULL,
  indiv = NULL,
  type = c("pdf", "md")
) {
  type <- match.arg(type)
  if (type == "pdf" && is.null(template)) {
    stop('`template` is required when type = "pdf"', call. = FALSE)
  }
  # Create feedback folder (if doesn't exist)
  make_dir(here::here('assignments', pars$assign, 'feedback'))
  # Get grades
  grades <- get_assignment_grades(pars, roster)
  # Build report for each id (or just the one provided)
  ids <- if (is.null(indiv)) get_enrolled_ids(roster) else indiv
  built <- character(0)
  for (i in seq_along(ids)) {
    if (type == "md") {
      path <- build_report_md(grades, pars, ids[i])
    } else {
      cat("BUILDING: ", ids[i], "\n")
      path <- build_report(grades, pars, template, indiv = ids[i])
    }
    if (!is.null(path)) {
      built <- c(built, ids[i])
    }
  }
  invisible(built)
}

build_report <- function(grades, pars, template, indiv) {
  netID <- NULL

  df <- dplyr::filter(grades, netID == indiv)
  output_file <- get_report_path(pars$assign, indiv)
  params_temp <- list(
    df = df,
    title = pars$title,
    name = unique(df$name)
  )
  pagedown::chrome_print(
    rmarkdown::render(
      input = template,
      output_file = tempfile(fileext = "html"),
      params = params_temp
    ),
    output_file
  )
  return(output_file)
}

# Write one student's feedback as a markdown file. Returns the path written,
# or NULL if the student has nothing graded for this assignment (no assessment
# rows at all), in which case no file is created.
build_report_md <- function(grades, pars, indiv) {
  netID <- NULL

  df <- dplyr::filter(grades, netID == indiv)
  if (nrow(df) == 0 || all(is.na(df$grade))) {
    return(NULL)
  }
  output_file <- get_report_path(pars$assign, indiv, ext = "md")
  writeLines(format_report_md(df, pars), output_file)
  return(output_file)
}

# Build the lines of a student's markdown report. Single-question assignments
# get one feedback block; multi-question ones get a section per question.
format_report_md <- function(df, pars) {
  name <- unique(stats::na.omit(df$name))
  name <- if (length(name)) name[1] else unique(df$netID)[1]
  title <- if (is.null(pars$title)) pars$assign else pars$title

  lines <- c(
    paste("#", title),
    "",
    paste0("**Student:** ", name),
    "",
    paste0("**Grade:** ", format_grade_pct(unique(df$grade)[1]))
  )
  if (nrow(df) == 1) {
    lines <- c(lines, "", "---", "", "## Feedback", "", get_feedback_md(df))
  } else {
    for (j in seq_len(nrow(df))) {
      row <- df[j, ]
      lines <- c(
        lines,
        "",
        "---",
        "",
        paste("##", "Question", row$question),
        "",
        paste0("**Score:** ", format_score_md(row, pars)),
        "",
        get_feedback_md(row)
      )
    }
  }
  return(lines)
}

get_feedback_md <- function(row) {
  feedback <- row$feedback
  if (length(feedback) == 0 || is.na(feedback) || !nzchar(trimws(feedback))) {
    return("_No feedback._")
  }
  return(trimws(feedback))
}

# Per-question score: a percentage when the assignment is graded on percent
# weights, otherwise points out of the question's weight.
format_score_md <- function(row, pars) {
  score <- if ("score" %in% names(row)) row$score else row$assessment
  if (length(score) == 0 || is.na(score)) {
    return("NA")
  }
  if (isTRUE(pars$percent)) {
    return(format_grade_pct(score))
  }
  if ("weight" %in% names(row) && !is.na(row$weight)) {
    return(paste0(format_num(score), " / ", format_num(row$weight)))
  }
  return(format_num(score))
}

format_grade_pct <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    return("NA")
  }
  return(paste0(format_num(100 * x), "%"))
}

format_num <- function(x) {
  return(format(round(x, 1), trim = TRUE, drop0trailing = TRUE, scientific = FALSE))
}

get_report_path <- function(assign, indiv = NULL, ext = "pdf") {
  report_name <- paste0(indiv, "-", assign, '.', ext)
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
