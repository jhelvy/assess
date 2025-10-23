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
      cat("BUILDING: ", ids[i], "\n")
      build_report(grades, pars, template, indiv = ids[i])
    }
  }
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
