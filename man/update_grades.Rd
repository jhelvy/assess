% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feedback.R
\name{update_grades}
\alias{update_grades}
\title{Update grades for all assignments}
\usage{
update_grades(assignments, roster, path_box, drop = NULL)
}
\arguments{
\item{assignments}{Assignments data frame}

\item{roster}{Course roster data frame}

\item{path_box}{Path to root box folder}

\item{drop}{Which assignments to drop from grade computation. Should be a
named vector defining the category and number to drop.}
}
\description{
Update grades for all assignments in Box folders
}
