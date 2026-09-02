# Tests for the pure due-date parsing behind check_late(). The git-calling path
# is not unit-tested (it shells out); these cover the schedule.csv schema
# handling, stub mapping, and deadline construction.

schedule_eda <- tibble::tibble(
  n_assign     = c(1, 2, NA, NA),
  due_assign   = c("2026-08-31", "2026-09-07", NA, NA),
  n_mini       = c(NA, NA, 1, NA),
  due_mini     = c(NA, NA, "2026-10-04", NA),
  stub_project = c(NA, NA, NA, "initial-report"),
  due_project  = c(NA, NA, NA, "2026-11-01")
)

test_that("parse_due_dates reads hw, mini and project deliverables", {
  due <- parse_due_dates(schedule_eda, tz = "America/New_York")

  expect_equal(due$assign, c("hw1", "hw2", "mini1", "initial_report"))
  expect_s3_class(due$due, "POSIXct")
})

test_that("parse_due_dates puts deadlines at 11:59:59pm by default", {
  due <- parse_due_dates(schedule_eda, tz = "America/New_York")

  expect_equal(
    due$due[1],
    as.POSIXct("2026-08-31 23:59:59", tz = "America/New_York")
  )
})

test_that("parse_due_dates honors a custom due time", {
  due <- parse_due_dates(schedule_eda, time = "17:00:00", tz = "America/New_York")

  expect_equal(
    due$due[1],
    as.POSIXct("2026-08-31 17:00:00", tz = "America/New_York")
  )
})

test_that("parse_due_dates converts hyphenated project stubs to underscores", {
  due <- parse_due_dates(schedule_eda, tz = "America/New_York")

  expect_true("initial_report" %in% due$assign)
  expect_false("initial-report" %in% due$assign)
})

test_that("parse_due_dates applies stub_map for stubs that need renaming", {
  schedule <- tibble::tibble(
    stub_project = c("presentation", "proposal"),
    due_project  = c("2026-12-08", "2026-09-20")
  )

  due <- parse_due_dates(
    schedule, tz = "America/New_York",
    stub_map = c(presentation = "final_presentation")
  )

  expect_equal(due$assign, c("final_presentation", "proposal"))
})

test_that("parse_due_dates skips absent column groups", {
  # MADD's schedule has no mini projects
  schedule <- tibble::tibble(
    n_assign   = c(1, 2),
    due_assign = c("2026-08-31", "2026-09-07")
  )

  due <- parse_due_dates(schedule, tz = "America/New_York")

  expect_equal(due$assign, c("hw1", "hw2"))
})

test_that("parse_due_dates drops rows with no due date", {
  schedule <- tibble::tibble(
    n_assign   = c(1, 2, 3),
    due_assign = c("2026-08-31", NA, "2026-09-21")
  )

  due <- parse_due_dates(schedule, tz = "America/New_York")

  expect_equal(due$assign, c("hw1", "hw3"))
})

test_that("parse_due_dates errors on a schedule with no recognized columns", {
  expect_error(
    parse_due_dates(tibble::tibble(week = 1:3)),
    "No due dates found"
  )
})

test_that("check_late errors when due lacks the required columns", {
  roster <- tibble::tibble(netID = "abc", enrolled = 1, gh = "eda-abc")

  expect_error(
    check_late(roster, tibble::tibble(assign = "hw1")),
    "read_due_dates"
  )
})

test_that("check_late reports nothing when no deadline has passed", {
  roster <- tibble::tibble(netID = "abc", enrolled = 1, gh = "eda-abc")
  future <- tibble::tibble(assign = "hw1", due = Sys.time() + 86400)

  expect_output(
    late <- check_late(roster, future),
    "No assignments past their deadline"
  )
  expect_equal(nrow(late), 0L)
})

test_that("check_late accepts a pars list and uses its $assign", {
  roster <- tibble::tibble(netID = "abc", enrolled = 1, gh = "eda-abc")
  due <- tibble::tibble(
    assign = c("hw1", "hw2"),
    due    = rep(Sys.time() + 86400, 2)   # neither deadline has passed
  )

  # hw2's deadline is in the future, so scoping to it reports nothing to check
  expect_output(
    check_late(roster, due, assign = list(assign = "hw2")),
    "No assignments past their deadline"
  )
})

test_that("check_late scopes to a single assignment from pars", {
  roster <- tibble::tibble(netID = "abc", enrolled = 1, gh = "nope")
  due <- tibble::tibble(
    assign = c("hw1", "hw2"),
    due    = rep(Sys.time() - 86400, 2)   # both deadlines have passed
  )

  # Repo isn't cloned, so the only output is the skip line -- but it is emitted
  # once, not once per assignment, showing the scoping took effect.
  out <- capture.output(check_late(roster, due, assign = list(assign = "hw1")))
  expect_equal(sum(grepl("SKIP", out)), 1L)
})

test_that("check_late errors on a list with no assign element", {
  roster <- tibble::tibble(netID = "abc", enrolled = 1, gh = "eda-abc")
  due <- tibble::tibble(assign = "hw1", due = Sys.time() - 86400)

  expect_error(
    check_late(roster, due, assign = list(title = "Homework 1")),
    "no `\\$assign` element"
  )
})
