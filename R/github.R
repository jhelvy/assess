# GitHub repo administration via the `gh` CLI.
#
# These functions wrap the GitHub CLI (`gh`) with processx so the whole
# per-student repo workflow can be driven from R using the class roster. They
# are course-agnostic: pass the GitHub `org` and keep each repo's exact name in
# a roster column (default `gh`), so the same functions serve any course that
# uses the roster format (columns include `netID`, `enrolled`, `gh`, and
# optionally `name` and `github_username`).
#
# Each repo is `<org>/<gh>`, e.g. org "eda-f26" with gh "eda-jph" gives
# "eda-f26/eda-jph". The same works for team repos driven from a teams table
# with its own `gh` column. Only rows with `enrolled == 1` are acted on.
#
# The local-clone functions split by job, and run in this order:
#   clone_repos()  clone anything missing (once, plus mid-semester adds)
#   pull_repos()   fetch + rebase onto what students have pushed
#   push_repos()   stage, commit, push your own changes
# pull before push, always: push_repos() never fetches, so a clone that has
# fallen behind commits fine and then has its push rejected.

# ---- internal helpers -----------------------------------------------------

# Enrolled rows of the roster.
enrolled_rows <- function(roster) {
  enrolled <- NULL
  dplyr::filter(roster, enrolled == 1)
}

# Planning table for repo operations: one row per enrolled student with the
# repo name (from `repo_col`) and the full `org/repo` slug. Pure (no gh calls),
# so the name lookup and enrolled-filtering logic is unit-testable.
plan_repos <- function(roster, org, repo_col = "gh") {
  if (!repo_col %in% names(roster)) {
    stop(sprintf("Roster has no '%s' column.", repo_col), call. = FALSE)
  }
  r <- enrolled_rows(roster)
  repo <- as.character(r[[repo_col]])
  tibble::tibble(
    netID = r$netID,
    name  = if ("name" %in% names(r)) r$name else NA_character_,
    repo  = repo,
    full  = paste0(org, "/", repo)
  )
}

# Planning table for collaborator invites: adds the student's GitHub handle
# and a `missing` flag for rows with no handle. Errors if a column is absent.
plan_collaborators <- function(roster, org, repo_col = "gh",
                               username_col = "github_username") {
  if (!username_col %in% names(roster)) {
    stop(sprintf("Roster has no '%s' column.", username_col), call. = FALSE)
  }
  plan <- plan_repos(roster, org, repo_col)
  r <- enrolled_rows(roster)
  user <- as.character(r[[username_col]])
  tibble::tibble(
    netID   = plan$netID,
    repo    = plan$repo,
    full    = plan$full,
    user    = user,
    missing = is.na(user) | !nzchar(trimws(user))
  )
}

# Thin process wrappers. error_on_status = FALSE so we inspect $status per repo
# and keep going rather than aborting the whole batch on one failure.
gh_run  <- function(args) processx::run("gh",  args, error_on_status = FALSE)
git_run <- function(args) processx::run("git", args, error_on_status = FALSE)

# Fail early with an actionable message if gh is unusable.
assert_gh_ready <- function() {
  if (!nzchar(Sys.which("gh"))) {
    stop("gh CLI not found. Install with: brew install gh", call. = FALSE)
  }
  if (gh_run(c("auth", "status"))$status != 0) {
    stop("gh is not authenticated. Run: gh auth login", call. = FALSE)
  }
  invisible(TRUE)
}

repo_exists <- function(full) gh_run(c("repo", "view", full))$status == 0

# ---- exported functions ---------------------------------------------------

#' Create a private GitHub repo for each enrolled student
#'
#' Creates one private repo inside `org` for every roster row with
#' `enrolled == 1`, named by that row's `repo_col` value (e.g. `eda-jph`).
#' Repos that already exist are skipped, so the function is safe to re-run.
#'
#' Requires the `gh` CLI to be installed and authenticated
#' (`gh auth login`).
#'
#' @param roster Course roster data frame. Must have `netID`, `enrolled`, and
#'   `repo_col` columns; `name`, if present, is used for the repo description.
#' @param org GitHub organization to create the repos in (e.g. `"eda-f26"`).
#' @param repo_col Roster column holding each repo's exact name (e.g.
#'   `"eda-jph"`). Defaults to `"gh"`.
#' @param add_readme Initialize each repo with a README? Defaults to `TRUE`.
#' @param dry_run If `TRUE`, print what would be created without calling `gh`.
#'   Defaults to `FALSE`.
#' @return Invisibly, a tibble with one row per enrolled student and a
#'   `status` column (`created`, `skipped`, `failed`, or `would_create`).
#' @export
create_repos <- function(roster, org, repo_col = "gh", add_readme = TRUE,
                         dry_run = FALSE) {
  plan <- plan_repos(roster, org, repo_col)
  if (!dry_run) assert_gh_ready()
  plan$status <- NA_character_

  for (i in seq_len(nrow(plan))) {
    full <- plan$full[i]

    if (!dry_run && repo_exists(full)) {
      cat("SKIP    ", full, "(already exists)\n")
      plan$status[i] <- "skipped"
      next
    }

    desc <- if (!is.na(plan$name[i])) {
      sprintf("%s submission repo \u2014 %s", org, plan$name[i])
    } else {
      sprintf("%s submission repo", org)
    }
    args <- c("repo", "create", full, "--private",
              if (add_readme) "--add-readme",
              "--description", desc)

    if (dry_run) {
      cat("WOULD CREATE", full, "\n")
      plan$status[i] <- "would_create"
      next
    }

    res <- gh_run(args)
    if (res$status == 0) {
      cat("CREATED ", full, "\n")
      plan$status[i] <- "created"
    } else {
      cat("FAILED  ", full, "\n", res$stderr, "\n")
      plan$status[i] <- "failed"
    }
  }

  cat("----\n")
  print(table(plan$status))
  invisible(plan)
}

#' Invite each enrolled student to their own repo
#'
#' Sends a collaborator invitation for every enrolled student to their repo
#' (`org/<repo_col>`), using the GitHub handle in the roster's `username_col`.
#' Students with no handle on file are reported and skipped. Idempotent:
#' GitHub treats an already-invited user as a no-op.
#'
#' Requires the `gh` CLI to be installed and authenticated, and the repos to
#' already exist (see [create_repos()]).
#'
#' @param roster Course roster data frame. Must have `netID`, `enrolled`, and
#'   the columns named by `repo_col` and `username_col`.
#' @param org GitHub organization the repos live in.
#' @param repo_col Roster column holding each repo's exact name. Defaults to
#'   `"gh"`.
#' @param permission Collaborator permission level: one of `"pull"`,
#'   `"triage"`, `"push"`, `"maintain"`, `"admin"`. Defaults to `"push"`.
#' @param username_col Roster column holding each student's GitHub handle.
#'   Defaults to `"github_username"`.
#' @param dry_run If `TRUE`, print what would be invited without calling `gh`.
#' @return Invisibly, a tibble with one row per enrolled student and a
#'   `status` column (`invited`, `missing`, `failed`, or `would_invite`).
#' @export
invite_collaborators <- function(roster, org, repo_col = "gh",
                                 permission = "push",
                                 username_col = "github_username",
                                 dry_run = FALSE) {
  plan <- plan_collaborators(roster, org, repo_col, username_col)
  if (!dry_run) assert_gh_ready()
  plan$status <- NA_character_

  for (i in seq_len(nrow(plan))) {
    full <- plan$full[i]

    if (plan$missing[i]) {
      cat("MISSING ", full, "(no", username_col, ")\n")
      plan$status[i] <- "missing"
      next
    }

    args <- c("api", "--method", "PUT",
              paste0("repos/", full, "/collaborators/", plan$user[i]),
              "-f", paste0("permission=", permission))

    if (dry_run) {
      cat("WOULD INVITE", plan$user[i], "->", full, "\n")
      plan$status[i] <- "would_invite"
      next
    }

    res <- gh_run(args)
    if (res$status == 0) {
      cat("INVITED ", plan$user[i], "->", full, "\n")
      plan$status[i] <- "invited"
    } else {
      cat("FAILED  ", full, "\n", res$stderr, "\n")
      plan$status[i] <- "failed"
    }
  }

  cat("----\n")
  print(table(plan$status))
  invisible(plan)
}

#' Clone each student repo that isn't already on disk
#'
#' Clones every enrolled student's repo from `org` into `dir/<repo_col>`.
#' Repos already cloned are left completely alone -- nothing is fetched,
#' committed, or overwritten -- so this is safe to re-run whenever students are
#' added mid-semester.
#'
#' This is the only function that creates local clones. [push_repos()] and
#' [pull_repos()] both skip repos that aren't on disk yet; run this first.
#'
#' Requires the `gh` CLI to be installed and authenticated
#' (`gh auth login`), and the repos to already exist on GitHub (see
#' [create_repos()]).
#'
#' @param roster Course roster data frame with `netID`, `enrolled`, and
#'   `repo_col`.
#' @param org GitHub organization the repos live in (e.g. `"eda-f26"`).
#' @param dir Directory to clone into. Defaults to [here::here()]; pass the
#'   folder that holds the clones (e.g. `here::here("repos")`).
#' @param repo_col Roster column holding each repo's exact name, also used as
#'   the local clone folder name under `dir`. Defaults to `"gh"`.
#' @param dry_run If `TRUE`, print what would be cloned without calling `gh`.
#' @return Invisibly, a tibble with one row per enrolled student and a
#'   `status` column (`cloned`, `exists`, `failed`, or `would_clone`).
#' @export
clone_repos <- function(roster, org, dir = here::here(), repo_col = "gh",
                        dry_run = FALSE) {
  plan <- plan_repos(roster, org, repo_col)
  if (!dry_run) assert_gh_ready()
  plan$path <- file.path(dir, plan$repo)
  plan$status <- NA_character_

  for (i in seq_len(nrow(plan))) {
    path <- plan$path[i]
    full <- plan$full[i]
    repo <- plan$repo[i]

    if (dir.exists(file.path(path, ".git"))) {
      cat("EXISTS  ", repo, "\n")
      plan$status[i] <- "exists"
      next
    }

    if (dry_run) {
      cat("WOULD CLONE", full, "\n")
      plan$status[i] <- "would_clone"
      next
    }

    res <- gh_run(c("repo", "clone", full, path))
    if (res$status == 0) {
      cat("CLONED  ", repo, "\n")
      plan$status[i] <- "cloned"
    } else {
      cat("FAILED  ", repo, "\n", res$stderr, "\n")
      plan$status[i] <- "failed"
    }
  }

  cat("----\n")
  print(table(plan$status))
  invisible(plan)
}

#' Pull student work into every local clone
#'
#' Fetches each enrolled student's repo and rebases the local clone onto it, so
#' the clones hold whatever students have pushed since you last looked.
#'
#' Run this before [push_repos()], every time. `push_repos()` only stages,
#' commits, and pushes -- it never fetches -- so the moment a student commits
#' their own work the clone is behind and the push is rejected with *"Updates
#' were rejected because the remote contains work that you do not have
#' locally"*.
#'
#' Rebase (not merge) keeps the history linear: local commits not yet pushed
#' (feedback PDFs, refreshed starter files) replay on top of the student's
#' work. Uncommitted changes are stashed and restored around the rebase
#' (`--autostash`). Instructor and student rarely touch the same file, so
#' conflicts are unusual; a repo that does conflict is left mid-rebase and
#' reported as `conflict` for you to resolve by hand.
#'
#' Purely local git plus a fetch from each clone's own `origin`, so no `org`
#' argument is needed. Repos that aren't cloned yet are skipped -- see
#' [clone_repos()].
#'
#' @param roster Course roster data frame with `netID`, `enrolled`, and
#'   `repo_col`.
#' @param dir Directory containing the local clones. Defaults to
#'   [here::here()]; pass the folder that holds them (e.g.
#'   `here::here("repos")`).
#' @param repo_col Roster column holding each repo's exact name, also used as
#'   the local clone folder name under `dir`. Defaults to `"gh"`.
#' @param dry_run If `TRUE`, report which repos are behind without rebasing.
#' @return Invisibly, a tibble with one row per enrolled student and a
#'   `status` column (`pulled`, `uptodate`, `skipped`, `conflict`, `failed`,
#'   or `would_pull`).
#' @export
pull_repos <- function(roster, dir = here::here(), repo_col = "gh",
                       dry_run = FALSE) {
  plan <- plan_repos(roster, org = "", repo_col)
  plan$path <- file.path(dir, plan$repo)
  plan$status <- NA_character_

  for (i in seq_len(nrow(plan))) {
    path <- plan$path[i]
    repo <- plan$repo[i]

    if (!dir.exists(file.path(path, ".git"))) {
      cat("SKIP    ", repo, "(not cloned; see clone_repos())\n")
      plan$status[i] <- "skipped"
      next
    }

    if (git_run(c("-C", path, "fetch", "--quiet", "origin"))$status != 0) {
      cat("FAILED  ", repo, "(fetch)\n")
      plan$status[i] <- "failed"
      next
    }

    # How many commits is the clone behind its upstream branch?
    behind <- git_run(c("-C", path, "rev-list", "--count", "HEAD..@{upstream}"))
    if (behind$status != 0) {
      cat("FAILED  ", repo, "(no upstream branch)\n")
      plan$status[i] <- "failed"
      next
    }
    if (identical(trimws(behind$stdout), "0")) {
      cat("UPTODATE", repo, "\n")
      plan$status[i] <- "uptodate"
      next
    }

    if (dry_run) {
      cat("WOULD PULL", repo, "(behind by", trimws(behind$stdout), ")\n")
      plan$status[i] <- "would_pull"
      next
    }

    res <- git_run(c("-C", path, "pull", "--rebase", "--autostash", "--quiet"))
    if (res$status == 0) {
      cat("PULLED  ", repo, "\n")
      plan$status[i] <- "pulled"
    } else {
      cat("CONFLICT", repo, "(left mid-rebase; resolve by hand)\n")
      plan$status[i] <- "conflict"
    }
  }

  cat("----\n")
  print(table(plan$status))
  invisible(plan)
}

#' Commit and push changes across every student repo
#'
#' Iterates over each enrolled student's local clone at `dir/<repo_col>`. A
#' repo with uncommitted changes gets everything staged, committed with
#' `message`, and pushed; a repo that is merely ahead of its remote (commits
#' made locally that never landed, e.g. a push rejected while the clone was
#' behind) is pushed as-is. Repos that are clean and level with the remote are
#' left untouched, so this is safe to run repeatedly (e.g. weekly as
#' assignments are graded).
#'
#' Run [pull_repos()] first. This function never fetches, so a clone that is
#' behind whatever the student has pushed will commit fine and then have its
#' push rejected. Repos that aren't cloned yet are skipped -- see
#' [clone_repos()].
#'
#' Requires the `gh` CLI installed and authenticated, and `gh auth setup-git`
#' run once so `git push` works non-interactively.
#'
#' @param roster Course roster data frame with `netID`, `enrolled`, and
#'   `repo_col`.
#' @param org GitHub organization the repos live in.
#' @param message Commit message (required).
#' @param dir Directory containing the local clones. Defaults to
#'   [here::here()].
#' @param repo_col Roster column holding each repo's exact name, also used as
#'   the local clone folder name under `dir`. Defaults to `"gh"`.
#' @param gitignore Character vector of `.gitignore` lines to seed into any
#'   repo lacking a `.gitignore`. Defaults to `".DS_Store"`. Use `NULL` to
#'   skip seeding.
#' @param dry_run If `TRUE`, print what would happen without writing, cloning,
#'   committing, or pushing.
#' @return Invisibly, a tibble with one row per enrolled student and a
#'   `status` column (`pushed`, `nochange`, `skipped`, `failed`, or a
#'   `would_*` value under `dry_run`).
#' @export
push_repos <- function(roster, org, message, dir = here::here(),
                       repo_col = "gh", gitignore = ".DS_Store",
                       dry_run = FALSE) {
  if (missing(message) || !nzchar(message)) {
    stop("Provide a commit `message`.", call. = FALSE)
  }
  plan <- plan_repos(roster, org, repo_col)
  if (!dry_run) assert_gh_ready()
  plan$path <- file.path(dir, plan$repo)
  plan$status <- NA_character_

  for (i in seq_len(nrow(plan))) {
    path <- plan$path[i]
    repo <- plan$repo[i]

    # Cloning is clone_repos()' job; a missing clone has nothing to push.
    if (!dir.exists(file.path(path, ".git"))) {
      cat("SKIP    ", repo, "(not cloned; see clone_repos())\n")
      plan$status[i] <- "skipped"
      next
    }

    # Seed a .gitignore if one doesn't exist yet.
    gi <- file.path(path, ".gitignore")
    if (!is.null(gitignore) && !file.exists(gi) && !dry_run) {
      writeLines(gitignore, gi)
    }

    # Two independent reasons to act: uncommitted work in the tree, and
    # commits already made locally that never reached the remote (e.g. a push
    # that was rejected because the clone was behind, then rebased by
    # pull_repos()). Checking only the tree would leave those stranded.
    changes <- git_run(c("-C", path, "status", "--porcelain"))$stdout
    dirty   <- nzchar(trimws(changes))

    ahead_res <- git_run(c("-C", path, "rev-list", "--count", "@{upstream}..HEAD"))
    ahead <- ahead_res$status == 0 && !identical(trimws(ahead_res$stdout), "0")

    if (!dirty && !ahead) {
      cat("NOCHANGE", repo, "\n")
      plan$status[i] <- "nochange"
      next
    }

    if (dry_run) {
      cat("WOULD PUSH", repo, "\n")
      plan$status[i] <- "would_push"
      next
    }

    ok <- if (dirty) {
      git_run(c("-C", path, "add", "-A"))$status == 0 &&
        git_run(c("-C", path, "commit", "-m", message))$status == 0 &&
        git_run(c("-C", path, "push"))$status == 0
    } else {
      git_run(c("-C", path, "push"))$status == 0
    }
    if (ok) {
      cat("PUSHED  ", repo, "\n")
      plan$status[i] <- "pushed"
    } else {
      cat("FAILED  ", repo, "(commit or push error)\n")
      plan$status[i] <- "failed"
    }
  }

  cat("----\n")
  print(table(plan$status))
  invisible(plan)
}

# ---- late submissions -----------------------------------------------------

# Build the assign/due lookup from an already-read schedule table. Pure, so the
# column handling and stub mapping are unit-testable without a file.
#
# The course sites share one schedule.csv schema: homework in
# `n_assign`/`due_assign`, mini projects in `n_mini`/`due_mini` (EDA only), and
# project deliverables in `stub_project`/`due_project`. Absent columns are
# skipped, so the same parser serves a course that has no minis.
parse_due_dates <- function(schedule, time = "23:59:59",
                            tz = Sys.timezone(), stub_map = NULL) {
  out <- list()

  numbered <- function(n_col, due_col, prefix) {
    if (!all(c(n_col, due_col) %in% names(schedule))) return(NULL)
    keep <- !is.na(schedule[[n_col]]) & !is.na(schedule[[due_col]])
    if (!any(keep)) return(NULL)
    tibble::tibble(
      assign = paste0(prefix, schedule[[n_col]][keep]),
      date   = as.character(schedule[[due_col]][keep])
    )
  }

  out$hw   <- numbered("n_assign", "due_assign", "hw")
  out$mini <- numbered("n_mini", "due_mini", "mini")

  if (all(c("stub_project", "due_project") %in% names(schedule))) {
    keep <- !is.na(schedule$stub_project) & !is.na(schedule$due_project)
    if (any(keep)) {
      # Site stubs are hyphenated (final-analysis); pars.R names use
      # underscores (final_analysis). stub_map handles the rest, e.g. EDA's
      # c(presentation = "final_presentation").
      stub <- gsub("-", "_", schedule$stub_project[keep])
      if (!is.null(stub_map)) {
        hit <- stub %in% names(stub_map)
        stub[hit] <- unname(stub_map[stub[hit]])
      }
      out$project <- tibble::tibble(
        assign = stub,
        date   = as.character(schedule$due_project[keep])
      )
    }
  }

  due <- dplyr::bind_rows(out)
  if (nrow(due) == 0) {
    stop("No due dates found in schedule; expected n_assign/due_assign, ",
         "n_mini/due_mini, or stub_project/due_project columns.",
         call. = FALSE)
  }
  due$due <- as.POSIXct(paste(due$date, time), tz = tz)
  due[, c("assign", "due")]
}

#' Read assignment deadlines from a course site's schedule.csv
#'
#' Builds an `assign`/`due` lookup for [check_late()] from the course website
#' repo's `schedule.csv`, which is the single source of truth for deadlines --
#' move a date there and the late check follows.
#'
#' Handles the three deliverable families in that file's schema: homework
#' (`n_assign`/`due_assign`, named `hw1`, `hw2`, ...), mini projects
#' (`n_mini`/`due_mini`, named `mini1`, ...), and project deliverables
#' (`stub_project`/`due_project`). Column groups that are absent are skipped.
#'
#' Project stubs on the site are hyphenated (`final-analysis`) while the
#' `assign` names in `pars.R` use underscores (`final_analysis`), so hyphens are
#' converted automatically. Use `stub_map` for anything that needs more than
#' that, e.g. `c(presentation = "final_presentation")`.
#'
#' Instructor-run work (quizzes, exams, interviews, participation) has no repo
#' submission and so gets no deadline; those simply never appear here.
#'
#' @param path Path to the course site's `schedule.csv`, e.g.
#'   `here::here("..", "2026-Fall", "schedule.csv")`.
#' @param time Time of day assignments are due, as `"HH:MM:SS"`. Defaults to
#'   `"23:59:59"` (the 11:59pm deadline in the syllabus late policy).
#' @param tz Time zone for the deadlines. Defaults to the system time zone.
#' @param stub_map Optional named character vector mapping project stubs that
#'   don't match their `assign` name after hyphen conversion.
#' @return A tibble with `assign` and `due` (POSIXct) columns.
#' @export
read_due_dates <- function(path, time = "23:59:59", tz = Sys.timezone(),
                           stub_map = NULL) {
  schedule <- readr::read_csv(path, show_col_types = FALSE)
  parse_due_dates(schedule, time = time, tz = tz, stub_map = stub_map)
}

#' Report student work committed after an assignment's deadline
#'
#' Run right after [pull_repos()], before grading, to see what arrived late.
#' Two things are flagged, and neither alone is enough:
#'
#' * `hours_late` -- committed past the deadline. This is the primary signal,
#'   and it is what catches the grading window: a typical `grade.R` pulls again
#'   immediately before [push_repos()], so a student who pushes while you are
#'   reviewing gets swept in *underneath* the feedback commit and would
#'   otherwise look like they had been there all along.
#' * `after_feedback` -- the commit arrived after that assignment's feedback was
#'   pushed, so it definitely was not reviewed. `FALSE` means it was already in
#'   the tree when you graded (it may or may not have been seen); `NA` means the
#'   assignment has not been graded yet.
#'
#' The anchor for `after_feedback` is the last commit touching
#' `feedback/<assign>.md` in that repo, which is written only when feedback is
#' delivered -- so no snapshot file or extra state is needed.
#'
#' This is a read-only report: nothing is written and no git state is touched.
#' Applying the course late policy stays a manual decision.
#'
#' @param roster Course roster data frame with `netID`, `enrolled`, and
#'   `repo_col` columns; `name`, if present, is carried into the result.
#' @param due Data frame with `assign` and `due` (POSIXct) columns, from
#'   [read_due_dates()] or an `assignments` table that carries a `due` column.
#'   Row order sets the report order.
#' @param assign Assignment(s) to check: either a character vector of names, or
#'   a `pars` list (its `$assign` element is used), so you can pass the `pars`
#'   you are grading with and see only that assignment. Defaults to `NULL`,
#'   which sweeps every assignment whose deadline has passed -- so a late `hw1`
#'   landing while you grade `hw3` still surfaces.
#' @param dir Directory containing the local clones, e.g.
#'   `here::here("repos")`.
#' @param repo_col Roster column holding each repo's exact name, also used as
#'   the clone's folder name. Defaults to `"gh"`.
#' @param instructor Instructor's git author email, whose commits are excluded.
#'   Match on email, not name: the same person often commits under more than one
#'   author name.
#' @return Invisibly, a tibble with one row per student and assignment with late
#'   activity: `netID`, `name`, `assign`, `n_commits`, `due`, `last_commit`,
#'   `hours_late`, `after_feedback`, and `files`.
#' @export
check_late <- function(roster, due, assign = NULL, dir = here::here("repos"),
                       repo_col = "gh",
                       instructor = "john.helveston@gmail.com") {
  if (!all(c("assign", "due") %in% names(due))) {
    stop("`due` must have `assign` and `due` columns; see read_due_dates().",
         call. = FALSE)
  }

  # Accept a pars list as well as a plain name, so grade.R can pass the pars
  # it is already grading with.
  if (is.list(assign)) {
    if (is.null(assign$assign)) {
      stop("`assign` is a list with no `$assign` element; pass a pars list ",
           "or a character vector of assignment names.", call. = FALSE)
    }
    assign <- assign$assign
  }

  checks <- due[!is.na(due$due) & due$due < Sys.time(), ]
  if (!is.null(assign)) {
    checks <- checks[checks$assign %in% assign, ]
  }
  if (nrow(checks) == 0) {
    cat("No assignments past their deadline to check.\n")
    return(invisible(tibble::tibble()))
  }

  plan <- plan_repos(roster, org = "", repo_col)
  out  <- list()

  for (i in seq_len(nrow(plan))) {
    path <- file.path(dir, plan$repo[i])
    if (!dir.exists(file.path(path, ".git"))) {
      cat("SKIP    ", plan$repo[i], "(not cloned; see clone_repos())\n")
      next
    }

    for (j in seq_len(nrow(checks))) {
      a   <- checks$assign[j]
      due_at <- as.numeric(checks$due[j])
      if (!dir.exists(file.path(path, a))) next

      # Author dates as unix epoch (%at), never the ISO form: parsing
      # "...T21:18:09-04:00" as a local wall clock silently shifts by the UTC
      # offset and flags on-time students as late.
      log <- git_run(c("-C", path, "log", "--format=%H%x09%at%x09%ae",
                       "--", paste0(a, "/")))
      lines <- strsplit(trimws(log$stdout), "\n", fixed = TRUE)[[1]]
      if (log$status != 0 || !nzchar(trimws(log$stdout))) next

      parts  <- strsplit(lines, "\t", fixed = TRUE)
      sha    <- vapply(parts, `[`, character(1), 1)
      at     <- as.numeric(vapply(parts, `[`, character(1), 2))
      email  <- vapply(parts, `[`, character(1), 3)
      keep   <- email != instructor & at > due_at
      if (!any(keep)) next
      sha <- sha[keep]
      at  <- at[keep]

      anchor <- git_run(c("-C", path, "log", "--format=%H", "-1", "--",
                          file.path("feedback", paste0(a, ".md"))))
      anchor <- trimws(anchor$stdout)
      after <- if (!nzchar(anchor)) {
        NA   # not graded yet
      } else {
        any(vapply(sha, function(s) {
          git_run(c("-C", path, "merge-base", "--is-ancestor",
                    s, anchor))$status != 0
        }, logical(1)))
      }

      shown <- git_run(c("-C", path, "show", "--pretty=", "--name-only", sha))
      files <- strsplit(trimws(shown$stdout), "\n", fixed = TRUE)[[1]]
      files <- unique(files[nzchar(files)])
      files <- files[!grepl("_files/", files, fixed = TRUE)]  # render artifacts

      out[[length(out) + 1]] <- tibble::tibble(
        netID          = plan$netID[i],
        name           = plan$name[i],
        assign         = a,
        n_commits      = length(sha),
        due            = checks$due[j],
        last_commit    = as.POSIXct(max(at), tz = Sys.timezone(),
                                    origin = "1970-01-01"),
        hours_late     = round((max(at) - due_at) / 3600, 1),
        after_feedback = after,
        files          = paste(files, collapse = ", ")
      )
    }
  }

  if (length(out) == 0) {
    cat("\nNo late submissions.\n")
    return(invisible(tibble::tibble()))
  }

  late <- dplyr::bind_rows(out)
  late <- late[order(match(late$assign, checks$assign), -late$hours_late), ]

  cat("\n")
  for (i in seq_len(nrow(late))) {
    cat(sprintf(
      "LATE  %-9s %-16s %5.1f hrs late (%s)  after_feedback: %s\n",
      late$assign[i], late$netID[i], late$hours_late[i],
      format(late$last_commit[i], "%b %d %H:%M"),
      if (is.na(late$after_feedback[i])) "not graded yet"
      else if (late$after_feedback[i]) "YES" else "no"
    ))
    cat("        ", late$files[i], "\n")
  }
  cat("\n", nrow(late), " late submission(s).\n", sep = "")

  invisible(late)
}
