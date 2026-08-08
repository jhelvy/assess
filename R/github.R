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

    args <- c("repo", "add-collaborator", full, plan$user[i],
              "--permission", permission)

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

#' Commit and push changes across every student repo
#'
#' Iterates over each enrolled student's local clone at `dir/<repo_col>` and,
#' for any repo with uncommitted changes, stages everything, commits with
#' `message`, and pushes. Repos with no changes are left untouched, so this is
#' safe to run repeatedly (e.g. weekly as assignments are graded).
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
#' @param clone_missing If `TRUE`, clone any repo not present locally before
#'   committing. Defaults to `FALSE`.
#' @param gitignore Character vector of `.gitignore` lines to seed into any
#'   repo lacking a `.gitignore`. Defaults to `".DS_Store"`. Use `NULL` to
#'   skip seeding.
#' @param dry_run If `TRUE`, print what would happen without writing, cloning,
#'   committing, or pushing.
#' @return Invisibly, a tibble with one row per enrolled student and a
#'   `status` column (`pushed`, `nochange`, `skipped`, `failed`, `cloned`, or
#'   a `would_*` value under `dry_run`).
#' @export
push_repos <- function(roster, org, message, dir = here::here(),
                       repo_col = "gh", clone_missing = FALSE,
                       gitignore = ".DS_Store", dry_run = FALSE) {
  if (missing(message) || !nzchar(message)) {
    stop("Provide a commit `message`.", call. = FALSE)
  }
  plan <- plan_repos(roster, org, repo_col)
  if (!dry_run) assert_gh_ready()
  plan$path <- file.path(dir, plan$repo)
  plan$status <- NA_character_

  for (i in seq_len(nrow(plan))) {
    path <- plan$path[i]
    full <- plan$full[i]
    repo <- plan$repo[i]

    # Clone if the local repo is missing (only when asked).
    if (!dir.exists(file.path(path, ".git"))) {
      if (!clone_missing) {
        cat("SKIP    ", repo, "(not cloned; use clone_missing = TRUE)\n")
        plan$status[i] <- "skipped"
        next
      }
      if (dry_run) {
        cat("WOULD CLONE", full, "\n")
        plan$status[i] <- "would_clone"
        next
      }
      if (gh_run(c("repo", "clone", full, path))$status != 0) {
        cat("FAILED   clone", repo, "\n")
        plan$status[i] <- "failed"
        next
      }
      cat("CLONED  ", repo, "\n")
    }

    # Seed a .gitignore if one doesn't exist yet.
    gi <- file.path(path, ".gitignore")
    if (!is.null(gitignore) && !file.exists(gi) && !dry_run) {
      writeLines(gitignore, gi)
    }

    # Anything to commit?
    changes <- git_run(c("-C", path, "status", "--porcelain"))$stdout
    if (!nzchar(trimws(changes))) {
      cat("NOCHANGE", repo, "\n")
      plan$status[i] <- "nochange"
      next
    }

    if (dry_run) {
      cat("WOULD PUSH", repo, "\n")
      plan$status[i] <- "would_push"
      next
    }

    ok <- git_run(c("-C", path, "add", "-A"))$status == 0 &&
      git_run(c("-C", path, "commit", "-m", message))$status == 0 &&
      git_run(c("-C", path, "push"))$status == 0
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
