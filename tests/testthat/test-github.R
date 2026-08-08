# Tests for the pure planning logic behind the GitHub repo functions. The
# gh/git-calling paths are not unit-tested (they shell out); these cover the
# repo-name lookup, enrolled filtering, and missing-handle detection.

test_that("plan_repos filters to enrolled and builds repo slugs from gh column", {
  roster <- tibble::tibble(
    netID    = c("jph", "abc", "xyz"),
    name     = c("John", "Abc Person", "Xyz Person"),
    enrolled = c(0, 1, 1),
    gh       = c("eda-jph", "eda-abc", "eda-xyz")
  )

  plan <- plan_repos(roster, org = "eda-f26")

  expect_equal(nrow(plan), 2L)
  expect_equal(plan$netID, c("abc", "xyz"))
  expect_equal(plan$repo, c("eda-abc", "eda-xyz"))
  expect_equal(plan$full, c("eda-f26/eda-abc", "eda-f26/eda-xyz"))
  expect_equal(plan$name, c("Abc Person", "Xyz Person"))
})

test_that("plan_repos respects a custom repo column", {
  roster <- tibble::tibble(netID = "abc", enrolled = 1, slug = "team-1")

  plan <- plan_repos(roster, org = "my-org", repo_col = "slug")

  expect_equal(plan$repo, "team-1")
  expect_equal(plan$full, "my-org/team-1")
})

test_that("plan_repos errors when the repo column is absent", {
  roster <- tibble::tibble(netID = "abc", enrolled = 1)

  expect_error(plan_repos(roster, org = "org"), "gh")
})

test_that("plan_repos tolerates a roster with no name column", {
  roster <- tibble::tibble(netID = "abc", enrolled = 1, gh = "eda-abc")

  plan <- plan_repos(roster, org = "org")

  expect_true(is.na(plan$name))
})

test_that("plan_collaborators flags NA and empty handles as missing", {
  roster <- tibble::tibble(
    netID           = c("a", "b", "c"),
    enrolled        = c(1, 1, 1),
    gh              = c("eda-a", "eda-b", "eda-c"),
    github_username = c("gh-a", NA, "  ")
  )

  plan <- plan_collaborators(roster, org = "org")

  expect_equal(plan$missing, c(FALSE, TRUE, TRUE))
  expect_equal(plan$full[1], "org/eda-a")
})

test_that("plan_collaborators errors when the handle column is absent", {
  roster <- tibble::tibble(netID = "a", enrolled = 1, gh = "eda-a")

  expect_error(
    plan_collaborators(roster, org = "org"),
    "github_username"
  )
})

test_that("plan_collaborators respects a custom username column", {
  roster <- tibble::tibble(
    netID    = "a",
    enrolled = 1,
    gh       = "eda-a",
    handle   = "octocat"
  )

  plan <- plan_collaborators(roster, org = "org", username_col = "handle")

  expect_false(plan$missing)
  expect_equal(plan$user, "octocat")
})
