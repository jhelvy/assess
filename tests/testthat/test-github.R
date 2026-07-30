# Tests for the pure planning logic behind the GitHub repo functions. The
# gh/git-calling paths are not unit-tested (they shell out); these cover the
# repo-name construction, enrolled filtering, and missing-handle detection.

test_that("plan_repos filters to enrolled and builds repo slugs", {
  roster <- tibble::tibble(
    netID    = c("jph", "abc", "xyz"),
    name     = c("John", "Abc Person", "Xyz Person"),
    enrolled = c(0, 1, 1)
  )

  plan <- plan_repos(roster, org = "eda-f26")

  expect_equal(nrow(plan), 2L)
  expect_equal(plan$netID, c("abc", "xyz"))
  expect_equal(plan$repo, c("eda-f26-abc", "eda-f26-xyz"))
  expect_equal(plan$full, c("eda-f26/eda-f26-abc", "eda-f26/eda-f26-xyz"))
  expect_equal(plan$name, c("Abc Person", "Xyz Person"))
})

test_that("plan_repos honors a custom prefix distinct from the org", {
  roster <- tibble::tibble(netID = "abc", enrolled = 1)

  plan <- plan_repos(roster, org = "my-org", prefix = "course-f26")

  expect_equal(plan$repo, "course-f26-abc")
  expect_equal(plan$full, "my-org/course-f26-abc")
})

test_that("plan_repos tolerates a roster with no name column", {
  roster <- tibble::tibble(netID = "abc", enrolled = 1)

  plan <- plan_repos(roster, org = "org")

  expect_true(is.na(plan$name))
})

test_that("plan_collaborators flags NA and empty handles as missing", {
  roster <- tibble::tibble(
    netID           = c("a", "b", "c"),
    enrolled        = c(1, 1, 1),
    github_username = c("gh-a", NA, "  ")
  )

  plan <- plan_collaborators(roster, org = "org")

  expect_equal(plan$missing, c(FALSE, TRUE, TRUE))
  expect_equal(plan$full[1], "org/org-a")
})

test_that("plan_collaborators errors when the handle column is absent", {
  roster <- tibble::tibble(netID = "a", enrolled = 1)

  expect_error(
    plan_collaborators(roster, org = "org"),
    "github_username"
  )
})

test_that("plan_collaborators respects a custom username column", {
  roster <- tibble::tibble(
    netID    = "a",
    enrolled = 1,
    handle   = "octocat"
  )

  plan <- plan_collaborators(roster, org = "org", username_col = "handle")

  expect_false(plan$missing)
  expect_equal(plan$user, "octocat")
})
