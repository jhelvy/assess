# assess 0.11.0

- `create_repos()`, `invite_collaborators()`, and `push_repos()` now read each
  repo's exact name from a roster column (`repo_col`, default `"gh"`) instead of
  building it from a `prefix`. The `prefix` argument is removed. Repos are
  `<org>/<gh>`, so the org's term (e.g. `f26`) never gets duplicated at the repo
  level. The same `gh`-column convention drives team repos.

# assess 0.9.0

- Added `drop_netid` argument in `save_final_grades()` to allow student-specific dropping while grading.

# assess 0.8.3

- Print ID when making reports

# assess 0.8.2

- Add override column in gradebook to assign an override grade

# assess 0.8.1

- Print out netIDs when unzipping

# assess 0.7.0

- Added `copy_team_grades()` function

# assess 0.3.2

- Updated how grades are computed where the weight of any missing grades (not yet assigned or graded) is proportionally re-distributed across other assignments.

# assess 0.2.0

- Copied changes made over from Spring 2023 P4A course

# assess 0.1.0

- Initial version of package
