# assess 0.12.1

- `push_repos()` now pushes a clone that is ahead of its remote even when the
  working tree is clean. Previously it inspected only `git status`, so a commit
  that had already been made locally but never landed -- e.g. a push rejected
  because the clone was behind, then rebased by `pull_repos()` -- was reported
  as `NOCHANGE` and silently left unpushed.

# assess 0.12.0

- Added `clone_repos()`: clones any enrolled student's repo that isn't on disk
  yet and leaves existing clones untouched. This is now the only function that
  creates clones.
- Added `pull_repos()`: fetches each clone and rebases it (`--autostash`) onto
  whatever students have pushed. Run it before `push_repos()`, every time --
  `push_repos()` never fetches, so a clone that has fallen behind commits fine
  and then has its push rejected ("Updates were rejected because the remote
  contains work that you do not have locally").
- Removed the `clone_missing` argument from `push_repos()`. Cloning is
  `clone_repos()`' job; `push_repos()` now always skips repos that aren't
  cloned, pointing at `clone_repos()` in the message.

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
