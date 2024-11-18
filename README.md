# pr-state

Prints a summary of the open PRs of a github project, grouped by the user who has to take an action next.

## Usage

*Important* Set _GITHUB_TOKEN_ variable if you want to get statistics about a private repository.

```
pr-state <org> <repo> (user)*

15 open PRs
user1:
  PR1238 - Add backend (3 files)
    Needs reviewers
  PR1172 - Fix bugs (17 files)
    Needs reviewers
  PR1157 - Add AI (2 files)
    Needs reviewers
user2:
  PR1229 - Fix UI (8 files)
    Awaiting review. Authored by user1
```

