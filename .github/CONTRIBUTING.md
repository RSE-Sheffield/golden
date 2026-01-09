# Golden Contribution Guide

Thanks for spending time contributing to Golden.

<!-- Could link to code of conduct/governance docs here if they ever exist -->

## Contribution types and what we're looking for

Content we typically accept:

- Bug fixes
- Typographic corrections
- New content filling identified gaps within the test suite or documentation

Content we do not typically accept:

- Edits purely for tone, readability, or efficiency
- Changes to the workflow for users of the library
- Niche features which break from the underlying design philosophy of Golden

These are general guidelines, if you're not sure whether you proposed change would be accepted then open an issue to discuss it with the maintainers before committing significant effort. It's always worth raising an issue to discuss feature proposals, before carrying out the work and submitting a pull request.

## Filing issues

- Please search existing [issues](https://github.com/RSE-Sheffield/golden/issues?q=is%3Aissue) before creating a new issue, to ensure you aren't submitting a duplicate.
- Each issue should have a single purpose. A single issue shouldn't report multiple bugs, or request multiple features.
- Feel free to react and comment on existing issues to show your support, or provide additional context and information.

By filing issues, you're contributing!

## Pull Requests (PRs)

If you are not fixing an open issues, we would encourage you to first discuss your idea in an issue to save your time. This both saves duplicate effort, by flagging to other users that you're working on something and avoids wasted effort if your PR is deemed out of scope.

1. Continuous integration checks within the PR must all be passing prior to review (this may require them being initialised by a maintainer).
2. Every new feature of bug-fix must have one or more new tests (found within `tests/testthat`). These tests are expected to fail without your new feature or bug-fix. Continuous integration checks will ensure they pass as part of your PR.
3. If extending the public API, functions exposed to users, the man pages (`man`) and potentially vignettes (`vignettes`) should be update, to introduce this to users.
4. Create new PRs against the `main` branch. This can be achieved by forking the repository and creating your changes within the fork and submitting a pull request with this parent repository as the base.
5. Single feature/bug-fix per PR. This ensures that commits to `main` are granular, and aides the ability for maintainers to review PRs.
6. You may wish to run `R CMD check` or `devtools::test()` locally before creating your PR, to faster identify and resolve likely continuous integration check failures.
7. In future, we may add a formal changelog (e.g. `NEWS`) to the repository. If such a file exists, this will require updating by every PR to detail what has changed.
