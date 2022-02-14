# Contributing to COBOL Programming Course

Thank you for your interest in contributing to COBOL Programming Course. This document explains our contribution process and procedures.

## Primary Intent

The primary intent of the COBOL Programming Course is to offer free and accessible education for individuals interested in learning COBOL on z/OS architecture. To ensure this, all developer tooling leveraged in this course to interact with the mainframe environment must be free and publicly accessible so that participants can continue leveraging these technologies after completing the course.

For major changes, please open an issue first to discuss what you would like to change. All contributions should be made with the primary intent of the course in mind.

## Sign all of your git commits!
All contributions must align with the [Open Mainframe Project contribution guidelines](https://github.com/openmainframeproject/tac/blob/master/process/contribution_guidelines.md), including having a [DCO signoff](https://developercertificate.org/) on all commits.

Whenever you make a commit, it is required to be signed. If you do not, you will have to re-write the git history to get all commits signed before they can be merged, which can be quite a pain.

Use the "-s" or "--signoff" flags to sign a commit.

Example calls:
* `git commit -s -m "Adding a test file to new_branch"`
* `git commit --signoff -m "Adding a test file to new_branch"`

Why? Sign-off is a line at the end of the commit message which certifies who is the author of the commit. Its main purpose is to improve tracking of who did what, especially with patches.

Example commit in git history:

```
Add tests for the payment processor.

Signed-off-by: Humpty Dumpty <humpty.dumpty@example.com>
```

What to do if you forget to sign off on a commit?

To sign old commits: `git rebase --exec 'git commit --amend --no-edit --signoff' -i <commit-hash>`

where commit hash is one before your first commit in history

If you are committing via the GitHub UI directly, check out these [useful tools](https://github.com/openmainframeproject/tac/blob/main/process/contribution_guidelines.md#useful-tools-to-make-doing-dco-signoffs-easier).

## Pull Requests
- All contributions must be reviewed by a [committer](COMMITTERS.csv) 
- Anyone can comment on a pull request to request delay on merging or to get questions answered.

## License and Copyright
All documentation and lab codes for the COBOL Programming Course will be made available under the [Creative Commons Attribution 4.0 International License](LICENSE).

All lab codes need to include a header to clearly show this information in [SPDX short-form identifier](https://spdx.dev/ids/) along with a general copyright statement as shown below:

```
/* Copyright Contributors to the COBOL Programming Course
/* SPDX-License-Identifier: CC-BY-4.0
```

For binary or configuration files that do not support comments, the license may be omitted. If comments are supported, the license header should be included.