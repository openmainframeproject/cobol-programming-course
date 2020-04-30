# COBOL Programming Course

![License](https://img.shields.io/github/license/OpenMainframeProject/cobol-programming-course)

This project is a set of training materials and labs for a "Getting Started" level course on COBOL.

## How to use

There are two sets of materials available

- [Chapters](https://github.com/openmainframeproject/cobol-programming-course/blob/master/COBOL%20Programming%20with%20VSCode.pdf) which contain the lessons, broken up into chapters.
- [Labs](https://github.com/openmainframeproject/cobol-programming-course/tree/master/Labs) which contain the source code used in the chapters.

## Build

The PDF is built using the following [pandoc](https://pandoc.org/) command. Note: pdflatex is required. [MiKTeX](https://miktex.org/) can be installed to accomodate this. 

```
pandoc "COBOL Programming with VSCode.md" -o "COBOL Programming with VSCode.pdf" --number-sections --toc -B Front_Matter.tex --listings
```

`Front_Matter.tex` contains the content before the table of contents. `COBOL Programming with VSCode.md` contains the body. The command combines the two, generates the section numbers and table of contents for the body, and outputs `COBOL Programming with VSCode.pdf`

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

All contributions must align with the [Open Mainframe Project contribution guidelines](https://github.com/openmainframeproject/tac/blob/master/process/contribution_guidelines.md), including having a DCO signoff on all commits.

# Discussion

You can connect with the community in a variety of ways...

- [COBOL Programming Course Discussion Mailing list](https://lists.openmainframeproject.org/g/cobol-course-discussion)
- [#cobol-programming-course channel on Open Mainframe Project Slack](https://slack.openmainframeproject.org)
- ['COBOL technical questions' category on Open Mainframe Project Community Forums](https://community.openmainframeproject.org/c/cobol-technical-questions/16)

## Providers

These materials are being used by other organizations to provide COBOL training to the community. This project, nor Open Mainframe Project, doesn't review, maintain, or endorse any one of these particular providers. If you are using these materials in your training materials, feel free [to edit and issue a pull request](https://github.com/openmainframeproject/cobol-programming-course/edit/governance-docs/README.md) to have it included.

- IBM has provided a [free environment for completing the labs](http://ibm.biz/cobollabs).
- IBM has provided a [free trial to IDz environment](https://developer.ibm.com/mainframe/products/ibm-z-open-development).

## Governance

This project is openly governed as defined in [GOVERNANCE.md](GOVERNANCE.md).

## Credits

The courseware materials were made available through a joint collaboration IBM, it's clients, and American River College and proposed as a new project by IBM.
