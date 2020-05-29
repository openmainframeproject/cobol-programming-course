# COBOL Programming Course #1 - Getting Started

This project is a set of training materials and labs for a "Getting Started" level course on COBOL. Free and publicly available client tooling is used to interact with the mainframe environment allowing participants to easily leverage these technologies after the course. See the Commercial Tooling [README](Commercial%20Tooling/README.md) for information on completing this course using commercial tooling.

## How to get started

To begin, select a recent [release](https://github.com/openmainframeproject/cobol-programming-course/releases) of the course. Under a given release, you should see two assets.

- The COBOL Programming Course #1 - Getting Started PDF asset contains the course instructions.
- The Source code asset contains everything that was in the project when the release was published.

To complete the course, you will also need to do one of the following: 
- Select a [provider](#providers) below and follow their registration process. Once complete, you should be given system details to use in the course.
- Upload the source code for the course (located in the Labs folder) to your mainframe environment.

If you run into any issues, please don't hesitate to reach out on our [slack channel](https://openmainframeproject.slack.com/archives/C011NE32Z1T).

## Providers

These materials are being used by other organizations to provide COBOL training to the community. Neither this project nor Open Mainframe Project reviews, maintains, or endorses any one of these particular providers. If you are using these materials in your training materials, feel free [to edit and issue a pull request](https://github.com/openmainframeproject/cobol-programming-course/edit/governance-docs/README.md) to have it included.

- IBM has provided a [free environment for completing this lab](http://ibm.biz/cobollabs).

## Build

The PDF is built using the following [pandoc](https://pandoc.org/) command. Note: pdflatex is required. [MiKTeX](https://miktex.org/) can be installed to accomodate this. 

```
pandoc "COBOL Programming Course #1 - Getting Started.md" -o "COBOL Programming Course #1 - Getting Started.pdf" --number-sections --toc -B Front_Matter.tex --listings
```

`Front_Matter.tex` contains the content before the table of contents. `COBOL Programming Course #1 - Getting Started.md` contains the body. The command combines the two, generates the section numbers and table of contents for the body, and outputs `COBOL Programming Course #1 - Getting Started.pdf`