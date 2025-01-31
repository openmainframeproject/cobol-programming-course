# COBOL Programming Course #4 - Testing

This project is a set of training materials and labs for testing COBOL applications on z/OS. Free and publicly available client tooling is used to interact with the mainframe environment allowing participants to easily leverage these technologies after the course.

## How to get started

To begin, select a recent [release](https://github.com/openmainframeproject/cobol-programming-course/releases) of the course. Under a given release, you should see two assets.

- The COBOL Programming Course #4 - Testing PDF asset contains the course instructions.
- The Source code asset contains everything that was in the project when the release was published.

To complete the course, you will also need to do one of the following: 
- Select a [provider](#providers) below and follow their registration process. Once complete, you should be given system details to use in the course.
- Upload the source code for the course (located in the Labs folder) to your mainframe environment.

## Providers

These materials are being used by other organizations to provide COBOL training to the community. Neither this project nor Open Mainframe Project reviews, maintains, or endorses any one of these particular providers. If you are using these materials in your training materials, feel free [to edit and issue a pull request](https://github.com/openmainframeproject/cobol-programming-course/edit/governance-docs/README.md) to have it included.

- IBM has provided a [free environment for completing this lab](https://ibm.biz/cobollabs).

## Build

A PDF can be built using the following [pandoc](https://pandoc.org/) command. Note: pdflatex is required. [MiKTeX](https://miktex.org/) can be installed to accommodate this. Prior to issuing the following command, all subscript and superscript notation must be changed. Find all instances of `<sub>` and `</sub>` and replace them with `~`. Find all instances of `<sup>` and `</sup>` and replace them with `^`. This is the notation required for pandoc but this would make it difficult to read the markdown in GitHub.

```
pandoc "COBOL Programming Course #4 - Testing.md" -o "COBOL Programming Course #4 - Testing.pdf" --number-sections --toc -B Front_Matter.tex --listings
```