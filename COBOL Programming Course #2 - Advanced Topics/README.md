# COBOL Programming Course #2 - Advanced Topics

This project is a set of training materials and labs for an Advanced Topics course on COBOL. Free and publicly available client tooling is used to interact with the mainframe environment allowing participants to easily leverage these technologies after the course.

## How to get started

There are currently no official releases of COBOL Programming Course #2 - Advanced Topics. All content remains in development. We encourage you to view and contribute to the [book](COBOL%20Programming%20Course%20%232%20-%20Advanced%20Topics.md) :)

If you run into any issues, please don't hesitate to reach out on our [slack channel](https://openmainframeproject.slack.com/archives/C011NE32Z1T).

## Providers

These materials are being used by other organizations to provide COBOL training to the community. Neither this project nor Open Mainframe Project reviews, maintains, or endorses any one of these particular providers. If you are using these materials in your training materials, feel free [to edit and issue a pull request](https://github.com/openmainframeproject/cobol-programming-course/edit/governance-docs/README.md) to have it included.

- IBM has provided a [free environment for completing this lab](http://ibm.biz/cobollabs).

## Build

A PDF can be built using the following [pandoc](https://pandoc.org/) command. Note: pdflatex is required. [MiKTeX](https://miktex.org/) can be installed to accomodate this. Prior to issuing the following command, all subscript and superscript notation must be changed. Find all instances of `<sub>` and `<\sub>` and replace them with `~`. Find all instances of `<sup>` and `<\sup>` and replace them with `^`. This is the notation required for pandoc but this would make it difficult to read the markdown in GitHub.

```
pandoc "COBOL Programming Course #2 - Advanced Topics.md" -o "COBOL Programming Course #2 - Advanced Topics.pdf" --number-sections --toc -B Front_Matter.tex --listings
```