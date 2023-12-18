---
geometry:
- margin=1in
header-includes:
- \lstset{basicstyle=\ttfamily,
    stepnumber=2,
    numbersep=5pt,
    backgroundcolor=\color{black!10},
    showspaces=false,
    showstringspaces=false,
    showtabs=false,
    tabsize=2,
    captionpos=b,
    breaklines=true,
    breakatwhitespace=true,
    breakautoindent=true,
    linewidth=\textwidth}
colorlinks: true
toccolor: Blue
---
\newpage
# Testing

In this chapter, we focus on the importance of thoroughly testing software to ensure its quality.  No language or program is exempt from the importance of testing and automated testing, including programs written in COBOL.  The chapter explains how this can only be achieved in an effective and efficient way by automating the testing as part of a continuous delivery pipeline and introduces a framework that can enable such automation.


We will look at some of the background to software testing and the different types of testing, the value to an enterprise of using a continuous integration/continuous delivery pipeline, why automation is vital in CI/CD pipelines, and the risks of not adopting automation.

 

- **Importance of testing**

     - **What is testing and why do it?**

     - **What is unit testing?**

     - **What is function/integration/system testing?**

     - **The role of exploratory testing**

- **Why does automated testing matter?**

    - **Value of automated testing in continuous delivery**

    - **Dangers of not automating testing**

    - **Risks of not adopting continuous delivery**

    - **Use of automated testing for system reliability**

- **Testing on z/OS**

     - **Importance of testing on z/OS**

     - **Challenges for test automation on z/OS**

- **Basics of continuous integration, continuous delivery**

    - **Introduction to DevOps and CI/CD pipelines**

    - **Focus on efficiency and automation through the pipeline**

    - **Tests as the quality gatekeeper through phases of the pipeline**

    - **Managing the pipeline when test cases fail**

- **How can test automation be achieved on z/OS?**


## Importance of testing

If software is not tested, how can you or your users have confidence that it will behave as intended?


### What is testing and why do it?

Testing is the process of validating that software carries out the actions and provides the results that are expected.  It can, and should, encompass a number of different types of testing carried out at a number of different phases during the development lifecycle of the product, and potentially continue after the software has been released.  This allows errors and omissions in the software to be uncovered, as well as ensuring that it meets the user requirements.

 

There are several alarming examples of software defects and problems that could have been avoided by applying the proper and sufficient testing.

 

Put simply, software testing can be described as verification of the system or application under test.  Cem Kaner offers this definition:

"Software testing is an empirical technical investigation conducted to provide stakeholders with information about the quality of the product or service under test."

 

### What is unit testing?

Unit testing is the earliest phase of testing and validates that an individual unit or component of a software solution performs as designed.  Unit testing is typically carried out by the developer who wrote the code within the unit or component under test, along with other early validation activities such as static analysis and code review.  That is not to say that the unit tests created by the developer might also be re-run at later stages in the software lifecycle, to validate that the individual units still behave as intended.

 

Unit testing is important because the earlier defects can be found (or avoided), the less resource is required to resolve them.  A problem found early might be corrected by simple editing in minutes, whereas the same problem found later might require a lot of rewriting and re-testing.  Unit testing also often takes advantage of the developer's knowledge of the internals of the unit or units being tested.

 

The unit tests that are created as part of unit testing can also be very important, if they are designed in such a way that they can be run and re-run later in the development cycle, to validate that the behavior of the software units is still correct.

 
### What is function/integration/system testing?

Beyond the realm of unit testing, which is carried out by the developer, are a range of types of testing that typically fall into the realm of the tester.  These validate that when individual units of code are integrated together, or introduced into an existing software system, the overall system both continues to work correctly without any regressions and displays the new functionality that the new code is intended to enable.  There are a multitude of terms used to define these different types of testing, and in most organizations, there will be a sequence of test phases through which code will pass, each with their own names.

 

A useful approach to categorizing types of testing are the **Agile Test Quadrants** , introduced by Brian Marick ([http://www.exampler.com/old-blog/2003/08/21.1.html#agile-testing-project-1](http://www.exampler.com/old-blog/2003/08/21.1.html#agile-testing-project-1)).  This looks at tests in terms of distinguishing whether they are business facing or technology facing, as well as whether they are used by or on behalf of programmers to support programming or are intended to critique the product.  This results in four quadrants (see Figure 1):

 

- **Quadrant Q1** looks at unit testing, as well as component and integration testing, all of which focus on the technology.

- **Quadrant Q2** looks at more business facing and system level tests, such as functional tests and story tests.

- **Quadrant Q3** represents tests that focus on the business-level capability of the product, and aim to critique or discover any problems with this, such as exploratory testing, usability and user acceptance testing

- **Quadrant Q4** is focused on the system as a whole and how it meets business needs, but from a technology perspective, such as performance, load, stress, and security testing.

- Quadrants Q1 and Q2 often lend themselves to automated testing, while Q3 requires a more exploratory and manual approach, and Q4 might require tools focused on testing for performance or security, for example.

![](Images/image207.jpg)

*Figure  1.   Agile Testing Quadrants (from http://tryqa.com/what-are-test-pyramid-and-testing-quadrants-in-agile-testing-methodology/ )*

 

Martin Fowler and Mike Kohn have discussed the concept of a Test Pyramid (see ’The Practical Test Pyramid’: [https://martinfowler.com/articles/practical-test-pyramid.html](https://martinfowler.com/articles/practical-test-pyramid.html)), which emphasizes the importance of a wide base of many small unit tests, then built upon that a set of equally important but less numerous tests that Mike Cohn called 'Service Tests'.  At the top of the pyramid are 'End to End tests', which include user interface tests, and which test the entire system from end to end.  Service tests covers a similar scope of testing as Integration Testing but is a term which has not gained much traction, and in our usage Integration Testing covers all the pyramid above the unit tests.

 

This chapter is not going to attempt to provide definitive definitions of the various types of testing but will use 'Integration Testing' as an umbrella term to cover much of the testing that occurs after development and which lends itself to automation.  This is where individual units of software are tested together, as well as being tested with other components including external parts of the system.  Such tests are usually run in an environment that matches some aspects of the ultimate target environment for the software.  Included within 'Integration Testing' are regression testing, functional testing, system testing, U.I. testing, end-to-end testing, user acceptance testing, performance testing.

 

A distinction should be made between types of **testing** and types of **tests** .  Unit tests for example can, and should, be run during later test phases, especially as part of regression testing.


### The role of exploratory testing

Exploratory testing was defined by Cem Kaner in 1984 (see [http://www.kaner.com/pdfs/QAIExploring.pdf](http://www.kaner.com/pdfs/QAIExploring.pdf)) as, "a style of software testing that emphasizes the personal freedom and responsibility of the individual tester to continually optimize the quality of their work by treating test-related learning, test design, test execution, and test result interpretation as mutually supportive activities that run in parallel throughout the project."

 

Exploratory testing allows a tester to use their skills and experience to discover, investigate and learn about the behavior of the software under test.  In the spirit of the Agile Manifesto, it emphasizes the "personal freedom and responsibility of the individual tester" ([https://www.guru99.com/exploratory-testing.html](https://www.guru99.com/exploratory-testing.html)).

 

In terms of the Agile Testing Quadrants, exploratory testing lies towards the side that aims to 'critique the product', which is covered nicely in this post: [https://www.testingexcellence.com/exploratory-testing-important-agile-projects/](https://www.testingexcellence.com/exploratory-testing-important-agile-projects/) .  

 

Exploratory testing is often referred to as being a 'thinking' activity.  It is also sometimes referred to as ad-hoc testing, but in reality, it is a much more directed and organized activity than being purely ad-hoc.

 

This type of testing makes the best use of the skills of the tester, but clearly by its very nature does not lend itself to automation.  The value of automation of testing lies in freeing the tester from the need to manually carry out repetitive and un-thinking testing.


## Why does automated testing matter?

Organizations that use z/OS for their mission-critical systems have enjoyed growth and success for many years.  So, does it matter if most of the testing is done manually?  In today's world, it is important to be able to respond very rapidly to new market opportunities and threats, and to have confidence in the quality and robustness of the services that you provide.


### Value of automated testing in continuous delivery

As we have seen, if testing cannot be automated, then it is difficult if not impossible to gain the benefits of continuous delivery, reducing the agility of an organization and its ability to react and innovate quickly.  Even responding to a new competitive threat or compliance regulation might not be possible without a CI/CD pipeline and automated testing.


### Dangers of not automating testing

If an organization continues to test manually, then they run the risk that the test cycle will be too long to enable the required agility and speed to market, or the amount of testing will have to be reduced, leading to uncertainty over the quality of the software being delivered, or perhaps both.


### Risks of not adopting continuous delivery

If an organization does not adopt a continuous delivery approach, in which small changes can be constantly delivered and tested, then there is a tendency to save up all the changes until enough have been made to justify the large effort involved in testing a software release.  Not only does this mean that users endure a long wait for new function to become available, but if problems occur then it is harder to identify the cause and to isolate the failing component.


Even more damaging could be the impact of being unable to deliver function rapidly into the marketplace, and missing opportunities as a result.


### Use of automated testing for system reliability

With automated testing in place, it is easy to run the checks which ensure the software is working as expected.  So, whenever a change is introduced into the system, such as applying maintenance, a hardware upgrade, or updates to another component, there is automation available to verify that all is as it should be.  This also means that such changes can be made more easily and with more confidence.


## Testing on z/OS

Many organizations have chosen IBM Z for the most critical aspects of their businesses, especially in industries including banking, insurance, and retail, where disruptions cannot be tolerated.  This means that in addition to relying on the inherent reliability, security and resilience of the platform, such companies also carry out extensive testing before introducing any change.


### Importance of testing on z/OS

Due to the mission critical nature of the software and applications running on z/OS, testing is vital to ensure that services can be provided uninterrupted, and that any change will both work as intended and not impact anything else.  This makes testing arguably more vital on z/OS than on any other platform.

 

The terminology used to describe the various phases of testing of z/OS applications indicate the importance that organizations attach to this testing, with terms like 'Quality Assurance' testing, 'Pre-Production' testing, 'User Acceptance' testing, not to mention the extensive phases of regression testing and performance testing required when introducing any change into the system.

 

This means that the estimates for any project that makes changes to an application on z/OS, or for a new application, include a very large portion of effort allocated to testing.


### Challenges for test automation on z/OS

With such extensive testing required, it might be thought that test automation would be widespread on z/OS.  However, test automation on z/OS has historically proved to be very difficult. 

 

Surveys and user research carried out by the IBM CICS Transaction Server for z/OS organization have shown that between 92 and 95% of testing on the platform is entirely or mostly manual, which is in line with industry estimates that place the percentage of manual testing at around 80% ("Even today, 80 percent of enterprise testing is done manually." - Sandeep Johri, Tricentis CEO).  The manual testing can vary in nature from a test suite that just needs to be set up and run manually, to typing in a sequence of steps that are described in a hardcopy book of test cases.

 

Why is test automation such a challenge on z/OS?  Some of the reasons are:

- Very large systems have been built up over the years; large both in terms of the number and size of components making up each application, and in terms of the environment in which the applications run.  Finding a way to drive these large systems as part of an automated test has proved challenging.

- There is a lack of test automation tools that understand the z/OS operating system, its subsystems and file stores, to make it practical to adopt these tools.

- The data is tightly integrated with the applications which use it and is often used by multiple applications.  Providing suitable test data, which can be isolated for use by each test run, and reset to known values, has been very challenging.

- Many applications rely on components which were developed many years ago, which means that testing needs to ensure these components will still run without issues or regressions. As Rosalind Radcliffe put it, "The best thing about the mainframe is a module compiled 40 years ago will still run. The worst thing about the mainframe is a module compiled 40 years ago will still run." [https://www.sonatype.com/an-innovators-journey-rosalind-radcliffe](https://www.sonatype.com/an-innovators-journey-rosalind-radcliffe)

The difficulty in achieving this has resulted in falling back to manual processes and checks, and as a result continuing to use waterfall processes.  To quote Sandeep Johri again, "If you move to Agile development but your testing cycle is still 6 to 12 weeks due to manual testing, you’ll fall right back into a Waterfall mode” ([https://devops.com/devops-chat-continuous-testing-w-sandeep-johri-ceo-tricentis/](https://devops.com/devops-chat-continuous-testing-w-sandeep-johri-ceo-tricentis/)).  If the testing cycle takes a long time, then developers will be tempted to group together lots of changes to get them tested all together, which is entirely counter to the idea of continuous integration.


# COBOL Check

In this chapter, we discuss the concept of unit testing in COBOL programming language with the use of COBOL Check. It delivers precise, fine-grained unit testing/checking capabilities for COBOL, matching the conceptual level of detail found in unit testing frameworks designed for popular languages like Python, Ruby, C#, Java etc.


- **Introduction to COBOL Check**

  - **What is COBOL Check?**
  - **Where Can you run COBOL Check?**
  - **How does COBOL Check accomplish unit testing?**


## Introduction to COBOL Check

### What is COBOL Check?

COBOL Check serves as a unit testing framework specifically designed to assist COBOL programmers who encounter challenges with modern development methods like test-driven development. One notable drawback of existing tools for COBOL and other mainframe languages is their limited granularity when it comes to unit testing, especially when compared to languages like Java, C++, and C# etc. However, COBOL Check offers a solution by enabling developers to conduct fine-grained unit testing using both gnuCOBOL and Enterprise COBOL.


### Where Can you run COBOL Check?

COBOL Check strives to facilitate the maintenance and modernization of legacy COBOL applications on IBM zSeries systems. It achieves this by providing developers with the flexibility to work on either the mainframe platform or off-platform environments such as Windows, Unix, Linux, or OS X instances disconnected from the mainframe. By harnessing the advantages of fine-grained "micro test" development at the level of individual COBOL paragraphs. 

In this chapter, our focus will be on utilizing COBOL Check with Enterprise COBOL on a mainframe environment.

## How does COBOL Check accomplish unit testing?

With COBOL Check, we can exercise individual COBOL paragraphs in isolation from the rest of the program and without any access to any external resources such as datasets or CICS facilities. COBOL is not designed to do this kind of thing at runtime then how does COBOL check accomplish it?

Developers write test cases using the DSL (domain specific language). The DSL is designed to look similar to COBOL source code, so that it could be intuitive for COBOL programmers.

COBOL Check interprets these test cases and converts them into standard COBOL statements and merges them with the source of the program under the test. This copy of the program under test which contains test code is then compiled and executed. The test code does not run the entire procedure division; instead, it only calls the specific paragraphs that are mentioned in the test case.

COBOL Check uses a default directory structure to retrieve the COBOL programs, the test suites, copybooks etc. you can take a look at the wiki to get a clear understanding of the directory structures: https://github.com/openmainframeproject/cobol-check/wiki/Default-Directory-Structure

### The test pyramid
![](Images/image235.png)

Cobol Check supports fine-grained unit-testing. Let's provide some contextual information to make the meaning of that clearer. This is a popular level of test automation; lets call it a pyramid or a triangle. The fundamental concept revolves around conducting software testing at various levels of abstraction. As we ascend in the diagram, the testing scope encompasses larger software components, whereas descending in the diagram involves testing smaller software elements.

The figure is wide at the bottom and narrow at the top to suggest that we want a large number of small test cases and a smaller number of big test cases. It is preferred because considering how difficult it is to test an entire application with all its external dependencies, environment configuration and test data and to test every combination of inputs and operating conditions at this level would be very tedious and time-consuming. Organizations that do all their testing by running the full system often lack sufficient time to test thoroughly before each release. If we push most of the test cases down to a level, we find we can write more test cases with less effort, so we can exercise each part of the code thoroughly. The test case runs in less time, and each test failure is easier to diagnose and fix.



### Test suites and test cases

* TESTSUITE - Provides a description for a series of test cases. The description is echoed in the output from the test run.  

* TESTCASE - identifies a test case. The description is echoed in the output of the test run.

A program can have multiple test suites, and each test suite can have multiple test cases.

The typical automated check follows these steps:

* Arrange: Establish the preconditions for the test case.

* Act: Execute the code under examination.

* Assert: Validate the anticipated outcome of the test

You can take a look at the COBOL Check wiki page for better understanding: https://github.com/openmainframeproject/cobol-check/wiki/A-Brief-Example


## Lab

In this lab exercise, you will learn to set up your environment for the COBOL Check by connecting to an IBM Z system to access the USS(Unix System Services), view a simple COBOL program and test suites in VS Code, compile them on the USS using COBOL Check to generate a copy of the program under test that includes the test cases and paragraphs to be tested. Then you will copy the newly generated program from USS to MVS datasets and submit JCL to compile the copied COBOL program, and view the output. Refer to “Installation of VS Code and extensions” to configure VS Code if you have not already done so. You can either use IBM Z Open Editor and Zowe Explorer, or Code4z.

To proceed further, it's better to have some knowledge of JCL and linux terminal commands.

1. Get the latest COBOL Check distribution from the GitHub repository of the COBOL Check https://github.com/openmainframeproject/cobol-check/tree/Developer/build/distributions.
  Click on the “View raw” button or the download button on the right most corner. You will get the .zip of COBOL Check.

![](Images/image209.png)

*Figure 1.  Download COBOL Check distribution*

![](Images/image210.png)

*Figure 2.  Button to download the .zip file*

2. Check your download location to view the .zip file and then extract it.

![](Images/image211.png)

*Figure 3.  COBOL Check folder*

3. Open your VS Code with the same team configuration file with the Learn Cobol folder which you have used in course 2. If not, you can also download it from https://github.com/openmainframeproject/cobol-programming-course/releases/latest
  
4. click on the icon of Zowe Explore of VS Code.

![](Images/image212.png)

*Figure 4.  Zowe Explorer*

   In the DATA SETS section, you can view all the PDS (Partitioned datasets) and sequential files present. In the USS (Unix System Services) section, you can view the files or folder that is stored in the USS. USS is a posix compliant linux like environment which makes it easy for developers to interact with previous knowledge of using linux terminals and commands. In the JOBS section, you can view all about the running or completed jobs.


5. Put your username and password in all the three sections by clicking on the search icon to view the files (DATA SETS, USS, JOBS) which you have learned in the previous chapters.

![](Images/image213.png)

*Figure 5.  Enter your username*

![](Images/image214.png)

*Figure 6.  Enter your password*

6. After entering your username and password for the USS section search for `/z/z999XX` . put your username in place of z999XX. Now you can view all the files that are present in the USS.

![](Images/image215.png)

*Figure 7.  Search for /z/z999XX*

7. Another way of interacting with the USS is through the SSH connection. Open your terminal in the vs code window. Issue the command `ssh z99998@192.86.32.250` , in the place of z99998 use your own username. You can find the ip address in the zowe.config file that comes with the team configuration folder.

![](Images/image216.png)

*Figure 8.  ssh connection*

8. Enter your password to view the directories in the USS. Issue your first command `pwd` which will show the directory you are currently in, that is your root directory. Issue the command `ls` to list all the files and folders in this directory.

![](Images/image217.png)

*Figure 9.  present working directory*

9. You need to create a directory for COBOL Check and copy the contents of the COBOL Check distribution that you have downloaded earlier. Issue the command `mkdir cobolcheck` - This will create an empty directory. Then use `ls` command to view the created directory.

![](Images/image218.png)

*Figure 10.  Create COBOL Check directory in the USS*

10. Issue `cd cobolcheck` to enter into the directory.

![](Images/image219.png)

*Figure 11.  Enter into COBOL Check directory in the USS*

    Note: If you left your terminal idle for some time, you need to kill that terminal, again you have to ssh into the mainframe. you can follow from step 8 again.

11. Open another terminal tab and go to the file location where the files and folders of COBOL Check are present. Then use `ls` to see those.

![](Images/image220.png)

*Figure 12.  COBOL Check directory on the local system*

12. To copy all the files and folders from this directory, you need to use the command : `zowe zos-files upload dir-to-uss "." "/z/z99998/cobolcheck" --recursive  --binary-files "cobol-check-0.2.8.jar"` . Then enter your username and password. (Remember to type all the alphabets in small letters).


`Note: Use the latest version (or the version you are using) of the COBOL Check on the above command.`

    
In this command the `.` represents the current directory
and the flag `--recursive` is used for copying all the folders which are even inside other folders.
By default, this command will copy all the files in ascii mode,
but you need to copy `the cobol-check-0.2.8.jar` in binary mode that's why the flag `--binary-files` is used.
You can look into your `/bin` folder of COBOL Check to see the latest version and name which need to use there.
At the time of writing this course material, it is version 0.2.8. 

To know more about copying the files from local machine to USS, you can refer to zowe cli docs: https://docs.zowe.org/v2.4.x/web_help/index.html?p=zowe_zos-files_upload_dir-to-uss.

![](Images/image221.png)

*Figure 13.  Command to copy the files from local machine to USS*

After entering the command, you will get a message uploaded successfully.

![](Images/image222.png)

*Figure 14.  Successfully uploaded message*

13. Now again open your terminal and ssh into the mainframe to view the files and folders in the COBOL Check directory. You can see the contents of uploaded.

![](Images/image223.png)

*Figure 15.  View the contents in the directory*

14. you can use the USS tab of your vs code to view the COBOL Check directory.

![](Images/image224.png)

*Figure 16.  Locate the directories on the USS section in the zowe*

15. Issue `ls -al` command to see the file permission of COBOL Check then issue `chmod +x cobolcheck`. This will give COBOL Check file the executable permission.

![](Images/image225.png)

*Figure 17.  Providing executable permission*

16. Do the same as done above in the `/scripts` folder. Issue command : `cd scripts` to enter into scripts directory then use `chmod +x linux_gnucobol_run_tests` to make this file executable.

![](Images/image226.png)

*Figure 18.  Providing executable permission*

Then issue command `cd ..` to come back to the parent directory.

17. View the COBOL source code and the test case files. You can also use the USS tab.

![](Images/image227.png)

*Figure 19. view files in the USS tab*

18. You can see it comes with some source code and test files. You need to run COBOL Check using the NUMBERS.CBL to check whether COBOL Check is working correctly.
    Issue the command `./cobolcheck -p NUMBERS`.

`Note: You may see the NullPointerException, but that can be ignored.`

![](Images/image228.png)

*Figure 20.  Run a COBOL program with the cobolcheck command*

19. After running the command, the COBOL Check will generate a new source code named `CC##99.CBL` which includes the COBOL source code along with the test cases embedded.

![](Images/image229.png)

*Figure 21.  Output: A new COBOL program with the test cases embedded as statements*

20. You need to copy this file to the MVS data sets (Z99998.CBL) which you can view in your DATA SETS tab. Use the command `cp CC##99.CBL "//'Z99998.CBL(NUMBERS)'"`

![](Images/image230.png)

*Figure 22.  Copy the newly generate COBOL program to the mainframe*

21. view the files in the DATA SETS tab, you can see NUMBERS in Z99998.CBL.

![](Images/image231.png)

*Figure 23.  View the file on the DATA SETS tab*

22. you can view the source code by right-clicking on it then click on ‘pull from mainframe’


23.  Now, to run this code on the mainframe, you need to write a JCL in the Z99XXX.CBL with the same name NUMBERS (this course assumes you have basic knowledge about JCL).

![](Images/image232.png)

*Figure 24.  JCL to run the program*

24.  Now submit the job.

![](Images/image233.png)

*Figure 25.  Submit the job*

25. Open the submitted job in the JOBS tab. Then expand it. Click on `RUN:SYSOUT` to see the desired output of the test case passed or failed. 
    The code `CC 0000` is success. If you get something else, then check the steps and JCL, then resubmit it.

![](Images/image234.png)

*Figure 26.  RUN:SYSOUT-117  view the job output*


\newpage
# Unit Testing with COBOL Check
In the previous Chapter, you have learned to set up the environment for COBOL Check on the USS (Unix System Services). In this chapter, you will see how to take any COBOL program and write unit tests for it (Lab 2). Also, Later in the chapter, you will see the concept of Test-Driven development (Lab 3).

## Lab 2

In this lab, You will have to write test cases for a given COBOL program to check the paragraphs of the program. Here the code is written first, then the tests are done to check the correctness of the code, but in a later portion of the chapter you will see the TDD where the tests are written first then the code is written to pass the tests. i.e., the tests drive the code.


1. In the USS tab, right-click on the cobol folder then click on create file. Name the file with .CBL extension.

![](Images/image236.png)

*Figure 1.  Create new file*

![](Images/image237.png)

*Figure 2.  Name the newly created file*

2. Click on the new file (EMPPAY.CBL) or click on pull it from mainframe to view the file in the vs code editor. Then write the COBOL code.

```
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPPAY.
       AUTHOR. ASHIS KUMAR NAIK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  REC-COUNTER              PIC 9(1).
       01  EMP-RECORD.
           05  EMP-NAME.
                10 EMP-FNAME        PIC X(15) VALUE 'FRANCISCO'.
                10 EMP-LNAME        PIC X(15).
           05  EMP-HOURLY-RATE      PIC 9(3)V99.
           05  EMP-OT-RATE          PIC V99.
           05  EMP-REWARD           PIC V99.
           05  EMP-HOURS            PIC 9(3).
           05  EMP-PAY-WEEK         PIC 9(7)V99.
           05  EMP-PAY-MONTH        PIC 9(7)V99.
       
       PROCEDURE DIVISION.
           PERFORM INITIALIZATION.
           PERFORM PAYMENT-WEEKLY.
           PERFORM PAYMENT-MONTHLY.
           PERFORM SHOW-OUTPUT.
           STOP RUN.
       INITIALIZATION.
           MOVE "Millard"           TO EMP-FNAME.
           MOVE "Fillmore"          TO EMP-LNAME.
           MOVE 19                  TO EMP-HOURS.
           MOVE 23.50               TO EMP-HOURLY-RATE.
       PAYMENT-WEEKLY.
           
           IF  EMP-HOURS >= 40
               MOVE .25 TO  EMP-OT-RATE
           ELSE IF EMP-HOURS >= 50
               MOVE .50 TO EMP-OT-RATE 
           ELSE
               MOVE ZERO TO EMP-OT-RATE.
           COMPUTE EMP-PAY-WEEK =
                (EMP-HOURS * EMP-HOURLY-RATE) * (1 + EMP-OT-RATE).
       PAYMENT-MONTHLY.
           
           IF  EMP-HOURS > 150
               MOVE .50 TO  EMP-REWARD
           ELSE
               MOVE ZERO TO EMP-REWARD.
           COMPUTE EMP-PAY-MONTH =
                (EMP-PAY-WEEK * 4) * (1 + EMP-REWARD).
       SHOW-OUTPUT.
           DISPLAY "Name: " EMP-NAME.
           DISPLAY "Hours Worked Per Week: " EMP-HOURS.
           DISPLAY "Hourly Rate: " EMP-HOURLY-RATE.
           DISPLAY "Bonus-Rate: " EMP-OT-RATE.
           DISPLAY "Gross Pay Per Week: " EMP-PAY-WEEK .
           DISPLAY "Gross Pay Per Month: " EMP-PAY-MONTH .
           DISPLAY "Hi Chris - how's Loretta today?".

```

3. Right-click on the cobol directory under the test directory. Click on the `Create Directory` and name it the same as the program name (EMPPAY).

![](Images/image238.png)

*Figure 3.  Create a new directory*

![](Images/image239.png)

*Figure 4.  Name the directory*

4. create a new file inside the EMPPAY directory as you learned above with the extension .cut which will be a testsuite file. Then write the test suites and test cases.

```
            TestSuite 'Checks the employee payment'

            TestCase 'checks the EMP-OT-RATE TO be 0.25'
            MOVE 50 TO EMP-HOURS
            MOVE 23.50 TO EMP-HOURLY-RATE
            PERFORM PAYMENT-WEEKLY
            EXPECT EMP-OT-RATE TO BE 0.25

            TestCase 'checks the EMP-PAY-WEEKLY > 900 if EMP-HOURS >= 40'
            MOVE 40 TO EMP-HOURS
            MOVE 23.50 TO EMP-HOURLY-RATE
            PERFORM PAYMENT-WEEKLY
            EXPECT EMP-PAY-WEEK >= 900

            TestCase 'checks the EMP-PAY-WEEKLY > 1600 '
            MOVE 60 TO EMP-HOURS
            MOVE 23.50 TO EMP-HOURLY-RATE
            PERFORM PAYMENT-WEEKLY
            EXPECT EMP-PAY-WEEK >= 1600

            TestCase 'checks the EMP-PAY-MONTHLY to be greater than 9600'
            MOVE 160 TO EMP-HOURS
            MOVE 1600 TO EMP-PAY-WEEK
            PERFORM PAYMENT-MONTHLY
            EXPECT EMP-PAY-MONTH >= 9600
```

5. SSH into your USS of the mainframe. Then run the usual command ./cobolcheck -p EMPPAY. Here the flag -p represents the program name. Now the new cobol source code with the name CC##99.CBL is generated as we have seen earlier.

![](Images/image240.png)

*Figure 5. ssh connection*

6. Copy the newly generate source code to DATA SETS (Z99998.CBL) just like as we have seen earlier, use the command `cp CC##99.CBL "//'Z99998.CBL(EMPPAY)'"`

![](Images/image241.png)

*Figure 6. Copy files from USS to PDS*

7. Write a JCL script with the same name as EMPPAY.JCL for running the program (EMPPAY.CBL) on z/os.

```
//EMPPAY JOB 1,NOTIFY=&SYSUID
//***************************************************/
//* Copyright Contributors to the COBOL Programming Course
//* SPDX-License-Identifier: CC-BY-4.0
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(EMPPAY),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(EMPPAY),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=EMPPAY
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//ACCTREC   DD DSN=&SYSUID..DATA,DISP=SHR
//PRTLINE   DD SYSOUT=*,OUTLIM=15000
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF

```

8. Submit the job and view the output in the JOBS section.

![](Images/image242.png)

*Figure 7. Job output*

## Test Driven Development: Unit test with TDD

If your team is hesitant to skip traditional unit tests, remember: TDD drives the code development, and every line of code has an associated test case, so unit testing is integrated into the practice. Unit testing is repeatedly done on the code until each unit function per the requirements, eliminating the need for you to write more unit test cases.

At IBM, teams found that the built-in unit testing produces better code. One team recently worked on a project where a small portion of the team used TDD while the rest wrote unit tests after the code. When the code was complete, the developers that wrote unit tests were surprised to see that the TDD coders were done and had more solid code.

Unlike unit testing that focuses only on testing the functions, classes, and procedures, TDD drives the complete development of the application. Therefore, you can also write functional and acceptance tests first.

To gain the full benefits of unit testing and TDD, automate the tests by using automated unit test tools. Automating your tests is essential for continuous integration and is the first step in creating an automated continuous delivery pipeline.

## Lab 3
1. First write the desired test cases in a file with `deptpay.cut` extension and store it in a directory (name of directory: DEPTPAY, same as the program name) inside the desired COBOL Check directory configuration as we have seen earlier.

```
            TestSuite "Calculation of average Salary"

            TestCase 'NUMBER OF PERSON TO BE 19'
            PERFORM AVERAGE-SALARY.
            EXPECT DEPT-NBR-EMPS TO BE 19

            TestCase 'TOTAL AVERAGE SALARY TO BE 111111.11'
            PERFORM AVERAGE-SALARY.
            EXPECT DEPT-TOTAL-SALARIES TO BE 111111.11
```

The name of testsuite describes with 

2. Write the COBOL source code for the program and save it as `DEPTPAY.CBL` in desired COBOL Check directory.

```

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEPTPAY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DEPT-RECORD.
           05  DEPT-NAME            PIC X(20).
           05  DEPT-LOC             PIC X(12).
           05  DEPT-MANAGER.
                10 MANAGER-FNAME    PIC X(15).
                10 MANAGER-LNAME    PIC X(15).
           05  DEPT-NBR-EMPS        PIC 9(3).
           05  DEPT-TOTAL-SALARIES  PIC 9(7)V99.
           05  DEPT-AVG-SALARY      PIC 9(7)V99.
       PROCEDURE DIVISION.
           PERFORM AVERAGE-SALARY.
           PERFORM DISPLAY-DETAILS.
           STOP RUN.

       AVERAGE-SALARY.
           MOVE "FINANCE"           TO DEPT-NAME.
           MOVE "SOUTHWEST"         TO DEPT-LOC.
           MOVE "Millard"           TO MANAGER-FNAME.
           MOVE "Fillmore"          TO MANAGER-LNAME.
           MOVE 19                  TO DEPT-NBR-EMPS.
           MOVE 111111.11           TO DEPT-TOTAL-SALARIES.
           COMPUTE DEPT-AVG-SALARY =
                (DEPT-TOTAL-SALARIES / DEPT-NBR-EMPS).
      *****
       DISPLAY-DETAILS.
           DISPLAY "Department Name: " DEPT-NAME.
           DISPLAY "Department Location: " DEPT-LOC.
           DISPLAY "Manager FNAME: " MANAGER-FNAME.
           DISPLAY "Manager NAME: " MANAGER-FNAME.
           DISPLAY "Department AVG Salary: " DEPT-AVG-SALARY.
           DISPLAY "Number of employees: " DEPT-NBR-EMPS.

```

3. Write the jcl for automatically copying the program form USS to PDS (partitioned Data sets) and submitting it. Each time you have made some changes to the code or test cases you need to run the command in USS `./cobolcheck -p DEPTPAY`

``` 

//DEPTPAYJ JOB 1,NOTIFY=&SYSUID
//COPY2DS1 EXEC PGM=IKJEFT01
//INUNIX DD PATHOPTS=(ORDONLY),
// PATH='/z/z99998/cobolcheck/CC##99.CBL'
//OUTMVS DD DSN=Z99998.CBL(DEPTPAY),DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSTSIN DD *
 OCOPY IND(INUNIX) OUTDD(OUTMVS) TEXT CONVERT(YES) PATHOPTS(USE)
/*
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=Z99998.CBL(DEPTPAY),DISP=SHR
//LKED.SYSLMOD DD DSN=Z99998.LOAD(DEPTPAY),DISP=SHR
//RUN     EXEC PGM=DEPTPAY
//STEPLIB   DD DSN=Z99998.LOAD,DISP=SHR


```
In the Jobs section, see the output. 
Note: JOBS status code `CC 0000` is for success. If you got something else, correct the code and the test suites. Then resubmit.

4. Add a new testcase to the `deptpay.cut` file.

```
TestCase 'average salary will be greater than 5840'
PERFORM AVERAGE-SALARY.
EXPECT DEPT-AVG-SALARY >= 5840
```

Then submit the jcl and see the output.

## Basics of continuous integration, continuous delivery

Continuous Integration and Continuous Delivery (CI/CD) are important practices within a DevOps approach, which allow software to be evolved and enhanced at a rate that meets the needs of the business and users, rather than being artificially delayed by long testing cycles which in turn result in a batching up of the delivery of software changes.


### Introduction to DevOps and CI/CD pipelines

At its heart, DevOps refers to a building a greater collaboration between the software development and IT operations teams within an organization.  By working together, the systems development lifecycle of building, testing and releasing software can be shortened, and software changes can be continuously integrated and continuously delivered to provide value more rapidly and reliably.

A key step in adopting a successful DevOps approach is to set up a **CI/CD pipeline** .

**Continuous Integration (CI)** is a technique first identified by Grady Booch, involving frequent checking in of small code changes made by a development team, which are merged together into a 'Master' code stream.  CI provides a consistent way of building and packaging changes and validating that they work together, encouraging teams to commit changes more frequently.  Martin Fowler summarized the benefits by saying that "Continuous Integration doesn't get rid of bugs, but it does make them dramatically easier to find and remove".

**Continuous Delivery (CD)** progresses the code changes further around the pipeline process, by automating the delivery of the changed software to a series of environments for testing, and ultimately production.  Some people distinguish between Continuous Delivery, which ensures working and tested releases of software are ready at any time, to production, but requires a manual decision process prior to that final deployment, and continuous deployment which also automates the releasing into production.



All of this requires **Continuous Testing** to ensure that quality software is being made available at each stage, and ultimately delivered to production.  To take this further, there is also value in continuous monitoring of the software in production, continuous feedback from stakeholders and users, and ultimately 'Continuous Everything'.



Making everything as continuous and as automated as possible is what a **DevOps pipeline** aims to achieve.  Such pipelines are often represented pictorially as a funnel, with code units being fed in at one end, passing through various phases of building and testing within a sequence of environments, and being delivered into production at the end.



We prefer to represent the pipeline as a cyclical and iterative process, where developers write, build and unit test their code, repeating steps as needed.  Then those units are fed into further cycles of integration and system styles of testing.  Following release into production, the software system is continually monitored, and further enhancements are planned, resulting in the cycle being repeated.   Figure 2 shows a schematic that represents this view of the pipeline.



![](Images/image208.png)

*Figure  2.  Representation of a DevOps pipeline*



In Figure 2, after planning for the next release, based on user input, and potentially analyzing the code to understand where to introduce the changes, the coding phase begins.  The developer will write, build and unit test the code, gradually adding in function and ensuring that it works as an individual unit as intended, using their preferred IDE and preferred tools for source code management, dependency resolution, etc.  When the code is ready, it will be delivered into the pipeline, which uses an artifact repository to manage the process.  The software now enters the testing phases, where the test environments are provisioned (or might already exist), the code changes are deployed into the environment to be tested, and tests are run.  The tests might, themselves, drive the provisioning and deployment, or this might be done separately.  This phase of the pipeline is an iterative process, moving through different levels of testing, often in different environments.  When failures occur, they need to be efficiently diagnosed, and if needed, the code will be amended, rebuilt, and unit tested again.  Feedback from users will be sought during this phase, to ensure that what is being delivered meets their needs.  When the code change has successfully passed all the required phases of testing, it will be released to production.  The software changes and the production environment will continue to be monitored and planning for the next release can begin.  Figure 2 illustrates some of the tools and products that might be used to implement the various stages of the pipeline.

Some of these building blocks are probably in place at many enterprises, but it is likely that most of the activities are carried out in a manual way and it is the lack of automation of those steps that slows delivery.



A DevOps pipeline can only be efficient, or indeed practical, if it can be automated.


### Focus on efficiency and automation through the pipeline

The driver for building a CI/CD pipeline is to increase efficiency and speed of delivery.  This means that all the steps in the pipeline need to be both quick and reliable and the only realistic way of achieving that is through automation.


### Tests as the quality gatekeeper through phases of the pipeline

As a software change is delivered through a CI/CD pipeline, it is important to ensure its quality and readiness to move on to the next phase.  Testing is the gatekeeper that can give confidence that this is the case - and can also flag up where it is not.

This testing also needs to be carried out continuously, as each change is delivered.  'Continuous testing' has been described as being a process of "testing early, testing often, testing everywhere, and automate" ([https://www.guru99.com/continuous-testing.html](https://www.guru99.com/continuous-testing.html)).



A key principle of the move to DevOps and a CI/CD pipeline is that as much of the testing as possible needs to be automated.  Also, it is not just the tests themselves that need to be automated, but also the checking of whether the tests have passed.



**Some quotes on the importance of test automation:**

"To achieve such speed and agility, it is important to automate all the testing processes and configure them to run automatically when the deployment is completed in the QA environment." From [https://www.softwaretestinghelp.com/devops-and-software-testing/](https://www.softwaretestinghelp.com/devops-and-software-testing/)

"Test automation has become crucial to keep quality control intact while maintaining the speed of releases." From [https://dzone.com/articles/role-of-test-automation-in-devops](https://dzone.com/articles/role-of-test-automation-in-devops)

[DevOps can simply not succeed if it still requires a large number of test cases to be run manually.](https://twitter.com/intent/tweet?source=webclient&amp;via=atlassian&amp;text=DevOps%20can%20simply%20not%20succeed%20if%20it%20still%20requires%20a%20large%20number%20of%20test%20cases%20to%20be%20run%20manually.&amp;url=https://www.atlassian.com/blog/devops/test-automation-secret-devops-success)



However, not all testing can or should be automated.  Exploratory testing might well follow different paths as a result of what is discovered during the process.  Penetration testing often relies on innovation and trying something new, which is difficult to automate.  User acceptance testing will often involve users interacting with the system in a flexible way.



**Testing versus Checking**

Testing is an interactive activity that involves evaluating whether software meets its purpose, by exploring and experimenting with its behavior.  This requires human creativity and cannot be automated.  What is possible to automate is the checking that the software meets these expectations that we have discovered by testing.  James Bach and Michael Bolton introduced this distinction between testing and checking (see [https://www.satisfice.com/blog/archives/856](https://www.satisfice.com/blog/archives/856)): "we distinguish between aspects of the testing process that machines can do versus those that only skilled humans can do. We have done this linguistically by adapting the ordinary English word “checking” to refer to what tools can do.".



When we speak of testing in this chapter from this point on, we are referring to what would more strictly be called **checking**, rather than testing.



The key point is that anything that lends itself to automation should be automated, to allow time for these other activities where automation is genuinely not appropriate.  The fact that something is quite difficult to automate should not be used as an excuse for failing to automate it.


### Managing the pipeline when test cases fail

As noted above, if the tests are automated, but checking or reacting to their results is not, then that is only half of the story.  When building a CI/CD pipeline, thought should be given to what happens when test cases fail.



The diagnostics that will reveal the cause of the failure should be collected in a known location and made available in an easy way to the engineer who will investigate the problem.  The software change that caused the failure should be easily identifiable, and its progress through the pipeline should be halted, or reversed.  Other software changes that have not caused the failure should be able to continue unimpeded, unless they are only viable with the failing change, which should also be something the pipeline can detect and act upon.  Finally, it is possible that the failure could be due to a problem with the test or the test environment, and those possibilities must also be easy to detect and investigate.


## How can test automation be achieved on z/OS?

Many organizations have built, or are now building, CI/CD pipelines for their z/OS applications, with considerable success.  These teams are looking to test automation to help them achieve an efficient pipeline. This chapter described an approach to test automation on z/OS, by using a framework for automating tests which offers deep integration with z/OS capabilities.
