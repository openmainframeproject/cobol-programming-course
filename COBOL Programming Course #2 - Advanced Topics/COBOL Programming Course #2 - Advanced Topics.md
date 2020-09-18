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
- \hypersetup{colorlinks=true,
              linkcolor=blue}
---
\newpage
# COBOL Application Programming Interface (API)
- **Enterprise COBOL APIs**
     - **z/OS Middleware**
     - **COBOL API Communication with Middleware**
     - **COBOL EXEC SQL**
     - **COBOL Data Items**

- **SQL Capability within Enterprise COBOL**
     - **Enterprise COBOL Application Programming and SQL Guide**
     - **Db2 Data Base Administration (DBA) vs Application Programming**

- **Lab**
    - **Using VSCode and Zowe Explorer**


# COBOL Challenges

As you have now handled some basic exercises, we have prepared a new section containing more advanced exercises that test your ability to resolve bugs and other issues in COBOL programs. Each exercise will have a short description and a goal to be accomplished.

In case you get stuck, a blog with instructions will be published shortly after each exercise.

Happy Coding!

\newpage

## COBOL Challenge - Debugging

It is 2020 in Washington, D.C. John Doe runs a program which provides financial reports on US Presidents and tallies the number of reports from the state of Virginia. Everything seems OK. (see below)

![](Images/cobolch1-img2.png)

John is satisfied, as he can see that everything is working as it should be. He calls it a day and goes home.

The next day, when he comes back to the office, his colleague Mari tells him “I’ve made some changes to one of your programs so that it also tallies the number of presidents who spent more than their allowed limit. Check it out.”

He runs his usual reports and sees the following:

![](Images/cobolch1-img1.png)

Clearly, Mari’s changes to the program that generates the reports have broken something.

Can you fix the code to get the correct result? The new source code is named **CBL0106** and the JCL is **CBL0106J**. In case you get stuck, the solution is in the file **CBL0106C**. 

You can find them in the github repository for the COBOL course, in the subfolder **/COBOL Programming Course #2 - Advanced Topics/Challenges/Debugging**.

## COBOL Challenge - The COVID-19 Reports

Today, you are tasked to create a COVID-19 Summary Report of all the countries around the world, using information from the COVID19API website.

### Instructions

1. Extract the response from this API: https://api.covid19api.com/summary. You will receive a JSON file that is similar to the image below:

    ![](Images/cobolchCOV19-img1.png)

2. Convert that file to CSV format. It should look like this. In my example, I only chose the “Countries” part.

    ![](Images/cobolchCOV19-img2.png)

3. Using Zowe, upload the CSV file to the mainframe.

**Hint:** You can use the command `zowe files ul ftds “file location” “dataset name”` to upload the CSV file to the mainframe.

4. Create a new member in your *.CBL data set to write your COBOL program.

**Hint:** You can create a member using Zowe Explorer or Zowe CLI.

5. Write a COBOL program that reads the uploaded CSV file and reformats it to display the contents like this:

    ```
    ********************************************************************
    DATE: 2020-04-22
    TIME: T16:54:5
    COUNTRY: "Antigua and Barbuda"
    COUNTRY CODE: "AG"
    SLUG: "antigua-and-barbuda"
    NEW CONFIRMED CASES: 00000
    TOTAL CONFIRMED CASES: 00023
    NEW DEATHS: 00000
    TOTAL DEATHS: 00003
    NEW RECOVERIES: 00004
    TOTAL RECOVERIES: 00007
    ********************************************************************
    ```

6. Compile and test your work.

### Advanced Tasks

If you want a more challenging approach, try the optional tasks below:

- Reformat the data into a Report Form like this:

![](Images/cobolchCOV19-img3.png)

- Automate. Using NPM and Zowe CLI, run all these steps and create a “one click” COBOL build similar to this:

    ![](Images/cobolchCOV19-img4.gif)

### Solution

To check the solution, refer to the blog post [here](https://medium.com/@jessielaine.punongbayan/solution-covid-19-reports-cobol-challenge-6c509579e3fe?source=friends_link&sk=5a662034a03c91d639b77267ed6abfc9).

Happy Coding! 😉

_Disclaimer: This challenge is also posted in [Medium.com](https://medium.com/@jessielaine.punongbayan/cobol-challenge-covid-19-reports-ee03a946bd23)._

## COBOL Challenge - The Unemployment Claims

Now let's try a more advanced challenge! Your task is to create an end-to-end solution. Our end goal is to build an application that will fire Zowe APIs to the mainframe and display the result in the application. This is how the flow would look:

![](Images/cobolchClaims-img1.png)

_Of course, you do not have to complete the whole challenge if you do not want to. But it would be great if you do_ 😉

### Our Data

The data that we are going to use will come from https://www.data.gov/. According to their website, this is a repository of data that is available for public use. For more information, please visit their website.

To be more specific, we are going to get the monthly unemployment claims of the state of Missouri.
I chose this because it is separated according to different categories:

- **By Age:** https://catalog.data.gov/dataset/missouri-monthly-unemployment-claims-by-age-d20a7
- **By Ethnicity:** https://catalog.data.gov/dataset/missouri-monthly-unemployment-claims-by-ethnicity-2a03b
- **By Industry:** https://catalog.data.gov/dataset/missouri-monthly-unemployment-claims-by-industry-80e86
- **By Race:** https://catalog.data.gov/dataset/missouri-monthly-unemployment-claims-by-race-32ab3
- **By Gender:** https://catalog.data.gov/dataset/missouri-monthly-unemployment-claims-by-sex-f5cb6

You can consume the data in different formats such as CSV, RDF, JSON or XML. You can choose whatever format you like.

### Use Case

You are given a new set of data for The Unemployment Claims. Your tasks are as follows:

- To create a new database for the new set of data and combine the data based on the Record ID field.
- To provide a way for other COBOL programs and other applications to access this newly created database.
- To create a report specifying all the information available in the newly created database. The report will contain, but not be limited to, the following information: Record ID, Age, Ethnicity, Industry, Race and Gender.

### Instructions

1. Create a database. This can be done in various ways but the easiest one the I could think of is a VSAM file. First, create a COBOL program that will consume your data. Then, using the RECORD-ID as the key (which is more visible when reading the CSV file), create a KSDS VSAM file and store all the information there.

    _What this means is that one record will have the RECORD-ID as the key and all the information from The Monthly Unemployment Claims (Fields from Age, Ethnicity, Industry, Race and Gender) will be added or connected to the RECORD-ID._

    The flow would look like this:

    ![](Images/cobolchClaims-img2.png)

2. Create a COBOL sub-routine. This program will allow other programs to read the data from the VSAM file. This sub-routine should be able to perform the following tasks:  
    - accept requests to get information about a specific record ID.
    - _(Optional)_ accept requests to get information about all the records inside the database. What does this mean? It means that instead of providing a record ID, I could provide an indicator that I want to create a report of all the records inside the database.

    ![](Images/cobolchClaims-img3.png)

    _The purpose of the COBOL sub-routine is to allow other COBOL programs or other application to access the information inside the VSAM file._

3. Create a Main COBOL Program. This program will create a report based on the records inside the newly created database. The process is as follows:  
    - The program calls the COBOL sub-routine passing the Record ID or, optionally, an indicator that you want to print all records in the database.
    - It receives the response from the sub-routine.
    - It processes the response and generates a report. This report can be a formal report or just a display in the SYSOUT. It’s up to you.

    The flow should look like this:

    ![](Images/cobolchClaims-img4.png)

4. Create your JCLs.

    _By this point, if you choose to do the exercise using COBOL programs only, you should be able to read the data from your VSAM file, process it and generate a report. The generated report could be an information of a specific record or multiple records._

5. _(Optional)_ Create an application. It can be any type of application; a Mobile App, a Web App or an Electron App. It is up to you. In this application you should be able to view a record by providing a RECORD-ID. The flow would be similar to Step #3.

    This is an example of a possible application design:

    ![](Images/cobolchClaims-img5.png)


    **Hint:** How can I accomplish this? By using the Zowe CLI NPM package, you can fire Zowe APIs that submit your JCLs and get the results. From there, you can view the output and display it in your application. This article can provide a good example.

    **Hint:** What APIs am I going to use? You will use the Jobs Submit API and View Jobs Spool or View Dataset API. For more information, please visit [this site](https://docs.zowe.org/stable/web_help/index.html).

6. _(Optional)_ Create a CI/CD process that will create a nightly build of your application. [This article](https://medium.com/modern-mainframe/beginners-guide-cobol-made-easy-leveraging-open-source-tools-eb4f8dcd7a98?source=friends_link&sk=443517b1feaba8e392f5807246c25ca4) can help explain that process.

    Sample CI/CD Build using CircleCI:

    ![](Images/cobolchClaims-img6.gif)

### Craving more programming challenges?

Add more functionality to your COBOL Sub-routine like:

- Insert a new record
- Update an existing record
- Delete an existing record

I hope that by taking this challenge, you will be able to learn something new! 

Happy Coding! 😉

_Disclaimer: This challenge is also posted in [Medium.com](https://medium.com/@jessielaine.punongbayan/zowe-cobol-challenge-the-unemployment-claims-2e35a42eabaa)._

\newpage

## Hacker News Rankings for Mainframe/COBOL Posts

![](Images/hacker-img1.png)

We will explore the popular Hacker News website for this challenge. Hacker News is an online community started by Paul Graham for sharing "Anything that good hackers would find interesting. That includes more than hacking and startups".


### A Little Background 
The site offers a dynamic list of posts/stories, submitted by users, each of which could be expanded into its own unique comment thread. Readers can upvote or downvote links and comments, and the top thirty links are featured on the front page. Today, more than five million people read Hacker News each month, and landing a blog post on the front page is a badge of honor for many technologists.

### Our Goal
We will be working on a Hacker News 2015-2016 dataset from Kaggle with a full year’s worth of stories:  Our goal is to extract only the Mainframe/COBOL related stories and assign ranking scores to them based on (a simplified version) the published Hacker News ranking algorithm. We will create a front page report that reflects this ranking order. The algorithm works in a way that nothing stays on the front page for too long, so a story’s score will eventually drop to zero over time (the gravity effect). Since our posts are spread out over a year and as older posts will always have a lower (or zero) ranking, we will distort the data so all our stories have the same date and and consider only the times in the ranking score calculation. This will give all our posts a fair chance of landing the front page.  Our front page report is published at 11:59pm. [Here's some additional information on the ranking.](http://www.righto.com/2013/11/how-hacker-news-ranking-really-works.html)

### The Plan
 - There are different creative ways of accomplishing this but here’s our plan: We will have a COBOL program that reads the input CSV file and retrieves only the ***Mainframe/COBOL*** stories. It then calculates the ranking score for the stories by factoring in the time they were posted and the number of votes they received. Each of the records is then written to an output dataset along with the ranking score. 

 - We will then use `DFSORT` to sort the output dataset on ranking score, highest to lowest and display the posts as a simple report mimicking the front page. 
 
 Let's get started!
1. Take a look and familiarize yourself with the dataset on z/OS: `ZOS.PUBLIC.HACKER.NEWS`. This is a CSV file that serves as input to your COBOL program. The file was created by downloading [this Kaggle dataset](https://www.kaggle.com/hacker-news/hacker-news-posts), removing the lengthy `URL` column that is of no relevance to us and uploading it to z/OS. You can directly reference this DS in your JCL. Please avoid making a copy as it is fairly large with around 300,000 records.

 ![](Images/hacker-img2.png)
  
2.	Create your COBOL program in `<userid>.CBL` using VS Code with the Code4z extension installed and enabled – This program will :
      1. Read in each record in the input CSV file
      2. Select only the records that have mention of the words ***Mainframe*** or ***COBOL*** (ignore case) in the `Title` field
      3. Calculate the ranking score for each record based on the number of votes it received and the time it was posted (Ignore date as we assume all posts were created on the same date)
          
          ![](Images/hacker-img3.png)
      4. Write the record to an output file along with the ranking score

      ![](Images/hacker-img4.png)
      
3. Copy/Modify/Create a JCL in `<userid>.JCL` for compiling/linking and running the program against input/output datasets.

4. Submit the job (via `Zowe Explorer` or `Zowe CLI`), debug and test to create the output dataset.

5. Next add a new step in the JCL member to run the `DFSORT` utility on the output dataset from the previous step. The sort should be done on the ranking score field, from highest to lowest. Use `DFSORT` to also print headers for our front page. As this is a new utility not covered in the course, please check out these links to explore this very powerful and versatile tool:
 
   [Getting started with DFSORT](https://www-01.ibm.com/servers/resourcelink/svc00100.nsf/pages/zOSV2R3sc236880/$file/iceg200_v2r3.pdf)

   [Example with DFSORT](https://www.ibm.com/support/knowledgecenter/en/SSLTBW_2.1.0/com.ibm.zos.v2r1.icea100/ice2ca_Example_10._Sort_with_OUTFIL.htm)


6. Run and debug until the front page looks ready! Which posts ranked among the highest? Here's a look at the generated report:

![](Images/hacker-img5.png)


Hope you have fun working on this Challenge. Happy COBOL coding!
