## Welcome to the Open Mainframe Project, Cobol Programming Course!

To obtain the greatest benefit out of this course, please follow the following instructions, STEP by STEP.

Request your mainframe credentials: [here](https://www-01.ibm.com/events/wwe/ast/mtm/cobolvscode.nsf/enrollall?openform)

The return email should direct you: [here](https://github.com/openmainframeproject/cobol-programming-course/)

Download the [PDF Version](https://github.com/openmainframeproject/cobol-programming-course/raw/master/COBOL%20Programming%20with%20VSCode.pdf) of the class instruction manual (materials), [COBOL Programming with VSCode](https://github.com/openmainframeproject/cobol-programming-course/blob/master/COBOL%20Programming%20with%20VSCode.md).  The free [Acrobat Reader](https://get.adobe.com/reader/) can be downloaded if needed to view the .pdf file.  Note: The .md version is the official, up-to-the-minute updated version of the class materials.  The .pdf version is generated from the .md version on a regular basis, so it may not reflect the most up-to-date changes.  A version number/date to be published within the .pdf document has been proposed by project participants, so currently, it is not easily determinable if the .pdf version includes the absolute most recent updates and corrections made to the .md version. 

Please follow the Course Manual instructions, STEP by STEP and always substitute your Z8xxxx id, issued above, in place of the example id, Z99998, wherever it is mentioned (as an example) in the class instruction manual. It is never a good practice to embed passwords into parameter files. Leave your password blank, when prompted, if blank is allowed. The system will prompt you for your password, if and when it is necessary.

The following IP addresses are needed for this course.  The class instruction manual will explain when and where they are needed:
IP address for VSCode extension: https://192.86.32.250:10443  (This is not a link meant to be clicked on!)
IP address for TN3270: 192.86.32.250 Port 623
IP address for IDz: 192.86.32.250 Port 4035

Additionally, the slack channel for questions and discussion is: [here](https://openmainframeproject.slack.com/)

FAQ:
1) "How do I make numbers print properly?"  [Explanation of Cobol Display Variables](https://github.com/openmainframeproject/cobol-programming-course/issues/44).

2) "I've completed the course, now what?"  Try: [this](https://github.com/openmainframeproject/cobol-programming-course/issues/46) or for a real world challenge, try: [this](https://medium.com/@jessielaine.punongbayan/cobol-challenge-covid-19-reports-ee03a946bd23).

**IBM Resources:**

**COBOL Fridays:**

[A Beginner's Practical Approach to COBOL](https://developer.ibm.com/technologies/cobol/videos/cobol-programming-with-vscode-course-launch-webinar) - Webinar - Originally broadcast: 2020-04-17 - 1 hour

[Get hands on! Run a COBOL program](https://developer.ibm.com/technologies/cobol/videos/get-hands-on-run-a-cobol-program) - Webinar - Originally broadcast: 2020-04-24 - 45 minutes

Next Cobol webinar: Fri, May 1, 2020, 11:00-12:00 Noon EDT:
[Let's Talk Syntax - Cobol Syntax, Structure and File Handling Unique to Cobol](https://www.eventbrite.com/e/cobol-fridays-lets-talk-syntax-tickets-103783635648) - Recorded session: TBD.

[Want to become a Tech Entrepreneur? Here’s how I did it & you can too](https://ibm.webex.com/recordingservice/sites/ibm/recording/play/16546b7dc0274b90a313f2fbe1f86706) - Webinar - Originally broadcast: 2020-04-23 - 1 hour

[IBM Developer - COBOL](https://developer.ibm.com/technologies/cobol/)
[IBM Z and LinuxONE Community](https://www.ibm.com/community/z/)
[Master the Mainframe - Get hands-on Experience Across a Variety of Technologies](https://www.ibm.com/it-infrastructure/z/education/master-the-mainframe)
[Open Mainframe Project Website - The Linux Foundation Projects](https://www.openmainframeproject.org/)

Additional Course Resources:

YouTube Video: [So you want to learn COBOL?](https://youtu.be/77o14aHUuSo)

[An Introduction to the Zowe Virtual Desktop](https://medium.com/zowe/an-introduction-to-the-zowe-virtual-desktop-6e0140644875)

[IBM Reference Manuals](https://www.ibm.com/support/pages/enterprise-cobol-zos-documentation-library)

[IBM Enterprise COBOL for z/OS,  Language Reference v6.3](http://publibfp.boulder.ibm.com/epubs/pdf/igy6lr30.pdf)

[IBM Enterprise COBOL for z/OS, Messages and Codes v6.3](http://publibfp.boulder.ibm.com/epubs/pdf/c2746481.pdf)

[IBM Setting property groups | IBM Z® Open Editor](https://ibm.github.io/zopeneditor-about/Docs/setting_propertygroup.html#mvs-property-groups-using-the-zowe-cli)

[Installing Zowe CLI from an online registry](https://docs.zowe.org/v1-1-x/user-guide/cli-installcli.html#installing-zowe-cli-from-a-local-package)
[Installing Zowe CLI from source](https://github.com/zowe/zowe-cli#install-zowe-cli-from-source)

[IBM z/OS MVS JCL Reference v2r3](https://www-01.ibm.com/servers/resourcelink/svc00100.nsf/pages/zOSV2R3SA231385/$file/ieab600_v2r3.pdf)

[IBM DFSORT: ICETOOL Mini-User Guide](https://www.ibm.com/support/pages/sites/default/files/inline-files/$FILE/sorttool.pdf) - October, 2010 by Frank L. Yaeger
[DFSORT′s Year 2000 Features (The New Generation)](https://www.ibm.com/support/pages/sites/default/files/inline-files/$FILE/sort2000.pdf) - October, 1998 by Frank L. Yaeger
[IBM DFSORT Application Programming Guide v2r3](https://www-01.ibm.com/servers/resourcelink/svc00100.nsf/pages/zOSV2R3sc236878/$file/icea100_v2r3.pdf) - Full Manual
Note: IBM DFSORT's ICETOOL can often replace the need to write COBOL program(s), when needing to (optionally) sort and tranform data contained in one or more input files to produce an output file.  All user guides written by Frank L. Yaeger on this subject are excellent.  Access the entire collection, [here](https://www.ibm.com/support/pages/dfsortmvs-downloads).

Restoring a Fresh Copy of Lab Data (or Cobol) Files:
Download data file from Labs folder from here: https://github.com/openmainframeproject/cobol-programming-course/tree/master/Labs/data then go to the command line and execute: zowe zos-files upload file-to-data-set "data" "Z80XXX.DATA" -b

[More education from IBM](http://ibm.biz/zOSclass)

History:
[Timeline and brief description of IBM Operating Systems. By Dave Morton, marspyrs. Dated: February, 2018](https://webfiles.uci.edu/scosel/_$OSTL37.5.pdf)

Internet resources:
[Beginner’s Guide: COBOL Made Easy](https://medium.com/modern-mainframe/beginners-guide-cobol-made-easy-introduction-ecf2f611ac76)
[Awesome Mainframes - An Indexed Reference of Mainframe Links](https://github.com/FuzzyMainframes/Awesome-Mainframes/blob/master/README.md)

Employment Opportunity - Cobol Programmers Needed Now!
https://forms.business.nj.gov/tech/
