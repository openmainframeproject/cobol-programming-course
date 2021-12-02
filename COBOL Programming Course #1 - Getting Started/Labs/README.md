# COBOL Programming Course #1 - Getting Started - Lab Setup

The labs for this course are set up on z/OS 2.4 with Enterprise COBOL v6.3. Modification may be necessary if your installation uses a different version.

Please note that the following instruction is provided as-is. Neither this project nor the Open Mainframe Project will be held responsible or liable for any loss, costs, liabilities, or damages resulting from your direct or indirect use of this Lab Setup.

## Contents

There are 4 folders within this directory:
- **cbl**, containing the actual COBOL source code
- **jcl**, containing the JCL used to compile, link, and execute COBOL source code
- **jclproc**, containing the supplied cataloged procedure used in the JCL
- **data**, containing the data used as input for the COBOL program

There are 2 kinds of data provided inside the **data** folder:
- **data** is a binary file that is still encoded in EBCDIC with packed decimal
- **xdata** is a viewable ASCII file, however, the packed decimal fields are unreadable

There are 3 supplied cataloged procedures inside the **jclproc** folder:
- **IGYWC** for COBOL code compilation
- **IGYWCL** for COBOL code compilation and link-edit
- **IGYWCLG** for COBOL code compilation, link-edit, and execution

## Setup 

For the following instructions, `&SYSUID.` refers to the course taker's ID.

To set up your environment for the Course,
- You will need to copy the members from **cbl** to your own PDS. The Course Document and JCL assume that the PDS is `&SYSUID..CBL`.
- You will need to copy the members from **jcl** to your own PDS. The Course Document assumes that the PDS is `&SYSUID..JCL`.
- You will need to transfer the binary **data** from the data folder to your own sequential data set. The Course Document and JCL assume that the PDS is `&SYSUID..DATA`.
- You will need to make sure that the procedures IGYWC, IGYWCL, and IGYWCLG are available in your procedure library. 

Note that the JCL references the following data sets,
- `&SYSUID..CBL` where the COBOL source codes are stored
- `&SYSUID..LOAD` where the COBOL load modules are stored
- `&SYSUID..DATA` where the input data are stored