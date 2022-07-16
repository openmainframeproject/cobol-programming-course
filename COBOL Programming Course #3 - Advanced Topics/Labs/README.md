# COBOL Programming Course #2 - Advanced Topics - Lab Setup

The labs for this course are set up on z/OS 2.4 with Enterprise COBOL v6.3 and Db2 for z/OS 12. Modification may be necessary if your installation uses a different version.

Please note that the following instruction is provided as-is. Neither this project nor the Open Mainframe Project will be held responsible or liable for any loss, costs, liabilities, or damages resulting from your direct or indirect use of this Lab Setup.

## Contents

There are 3 folders within this directory:
- **cbl**, containing the actual COBOL source code
- **jcl**, containing the JCL used to compile, link, and execute COBOL source code
- **jclproc**, containing the supplied cataloged procedure used in the JCL

There are 3 supplied cataloged procedures inside the **jclproc** folder:
- **DB2CBL** for COBOL code compilation, link-edit, and Db2 binding
- **DB2JCL** for the execution of SQL statements via JCL
- **DSNUPROC** for invoking Db2 online utility

## Setup

For the following instructions, `&SYSUID.` refers to the course taker's ID.

To set up your own environment for the Course,
- You will need to make sure that the Lab Setup for Course 1 has been followed.
- You will need to copy the members from **cbl** to your own PDS. The Course Document and JCL assume that the PDS is `&SYSUID..CBL`.
- You will need to copy the members from **jcl** to your own PDS. The Course Document assumes that the PDS is `&SYSUID..JCL`.
- You will need to make sure that the procedures DB2CBL, DB2JCL, and DSNUPROC are available in your procedure library.

Note that the JCL references the following data sets,
- `&SYSUID..CBL` where the COBOL source codes are stored
- `&SYSUID..LOAD` where the COBOL load modules are stored
- `&SYSUID..DATA` where the input data are stored
- `&SYSUID..DBRMLIB` where the Db2 database request module output is stored, generated when the course taker submitted either the DB2SETUP or DBRMLIB JCL