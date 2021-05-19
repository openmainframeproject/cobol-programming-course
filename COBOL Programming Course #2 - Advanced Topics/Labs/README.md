COBOL labs
4 folders

1. **cbl** – COBOL source code
2. **jcl** – JCL to compile, link, and execute COBOL source code
3. **jclproc** – JCL PROCEDUREs used for labs in JCL folder

Note:
**cbl** folder
- copy cobol source members into student id.CBL partitioned data set name

**jcl** folder 
- copy jcl members into student id.JCL partitioned data set name
- jcl members reference student id.CBL and student id.LOAD partitioned data set names
- jcl setup for DB2 assume Course 1 data are available 

**jclproc** folder 
- members need to be copied into z/OS JES procedure library


