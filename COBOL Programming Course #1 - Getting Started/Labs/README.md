COBOL labs
4 folders

1. **cbl** – COBOL source code
2. **jcl** – JCL to compile, link, and execute COBOL source code
3. **jclproc** – JCL PROCEDUREs used for labs in JCL folder
4. **data** – COBOL program input records

Note:
**cbl** folder
- copy cobol source members into student id.CBL partitioned data set name

**jcl** folder 
- copy jcl members into student id.JCL partitioned data set name
- jcl members reference student id.CBL and student id.LOAD partitioned data set names

**jclproc** folder 
- members need to be copied into z/OS JES procedure library

**data** folder
- member **data** must be transferred binary to z/OS because the data contains EBCDIC and Packed Decimal
- member **datax** is a viewable ASCII data but the packed decimal fields were translated to ASCII and are unreadable

