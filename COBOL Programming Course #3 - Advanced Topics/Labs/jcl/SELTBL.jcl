//SELTBL  JOB 1                                                         00010003
//SQLEXEC EXEC DB2JCL                                                   00020002
//SYSIN   DD *,SYMBOLS=CNVTSYS                                          00021003
--******* SQL FOLLOWS                                                   00030003
  SELECT * FROM &SYSUID.T;                                              00040003
