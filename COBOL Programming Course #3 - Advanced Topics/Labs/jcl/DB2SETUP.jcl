//DB2SETUP JOB   1
//JOBSTEP  EXEC  PGM=IEBCOPY
//SYSPRINT DD  SYSOUT=A
//JCL      DD  DSNAME=&SYSUID..JCL,DISP=SHR
//DB2JCL   DD  DSNAME=ZOS.PUBLIC.DB2.JCL,DISP=SHR
//CBL      DD  DSNAME=&SYSUID..CBL,DISP=SHR
//DB2CBL   DD  DSNAME=ZOS.PUBLIC.DB2.CBL,DISP=SHR
//SYSUT3   DD  UNIT=SYSDA,SPACE=(TRK,(1))
//SYSUT4   DD  UNIT=SYSDA,SPACE=(TRK,(1))
//SYSIN    DD  *
COPYOPER   COPY    OUTDD=JCL,INDD=DB2JCL
           SELECT  MEMBER=((CBLDB21C,,R),                              X
                           (CBLDB21R,,R),                              X
                           (CBLDB22C,,R),                              X
                           (CBLDB22R,,R),                              X
                           (CBLDB23C,,R),                              X
                           (CBLDB23R,,R),                              X
                           (CRETBL,,R),                                X
                           (DBRMLIB,R),                                X
                           (LOADTBL,,R),                               X
                           (SELTBL,,R),                                X
                           ($README,,R))
COPYOPER   COPY    OUTDD=CBL,INDD=DB2CBL
           SELECT  MEMBER=((CBLDB21,,R),                               X
                           (CBLDB22,,R),                               X
                           (CBLDB23,,R),                               X
                           ($README,,R))
//DBRMLIB EXEC PGM=IEFBR14
//DBRMLIB DD DSN=&SYSUID..DBRMLIB,DISP=(,CATLG),
// UNIT=3390,VOL=SER=DB2004,SPACE=(CYL,(1,1)),
// DCB=(RECFM=FB,LRECL=80,BLKSIZE=4000,DSORG=PO),DSNTYPE=LIBRARY
/*
