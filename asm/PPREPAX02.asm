*          DATA SET PPREPAX02  AT LEVEL 018 AS OF 11/20/13                      
*PHASE PPAX02A                                                                  
*INCLUDE PUBEDIT                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'PPAX02 - PRINT CLEARANCE UPDATE'                                
         SPACE 1                                                                
PPAX02   CSECT                                                                  
         SPACE 1                                                                
         PRINT NOGEN                                                            
         NMOD1 0,PPAX02,R9                                                      
         L     RC,0(R1)                                                         
         USING PPWORKD,RC         GLOBAL WORKING STORAGE                        
         L     RA,PPFILEC                                                       
         USING PPFILED,RA                                                       
         MVI   RC2DSECT,C'Y'      USE SECOND DSECT                              
         L     R8,PPWORK2C                                                      
         USING PPWORK2D,R8                                                      
         SPACE 2                                                                
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    FRST                                                             
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
*=========================================================*                     
* RUNFRST PROCESSING                                      *                     
*=========================================================*                     
         SPACE 1                                                                
FRST     DS    0H                                                               
         MVI   FIRST,C'Y'                                                       
*                                                                               
         USING MASTD,R1                                                         
         ICM   R1,15,VMASTC                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   VREMOTEC,MCVREMOT                                                
         MVC   RCRUN,MCTSTRUN      SAVE TEST RUN INDICATORS                     
         DROP  R1                                                               
*                                                                               
         L     R1,=A(IO)                                                        
         ST    R1,AREC                                                          
         OPEN  (DATAFILE,(INPUT))  OPEN INPUT FILE                              
         BAS   RE,FINDPRNT         FIND SE NUMBER FOR THIS PRINT SYS            
*                                                                               
FRST10   L     R1,=A(DATAFILE)                                                  
         LA    R2,REC                                                           
         USING RECD,R2                                                          
         GET   (1),(2)                                                          
         CLC   RSENUM,THISSE       LOOP THROUGH DATA TIL FIND START             
         BNE   FRST10                                                           
*                                                                               
FRST15   CLC   RALPHA,LASTALPH     SAME AGENCY                                  
         BE    FRST20                                                           
         MVC   LASTALPH,RALPHA                                                  
         BAS   RE,NEWAGY                                                        
*                                                                               
FRST20   BAS   RE,PROCESS          PROCESS THE RECORD                           
         L     R1,=A(DATAFILE)                                                  
         LA    R2,REC                                                           
         GET   (1),(2)                                                          
         CLC   RSENUM,THISSE       IF NEW SE NUMBER                             
         BE    FRST15                                                           
*                                                                               
DATAEND  CLOSE (DATAFILE)          DONE                                         
         LTR   RF,RF                                                            
         BZ    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*        SET REMOTE INFO FOR NEW AGENCY                                         
*                                                                               
NEWAGY   NTR1                                                                   
         CLI   FIRST,C'Y'          FIRST AGENCY                                 
         BE    NEWAGY10                                                         
         GOTO1 REPORT                                                           
         MVC   P1(27),=C'CLEARANCE RECORDS PROCESSED'                           
         EDIT  COUNTER,(9,P1+30),COMMAS=YES,ALIGN=LEFT                          
         GOTO1 REPORT                                                           
         MVC   P1(27),=CL27'RECORDS IN ERROR'                                   
         EDIT  ERRCNT,(9,P1+30),COMMAS=YES,ALIGN=LEFT                           
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
         XC    COUNTER,COUNTER                                                  
         XC    ERRCNT,ERRCNT                                                    
         CLI   OPENPQ,C'Y'                                                      
         BNE   NEWAGY10                                                         
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         MVC   PAGE,=X'0001'                                                    
         MVI   OPENPQ,C'N'                                                      
*                                                                               
NEWAGY10 DS    0H                                                               
         CLI   RCRUN,RUNTST        IS THIS A TEST RUN?                          
         BE    NEWAGY20            - THEN NO REMOTE.                            
*******  BAS   RE,GETPQNUM         GET NEXT AGENCY ID NUMBER                    
         MVI   FIRST,C'N'                                                       
*                                                                               
         CLC   =C'YN',RALPHA       SPECIAL CODE TO SEND YN TO PQ                
         BE    NEWAGY12                                                         
         CLC   =C'FR',RALPHA       SPECIAL CODE TO SEND FR TO PQ                
         BE    NEWAGY12                                                         
         CLC   =C'H7',RALPHA       SPECIAL CODE TO SEND H7 TO PQ                
         BE    NEWAGY12                                                         
         CLC   =C'JW',RALPHA       SPECIAL CODE TO SEND JW TO PQ                
         BNE   NEWAGY20                                                         
*                                                                               
         USING REMOTED,R1                                                       
NEWAGY12 L     R1,VREMOTEC                                                      
         XC    REMOTKEY,REMOTKEY                                                
         MVC   REMOTJID,=C'PAX'                                                 
         MVC   REMOTKEY(11),=C'PRINT CLRST'                                     
         MVI   REMOTCLS,C'K'                                                    
         MVC   REMOTDST,=X'099F'   USER ID NUMBER FOR YNRO (YN)                 
         CLC   =C'YN',RALPHA       SPECIAL CODE TO SEND YN TO PQ                
         BE    NEWAGY18                                                         
         MVC   REMOTDST,=X'1974'   USER ID NUMBER FOR FDMJW (FR)                
         CLC   =C'FR',RALPHA       SPECIAL CODE TO SEND FR TO PQ                
         BE    NEWAGY18                                                         
         MVC   REMOTDST,=X'2192'   USER ID NUMBER FOR MSNYA (H7)                
         CLC   =C'H7',RALPHA       SPECIAL CODE TO SEND H7 TO PQ                
         BE    NEWAGY18                                                         
         MVC   REMOTDST,=X'0865'   USER ID NUMBER FOR JWNYSH (JW)               
NEWAGY18 MVI   OPENPQ,C'Y'                                                      
NEWAGY20 B     XIT                                                              
         DROP  R1                                                               
*                                                                               
*        GET ID NUMBER (FOR PRINT QUEUE) FROM POWER CODE IN RALPHA              
*                                                                               
GETPQNUM NTR1                                                                   
         USING CT5REC,R1                                                        
         LA    R1,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   CT5KTYP,CT5KTYPQ    C'5'                                         
         MVC   CT5KALPH,RALPHA                                                  
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'CTFILE',KEY,AREC,0                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AREC                                                          
         CLC   0(L'CT5KEY,R1),KEY                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,CT5DATA                                                       
GPQ10    CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'02'                                                      
         BE    GPQ20                                                            
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GPQ10                                                            
GPQ20    MVC   PQIDNUM,2(R2)       SAVE USER NUMBER FOR PQ                      
         B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
*        FIND OUT WHICH PRNT WE'RE RUNNING & THE CORRESPONDING SE #             
*                                                                               
FINDPRNT NTR1                                                                   
         USING CTWREC,R1                                                        
         LA    R1,KEY                                                           
         XC    KEY,KEY             GET SYS LIST RECORD FROM CT FILE             
         MVI   KEY,C'W'                                                         
         MVI   KEY+17,C'S'                                                      
         MVI   CTWKSYSN,CTWKPRNT     ONLY NEED PRINT LIST                       
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'CTFILE',KEY,AREC,0                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AREC                                                          
         USING CTWREC,R1                                                        
         CLC   0(L'CTWKEY,R1),KEY                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,CTWDATA                                                       
         USING CTLSTD,R2                                                        
*                                                                               
FS10     GOTO1 CARDS,DMCB,CARD,=C'RE00'                                         
         CLC   =C'/*',CARD                                                      
         BE    FSX                                                              
         CLC   =C'PRNT',CARD       FIND CARD WITH PRNT                          
         BNE   FS10                                                             
*                                                                               
FS15     CLI   0(R2),0             END OF RECORD                                
         BNE   *+6                                                              
         DC    H'0'                ENTRY MUST BE IN TABLE                       
         CLC   3(6,R2),CARD                                                     
         BE    FS20                                                             
         ZIC   R4,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,R4                                                            
         B     FS15                                                             
*                                                                               
FS20     MVC   THISSE,11(R2)       THIS IS THE SE NUMBER WE NEED                
         L     RE,UTL                                                           
         MVC   4(1,RE),THISSE      SET SYSTEM NUMBER                            
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'PRINT',FLIST,AREC                     
*                                                                               
FSX      B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*        PROCESS THE RECORD                                                     
*                                                                               
PROCESS  NTR1                                                                   
*                                                                               
         LA    R2,REC              ESTABLISH RECORD FROM ACC                    
         USING RECD,R2                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,RCKCDT),(3,TEMPDATE) DATE ON REC 3 BYTES          
*                                                                               
         XC    KEY,KEY             ESTABLISH CLRST RECORD KEY                   
         LA    R6,KEY                                                           
         USING PPCLRST,R6                                                       
*                                                                               
         LA    R1,PPCLELEM-PPCLRST SET DISPLACEMENT TO 1ST ELEMENT              
         STH   R1,DATADISP         FOR GETEL MACRO                              
*                                                                               
         MVC   WORK(3),TEMPDATE    SET DATE & SEQ FOR CONTINUE RECS             
         MVC   WORK+3(1),RSEQNUM                                                
         MVC   WORK+4(2),RSEQNUM2                                               
*                                                                               
*        BUILD BASIC CLRST KEY                                                  
*                                                                               
         MVC   PPCLAGY,RALPHA      AGENCY                                       
         MVC   PPCLMED,RMED        MEDIA                                        
         MVI   PPCLTYPE,X'25'                                                   
         MVC   PPCLCLT,RCLT        CLIENT                                       
         MVC   PPCLPUB,RPUB        PUB                                          
*                                                                               
         GOTO1 HIGH                READ FIRST CLRST KEY                         
*                                                                               
         MVI   ERRSW,C'H'                                                       
*                                                                               
         CLC   KEY(PPCLDATE-PPCLKEY),KEYSAVE ERR IF NO CLRST FOR CL/PB          
         BNE   PROCERR                                                          
*                                                                               
PROCRCLP DS    0H                                                               
*                                                                               
         MVC   SVDSKADD,KEY+27     SAVE DISK ADDR OF PREVIOUS RECORD            
*                                                                               
         GOTO1 SEQ                 READ NEXT CLRST KEY                          
*                                                                               
         CLC   KEY(PPCLDATE-PPCLKEY),KEYSAVE  DONE IF NEW CLT/PUB               
         BNE   PROCRCDN            USE PREV REC FOR CLT/PUB                     
*                                                                               
         CLC   PPCLDATE(6),WORK    IF LOWEST CLEARANCE DATE & SEQ NUM           
         BH    PROCRCDN            IS LOWER - GET NEXT RECORD                   
*                                                                               
PROCRCCN DS    0H                                                               
         B     PROCRCLP                                                         
*                                                                               
PROCRCDN DS    0H                                                               
*                                                                               
*        WE'VE FOUND KEY OF ONE RECORD PAST THE ONE WE WANT                     
*              READ PREVIOUS RECORD INTO CORE - SVDSKADD                        
*                                                                               
         XC    KEY,KEY             SET D/A OF RECORD THAT CONTAINS              
         MVC   KEY+27(4),SVDSKADD  THE NEEDED CLEARANCE                         
*                                                                               
         GOTO1 GETPRT              GET THE RECORD                               
*                                                                               
*        FIND X'01' ELEMENT FOR PAYMENT                                         
*                                                                               
         L     R6,AREC                                                          
         USING PPCLEL01,R6                                                      
*                                                                               
         MVI   ELCODE,X'01'        GET CLEARANCE DETAILS                        
         MVI   ERRSW,C'E'          INIT ERROR CODE                              
         MVI   ELEMFND,C'N'        INIT ACTIVITY INDICATOR                      
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   PROCERR             RECORD MUST HAVE ONE 01 ELEMENT              
*                                                                               
PROC01LP DS    0H                  MATCH ON CLR DATE AND SQN                    
*                                                                               
         CLC   PPCLCLRD,TEMPDATE   HAVE WE FOUND CORRECT ELEMENT                
         BNE   PROC01CN            MATCH ON CLEARANCE DATE                      
*                                                                               
         CLC   PPCLCLSQ,RSEQNUM    & SEQUENCE NUMBER                            
         BNE   PROC01CN                                                         
*                                                                               
         CLI   PPCLCLSQ,255        FOUND IF NOT 255                             
         BNE   PROC01FD                                                         
*                                                                               
         CLI   PPCLEL01+1,PPCLELL2 MAKE SURE ELM LONG ENOUGH                    
         BL    PROC01FD                                                         
*                                                                               
         CLC   PPCLCLS2,RSEQNUM2   & SECONDARY SEQUENCE NUMBER                  
         BNE   PROC01CN                                                         
*                                                                               
         B     PROC01FD                                                         
*                                                                               
PROC01CN DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL           READ NEXT X'01' ELEMENT                      
         BE    PROC01LP               FOUND                                     
*                                                                               
         B     PROC01DN            ELEMENT NOT FOUND                            
*                                                                               
PROC01FD DS    0H                                                               
*                                                                               
         TM    PPCLSTAT,X'02'      IF X'03'/X'05' ELEMENTS FOLLOW               
         BNO   *+8                                                              
         BRAS  RE,NEWCLRST            HANDLE DIFFERENTLY                        
*                                                                               
         CLC   RCKNUM,SPACES       SKIP IF NO INCOMING CHECK NUMBER             
         BNH   PROC0120                                                         
*                                                                               
         CLC   PPCLCHK,SPACES      OR NO CHECK ON FILE                          
         BNH   PROC0120                                                         
*                                                                               
         CLC   PPCLCHK(4),=C'VOID' OR IF WAS A PREVIOUSLY VOIDED CHECK          
         BE    PROC0120                                                         
*                                                                               
*        HAVE CHECK NUMBER FROM ACC AND CHECK NUMBER IN CLRST REC               
*                                                                               
         CLC   RCKNUM,PPCLCHK     SKIP IF DIFFERENT CHECK NUMBERS               
         BNE   PROC01DN                                                         
*                                                                               
         TM    RSTATUS,TRNSBREC   IF CHECK IS UN-RECONCILED NOW                 
         BO    PROC0110                                                         
*                                                                               
         NI    PPCLSTAT,X'FF'-X'80'   SET RECORD UN-RECONCILED                  
         B     *+8                                                              
PROC0110 OI    PPCLSTAT,X'80'      ELSE SET RECORD RECONCILED                   
*                                                                               
         TM    RSTATUS,X'01'       IS THIS A BANK CLEARED DATE                  
         BNO   *+10                                                             
         MVC   PPCLBKDT,RBNKCLRD   THEN FILL IN BANK CLEARED DATE               
*                                                                               
         MVI   ELEMFND,C'Y'        WE UPDATED RECORD                            
*                                                                               
         B     PROC01DN                                                         
*                                                                               
PROC0120 DS    0H                                                               
*                                                                               
         CLC   RCKNUM,SPACES       IF NO CHECK NUMBER                           
         BH    *+14                                                             
         CLC   PPCLCHK,SPACES      AND NO CLRST CHECK                           
         BNH   PROC01DN               SKIP                                      
*                                                                               
         MVC   PPCLCHK,RCKNUM      SET CHECK NUMBER                             
         MVC   PPCLCHDT,RCKDATE         & DATE                                  
*                                                                               
         CLC   RCKNUM,SPACES       IF THIS IS A VOID (NO CHECK NUMBER)          
         BH    *+10                                                             
         MVC   PPCLCHK(4),=C'VOID' SET IT TO VOID                               
*                                                                               
         TM    RSTATUS,TRNSBREC    IF RECORD IS UN-RECONCILED NOW               
         BNO   *+8                                                              
         NI    PPCLSTAT,X'7F'      SET RECORD UN-RECONCILED                     
*                                                                               
         MVI   ELEMFND,C'Y'        CLRST REC WAS UPDATED                        
*                                                                               
PROC01DN DS    0H                                                               
*                                                                               
         CLI   ELEMFND,C'Y'                                                     
         BNE   PROCERR             NEVER FOUND ELM FOR CLEARANCE                
*                                                                               
         AF    COUNTER,=F'1'                                                    
*                                                                               
         BAS   RE,PRREP            PRINT LINE ON REPORT                         
*                                                                               
******   GOTO1 MYTRACE,DMCB,AREC,0,=C'CLEARANCE RECORD',17                      
*                                                                               
         CLI   RCWRITE,C'Y'        TEST WRITE = NO                              
         BNE   PROCPUTX                                                         
*                                                                               
         GOTO1 PUTPRT                                                           
*                                                                               
PROCPUTX DS    0H                                                               
*                                                                               
         B     PROCX                                                            
*                                                                               
PROCERR  BAS   RE,CLRERR           CLEARANCE ERROR                              
*                                                                               
PROCX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        ROUTINE TO HANDLE NEW CLEARANCE STATUS ELEMENTS                        
*                                                                               
NEWCLRST NTR1                                                                   
*                                                                               
*        MATCH ON INVOICE NUMBER IN FOLLOWING X'03' ELEMENT                     
*                                                                               
NWCL03LP DS    0H                                                               
*                                                                               
         USING PPCLEL03,R6         ESTABLISH INVOICE ELEMENT                    
*                                                                               
         LLC   RF,PPCLEL03+1       ELEMENT LENGTH                               
         LA    R6,PPCLEL03(RF)     NEXT ELEMENT                                 
*                                                                               
         CLI   PPCLEL03,0          DONE IF END OF RECORD                        
         BE    NWCL03DN                                                         
*                                                                               
         CLI   PPCLEL03,X'01'      DONE AT NEXT 01 ELEMENT                      
         BE    NWCL03DN                                                         
*                                                                               
         CLI   PPCLEL03,X'03'      CONTINUE IF NOT AN 03 ELEMENT                
         BNE   NWCL03CN                                                         
*                                                                               
         CLC   PPCLINV,RINVOICE    MATCH ON INVOICE NUMBER                      
         BE    NWCL03FD                                                         
*                                                                               
NWCL03CN DS    0H                                                               
*                                                                               
         B     NWCL03LP            GET NEXT ELEMENT                             
*                                                                               
NWCL03DN DS    0H                                                               
*                                                                               
         B     NEWCLSTX                                                         
*                                                                               
NWCL03FD DS    0H                  CORRECT INVOICE ELEMENT FOUND                
*                                                                               
         LLC   RF,PPCLEL03+1       ELEMENT LENGTH                               
         LA    R6,PPCLEL03(RF)     NEXT ELEMENT                                 
*                                                                               
         USING PPCLEL05,R6         ESTABLISH X'05' ELEMENT                      
*                                                                               
         CLI   PPCLEL05,X'05'      DONE IF NOT AND X'05' ELEMENT                
         BNE   NEWCLSTX                                                         
*                                                                               
         CLC   RCKNUM,SPACES       SKIP IF NO INCOMING CHECK NUMBER             
         BNH   NWCL0520                                                         
*                                                                               
         CLC   PCL5CHK,SPACES      OR NO CHECK ON FILE                          
         BNH   NWCL0520                                                         
*                                                                               
         CLC   PCL5CHK(4),=C'VOID' OR IF WAS A PREVIOUSLY VOIDED CHECK          
         BE    NWCL0520                                                         
*                                                                               
*        HAVE CHECK NUMBER FROM ACC AND CHECK NUMBER IN CLRST REC               
*                                                                               
         CLC   RCKNUM,PCL5CHK     SKIP IF DIFFERENT CHECK NUMBERS               
         BNE   NEWCLSTX                                                         
*                                                                               
         TM    RSTATUS,TRNSBREC   IF CHECK IS UN-RECONCILED NOW                 
         BO    NWCL0510                                                         
*                                                                               
         NI    PCL5STAT,X'FF'-X'80'   SET RECORD UN-RECONCILED                  
         B     *+8                                                              
NWCL0510 OI    PCL5STAT,X'80'      ELSE SET RECORD RECONCILED                   
*                                                                               
         TM    RSTATUS,X'01'       IS THIS A BANK CLEARED DATE                  
         BNO   *+10                                                             
         MVC   PCL5BKDT,RBNKCLRD   THEN FILL IN BANK CLEARED DATE               
*                                                                               
         MVI   ELEMFND,C'Y'        WE UPDATED RECORD                            
*                                                                               
         B     NEWCLSTX                                                         
*                                                                               
NWCL0520 DS    0H                                                               
*                                                                               
         CLC   RCKNUM,SPACES       IF NO CHECK NUMBER                           
         BH    *+14                                                             
         CLC   PCL5CHK,SPACES      AND NO CLRST CHECK                           
         BNH   NEWCLSTX               SKIP                                      
*                                                                               
         MVC   PCL5CHK,RCKNUM      SET CHECK NUMBER                             
         MVC   PCL5CHDT,RCKDATE         & DATE                                  
*                                                                               
         CLC   RCKNUM,SPACES       IF THIS IS A VOID (NO CHECK NUMBER)          
         BH    *+10                                                             
         MVC   PCL5CHK(4),=C'VOID' SET IT TO VOID                               
*                                                                               
         TM    RSTATUS,TRNSBREC    IF RECORD IS UN-RECONCILED NOW               
         BNO   *+8                                                              
         NI    PCL5STAT,X'FF'-X'80'  SET RECORD UN-RECONCILED                   
*                                                                               
         MVI   ELEMFND,C'Y'        CLRST REC WAS UPDATED                        
*                                                                               
NEWCLSTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*        PRINT THE REPORT                                                       
*                                                                               
PRREP    NTR1                                                                   
         LA    R3,P1                                                            
         USING LINED,R3                                                         
         MVC   LAGY,RALPHA         ALPHA ID                                     
         MVC   LMED,RMED           MEDIA                                        
         MVC   LCLT,RCLT           CLIENT                                       
         GOTO1 =V(PUBEDIT),DMCB,(L'LPUB,RPUB),(C'S',LPUB)                       
         GOTO1 DATCON,DMCB,(2,RCKDATE),(5,LDATE)                                
         MVC   LCKNUM,RCKNUM       CHECK NUMBER                                 
         EDIT  RSEQNUM,(3,LSEQNUM) SEQUENCE NUMBER                              
         TM    RSTATUS,TRNSBREC                                                 
         BNO   *+8                                                              
         MVI   LSTATUS,C'R'                                                     
***                                                                             
* BANK CLEARED DATE ADDED 02/14/02                                              
***                                                                             
         TM    RSTATUS,X'01'                                                    
         BNO   NOCLRDT                                                          
         OC    RBNKCLRD,RBNKCLRD                                                
         BZ    NOCLRDT                                                          
         GOTO1 DATCON,DMCB,(2,RBNKCLRD),(5,LBNKDT)                              
NOCLRDT  GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS RUN LAST                                                       
*                                                                               
LAST     DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P1(27),=C'CLEARANCE RECORDS PROCESSED'                           
         EDIT  COUNTER,(9,P1+30),COMMAS=YES,ALIGN=LEFT                          
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(27),=CL27'RECORDS IN ERROR'                                   
         EDIT  ERRCNT,(9,P1+30),COMMAS=YES,ALIGN=LEFT                           
         GOTO1 REPORT                                                           
         CLI   OPENPQ,C'Y'                                                      
         BNE   XIT                                                              
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         B     XIT                                                              
         EJECT                                                                  
*=================================================================*             
*        PARAMETER 1 - A(DATA)                                                  
*        PARAMETER 2 - L(DATA) OR ZERO FOR RECORD                               
*        PARAMETER 3 - A(LABEL) OR ZERO FOR NO LABEL                            
*        PARAMETER 4 - L(LABEL) IF PARM 3 IS NOT ZERO                           
*=================================================================*             
MYTRACE  NTR1                                                                   
         LM    R2,R5,0(R1)         R2 = A(DATA)                                 
*                                  R3 = L(DATA)                                 
*                                  R4 = A(LABEL)                                
*                                  R5 = L(LABEL)                                
*                                                                               
         LTR   R4,R4               IF CALLER SUPPLIED A LABEL                   
         BZ    TR10                                                             
         MVI   P1,C'-'             THEN FILL PRINT LINE WITH '-'S               
         MVC   P1+1(131),P1                                                     
*                                                                               
         LR    RE,R5               RF = A(PLACE TO CENTER LABEL)                
         SRL   RE,1                                                             
         LA    RF,66                                                            
         SR    RF,RE                                                            
         LA    RF,P1(RF)                                                        
*                                                                               
         BCTR  R5,0                MOVE LABEL TO CENTER OF PRINT LINE           
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)                                                    
*                                                                               
         GOTO1 REPORT              PRINT LABEL LINE                             
*                                                                               
TR10     LTR   R3,R3               IF DATA IS A RECORD                          
         BNZ   TR50                                                             
         OC    DATADISP,DATADISP   IF THERE IS A KEY                            
         BZ    TR15                                                             
*                                  PRINT OUT ITS KEY                            
         LH    R3,DATADISP                                                      
         GOTO1 =V(PRNTBL),DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                     
*                                                                               
TR15     LR    R6,R2               A(RECORD)                                    
         AH    R6,DATADISP         + DISPLACEMENT TO FIRST ELEMENT              
         MVI   ELCODE,0                                                         
         BAS   RE,FIRSTEL                                                       
         BNE   TRX                                                              
*                                                                               
TR20     ZIC   R4,1(R6)            PRINT ELEMENT                                
         GOTO1 =V(PRNTBL),DMCB,0,(R6),C'DUMP',(R4),=X'01C4'                     
*                                                                               
         BAS   RE,NEXTEL           REPEAT UNTIL NO MORE ELEMENTS                
         BE    TR20                                                             
         B     TRX                                                              
*                                  ELSE PRINT ENTIRE DATA BLOCK                 
TR50     GOTO1 =V(PRNTBL),DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                     
*                                                                               
TRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
CLRERR   NTR1                                                                   
         BAS   RE,PRREP                                                         
         MVC   P1(22),=CL22'*** CLEARANCE ERROR ***'                            
         MVC   P1+23(1),ERRSW                                                   
         B     ERRPRNT                                                          
*                                                                               
ERRPRNT  AF    ERRCNT,=F'1'                                                     
         GOTO1 REPORT                                                           
         B     XIT                                                              
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
DATAFILE DCB   DDNAME=DATAFILE,DSORG=PS,MACRF=GM,                      X        
               RECFM=FB,LRECL=49,BLKSIZE=490,EODAD=DATAEND                      
*                                                                               
FLIST    DC    CL8'UPRTFILE'                                                    
         DC    CL8'UPRTDIR '                                                    
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
*                                                                               
         EJECT                                                                  
COUNTER  DS    F                   RECORD COUNTER                               
SVDSKADD DS    F                   SAVED DISK ADDRESS                           
ERRCNT   DS    F                   ERROR RECORD COUNTER                         
VREMOTEC DS    F                                                                
*                                                                               
DATADISP DS    H                   DISPLACEMENT TO FIRST ELEMENT                
*                                                                               
TEMPDATE DS    XL3                 TEMPORARY AREA FOR PACKED DATE               
RECCHG   DS    CL1                                                              
ERRSW    DS    CL1                                                              
*                                                                               
PQIDNUM  DS    XL2                 USER ID NUM FOR PQ DEST                      
FIRST    DS    CL1                                                              
OPENPQ   DS    XL1                                                              
RCRUN    DS    XL1                 SYSTEM RUN INDICATOR (MCTSTRUN)              
RUNTST   EQU   X'FF'                                                            
LASTALPH DS    CL2                                                              
*                                                                               
ELEMFND  DS    XL1                 SWITCH - RECONCILED ELEMENT MARKED           
*                                                                               
THISSE   DS    XL1                      SE NUMBER                               
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
         DS    0D                                                               
         DC    C'**REC***'                                                      
REC      DS    256C                                                             
*                                                                               
         DS    0D                                                               
         DC    C'**I/O***'                                                      
IO       DS    3000C                                                            
         EJECT                                                                  
*                                                                               
LINED    DSECT                                                                  
         DS    CL4                                                              
LAGY     DS    CL2                 ALPHA AGENCY                                 
         DS    CL10                                                             
LMED     DS    CL1                 MEDIA                                        
         DS    CL6                                                              
LCLT     DS    CL3                 CLIENT                                       
         DS    CL6                                                              
LPUB     DS    CL11                PUB                                          
         DS    CL3                                                              
LDATE    DS    CL8                 DATE                                         
         DS    CL5                                                              
LCKNUM   DS    CL6                 CHECK NUMBER                                 
         DS    CL9                                                              
LSEQNUM  DS    CL3                 SEQUENCE NUMBER                              
         DS    CL2                                                              
LSTATUS  DS    CL1                 STATUS                                       
         DS    CL2                                                              
LBNKDT   DS    CL8                 BANK CLEARED DATE                            
         DS    CL2                                                              
LINVOICE DS    CL12                INVOICE NUMBER                               
         SPACE 3                                                                
*                                                                               
RECD     DSECT                                                                  
RSENUM   DS    XL1                 SE NUMBER                                    
RALPHA   DS    CL2                 ALPHA ID                                     
RMED     DS    CL1                 MEDIA CODE                                   
RCLT     DS    CL3                 CLIENT                                       
RPUB     DS    XL6                 PUB                                          
RCKCDT   DS    XL2                 CHECK CLEARANCE DATE                         
RSEQNUM  DS    XL1                 SEQUENCE NUM                                 
RCKNUM   DS    CL6                 CHECK NUMBER                                 
RCKDATE  DS    XL2                 CHECK DATE                                   
RSTATUS  DS    XL1                 STATUS                                       
TRNSBREC EQU   X'02'               CHECK IS RECONCILED                          
*              X'01'               CHECK IS CLEARED                             
RBNKCLRD DS    XL2                 BANK CLEARED DATE                            
RINVOICE DS    CL12                INVOICE NUMBER                               
         DS    CL3                 SPARE                                        
         DS    CL3                 SPARE                                        
         DS    XL2                 SPARE                                        
RSEQNUM2 DS    XL2                 SECONDARY SEQUENCE NUMBER                    
RRECLNQ  EQU   *-RECD              LENGTH OF RECORD                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE PPCLRST                                                        
         EJECT                                                                  
*PPMODEQU                                                                       
*PPREPWORK                                                                      
*PPREPWORK2                                                                     
*PPNEWFILE                                                                      
*CTGENFILE                                                                      
*DDMASTD                                                                        
*DDREMOTED                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018PPREPAX02 11/20/13'                                      
         END                                                                    
