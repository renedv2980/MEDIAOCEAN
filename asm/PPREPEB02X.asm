*          DATA SET PPREPEB02X AT LEVEL 062 AS OF 05/01/02                      
*PHASE PPEB02A,+0,NOAUTO                                                        
*INCLUDE BINSRCH2                                                               
*INCLUDE DYNALLOC                                                               
         TITLE 'PPEB02 - EDI FOR PRINT BILLING'                                 
PPEB02   CSECT                                                                  
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
*   BPLA  1/27/00  FIX FILE READ LOGIC TO ONLY DO CERTAIN THINGS                
*                  WHEN PROCESSIN GTHE FIRST RECORD FO AN INDEX                 
*                 (LARGE NUMBER OF RECORDS CAUSED A PROBLEM)                    
*                                                                               
*   KWAN 10/20/99  CHECK CLIENT'S BILL RECORDS WITH RECORDS BUILT               
*                  WITH BINSRCH, IF RECORD IS FOUND IN BINSRCH TABLE,           
*                  UPDATE BILL RECORD'S X'08' ELEM.  THE FIELD TO BE            
*                  CHANGED IS PEDIDTE AND IT WILL CONTAIN TODAYP.               
*                                                                               
*   BPLA 10/07/99  CHANGE THE WAY THE RERUN OPTION WORKS                        
*                  SAVE THE TRANSMITTAL DATE IN WKRFIL (W_DESC+11)              
*                  UPDATE THE TRANSMITTAL DATA WHEN RERUNNING                   
*                                                                               
**********************************************************************          
*        START AND END INVOIVE NUMBERS COL 53(4), AND COL 56 (4)                
*        QINVNO1   OPTIONAL STARTING INVOICE NUMBER COL 53 (4)                  
*        QINVNO2   OPTIONAL ENDING INVOICE NUMBER COL 57 (4)                    
*                                                                               
*         QOPT1    T=TRACE WORKER AND OUTPUT RECORDS                            
*         QOPT2    Y=TEST MODE                                                  
*         QOPT3    R=REDO                                                       
**********************************************************************          
*                                                                               
*       NOTE- CODE IS PRESENT TO BUILD A TABLE OF REQUESTS                      
*             AND FILTER THE WORKER FILE READS USING THE MULTI-REQ              
*             TABLE. THIS WOULD BE MORE EFFICIENT AS FAR AS                     
*             THE WORKER FILE IS CONCERNED, BUT CREATES                         
*             OTHER PROBLEMS AND HAS BEEN ABANDOND . THE CODE IS STILL          
*             USED BUT ONLY HANDLES ONE REQUEST AT A TIME. COULD BE             
*             REVIVED.                                                          
*                                                                               
**********************************************************************          
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,PPEB02,CLEAR=YES,RR=R2                                         
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'                                                    
         L     R9,PPWORK2C                                                      
         USING PPWORK2D,R9                                                      
         LA    RC,SPACEND                                                       
         USING WORKD,RC                                                         
*                                                                               
         LA    R6,PPFILEC                                                       
         USING PPFILED,R6                                                       
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
*        RUN FIRST                                                              
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   LIVERUN,C'N'   WILL BE SET TO "Y" IF LIVE REQUEST                
*                             IS PROCESSED                                      
         L     RF,=A(RQTAB)                                                     
         A     RF,RELO                                                          
         ST    RF,ARQTAB                                                        
         L     RF,=A(RQTABX)                                                    
         A     RF,RELO                                                          
         ST    RF,ARQTABX                                                       
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         MVC   SAVUIDN,MCUSERID                                                 
         DROP  RF                                                               
*                                                                               
* SET TODAY                                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,TODAY)                                 
         GOTO1 DATCON,DMCB,TODAY,(3,TODAYB)                                     
         GOTO1 DATCON,DMCB,TODAY,(2,TODAYP)                                     
*                                                                               
* CLEAR BINSRCH TABLE ENTRIES                                                   
*                                                                               
         L     RE,=A(BRTAB)        GET ADDRESS OF TABLE                         
         A     RE,RELO                                                          
         L     RF,=F'18001'        GET LENGTH OF TABLE                          
         XCEF                                                                   
*                                                                               
* CLEAR CLIENT/MEDIA CODE TABLE                                                 
*                                                                               
         LA    RE,CLTMTAB          GET ADDRESS OF TABLE                         
         LA    RF,401              GET LENGTH OF TABLE                          
         XCEF                                                                   
*                                                                               
* PREPARE PARAMETERS FOR BINSRCH                                                
*                                                                               
         XC    WKPARMS,WKPARMS                                                  
         L     RF,=A(BRTAB)                                                     
         A     RF,RELO                                                          
         ST    RF,WKPARMS+04       ADDRESS OF TABLE                             
         LA    RF,9                                                             
         ST    RF,WKPARMS+12       LENGTH OF KEY                                
         ST    RF,WKPARMS+16       DIPLACEMENT OF KEY INTO RECORD               
         LA    RF,2000                                                          
         ST    RF,WKPARMS+20       MAXIUM NUMBER OF RECORDS IN TABLE            
*                                                                               
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*        REQUEST FIRST                                                          
***********************************************************************         
*                                                                               
REQF     DS    0H                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         MVI   NETPAKSW,C'N'                                                    
         LA    R3,PPDYNDSN                                                      
         DROP  RF                                                               
*                                                                               
         MVC   SVQAGY,QAGENCY      SAVE QAGENCY FOR RUNLAST                     
*                                                                               
         CLI   QOPT2,C'Y'          IF TEST, NO OUTPUT                           
         BE    REQF1H                                                           
         MVI   LIVERUN,C'Y'                                                     
         CLI   OPENSW,C'Y'         TEST FILE ALREADY OPEN                       
         BE    REQF1H                                                           
*                                                                               
         MVC   13(2,R3),QAGENCY                                                 
         GOTO1 =V(DYNALLOC),DMCB,(0,=C'EDIOUT  '),(0,0(R3))                     
         LA    R5,EDIOUT                                                        
         OPEN  ((R5),OUTPUT)                                                    
         MVI   OPENSW,C'Y'                                                      
         B     REQF1H                                                           
*                                                                               
PPDYNDSN DC    CL20'PRTTAPE.PP0EBAG'                                            
*                                                                               
REQF1H   DS    0H                                                               
         MVI   FCRDBUY,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
*                                  ADD TO REQUEST TABLE                         
         L     R4,ARQTAB           NOTE- ONE REQUEST AT A TIME FOR NOW          
         USING RQTABD,R4                                                        
         C     R4,ARQTABX                                                       
         BL    *+6                                                              
         DC    H'0'                REQUEST TABLE FULL                           
*                                                                               
         XC    RQTABD(RQTABL),RQTABD                                            
         MVI   RQTSYS,C'P'         PRINTPAK                                     
*                                                                               
REQF03   DS    0H                                                               
         MVC   RQTMED,QMEDIA                                                    
         MVC   RQTCLT,QCLIENT                                                   
*                                  NOTE- PRD/EST NOT USED NOW                   
         MVC   RQTPRD,QPRODUCT                                                  
         CLI   QEST,C'0'                                                        
         BL    REQF04                                                           
         PACK  DUB,QEST(3)                                                      
         CVB   R0,DUB                                                           
         STC   R0,RQTEST                                                        
*                                                                               
REQF04   DS    0H                                                               
         CLI   QSTART,C' '                                                      
         BNH   REQF05                                                           
         GOTO1 DATCON,DMCB,QSTART,(2,RQTSTA)                                    
*                                                                               
REQF05   DS    0H                                                               
         MVC   RQTEND,=X'FFFF'                                                  
         CLI   QEND,C' '                                                        
         BNH   REQF06                                                           
         GOTO1 DATCON,DMCB,QEND,(2,RQTEND)                                      
*                                                                               
REQF06   DS    0H                                                               
         MVC   RQTINV1,QINVNO1                                                  
         MVC   RQTINV2,QINVNO2                                                  
         CLI   QINVNO1,C' '                                                     
         BH    *+10                                                             
         XC    RQTINV1(4),RQTINV1                                               
         CLI   QINVNO2,C' '                                                     
         BH    *+10                                                             
         MVC   RQTINV2(4),=X'FFFFFFFF'                                          
         MVC   RQTOPTS,QOPT1                                                    
         MVC   RQTRQS,QUESTOR                                                   
         MVI   RQTABD+RQTABL,X'FF'  SET END OF TABLE                            
*                                                                               
         BAS   RE,RWPROC           READ AND PROCESS WORKER FILES                
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
***********************************************************************         
*        RUN LAST                                                               
*                                                                               
*        NOTE- IF CHANGE TO DO MULTI-REQUEST TABLE, CALL RWPROC                 
*              FROM HERE                                                        
*                                                                               
***********************************************************************         
*                                                                               
RUNL     DS    0H                                                               
         LA    RE,CLTMTAB                                                       
RUNL05   CLI   0(RE),0                                                          
         BE    RUNL50              CLT/MED TABLE IS EMPTY                       
*                                                                               
         ST    RE,FULL             SAVE CLT/MED TABLE POINTER                   
         XC    KEY,KEY                                                          
         MVC   KEY+0(2),SVQAGY     AGENCY                                       
         MVC   KEY+2(1),3(RE)      MEDIA                                        
         MVI   KEY+3,X'08'         BILL RECORD CODE                             
         MVC   KEY+4(3),0(RE)      CLIENT                                       
*                                                                               
RUNL10   GOTO1 HIGH                                                             
         B     RUNL20                                                           
*                                                                               
RUNL15   GOTO1 SEQ                                                              
*                                                                               
RUNL20   CLC   KEY(7),KEYSAVE      SAME AG/MED/X08/CLT?                         
         BNE   RUNL45              DO NEXT ENTRY IN CLT/MED TABLE               
*                                                                               
         XC    BRKEY,BRKEY                                                      
         LA    RE,KEY                                                           
         USING PBILLKEY,RE                                                      
         MVC   BRKMED,PBILKMED                                                  
         MVC   BRKCLT,PBILKCLT                                                  
         MVC   BRKINV,PBILKBNO                                                  
         DROP  RE                                                               
*                                                                               
         LA    R2,PBILLREC                                                      
         ST    R2,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         LA    R5,PBILLREC+33                                                   
         CLI   0(R5),X'08'         SEE IF FIRST ELEM IS BILL ELEM               
         BE    *+6                                                              
         DC    H'0'                MUST PRESENT                                 
         USING PBILLEL,R5                                                       
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(1,BRKBDT)                              
*                                                                               
         GOTO1 BINSRCH,WKPARMS,(0,BRKEY)                                        
         TM    0(R1),X'01'                                                      
         BO    RUNL15              RECORD IS NOT IN TABLE                       
*                                                                               
         MVC   PBILEDID,TODAYP                                                  
         GOTO1 PUTPRT                                                           
         B     RUNL15              DO NEXT RECORD                               
         DROP  R5                                                               
*                                                                               
RUNL45   L     RE,FULL                                                          
         LA    RE,4(RE)            NEXT CLT/MED ENTRY                           
         B     RUNL05                                                           
*                                                                               
RUNL50   CLI   OPENSW,C'Y'                                                      
         BNE   EXIT                                                             
         CLOSE EDIOUT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        RWPROC- READ WORKER FILES                                              
***********************************************************************         
*                                                                               
RWPROC   NTR1                                                                   
         L     RF,=A(EDIFLDS)                                                   
         A     RF,RELO                                                          
         ST    RF,AFLDTAB                                                       
         L     RF,=A(WRKRBFX)                                                   
         A     RF,RELO                                                          
         ST    RF,AWRKRBFX                                                      
*                                                                               
         XC    WRKRIND,WRKRIND                                                  
         MVC   WRKRIND(2),RCORIGID                                              
         GOTO1 DATAMGR,DMCB,(0,=C'GFILE'),=C'WRKFIL',WRKRIND,WRKRREC,  X        
               WRKRBUFF                                                         
         LA    R1,WRKRIND                                                       
         USING UKRECD,R1                                                        
         MVC   WRKFIL,UKUSRINF                                                  
         DROP  R1                                                               
*                                                                               
         XC    WRKRIND,WRKRIND                                                  
*                                                                               
RWP4     DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX',WRKFIL,WRKRIND,WRKRREC,          X        
               WRKRBUFF                                                         
*                                                                               
         TM    DMCB+8,X'80'       EOF                                           
         BNZ   RWP50                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   HAVBUF,C'N'                                                      
         LA    R7,WRKRIND                                                       
         USING UKRECD,R7                                                        
         CLC   UKUSRID,RCORIGID    MAKE SURE RIGHT USER ID                      
         BNE   RWP4                                                             
         CLI   UKDAY,X'97'         97'S ARE BILLING EDI'S                       
         BNE   RWP4                                                             
*                                  GO THRU REQUEST TABLE                        
         L     R4,ARQTAB                                                        
         USING RQTABD,R4                                                        
*                                                                               
RWP5     DS    0H                                                               
         CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    RWP4                PASSED NO REQUEST                            
         CLC   UKCLASS(1),RQTSYS   RIGHT SYSTEM                                 
         BNE   RWP5X                                                            
         CLC   UKSYSPRG(1),RQTMED  MEDIA                                        
         BNE   RWP5X                                                            
         CLC   RQTCLT,=C'ALL'                                                   
         BE    RWP5P                                                            
         MVC   WORK(3),UKSYSPRG+1   CLIENT                                      
         CLI   WORK+2,C'.'                                                      
         BNE   *+8                                                              
         MVI   WORK+2,C' '                                                      
         CLC   WORK(3),RQTCLT                                                   
         BNE   RWP5X                                                            
*                                                                               
RWP5P    DS    0H                                                               
         CLC   UKAGELD,RQTSTA      DATE VS START DATE                           
         BL    RWP5X                                                            
         CLC   UKAGELD,RQTEND      AND END DATE                                 
         BH    RWP5X                                                            
*                                                                               
RWP5T    DS    0H                                                               
         CLI   RQTOPT3,C'R'        REDO?                                        
         BE    RWP5W                                                            
*                                  NORMAL RUN                                   
         TM    UKSTAT,X'08'        NO, CAN'T BE ALREADY DONE (KEEP)             
         BZ    RWP6           PROCESS                                           
         B     RWP5X          SKIP IF ALREADY DONE                              
*                                                                               
RWP5W    DS    0H             FOR RERUNS                                        
         TM    UKSTAT,X'08'   MUST BE ALREADY DONE                              
         BO    RWP6           PROCESS                                           
*                                                                               
RWP5X    DS    0H                  SKIP                                         
         LA    R4,RQTABL(R4)       ELSE NEXT REQUEST TABLE ENTRY                
         B     RWP5                                                             
*                                                                               
*                                  INDEX PASSES, NOW READ RECORDS               
RWP6     DS    0H                                                               
         MVI   FIRST,C'Y'        FIRST RECORD                                   
         B     RWP6AX                                                           
*                                                                               
RWP6A    DS    0H                                                               
         MVI   FIRST,C'N'                                                       
RWP6AX   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'READ',WRKFIL,WRKRIND,WRKRREC,           X        
               WRKRBUFF                                                         
*                                                                               
         TM    DMCB+8,X'80'         EOF                                         
         BNZ   RWP40                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   HAVBUF,C'Y'         HAVE WE DONE BUFFER?                         
         BE    RWP7                YES, READ RCORDS                             
         MVI   HAVBUF,C'Y'                                                      
*                                                                               
         LA    R5,WRKRBUFF                                                      
         USING W_RECD,R5                                                        
*                                                                               
*              NOTE- W_DESC IS APPARENTLY INITIALIZED TO SPACES,                
*                    NOT NULLS. IT IS USED AS FOLLOWS-                          
*                      INVOICE NUMBER(10),                                      
*                      CONTROL(1) - X'80'=TRANSFERED                            
*                      DATE(2) - TRANSFER DATE                                  
*                                                                               
         CLI   FIRST,C'Y'                                                       
         BNE   RWP6D                                                            
*                                                                               
         MVC   SAVDESC,W_DESC                                                   
         LA    RF,W_DESC+9         PICK UP LAST 4 OF INVNO                      
         CLI   0(RF),C' '          TO DO FILTERING                              
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         AHI   RF,-3                                                            
         CLC   0(4,RF),RQTINV1     INVNO VS START                               
         BL    RWP5X                                                            
         CLC   0(4,RF),RQTINV2     AND END                                      
         BH    RWP5X                                                            
*                                                                               
RWP6D    DS    0H                                                               
*                                                                               
RWP7     DS    0H                                                               
         LA    RF,WRKRREC                                                       
         SR    R1,R1                                                            
         ICM   R1,3,WRKRREC                                                     
         AR    RF,R1                                                            
         ST    RF,EOR                                                           
*                                                                               
         CLI   RQTOPT1,C'T'        TRACE ?                                      
         BNE   *+8                 NO, SKIP WKR REC PRINT                       
         BAS   RE,PRTWKR                                                        
         BAS   RE,PUTFXR                                                        
*                                                                               
         CLI   FIRST,C'Y'          ONLY NEED TO ONCE                            
         BNE   RWP8A                                                            
*                                                                               
*        FOR TEST RERUNS USE ORIGINAL DATE                                      
*                                                                               
         IF    RQTOPT2,EQ,C'Y',AND,RQTOPT3,EQ,C'R',RWP8                         
*                                                                               
         MVI   W_DESC+10,X'80'     TRANSMITTED                                  
         MVC   W_DESC+11(2),TODAYP SAVE TRANSMITTAL DATE                        
*                                                                               
         CLI   RQTOPT2,C'Y'     SEE IF TEST RUN                                 
         BE    RWP8             SKIP WRITE                                      
*                                                                               
*        MUST WRITE BACK RECORD                                                 
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'WRITE',WRKFIL,WRKRIND,WRKRREC,          X        
               WRKRBUFF                                                         
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RWP8     MVC   SVTDATE,W_DESC+11     MUST SAVE TRANSMITTAL DATE                 
*                                    FOR DISPLAY                                
*                                                                               
*                                                                               
RWP8A    DS    0H                                                               
         CLI   RQTOPT2,C'Y'        IF TEST MODE                                 
         BE    RWP8B               DON'T NEED TO PUT CLT CODE IN TABLE          
         LA    RE,CLTMTAB                                                       
         SR    RF,RF                                                            
*                                                                               
         MVC   FULL(3),UKSYSPRG+1                                               
         CLI   FULL+2,X'4B'        A PERIOD?                                    
         BNE   *+8                                                              
         MVI   FULL+2,C' '         IT'S REALL A SPACE, NOT PERIOD               
*                                                                               
RWP8AD   CHI   RF,100                                                           
         BNH   *+6                                                              
         DC    H'0'                CLIENT TABLE IS FULL                         
         CLC   0(3,RE),FULL                                                     
         BE    RWP8AX              ALREADY IN TABLE                             
         CLI   0(RE),0                                                          
         BNE   RWP8AH                                                           
         MVC   0(3,RE),FULL        CLIENT CODE INTO TAB                         
         MVC   3(1,RE),UKSYSPRG    MEDIA CODE INTO TAB                          
         B     RWP8AX                                                           
RWP8AH   LA    RE,4(RE)            NEXT TABLE ENTRY                             
         AHI   RF,1                COUNTING NUMBER OF ENTRIES                   
         B     RWP8AD                                                           
*                                                                               
RWP8AX   DS    0H                                                               
*                                                                               
         CLI   RQTOPT2,C'Y'      SEE IF TEST RUN                                
         BE    RWP8B             YES - DON'T ADD TO BINSRCH TABLE               
*                                                                               
         CLI   FIRST,C'Y'                                                       
         BNE   RWP8B                                                            
*                                                                               
         XC    BRKEY,BRKEY                                                      
         MVC   BRKMED,UKSYSPRG                                                  
         MVC   BRKCLT,FULL         CLIENT CODE                                  
*                                                                               
         GOTO1 DATCON,DMCB,(2,UKAGELD),(1,BRKBDT)                               
*                                                                               
         LA    RF,W_DESC+9         PICK UP LAST 4 OF INVNO                      
         CLI   0(RF),C' '          TO DO FILTERING                              
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         AHI   RF,-3                                                            
         PACK  DUB,0(4,RF)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,BRKINV                                                      
*                                                                               
         GOTO1 BINSRCH,WKPARMS,(1,BRKEY)                                        
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
RWP8B    DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
         B     RWP6A                                                            
*                                                                               
*                                                                               
*                                                                               
RWP40    DS    0H                                                               
*                                                                               
         LA    R2,P1                                                            
         USING LIND,R2                                                          
         MVC   LINMED,UKSYSPRG                                                  
         MVC   LINCLT,UKSYSPRG+1                                                
         CLI   LINCLT+2,C'.'                                                    
         BNE   *+8                                                              
         MVI   LINCLT+2,C' '                                                    
         MVC   LININV,SAVDESC                                                   
         MVC   LINIDAT,SAVINVD                                                  
         GOTO1 DATCON,DMCB,(2,UKAGELD),(5,LINBDAT)                              
         TM    UKSTAT,X'08'        IF NOT PREVIOUSLY DONE                       
         BO    RWP42                                                            
         MVC   WORK(2),TODAYP      SAY DOING IT NOW                             
         MVI   LINEDAT+8,C'*'      MARK AS TODAY (THIS RUN)                     
         B     RWP43                                                            
*                                                                               
RWP42    DS    0H                                                               
*******  MVC   WORK(2),UKAGEDD     ELSE SHOW KILL (TRNSFR) DATE                 
         MVC   WORK(2),SVTDATE     (TRANSFER DATE)                              
*                                                                               
RWP43    DS    0H                                                               
         CLC   WORK(2),=X'FFFF'      BE SURE I HAVE A REAL DATE                 
         BE    RWP43C                                                           
         CLC   WORK(2),SPACES        BE SURE I HAVE A REAL DATE                 
         BE    RWP43C                                                           
         OC    WORK(2),WORK          BE SURE I HAVE A REAL DATE                 
         BZ    RWP43C                                                           
         GOTO1 DATCON,DMCB,(2,WORK),(5,LINEDAT)                                 
RWP43C   DS    0H                                                               
         MVC   LINADDR,SAVHEAD     EDI ADDRESS, ETC.                            
         BAS   RE,MYPRNT                                                        
         DROP  R2                                                               
*                                                                               
         CLI   RQTOPT2,C'Y'        IF TEST MODE                                 
         BE    RWP44               SKIP KEEP                                    
*******  TM    UKSTAT,X'08'        IF ALREADY DONE (KEEP)                       
*******  BO    RWP44               SKIP                                         
*******                                                                         
*******        DATAMGR MAY NOT LET ME KEEP AN ALREADY KEPT FILE                 
*******         - IT DOESN'T SEEM TO CARE                                       
*******                                                                         
         GOTO1 DATAMGR,DMCB,=C'KEEP',WRKFIL,WRKRIND,WRKRREC,           X        
               WRKRBUFF                                                         
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RWP44    DS    0H                                                               
         B     RWP4                NEXT INDEX                                   
*                                                                               
RWP50    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R5                                                               
*                                                                               
PRTWKR   NTR1                      PRINT WKR RECORD                             
         SR    R4,R4                                                            
         ICM   R4,3,WRKRREC                                                     
         AHI   R4,4                                                             
         CHI   R4,65                                                            
         BNH   *+8                                                              
         LA    R4,65                                                            
         BCTR  R4,0                                                             
         EX    R4,WRKMVC                                                        
         BAS   RE,MYPRNT                                                        
         B     EXIT                                                             
*                                                                               
WRKMVC   MVC   P1+2(0),WRKRREC+4                                                
         EJECT                                                                  
***********************************************************************         
*        PUTFXR -  CONVERT TO FIXED RECORD FORMAT                               
***********************************************************************         
*                                                                               
PUTFXR   NTR1                                                                   
*                                    SPECIAL IDENTIFICATION RECORD              
         CLI   WRKRREC+4,C'H'        STARTS WITH H                              
         BNE   PF04                                                             
         CLI   WRKRREC+6,FDELIM      AND 3RD CHAR NOT A FLD DELIM               
         BE    PF04                                                             
*                                                                               
         MVC   FIXREC(2),=H'45'    RECORD LENGTH                                
         XC    FIXREC+2(2),FIXREC+2                                             
         MVC   FIXREC+4(41),WRKRREC+4                                           
         MVC   SAVHEAD(41),WRKRREC+4   SAVE EDI ADDRESS, ETC..                  
         CLI   OPENSW,C'Y'                                                      
         BNE   PF03                                                             
         PUT   EDIOUT,FIXREC                                                    
*                                                                               
PF03     DS    0H                                                               
         CLI   RQTOPT1,C'T'        TRACE?                                       
         BNE   PFX                                                              
         MVC   P1+2(41),FIXREC                                                  
         BAS   RE,MYPRNT                                                        
         B     PFX                                                              
*                                                                               
PF04     DS    0H                                                               
         LA    R8,WRKRREC+4        FIRST WORKER REC POSITION                    
         L     R5,AFLDTAB          START OF FIELD TABLE                         
*                                                                               
PF04D    DS    0H                  FIND ENTRY FOR THIS RECORD TYPE              
         CLI   0(R5),X'FF'         EOT                                          
         BNE   *+6                                                              
         DC    H'0'                INVALID RECORD                               
         CLC   0(2,R8),0(R5)       TEST RECORD TYPE                             
         BE    PF05                                                             
         ICM   R5,15,22(R5)        TRY NEXT RECORD TYPE                         
         B     PF04D                                                            
*                                                                               
PF05     DS    0H                                                               
         LA    R5,26(R5)           POINT TO FIRST FIELD DESCRIPTOR              
         LA    R2,FIXREC+4         FIRST POS IN OUTPUT REC                      
*                                                                               
PF05D    DS    0H                                                               
         CLI   0(R5),X'FF'         END OF FIELD DESCRIPTORS                     
         BE    PF20                DONE, IGNORE ANY FIELDS BEYOND TABLE         
*                                                                               
         LR    R1,R8               SAVE START OF WKR FIELD                      
*                                  LOOK FOR FIELD DELIMITER                     
PF06     DS    0H                                                               
         CLI   0(R8),FDELIM                                                     
         BE    PF06D                                                            
*                                                                               
         C     R8,EOR                                                           
         BL    *+6                                                              
         DC    H'0'                FIELD MISSING                                
*                                                                               
         LA    R8,1(R8)            NEXT POSITION                                
         B     PF06                                                             
*                                                                               
PF06D    DS    0H                                                               
         LR    R3,R8                                                            
         SR    R3,R1                                                            
         BP    PF07                IF FIELD LEN=0                               
         MVI   VLEN,1              TREAT AS 1                                   
         MVI   VTXT,C' '                                                        
         B     PF08                                                             
*                                                                               
PF07     DS    0H                                                               
         STC   R3,VLEN             LENGTH OF VARIABLE FIELD                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   VTXT(0),0(R1)       SAVE TEXT                                    
*                                                                               
PF08     DS    0H                                                               
         ZIC   R3,0(R5)            FIXED LENGTH                                 
         STC   R3,FLEN                                                          
         CLC   VLEN,FLEN                                                        
         BNH   *+6                                                              
         DC    H'0'                FIELD DATA TOO LONG                          
*                                                                               
         BCTR  R3,0                CLEAR OUTPUT RECORD AREA                     
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,(R2)),SPACES                                                 
*                                                                               
         ZIC   R3,VLEN             THEN MOVE VARIABLE TEXT                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,(R2)),VTXT                                                   
*                                                                               
*                                  TEST ANY SPECIAL FIELD HANDLING              
         CLC   WRKRREC+4(2),=C'IH' INV HEADER RECORD                            
         BNE   PF08B                                                            
         CLC   1(12,R5),=C'INVOICE DATE'                                        
         BNE   PF08B                                                            
         MVC   SAVINVD,VTXT        SAVE INVOICE DATE                            
*                                                                               
PF08B    DS    0H                                                               
         ZIC   R3,FLEN             NEXT OUTPUT POSITION                         
         AR    R2,R3                                                            
         LA    R8,1(R8)            BUMP PAST THIS FIELD DELIM                   
         LA    R5,21(R5)           NEXT FIELD DESCRIPTOR                        
         B     PF05D                                                            
*                                                                               
PF20     DS    0H                                                               
         LA    R1,FIXREC                                                        
         SR    R2,R1                                                            
         STCM  R2,3,FIXREC                                                      
         XC    FIXREC+2(2),FIXREC+2                                             
*                                                                               
         CLI   OPENSW,C'Y'                                                      
         BNE   PF22                                                             
         PUT   EDIOUT,FIXREC                                                    
*                                                                               
PF22     DS    0H                                                               
         CLI   RQTOPT1,C'T'        TRACE?                                       
         BNE   PFX                 NO, DONE                                     
         CHI   R2,132                                                           
         BNH   *+8                                                              
         LHI   R2,132                                                           
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P1+2(0),FIXREC                                                   
         BAS   RE,MYPRNT                                                        
*                                                                               
PFX      DS    0H                                                               
         B     EXIT                                                             
*        EJECT                                                                  
**********************************************************************          
*        MYPRNT                                                                 
**********************************************************************          
*                                                                               
MYPRNT   NTR1                                                                   
*                                                                               
MYPRNT4  DS    0H                                                               
         MVC   HEAD3(7),=C'USER ID'                                             
         MVC   HEAD3+10(10),SAVUIDN                                             
*                                                                               
         MVC   HEAD5(14),=C'** LIVE RUN **'                                     
         CLI   RQTOPT2,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+3(4),=C'TEST'                                              
*                                                                               
         CLI   RQTOPT3,C'R'                                                     
         BNE   *+10                                                             
         MVC   HEAD6(11),=C'** RERUN **'                                        
         GOTO1 REPORT                                                           
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
EDIOUT   DCB   DDNAME=EDIOUT,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=400,                                              X        
               BLKSIZE=4000,                                           X        
               MACRF=PM                                                         
*                                                                               
WRKRBUFF DS    14336X                                                           
WRKRBFX  EQU   *-WRKRBUFF                                                       
*                                                                               
*        SPACE 2                                                                
WORKD    DSECT                                                                  
*                                                                               
FDELIM   EQU   X'5E'               FIELD DELIMITER - SEMICOLON                  
MAXREQS  EQU   100                 MAX REQUESTS                                 
*                                                                               
SVTDATE  DS    XL2        TRANSFER DATE                                         
*                                                                               
FIRST    DS    CL1    SET TO "Y" FOR FIRST RECORD FOR AN INDEX                  
*                                                                               
OPENSW   DS    XL1                                                              
NETPAKSW DS    CL1                                                              
LIVERUN  DS    CL1    WILL BE Y IF NON-TEST REQ ENCOUNTERED                     
*                                                                               
FIXREC   DS    0XL400                                                           
         DS    XL4                                                              
         DS    XL396                                                            
*                                                                               
CLTMTAB  DS    100CL4              100 ENTRIES OF CLT/MED CODE                  
CLTMTABX DS    X                                                                
*                                                                               
WKPARMS  DS    6F                  WORKING STORAGE FOR PARAMETERS               
*                                                                               
BRKEY    DS    0XL9                BILL RECORD KEY FOR BINSRCH TAB              
BRKMED   DS    CL1                                                              
BRKCLT   DS    CL3                                                              
BRKINV   DS    XL2                                                              
BRKBDT   DS    XL3                                                              
*                                                                               
SVQAGY   DS    CL2                 SAVE QAGENCY FOR RUNLAST                     
*                                                                               
HAVBUF   DS    CL1                                                              
AFLDTAB  DS    A                                                                
EOR      DS    A                                                                
AWRKRBFX DS    A                                                                
ARQTAB   DS    A                                                                
ARQTABX  DS    A                                                                
VLEN     DS    X                                                                
VTXT     DS    XL132                                                            
FLEN     DS    X                                                                
SAVDESC  DS    CL16                                                             
SAVUIDN  DS    CL10                                                             
SAVINVD  DS    CL8                                                              
SAVXFRD  DS    CL8                                                              
SAVHEAD  DS    CL45                SAVE 'H' HEADER RECORD                       
WRKFIL   DS    CL8                                                              
WRKRIND  DS    XL44                                                             
WRKRREC  DS    XL1024                                                           
         SPACE 2                                                                
LIND     DSECT                     DSECT FOR PRINT LINE                         
LINMED   DS    CL1                                                              
         DS    CL1                                                              
LINCLT   DS    CL3                                                              
         DS    CL2                                                              
LININV   DS    CL10                                                             
         DS    CL2                                                              
LINBDAT  DS    CL8                                                              
         DS    CL2                                                              
LINIDAT  DS    CL8                                                              
         DS    CL2                                                              
LINEDAT  DS    CL8                                                              
         DS    CL2                                                              
LINADDR  DS    CL45                                                             
         EJECT                                                                  
PPEB02   CSECT                                                                  
*                                                                               
***********************************************************************         
*        EDI FIELD LENGTHS FOR PRINT BILLS                                      
*                                                                               
*        USED WHEN CONVERTING INVOICES IN WORKER FILE FORMAT                    
*        WHERE FIELDS ARE VARIABLE LENGTH, TO A FIXED FIELD                     
*        LENGTH FORMAT FOR DATASETS.                                            
*                                                                               
*        TABLE FORMAT AS FOLLOWS:                                               
*                                                                               
*        RECORD LINE -CODE(2),DESC(20),A(NEXT)                                  
*        FIELD LINE  -LENGTH(1),DESC(20)                                        
*                                                                               
*        ONE LINE FOR EACH RECORD TYPE, FOLLOWED BUY A SET OF                   
*        LINES, ONE FOR EACH FIELD.                                             
*                                                                               
***********************************************************************         
*                                                                               
         DS    0D                                                               
         DC    CL8'*EDIFLDS'                                                    
EDIFLDS  DS    0X                                                               
EDFLIH   DC    CL2'IH',CL20'INVOICE HEADER      ',AL4(EDFLAG)                   
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(010),CL20'INVOICE NUMBER      '                              
         DC    AL1(008),CL20'INVOICE DATE        '                              
         DC    AL1(008),CL20'DUE DATE            '                              
         DC    AL1(001),CL20'INVOICE TYPE        '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLAG   DC    CL2'AG',CL20'AGENCY RECORD       ',AL4(EDFLME)                   
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(033),CL20'NAME                '                              
         DC    AL1(033),CL20'ADDRESS             '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLME   DC    CL2'ME',CL20'MEDIA RECORD        ',AL4(EDFLAV)                   
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(002),CL20'CODE                '                              
         DC    AL1(010),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLAV   DC    CL2'AV',CL20'ADVERTISER          ',AL4(EDFLBT)                   
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(003),CL20'CODE                '                              
         DC    AL1(020),CL20'NAME                '                              
         DC    AL1(005),CL20'INTERFACE NUMBER    '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLBT   DC    CL2'BT',CL20'BILL TO RECORD      ',AL4(EDFLP1)                   
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(036),CL20'ADDRESS LINE 1      '                              
         DC    AL1(036),CL20'ADDRESS LINE 2      '                              
         DC    AL1(036),CL20'ADDRESS LINE 3      '                              
         DC    AL1(036),CL20'ADDRESS LINE 4      '                              
         DC    AL1(036),CL20'ADDRESS LINE 5      '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLP1   DC    CL2'P1',CL20'PRODUCT GRP LEVEL 1  ',AL4(EDFLP2)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(005),CL20'CODE                '                              
         DC    AL1(012),CL20'LEVEL DESC          '                              
         DC    AL1(024),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLP2   DC    CL2'P2',CL20'PRODUCT GRP LEVEL 2  ',AL4(EDFLP3)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(005),CL20'CODE                '                              
         DC    AL1(012),CL20'LEVEL DESC          '                              
         DC    AL1(024),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLP3   DC    CL2'P3',CL20'PRODUCT GRP LEVEL 3  ',AL4(EDFLPR)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(005),CL20'CODE                '                              
         DC    AL1(012),CL20'LEVEL DESC          '                              
         DC    AL1(024),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLPR   DC    CL2'PR',CL20'PRODUCT RECORD       ',AL4(EDFLES)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(003),CL20'CODE                '                              
         DC    AL1(020),CL20'NAME                '                              
         DC    AL1(005),CL20'INTERFACE NUMBER    '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLES   DC    CL2'ES',CL20'ESTIMATE RECORD      ',AL4(EDFLM1)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(003),CL20'CODE                '                              
         DC    AL1(020),CL20'NAME                '                              
         DC    AL1(020),CL20'NAME - LINE 2       '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLM1   DC    CL2'M1',CL20'MARKET GRP LEVEL 1   ',AL4(EDFLM2)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(005),CL20'CODE                '                              
         DC    AL1(012),CL20'LEVEL DESC          '                              
         DC    AL1(024),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLM2   DC    CL2'M2',CL20'MARKET GRP LEVEL 2   ',AL4(EDFLM3)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(005),CL20'CODE                '                              
         DC    AL1(012),CL20'LEVEL DESC          '                              
         DC    AL1(024),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLM3   DC    CL2'M3',CL20'MARKET GRP LEVEL 3   ',AL4(EDFLMK)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(005),CL20'CODE                '                              
         DC    AL1(012),CL20'LEVEL DESC          '                              
         DC    AL1(024),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLMK   DC    CL2'MK',CL20'MARKET RECORD        ',AL4(EDFLJB)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(020),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLJB   DC    CL2'JB',CL20'JOB RECORD           ',AL4(EDFLST)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(025),CL20'JOB INFO - LINE 1   '                              
         DC    AL1(025),CL20'JOB INFO - LINE 2   '                              
         DC    AL1(030),CL20'JOB INFO - LINE 3   '                              
         DC    AL1(030),CL20'JOB INFO - LINE 4   '                              
         DC    AL1(030),CL20'JOB INFO - LINE 5   '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLST   DC    CL2'PB',CL20'PUBLICATION RECORD   ',AL4(EDFLCM)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(060),CL20'NAME                '                              
         DC    AL1(020),CL20'NAME - PART 2       '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLCM   DC    CL2'CM',CL20'COMMENT RECORD       ',AL4(EDFLUD)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(004),CL20'COMMENT LEVEL NAME  '                              
         DC    AL1(080),CL20'COMMENT TEXT        '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLUD   DC    CL2'UD',CL20'USER-DEFINED DATA    ',AL4(EDFLMT)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(002),CL20'DATA LOCATION       '                              
         DC    AL1(020),CL20'DATA DESCRIPTION    '                              
         DC    AL1(032),CL20'DATA TEXT           '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLMT   DC    CL2'MT',CL20'MONTHLY TOTAL RECPRD ',AL4(EDFLAD)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(004),CL20'DETAIL LEVEL NAME   '                              
         DC    AL1(008),CL20'MONTH OF SERVICE    '                              
         DC    AL1(010),CL20'PREVIOUS INV NUMBER '                              
         DC    AL1(011),CL20'ORDERED GROSS       '                              
         DC    AL1(011),CL20'ORDERED NET         '                              
         DC    AL1(011),CL20'ORDERED CASH DISC.  '                              
         DC    AL1(011),CL20'ORDERED TAX         '                              
         DC    AL1(011),CL20'ORDERED INSERTIONS  '                              
         DC    AL1(011),CL20'PREVIOUS GROSS      '                              
         DC    AL1(011),CL20'PREVIOUS NET        '                              
         DC    AL1(011),CL20'PREVIOUS CASH DISC. '                              
         DC    AL1(011),CL20'PREVIOUS TAX        '                              
         DC    AL1(011),CL20'PREVIOUS INSERTIONS '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLAD   DC    CL2'AD',CL20'AMOUNT DUE RECORD    ',AL4(EDFLPI)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(004),CL20'DETAIL LEVEL NAME   '                              
         DC    AL1(001),CL20'BILL BASIS          '                              
         DC    AL1(001),CL20'ADJUSTMENT BASIS    '                              
         DC    AL1(007),CL20'ADJUSTMENT %        '                              
         DC    AL1(011),CL20'BASIS AMOUNT        '                              
         DC    AL1(011),CL20'ADJUSTMENT AMOUNT   '                              
         DC    AL1(011),CL20'AMOUNT DUE AT LEVEL '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLPI   DC    CL2'PI',CL20'PREV INV LIST RECORD ',AL4(EDFLRA)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(010),CL20'INVOICE NUMBER      '                              
         DC    AL1(011),CL20'GROSS               '                              
         DC    AL1(011),CL20'NET                 '                              
         DC    AL1(011),CL20'CASH DISC.          '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
****     NOT USED FOR PRINTPAK                                                  
****                                                                            
EDFLRA   DC    CL2'RA',CL20'REMITTANCE ADDRESS   ',AL4(EDFLXX)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(020),CL20'NAME                '                              
         DC    AL1(024),CL20'ADDRESS             '                              
         DC    AL1(024),CL20'CITY                '                              
         DC    AL1(003),CL20'STATE               '                              
         DC    AL1(010),CL20'POSTAL CODE         '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLXX   DC    X'FF'                                                            
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL8'**RQTAB*'                                                    
RQTAB    DS    0C                                                               
         ORG   *+(RQTABL*MAXREQS)                                               
RQTABX   EQU   *                                                                
*                                                                               
RQTABD   DSECT   DSECT FOR REQUEST TABLE                                        
RQTSYS   DS    CL1                                                              
RQTMED   DS    CL1                                                              
RQTCLT   DS    CL3                                                              
RQTPRD   DS    CL3                                                              
RQTEST   DS    XL1                                                              
RQTSTA   DS    XL2                                                              
RQTEND   DS    XL2                                                              
RQTINV1  DS    CL4                                                              
RQTINV2  DS    CL4                                                              
RQTOPTS  DS    0CL6                                                             
RQTOPT1  DS    CL1                                                              
RQTOPT2  DS    CL1                                                              
RQTOPT3  DS    CL1                                                              
RQTOPT4  DS    CL1                                                              
RQTOPT5  DS    CL1                                                              
RQTOPT6  DS    CL1                                                              
RQTRQS   DS    CL12                                                             
RQTABL   EQU   *-RQTABD                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE DMWRKFK                                                        
*                                                                               
       ++INCLUDE DMWRKFL                                                        
*                                                                               
       ++INCLUDE DMWRKFD                                                        
*                                                                               
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDMASTD                                                        
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
*                                                                               
* START AND END INVOIVE NUMBERS COL 53(4), AND COL 56 (4)                       
*                                                                               
QINVNO1  EQU   QRECORD+52                                                       
QINVNO2  EQU   QRECORD+56                                                       
*                                                                               
       ++INCLUDE PPREPWORK2                                                     
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
*                                                                               
*                                                                               
*                                                                               
BRMAX    EQU   2000                MAX RECORD IN TABLE                          
BRTABLN  EQU   9                                                                
*                                                                               
*                                                                               
*                                                                               
BRTAB    CSECT                     BILL RECORD TABLE FOR BINSRCH                
         ORG   *+(BRMAX*BRTABLN)                                                
         DC    X'00'                                                            
BRTABLNQ EQU   *-BRTAB                                                          
*                                                                               
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062PPREPEB02X05/01/02'                                      
         END                                                                    
