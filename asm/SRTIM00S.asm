*          DATA SET SRTIM00S   AT LEVEL 049 AS OF 01/18/02                      
*PHASE T10400A                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE WKSCAN                                                                 
*INCLUDE ENDOFMOD                                                               
*INCLUDE BINSR31                                                                
*&&      SET   NOP=N                                                            
T10400   TITLE 'SRTIM00 ($TIM) - FACPAK TIMER EXPIRATION ROUTINES'              
         PRINT NOGEN                                                            
TIMERS   CSECT                                                                  
         NMODL WORKX-WORKD,**$TIM**,RA,CLEAR=YES,RR=RE                          
         USING WORKD,RC            RC=A(TEMP W/S)                               
         ST    RE,RELO                                                          
         ST    R1,SAVER1                                                        
         MVC   SRPARAS(SRPARAL),0(R1)                                           
         L     R9,SRPARA1                                                       
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R8,SRPARA3                                                       
         USING UTLD,R8             R8=A(UTL)                                    
         MVI   MODE,0                                                           
*                                                                               
         GOTO1 VTICTOC,DUB,C'SGET' SAVE TIME AS P'0HHMMSS+'                     
         MVC   TIMEHMS,DUB                                                      
         GOTO1 VTICTOC,DUB,C'1SET',0 DISABLE LOOP TIMER                         
         TIME  BIN                                                              
         STM   R0,R1,DUB           R0=TIME(BIN 1/100 SECS),R1=DATE              
         L     RF,SRPARA4                                                       
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(6,DUB+4),(2,DATEC)                                    
*                                                                               
         GOTO1 (RF),DMCB,(2,DATEC),(0,DUB) DUB = YYMMDD                         
         L     RF,SRPARA4                                                       
         L     RF,CGETDAY-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DUB,DUB1          DUB1 = DDD                           
         L     RF,SRPARA4                                                       
         L     RF,CDEJAVU-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,DUB1),(X'10',DAY)                                   
*                                                                               
         LA    RE,L'PQINDEX        SET PRTQUE CONTROL INTERVAL DATA             
         STH   RE,CINDXLN                                                       
         L     RE,VENQDEQ                                                       
         ST    RE,CIENQDEQ                                                      
         MVC   PRTQID,PRTQUE                                                    
         XC    CIDATA,CIDATA                                                    
         L     RE,=A(CIREC-WORKD)                                               
         LA    RE,WORKD(RE)                                                     
         ST    RE,ACIREC                                                        
         L     RE,=A(IO-WORKD)                                                  
         LA    RE,WORKD(RE)                                                     
         ST    RE,AIO              SET A(AIO) IN W/S                            
         L     RE,=A(IO2-WORKD)                                                 
         LA    RE,WORKD(RE)                                                     
         ST    RE,AIO2             SET A(AIO2) IN W/S                           
         L     RE,=A(PRTLST-WORKD)                                              
         LA    RE,WORKD(RE)                                                     
         ST    RE,APRTLST                                                       
         L     RE,=A(PRTLSTX-WORKD)                                             
         LA    RE,WORKD(RE)                                                     
         ST    RE,APRTLSTX                                                      
*                                                                               
         L     RE,VSSB             EXTRACT SSB DATA                             
         USING SSBD,RE                                                          
         OI    SSBSTAT3,SSBSRTIM                                                
         MVC   RECLEN,SSBTWAL      SET TEMPSTR RECORD LENGTH                    
         IC    R0,SSBSYSID                                                      
         STC   R0,SYSID                                                         
         SLL   R0,4                                                             
         STC   R0,SYSIDHOB         SET FACPAK SYSTEM ID AS HOB                  
         MVC   SYSNAME,SSBSYSNA    SET FACPAK 3 CHR NAME                        
         SR    R1,R1                                                            
         IC    R1,SSBSYSIX         ADD 1 CHR FOR AORS                           
         SRL   R1,4                                                             
         LTR   R1,R1                                                            
         BZ    *+12                                                             
         LA    R1,X'C0'(R1)                                                     
         STC   R1,SYSNAME+3                                                     
         MVC   SYSNAM4,SSBSYSN4                                                 
         MVC   SYSDATE,SSBDATE     SET FACPAK START DATE                        
         MVI   RSTRDONE,C'N'                                                    
         TM    SSBSTAT1,SSBSRSPR                                                
         BZ    *+8                                                              
         MVI   RSTRDONE,C'Y'       SET PRINTER RESTART DONE                     
*                                                                               
         MVI   FRST,C'N'           TEST IF FIRST TIME FLAG SET                  
*                                                                               
*NOP     TM    TAGYB,X'80'                                                      
*        BZ    T2                                                               
*        NI    TAGYB,X'7F'         YES - TURN OFF FLAG AND GO TO INIT           
*                                                                               
         OC    SSBTPOPC,SSBTPOPC   IS THIS THE FIRST SRTIM                      
         BNZ   T2                                                               
         MVC   SSBTPOPC,=F'1'      SET TO INITIAL                               
         MVI   FRST,C'Y'                                                        
         B     INIT                AND INIT FACPAK TABLES                       
         DROP  RE                                                               
         EJECT                                                                  
* FIRST TIME SYSTEM INITIALISATION CODE                                         
*                                                                               
INIT     L     RE,VSSB             DISABLE MULTI-TASKING WAITS                  
         MVI   SSBMTIND-SSBD(RE),0                                              
*                                                                               
INITRP   CLI   RSTRDONE,C'Y'                                                    
         BE    *+8                                                              
         BAS   RE,RSTRPRNS         RESTART REMOTE PRINTERS                      
*                                                                               
         L     RE,VSSB                                                          
         XC    LOGREC,LOGREC       SET TIMER 2 LOG DATA                         
         MVC   LOGID,MYLOGID                                                    
         MVI   LOGID+3,C'S'        $PQS FOR START TIMER POP                     
         MVC   LOGLUID,SPACES                                                   
         MVC   LOGINFO,=C'STRT1'                                                
         MVC   LOGINFO+4,SSBRCNT-SSBD(RE)                                       
         OI    LOGINFO+4,X'F0'                                                  
         MVC   LOGTIME,TIMEHMS                                                  
         MVC   LOGNAME,SYSNAME                                                  
         MVC   LOGSYSID,SSBSYSIX-SSBD(RE)                                       
         MVC   LOGDATE,SYSDATE                                                  
         TIME                                                                   
         ST    R1,LOGJLND                                                       
         GOTO1 VLOGGER,LOGREC                                                   
*                                                                               
INITX    L     RE,VSSB             ENABLE MULTI-TASKING WAITS                   
         MVI   SSBMTIND-SSBD(RE),C'M'                                           
         CLI   TAGYB,0             TEST IF ANY TIMER EXPIRIES                   
         BE    TIMEXIT             NO - EXIT                                    
         B     T2                                                               
         EJECT                                                                  
* TIMER 2 - FACPAK MAJOR TIMER                                                  
*                                                                               
T2       L     RE,VSSB             AOR HAS NO T2                                
         TM    SSBSTAT4-SSBD(RE),SSBSAOR                                        
         BO    TIMEXIT                                                          
*                                                                               
         CLI   TAGYB,2             TEST IF TIMER 2 HAS EXPIRED                  
         BNE   T3                                                               
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         L     R1,SSBTPOPC         BUMP NUMBER OF TIMER POPS IN SSB             
         LA    R1,1(R1)                                                         
         ST    R1,SSBTPOPC                                                      
         ST    R1,FULL             SAVE NUMBER OF POPS                          
         MVC   TIMEHMSL,SSBTPOPT   SAVE TIME OF LAST POP                        
         MVC   SSBTPOPT,TIMEHMS    SET TIME OF THIS POP IN SSB                  
*&&US*&& OI    SSBDARFL,SSBDRSDR   CALL $SPTDARE                                
         DROP  RE                                                               
         TM    FULL+3,X'03'        TEST EVERY FOURTH POP                        
         BNZ   *+8                                                              
         OI    MODE,X'01'          YES SET EXTENDED WORK MODE                   
         TM    FULL+3,X'0F'        TEST EVERY 16TH POP                          
*&&UK*&& BNZ   *+8                                                              
         OI    MODE,X'80'          YES SET EXTENDED PQ MODE                     
*                                                                               
         XC    LOGREC,LOGREC       SET TIMER 2 LOG DATA                         
         MVC   LOGID,MYLOGID                                                    
         MVI   LOGID+3,C'T'        $PQT FOR BASIC TIMER POP                     
         MVC   LOGLUID,SPACES                                                   
         MVC   LOGTIME,TIMEHMS                                                  
         MVC   LOGNAME,SYSNAM4                                                  
         MVC   LOGSYSID,SYSID                                                   
         MVC   LOGDATE,SYSDATE                                                  
         TIME                                                                   
         ST    R1,LOGJLND                                                       
         GOTO1 VLOGGER,LOGREC                                                   
*                                                                               
         BRAS  RE,BLDJOB                                                        
*                                                                               
         BRAS  RE,BLDBC            BUILD LIST OF BROADCAST MSGS                 
         BRAS  RE,SETBC            SET ANY TERMINALS THAT NOW QUALIFY           
         TM    MODE,X'01'                                                       
         BZ    *+8                                                              
         BRAS  RE,ESSTIME          CHECK ESS TIMEOUTS EVERY 4TH POP             
*                                                                               
*&&US*&& BRAS  RE,SETDAR           CHECK AND SET ANY DARE TERMS                 
*                                                                               
*&&UK*&& BRAS  RE,CHKMSG           CHECK ON MESSAGE BUFFER                      
*&&UK*&& BRAS  RE,CHKCOM           CHECK COMMAND BLOCK                          
*                                                                               
         CLI   RSTRDONE,C'Y'       TEST TO RESTART PRINTERS                     
         BE    T21                                                              
         BRAS  RE,RSTRPRNS         RESTART REMOTE PRINTERS                      
*                                                                               
T21      L     R8,VUTL             SEARCH UTL LOOKING FOR PRINTERS              
         LH    R6,0(R8)                                                         
         L     R7,2(R8)                                                         
         LA    R8,6(R8)            R8=A(UTL ENTRY)                              
         USING UTLD,R8                                                          
         L     R4,APRTLST          R4=A(LIST OF PRINTER ADDRS)                  
*                                                                               
T22      SR    R5,R5               R5=A(PRINTER QUEUE ENTRY)                    
         ICM   R5,7,TPRNT                                                       
         BZ    T2G                                                              
         USING PRQD,R5                                                          
         XC    PR1PEND,PR1PEND     CLEAR PENDING COUNT                          
         OC    PRCIADDR,PRCIADDR   SET TO 1 IF A PRINTER IS BUSY                
         BZ    *+8                                                              
         MVI   PR1PEND,X'01'       THIS ONE SHOULD BE PRINTING                  
*                                                                               
         TM    PRSTAT,PRSERR       TEST IF POOR LITTLE LOST PRINTER             
         BZ    T23                                                              
         TM    PRSTAT2,PRS2PATH    THIS HAPPENS WHEN ITS LOST ITS PATH          
         BZ    T23                                                              
         GOTO1 VLCM,DMCB,VTGETCID,(R8),0                                        
         OC    12(2,R1),12(R1)                                                  
         BZ    *+10                                                             
         XC    TCID,TCID           CLEAR CID IF NO SESSION ESTABLISHED          
         GOTO1 VLCM,DMCB,VTPRSTRT,(R8),0                                        
         B     T2G                                                              
*                                                                               
T23      TM    PRQMODE,X'80'       IGNORE NON PERM QUEUE PRINTERS               
         BZ    T2E                                                              
         TM    PRSTAT1,PRS1ARS     IGNORE RESTART PENDING PRINTERS              
         BO    T2E                                                              
         CLI   PRQNE,0             IS PRINTER QUEUE ALREADY INITIALIZED         
         BNE   T2E                 YES                                          
*                                                                               
         NI    PRQMODE,X'7F'       TURN OFF PERM ENTRY BIT                      
         L     R3,AIO                                                           
         USING CTTREC,R3           R3=A(TERMINAL RECORD)                        
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,C'T'                                                     
         MVC   CTTKLINE(8),TSYM                                                 
         MVC   KEY(25),CTTKEY                                                   
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),CTFILE,CTTKEY,CTTKEY                
         CLI   DMCB+8,0                                                         
         BNE   T2G                                                              
*                                                                               
T24      LA    R3,CTTDATA          POINT TO FIRST ELEMENT                       
*                                                                               
T25      CLI   0(R3),0             END OF TERMINAL RECORD                       
         BE    T2B                                                              
         CLI   0(R3),X'28'         PRINTER ID ELEMENT                           
         BE    T28                                                              
         CLI   0(R3),X'29'         PRINTER QUEUE ELEMENT                        
         BE    T2A                                                              
T26      ZIC   R1,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R1                                                            
         B     T25                                                              
*                                                                               
         USING CTPRTD,R3           MOVE PRINTER ID TO QUEUE HEADER              
T28      OC    PRID,PRID           DONT SET PRINTER-ID MORE THAN ONCE           
         BNZ   T26                                                              
         MVC   PRID,CTPRTID                                                     
         MVC   PRNUM,CTPRTNUM                                                   
         B     T26                                                              
*                                                                               
         USING CTPRQD,R3           MOVE ENTRY TO PRINTER QUEUE                  
T2A      LA    RF,WORK1            BUILD A PERM QUEUE ENTRY                     
         XC    0(L'PNTRY,RF),0(RF)                                              
         MVC   PNSRCID-PNTRY(L'PNSRCID,RF),CTPRQUSR                             
         MVC   PNSUBID-PNTRY(L'PNSUBID,RF),CTPRQSUB                             
         MVC   PNCLASS-PNTRY(L'PNCLASS,RF),CTPRQCLS                             
         CLI   CTPRQLEN,8          TEST NEW STYLE SHORT ELEMENT                 
         BE    *+10                YES                                          
         MVC   PNCLASS-PNTRY(L'PNCLASS,RF),CTPRQDTA+7                           
         MVI   PNEX-PNTRY(RF),PREXPERM                                          
         CLI   PRQNE,0             IS THIS THE FIRST QUEUE ENTRY                
         BNE   T2A1                NO                                           
         MVC   PNTRY,0(RF)         YES MOVE TO FIRST SLOT                       
         XC    PNLAST,PNLAST                                                    
         B     T2A2                                                             
*                                                                               
T2A1     GOTO1 VLCM,DMCB,VTADDPRQ,(RF),0                                        
         ICM   R1,15,8(R1)                                                      
         BZ    T2C                 PRINTER ENTRY POOL FULL                      
         MVC   FULL(2),PNLAST      SAVE OLD LAST ENTRY NUMBER                   
         STH   R1,PNLAST           R1=NEW LAST ENTRY NUMBER                     
         LA    RE,PNTRY                                                         
         SR    RF,RF                                                            
         ICM   RF,3,FULL                                                        
         BZ    T2A1A               OLD LAST ENTRY WAS THE FIRST                 
         BCTR  RF,0                                                             
         MH    RF,=Y(L'PNTRY)                                                   
         LA    RE,6(RF)                                                         
         A     RE,VPRQENTS         RE=A(OLD LAST ENTRY)                         
T2A1A    STH   R1,PNNEXT-PNTRY(RE)                                              
*                                                                               
T2A2     OI    PRQMODE,X'80'       SET PRINTER MODE TO AUTO                     
         ZIC   RE,PRQNE                                                         
         LA    RE,1(RE)                                                         
         STC   RE,PRQNE            BUMP NUMBER OF PRQ ENTRIES                   
         CLC   PRQNE,PRQNEMAX      TEST IF PRINTER QUEUE FULL                   
         BE    T2C                                                              
         B     T26                                                              
*                                                                               
T2B      L     R3,AIO              GET NEXT PRINTER QUEUE PAGE                  
         USING CTTREC,R3           R3=A(TERMINAL RECORD)                        
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,CTTKEY,CTTKEY                
         CLI   DMCB+8,0                                                         
         BNE   T2C                                                              
         CLC   CTTKEY(CTTKPASS-CTTKEY),KEY                                      
         BNE   T2C                                                              
         CLI   CTTKPASS,C'1'       ENSURE PRINTER QUEUE PAGE RECORD             
         BL    T2B                                                              
         CLI   CTTKPASS+1,C' '                                                  
         BNE   T2B                                                              
         CLC   CTTKPASS+2(L'CTTKPASS-2),CTTKPASS+1                              
         BNE   T2B                                                              
         B     T24                                                              
*                                                                               
T2C      CLI   PRQNE,0             ANY ELEMENTS FOUND                           
         BE    T2G                 NO - IGNORE THIS PRINTER                     
*                                                                               
T2E      ST    R5,0(R4)            SET A(PRQ) IN LIST                           
         LA    R4,4(R4)                                                         
         L     R1,APRTLSTX         TEST FOR TABLE FULL                          
         CR    R4,R1                                                            
         BL    T2G                                                              
         DC    H'0'                PRTLST TOO SMALL                             
*                                                                               
T2G      BXLE  R8,R6,T22           BUMP TO NEXT TERMINAL IN UTL                 
*                                                                               
T2I      L     RE,APRTLST          END OF PRINTER QUEUE                         
         SR    R4,RE                                                            
         SRL   R4,2                                                             
         ST    R4,PRTNUM           SET NUMBER OF PRINTERS IN PRTLST             
         XC    MTCHNUM,MTCHNUM                                                  
         MVI   NOTIFY,0                                                         
*                                                                               
         OC    PRTNUM,PRTNUM       TEST ANY PRINTERS QUALIFY                    
         BNZ   T2NDX               YES - SEARCH PQ INDEX                        
*                                                                               
         OC    JNUM,JNUM           TEST ANY JOBS PENDING NOTIFY                 
         BNZ   T2NDX               YES - SEARCH PQ INDEX                        
*                                                                               
         B     T2UPDT              GO SEARCH FACWRK INDEX                       
         DROP  R5                                                               
         EJECT                                                                  
* READ PRTQ FILES INDICIES AND MATCH REPORTS TO PRINTERS IN PRTLST              
*                                                                               
T2NDX    GOTO1 VDATAMGR,DMCB,(0,GLIST),PRTQUE,NDX,,CXREC                        
         L     RE,NDX+32                                                        
         LA    RE,8(RE)                                                         
         ST    RE,APRTQLST         SAVE ADR OF FIRST PRTQ FILE ENTRY            
         MVC   PRTQID+4(1),1(RE)   SET PRTQ ID FROM LIST ENTRY                  
*                                                                               
T2NDX0   GOTO1 VDATAMGR,DMCB,(0,BUFFER),PRTQID,,,CXREC                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CIDATA,CXREC+12     SET CI DATA FOR THIS PRTQ FILE               
         XC    DUB,DUB             SET TO READ FIRST INDEX PAGE                 
         MVC   DUB(2),EFFS                                                      
         BRAS  RE,CXLOOPI                                                       
         USING PQRECD,R5                                                        
*                                                                               
T2NDX1   BRAS  RE,GETXAD                                                        
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),PRTQID,CXADDR,CXREC                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIE ON FUNNY PRTQUE I/O                      
*                                                                               
T2NDX2   CLI   PQSEQ,1             LOW ORDER CI'S ONLY                          
         BH    T2NDXX                                                           
*&&US                                                                           
T2NDXDAR CLI   PQCLASS,C'N'        DARE - CLASS N ACTIVE REPORT                 
         BNE   T2NDXDAX                                                         
         TM    PQSTAT,PQSTAC                                                    
         BZ    T2NDXDAX                                                         
         CLC   PQSUBID,=C'DAR'     DAR REPORT?                                  
         BNE   T2NDXDA5                                                         
         CLI   SYSID,1             FOR TST SYSTEM?                              
         BE    T2NDXDA2            YES, PUT INTO DARTAB                         
***                                                                             
         CLI   SYSID,6             FOR  MEL  OR  FQA  SYSTEM?                   
         BE    T2NDXDAX                                                         
         CLI   SYSID,15                                                         
         BE    T2NDXDAX            YES, DON'T PROCESS-STOP CONFLICTS            
***                                                                             
         LA    RF,FACIDTAB                                                      
         USING FACITABD,RF                                                      
*                                                                               
T2NDXDA1 CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                THIS BETTER EXIST ON FACIDTAB                
         CLC   FACIID,SYSID                                                     
         BE    *+12                                                             
         LA    RF,L'FACITAB(RF)                                                 
         B     T2NDXDA1                                                         
         TM    FACIFL,FACIREP      ARE WE ON THE REP SYSTEM?                    
         BZ    T2NDXDAX            NO - DONE                                    
         DROP  RF                                                               
*                                                                               
T2NDXDA2 BRAS  RE,XTODTAB          PUT ENTRY INTO DARPQTAB                      
         B     T2NDXDAX                                                         
*                                                                               
T2NDXDA5 DS    0H                                                               
*                                                                               
T2NDXDAX EQU   *                                                                
*&&                                                                             
         TM    MODE,X'01'          TEST EXTENDED CHECKING MODE                  
         BO    T2NDX2B0                                                         
         SPACE 1                                                                
* MODE 0 PROCESS ONLY THOSE REPORTS FLAGGED AS COMPLETED JOBS                   
* MATCH ON PRTQUE KEY TO CORRESPONDING JOB TAB ENTRY                            
*                                                                               
T2NDX2A0 TM    PQATTB,PQATJOBI     IGNORE SCHEDULED JOBS                        
         BO    T2NDXX                                                           
         TM    PQSTAT,PQSTIN       IGNORE INVISIBLE                             
         BO    T2NDXX                                                           
         TM    PQATTB,PQATJOBO     ONLY WANT COMPLETED JOBS                     
         BZ    T2NDX28                                                          
*                                                                               
T2NDX2A1 ICM   R0,15,JNUM          R0=N'JOB NOTIFIES PENDING                    
         BZ    T2NDX28                                                          
         L     R1,AJTAB                                                         
         USING JTABD,R1            R1=A(JOB NOTIFY TABLE)                       
T2NDX2A2 CLC   PQKEY,JPQKEY                                                     
         BE    T2NDX24             PRTQUE KEY MATCHES JOBTAB KEY                
T2NDX2A3 LA    R1,JTABL(R1)                                                     
         BCT   R0,T2NDX2A2                                                      
         B     T2NDX28             THIS COMPLETED JOB NOT IN JOBTAB             
         SPACE 1                                                                
* MODE 1 TEST EVERY PQ REPORT AGAINST EVERY JOB TAB ENTRY                       
* CHECK FOR CHANGE OF KEY IN PRTQUE FROM THAT IN JOB TAB                        
*                                                                               
T2NDX2B0 OC    JNUM,JNUM           TEST IF ANY JOBS TO CHECK                    
         BZ    T2NDX28             NO                                           
         BRAS  RE,GETCAD           YES COMPUTE CIADDR OF REPORT                 
*                                                                               
T2NDX2B1 ICM   R0,15,JNUM          R0=N'JOB NOTIFIES PENDING                    
         BZ    T2NDX28                                                          
         L     R1,AJTAB                                                         
         USING JTABD,R1            R1=A(JOB NOTIFY TABLE)                       
T2NDX2B2 CLC   PRTQID+4(1),JPQID   TEST IF SAME PRTQUE FILE                     
         BNE   T2NDX2B3                                                         
         CLC   CIADDR(2),JPQCIA    TEST IF SAME CIADDR                          
         BNE   T2NDX2B3                                                         
         CLC   PQKEY,JPQKEY        TEST IF KEYS HAVE CHANGED                    
         BNE   T2NDX25                                                          
         TM    PQATTB,PQATJOBI     IGNORE SCHEDULED JOBS                        
         BO    T2NDXX                                                           
         TM    PQSTAT,PQSTIN       IGNORE INVISIBLE                             
         BO    T2NDXX                                                           
         TM    PQATTB,PQATJOBO     TEST IF JOB HAS COMPLETED                    
         BO    T2NDX24                                                          
         B     T2NDX28                                                          
T2NDX2B3 LA    R1,JTABL(R1)                                                     
         BCT   R0,T2NDX2B2                                                      
         B     T2NDX28             THIS REPORT NOT IN JOB TABLE                 
         SPACE 1                                                                
* THIS COMPLETED PRTQUE REPORT MATCHES ENTRY IN JOBTAB                          
*                                                                               
T2NDX24  OI    JSTAT,JSOUT         SET TO NOTIFY THIS JOB HAS COMPLETED         
         LH    RE,OUTCNT           BUMP JOBS OUT COUNTER                        
         LA    RE,1(RE)                                                         
         STH   RE,OUTCNT                                                        
         TM    PQATTB,PQATERR      TEST THIS JOB ABENDED                        
         BZ    *+16                                                             
         OI    JSTAT,JSINV         YES - SET JOB INVALID                        
         OI    NOTIFY,JSOUT                                                     
         B     T2NDX26                                                          
         TM    PQATTB,PQATUSRW     TEST WORKER KEY PRESENT                      
         BO    *+12                                                             
         OI    NOTIFY,JSOUT                                                     
         B     T2NDX26                                                          
         OI    JSTAT,JSUPD         KEY PRESENT - SET UPDATE PENDING             
         OI    NOTIFY,JSUPD                                                     
         B     T2NDX26                                                          
         SPACE 1                                                                
* THIS JOBTAB ENTRY HAS A CHANGE IN THE PRTQUE KEY                              
*                                                                               
T2NDX25  OI    JSTAT,JSOUT         SET TO NOTIFY THIS JOB HAS VANISHED          
         LH    RE,OUTCNT           BUMP JOBS OUT COUNTER                        
         LA    RE,1(RE)                                                         
         STH   RE,OUTCNT                                                        
         OI    NOTIFY,JSOUT+JSNPQ                                               
         OI    JSTAT,JSNPQ         SET JOB NO LONGER IN PRTQUE                  
         B     T2NDX26                                                          
         DROP  R1                                                               
         SPACE 1                                                                
* RECORD THE COMPLETION OF A JOB EVENT IF YOU WANT TO                           
*                                                                               
T2NDX26  DS    0H                                                               
         SPACE 1                                                                
* PROCESS ACTIVE PRTQUE REPORTS AGAINST PRINTER WORK LISTS                      
*                                                                               
T2NDX28  TM    PQATTB,PQATJOBI     IGNORE SCHEDULED JOBS                        
         BO    T2NDXX                                                           
         TM    PQSTAT,PQSTAC       REPORT MUST BE ACTIVE                        
         BZ    T2NDXX                                                           
         TM    PQSTAT,PQSTIN       AND NOT INVISIBLE                            
         BO    T2NDXX                                                           
         TM    PQSTAT,PQSTPG       AND NOT PRINTING                             
         BO    T2NDXX                                                           
         TM    MODE,X'80'          IF MODE NOT 80                               
         BO    T2NDX28A                                                         
         CLC   PQAGELD,DATEC       TODAYS REPORTS ONLY                          
         BNE   T2NDXX                                                           
         CLC   PQAGELT,TIMEC       AND WITHIN LAST HALF HOUR                    
         BL    T2NDXX                                                           
T2NDX28A ICM   R3,15,PRTNUM        R3=NUMBER OF PRINTERS IN PRTLST              
         BZ    T2NDXX                                                           
         L     R4,APRTLST          R4=A(A(PRINTER QUEUE))                       
*                                                                               
T2NDX3   L     R6,0(R4)            R6=A(PRINTER QUEUE)                          
         USING PRQD,R6                                                          
         SR    R7,R7                                                            
         ICM   R7,1,PRQNE          R7=NUMBER OF ENTRIES IN QUEUE                
         BZ    T2NDXW                                                           
         LA    R6,PNTRY            R6=A(NEXT PRQ ENTRY)                         
         USING PNTRY,R6                                                         
         B     T2NDX4A                                                          
*                                                                               
T2NDX4   SR    RF,RF               BUMP TO NEXT ENTRY                           
         ICM   RF,3,PNNEXT                                                      
         BZ    T2NDXW              C'EST LA VIE                                 
         BCTR  RF,0                                                             
         MH    RF,=Y(L'PNTRY)                                                   
         LA    R6,6(RF)                                                         
         A     R6,VPRQENTS         LOCATE PRQ ENTRY                             
T2NDX4A  OC    0(8,R6),0(R6)                                                    
         BZ    T2NDXW              HONI SOI QUI MAL Y PENSE                     
*&&US                                                                           
         CLC   PNSRCID,=X'0406'    GRAFNET ONLY HAS CLASS G REPORTS             
         BNE   *+16                                                             
         CLI   PQCLASS,C'G'                                                     
         BNE   T2NDXV                                                           
         B     T2NDX6                                                           
         CLI   PQCLASS,C'G'        CLASS G RESERVED FOR GRAFNET                 
         BE    T2NDXV                                                           
         CLI   PQCLASS,C'N'        DO NOT PRINT CLASS N REPORTS                 
         BE    T2NDXV                                                           
*&&                                                                             
T2NDX4B  TM    PNSRCID,X'80'       TEST GENERIC USER-ID                         
         BZ    T2NDX4D             NO                                           
         OC    VGENIDS,VGENIDS     TEST A(GENIDS) SET                           
         BNZ   T2NDX4C             YES                                          
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000AFC',0                                     
         MVC   VGENIDS+0(1),4(R1)                                               
         MVC   VGENIDS+1(3),1(R1)                                               
*                                                                               
T2NDX4C  CLI   VGENIDS,X'FF'       TEST VGENIDS FOUND                           
         BE    T2NDX4D             NO                                           
         GOTO1 VGENIDS,DMCB,PNSRCID,VDATAMGR                                    
         BNE   T2NDX4D                                                          
         LM    RE,RF,0(R1)         RE=N'ENTRIES, RF=A(ENTRIES)                  
         CLC   PQSRCID,0(RF)       MATCH SOURCE ID                              
         BE    T2NDX4E                                                          
         LA    RF,2(RF)            BUMP TO NEXT                                 
         BCT   RE,*-14                                                          
         B     T2NDXV                                                           
*                                                                               
T2NDX4D  CLC   PNSRCID,PQSRCID     MATCH SOURCE ID                              
         BNE   T2NDXV                                                           
T2NDX4E  TM    PNCOPYS,PNCENDP     TEST IF SUBID CONTAINS SUBID                 
         BO    T2NDX5                                                           
         CLC   PNSUBID,=C'ALL'     MATCH REPORT SUB-ID                          
         BE    T2NDX5                                                           
         CLI   PNSUBID+1,C'*'      GENERIC SUB-ID X**                           
         BNE   T2NDX4F                                                          
         CLC   PNSUBID(1),PQSUBID  MATCH ON FIRST CHR ONLY                      
         BNE   T2NDXV                                                           
         B     T2NDX5                                                           
T2NDX4F  CLI   PNSUBID+2,C'*'      GENERIC SUB-ID XX*                           
         BNE   T2NDX4G                                                          
         CLC   PNSUBID(2),PQSUBID  MATCH ON FIRST TWO CHRS ONLY                 
         BNE   T2NDXV                                                           
         B     T2NDX5                                                           
T2NDX4G  CLC   PNSUBID,PQSUBID                                                  
         BNE   T2NDXV                                                           
*                                                                               
T2NDX5   CLI   PNCLASS,0           MATCH REPORT CLASS                           
         BE    T2NDX6                                                           
         MVC   FLAG,PNCLASS                                                     
         TM    FLAG,X'40'                                                       
         BZ    T2NDX5A                                                          
         CLC   FLAG,PQCLASS        POSITIVE CLASS FILTER                        
         BNE   T2NDXV                                                           
         B     T2NDX6                                                           
T2NDX5A  OI    FLAG,X'40'                                                       
         CLC   FLAG,PQCLASS        NEGATIVE CLASS FILTER                        
         BE    T2NDXV                                                           
*                                                                               
T2NDX6   TM    PNCOPYS,PNCTIME     TEST IF SEQNUM CONTAINS SEQ NUM              
         BZ    T2NDX6A                                                          
         CLC   PQAGELD,DATEC       BEFORE TODAY ALWAYS QUALIFIES                
         BL    T2NDX7                                                           
         BH    T2NDXV                                                           
         CLC   PQAGELT,PNSEQN                                                   
         BNH   T2NDX7                                                           
         B     T2NDXV                                                           
T2NDX6A  OC    PNSEQN,PNSEQN       TEST ALL SEQUENCE NUMBERS                    
         BZ    T2NDX7                                                           
         CLC   PQREPNO,PNSEQN      MATCH REPORT SEQUENCE NUMBER                 
         BE    T2NDX7                                                           
         B     T2NDXV                                                           
*                                                                               
T2NDX7   OI    0(R4),X'80'         SET PRINTER/REPORT MATCHED                   
         L     R1,0(R4)                                                         
         SR    RE,RE                                                            
         IC    RE,PR1PEND-PRQD(R1)                                              
         LA    RE,1(RE)            BUMP PENDING COUNT                           
         STC   RE,PR1PEND-PRQD(R1)                                              
         TM    PNEX,PREXACTV       IS QUEUE ENTRY ACTIVE                        
         BO    T2NDXW              YES - TRY ANOTHER PRINTER                    
         OI    PNEX,PREXACTV                                                    
         L     RE,MTCHNUM          BUMP PRINTER/REPORT MATCH COUNT              
         LA    RE,1(RE)                                                         
         ST    RE,MTCHNUM                                                       
         B     T2NDXX                                                           
*                                                                               
T2NDXV   BCT   R7,T2NDX4           BUMP TO NEXT PRINTER QUEUE ENTRY             
*                                                                               
T2NDXW   LA    R4,4(R4)            BUMP TO NEXT PRINTER                         
         BCT   R3,T2NDX3                                                        
*                                                                               
T2NDXX   BRAS  RE,CXLOOPX          BUMP TO NEXT REPORT                          
         B     T2NDX2                                                           
         B     T2NDX1              END OF PRTQUE PAGE                           
         ICM   RE,15,APRTQLST                                                   
         BZ    T2STRT                                                           
         LA    RE,8(RE)                                                         
         ST    RE,APRTQLST         BUMP TO NEXT PRTQ FILE                       
         CLI   0(RE),0                                                          
         BE    T2STRT              EXIT IF END OF PRTQ FILE LIST                
         MVC   PRTQID+4(1),1(RE)   SET NEXT PRTQ FILE ID                        
         B     T2NDX0              BACK TO PROCESS NEXT PRTQ FILE               
         DROP  R5,R6                                                            
         EJECT                                                                  
* ALL INDEX RECORDS READ - NOW START PRINTERS THAT HAVE ANY WORK                
*                                                                               
T2STRT   ICM   R3,15,PRTNUM        R3=NUMBER OF PRINTERS IN PRTLST              
         BZ    T2STRTX                                                          
         L     R4,APRTLST          R4=A(A(PRINTER QUEUE))                       
*                                                                               
T2STRT1  TM    0(R4),X'80'         MATCHED PRINTERS ONLY                        
         BZ    T2STRT2                                                          
         L     R6,0(R4)                                                         
         USING PRQD,R6                                                          
         TM    PRQMODE,X'80'       IGNORE NON PERM QUEUE PRINTERS               
         BZ    T2STRT2                                                          
         TM    PRSTAT1,PRS1ARS     IGNORE RESTART PENDING PRINTERS              
         BO    T2STRT2                                                          
         CLI   PRSTAT,0            IGNORE IF PRINTER ALREADY ACTIVE             
         BNE   T2STRT2                                                          
         OC    PRCIADDR,PRCIADDR                                                
         BNZ   T2STRT2                                                          
         MVI   PRSTAT,PRSACTV      SET STATUS TO ACTIVE                         
         MVI   PRSTAT1,PRS1SOS     SET START OF SESSION                         
         SR    R8,R8                                                            
         ICM   R8,7,PRQUTLA        R8=A(PRINTER UTL ENTRY)                      
         BRAS  RE,STRT             PHYSICALLY START PRINTER                     
*                                                                               
         XC    LOGREC,LOGREC       SET TIMER 2 LOG DATA                         
         MVC   LOGID,MYLOGID                                                    
         MVI   LOGID+3,C'U'        $PQU FOR START PRINTER ON POP                
         SR    RE,RE                                                            
         ICM   RE,7,PRQUTLA                                                     
         MVC   LOGLUID,TSYM-UTLD(RE)                                            
         MVC   LOGTIME,TIMEHMS                                                  
         GOTO1 VLOGGER,LOGREC                                                   
*                                                                               
T2STRT2  LA    R4,4(R4)            BUMP TO NEXT PRINTER                         
         BCT   R3,T2STRT1                                                       
T2STRTX  B     T2JOBS                                                           
         DROP  R6                                                               
         EJECT                                                                  
* UPDATE S/R SAVE PAGES FOR JOB NOTIFICATION & SET FLAGS IN UTL                 
*                                                                               
T2JOBS   TM    NOTIFY,JSOUT+JSUPD                                               
         BZ    T2JOBS50                                                         
         L     R6,AJTAB                                                         
         USING JTABD,R6            R6=A(JOB NOTIFY TABLE)                       
         ICM   R3,15,JNUM          R3=N'ENTRIES IN JOB TABLE                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R3,1(R3)            ADD ONE ENTRY FOR E-O-L                      
         XC    DUB(3),DUB          DUB+0(2)=TNUM, DUB+2(1)=STATUS               
*                                                                               
T2JOBS2  CLC   JTERM,DUB           TEST CHANGE OF TERMINAL                      
         BE    T2JOBS14            NO                                           
         TM    DUB+2,TWAR+TWAU     TEST S/R PAGE READ/UPDATED                   
         BZ    T2JOBS12            NOT READ NOR UPDATED                         
         BM    T2JOBS10            READ BUT NOT UPDATED                         
*                                                                               
         LA    RF,SRPAGENO         WRITE BACK UPDATED SAVE PAGE                 
         SLL   RF,32-8                                                          
         ICM   RF,3,DUB                                                         
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,(RF),SRPARA2                         
*                                                                               
         L     RE,VSSB             UPDATE MONSOON COUNTERS                      
         CLI   SSBJESIO-SSBD(RE),C' '                                           
         BNE   T2JOBS6                                                          
         SR    RF,RF               COMPLETED JOB COUNTER                        
         L     R1,SRPARA2                                                       
         USING SRSD,R1             R1=A(S/R TWA SAVE PAGE)                      
         ZIC   RE,SRJOBINQ         RE=N'ENTRIES IN JOB Q                        
         LTR   R0,RE               R0=SAVE IT AROUND                            
         BZ    T2JOBS4                                                          
         LA    R1,SRJOBQ                                                        
         USING SRJOBQ,R1           R1=A(JOB Q)                                  
         TM    SRJOBSTA,SRJOBOUT+SRJOBNOT+SRJOBINV                              
         BZ    *+8                                                              
         LA    RF,1(RF)            INCREMENT COMPLETED JOB COUNTER              
         LA    R1,SRJOBQLN(R1)                                                  
         BCT   RE,*-16                                                          
         DROP  R1                                                               
*                                                                               
T2JOBS4  SR    R0,RF               R0 = NUMBER OF USED ENTRIES                  
         L     R1,VUTL             UPDATE UTL ENTRY                             
         LH    RF,0(R1)                                                         
         SR    RE,RE                                                            
         ICM   RE,3,DUB                                                         
         BCTR  RE,0                                                             
         MR    RE,RE                                                            
         LA    R1,6(R1,RF)                                                      
         STC   R0,TJOBSINQ-UTLD(R1)                                             
*                                                                               
T2JOBS6  L     R1,SRPARA2                                                       
         USING SRSD,R1             R1=A(S/R TWA SAVE PAGE)                      
         SR    R0,R0                                                            
         ICM   R0,1,SRJOBINQ       R0=N'ENTRIES IN JOB Q                        
         BZ    T2JOBS8                                                          
         LA    R1,SRJOBQ                                                        
         USING SRJOBQ,R1           R1=A(JOB Q)                                  
         TM    SRJOBSTA,SRJOBPUT+SRJOBOUT                                       
         BNO   T2JOBS8                                                          
         LA    R1,SRJOBQLN(R1)                                                  
         BCT   R0,*-12                                                          
         OI    DUB+2,ALLD          SET ALL JOBS COMPLETED                       
         DROP  R1                                                               
*                                                                               
T2JOBS8  L     R1,VUTL             UPDATE UTL ENTRY                             
         LH    RF,0(R1)                                                         
         SR    RE,RE                                                            
         ICM   RE,3,DUB                                                         
         BCTR  RE,0                                                             
         MR    RE,RE                                                            
         LA    R1,6(R1,RF)                                                      
         TM    DUB+2,ALLD          TEST ALL JOBS COMPLETE                       
         BZ    *+8                                                              
         NI    TJOBFLAG-UTLD(R1),255-TJOBFSUB                                   
         TM    DUB+2,NOTP          TEST NOTIFY PENDING                          
         BZ    *+8                                                              
         OI    TJOBFLAG-UTLD(R1),TJOBFOUT                                       
         TM    DUB+2,EXTP          TEST EXTRACT UPDATE PENDING                  
         BZ    T2JOBS10                                                         
         OI    TJOBFLAG-UTLD(R1),TJOBFEXT                                       
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         OI    SSBJFLAG,SSBJFEXT   SET SYSTEM EXTRACT UPDATE PENDING            
         DROP  RE                                                               
*                                                                               
T2JOBS10 GOTO1 VDATAMGR,DMCB,DMUNLK,TEMPSTR                                     
*                                                                               
T2JOBS12 MVC   DUB(2),JTERM        SET CURRENT TERMINAL NUMBER                  
         MVI   DUB+2,0             SET PAGE NOT READ/UPDATED                    
*                                                                               
T2JOBS14 TM    JSTAT,JSOUT+JSUPD                                                
         BZ    T2JOBS30                                                         
*&&NOP                                                                          
         L     R2,VSSB                                                          
         LAM   R2,R2,SSBTBLET-SSBD(R2)                                          
         SAC   512                                                              
         L     R2,AJOBTAB          LOCATE & FREE JOBTAB ENTRY FOR JOB           
         ICM   R0,15,0(R2)                                                      
         BZ    T2JOBS17                                                         
         AH    R2,JOBHDRL                                                       
         USING TBJOBTAB,R2         R1=A(JOB TABL                                
T2JOBS15 OC    TBJNTRY(4),TBJNTRY  TEST USED ENTRY                              
         BNZ   *+12                                                             
         AH    R2,JOBTABL                                                       
         B     T2JOBS15                                                         
*                                                                               
         CLC   TBJTERM,JTERM       MATCH ON KEY VALUES                          
         BNE   T2JOBS16                                                         
         CLC   TBJPQKEY(7),JPQKEY                                               
         BNE   T2JOBS16                                                         
         MVI   TBJSTAT,0           FREE JOBTAB ENTRY                            
         L     R1,VSSB                                                          
         OI    SSBSTAT1-SSBD(R1),SSBSCHK1                                       
         B     T2JOBS17                                                         
*                                                                               
T2JOBS16 AH    R2,JOBTABL                                                       
         BCT   R0,T2JOBS15                                                      
*&&                                                                             
*                                                                               
T2JOBS17 SAC   0                                                                
         TM    DUB+2,TWAR          TEST SAVE PAGE IN CORE                       
         BNZ   T2JOBS18            YES                                          
         LA    RF,SRPAGENO         READ S/R SAVE PAGE                           
         SLL   RF,32-8                                                          
         ICM   RF,3,DUB            RF=PAGE/TERMINAL                             
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,(RF),SRPARA2                
         OI    DUB+2,TWAR          SET SAVE PAGE IN CORE                        
*                                                                               
T2JOBS18 L     R1,SRPARA2                                                       
         USING SRSD,R1             R1=A(S/R TWA SAVE PAGE)                      
         SR    R0,R0                                                            
         ICM   R0,1,SRJOBINQ       R0=N'ENTRIES IN JOB Q                        
         BZ    T2JOBS30                                                         
         LA    R1,SRJOBQ                                                        
         USING SRJOBQ,R1           R1=A(JOB Q)                                  
*                                                                               
T2JOBS19 CLC   JPQUSR(JPQCIA-JPQUSR),SRJOBUSR                                   
         BNE   T2JOBS28                                                         
         TM    SRJOBSTA,SRJOBOUT   TEST COMPLETED                               
         BNZ   T2JOBS28                                                         
         OI    DUB+2,TWAU          SET SAVE PAGE UPDATED                        
         TM    JSTAT,JSINV         TEST REPORT IS INVALID                       
         BZ    T2JOBS20                                                         
         OI    SRJOBSTA,SRJOBOUT+SRJOBINV                                       
         OI    DUB+2,NOTP          SET JOB NOTIFY PENDING                       
         B     T2JOBS30                                                         
*                                                                               
T2JOBS20 TM    JSTAT,JSUPD         TEST EXTRACT UPDATE PENDING                  
         BNZ   T2JOBS22                                                         
         OI    SRJOBSTA,SRJOBOUT                                                
         OI    DUB+2,NOTP          SET JOB NOTIFY PENDING                       
         B     T2JOBS30                                                         
*                                                                               
T2JOBS22 L     R4,SRPARA2          CREATE EXTRACT UPDATE Q ENTRY                
         LA    R4,SRUPDQ-SRSD(R4)                                               
         USING SRUPDQ,R4           R4=A(EXTRACT UPDATE QUEUE)                   
         LA    R5,SRUPDMAX         R5=NUMBER OF QUEUE ENTRIES                   
T2JOBS24 CLI   SRUPDSTA,SRUPDAVA   FIND A FREE SRUPDQ ENTRY                     
         BE    T2JOBS26                                                         
         CLI   SRUPDSTA,SRUPDERR   ERROR ENTRIES CAN BE RE-USED                 
         BE    T2JOBS26                                                         
         LA    R4,SRUPDQLN(R4)                                                  
         BCT   R5,T2JOBS24                                                      
         B     T2JOBS30            NO SLOTS AVAILABLE - DO NEXT TIME            
*                                                                               
T2JOBS26 XC    SRUPDQ(SRUPDQLN),SRUPDQ                                          
         LR    R0,R1               SAVE R1 (JOB Q ENTRY ADDRESS)                
         MVC   CIADDR+0(2),SRJOBCIA                                             
         MVC   CIADDR+2(2),=X'0100'                                             
         XC    NDX,NDX             FIND WHICH PRTQ FILE FROM USER ID            
         MVC   NDX(2),SRJOBUSR                                                  
         GOTO1 VDATAMGR,DMCB,GFILE,PRTQID,NDX,,ACIREC                           
         MVC   PRTQID,NDX+32                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,PRTQID,CIADDR,ACIREC                        
         CLI   8(R1),0             TEST FOR ERRORS                              
         BNE   T2JOBS30                                                         
         LR    R1,R0               RESTORE JOB Q POINTER                        
         L     RF,ACIREC                                                        
         MVC   SRUPDUSR,PQSRCID-PQRECD(RF)                                      
         MVC   SRUPDKEY,PQUSRINF-PQRECD(RF)                                     
         MVC   SRUPDCIA,SRJOBCIA   SET REPORT C/I ADDRESS                       
         OI    SRJOBSTA,SRJOBUPD   SET UPDATE PENDING IN JOB Q                  
         MVI   SRUPDSTA,SRUPDSCH   SET UPDATE PENDING IN UPDATE Q               
         OI    DUB+2,EXTP          SET UPDATE PENDING FOR TERMINAL              
         B     T2JOBS30                                                         
*                                                                               
T2JOBS28 LA    R1,SRJOBQLN(R1)     BUMP TO NEXT QUEUE ENTRY                     
         BCT   R0,T2JOBS19                                                      
*                                                                               
T2JOBS30 LA    R6,JTABL(R6)        BUMP TO NEXT JOB ENTRY                       
         BCT   R3,T2JOBS2                                                       
         EJECT                                                                  
*****************************************************************               
* CHECK JOBTAB FOR FREE ENTRIES. FILE AND REMOVE + UPDATE COUNT *               
*****************************************************************               
         SPACE 1                                                                
T2JOBS50 L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         CLI   SSBPGMUP,C'Y'       ONLY ONE SYSTEM NEED DO THIS                 
         BNE   T2UPDT                                                           
*                                                                               
         GOTO1 VLOCKSPC,DMCB,X'00008005',0   LOCK JOBTAB                        
*                                                                               
         L     R2,4(R1)            GET A(JOBTAB HEADER)                         
         L     R1,12(R2)                                                        
         ST    R1,FULL             SAVE A(END)                                  
         L     R2,60(R2)           GET A(JOB TABLE)                             
         ST    R2,FUL1                                                          
*                                                                               
         L     RF,VSSB                                                          
         LAM   R2,R2,SSBTBLET-SSBD(RF)                                          
         TIME  BIN                                                              
         SAC   512                                                              
*                                                                               
         S     R0,TBJTIME-TBJOBHDR(,R2)                                         
         CH    R0,=H'30000'        5 MINS IN 100TH SEC                          
         BNL   T2JOBS60            IF NOT CLEANED UP GO DO IT                   
         XC    FUL1,FUL1                                                        
         B     T2JOBS69            OTHERWISE RCVR JOB HAS DONE IT               
         EJECT                                                                  
*******************************************************************             
* NOW SCAN,LOG,UPDATE CLASS AND REMOVE JOB ENTRIES                *             
*******************************************************************             
         SPACE 1                                                                
         USING TBJOBTAB,R2                                                      
T2JOBS60 L     R2,FUL1             GET A(JOB TABLE)                             
         SR    RE,RE                                                            
         ICM   R0,15,0(R2)         SET R0 TO NUMBER OF ENTRIES                  
         BZ    T2JOBS66                                                         
         AH    R2,JOBHDRL          BUMP PAST HEADER                             
T2JOBS62 CLI   TBJSTAT,X'FF'       TEST FOR NOT FOUND                           
         BNE   *+14                                                             
         OC    TBJETIME,TBJETIME   CLEAR IF JOB RAN TO COMPLETION               
         BNZ   *+12                                                             
         CLI   TBJSTAT,0           TEST FOR CLEARED ENTRY                       
         BNE   T2JOBS64                                                         
         OC    0(4,R2),0(R2)                                                    
         BNZ   T2JOBS63                                                         
         AH    R2,JOBTABL          BUMP TO NEXT                                 
         C     R2,FULL                                                          
         BL    T2JOBS62                                                         
         B     T2JOBSXX                                                         
*                                                                               
T2JOBS63 ST    RE,DUB                                                           
         MVC   WORK+1(48),TBJNTRY                                               
         MVI   WORK,49                                                          
         SAC   0                                                                
         GOTO1 VTEMPTRC,DMCB,TSOONTRC,WORK                                      
         SAC   512                                                              
         L     RE,DUB                                                           
*                                                                               
T2JOBS70 ST    R2,DUB                                                           
         B     T2JOBS79                                                         
         MVC   HALF,0(R2)          PUT CLASS INTO HALF                          
         ST    R2,DUB              SAVE R2                                      
         L     R2,AJCLASS          PICK UP CLASS TABLE                          
         LA    R1,16                                                            
T2JOBS71 CLC   HALF,0(R2)          IS THIS MY CLASS                             
         BE    T2JOBS72                                                         
         OC    0(2,R2),0(R2)       OR EOT                                       
         BZ    T2JOBS72                                                         
         LA    R2,16(,R2)          TRY NEXT                                     
         BCT   R1,T2JOBS71                                                      
         B     T2JOBS79            CLASS TABLE FULL (DON'T DIE)                 
*                                                                               
T2JOBS72 MVC   0(2,R2),HALF        CONFIRM OR SET CLASS ENTRY                   
         IC    R1,15(,R2)                                                       
         LA    R1,1(R1)            BUMP CURRENT BY 1                            
         STC   R1,15(,R2)                                                       
*                                                                               
T2JOBS79 L     R2,DUB              RESTORE R2                                   
*                                                                               
         LH    R1,JOBTABL          CLEAR JOB ENTRY                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    TBJNTRY(0),TBJNTRY                                               
         AH    R2,JOBTABL          BUMP TO NEXT                                 
         B     T2JOBS65                                                         
*                                                                               
T2JOBS64 AH    R2,JOBTABL          BUMP TO NEXT                                 
         LA    RE,1(RE)                                                         
T2JOBS65 BCT   R0,T2JOBS62                                                      
*                                                                               
T2JOBSXX L     R2,FUL1                                                          
         ST    RE,0(,R2)           SAVE NEW COUNT                               
T2JOBS66 ST    R0,FUL1             R0 SHOULD BE ZERO                            
*                                                                               
T2JOBS69 SAC   0                                                                
         GOTO1 VLOCKSPC,DMCB,X'10008005',0   UNLOCK JOBTAB                      
*                                                                               
         OC    FUL1,FUL1           WAS THE COUNTER CORRECT                      
         BZ    T2JOBS99                                                         
         WTO   '*FACPAK* =RUN COUNTER WAS RESET '                               
*                                                                               
T2JOBS99 EQU   *                                                                
*NOP     BRAS  RE,BLDQUE           <--MAKE THIS EXTERNAL                        
*                                                                               
         B     T2UPDT                                                           
         EJECT                                                                  
**********************************************************************          
* SCAN FACWRK TO SEE IF ANY ACTIVE RECOVERY FILES FOR UPDATE PENDING *          
**********************************************************************          
         SPACE 1                                                                
T2UPDT   GOTO1 VDATAMGR,DMCB,(X'00',=C'DTFAD'),=C'FACWRK'                       
         L     RE,DMCB+12          A(DTF)                                       
         TM    36(RE),X'40'        IS FACWRK FILE NO-OP?                        
         BO    T2UPDTX             YES -- CAN'T READ IT                         
*                                                                               
         L     RE,VSSB             UPDATES ENABLED & NONE IN PROGRESS?          
         TM    SSBJFLAG-SSBD(RE),SSBJFWKR+SSBJFNUP                              
         BNZ   T2UPDTX             WAIT UNTIL LAST UPDATE DONE                  
         LA    R2,FWNDX                                                         
         USING UKRECD,R2           SET TO READ FACWRK INDEX                     
         XC    UKINDEX,UKINDEX                                                  
         LA    RE,FACWRK           SET SPECIAL DMCB FOR FACWRK I/OS             
         ST    RE,FWAFILE                                                       
         ST    R2,FWANDX                                                        
         LH    R3,=Y(FWREC-WORKD)                                               
         LA    R3,WORKD(R3)                                                     
         ST    R3,FWAREC                                                        
         USING FWRECD,R3                                                        
         LH    R4,=Y(FWBUF-WORKD)                                               
         LA    R4,WORKD(R4)                                                     
         ST    R4,FWABUF                                                        
         USING WKRECD,R4                                                        
         GOTO1 VDATAMGR,FWDMCB,(X'00',=C'BUF')                                  
*                                                                               
T2UPDT1  GOTO1 VDATAMGR,FWDMCB,(X'08',=C'IND')                                  
         CLI   8(R1),0                                                          
         BE    T2UPDT2                                                          
         TM    8(R1),X'80'         TEST END OF INDEX                            
         BO    T2UPDTX                                                          
         DC    H'0'                DIE IF DISK ERROR                            
*                                                                               
T2UPDT2  TM    UKSTAT,WKSTAC       FILE MUST BE ACTIVE                          
         BZ    T2UPDT3                                                          
         TM    UKSTAT,WKSTKE       AND NOT KEEP                                 
         BO    T2UPDT1                                                          
         CLI   UKCLASS,C'R'        FILE MUST HAVE CORRECT CLASS                 
         BNE   T2UPDT1                                                          
         CLI   UKSUBPRG,C' '       TEST FACPAK ID IN KEY                        
         BNH   T2UPDT4             NO                                           
         L     RE,VSSB             YES MUST MATCH ON FACPAK ID                  
         CLC   UKSUBPRG,SSBSYSN1-SSBD(RE)                                       
         BNE   T2UPDT1                                                          
         B     T2UPDT4                                                          
*                                                                               
T2UPDT3  TM    UKSTAT,WKSTHO       LOOK FOR HOLD FILES THAT DIDNT GET           
         BZ    T2UPDT1             PROCESSED. WE DONT KNOW WHY !!               
         TM    UKSTAT,WKSTKE       TEST IF HOLD AND NOT KEEP                    
         BO    T2UPDT1                                                          
         CLI   UKCLASS,C'R'        FILE MUST HAVE CORRECT CLASS                 
         BNE   T2UPDT1                                                          
         CLI   UKSUBPRG,C' '       TEST FACPAK ID IN KEY                        
         BNH   T2UPDT1             NO                                           
         L     RE,VSSB             YES MUST MATCH ON FACPAK ID                  
         CLC   UKSUBPRG,SSBSYSN1-SSBD(RE)                                       
         BNE   T2UPDT1                                                          
         L     RE,VSSB             FLAG UPDATE PENDING                          
         OI    SSBJFLAG-SSBD(RE),SSBJFWKR                                       
         B     T2UPDTX             EXIT WITH FLAG SET TO GET GOING              
*                                                                               
T2UPDT4  GOTO1 VDATAMGR,FWDMCB,(X'00',=C'HOL')                                  
         L     RE,VSSB             FLAG UPDATE PENDING                          
         OI    SSBJFLAG-SSBD(RE),SSBJFWKR                                       
*                                                                               
T2UPDTX  DS    0H                                                               
         EJECT                                                                  
*************************************************************                   
*        SCAN WRKF FOR ACTIVE SCRIPT FILES                  *                   
*************************************************************                   
         SPACE 1                                                                
WRKF000  EQU   *                                                                
         LA    R1,ERLYSTRT                    START/END PAIR                    
         L     RE,VSSB                                                          
*&&US                                                                           
         CLI   SSBDAYNO-SSBD(RE),6            TEST SAT/SUN                      
         BH    WRKFXXX                        SUNDAY - EXIT                     
         BNE   *+8                                                              
         LA    R1,ERLYSTR6                                                      
*&&                                                                             
         CLC   SSBTPOPT-SSBD(4,RE),4(R1)      NO SCRIPTS > LATE STOP            
         BH    WRKFXXX                                                          
         CLC   SSBTPOPT-SSBD(4,RE),0(R1)      NO SCRIPTS < EARLY START          
         BL    WRKFXXX                                                          
         TM    SSBSTAT1-SSBD(RE),SSBUII       NO SCAN IF SSB STOP               
         BO    WRKFXXX                                                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',=C'DTFAD'),=C'WRKF1'                        
         L     RE,DMCB+12                                                       
         TM    36(RE),X'40'        IS FIRST WRKF FILE NOP?                      
         BO    WRKFXXX             YES -- CAN'T READ IT                         
*                                                                               
         LA    R2,WORK1            SCAN WRKF FOR ACTIVE SCRIPT FILES            
         USING WSBLOCKD,R2                                                      
         XC    WSBLOCK,WSBLOCK                                                  
         MVI   WSFILE,X'FF'        ALL FILES                                    
         MVI   WSSORT,2            SORT BY CREATION                             
         MVI   WSFLAGS,WSFXUSER    IGNORE USER IN SORT                          
         OI    WSFLAGS,WSFREFNO    RETURN REF# IN CIADDR                        
         MVI   WSSTAT,X'80'        FIND ACTIVE FILES                            
         MVI   WSSTATN,X'23'       EXCLUDE TEMP AND PROCESSED FILES             
         MVI   WSTYPE,C'A'         FIND TYPE A (AUTO SCRIPT)                    
         MVC   WSSMAX,=PL2'100'    SET MAX 100 ENTRIES                          
*                                                                               
         L     RE,VSSB                                                          
         MVC   WSSUBPRG,SSBSYSN1-SSBD(RE)    SET SYSTEM 1 CHR NAME              
*                                                                               
         LA    R1,LATERUN                                                       
         CLI   SSBDAYNO-SSBD(RE),6   TEST SATURDAY                              
         BH    WRKF010                                                          
         BNE   *+8                                                              
         LA    R1,LATERUN6           SATURDAY LATERUN TIME                      
*                                                                               
         CLC   SSBTPOPT-SSBD(4,RE),0(R1)     LATERUN=5 O CLOCK                  
         BL    *+8                                                              
         MVI   WSTYPE+1,C'L'       ALSO FIND TYPE L (LATE AUTO SCRIPT)          
*                                                                               
WRKF010  GOTO1 =V(WKSCAN),DMCB,(R2),ACIREC,AIO,CXREC,SRPARA4,RR=RELO            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO              POINT TO TABLE                               
         USING WSEDATAD,R2                                                      
MAIN010  CLC   WSEDATA,EFFS        TEST FOR END OF TABLE                        
         BE    WRKFXXX                                                          
         L     RE,VSSB             FLAG SCRIPT PENDING                          
         OI    SSBJFLG2-SSBD(RE),SSBJFSCP                                       
*                                                                               
WRKFXXX  B     T3                                                               
         SPACE 1                                                                
*===============================================================*               
* START TIMES MUST BE 4 BYTES FOLLOWED BY END TIMES             *               
*===============================================================*               
*&&US                                                                           
ERLYSTRT DC    PL4'023000'         DO NOT RUN BEFORE 08.30 AM                   
LATESTOP DC    PL4'151500'         DO NOT RUN AFTER 9.15 PM                     
*                                                                               
ERLYSTR6 DC    PL4'044500'         DO NOT RUN BEFORE 10.45 AM SAT               
LATESTP6 DC    PL4'110000'         DO NOT RUN AFTER 5PM SAT                     
*                                                                               
LATERUN  DC    PL4'110000'         START TIME FOR LATE SCRIPTS                  
LATERUN6 DC    PL4'090000'         START TIME FOR LATE SATURDAY                 
*&&                                                                             
*                                                                               
*&&UK                                                                           
ERLYSTRT DC    PL4'073000'         DO NOT RUN BEFORE 07.30 AM                   
LATESTOP DC    PL4'230000'         DO NOT RUN AFTER 11PM                        
*                                                                               
LATERUN  DC    PL4'170000'         START TIME FOR LATE SCRIPTS                  
LATERUN6 DC    PL4'170000'         START TIME FOR LATE SATURDAY                 
*&&                                                                             
         EJECT                                                                  
*                                                                               
* OTHER TIMER SUPPORT                                                           
*                                                                               
T3       B     TIMEXIT                                                          
*                                                                               
TIMX     XIT1  ,                                                                
*                                                                               
TIMEXIT  L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         NI    SSBSTAT3,255-SSBSRTIM                                            
         DROP  RE                                                               
         XMOD1                                                                  
         EJECT                                                                  
* RESTART ALL PRINTERS WITH AUTO RESTART PENDING                                
* LINE MUST BE UP AND OPEN AND REPORT AVAIL FOR PRINTING                        
*                                                                               
RSTRPRNS NTR1  ,                                                                
         MVI   RSTRDONE,C'Y'       SET RESTART DONE                             
         L     RE,VSSB             AOR HAS NO PRINT QUEUES                      
         TM    SSBSTAT4-SSBD(RE),SSBSAOR                                        
         BO    RSTRX                                                            
*                                                                               
RSTR1    L     R8,VUTL             SEARCH UTL FOR RESTARTABLE PRINTERS          
         LH    R6,0(R8)                                                         
         L     R7,2(R8)                                                         
         LA    R8,6(R8)                                                         
         USING UTLD,R8             R8=A(TERMINAL UTL ENTRY)                     
         SR    R1,R1               SET RESTART PENDING COUNT                    
         L     R4,APRTLST          R4=A(LIST OF RESTARTABLE PRINTERS)           
*                                                                               
RSTR1A   SR    R5,R5               R5=A(PRINTER QUEUE ENTRY)                    
         ICM   R5,7,TPRNT                                                       
         BZ    RSTR1E                                                           
         USING PRQD,R5                                                          
         TM    PRSTAT1,PRS1ARS     TEST RESTART PENDING                         
         BZ    RSTR1E                                                           
         LA    R1,1(R1)            BUMP RESTART PENDING COUNT                   
         CLI   PRSTAT,0            TEST PRINTER STILL INACTIVE                  
         BNE   RSTR1E                                                           
         OC    PRCIADDR,PRCIADDR                                                
         BNZ   RSTR1E                                                           
         ST    R5,0(R4)            SET A(PRQ) IN LIST                           
         LA    R4,4(R4)                                                         
*                                                                               
RSTR1E   BXLE  R8,R6,RSTR1A        BUMP TO NEXT TERMINAL IN UTL                 
*                                                                               
RSTR1F   L     RE,APRTLST          END OF PRINTER QUEUES                        
         SR    R4,RE                                                            
         SRL   R4,2                                                             
         ST    R4,PRTNUM           SET NUMBER OF PRINTERS IN PRTLST             
         LTR   R4,R4                                                            
         BNZ   RSTR2               CONTINUE IF PRINTERS IN PRTLST               
         LTR   R1,R1                                                            
         BP    RSTRX               EXIT IF RESTARTS STILL PENDING               
         L     RE,VSSB                                                          
         OI    SSBSTAT1-SSBD(RE),SSBSRSPR                                       
         B     RSTRX               SET ALL RESTARTS DONE AND EXIT               
         SPACE 2                                                                
RSTR2    GOTO1 VDATAMGR,DMCB,(0,GLIST),PRTQUE,NDX,,CXREC                        
         L     RE,NDX+32                                                        
         LA    RE,8(RE)                                                         
         ST    RE,APRTQLST         SAVE ADR OF FIRST PRTQ FILE ENTRY            
         MVC   PRTQID+4(1),1(RE)   SET PRTQ ID FROM LIST ENTRY                  
*                                                                               
RSTR2A   GOTO1 VDATAMGR,DMCB,(0,BUFFER),PRTQID,,,CXREC                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CIDATA,CXREC+12     SET CI DATA FOR THIS PRTQ FILE               
         MVI   FLAG,0                                                           
         TM    CIRSNF,CIXDSPQ                                                   
         BZ    *+8                                                              
         OI    FLAG,X'80'          SET INDEX RECORDS IN DATA SPACE              
*                                                                               
RSTR2B   BRAS  RE,PQLOCK           ENQUEUE PRTQ FILE                            
         USING PQRECD,R5                                                        
         MVC   CXADDR,=X'00010100'                                              
         MVC   FULL,CXADDR                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),PRTQID,CXADDR,CXREC                 
         CLI   8(R1),0                                                          
         BNE   RSTRERR                                                          
         NI    FLAG,X'FE'          TURN OFF PAGE UPDATED FLAG                   
         L     R3,PRTNUM           R3=NUMBER OF PRINTERS IN PRTLST              
         L     R4,APRTLST          R4=A(PRTLST ENTRY)                           
*                                                                               
RSTR2C   L     R6,0(R4)            R6=A(PRINTER QUEUE)                          
         USING PRQD,R6                                                          
         CLC   PRTQID+4(1),PRPRTQA TEST IF SAME PRTQ FILE AS THIS ONE           
         BNE   RSTR2G              NO IGNORE THIS TIME                          
         MVC   CIADDR,PR1CIFST                                                  
         BRAS  RE,GETXPE           GET INDEX PAGE/ENTRY FOR REPORT              
         BRAS  RE,GETXAD           GET INDEX DISK ADDR                          
         CLC   CXADDR,FULL                                                      
         BE    RSTR2E              INDEX REC ALREADY IN CORE                    
*                                                                               
RSTR2D   TM    FLAG,X'01'          TEST IF INDEX PAGE UPDATED                   
         BZ    RSTR2D1                                                          
         GOTO1 VDATAMGR,DMCB,(X'00',DMWRT),PRTQID,FULL,CXREC                    
         CLI   8(R1),0                                                          
         BNE   RSTRERR                                                          
RSTR2D1  LTR   R3,R3               TEST IF FINAL WRITE                          
         BE    RSTR2H                                                           
         MVC   FULL,CXADDR                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),PRTQID,CXADDR,CXREC                 
         CLI   8(R1),0                                                          
         BNE   RSTRERR                                                          
         NI    FLAG,X'FE'          SET INDEX PAGE NOT UPDATED                   
*                                                                               
RSTR2E   LH    R5,CXENTRY          POINT TO INDEX ENTRY FOR REPORT              
         MH    R5,CINDXLN                                                       
         LA    R5,CXREC(R5)                                                     
         CLC   PR1KEY,PQKEY        TEST IF KEYS MATCH                           
         BNE   RSTR2G                                                           
         TM    PQSTAT,PQSTAC       TEST IF REPORT ACTIVE                        
         BZ    RSTR2G                                                           
         TM    PQSTAT,PQSTIN       TEST IF REPORT INVISIBLE                     
         BO    RSTR2G                                                           
         TM    PQSTAT,PQSTPG       TEST IF ALREADY PRINTING                     
         BO    RSTR2G                                                           
*                                                                               
RSTR2F   OI    PQSTAT,PQSTPG       SET REPORT PRINTING                          
         MVI   PQAGERT,X'FE'       SET VALUE TO SHOW SYSID IN INDEX             
         SR    R8,R8                                                            
         ICM   R8,7,PRQUTLA                                                     
         MVC   PQAGEDD(2),TNUM     SET SYSID/TRMNUM IN INDEX                    
         OC    PQAGEDD(1),SYSIDHOB                                              
         OI    0(R4),X'80'         SET THIS PRINTER OK TO START                 
         TM    FLAG,X'80'          TEST IF INDEX PAGE IN DATA SPACE             
         BO    *+12                YES                                          
         OI    FLAG,X'01'          NO SET INDEX PAGE UPDATED                    
         B     RSTR2G                                                           
         LH    RF,CXENTRY          SET RF=DISP TO INDEX ENTRY                   
         MH    RF,CINDXLN                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMWRT),PRTQID,FULL,CXREC,(RF)               
         CLI   8(R1),0                                                          
         BNE   RSTRERR                                                          
         NI    FLAG,X'FE'          SET INDEX PAGE NOT UPDATED                   
*                                                                               
RSTR2G   LA    R4,4(R4)            BUMP TO NEXT PRINTER LIST ENTRY              
         BCT   R3,RSTR2C                                                        
         B     RSTR2D              BACK TO WRITE FINAL INDEX PAGE               
*                                                                               
RSTR2H   GOTO1 VDATAMGR,DMCB,DMUNLK,PRTQID                                      
         BRAS  RE,PQUNLK           DEQUEUE PRTQ FILE                            
         L     RE,APRTQLST                                                      
         LA    RE,8(RE)            BUMP TO NEXT PRTQ FILE                       
         ST    RE,APRTQLST                                                      
         CLI   0(RE),0             END OF LIST                                  
         BE    RSTR3                                                            
         MVC   PRTQID+4(1),1(RE)                                                
         B     RSTR2A              BACK TO PROCESS NEXT PRTQ FILE               
*                                                                               
RSTRERR  GOTO1 VDATAMGR,DMCB,DMUNLK,PRTQID                                      
         BRAS  RE,PQUNLK           DEQUEUE PRTQ FILE ON ERROR                   
         B     RSTRX                                                            
         SPACE 2                                                                
RSTR3    L     R3,PRTNUM           R3=NUMBER OF PRINTERS IN PRTLST              
         L     R4,APRTLST          R4=A(PRTLST ENTRY)                           
*                                                                               
RSTR3A   TM    0(R4),X'80'         PRINTER OK TO START                          
         BZ    RSTR3B                                                           
         L     R6,0(R4)            R6=A(PRINTER QUEUE)                          
         LA    R6,0(R6)                                                         
         MVI   PRSTAT,PRSACTV                                                   
         SR    R8,R8                                                            
         ICM   R8,7,PRQUTLA        R8=A(PRINTER UTL ENTRY)                      
         BRAS  RE,STRT             PHYSICALLY START PRINTER                     
         B     RSTR3C                                                           
*                                                                               
RSTR3B   L     R6,0(R4)            REPORT NOT AVAIL FOR RESTART                 
         MVI   PRSTAT1,0                                                        
         XC    PRHDR1,PRHDR1       RESET AUTO RESTART PENDING                   
         B     RSTR3C                                                           
*                                                                               
RSTR3C   LA    R4,4(R4)            BUMP TO NEXT PRINTER IN LIST                 
         BCT   R3,RSTR3A                                                        
*                                                                               
RSTRX    B     TIMX                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
* PHYSICALLY START PRINTER DEVICE DEFINED BY UTL ENTRY IN R8                    
*                                                                               
STRT     NTR1  ,                                                                
         LA    R5,VTPRSTRT         CALL LCM TO START PRINTER                    
         GOTO1 VLCM,DMCB,(R5),(R8)                                              
STRTX    B     TIMX                                                             
         DROP  R8                                                               
         EJECT                                                                  
* DMPRTQR                                                                       
       ++INCLUDE DMPRTQR                                                        
         EJECT                                                                  
* READ GENDIR TO SEE IF ANY BRDCST MESSAGES SINCE LAST TIME                     
* IF ANY NEW MESSAGES MOVE TO BCTAB                                             
*                                                                               
BLDBC    NTR1  ,                                                                
         MVI   NEWBC,0             READ GENDIR BROADCAST MESSAGES               
         L     RE,VSSB                                                          
         ICM   R5,15,SSBABC-SSBD(RE)                                            
         ST    R5,ABCTAB                                                        
         BZ    BLDBCX              R5=A(BROADCAST TABLE)                        
         LA    R5,6(R5)                                                         
         USING BCTABD,R5                                                        
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         TIME  BIN                                                              
         A     R0,TIMEADJ                                                       
         STM   R0,R1,DUB           R0=TIME(BIN 1/100 SECS),R1=DATE              
         SR    R0,R0                                                            
         ICM   R0,3,DATEC                                                       
         SRDL  R0,9                R0=THIS YEAR                                 
         STC   R0,YEARB                                                         
         SRL   R1,32-4             R1=THIS MONTH                                
         STC   R1,MONTHB                                                        
         SLL   R0,9                                                             
         STH   R0,YEARC                                                         
         SR    R1,R1               COMPUTE YEAR+1                               
         IC    R1,YEARB                                                         
         LA    R1,1(R1)                                                         
         SLL   R1,9                                                             
         STH   R1,YEARCP1                                                       
         SR    R1,R1               COMPUTE YEAR-1                               
         IC    R1,YEARB                                                         
         BCTR  R1,0                                                             
         SLL   R1,9                                                             
         STH   R1,YEARCM1                                                       
*                                                                               
         SR    R0,R0                                                            
         L     R1,DUB              CALCULATE REPORT SCAN TIME                   
         D     R0,=F'100'                                                       
         SH    R1,=Y(32*60)        NOW-32 MINTES                                
         MH    R1,=H'3'                                                         
         SRL   R1,2                                                             
         STH   R1,TIMEC                                                         
*                                                                               
         L     R1,DUB              CONVERT TIME TO BINARY HHMMSS                
         SR    R0,R0                                                            
         D     R0,=F'100'          CONVERT TO SECONDS                           
         SR    R0,R0                                                            
         D     R0,=F'60'           CONVERT TO MINUTES                           
         STC   R0,TIMEB+2                                                       
         SR    R0,R0                                                            
         D     R0,=F'60'           CONVERT TO HOURS                             
         STC   R0,TIMEB+1                                                       
         STC   R1,TIMEB+0                                                       
*                                                                               
BLDBC1   L     RF,SRPARA4          SWITCH TO CONTROL SYSTEM                     
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,X'0AFFFFFF',0                                          
         CLI   4(R1),1                                                          
         BL    BLDBC2              VALID SWITCH                                 
         BE    BLDBCX              EXIT IF INVALID SWITCH                       
         B     BLDBCW              CONTROL SYSTEM IS NOP                        
*                                                                               
BLDBC2   LA    R3,DIR              SET GENDIR KEY FOR FIRST MEASSAGE            
         USING BRDKEYD,R3                                                       
         XC    BRDKEY,BRDKEY                                                    
         MVI   BRDKSYS,BRDKSYSQ                                                 
         MVI   BRDKSTYP,BRDKSTYQ                                                
         MVI   BRDKTYPE,BRDKTEMQ                                                
         MVC   BRDKMSGN,BCTLDNUM   SET LAST DISK MSG NUM FROM BCTAB             
         MVC   KEY(32),BRDKEY                                                   
         GOTO1 VDATAMGR,DMCB,(X'08',DMRDHI),GENDIR,(R3),(R3)                    
         TM    DMCB+8,X'02'                                                     
         BO    *+12                INCLUDE DELETED RECORDS                      
         CLI   DMCB+8,0                                                         
         BNE   BLDBCW                                                           
         CLC   KEY(BRDKMSGN-BRDKEYD),BRDKEY                                     
         BNE   BLDBCW              EXIT IF NO BC MESSAGES                       
         MVC   FULL(2),BCTLCNUM    SAVE NUMBER OF FIRST CORE MESSAGE            
         MVC   FULL+2(2),FULL      SAVE NUMBER OF LAST CORE MESSAGE             
         MVC   FUL1(2),BCTLDNUM    SAVE NUMBER OF FIRST DISK MESSAGE            
         MVC   FUL1+2(2),FUL1      SAVE NUMBER OF LAST DISK MESSAGE             
*                                                                               
BLDBC3   LA    R3,DIR              READ NEXT GENDIR BRDCST RECORD               
         MVC   KEY(32),BRDKEY                                                   
         GOTO1 VDATAMGR,DMCB,(X'08',DMRSEQ),GENDIR,(R3),(R3)                    
         TM    DMCB+8,X'02'                                                     
         BO    *+12                INCLUDE DELETED RECORDS                      
         CLI   DMCB+8,0                                                         
         BNE   BLDBCA                                                           
         CLC   KEY(BRDKMSGN-BRDKEYD),BRDKEY                                     
         BNE   BLDBCA                                                           
         MVC   FUL1+2(2),BRDKMSGN  SAVE NUMBER OF NEW LAST DISK MSG             
         TM    BRDKSTAT,X'80'                                                   
         BO    BLDBC3              IGNORE DELETED RECORDS                       
*                                                                               
BLDBC4   SR    R0,R0               UNPACK GENDIR DIR STATUS BYTES               
         ICM   R0,7,BRDKSTAT+1                                                  
         SRDL  R0,18               R0=FACPAKID,R1=DATES                         
         STC   R0,DUB                                                           
*                                                                               
* IF THIS IS A TEST SYSTEM (IE: TST, MEL) DO NOT DISPLAY 'ALL' MSGS             
         LA    RF,FACIDTAB                                                      
BLDBC4A  CLI   0(RF),X'FF'                                                      
         BE    BLDBC3              IF NOT FOUND, IGNORE MESSAGE                 
         CLC   SYSID,4(RF)                                                      
         BE    *+12                                                             
         LA    RF,L'FACIDTAB(RF)                                                
         B     BLDBC4A                                                          
*                                                                               
         TM    5(RF),X'80'         IS THIS A TEST SYSTEM?                       
         BNZ   *+12                YES - SKIP 'ALL' CHECK                       
         CLI   DUB,0               VALID FOR ALL SYSTEMS                        
         BE    *+14                YES                                          
         CLC   DUB(1),SYSID                                                     
         BNE   BLDBC3              IGNORE IF NOT THIS FACPAK                    
*&&US                                                                           
         SLL   R1,2                SHIFT OUT ALL BUT END DATE                   
         CLM   R1,B'1100',DATEC    DID MESSAGE EXPIRE?                          
         BL    BLDBC3              YES -- IGNORE IT                             
         B     BLDBC7                                                           
*&&                                                                             
         SR    R0,R0                                                            
         SLDL  R0,9                R0=START MMDD                                
         SRL   R1,32-9             R1=END MMDD                                  
         STH   R0,STRDC                                                         
         STH   R1,ENDDC                                                         
         CR    R0,R1               IF START GT END HAVE CROSSED YEAR            
         BH    BLDBC5A                                                          
         OC    STRDC,YEARC         ELSE ASSUME THIS YEAR                        
         OC    ENDDC,YEARC                                                      
         B     BLDBC6                                                           
BLDBC5A  CLI   MONTHB,3                                                         
         BH    BLDBC5B                                                          
         OC    STRDC,YEARCM1       IF IN FIRST QTR ASSUME LAST YEAR             
         OC    ENDDC,YEARC                                                      
         B     BLDBC6                                                           
BLDBC5B  OC    STRDC,YEARC         ELSE ASSUME NEXT YEAR                        
         OC    ENDDC,YEARCP1                                                    
*                                                                               
BLDBC6   CLC   DATEC,STRDC         TEST IF BRDCST MSG VALID FOR TODAY           
         BL    BLDBC3                                                           
         CLC   DATEC,ENDDC                                                      
         BH    BLDBC3                                                           
*                                                                               
BLDBC7   MVC   DSKADR,BRDDA        READ FULL MESSAGE DETAILS                    
         L     R3,AIO                                                           
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),GENFIL,DSKADR,(R3),DMWORK           
         CLI   DMCB+8,0                                                         
         BNE   BLDBC3              IGNORE FUNNY RECORDS                         
         LA    R0,100                                                           
         SR    R1,R1                                                            
         LA    R3,BRDFSTEL                                                      
         USING BRDFLTD,R3                                                       
BLDBC7A  CLI   0(R3),0             SEARCH FOR FILTER ELEMENT                    
         BE    BLDBC3                                                           
         CLI   0(R3),BRDFLTCQ                                                   
         BE    BLDBC8                                                           
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         BCT   R0,BLDBC7A                                                       
         B     BLDBC3                                                           
*                                                                               
BLDBC8   CLC   DATEC,BRDFSTDT      CHECK TODAY WITH FULL DATES                  
         BL    BLDBC3                                                           
         CLC   DATEC,BRDFENDT                                                   
         BH    BLDBC3                                                           
* CHECK IF IT WILL BE SENT TODAY                                                
         CLI   BRDFDAYS,X'00'      PRE-DAY RECORD                               
         BNE   *+12                                                             
         MVI   DUB,X'7F'           ASSUME ALL DAYS                              
         B     *+10                                                             
         MVC   DUB(1),BRDFDAYS     CHECK IF TODAY IS SET                        
         NC    DUB(1),DAY          DAY = TODAY                                  
         BZ    BLDBC3              WON'T BE SENT TODAY                          
*                                                                               
         L     R5,ABCTAB           POINT TO BCTAB                               
         LH    R1,FULL+2           GET LAST ENTRY NUMBER IN BCTAB               
         LA    R1,1(R1)                                                         
         LR    R0,R1               SAVE NEW NUMBER                              
         MH    R1,0(R5)                                                         
         LA    R1,6(R5,R1)                                                      
         C     R1,2(R5)            TEST END OF BCTAB                            
         BH    BLDBC9                                                           
         STH   R0,FULL+2           SET NEW BC NUMBER                            
         MVI   NEWBC,1                                                          
         LR    R5,R1               R5=A(NEXT AVAILABLE ENTRY)                   
         XC    0(BCTABL,R5),0(R5)                                               
         MVC   BCTNUM,FULL+2       ENTRY NUMBER                                 
         MVC   BCTDNUM,FUL1+2      DISK MESSAGE NUMBER                          
         MVC   BCTDADR,DSKADR      DISK MESSAGE DISK ADDRESS                    
         MVC   BCTNAME,BRDFNAME    MESSAGE NAME                                 
*                                                                               
         TM    BRDFLAGS,BRDFLLST   LUID IS A LIST                               
         BZ    *+8                                                              
         OI    BCTFLAG,BCTFLST                                                  
         TM    BRDFLAGS,BRDFLMST   MUST DISPLAY FLAG                            
         BZ    *+8                                                              
         OI    BCTFLAG,BCTFMST                                                  
         MVC   BCTCTRY,BRDFCTRY    COUNTRY CODE                                 
         MVC   BCTSTTM,BRDFSTTM    START TIME                                   
         MVC   BCTENTM,BRDFENTM    END TIME                                     
         MVC   BCTOVSYS,BRDFOVSY   OVERLAY SYSTEM                               
         MVC   BCTSYS,BRDFSNUM     SE NUMBER                                    
         MVC   BCTPRG,BRDFPROG     PROGRAM NUMBER                               
         MVC   BCTLUID,BRDFLUID    LUID                                         
*                                                                               
BLDBC9   B     BLDBC3              BACK FOR NEXT                                
*                                                                               
BLDBCA   L     R5,ABCTAB           POINT TO START OF BROADCAST TABLE            
         LA    R5,6(R5)                                                         
         CLC   FULL(2),FULL+2      TEST IF NEW CORE ENTRY                       
         BNE   *+14                                                             
         CLC   FUL1(2),FUL1+2                                                   
         BE    BLDBCW                                                           
         MVC   BCTLCNUM,FULL+2     SET NEW LAST CORE                            
         MVC   BCTLDNUM,FUL1+2     SET NEW LAST DISK                            
         MVC   BCTPCNUM,FULL       SET PREV LAST CORE                           
         MVC   BCTPDNUM,FUL1       SET PREV LAST DISK                           
         MVC   BCTLTIME,TIMEB      SET TIME THIS TABLE WAS UPDATED              
         OI    SSBSTAT1-SSBD(RE),SSBSCHK1                                       
*                                                                               
BLDBCW   L     RF,SRPARA4          SWITCH BACK TO SERVICE SYSTEM                
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,X'01FFFFFF',0                                          
         L     RF,SRPARA3          TURN OFF HAVE SWITCHED FLAG IN UTL           
         NI    TFLAG-UTLD(RF),255-TFLAGSSW                                      
*                                                                               
BLDBCX   CLI   NEWBC,0             EXIT WITH CC NEQ IF NEW BC MSGS              
         XIT1                                                                   
         EJECT                                                                  
* SEARCH BROADCAST TABLE FOR RELEVENT MESSAGES.                                 
* SCAN UTL AND FLAG TERMINALS THAT MATCH THE FILTERS ON THE MESSAGES.           
*                                                                               
SETBC    NTR1  ,                                                                
         ICM   R5,15,ABCTAB        R5=A(BROADCAST TABLE)                        
         BZ    SETBCX                                                           
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6+BCTABL(R5)     POINT TO FIRST MESSAGE ENTRY                 
         USING BCTABD,R5                                                        
*                                                                               
SETBC1   STM   R6,R7,SVREGS        SAVE BXLE REGS                               
         OC    BCTNUM,BCTNUM       EXIT IF NO MORE BRDCST MESSAGES              
         BZ    SETBCX                                                           
         TM    BCTFLAG,BCTFDEL+BCTFPRC IGNORE DELETED/PROCESSED MSGS            
         BNZ   SETBC2                                                           
SETBC1A  OC    BCTSTTM,BCTSTTM     TEST IF MSG HAS START TIME                   
         BZ    SETBC1B                                                          
         CLC   TIMEB(2),BCTSTTM                                                 
         BL    SETBC2                                                           
SETBC1B  OC    BCTENTM,BCTENTM     TEST IF MSG HAS END TIME                     
         BZ    SETBC1C                                                          
         CLC   TIMEB(2),BCTENTM                                                 
         BH    SETBC2                                                           
SETBC1C  B     SETBC3                                                           
SETBC2   LM    R6,R7,SVREGS        RESTORE BXLE REGS                            
         BXLE  R5,R6,SETBC1                                                     
         B     SETBCX              EXIT IF BCTAB FULL CUNT HEAD                 
*                                                                               
SETBC3   OI    BCTFLAG,BCTFPRC     SET HAVE PROCESSED THIS MESSAGE              
         L     R8,VUTL                                                          
         LH    R6,0(R8)                                                         
         L     R7,2(R8)                                                         
         LA    R8,6(R8)                                                         
         USING UTLD,R8             SET TO SEARCH UTL FOR MATCHES                
*                                                                               
SETBC4   OC    TPRNT,TPRNT         IGNORE PRINTERS                              
         BNZ   SETBCU                                                           
*                                                                               
SETBC5   CLI   BCTCTRY,X'FF'       FILTER ON COUNTRY                            
         BE    SETBC6                                                           
         CLC   BCTCTRY,TCTRY                                                    
         BE    SETBC6                                                           
         CLI   BCTCTRY,0           USE DEFAULT CTRY IF NOT DEFINED              
         BNE   SETBCU                                                           
         CLC   BCTCTRY,DEFCTRY                                                  
         BNE   SETBCU                                                           
*                                                                               
SETBC6   CLI   TSYS,0              IGNORE DISCONNECTED TERMINALS                
         BE    SETBCU                                                           
         CLI   BCTSYS,0            FILTER ON SPECIFIC SYSTEM                    
         BE    SETBC6A                                                          
         CLC   BCTSYS,TSYS                                                      
         BNE   SETBCU                                                           
         B     SETBC6B                                                          
SETBC6A  CLI   BCTOVSYS,0          FILTER ON MAJOR SYSTEM                       
         BE    SETBC7                                                           
         CLC   BCTOVSYS,TOVSYS                                                  
         BNE   SETBCU                                                           
SETBC6B  CLI   BCTPRG,0            FILTER ON PROGRAM (WITHIN SYSTEM)            
         BE    SETBC7                                                           
         CLC   BCTPRG,TPRG                                                      
         BNE   SETBCU                                                           
*                                                                               
SETBC7   CLI   BCTLUID,0           FILTER ON LUID                               
         BE    SETBC9                                                           
         MVC   DUB,TSYM            SET TERMINAL LUID IN DUB                     
         TM    BCTFLAG,BCTFLST                                                  
         BO    SETBC8                                                           
         LA    R0,8                PROCESS FIRST 8 CHRS                         
         LA    R1,BCTLUID                                                       
         LA    R2,DUB                                                           
SETBC7A  CLI   0(R1),C'*'          ALLOW WILD CARD CHRS                         
         BE    SETBC7B                                                          
         CLI   0(R1),C' '                                                       
         BNH   SETBC7B                                                          
         CLC   0(1,R1),0(R2)                                                    
         BNE   SETBCU                                                           
SETBC7B  LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,SETBC7A                                                       
         B     SETBC9                                                           
*                                                                               
SETBC8   B     SETBCP              FILTER ON LUID LIST                          
*                                                                               
SETBC9   EQU   *                   END OF ALL FILTERS                           
*                                                                               
SETBCP   OI    TSTAT2,TSTATBCP     SET BRDCST PENDING IN UTL ENTRY              
*                                                                               
SETBCU   BXLE  R8,R6,SETBC4                                                     
         B     SETBC2              BACK FOR NEXT BRDCST MESSAGE                 
*                                                                               
SETBCX   XIT1                                                                   
         DROP  R8                                                               
         EJECT                                                                  
*&&UK                                                                           
* TEST TO SEE IF 5000 MESSAGES READ SINCE LAST CLEANUP                          
* IF SO SCAN BUFFER AND REMOVE ALL MESSAGES WITH LOW USEAGE                     
*                                                                               
CHKMSG   NTR1  ,                                                                
         GOTO1 VCALLOV,DMCB,0,X'D9000AA0'                                       
         L     R1,0(R1)            GET A(MSGBUFF IN R1)                         
         ST    R1,AMSGBUFF                                                      
         LR    R5,R1               POINT R5 TO MESSAGE BUFFER                   
         USING MSGBUFFD,R5                                                      
         CLI   MSGBSTAT,C'+'       BUFFER MUST BE ENABLED                       
         BNE   CHKX                                                             
         L     R1,MSGBTOTL                                                      
         S     R1,MSGBPREV                                                      
         CH    R1,=H'5000'         ONLY WHEN 5000+ CALLS HAVE GONE              
         BL    CHKX                                                             
         MVI   MSGBSTAT,C'*'       FLAG REORG IN PROCESS                        
         MVC   MSGBPREV,MSGBTOTL                                                
         MVC   FULL(1),MSGBCUT                                                  
         LH    R6,MSGBNUM          R6 = NUMBER OF MESSAGES                      
         LA    R5,MSGBNDX          R5 = A(INDEX)                                
         LTR   R6,R6                                                            
         BZ    CHKX                NO MESSAGES                                  
*                                                                               
CHK010   CLC   5(1,R5),FULL        DELETE MSGS LOWER THAN CUT OFF               
         MVI   5(R5),0             RESET COUNTER FOR NEXT TIME                  
         BH    CHK020                                                           
         LR    R1,R5                                                            
         BRAS  RE,DELMSG                                                        
         B     *+8                 DON'T BUMP INDEX                             
CHK020   LA    R5,8(R5)            NEXT INDEX ENTRY                             
         BCT   R6,CHK010                                                        
         L     R5,AMSGBUFF         POINT R5 TO MESSAGE BUFFER                   
         MVI   MSGBSTAT,C'+'       RE ENABLE BUFFER                             
         B     CHKX                                                             
         EJECT                                                                  
*********************************************************                       
* DELETE ENTRY FROM MESSAGE TABLE   R1=A(INDEX ENTRY)   *                       
*********************************************************                       
         SPACE 1                                                                
DELMSG   NTR1                                                                   
         L     R5,AMSGBUFF         POINT R5 TO MESSAGE BUFFER                   
         USING MSGBUFFD,R5                                                      
         LR    R2,R1               SAVE A(INDEX ENTRY) IN R2                    
         SR    RF,RF                                                            
         ICM   RF,3,6(R1)          RF=DISPLACEMENT TO MESSAGE ELEMENT           
         LA    RF,MSGBUFFS(RF)                                                  
         SR    R1,R1                                                            
         IC    R1,1(RF)            GET ELEMENT LEN                              
         LR    R4,R1               SAVE IT IN R4                                
         LR    R0,RF               R0=A(ELEMENT)                                
         LA    RE,0(RF,R1)         RE=A(NEXT ELEMENT)                           
         L     RF,MSGBNXT                                                       
         LR    R1,RF                                                            
         SR    RF,RE               CALCULATE LENGTHS FOR MOVE                   
         SR    R1,R0                                                            
         BRAS  R9,CHECKIT                                                       
         MVCL  R0,RE               MOVE TEXT UP                                 
         L     RF,MSGBNXT                                                       
         SR    RF,R4               RESET END OF TEXT POINTER                    
         ST    RF,MSGBNXT                                                       
*                                                                               
         LA    RE,8(R2)            RE=A(NEXT INDEX)                             
         LR    R0,R2               R0=A(THIS INDEX)                             
         LH    RF,MSGBNUM                                                       
         BCTR  RF,0                                                             
         STH   RF,MSGBNUM          REMOVE 1 FROM INDEX                          
         LA    RF,1(RF)                                                         
         SLL   RF,3                RF=TOTAL * 8                                 
         LA    R1,MSGBNDX                                                       
         AR    RF,R1               RF=A(INDEX END)                              
         SR    RF,RE               RF=LEN SOURCE                                
         LA    R1,8(RF)            R1=LEN DEST (SOURCE+8)                       
         BRAS  R9,CHECKIT                                                       
         MVCL  R0,RE                                                            
         SR    RF,RF                                                            
         LR    R1,R2               SCAN LOWER INDEX ENTRIES                     
DEL1     OC    0(8,R1),0(R1)       TEST FOR INDEX END                           
         BZ    CHKX                                                             
         ICM   RF,3,6(R1)          GET DISPLACEMENT                             
         SR    RF,R4               SUBTRACT L'DELETED ELEMENT                   
         STCM  RF,3,6(R1)          SAVE IT                                      
         LA    R1,8(R1)            NEXT INDEX                                   
         B     DEL1                                                             
         EJECT                                                                  
CHECKIT  STM   RE,R1,REGSAVE                                                    
         SR    R1,RF                                                            
         CLM   R1,1,=X'53'         TEST OFFSET < 84                             
         BH    FOFF                                                             
         C     R0,AMSGBUFF         TEST A(DEST)   > A(BUFFER)                   
         BNH   FOFF                                                             
         C     RE,AMSGBUFF         TEST A(SOURCE) > A(BUFFER)                   
         BNH   FOFF                                                             
         AR    RF,R0                                                            
         AR    RF,R1                                                            
         L     R1,AMSGBUFF                                                      
         L     R1,MSGBEND-MSGBUFFD(R1)                                          
         CR    RF,R1               TEST DEST+LEN  < A(BUFFERX)                  
         BH    FOFF                                                             
         LM    RE,R1,REGSAVE       OK DO IT!!                                   
         BR    R9                                                               
FOFF     DC    H'0'                DON'T DO THAT MVCL                           
*                                  LEAVE TABLE DISABLED                         
CHKX     XIT1                                                                   
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* CHECK ESS TIMEOUTS FOR PRESTO                                       *         
***********************************************************************         
         SPACE 1                                                                
ESSTIME  NTR1  ,                                                                
         TIME  TU                  TIME NOW                                     
         SL    R0,=A(5*60*38400)   LESS 5 MINUTES                               
         ST    R0,FULL                                                          
*                                                                               
         L     R8,VUTL                                                          
         LH    R6,0(R8)                                                         
         L     R7,2(R8)                                                         
         LA    R8,6(R8)            R8=A(UTL ENTRY)                              
         USING UTLD,R8                                                          
ESS02    CLC   =C'ES',TLUID+4      ESS SERVER HAS ES AS 5/6 CHARS               
         BNE   ESS04                                                            
         CLC   TTIME,FULL          WORKED IN LAST 5 MINUTES                     
         BH    ESS04                                                            
*                                                                               
         MVC   WORK1,ESSMSG                                                     
         MVC   WORK1+19(8),TLUID                                                
         ICM   RF,15,=C'LVL2'      OUTPUT ERROR MESSAGE                         
         GOTO1 VDMOD000,DMCB,VWCTYPE,WORK1,L'WORK1,(RF)                         
*                                                                               
ESS04    BXLE  R8,R6,ESS02                                                      
         XIT1  ,                                                                
*                                                                               
ESSMSG   DC    CL64'PRMON **ESS SERVER XXXXXXXX HAS TIMED OUT**'                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
TIMEADJ  DS    0F                                                               
*&&UK*&& DC    A(0)                                                             
*&&US*&& DC    A(6*60*60*100)      DDSTIME=YES                                  
SPACES   DC    CL20' '                                                          
DMREAD   DC    C'DMREAD  '                                                      
DMRDHI   DC    C'DMRDHI  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
GETREC   DC    C'GETREC  '                                                      
DMWRT    DC    C'DMWRT   '                                                      
DMUNLK   DC    C'DMUNLK  '                                                      
BUFFER   DC    C'BUFFER  '                                                      
GLIST    DC    C'GLIST   '                                                      
GFILE    DC    C'GFILE   '                                                      
PRTQUE   DC    C'PRTQUE  '                                                      
CTFILE   DC    C'CTFILE  '                                                      
GENDIR   DC    C'GENDIR  '                                                      
GENFIL   DC    C'GENFIL  '                                                      
TEMPSTR  DC    C'TEMPSTR '                                                      
FACWRK   DC    C'FACWRK  '                                                      
MYLOGID  DC    C'$PQ '                                                          
SRTXT    DC    X'0144'                                                          
EFFS     DC    16X'FF'                                                          
DEFCTRY  DS    0X                                                               
*&&UK*&& DC    AL1(1)                                                           
*&&US*&& DC    AL1(2)                                                           
         SPACE 1                                                                
TWAR     EQU   X'80'               S/R SAVE PAGE READ                           
TWAU     EQU   X'40'               S/R SAVE PAGE UPDATED                        
NOTP     EQU   X'20'               NOTIFY PENDING FOR TERMINAL                  
EXTP     EQU   X'10'               EXTRACT UPDATE PENDING FOR TERMINAL          
ALLD     EQU   X'01'               ALL JOBS COMPLETE FOR TERMINAL               
* FACIDTAB                                                                      
       ++INCLUDE FACIDTAB                                                       
         DC    X'FF'               MARK END OF TABLE                            
         EJECT                                                                  
********************************************************************            
* BUILD LIST OF SUBMITTED JOBS WHICH ARE COMPLETE FOR NOTIFICATION *            
********************************************************************            
         SPACE 1                                                                
TIMERS   CSECT                                                                  
*                                                                               
BLDJOB   NTR1  BASE=*,LABEL=*                                                   
         USING SYSFACD,R9                                                       
         L     R2,VSSB             LOCATE JOBTAB IN TABS                        
         LAM   R2,R2,SSBTBLET-SSBD(R2)                                          
         SAC   512                                                              
         LA    R2,5                JOBTAB IS RESOURCE 5 (DTJOB)                 
         SLL   R2,6                                                             
         ICM   R2,15,60(R2)        ADDR IS AT 60(HEADER)                        
         SLL   R2,2                                                             
         SRL   R2,2                DROP X'40'                                   
         ST    R2,AJOBTAB                                                       
*                                                                               
         USING TABJOBS,R2                                                       
         MVC   JOBHDRL,TBJLHEAD    SET SOFT LENGTHS                             
         MVC   JOBTABL,TBJLNTRY                                                 
         DROP  R2                                                               
*                                                                               
         OC    JOBHDRL,JOBHDRL     IF NOT SET                                   
         BNZ   *+16                                                             
         MVC   JOBHDRL,=H'32'      USE OLD HARD DEFAULTS                        
         MVC   JOBTABL,=H'48'                                                   
*                                                                               
         L     RF,=A(JTAB-WORKD)                                                
         AR    RF,RC                                                            
         ST    RF,AJTAB            SET A(JTAB) IN W/S                           
         XC    JNUM,JNUM           CLEAR N'ENTRIES IN TABLE                     
         L     R1,AJTAB                                                         
         USING JTABD,R1            R1=A(JOB TABLE ENTRY)                        
         LA    R3,JMAX             R3=MAXIMUM ENTRIES IN TABLE                  
         SR    R4,R4               R4=N'JTAB ENTRIES                            
         L     R2,AJOBTAB                                                       
         ICM   R0,15,0(R2)                                                      
         BZ    BLDJOBX                                                          
         AH    R2,JOBHDRL                                                       
         USING TBJOBTAB,R2         R2=A(JOB TABLE)                              
BLDJOB2  OC    TBJNTRY(4),TBJNTRY  TEST USED ENTRY                              
         BNZ   *+12                                                             
         AH    R2,JOBTABL                                                       
         B     BLDJOB2                                                          
*                                                                               
         MVC   DUB(1),TBJADV       ONLY JOBS OF MY TOR GROUP                    
         NC    DUB(1),=X'0F'                                                    
         CLC   DUB(1),SYSID                                                     
         BNE   BLDJOB4                                                          
*                                                                               
         OC    TBJETIME,TBJETIME   ONLY COMPLETED JOBS                          
         BZ    BLDJOB4                                                          
         MVI   TBJSTAT,0           FLAG OK TO DELETE NOW                        
*                                                                               
         XC    JTABD(JTABL),JTABD                                               
         MVC   JTERM,TBJTERM                                                    
         MVC   JPQKEY,TBJPQKEY                                                  
         MVC   JPQID,TBJPQID                                                    
         LA    R1,JTABL(R1)                                                     
         LA    R4,1(R4)                                                         
         BCT   R3,*+6                                                           
         DC    H'0'                DIE IF JTAB IS FULL                          
BLDJOB4  AH    R2,JOBTABL                                                       
         BCT   R0,BLDJOB2                                                       
         SAC   0                                                                
*                                                                               
         ST    R4,JNUM             SET N'ENTRIES IN JTAB                        
         XC    JTABD(JTABL),JTABD                                               
         MVC   JTERM,EFFS          SET E-O-L                                    
         LTR   R4,R4                                                            
         BZ    BLDJOBX                                                          
*                                                                               
         LA    R0,1(R4)            SORT JTAB INTO TNUM SEQUENCE                 
         ST    R0,DMCB+04                                                       
         LA    R0,JTABL                                                         
         ST    R0,DMCB+08                                                       
         LA    R0,L'JTERM                                                       
         ST    R0,DMCB+12                                                       
         LA    R0,JTERM-JTABD                                                   
         ST    R0,DMCB+16                                                       
         GOTO1 =V(XSORT),DMCB,(0,AJTAB),RR=RELO                                 
*                                                                               
BLDJOBX  SAC   0                                                                
         B     TIMX                                                             
         LTORG                                                                  
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD JOB QUEUE TO CALCULATE SEQUENCE                               *         
***********************************************************************         
         SPACE 1                                                                
BLDQUE   NTR1  BASE=*,LABEL=*                                                   
         USING SYSFACD,R9                                                       
         L     R2,VSSB             LOCATE JOBTAB IN TABS                        
         LAM   R2,R2,SSBTBLET-SSBD(R2)                                          
         SAC   512                                                              
         LA    R2,5                JOBTAB IS RESOURCE 5 (DTJOB)                 
         SLL   R2,6                                                             
         ICM   R2,15,60(R2)        ADDR IS AT 60(HEADER)                        
         SLL   R2,2                                                             
         SRL   R2,2                DROP X'40'                                   
         ST    R2,AJOBTAB                                                       
*                                                                               
         USING TABJOBS,R2                                                       
         MVC   JOBHDRL,TBJLHEAD    SET SOFT LENGTHS                             
         MVC   JOBTABL,TBJLNTRY                                                 
         DROP  R2                                                               
*                                                                               
         OC    JOBHDRL,JOBHDRL     IF NOT SET                                   
         BNZ   *+16                                                             
         MVC   JOBHDRL,=H'32'      USE OLD HARD DEFAULTS                        
         MVC   JOBTABL,=H'48'                                                   
*                                                                               
         L     RF,=A(JTAB-WORKD)                                                
         AR    RF,RC                                                            
         ST    RF,AJTAB            SET A(JTAB) IN W/S                           
         XC    JNUM,JNUM           CLEAR N'ENTRIES IN TABLE                     
         L     R1,AJTAB                                                         
         USING QTABD,R1            R1=A(QUEUE TABLE ENTRIES)                    
         LA    R3,JMAX             R3=MAXIMUM ENTRIES IN TABLE                  
         SR    R4,R4               R4=N'JTAB ENTRIES                            
         L     R2,AJOBTAB                                                       
         ICM   R0,15,0(R2)                                                      
         BZ    BLDQ50                                                           
         AH    R2,JOBHDRL                                                       
         USING TBJOBTAB,R2         R2=A(JOB TABLE)                              
BLDQUE2  OC    TBJNTRY(4),TBJNTRY  TEST USED ENTRY                              
         BNZ   *+12                                                             
         AH    R2,JOBTABL                                                       
         B     BLDQUE2                                                          
*                                                                               
         XC    QTABD(QTABL),QTABD                                               
         MVC   QTIME,TBJSTIME                                                   
         MVC   QCLASS,TBJCLASS                                                  
         STCM  R2,15,QOFFS                                                      
         MVC   QUSER,TBJPQUSR                                                   
         LA    R1,QTABL(R1)                                                     
         LA    R4,1(R4)                                                         
         BCT   R3,*+6                                                           
         DC    H'0'                DIE IF JTAB IS FULL                          
BLDQUE4  AH    R2,JOBTABL                                                       
         BCT   R0,BLDQUE2                                                       
         SAC   0                                                                
*                                                                               
         ST    R4,JNUM             SET N'ENTRIES IN JTAB                        
         XC    QTABD(QTABL),QTABD                                               
         MVC   0(QTABL,R1),EFFS    SET E-O-L                                    
         LTR   R4,R4                                                            
         BZ    BLDQ50                                                           
*                                                                               
         LA    R0,1(R4)            SORT QTAB INTO USER/TIME SEQUENCE            
         ST    R0,DMCB+04                                                       
         LA    R0,QTABL                                                         
         ST    R0,DMCB+08                                                       
         LA    R0,7                                                             
         ST    R0,DMCB+12                                                       
         LA    R0,0                                                             
         ST    R0,DMCB+16                                                       
         GOTO1 =V(XSORT),DMCB,(0,AJTAB),RR=RELO                                 
*                                                                               
         L     R1,AJTAB            EXTRACT TOP 5 JOBS                           
BLDQ20   LA    RE,0                                                             
         LA    RF,5                                                             
         MVC   FULL,0(R1)          SAVE CLASS/USERID                            
         CLC   0(2,R1),EFFS                                                     
         BNE   *+10                                                             
         MVC   0(QTABL,R1),EFFS                                                 
BLDQ21   STCM  RE,3,2(R1)          STORE USER FACTOR                            
         LA    R1,QTABL(R1)                                                     
         CLC   0(QTABL,R1),EFFS                                                 
         BE    BLDQ30                                                           
         CLC   FULL,0(R1)          SAME CLASS/USER                              
         BNE   BLDQ20                                                           
         BCT   RF,BLDQ21           ALLOW 5 JOBS                                 
         LA    RE,1(RE)            BUMP USER FACTOR BY 1                        
         LA    RF,5                                                             
         B     BLDQ21              REMOVE THE REST FOR THIS CLASS/USER          
*                                                                               
BLDQ30   L     R0,JNUM             SORT QTAB INTO CLASS/TIME SEQUENCE           
         ST    R0,DMCB+04                                                       
         LA    R0,QTABL                                                         
         ST    R0,DMCB+08                                                       
         LA    R0,7                                                             
         ST    R0,DMCB+12                                                       
         LA    R0,0                                                             
         ST    R0,DMCB+16                                                       
         GOTO1 =V(XSORT),DMCB,(0,AJTAB),RR=RELO                                 
*                                                                               
BLDQ50   SAC   512                                                              
         L     R1,AJTAB            SET POSITION QUESS                           
BLDQ51   LA    RF,1                                                             
         MVC   HALF,QCLASS                                                      
BLDQ52   CLC   0(4,R1),EFFS        TEST FOR EOT                                 
         BE    BLDQUEX                                                          
         CLC   QCLASS,HALF         NEW CLASS                                    
         BNE   BLDQ51                                                           
         ICM   R2,15,QOFFS         PICK UP DSPACE OFFS                          
         BZ    BLDQUEX                                                          
         STC   RF,TBJPOSG          SAVE POSITION QUESS                          
         LA    RF,1(RF)                                                         
         LA    R1,12(R1)           NEXT ENTRY                                   
         B     BLDQ52                                                           
*                                                                               
BLDQUEX  SAC   0                                                                
         B     TIMX                                                             
         LTORG                                                                  
         DROP  R1,R2                                                            
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* BUILD DARTAB                                                        *         
* NTRY: R5 = A(PQREC)                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING PQRECD,R5                                                        
XTODTAB  NTR1  BASE=*,LABEL=*                                                   
         L     R8,VSSB                                                          
         USING SSBD,R8                                                          
         L     R1,SSBDARPQ                                                      
         LH    RE,0(R1)            RE = L(PQ ENTRY)                             
         L     RF,2(R1)            RF = A(LAST BYTE OF TABLE)                   
         LA    R1,6(R1)            R1 = A(FIRST SAVED PQ ENTRY)                 
*                                                                               
XTOD02   CLC   0(8,R1),XEFFS       FOUND AN COMPLETED ENTRY?                    
         BE    XTOD06              YES                                          
         OC    0(8,R1),0(R1)       FOUND AN EMPTY ENTRY?                        
         BZ    XTOD06              YES                                          
*                                                                               
TODTABD1 USING PQKEY,R1            SAME ENTRY AS BEFORE?                        
         CLC   TODTABD1.PQSRCID,PQSRCID                                         
         BNE   XTOD04                                                           
         CLC   TODTABD1.PQSUBID,PQSUBID                                         
         BNE   XTOD04                                                           
         CLC   TODTABD1.PQREPNO,PQREPNO                                         
         BE    XTOD08              YES                                          
*                                                                               
XTOD04   BXLE  R1,RE,XTOD02        NO, GO TO THE NEXT ENTRY                     
         B     XTODX               HIT THE END, BUILD ON NEXT TIMER POP         
*                                                                               
XTOD06   MVC   TODTABD1.PQSRCID,PQSRCID                                         
         MVC   TODTABD1.PQSUBID,PQSUBID                                         
         MVC   TODTABD1.PQREPNO,PQREPNO                                         
         DROP  TODTABD1                                                         
*                                                                               
XTOD08   CLC   PQSUBID,=C'DAR'     TEST DARE                                    
         BNE   *+8                                                              
         OI    SSBDARFL,SSBDRRPQ   YES                                          
*****    B     *+8                                                              
*****    OI    SSBDARFL,SSBDRSPQ   HAVE AN ENTRY FOR $MKGDARE  (SPOT)           
*                                                                               
         L     R2,AIO2                                                          
         USING CTIKEY,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,PQSRCID                                                  
         MVC   HALF,PQSRCID        USE HALF FOR ID NUMBER                       
         MVC   KEY,CTIKEY                                                       
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,XDMREAD),XCTFILE,AIO2,AIO2                      
         CLI   0(R1),0                                                          
         BNE   XTOD20              DON'T DIE EVERY POP                          
*                                                                               
         XR    R0,R0                                                            
         LA    RE,DARXAGY          EXCLUDED AGENCY LIST                         
         LA    RF,CTIDATA                                                       
         USING CTAGYD,RF                                                        
*                                                                               
XTOD10   CLI   CTAGYEL,0           FIND AGENCY CODE                             
         BE    XTOD20              BAIL OUT                                     
         CLI   CTAGYEL,CTAGYELQ                                                 
         BE    XTOD12                                                           
         IC    R0,CTAGYLEN                                                      
         AR    RF,R0                                                            
         B     XTOD10                                                           
*                                                                               
XTOD12   CLI   0(RE),0             CHECK EXCLUDED AGENCY LIST                   
         BE    XTOD14                                                           
         CLC   CTAGYID,0(RE)       AGENCY CODE MATCH?                           
         BE    XTODX               YES - NOTHING TO ADD                         
         AHI   RE,L'DARXAGY                                                     
         B     XTOD12                                                           
*                                                                               
XTOD14   LA    RF,CTIDATA                                                       
         USING CTDSCD,RF                                                        
*                                                                               
XTOD16   CLI   CTDSCEL,0                                                        
         BE    XTOD20                                                           
         CLI   CTDSCEL,CTDSCELQ    FIND ALPHA ID                                
         BE    XTOD18                                                           
         IC    R0,CTDSCLEN                                                      
         AR    RF,R0                                                            
         B     XTOD16                                                           
*                                                                               
XTOD18   XC    CTIKEY,CTIKEY       POINT TO ALPHA ID RECORD                     
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,CTDSC                                                     
         MVC   KEY,CTIKEY                                                       
         DROP  RF                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,XDMRDHI),XCTFILE,AIO2,AIO2                      
         CLI   8(R1),0                                                          
         BNE   XTOD20              DON'T DIE EVERY POP - ITS DARE'S             
*                                                                               
         CLC   CTIKEY,KEY                                                       
         BE    XTOD22                                                           
         CLC   PQSUBID,=C'DAR'     HAVE AN ENTRY FOR $REPDARE? (REP)            
         BE    XTOD22              YES - DO MULTIPLE ID'S                       
*                                                                               
XTOD20   XC    CTIKEY,CTIKEY       CLEAR ID INFORMATION                         
*                                                                               
XTOD22   L     RE,SSBDARTB         EXTRACT SSB DATA                             
*                                                                               
XTOD24   CLC   0(4,RE),XEFFS                                                    
         BE    XTODX               NO ROOM LEFT                                 
         CLC   HALF,0(RE)          USER ID MATCH?                               
         BE    XTOD26              YES                                          
         OC    0(4,RE),0(RE)       EMPTY SLOT?                                  
         BZ    XTOD26              YES                                          
         LA    RE,4(RE)                                                         
         B     XTOD24                                                           
*                                                                               
XTOD26   MVC   0(2,RE),HALF                                                     
         ZAP   DUB(4),=P'0'                                                     
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ADDJUST FOR DDS TIME                         
         AP    DUB(4),TIMEHMS                                                   
         MVO   FULL,DUB(4)         PWOS HHMM??                                  
         MVC   2(2,RE),FULL                                                     
*                                                                               
* REPDARE IS SOMETIMES ACROSS MULTIPLE IDS, SO CHECK FOR MORE IDS               
*                                                                               
         OC    CTIKEY,CTIKEY                                                    
         BZ    XTODX                   NO - JUST EXIT                           
*                                                                               
XTOD28   GOTO1 VDATAMGR,DMCB,(0,XDMRSEQ),XCTFILE,AIO2,AIO2                      
         CLI   DMCB+8,0                                                         
         BNE   XTODX               DON'T DIE EVERY POP - ITS DARE'S             
*                                                                               
* PARSE ALPHA ID                                                                
* ADDITIONAL IDS WILL BE ALPHA ID PLUS OTHER CHARACTERS WITH SAME LIMIT         
* ACCESS (E.G. SELNY (O=NY) HAS ADDITIONAL IDS, SELNY1 AND SELNY3)              
*                                                                               
         LA    R1,KEY+((CTIKID-CTIKEY)+L'CTIKID-1)                              
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         B     *-10                                                             
*                                                                               
         LA    R0,KEY                                                           
         SR    R1,R0                                                            
         EX    R1,*+8                                                           
         BNE   XTODX                                                            
         CLC   CTIKEY,KEY                                                       
*                                                                               
         AHI   R1,-1                                                            
         BNP   XTODX                                                            
         LA    R1,KEY(R1)                                                       
         MVC   FULL(2),0(R1)                                                    
*                                                                               
         LA    RF,CTIDATA                                                       
         USING CTSYSD,RF                                                        
         XR    R0,R0                                                            
XTOD30   CLI   CTSYSEL,0                                                        
         BE    XTOD28              NEXT RECORD                                  
         CLI   CTSYSEL,CTSYSELQ                                                 
         BNE   *+12                                                             
         CLI   CTSYSNUM,8          REP LIMIT ACCES?                             
         BE    XTOD32                                                           
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     XTOD30                                                           
*                                                                               
XTOD32   CLC   CTSYSLMT(2),=C'O='                                               
         BNE   XTOD28                                                           
         CLC   CTSYSLMT+2(2),FULL                                               
         BNE   XTOD28                                                           
         DROP  RF                                                               
*                                                                               
         LA    RF,CTIDATA                                                       
         USING CTDSCD,RF                                                        
         XR    R0,R0                                                            
XTOD34   CLI   CTDSCEL,0                                                        
         BE    XTOD28              NEXT RECORD                                  
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    XTOD36                                                           
         IC    R0,CTDSCLEN                                                      
         AR    RF,R0                                                            
         B     XTOD34                                                           
*                                                                               
XTOD36   MVC   HALF,2(RF)                                                       
         B     XTOD22                                                           
*                                                                               
XTODX    BRAS  RE,SETDAR                                                        
         XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
XEFFS    DC    16X'FF'                                                          
XDMRDHI  DC    CL8'DMRDHI'                                                      
XDMREAD  DC    CL8'DMREAD'                                                      
XDMRSEQ  DC    CL8'DMRSEQ'                                                      
XCTFILE  DC    CL8'CTFILE'                                                      
         SPACE 2                                                                
***********************************************************************         
* AGENCIES EXCLUDED FROM DARE NOTIFIES                                *         
***********************************************************************         
         SPACE 1                                                                
DARXAGY  DS    0CL2                                                             
         DC    C'WI'               WESTERN                                      
         DC    C'WJ'               WITEST                                       
         DC    X'00'                                                            
         DROP  R5,R8                                                            
         EJECT                                                                  
***********************************************************************         
* SET DARE MESSAGE FLAG FROM DARETAB IN UTLS                          *         
*                                                                     *         
* NTRY: DARETAB IN TABS DATASPACE HOLDS 2 TABLES, ONE SORTED, ONE NOT *         
*       SORTED. THE PROGRAM ATTEMPTS TO MERGE ALL UNSORTED (I.E. NEW) *         
*       ENTRIES INTO THE SORTED TABLE, BEFORE RESETTING THE POINTER   *         
*       FOR THE UNSORTED TABLE TO THE START OF THE TABLE.             *         
*                                                                     *         
*       NEW POSTINGS COME IN THROUGH DDMQIO AND ARE PLACED INTO THE   *         
*       UNSORTED TABLE. IT IS NOT POSSIBLE TO LOCK THIS TABLE DUE TO  *         
*       THE INLINE NATURE OF THE MQIO EXECUTION.                      *         
*                                                                     *         
*       AFTER MERGING THE TABLES AND RESETTING THE UNSORTED COUNT,    *         
*       APPLICATION BXLES AROUND ALL THE UTL ENTRIES.                 *         
*       FOR EACH ENTRY AN ATTEMPT TO FIND ANY SORTED DARETAB ENTRIES  *         
*       WHICH MATCH USERID AND ARE MORE RECENT IS MADE IN THE         *         
*       FOLLOWING SEQUENCE.                                           *         
*                                                                     *         
*       1. RDHI FOR USERID ONLY                                       *         
*          NO ENTRIES FOR THIS USERID          - NEXT UTL ENTRY       *         
*          USERID (NO INITIALS)                - SET TIME IF REQUIRED *         
*                                                                     *         
*       2. TEST INITIALS IN USE                                       *         
*          NO INITIALS                         - NEXT UTL ENTRY       *         
*                                                                     *         
*       3. EXACT READ FOR EACH USERID/INITIALS COMBO IN LIST          *         
*          MATCH COMBO                         - SET TIME IF REQUIRED *         
*                                                                     *         
*       4. NEXT UTL ENTRY                                             *         
***********************************************************************         
         SPACE 1                                                                
SETDAR   NTR1  BASE=*,LABEL=*                                                   
         STAR  CLEAR=DARZERO,ARS=OFF                                            
         L     R8,VSSB                                                          
         USING SSBD,R8                                                          
*                                                                               
         XC    DUB,DUB             MOVE & SORT ALL UNSORTED RECORDS             
         MVC   DUB(4),=AL4(DTDARE)                                              
         MVI   DUB,X'80'           THIS WILL BE A LONG ALLOCATE                 
         GOTO1 VLOCKSPC,DUB        LOCK DARE TABLE ENTRY                        
         SAFE  CLEAR=DARZERO                                                    
         ICM   RF,15,DUB+4         WARNING ARS ARE ON FROM HERE                 
         BNZ   *+6                                                              
         DC    H'0'                WHY NO LOCK POSSIBLE?                        
*                                                                               
         XR    R2,R2                                                            
         ICM   R2,15,DSPTFRST-DMSPACED(RF)                                      
         LA    R2,0(R2)            CLEAR HOB                                    
         LAM   R2,R2,SSBTBLET                                                   
         USING TABDARED,R2         R2=BLOCK HEADER INFORMATION                  
*                                                                               
         MVC   PLIST(L'TBDBSP),TBDBSP                                           
         ICM   R3,15,TBDFRST       R3=A(UNSORTED TABLE)                         
         CPYA  R3,R2                                                            
UNSORT   USING TBDDARD,R3                                                       
*                                                                               
         ICM   R0,15,TBDNOW        GET CURRENT COUNT                            
         BZ    SDAR08              NO UNSORTED RECORDS                          
         ST    R0,FULL             SAVE COUNT FOR LATER                         
         B     SDAR04                                                           
*                                                                               
SDAR02   L     R1,FULL             ENTRIES PROCESSED ALREADY                    
         ST    R0,FULL             NEW PROCESSED ENTRY COUNT                    
         SR    R0,R1               R0=REMAINDER REQUIRING PROCESSING            
         BP    *+6                                                              
         DC    H'0'                DEBUG                                        
*                                                                               
UNCOPY   USING TBDDARD,WORK1       COPY UNSORTED ENTRY LOCALLY                  
SDAR04   MVC   UNCOPY.TBDDARD(TBDDARL),UNSORT.TBDDARD                           
         LA    R1,PLIST                                                         
         USING BSPARA,R1                                                        
         MVI   BSPLENR,X'01'       SET ADD IF NOT FOUND                         
         LA    RF,UNCOPY.TBDDARD                                                
         ST    RF,BSPAREC          SET RECORD                                   
         MVI   BSPLENR+1,X'80'     SET USING ACCESS REGISTER                    
         STAM  R2,R2,BSPARS        SET ACCESS REGISTER                          
*                                                                               
         GOTO1 VBINSRCH,(R1),RR=RELO                                            
         SAFE  CLEAR=DARZERO                                                    
         TM    0(R1),X'80'         WAS RECORD ADDED?                            
         BO    SDAR06              YES                                          
         ICM   R4,15,BSPAREC       PICK UP A(SORTED RECORD)                     
         CPYA  R4,R3               AND SEE IF THIS POSTING IS NEWER             
SORTED   USING TBDDARD,R4                                                       
*                                                                               
         CLC   SORTED.TBDTIME,UNCOPY.TBDTIME                                    
         BH    SDAR06                                                           
         MVC   SORTED.TBDTIME,UNCOPY.TBDTIME                                    
         MVC   SORTED.TBDTYPE,UNCOPY.TBDTYPE                                    
*                                                                               
SDAR06   AHI   R3,TBDDARL          NEXT UNSORTED RECORD                         
         BCT   R0,SDAR04                                                        
*                                                                               
* WE HAVE NOW PROCESSED ALL UNSORTED RECORDS.                                   
* SEE IF ANY NEW ONES CAME IN WHILST WE WERE PROCESSING THEM AND IF SO          
* PROCESS THESE NEW ONES ALSO                                                   
*                                                                               
         L     R0,FULL             MAKE SURE NO NEW ENTRIES HAVE BEEN           
         XR    R1,R1               ADDED WHILST PROCESSING SORT TABLE           
         CS    R0,R1,TBDNOW                                                     
         BNE   SDAR02              COUNT HAS CHANGED WHILST SORTING             
         DROP  UNSORT,UNCOPY,SORTED                                             
*                                                                               
SDAR08   LAM   R3,R4,DARZERO       CLEAR DOWN THE ACCESS REGISTERS              
*                                                                               
SDAR10   L     R5,VUTL             LOOP AROUND THE UTL'S                        
         USING UTLD,R5                                                          
         LH    R6,0(R5)            BXLE SETUP                                   
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
*                                                                               
SDAR12   OC    TPRNT,TPRNT         UTL MUST BE A CONNECTED TERMINAL             
         BNZ   SDAR42                                                           
         OC    TUSER,TUSER                                                      
         BZ    SDAR42                                                           
*                                                                               
         XC    WORK1,WORK1         SEE IF POSTINGS FOR THIS USERID              
USERID   USING TBDDARD,WORK1                                                    
         MVC   USERID.TBDUSER,TUSER                                             
*                                                                               
         TM    SSBSYSFL,FACITST    FOR THE TEST SYSTEM                          
         BZ    SDAR14              NEED TO DISTINGUISH REP OR ADV USER          
         CLC   =C'SJ',TAGY         BASED ON AGENCY                              
         BE    SDAR16              ADV USES SJ AND REP USES B3                  
         B     SDAR18                                                           
*                                                                               
SDAR14   TM    SSBSYSFL,FACIREP    REP SYSTEMS USE USERID IN BINARY             
         BO    SDAR18                                                           
*                                                                               
SDAR16   MVC   USERID.TBDAGY,TAGY  WHILE ADV SYSTEMS USE EBCDIC                 
*                                                                               
SDAR18   LA    R1,PLIST                                                         
         USING BSPARA,R1                                                        
         MVI   BSPLENR,X'02'       READ HIGH                                    
         LA    RF,WORK1                                                         
         ST    RF,BSPAREC          A(RECORD)                                    
         MVI   BSPLENR+1,X'80'     USING ARS                                    
         STAM  R2,R2,BSPARS                                                     
                                                                                
         GOTO1 VBINSRCH,(R1),RR=RELO                                            
         SAFE  CLEAR=DARZERO                                                    
         TM    0(R1),X'80'                                                      
         BO    SDAR42              NO ENTRIES FOR THIS USERID                   
*                                                                               
         ICM   R3,15,BSPAREC       PICK UP A(SORTED TABLE ENTRY)                
         CPYA  R3,R2                                                            
SORTED   USING TBDDARD,R3                                                       
*                                                                               
SDAR20   TM    SSBSYSFL,FACITST    FOR THE TEST SYSTEM                          
         BZ    SDAR22              NEED TO DISTINGUISH REP OR ADV USER          
         CLC   =C'SJ',TAGY         BASED ON AGENCY                              
         BE    SDAR24              ADV USES SJ AND REP USES B3                  
         B     SDAR26                                                           
*                                                                               
SDAR22   TM    SSBSYSFL,FACIREP    REP SYSTEMS USE USERID IN BINARY             
         BO    SDAR26                                                           
*                                                                               
SDAR24   CLC   SORTED.TBDAGY,TAGY  ADV SYSTEMS USER USERID IN EBCDIC            
         BE    SDAR28                                                           
         B     SDAR42              NO RECORDS FOR THIS USERID                   
*                                                                               
SDAR26   CLC   SORTED.TBDUSER,TUSER                                             
         BNE   SDAR42              NO RECORDS FOR THIS USERID                   
*                                                                               
SDAR28   OC    SORTED.TBDINIT,SORTED.TBDINIT                                    
         BNZ   SDAR30              NO GLOBAL USERID POSTING                     
         CLC   TTIMEDAR,SORTED.TBDTIME                                          
         BNL   SDAR30              POSTING IS OLDER THAN UTL SETTING            
*                                                                               
         MVC   TTIMEDAR,SORTED.TBDTIME                                          
         MVC   TDARETYP,SORTED.TBDTYPE                                          
         OI    TSTATU,TSTATD4U     COPY TIME AND SET FLAG                       
         MVC   TDAREUSR,TUSER      FOR =CT                                      
         DROP  USERID,SORTED                                                    
*                                                                               
SDAR30   ICM   R4,15,TAINITS       ARE WE USING INITIALS SCHEME?                
         BZ    SDAR42              NO - NEXT UTL ENTRY                          
*                                                                               
         CPYA  R4,R2                                                            
         USING ASSISTD,R4          R4=INITIALS SCHEME ENTRY                     
         XR    R0,R0                                                            
         ICM   R0,1,ASSCNT         COUNT OF INITIAL SETS                        
         BZ    SDAR42              NONE CURRENTLY                               
         LA    R4,ASSINITS         R4=FIRST INITIAL SET IN LIST                 
*                                                                               
COMBO    USING TBDDARD,WORK1                                                    
SDAR32   XC    WORK1,WORK1         SEE IF POSTING FOR USERID/INITIALS           
         MVC   COMBO.TBDUSER,TUSER                                              
*                                                                               
         TM    SSBSYSFL,FACITST    FOR THE TEST SYSTEM                          
         BZ    SDAR34              NEED TO DISTINGUISH REP OR ADV USER          
         CLC   =C'SJ',TAGY         BASED ON AGENCY                              
         BE    SDAR36              ADV USES SJ AND REP USES B3                  
         B     SDAR38                                                           
*                                                                               
SDAR34   TM    SSBSYSFL,FACIREP    REP SYSTEMS USE USERID IN BINARY             
         BO    SDAR38                                                           
*                                                                               
SDAR36   MVC   COMBO.TBDAGY,TAGY   WHILE ADV SYSTEMS USE EBCDIC                 
*                                                                               
SDAR38   MVC   COMBO.TBDINIT,0(R4)                                              
*                                                                               
         LA    R1,PLIST                                                         
         USING BSPARA,R1                                                        
         MVI   BSPLENR,X'00'       READ EXACT                                   
         LA    RF,COMBO.TBDDARD                                                 
         ST    RF,BSPAREC          A(RECORD)                                    
         MVI   BSPLENR+1,X'80'     USING ARS                                    
         STAM  R2,R2,BSPARS                                                     
                                                                                
         GOTO1 VBINSRCH,(R1),RR=RELO                                            
         SAFE  CLEAR=DARZERO                                                    
         TM    0(R1),X'80'         NO ENTRIES FOR THIS COMBINATION              
         BO    SDAR40              YES                                          
*                                                                               
         ICM   R3,15,BSPAREC       PICK UP A(SORT TABLE ENTRY)                  
         CPYA  R3,R2                                                            
SORTED   USING TBDDARD,R3                                                       
*                                                                               
         CLC   TTIMEDAR,SORTED.TBDTIME                                          
         BNL   SDAR40              POSTING IS OLDER THAN UTL SETTING            
         MVC   TTIMEDAR,SORTED.TBDTIME                                          
         MVC   TDARETYP,SORTED.TBDTYPE                                          
         OI    TSTATU,TSTATD4U     COPY TIME AND SET FLAG                       
         MVC   TDAREUSR,TUSER      FOR =CT                                      
*                                                                               
SDAR40   AHI   R4,L'ASSINITS       NEXT SET OF INITIALS                         
         BCT   R0,SDAR32                                                        
*                                                                               
SDAR42   BXLE  R5,R6,SDAR12        NEXT UTL ENTRY                               
         DROP  R4,R5,SORTED,COMBO                                               
         MVC   TBDBSP,PLIST        SAVE OFF NEW BINSRCH PLIST                   
         DROP  R2                                                               
*                                                                               
         REAR  ARS=OFF                                                          
         XC    DUB,DUB             FREE DATASPACE                               
         MVC   DUB(4),=AL4(DTDARE)                                              
         MVI   DUB,X'10'                                                        
         GOTO1 VLOCKSPC,DUB        UNLOCK DARE TABLE ENTRY                      
*                                                                               
SDARX    LAM   R0,RF,DARZERO                                                    
         XIT1  ,                                                                
*                                                                               
VBINSRCH DC    V(BINSRCH)                                                       
DARZERO  DC    16F'0'                                                           
         LTORG                                                                  
*                                                                               
         DROP  R1,R8,R9,RB                                                      
         EJECT                                                                  
*&&                                                                             
************************************************************                    
*        CHECK COMMAND TABLE FOR UN-PROCESSED COMMANDS     *                    
************************************************************                    
         SPACE 1                                                                
CHKCOM   NTR1  BASE=*,LABEL=*                                                   
         USING SYSFACD,R9                                                       
         XC    WORK,WORK                                                        
*                                                                               
         L     RE,VSSB                                                          
         LAM   R2,R2,SSBALET-SSBD(RE)                                           
         SR    R2,R2                                                            
         USING DMDHDR,R2                                                        
         SAC   512                                                              
         L     R2,DHACOMM          SET R2 TO COMMS BLOCK                        
         LA    R0,128                                                           
         USING DSCOMM,R2                                                        
*                                                                               
CHKCO010 OC    DSCDEST,DSCDEST     IS THERE A COMMAND                           
         BZ    CHKCO090                                                         
         LA    R1,WORK             BUILD A LIST IN WORK                         
CHKCO020 CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   0(1,R1),DSCDEST     ADD THIS ENTRY                               
         B     CHKCO090                                                         
         CLC   0(1,R1),DSCDEST     IGNORE IF ALREADY IN LIST                    
         BE    CHKCO090                                                         
         LA    R1,1(R1)                                                         
         B     CHKCO020                                                         
*                                                                               
CHKCO090 LA    R2,32(,R2)          POINT TO NEXT COMM ENTRY                     
         BCT   R0,CHKCO010         NEXT COMMAND                                 
*                                                                               
CHKCOX   EQU   *                                                                
         SR    R2,R2                                                            
         USING DMDSHDR,R2                                                       
         L     R2,DHAADVS                                                       
         LA    R0,32                                                            
CHKCO100 CLI   0(R2),0                                                          
         BE    CHKCO190                                                         
         LA    R1,WORK                                                          
*                                                                               
CHKCO110 CLI   0(R1),0                                                          
         BE    CHKCO190                                                         
         CLC   10(1,R2),0(R1)                                                   
         BNE   *+18                                                             
         MVC   WORK1(32),0(R2)                                                  
         SAC   0                                                                
         BAS   RE,POSTIT                                                        
         SAC   512                                                              
         LA    R1,1(R1)                                                         
         B     CHKCO110                                                         
*                                                                               
CHKCO190 LA    R2,32(,R2)                                                       
         BCT   R0,CHKCO100                                                      
         SAC   0                                                                
         XIT1                                                                   
         DROP  R2                                                               
*************************************************************                   
*        DO X MEMORY POST TO FACPAK (DETAILS IN CARD)       *                   
*************************************************************                   
         SPACE 1                                                                
POSTIT   NTR1  ,                                                                
         LH    R4,WORK1+12         ASID IS AT +12                               
         L     R2,WORK1+16         SSBOPECB IS AT +16                           
         LOCASCB ASID=(R4)                                                      
         LR    R3,R1                                                            
         LTR   RF,RF                                                            
         BNZ   NOPOST                                                           
         USING ASCB,R3                                                          
         CLC   ASCBASCB,=C'ASCB'                                                
         BNE   NOPOST                                                           
         L     R4,ASCBASSB                                                      
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
         L     R4,ASSBJSAB-ASSB(R4) R4 = A(JSAB)                                
         USING JSAB,R4                                                          
         CLC   WORK1(8),JSABJBNM                                                
         LA    RE,*+6              SWITCH BACK TO 24 BT MODE                    
         BSM   0,RE                                                             
         BNE   NOPOST                                                           
*                                                                               
         LA    R5,99               SET COMPLETION CODE                          
         POST  (R2),(R5),ASCB=(R3),LINKAGE=SYSTEM,ECBKEY=8,MF=(E,POSTX)         
         B     POSTXX                                                           
NOPOST   MVC   WORK1+10(8),WORK1                                                
         MVC   WORK1+2(8),WORK1+10                                              
         MVC   WORK1+10(18),=C' SYSTEM NOT POSTED'                              
         MVC   WORK1+0(2),=H'26'                                                
         WTO   TEXT=WORK1                                                       
         B     POSTXX                                                           
POSTX    POST  ERRET=DEAD,ECBKEY=YES,MF=L                                       
DEAD     DC    H'0'                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
POSTXX   XIT1                                                                   
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TEMP W/S                                             *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
FUL1     DS    F                                                                
DMCB     DS    6F                                                               
PLIST    DS    8F                                                               
VGENIDS  DS    V                                                                
RELO     DS    A                                                                
APRTLST  DS    A                                                                
APRTLSTX DS    A                                                                
SVREGS   DS    2A                                                               
SAVER1   DS    A                                                                
SAVERD   DS    A                                                                
SAVERDO  DS    A                                                                
ASRTXT   DS    A                                                                
JNUM     DS    F                   N'ENTRIES IN JTAB                            
AJTAB    DS    A                   A(JTAB)                                      
AJCLASS  DS    A                   A(JOB CLASS TABLE)                           
ACIREC   DS    A                                                                
AIO      DS    A                                                                
AIO2     DS    A                                                                
AJOBTAB  DS    A                                                                
JOBHDRL  DS    H                                                                
JOBTABL  DS    H                                                                
*                                                                               
SRPARAS  DS    0F                                                               
SRPARA1  DS    A                   A(SYSFACS)                                   
SRPARA2  DS    A                   A(TIA)                                       
SRPARA3  DS    A                   A(UTL ENTRY)                                 
SRPARA4  DS    A                   A(COMFACS)                                   
SRPARA5  DS    A                   A(SELIST ENTRY)                              
SRPARA6  DS    A                   A(TWA)                                       
SRPARAL  EQU   *-SRPARAS                                                        
*                                                                               
LOGREC   DS    0CL64                                                            
LOGID    DS    CL4                                                              
LOGLUID  DS    CL8                                                              
LOGTIME  DS    PL4                                                              
LOGNUM   DS    CL4                                                              
LOGINFO  DS    CL5                                                              
LOGREPU  DS    CL6                                                              
LOGREPI  DS    CL3                                                              
LOGRENO  DS    CL4                                                              
*                                                                               
LOGSYSID DS    XL1                                                              
         DS    XL1                                                              
LOGNAME  DS    CL4                                                              
LOGJLND  DS    PL4                                                              
LOGDATE  DS    CL8                                                              
         DS    XL8                                                              
*                                                                               
TIMEHMSL DS    F                                                                
TIMEHMS  DS    F                                                                
MTCHNUM  DS    F                                                                
PRTNUM   DS    F                                                                
OUTCNT   DS    H                                                                
NOTIFY   DS    X                                                                
FLAG     DS    X                                                                
FRST     DS    X                                                                
MODE     DS    X                                                                
RSTRDONE DS    X                                                                
SYSID    DS    X                                                                
SYSIDHOB DS    X                                                                
         DS    XL3                                                              
SYSNAME  DS    CL4                                                              
SYSNAM4  DS    CL4                                                              
SYSDATE  DS    CL8                                                              
RECLEN   DS    H                                                                
HALF     DS    H                                                                
*                                                                               
ABCTAB   DS    A                                                                
ABCTABX  DS    A                                                                
ABCNXT   DS    A                                                                
NEWBC    DS    XL1                                                              
TIMEB    DS    XL3                                                              
YEARB    DS    XL1                                                              
MONTHB   DS    XL1                                                              
         DS    XL2                                                              
DAY      DS    X                   DEJAVU TYPE DAY VALUE (TODAY)                
         DS    X                   SPARE (TO FORCE ALIGNMENT)                   
DATEC    DS    XL2                                                              
YEARC    DS    XL2                                                              
YEARCM1  DS    XL2                                                              
YEARCP1  DS    XL2                                                              
STRDC    DS    XL2                                                              
ENDDC    DS    XL2                                                              
TIMEC    DS    H                                                                
*                                                                               
AMSGBUFF DS    A                                                                
REGSAVE  DS    4F                                                               
*                                                                               
DMWORK   DS    10F                                                              
DSKADR   DS    F                                                                
KEY      DS    CL32                                                             
DIR      DS    CL40                I/O AREA FOR GENDIR READS                    
WORK1    DS    CL64                                                             
WORK     DS    CL64                                                             
         SPACE 1                                                                
APRTQLST DS    A                                                                
PRTQID   DS    CL8                                                              
NDX      DS    CL40                                                             
* DMPRTQW                                                                       
       ++INCLUDE DMPRTQW                                                        
*                                                                               
WORKX1   DS    D                   END OF PART 1 W/S (MUST BE DUB)              
*                                                                               
CXREC    DS    14336C              PRTQ BUFFERS                                 
CIREC    DS    14336C                                                           
         ORG   CXREC                                                            
FWDMCB   DS    0XL24               FACWRK DMCB                                  
FWAACTN  DS    A                                                                
FWAFILE  DS    A                                                                
FWANDX   DS    A                                                                
FWAREC   DS    A                                                                
FWABUF   DS    A                                                                
         DS    A                                                                
*                                                                               
FWNDX    DS    XL32                FACWRK INDEX                                 
FWREC    DS    2076C               FACWRK RECORD                                
FWBUF    DS    4096C               FACWRK BUFFER                                
FIREC    DS    2076C               FILE RECORD                                  
         ORG                                                                    
PRTLST   DS    2000F                                                            
PRTLSTX  DS    F                                                                
*                                                                               
JTAB     DS    (JMAX+1)XL(JTABL)                                                
IO       DS    2000C               I/O AREA FOR CTFILE/GENFILE READS            
IO2      DS    2000C               I/O AREA FOR CTFILE/GENFILE READS            
*                                                                               
WORKX    EQU   *                   END OF TOTAL W/S                             
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER JOB NOTIFY TABLE                                     *         
***********************************************************************         
         SPACE 1                                                                
JTABD    DSECT                                                                  
JSTAT    DS    XL1                 JOB STATUS                                   
JTERM    DS    XL2                 TERMINAL NUMBER                              
JSOUT    EQU   X'80'               JOB COMPLETE                                 
JSINV    EQU   X'40'               JOB COMPLETE BUT INVALID (ABEND)             
JSUPD    EQU   X'20'               JOB COMPLETE EXTRACT UPDATE PENDING          
JSNPQ    EQU   X'10'               JOB NOT IN PRTQUE ANY MORE                   
JPQKEY   DS    0XL7                PRTQUE KEY                                   
JPQUSR   DS    XL2                 USER-ID                                      
JPQSUB   DS    CL3                 REPORT SUB-ID                                
JPQSEQ   DS    XL2                 REPORT SEQUENCE NUMBER                       
JPQCIA   DS    XL2                 REPORT C/I ADDRESS                           
JPQID    DS    CL1                 PRTQUE FILE ID                               
JJESNO   DS    XL2                 JES JOB NUMBER                               
JTABL    EQU   *-JTABD             L'TABLE ENTRY                                
JMAX     EQU   1999                MAXIMUM N'ENTRIES IN TABLE                   
*                                                                               
QTABD    DSECT                                                                  
QCLASS   DS    XL2                 CLASS                                        
QUSER    DS    XL2                 USER ID                                      
QTIME    DS    XL3                 SUBMIT TIME                                  
QSPARE   DS    XL1                 SPARE                                        
QOFFS    DS    XL4                 OFFSET                                       
QTABL    EQU   *-QTABD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER FACWRK RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
FWRECD   DSECT                                                                  
FWRLEN   DS    XL2                 RECORD LENGTH  (INCLUDES HDR DATA)           
         DS    XL2                                                              
* DMRCVRHDR                        RECORD RECOVERY HEADER                       
       ++INCLUDE DMRCVRHDR                                                      
FWRDATA  DS    X                   RECORD DATA                                  
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* FABCTAB                                                                       
       ++INCLUDE FABCTAB                                                        
         EJECT                                                                  
* CTGENBRD                                                                      
       ++INCLUDE CTGENBRD                                                       
         EJECT                                                                  
SRTXTFFD DSECT                                                                  
         DS    CL64                                                             
* SRTXTFFD                                                                      
       ++INCLUDE SRTXTFFD                                                       
         EJECT                                                                  
* DMDSHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* DDDICTBLDD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDDICTBLDD                                                     
         PRINT ON                                                               
* FAPRQ                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAPRQ                                                          
         PRINT ON                                                               
* DMPRTQD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQD                                                        
         PRINT ON                                                               
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKRK                                                        
         PRINT ON                                                               
* DMWRKRD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKRD                                                        
         PRINT ON                                                               
* DDWKSCAND                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDWKSCAND                                                      
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
* FACIDTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FATABSDEQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDEQU                                                     
         PRINT ON                                                               
* FATABSDAR                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSDAR                                                      
         PRINT ON                                                               
* DDASSISTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDASSISTD                                                      
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* DDBSPARA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBSPARA                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* FAMSGBUFFD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FAMSGBUFFD                                                     
         PRINT ON                                                               
* FATABSJOB                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSJOB                                                      
         PRINT ON                                                               
*DDTEMPREC                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDTEMPREC                                                      
         PRINT ON                                                               
         EJECT                                                                  
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049SRTIM00S  01/18/02'                                      
         END                                                                    
