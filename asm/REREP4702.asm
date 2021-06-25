*          DATA SET REREP4702  AT LEVEL 112 AS OF 09/27/02                      
*PHASE RE4702A,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'RE4702 - REREP4702 - NEW COMMISSION REPORT'                     
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP4702 --- NEW REQUESTABLE COMMISSION REPORT            *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JUL10/89 (MRR) --- INITIAL RELEASE                                *           
*                                                                   *           
* AUG28/89 (MRR) --- MAKE AS AT DATE WORK                           *           
*                                                                   *           
* FEB07/90 (MRR) --- ADD REG DROPS THAT WERE EFFECTING COMM ROUTINES*           
*                                                                   *           
* FEB28/90 (MRR) --- HEADHOOK ADDRESSABILITY BUG                    *           
*                                                                   *           
* MAY18/90 (MRR) --- ADD DOWNLOADING                                *           
*                                                                   *           
* DEC14/90 (MRR) --- CHANGE POSTING FOR FIRST MONTH TO NET OF FIRST *           
*                     MONTH PLUS PRIOR                              *           
*                                                                   *           
* MAR19/91 (MRR) --- BUG IN FIXPOST                                 *           
*                                                                   *           
* MAY16/91 (MRR) --- BUG IN FIXPOST                                 *           
*                                                                   *           
* JUN05/91 (MRR) --- BUG IN FIXPOST                                 *           
*                                                                   *           
* JUL16/91 (MRR) --- BUG IN FIXPOST                                 *           
*                                                                   *           
* AUG12/91 (MRR) --- BUG IN FIXPOST                                 *           
*                                                                   *           
* NOV22/91 (BU ) --- INCORPORATE VALUENEW IN PLACE OF VALUEMON      *           
*                                                                   *           
* JAN22/92 (MRR) --->CHANGE 'LAST MONTH' COLUMN TO REFLECT CHANGES  *           
*                     ONLY                                          *           
*                   >ZERO COMM ERROR RECAP                          *           
*                                                                   *           
* MAR27/92 (BU ) --- MAKE COMPATIBLE WITH VALU2NEW                  *           
*                    REREPRGEQU ---> REREPRGEQA                     *           
*                                                                   *           
* MAY13/92 (BU ) --- IF NO ACTIVITY FOR A STATION, DON'T PRINT.     *           
*                    IF NO ACTIVITY FOR A GROUP, SKIP IT ALSO.      *           
*                                                                   *           
* SEP10/92 (BU ) --- FIX 'FIXPOST':  INVOICE-ELEMENT-ONLY PROCESS   *           
*                    NOT ACCUMULATING WITHIN MONTH                  *           
*                                                                   *           
* MAY18/93 (BU ) --- DISPLAY/DON'T DISPLAY $$ WHEN $0 COMMISSION    *           
*                    RATE FOUND.                                    *           
*                                                                   *           
* AUG20/93 (BU ) --- SPECIAL TEST FOR CONTRACT WITH WRONG SUBGROUP  *           
*                                                                   *           
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                *           
*                                                                   *           
* APR11/96 (BU ) --- FOR CABALLERO (I8): IF AGY = MISC AND ADV =    *           
*                    MIS, DON'T POST BUCKETS WHERE DATE < 11/95.    *           
*                                                                   *           
* APR30/96 (BU ) --- CHANGE CUTOFF DATE FOR ABOVE CHANGE TO < 10/95.*           
*                                                                   *           
* JUN17/96 (BU ) --- BACK OUT ABOVE CHANGE:  PERMANENTLY            *           
*                                                                   *           
* MAY22/97 (BU ) --- UPGRADE FOR YR 2000                            *           
*                                                                   *           
* NOV25/97 (BU ) --- EXPAND SIZE OF OFFTAB                          *           
*                                                                   *           
* JAN26/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
* AUG09/99 (BU ) --- TEST VERSION                                   *           
*                                                                   *           
* SEP27/02 (BU ) --- DON'T DOWNLOAD 'COMMISSION MISSING' ERRORS     *           
*                                                                   *           
*                                                                   *           
*                    **  END TOMBSTONE  **                          *           
*********************************************************************           
*                                                                               
RE4702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE4702,R9,R8,RR=RE                                           
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         STM   R2,RC,SAVEREGS      SAVE REGS 2 -> C                             
         EJECT                                                                  
*                                                                               
*        CHECK AND PROCESS MODE SETTINGS                                        
*                                                                               
         LA    R1,MODETAB          POINT TO MODE/PROC TABLE                     
         ZIC   R2,0(R1)            GET NUMBER OF ENTRIES                        
         ZIC   R3,1(R1)            GET LENGTH OF EACH ENTRY                     
         LA    R1,2(R1)            POINT TO 1ST ENTRY                           
         ZIC   R0,MODE             GET CURRENT MODE                             
MAIN10   EQU   *                                                                
         ZIC   RF,0(R1)            GET TABLE ENTRY MODE                         
         CR    R0,RF               GOT IT?                                      
         BNE   MAIN20              NO, CHECK NEXT                               
         ZICM  RF,1(R1),3          YES, GET THE ROUTINE ADDR                    
         GOTO1 (RF)                GO TO THE ROUTINE                            
         BZ    MAIN30              ZERO IS GOOD RETURN                          
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    MAINBAD             YES - DON'T OUTPUT ERROR MESSAGES            
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
         MVC   P(24),=C'>>> PROCESSING ERROR <<<'                               
         GOTO1 LOCALREP,DMCB,ERRORLIN                                           
         B     MAINBAD                                                          
MAIN20   EQU   *                                                                
         AR    R1,R3               POINT TO THE NEXT ENTRY                      
         BCT   R2,MAIN10           LOOP                                         
*                                                                               
MAIN30   EQU   *                                                                
*                                                                               
*        MAIN COMMON EXIT                                                       
*                                                                               
MAINGOOD EQU   *                                                                
         B     TESTGOOD                                                         
MAINBAD  EQU   *                                                                
         B     TESTBAD                                                          
         LA    RF,RCDNLOAD                                                      
         EJECT                                                                  
*                                                                               
*        MODE/PROCESS ROUTINE TABLE                                             
*                                                                               
*                                                                               
*        CL1  -  NUMBER OF ENTRIES                                              
*        CL1  -  LENGTH OF ONE ENTRY                                            
*        CL4  -  ENTRY:                                                         
*                  CL1 - MODE                                                   
*                  CL3 - ROUTINE ADDRESS                                        
*                                                                               
*                                                                               
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
         DC    AL1(REQFRST),AL3(INITIAL)    REQUEST FIRST                       
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
         DC    AL1(SUBFRST),AL3(GRUPINIT)   START OF SUBGROUP                   
         DC    AL1(STAFRST),AL3(STAINIT)    STATION FIRST                       
         DC    AL1(PROCCONT),AL3(POST)      PROCESS A CONTRACT                  
         DC    AL1(STALAST),AL3(STADONE)    STATION BREAK                       
         DC    AL1(SUBLAST),AL3(GRUPDONE)   SUB-GROUP BREAK                     
         DC    AL1(REQLAST),AL3(RPTDONE)    END OF REPORT                       
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*                                                                               
*        INITIAL --- PROCESS TO START THE REPORT                                
*                                                                               
*        - READ FOR OFFICES AND BUILD OFFICE TABLES                             
*        - SET DATES FOR BUCKETING THE CONTRACT DOLLARS                         
*        - SET INITIAL REPORT VALUES                                            
*                                                                               
INITIAL  NTR1                                                                   
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFASTART)                                
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFAEND)                                  
*                                  CONVERT ORIGINAL EBCDIC DATE TO              
*                                     SPECIAL YR 2000 FORMAT                    
         BAS   RE,INITADDR                                                      
         BNZ   INITBAD                                                          
         BAS   RE,BLDOFF                                                        
         BNZ   INITBAD                                                          
         BAS   RE,SETACTIV                                                      
         BNZ   INITBAD                                                          
         BAS   RE,INITVALS                                                      
         BNZ   INITBAD                                                          
*                                                                               
         OPEN  (RRECAP,(OUTPUT))                                                
         LTR   RF,RF                                                            
         BZ    INIT10                                                           
         DC    H'0'                                                             
INIT10   EQU   *                                                                
*                                                                               
INITGOOD EQU   *                                                                
         B     TESTGOOD                                                         
INITBAD  EQU   *                                                                
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    TESTBAD             YES - DON'T SEND ERROR MESSAGES              
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
         MVC   P(37),=C'>>> ERROR IN INITIALIZING PROGRAM <<<'                  
         GOTO1 LOCALREP,DMCB,ERRORLIN                                           
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        GRUPINIT --- PROCESS FOR FIRST/CHANGE OF SUBGROUP                      
*                                                                               
GRUPINIT NTR1                                                                   
*                                                                               
         MVC   GRPNAME(10),RGRPNAME                                             
         MVC   SGRPNAME(10),RGRPSBNM                                            
         CLC   =C'$$BILL',QUESTOR  SPECIAL TEST REQUESTOR?                      
         BNE   TESTGOOD                                                         
         MVC   P+1(5),=C'GROUP'                                                 
         BAS   RE,DISPCON          YES - SHOW CONTRACT CAUSING BREAK            
*                                                                               
         B     TESTGOOD                                                         
*                                                                               
*    SPECIAL DISPLAY ROUTINE                                                    
*                                                                               
DISPCON  NTR1                                                                   
         MVC   P+10(10),=C'CONTRACT: '                                          
         GOTO1 HEXOUT,DMCB,RCONKCON,P+21,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         B     TESTGOOD                                                         
*                                                                               
                                                                                
*        STAINIT --- PROCESS FOR FIRST ENCOUNTER OF A STATION                   
*                                                                               
*        - READ COMMISSION RATE RECORDS FOR A STATION AND BUILD                 
*           A TABLE                                                             
*                                                                               
*                                                                               
STAINIT  NTR1                                                                   
*                                                                               
         MVC   CURSTA(5),RSTAKSTA                                               
         BAS   RE,BLDSTA                                                        
         BNZ   STAIBAD                                                          
*                                                                               
STAIGOOD EQU   *                                                                
         CLC   =C'$$BILL',QUESTOR  SPECIAL TEST REQUESTOR?                      
         BNE   TESTGOOD                                                         
         MVC   P+1(7),=C'STATION'                                               
         BAS   RE,DISPCON          YES - SHOW CONTRACT CAUSING BREAK            
*                                                                               
         B     TESTGOOD                                                         
STAIBAD  EQU   *                                                                
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    TESTBAD             YES - DON'T SEND ERROR MESSAGES              
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
         MVC   P(42),=C'>>> ERROR IN BUILDING COMMISSION TABLE <<<'             
         GOTO1 LOCALREP,DMCB,ERRORLIN                                           
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        STADONE --- PROCESS TO FINISH UP A STATION                             
*                                                                               
*                                                                               
*        - PRINT THE STATION/OFFICE TABLE                                       
*        - ROLL DATA: STATION -> GROUP                                          
*                                                                               
STADONE  NTR1                                                                   
*                                                                               
         CLI   STADATA,C'Y'        IS THERE DATA FOR STATION?                   
         BNE   STADGOOD            NO  - DON'T PRINT ANYTHING                   
*                                                                               
         MVI   GRPDATA,C'Y'        YES - INDICATE GROUP DATA ALSO               
*                                                                               
         MVI   STADATA,C'N'        TURN OFF DATA FLAG                           
*                                     AND PRINT THE DATA                        
         GOTO1 DOPRINT,DMCB,1                                                   
         BNZ   STADBAD                                                          
         GOTO1 ROLLTOT,DMCB,1                                                   
         BNZ   STADBAD                                                          
*                                                                               
STADGOOD EQU   *                                                                
         B     TESTGOOD                                                         
STADBAD  EQU   *                                                                
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    TESTBAD             YES - DON'T SEND ERROR MESSAGES              
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
         MVC   P(42),=C'>>> ERROR IN PROCESSING STATION TOTALS <<<'             
         GOTO1 LOCALREP,DMCB,ERRORLIN                                           
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        GRUPDONE --- PROCESS TO FINISH UP A GROUP                              
*                                                                               
*                                                                               
*        - PRINT THE GROUP/OFFICE TABLE                                         
*        - ROLL DATA: GROUP -> REPORT                                           
*                                                                               
GRUPDONE NTR1                                                                   
*                                                                               
         CLI   GRPDATA,C'Y'        IS THERE DATA FOR GROUP?                     
         BNE   GRPDGOOD            NO  - DON'T PRINT ANYTHING                   
*                                                                               
         MVI   GRPDATA,C'N'        TURN OFF DATA FLAG                           
*                                     AND PRINT THE DATA                        
         GOTO1 DOPRINT,DMCB,2                                                   
         BNZ   GRPDBAD                                                          
         GOTO1 ROLLTOT,DMCB,2                                                   
         BNZ   GRPDBAD                                                          
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
GRPDGOOD EQU   *                                                                
         B     TESTGOOD                                                         
GRPDBAD  EQU   *                                                                
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    TESTBAD             YES - DON'T SEND ERROR MESSAGES              
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
         MVC   P(40),=C'>>> ERROR IN PROCESSING GROUP TOTALS <<<'               
         GOTO1 LOCALREP,DMCB,ERRORLIN                                           
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        RPTDONE --- PROCESS TO FINISH THE REPORT                               
*                                                                               
*                                                                               
*        - PRINT THE REPORT/OFFICE TABLE                                        
*        - FINISH THE REPORT                                                    
*                                                                               
RPTDONE  NTR1                                                                   
*                                                                               
         MVC   SGRPNAME(10),SPACES     NO SUB-GROUP NAME ON RPT TOT             
         GOTO1 DOPRINT,DMCB,3                                                   
         BNZ   GRPDBAD                                                          
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         MVC   P(27),=C'--- ERROR RECAP SECTION ---'                            
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         CLOSE (RRECAP)                                                         
         SR    R2,R2                                                            
         OPEN  (RRECAP,(INPUT))                                                 
         LTR   RF,RF                                                            
         BNZ   RPTDEOF                                                          
RPTD10   EQU   *                                                                
         GET   RRECAP,P                                                         
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         LR    R2,RE                                                            
         B     RPTD10                                                           
*                                                                               
RPTDEOF  EQU   *                                                                
         LTR   R2,R2                                                            
         BNZ   RPTDEOF1                                                         
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         MVC   P+2(25),=C'NO RUN ERRORS ENCOUNTERED'                            
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
RPTDEOF1 EQU   *                                                                
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         MVC   P(16),=C'* END OF RECAP *'                                       
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         CLOSE (RRECAP)                                                         
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
         MVC   P+2(21),=C'*** END OF REPORT ***'                                
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
*                                                                               
RPTDGOOD EQU   *                                                                
         B     TESTGOOD                                                         
RPTDBAD  EQU   *                                                                
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    TESTBAD             YES - DON'T SEND ERROR MESSAGES              
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
         MVC   P(41),=C'>>> ERROR IN PROCESSING REPORT TOTALS <<<'              
         GOTO1 LOCALREP,DMCB,ERRORLIN                                           
         B     TESTBAD                                                          
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
*                                                                               
*        POST --- PROCESS A CONTRACT                                            
*                                                                               
         SPACE 2                                                                
POST     NTR1                                                                   
***      BAS   RE,DISPCON          DISPLAY ALL CONTRACTS                        
*                                                                               
         XC    FRSTIME,FRSTIME     SET FIRST-TIME = YES                         
         L     R2,ANEWMON          A(NEW MONTH TABLE)                           
         XC    ADJGROS(24),ADJGROS CLEAR ACCUMULATORS                           
POST02   EQU   *                                                                
*                                                                               
*        B     POST10              BYPASS SPECIAL TEST                          
*                                                                               
*   SPECIAL TEST FOR CABALLERO:  IF BACK BILLING, WHERE AGY = MISC,             
*        ADV = MIS, AND MONTH IS PRE-OCT95, DON'T POST IT.                      
*                                                                               
*        CLC   =C'I8',RCONKREP     CABALLERO?                                   
*        BNE   POST10              NOT I8                                       
*        CLC   =C'MISC',RCONKAGY   YES - BACK BILLING AGENCY?                   
*        BNE   POST10              NO                                           
*        CLC   =C'MIS',RCONKADV    YES - BACK BILLING ADVERTISER?               
*        BNE   POST10              NO                                           
*                                                                               
*   TEST                                                                        
*        MVC   P+1(4),=C'PRE:'                                                  
*        MVC   P+10(2),RCONKREP                                                 
*        MVI   P+13,C'/'                                                        
*        MVC   P+14(4),RCONKAGY                                                 
*        MVI   P+18,C'/'                                                        
*        MVC   P+19(4),RCONKADV                                                 
*        MVI   P+23,C'/'                                                        
*        MVC   P+24(4),0(R2)                                                    
*        GOTO1 HEXOUT,DMCB,RCONKCON,P+30,4,=C'TOG'                              
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
*        CLC   0(4,R2),=C'9510'    YES - PRE-OCT95?                             
*        BL    POST12              YES - DON'T POST THIS VALUE                  
*        CLC   0(4,R2),QSTART      NO  - TABLE ENTRY VS REQ START DATE          
*        BH    POST20              PASSED - BEGIN TO PULL DATA                  
POST10   EQU   *                                                                
         CLC   0(4,R2),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST20              FOUND - BEGIN TO PULL DATA                   
         BH    POST100             SHOULDN'T HAPPEN                             
POST12   EQU   *                                                                
         LA    R2,NEXTBUCK(R2)     BUMP TO NEXT BUCKET                          
         B     POST02              GO BACK FOR NEXT                             
*                                                                               
POST20   EQU   *                                                                
         CLI   0(R2),0                                                          
         BE    POST100                                                          
         CLC   0(4,R2),QFAEND      TABLE ENTRY VS REQ END   DATE                
         BH    POST100             TABLE > END DATE - EXIT                      
*                                                                               
*   TEST                                                                        
*        MVC   P+1(5),=C'POST:'                                                 
*        MVC   P+10(2),RCONKREP                                                 
*        MVI   P+13,C'/'                                                        
*        MVC   P+14(4),RCONKAGY                                                 
*        MVI   P+18,C'/'                                                        
*        MVC   P+19(4),RCONKADV                                                 
*        MVI   P+23,C'/'                                                        
*        MVC   P+24(4),0(R2)                                                    
*        GOTO1 HEXOUT,DMCB,RCONKCON,P+30,4,=C'TOG'                              
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         LR    R6,R2               SET R6 TO A(BUCKETS IN MONTH)                
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROLS                          
         CLC   0(4,R2),QFAEND      MONTH OF REPORT?                             
         BE    POST60                                                           
         CLC   NEXTBUCK(4,R2),QFAEND                                            
*                                  MONTH PRIOR TO MONTH OF REPORT?              
         BE    POST40                                                           
*                                                                               
*                                  PRIOR MONTHS ADJUSTMENTS                     
         CLI   FRSTIME,X'00'       FIRST MONTH ?                                
         BNE   POST30                                                           
         MVI   FRSTIME,X'01'       SET FIRST-TIME = NO                          
         XC    FULL,FULL                                                        
         BAS   RE,FIXPOST                                                       
         L     R4,FULL                                                          
         B     POST35                                                           
POST30   EQU   *                                                                
         L     R4,PRASATOR(R6)     GET AMOUNT                                   
POST35   EQU   *                                                                
         LTR   R4,R4                                                            
         BZ    POST80                                                           
         LR    R5,R4                                                            
         A     R5,ADJGROS                                                       
         ST    R5,ADJGROS                                                       
         MVC   P6+3(1),QOPTION2    SET $0 COMM BILLING                          
         GOTO1 DOCOMM,DMCB,(R2),(R4),ADJCOMM,FULL,1                             
         BNZ   POSTBCOM                                                         
         B     POST80                                                           
*                                                                               
POST40   EQU   *                   LAST MONTH'S ACTUAL                          
         L     R4,PRASATOR(R6)     GET AMOUNT                                   
POST45   EQU   *                                                                
         LTR   R4,R4                                                            
         BZ    POST80                                                           
         LR    R5,R4                                                            
         A     R5,ACTGROS                                                       
         ST    R5,ACTGROS                                                       
         MVC   P6+3(1),QOPTION2    SET $0 COMM BILLING                          
         GOTO1 DOCOMM,DMCB,(R2),(R4),ACTCOMM,FULL,1                             
         BNZ   POSTBCOM                                                         
         B     POST80                                                           
*                                                                               
POST60   EQU   *                                                                
         SR    R4,R4                                                            
         L     R4,GROSSORD(R6)     CURRENT MONTH ESTIMATE                       
         TM    FLAG6(R2),X'01'     TEST ANY INVOICE DATA                        
         BO    POST65                                                           
         CLI   QOPTION1,C'W'                                                    
         BE    POST65                                                           
         CLI   QOPTION1,C'P'                                                    
         BE    POST65                                                           
         L     R4,TOTORD(R6)       TOTAL ORDERED                                
         A     R4,CUASATIN(R6)     CURRENT AS AT INVOICED                       
POST65   EQU   *                                                                
         LTR   R4,R4                                                            
         BZ    POST80                                                           
         LR    R5,R4                                                            
         A     R5,ESTGROS                                                       
         ST    R5,ESTGROS                                                       
         MVC   P6+3(1),QOPTION2    SET $0 COMM BILLING                          
         GOTO1 DOCOMM,DMCB,(R2),(R4),ESTCOMM,FULL,1                             
         BNZ   POSTBCOM                                                         
         SPACE 1                                                                
POST80   LA    R2,NEXTBUCK(R2)     BUMP TO NEXT MONTH                           
         B     POST20                                                           
         SPACE 2                                                                
POST100  EQU   *                                                                
         OC    ADJGROS(24),ADJGROS                                              
         BZ    POSTGOOD            NO DATA TO ACCUMULATE                        
*                                                                               
         MVI   STADATA,C'Y'        SET 'STATION HAS DATA' FLAG                  
*                                                                               
         L     R2,ACCUMS                                                        
         LA    R3,ADJGROS                                                       
         LA    R6,1                                                             
POST110  EQU   *                                                                
         L     R4,0(R3)                                                         
         GOTO1 ROLLER,DMCB,3,(R2),(R4),1,(R6)                                   
         CH    R6,=H'6'                                                         
         BE    POST120                                                          
         LA    R6,1(R6)                                                         
         LA    R3,4(R3)                                                         
         B     POST110                                                          
POST120  EQU   *                                                                
         SPACE 2                                                                
         L     R3,AOFFTAB                                                       
         LA    R4,2                R4 HOLDS OFFICE LINE NUMBER                  
POST210  EQU   *                                                                
         CLI   0(R3),0                                                          
         BE    POST250             OFFICE CODE NOT FOUND                        
         CLC   RCONKOFF(2),0(R3)                                                
         BE    POST220                                                          
         LA    R3,OTABLEN(R3)                                                   
         LA    R4,1(R4)                                                         
         B     POST210                                                          
POST220  EQU   *                                                                
         GOTO1 ROLLER,DMCB,4,(R2),1,(R4)      ADD LINE 1 TO OFFICE              
         GOTO1 ROLLER,DMCB,2,(R2),1           CLEAR LINE 1                      
         B     POSTGOOD                                                         
POST250  EQU   *                                                                
         DC    H'0'                                                             
*                                                                               
*        POST EXIT                                                              
*                                                                               
POSTGOOD EQU   *                                                                
         B     TESTGOOD                                                         
POSTBCOM EQU   *                                                                
         CLI   DMCB,1                                                           
         BNE   PBCOM10                                                          
         MVC   P(26),=C'NO COMMISSION RATE FOUND  '                             
         B     PBCOM50                                                          
PBCOM10  EQU   *                                                                
         CLI   DMCB,2                                                           
         BNE   PBCOM20                                                          
         MVC   P(26),=C'ZERO COMMISSION RATE FOUND'                             
         B     PBCOM50                                                          
PBCOM20  EQU   *                                                                
         MVC   P(26),=C'COMMISSION RATE ERROR     '                             
PBCOM50  EQU   *                                                                
         MVC   P+30(20),RREPSHRT                                                
         MVC   P+54(15),=C'CONTRACT NUMBER'                                     
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),RCONKCON                                                
         EDIT  (P5,DUB+3),(8,P+70)                                              
         MVC   P+80(11),=C'FOR STATION'                                         
         MVC   P+92(4),RCONKSTA                                                 
         MVI   P+96,C'-'                                                        
         MVC   P+97(1),RCONKSTA+4                                               
         PUT   RRECAP,P                                                         
         MVC   P,SPACES                                                         
POSTBAD  EQU   *                                                                
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    TESTBAD             YES - DON'T SEND ERROR MESSAGES              
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
         MVC   P(35),=C'>>> ERROR IN POSTING A CONTRACT <<<'                    
         GOTO1 LOCALREP,DMCB,ERRORLIN                                           
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        FIXPOST --- IF THE ACCOUNTING OPTION IS 'A' AND IT IS THE              
*                    FIRST MONTH, THEN ONLY REPORT THE ACTIVITY FOR             
*                    THE EOM PERIOD AND NOT THE TOTAL DOLLARS                   
*                                                                               
FIXPOST  NTR1                                                                   
*                                                                               
         L     R7,AMONARCH                                                      
         USING MONARCHD,R7                                                      
*                                                                               
         XC    FULL,FULL                                                        
*                                                                               
         L     R3,=F'-2'                USE R3 TO HOLD EST $                    
         L     R4,=F'-2'                USE R4 TO HOLD INV $                    
         SR    R5,R5                    USE R5 AS A FLAG                        
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   FPOST200                                                         
*                                                                               
FPOST10  EQU   *                                                                
         CLC   2(2,R6),QSTBIN                                                   
         BH    FPOST150                                                         
         ST    R6,WORD                  SAVE POINTER TO THIS EL                 
FPOST15  EQU   *                                                                
         SR    RF,RF                                                            
         ZICM  RE,6(R6),4                                                       
         BZ    FPOST15A                                                         
         SRDA  RE,31                                                            
         D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
FPOST15A EQU   *                                                                
         C     R3,=F'-2'                                                        
         BNE   FPOST16                                                          
         SR    R3,R3                                                            
FPOST16  EQU   *                                                                
         AR    R3,RF                                                            
         CLC   4(2,R6),KEYMONTH                                                 
         BL    FPOST17                                                          
         CLC   4(2,R6),KEYMONTH+2                                               
         BH    FPOST20              PAST USEFUL ELEMENTS                        
         LA    RE,1                                                             
         OR    R5,RE                                                            
         STCM  RF,15,WORK                                                       
         MVI   WORK+4,X'F1'                                                     
*        BAS   RE,PRINTIT                                                       
FPOST17  EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BNE   FPOST20                                                          
         L     RF,WORD                                                          
         CLC   0(4,RF),0(R6)         SAME MONTH?                                
         BE    FPOST15                                                          
FPOST20  EQU   *                                                                
         LA    R6,RCONREC            NOW LOOK FOR INV EL'S                      
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   FPOST100                                                         
FPOST50  EQU   *                                                                
         L     RF,WORD                                                          
         CLC   2(2,RF),2(R6)        SAME MONTH?                                 
         BNE   FPOST60                                                          
         CLC   4(2,R6),KEYMONTH+2                                               
         BH    FPOST100                                                         
         SR    RF,RF                                                            
         ZICM  RE,6(R6),4                                                       
         BZ    FPOST52                                                          
         SRDA  RE,31                                                            
         D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
FPOST52  EQU   *                                                                
         C     R4,=F'-2'                                                        
         BNE   FPOST53                                                          
         SR    R4,R4                                                            
FPOST53  EQU   *                                                                
         AR    R4,RF                                                            
         CLC   4(2,R6),KEYMONTH                                                 
         BL    FPOST55                                                          
         LA    RE,2                                                             
         OR    R5,RE                                                            
         STCM  RF,15,WORK                                                       
         MVI   WORK+4,X'F2'                                                     
*        BAS   RE,PRINTIT                                                       
         L     RF,WORD                                                          
         CLC   4(2,RF),4(R6)        ACTIVITY IN SAME WEEK?                      
         BE    FPOST54              IF SO, INV REPLACES ORDERED                 
         CLC   4(2,RF),KEYMONTH     ORDERED IN KEYMONTH STILL REPLACE           
         BL    FPOST60                                                          
FPOST54  EQU   *                                                                
         L     R3,=F'-2'                                                        
         B     FPOST60                                                          
FPOST55  EQU   *                                                                
         C     R3,=F'-2'                                                        
         BE    FPOST60                                                          
         LR    R3,R4               IF INV IS IN PAST, USE BEST DOLLAR           
         LA    RE,2                                                             
         NR    R5,RE               AND UPDATES TO ORDERED ARE USELESS           
         XC    WORK(4),WORK                                                     
         MVI   WORK+4,X'F3'                                                     
*        BAS   RE,PRINTIT                                                       
FPOST60  EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BE    FPOST50                                                          
FPOST100 EQU   *                                                                
         LTR   R5,R5                FLAG SET?                                   
         BZ    FPOST120             NOPE, SKIP THIS MONTH                       
         C     R3,=F'-2'                                                        
         BE    FPOST105                                                         
         C     R4,=F'-2'                                                        
         BE    FPOST102                                                         
         SR    R4,R3                GET NET AMOUNT FOR THE B.C. MONTH           
         B     FPOST105                                                         
FPOST102 EQU   *                                                                
         LR    R4,R3                                                            
FPOST105 EQU   *                                                                
         A     R4,FULL              ADD IN RUNNING TOTAL                        
         ST    R4,FULL              STORE IT                                    
FPOST120 EQU   *                                                                
         L     R3,=F'-2'                                                        
         L     R4,=F'-2'                                                        
         SR    R5,R5                                                            
         MVI   ELCODE,X'03'                                                     
         L     R6,WORD             RE-SET TO LAST X'03' EL                      
FPOST130 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BNE   FPOST150                                                         
         L     RF,WORD                                                          
         CLC   0(4,R6),0(RF)      MAKE SURE WE'RE NOT ON THE NEXT MONTH         
         BE    FPOST130                                                         
         B     FPOST10                                                          
*                                                                               
FPOST150 EQU   *               PROCESS X'04' ELS NOT COVERED BY X'03'           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   FPOSTXIT                                                         
         B     FPOST165                                                         
FPOST160 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BNE   FPOSTXIT                                                         
FPOST165 EQU   *                                                                
         CLC   2(2,R6),QSTBIN                                                   
         BH    FPOSTXIT                                                         
         CLC   4(2,R6),KEYMONTH                                                 
         BL    FPOST160                                                         
         CLC   4(2,R6),KEYMONTH+2                                               
         BH    FPOST160                                                         
         XC    P4(4),P4                                                         
         GOTO1 =V(HELLO),P1,(C'G',=C'REPFIL'),(X'03',RCONREC),         X        
               (2,2(R6)),RR=RELO                                                
         CLI   P4,0                                                             
         BE    FPOST160              ORDERED                                    
         ZICM  RE,6(R6),4                                                       
         BZ    FPOST160                                                         
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         L     RE,FULL                                                          
         AR    RE,RF                                                            
         ST    RE,FULL                                                          
         B     FPOST160                                                         
*                                                                               
*        SEPERATE PROCESSING FOR CONTRACTS WITH ONLY INV ELEMENTS               
*                                                                               
FPOST200 EQU   *                                                                
         SR    R4,R4                    USE R4 TO HOLD INV $                    
         SR    R5,R5                    USE R5 AS A FLAG                        
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   FPOSTXIT                                                         
*                                                                               
FPOST210 EQU   *                                                                
         SR    R3,R3                                                            
         CLC   2(2,R6),QSTBIN                                                   
         BH    FPOSTXIT                                                         
         ST    R6,WORD                  SAVE POINTER TO THIS EL                 
FPOST220 EQU   *                                                                
         SR    RF,RF                                                            
         ZICM  RE,6(R6),4                                                       
         BZ    FPOST222                                                         
         SRDA  RE,31                                                            
         D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
FPOST222 EQU   *                                                                
         CLC   4(2,R6),KEYMONTH                                                 
         BL    FPOST230                                                         
         CLC   4(2,R6),KEYMONTH+2                                               
         BH    FPOST240                 PAST USEFUL ELEMENTS                    
         LA    R5,1                                                             
         AR    R4,RF                                                            
         STCM  RF,15,WORK                                                       
         MVI   WORK+4,X'F4'                                                     
*        BAS   RE,PRINTIT                                                       
FPOST230 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BNE   FPOST240                                                         
         L     RF,WORD                                                          
         CLC   0(4,RF),0(R6)         SAME MONTH?                                
         BE    FPOST220                                                         
         LR    R3,R6                                                            
FPOST240 EQU   *                                                                
         LTR   R5,R5                FLAG SET?                                   
         BZ    FPOST250             NOPE, SKIP THIS MONTH                       
         A     R4,FULL              ADD IN RUNNING TOTAL                        
         ST    R4,FULL              STORE IT                                    
FPOST250 EQU   *                                                                
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         CR    R3,R6                                                            
         BE    FPOST210                                                         
*                                                                               
*        FIXPOST EXIT                                                           
*                                                                               
FPOSTXIT EQU   *                                                                
         B     TESTGOOD                                                         
         SPACE 2                                                                
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
*        PRINTIT --- PRINT THE BUCKET ABOUT TO BE USED                          
*                                                                               
PRINTIT  NTR1                                                                   
*                                                                               
         CLI   WORK+4,0                                                         
         BE    PIT10                                                            
         MVC   P+00(09),=C'PGM PLACE'                                           
         MVC   P+11(01),WORK+4                                                  
         MVC   P+14(07),=C'EL CODE'                                             
         ZIC   R1,0(R6)                                                         
         STC   R1,P+24                                                          
         OI    P+24,X'F0'                                                       
         MVC   WORK+6(2),2(R6)                                                  
         MVI   WORK+8,X'01'                                                     
         MVC   P+26(15),=C'BROADCAST MONTH'                                     
         GOTO1 DATCON,DMCB,(3,WORK+6),(6,P+42)                                  
PIT10    EQU   *                                                                
         MVC   P+52(13),=C'ACTIVITY DATE'                                       
         GOTO1 DATCON,DMCB,(2,4(R6)),(5,P+66)                                   
         OC    WORK(4),WORK                                                     
         BZ    PIT20                                                            
         MVC   P+76(13),=C'DOLLAR AMOUNT'                                       
         EDIT  (B4,WORK),(8,P+90)                                               
PIT20    EQU   *                                                                
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        ROLLTOT - ROLL OFFICE GROUP TOTALS TO THE NEXT LEVEL                   
*                                                                               
*                                                                               
*        PARM1  =  1 DO STATION TO GROUP                                        
*                  2 DO GROUP TO REPORT                                         
*                                                                               
         SPACE 3                                                                
ROLLTOT  NTR1                                                                   
*                                                                               
         L     R1,0(R1)            GET PASSED TABLE REQUEST                     
         LA    R2,RPTCLIN          POINT TO REPORT LINE CONTROL TABLE           
RTOT10   EQU   *                                                                
         ZIC   R3,0(R2)                                                         
         LTR   R3,R3                                                            
         BZ    RTOTBAD             ZERO IS HIT END OF TABLE MARKER              
         CR    R1,R3                                                            
         BE    RTOT20                                                           
         LA    R2,3(R2)                                                         
         B     RTOT10                                                           
*                                                                               
RTOT20   EQU   *                                                                
         LR    R1,R2                                                            
         ZIC   R2,1(R1)            GET FROM START LINE INTO R2                  
         ZIC   R3,2(R1)            GET FROM END LINE INTO R3                    
         ZIC   R4,4(R1)            GET TO START LINE INTO R2                    
         ZIC   R5,5(R1)            GET TO END LINE INTO R3                      
         L     R6,ACCUMS                                                        
         L     R7,AOFFTAB                                                       
RTOT50   EQU   *                                                                
         CLI   0(R7),0                                                          
         BE    RTOT100                                                          
         GOTO1 ROLLER,DMCB,4,(R6),(R2),(R4)  ROLL TOTAL                         
         GOTO1 ROLLER,DMCB,2,(R6),(R2)       CLEAR LINE                         
         LA    R2,1(R2)                                                         
         LA    R4,1(R4)                                                         
         LA    R7,OTABLEN(R7)                                                   
         CR    R2,R3                                                            
         BNH   RTOT50                                                           
*                                                                               
RTOT100  EQU   *                                                                
         LA    R3,1(R3)            GET OFFICE TOTAL LINE NUMBER                 
*        LA    R5,1(R5)                                                         
*        GOTO1 ROLLER,DMCB,4,(R6),(R3),(R5)  ROLL TOTAL                         
         GOTO1 ROLLER,DMCB,2,(R6),(R3)       CLEAR LINE                         
*                                                                               
*        ROLLTOT EXIT                                                           
*                                                                               
RTOTGOOD EQU   *                                                                
         B     TESTGOOD                                                         
RTOTBAD  EQU   *                                                                
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    TESTBAD             YES - DON'T SEND ERROR MESSAGES              
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
         MVC   P(40),=C'>>> ERROR IN ROLLING A TOTAL SECTION <<<'               
         GOTO1 LOCALREP,DMCB,ERRORLIN                                           
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        DOPRINT --- PRINT OFFICE TABLE                                         
*                                                                               
*        PARM1  =  1 DO STATION                                                 
*                  2 DO GROUP                                                   
*                  3 DO REPORT                                                  
*                                                                               
         SPACE 3                                                                
DOPRINT  NTR1                                                                   
*                                                                               
         XC    OFFPRINT(1),OFFPRINT                                             
*                                                                               
         CLI   LINE,51                                                          
         BL    DPNT05                                                           
         MVI   FORCEHED,C'Y'                                                    
DPNT05   EQU   *                                                                
*                                                                               
         ZIC   R2,3(R1)            GET PASSED VALUE INTO R2                     
*                                                                               
         CLI   3(R1),1             LEVEL 1 REPORTING?                           
         BNE   DPNT20              NO, CHECK FURTHER                            
         BAS   RE,DPNTSTA                                                       
         B     DPNT100                                                          
DPNT20   EQU   *                                                                
         CLI   3(R1),2             LEVEL 2 REPORTING                            
         BNE   DPNT40              NO, CHECK FURTHER                            
         BAS   RE,DPNTGRP                                                       
         B     DPNT100                                                          
DPNT40   EQU   *                                                                
         CLI   3(R1),3             LEVEL 3 REPORTING?                           
         BNE   DPNT100             NO, NOW I'M LOST                             
         BAS   RE,DPNTRPT                                                       
DPNT100  EQU   *                                                                
*                                                                               
         LA    R1,RPTCLIN          POINT TO REPORT LINE CONTROL TABLE           
DPNT110  EQU   *                                                                
         ZIC   R3,0(R1)                                                         
         LTR   R3,R3                                                            
         BZ    DPNTBAD             ZERO IS HIT END OF TABLE MARKER              
         CR    R2,R3               PASSED VALUE STILL IN R2                     
         BE    DPNT120                                                          
         LA    R1,3(R1)                                                         
         B     DPNT110                                                          
*                                                                               
DPNT120  EQU   *                                                                
         ZIC   R3,1(R1)            GET START LINE INTO R3                       
         ZIC   R4,2(R1)            GET END LINE INTO R4                         
         ST    R3,WORD             USE R3 AND SAVE IT                           
         L     R5,ACCUMS                                                        
         GOTO1 ROLLER,DMCB,1,(R5),(R3)                                          
         L     R5,0(R1)            POINT TO FIRST CELL                          
         L     R6,AOFFTAB          POINT TO THE OFFICE TABLE                    
*                                                                               
DPNT150  EQU   *                   OUTER LOOP - OFFICE-DOLLARS                  
         CLI   0(R6),0                                                          
         BE    DPNT200                                                          
         OC    0(24,R5),0(R5)                                                   
         BNZ   DPNT155             NO PRINT LINE IF NO DATA                     
         LA    R5,24(R5)                                                        
         B     DPNT170                                                          
DPNT155  EQU   *                                                                
         ZIC   RF,OFFPRINT                                                      
         LA    RF,1(RF)                                                         
         STC   RF,OFFPRINT         COUNT NUMBER OF OFFICE LINES                 
         MVC   P+3(20),2(R6)       LOAD OFFICE NAME                             
         XC    DOUBLE,DOUBLE                                                    
         LA    R7,P+28                                                          
         LA    R2,3                                                             
DPNT160  EQU   *                   INNER LOOP - GROSS AND COMM                  
         L     RF,DOUBLE                                                        
         A     RF,0(R5)                                                         
         ST    RF,DOUBLE                                                        
         BAS   RE,NOCENT                                                        
         LA    R5,4(R5)                                                         
         LA    R7,12(R7)                                                        
         L     RF,DOUBLE+4                                                      
         A     RF,0(R5)                                                         
         ST    RF,DOUBLE+4                                                      
         BAS   RE,NOCENT                                                        
         LA    R5,4(R5)                                                         
         LA    R7,12(R7)                                                        
         BCT   R2,DPNT160                                                       
*                                                                               
         ST    R5,HALF                  USE HALF AND HALF2                      
         LA    R5,DOUBLE                                                        
         BAS   RE,NOCENT                                                        
         LA    R5,4(R5)                                                         
         LA    R7,12(R7)                                                        
         BAS   RE,NOCENT                                                        
         L     R5,HALF                                                          
         GOTO1 LOCALREP,DMCB,PAT1LIN                                            
*                                                                               
DPNT170  EQU   *                                                                
         LA    R3,1(R3)                                                         
         LA    R6,OTABLEN(R6)                                                   
         CR    R3,R4                                                            
         BNH   DPNT150                                                          
*                                                                               
*        DO OFFICE TOTAL LINE                                                   
*                                                                               
DPNT200  EQU   *                                                                
         CLI   OFFPRINT,1                                                       
         BNE   DPNT210                                                          
         MVI   SPACING,2                                                        
         B     DPNT290                                                          
DPNT210  EQU   *                                                                
         LR    R3,R4                                                            
         LA    R4,1(R4)            GROUP TOTAL LINE NUMBER                      
         L     R5,ACCUMS                                                        
         L     R2,WORD                                                          
DPNT220  EQU   *                                                                
         GOTO1 ROLLER,DMCB,4,(R5),(R2),(R4)  ADD LINES TO GET TOTAL             
         LA    R2,1(R2)                                                         
         CR    R2,R3                                                            
         BNE   DPNT220                                                          
         GOTO1 ROLLER,DMCB,1,(R5),(R4)                                          
         L     R5,0(R1)            POINT TO FIRST CELL                          
         MVC   P+3(20),CALLTOT                                                  
         LA    R7,P+28                                                          
         LA    R2,3                                                             
         XC    DOUBLE,DOUBLE                                                    
DPNT230  EQU   *                   GROSS AND COMM DOLLARS                       
         L     RF,DOUBLE                                                        
         A     RF,0(R5)                                                         
         ST    RF,DOUBLE                                                        
         BAS   RE,NOCENT                                                        
         LA    R5,4(R5)                                                         
         LA    R7,12(R7)                                                        
         L     RF,DOUBLE+4                                                      
         A     RF,0(R5)                                                         
         ST    RF,DOUBLE+4                                                      
         BAS   RE,NOCENT                                                        
         LA    R5,4(R5)                                                         
         LA    R7,12(R7)                                                        
         BCT   R2,DPNT230                                                       
*                                                                               
         ST    R5,HALF                  USE HALF AND HALF2                      
         LA    R5,DOUBLE                                                        
         BAS   RE,NOCENT                                                        
         LA    R5,4(R5)                                                         
         LA    R7,12(R7)                                                        
         BAS   RE,NOCENT                                                        
         L     R5,HALF                                                          
         MVI   SPACING,3                                                        
*                                                                               
DPNT290  EQU   *                                                                
         GOTO1 LOCALREP,DMCB,PAT1LIN                                            
*                                                                               
*        DOREPORT TOTALS                                                        
*                                                                               
DPNTGOOD EQU   *                                                                
         B     TESTGOOD                                                         
DPNTBAD  EQU   *                                                                
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    TESTBAD             YES - DON'T SEND ERROR MESSAGES              
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
         MVC   P(41),=C'>>> ERROR IN PRINTING AN OFFICE TABLE <<<'              
         GOTO1 LOCALREP,DMCB,ERRORLIN                                           
         B     TESTBAD                                                          
         SPACE 3                                                                
NOCENT   EQU   *                                                                
         EDIT  (4,(R5)),(11,(R7)),FLOAT=-                                       
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
*        RPTCLIN --- REPORT LINE CONTROL TABLE                                  
*                                                                               
RPTCLIN  EQU   *                                                                
         DC    X'01',AL1(ACFR1),AL1(ACTO1)                                      
         DC    X'02',AL1(ACFR2),AL1(ACTO2)                                      
         DC    X'03',AL1(ACFR3),AL1(ACTO3)                                      
         DC    X'00'                       END OF TABLE MARKER                  
         EJECT                                                                  
*                                                                               
*        DPNTSTA --- DOPRINT STATION LEVEL HOUSEKEEPING                         
*                                                                               
DPNTSTA  NTR1                                                                   
*                                                                               
         MVC   CALLTOT,=C'* STATION TOTAL *   '                                 
         MVC   P+0(4),RSTAKSTA     STATION-BAND/MARKET ON 1ST LINE              
         LA    R3,P+3                                                           
         CLI   0(R3),0                                                          
         BE    DSTA10                                                           
         CLI   0(R3),C' '                                                       
         BNE   DSTA20                                                           
DSTA10   EQU   *                                                                
         LA    R3,P+2                                                           
DSTA20   EQU   *                                                                
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         CLI   RSTAKSTA+4,0                                                     
         BE    DSTA30                                                           
         CLI   RSTAKSTA+4,C' '                                                  
         BNE   DSTA40                                                           
DSTA30   EQU   *                                                                
         MVC   0(2,R3),=C'TV'                                                   
         B     DSTA60                                                           
DSTA40   EQU   *                                                                
         MVC   0(1,R3),RSTAKSTA+4                                               
         CLI   RSTAKSTA+4,C'T'                                                  
         BNE   DSTA50                                                           
         MVI   1(R3),C'V'                                                       
         B     DSTA60                                                           
DSTA50   EQU   *                                                                
         MVI   1(R3),C'M'                                                       
DSTA60   EQU   *                                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),C'/'                                                       
         MVC   1(20,R3),RSTAMKT                                                 
         MVI   SPACING,2                                                        
         GOTO1 LOCALREP,DMCB,STNTEXT                                            
         ZIC   RF,LV1PRINT         AND  BUMP COUNT                              
         LA    RF,1(RF)                                                         
         STC   RF,LV1PRINT                                                      
*                                                                               
DSTAGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        DPNTGRP --- DOPRINT GROUP HOUSEKEEPING                                 
*                                                                               
DPNTGRP  NTR1                                                                   
*                                                                               
         MVC   CALLTOT,=C'** GROUP TOTAL **   '                                 
         ZIC   RF,LV2PRINT         BUMP LEVEL 2 COUNT                           
         LA    RF,1(RF)                                                         
         STC   RF,LV2PRINT                                                      
         ZIC   RF,LV1PRINT         LEVEL 2 IFF > 1 LEVEL 1 PRINTED              
         SR    R0,R0                AND CLEAR LEVEL 1 COUNTER                   
         STC   R0,LV1PRINT                                                      
         LTR   RF,RF                                                            
         BZ    DPNTGOOD                                                         
         C     RF,=F'1'                                                         
         BE    DPNTGOOD                                                         
         MVC   P+0(10),RGRPNAME    LABEL GROUP TOTALS                           
         MVC   P+11(10),RGRPSBNM                                                
         MVI   SPACING,2                                                        
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
*                                                                               
DGRPGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        DPNTPRT --- DOPRINT REPOR HOUSEKEEPING                                 
*                                                                               
DPNTRPT  NTR1                                                                   
*                                                                               
         MVC   CALLTOT,=C'*** REPORT TOTAL ***'                                 
         ZIC   RF,LV2PRINT         LEVEL 3 IFF > 1 LEVEL 2 PRINTED              
         LTR   RF,RF                                                            
         BZ    DPNTGOOD                                                         
         C     RF,=F'1'                                                         
         BE    DPNTGOOD                                                         
         MVC   P+0(10),RGRPNAME    LABEL GROUP TOTALS                           
         MVI   SPACING,2                                                        
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
*                                                                               
DRPTGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        INITADDR --- INITIALIZE LINK AND SPACE ADDRESSES                       
*                                                                               
INITADDR NTR1                                                                   
*                                                                               
         LA    R0,HHOOK                                                         
         ST    R0,HEADHOOK                                                      
*                                                                               
         L     R0,=V(ACCUMC)                                                    
         A     R0,RELO                                                          
         ST    R0,ACCUMS           USE IT FOR ACCUMS                            
*                                                                               
         L     R0,=V(IOBUFF)                                                    
         A     R0,RELO                                                          
         ST    R0,AIOBUFF          USE IT FOR INTERNAL I/O'S                    
*                                                                               
         L     R0,=V(STATAB)                                                    
         A     R0,RELO                                                          
         ST    R0,ASTATAB          USE IT FOR STATION RATE RECORDS              
*                                                                               
         L     R0,=V(OFFTAB)                                                    
         A     R0,RELO                                                          
         ST    R0,AOFFTAB          USE IT FOR OFFICE NAMES AND CODES            
*                                                                               
IADRGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         SPACE 3                                                                
*                                                                               
*        INITVALS --- INITIALIZE REPORT VALUES AND CLEAR ACCUM TABLE            
*                                                                               
INITVALS NTR1                                                                   
*                                                                               
         XC    QSTBIN,QSTBIN                                                    
         GOTO1 DATCON,DMCB,(0,QSTART),(3,QSTBIN)                                
*                                                                               
         MVC   MONTHT1,SPACES                                                   
         MVC   MONTHT2,SPACES                                                   
         MVC   MONTHT3,SPACES                                                   
         GOTO1 DATCON,DMCB,(0,QEND),(6,MONTHT1)                                 
*                                                                               
         GOTO1 DATCON,DMCB,(0,QEND),(3,FULL)                                    
         CLI   FULL+1,1                                                         
         BE    INIV10                                                           
         ZIC   R1,FULL+1                                                        
         BCTR  R1,0                                                             
         STC   R1,FULL+1                                                        
         B     INIV20                                                           
INIV10   EQU   *                                                                
         ZIC   R1,FULL                                                          
         BCTR  R1,0                                                             
         STC   R1,FULL                                                          
         MVI   FULL+1,12                                                        
INIV20   EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,FULL),(6,MONTHT2)                                 
*                                                                               
         CLI   FULL+1,1                                                         
         BE    INIV50                                                           
         ZIC   R1,FULL+1                                                        
         BCTR  R1,0                                                             
         STC   R1,FULL+1                                                        
         B     INIV60                                                           
INIV50   EQU   *                                                                
         ZIC   R1,FULL                                                          
         BCTR  R1,0                                                             
         STC   R1,FULL                                                          
         MVI   FULL+1,12                                                        
INIV60   EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,FULL),(6,MONTHT3)                                 
*                                                                               
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     R2,ACCUMS                                                        
         GOTO1 ROLLER,DMCB,0,(R2),ACNUMLIN,ACNUMCOL                             
*                                                                               
IVALGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        SETACTIV --- SET ACTIVITY DATES FOR VALUMON CALLS IN REPORTER          
*                                                                               
SETACTIV NTR1                                                                   
*                                                                               
         L     R6,AMONARCH                                                      
         USING MONARCHD,R6                                                      
*                                                                               
         MVC   QSTART+4(2),=C'01'                                               
         MVC   QEND+4(2),=C'01'                                                 
         XC    WORK(16),WORK                                                    
         GOTO1 DATCON,DMCB,(0,QEND),(2,MONACT)                                  
         GOTO1 DATCON,DMCB,(0,QEND),(3,WORK)                                    
         CLI   WORK+1,X'0C'                                                     
         BNE   SACT10                                                           
         ZIC   R1,WORK                                                          
         LA    R1,1(R1)                                                         
         STC   R1,WORK                                                          
         MVI   WORK+1,X'01'                                                     
         B     SACT20                                                           
SACT10   EQU   *                                                                
         ZIC   R1,WORK+1                                                        
         LA    R1,1(R1)                                                         
         STC   R1,WORK+1                                                        
SACT20   EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,WORK),(0,WORK+4)                                  
         GOTO1 ADDAY,DMCB,WORK+4,WORK+10,-1                                     
         GOTO1 DATCON,DMCB,(0,WORK+10),(2,MONACT+2)                             
         MVC   MONACT+4(4),MONACT                                               
*                                                                               
SACTGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        BLDOFF --- BUILD OFFICE TABLE                                          
*                                                                               
BLDOFF   NTR1                                                                   
*                                                                               
*        CLEAR THE OFFICE TABLE                                                 
*                                                                               
         L     R1,AOFFTAB                                                       
         LR    R2,R1                                                            
         LA    R3,OTABNUM                                                       
BOFF10   EQU   *                                                                
         LA    R4,OTABLEN                                                       
         EX    R4,BOFF15                                                        
         B     BOFF20                                                           
BOFF15   XC    0(0,R1),0(R1)                                                    
BOFF20   EQU   *                                                                
         BCT   R3,BOFF10                                                        
*                                                                               
*        SET I/O AREA AND POINTERS AND READ OFFICE RECORDS TO BUILD             
*        THE OFFICE TABLE                                                       
*                                                                               
         L     R3,AIOBUFF                                                       
         L     R4,AIOAREA                                                       
         ST    R3,AIOAREA                                                       
         USING ROFFREC,R3                                                       
*                                                                               
         SR    R5,R5               USE R5 AS A COUNTER                          
         LA    R6,OTABNUM          LOAD FOR MAX COMAPRE                         
         MVC   KEY2(27),KEY        SAVE CURRENT KEY                             
*                                                                               
         XC    ROFFREC(32),ROFFREC                                              
         MVI   ROFFKEY,X'04'                                                    
         MVC   ROFFKREP(2),QREP                                                 
         MVC   KEY(27),ROFFKEY                                                  
         GOTO1 HIGH                                                             
         B     BOFF60                                                           
*                                                                               
BOFF50   EQU   *                                                                
         GOTO1 SEQ                                                              
BOFF60   EQU   *                                                                
         CLI   KEY,X'04'           STILL OFFICE KEYS?                           
         BNE   BOFF100             NO-ALL DONE                                  
         CLC   KEY+23(2),QREP      STILL MY REP                                 
         BNE   BOFF100             NO-ALL DONE                                  
         GOTO1 GREC                GET THE RECORD                               
         MVC   0(2,R2),ROFFKOFF    LOAD OFFICE CODE                             
         MVC   2(20,R2),ROFFNAME   LOAD OFFICE NAME                             
         LA    R2,OTABLEN(R2)      POINT TO THE NEXT ENTRY                      
         LA    R5,1(R5)            BUMP COUNT                                   
         CR    R5,R6               TABLE FULL?                                  
         BL    BOFF50              NO-CHECK FOR MORE                            
*                                                                               
BOFF100  EQU   *                                                                
         STC   R5,NUMOFFS          SAVE NUM OF ENTRIES                          
         MVC   0(2,R2),=X'0000'    END OF TABLE MARKER                          
         ST    R4,AIOAREA          RESET IO BUFFER                              
         MVC   KEY(27),KEY2        RESTORE READING ORDER                        
         GOTO1 HIGH                                                             
*                                                                               
BOFFGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*        HHOOK --- HEADHOOK ROUTINE                                             
*                                                                               
HHOOK    NTR1                                                                   
*                                                                               
         USING HHOOK,RF                                                         
         LA    R1,SAVEREGS-HHOOK                                                
         AR    R1,RF                                                            
         LM    R2,RC,0(R1)                                                      
         DROP  RF                                                               
*                                                                               
         MVC   HEAD3+1(10),GRPNAME                                              
         MVC   HEAD3+12(10),SGRPNAME                                            
*                                                                               
         MVC   HEAD3+070(6),MONTHT1                                             
*                                                                               
         MVC   ACTTEXT,SPACES                                                   
         L     R4,AMONARCH                                                      
         USING MONARCHD,R4                                                      
         GOTO1 DATCON,DMCB,(X'12',KEYMONTH),(5,ACTTEXT)                         
         DROP  R4                                                               
         MVC   HEAD4+071(19),ACTTEXT                                            
*                                                                               
         MVC   HEAD6+030(6),MONTHT3                                             
         MVC   HEAD6+055(6),MONTHT2                                             
         MVC   HEAD6+080(6),MONTHT1                                             
*                                                                               
HHOOKEXT EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        LOCALREP --- LOCAL REPORT INTERFACE                                    
*                                                                               
LOCALREP NTR1                                                                   
*                                                                               
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
*                                                                               
         L     R2,VXDOWNDF         R2 -> COMMON DEFINITION LIST                 
         L     R1,0(R1)            R1 -> PASSED DEFINITION LIST                 
LREP10   EQU   *                                                                
         MVC   0(2,R2),0(R1)       MOVE PAIRS UNTIL ZERO                        
         CLI   0(R1),0                                                          
         BE    LREP20                                                           
         LA    R1,2(R1)                                                         
         LA    R2,2(R2)                                                         
         B     LREP10                                                           
LREP20   EQU   *                                                                
         GOTO1 REPORT,DMCB,WORKC,=C'PRINT'                                      
*                                                                               
LREPGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         SPACE 2                                                                
         DROP  R5                                                               
         SPACE 3                                                                
BLANKLIN DC    X'0000'                                                          
ERRORLIN DC    C'T',AL1(32),X'0000'                                             
ALLTEXT  DC    C'T',AL1(32),X'0000'                                             
STNTEXT  DC    C'T',AL1(07),C'O',AL1(01),C'T',AL1(20),X'0000'                   
PAT1LIN  DC    C'O',AL1(03),C'T',AL1(20),C'O',AL1(06)                           
         DC    C'N',AL1(11),C'O',AL1(01),C'N',AL1(11),C'O',AL1(01)              
         DC    C'N',AL1(11),C'O',AL1(01),C'N',AL1(11),C'O',AL1(01)              
         DC    C'N',AL1(11),C'O',AL1(01),C'N',AL1(11),C'O',AL1(01)              
         DC    C'N',AL1(11),C'O',AL1(01),C'N',AL1(11),X'0000'                   
         EJECT                                                                  
*                                                                               
*        COMMON ROUTINES FOR SAVING CODE SPACE                                  
*                                                                               
         SPACE 3                                                                
TESTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
TESTBAD  EQU   *                                                                
         LA    R0,1                                                             
TESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         SPACE 3                                                                
         GETEL R6,34,ELCODE                                                     
         EJECT                                                                  
*                                                                               
*   CALLS A MODIFIED VERSION OF RECOMRTN, TO HANDLE DOWNLOADING                 
*                                                                               
       ++INCLUDE RECOMLOC                                                       
         EJECT                                                                  
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*                                                                               
*        LOCAL CONSTANTS                                                        
*                                                                               
CALLTOT  DS    CL20                                                             
*                                                                               
*        DCB'S                                                                  
*                                                                               
RRECAP   DCB   DDNAME=RRECAP,DSORG=PS,RECFM=FB,LRECL=132,BLKSIZE=132,  X        
               MACRF=(GM,PM),EODAD=RPTDEOF                                      
*                                                                               
*        PROGRAM LITERAL AREA                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*        WORK AREA                                                              
         SPACE 2                                                                
SAVEGRP  DS    CL2                                                              
ANYPRINT DC    C'N'                                                             
STADATA  DC    C'N'                STATION DATA FLAG                            
*                                  Y  =  YES                                    
*                                  N  =  NO                                     
GRPDATA  DC    C'N'                GROUP   DATA FLAG                            
*                                  Y  =  YES                                    
*                                  N  =  NO                                     
         SPACE 2                                                                
RELO     DS    A                                                                
SAVEREGS DS    11F                                                              
ACCUMS   DS    A                   A(ACCUMS)                                    
AOFFTAB  DS    A                   A(OFFTAB)                                    
*                                                                               
ADJGROS  DS    F                   CONTRACT ACCUMULATORS                        
ADJCOMM  DS    F                                                                
ACTGROS  DS    F                                                                
ACTCOMM  DS    F                                                                
ESTGROS  DS    F                                                                
ESTCOMM  DS    F                                                                
*                                                                               
COMMAND  DS    CL8                 IO COMMAND                                   
KEY2     DS    CL32                EXTRA KEY HOLDER                             
CURSTA   DS    CL5                 CURRENT STATION CALLS                        
NUMOFFS  DS    CL1                 NUMBER OF OFFICES                            
OFFPRINT DS    CL1                 NUM OFF LINES PRINTED THIS CALL              
LV1PRINT DS    CL1                 NUM LEVEL 1 GROUPS PRINTED                   
LV2PRINT DS    CL1                 NUM LEVEL 2 GROUPS PRINTED                   
ELCODE   DS    CL1                 ELEMENT CODE                                 
QSTBIN   DS    CL3                 START MONTH BINARY                           
FRSTIME  DS    CL1                 FIRST-TIME SWITCH                            
GRPNAME  DS    CL10                CURRENT GROUP NAME                           
SGRPNAME DS    CL10                CURRENT SUB-GROUP NAME                       
ACTTEXT  DS    CL20                ACCOUNT PERIOD TEXT                          
MONTHT1  DS    CL6                 CURRENT MONTH TEXT                           
MONTHT2  DS    CL6                 LAST MONTH TEXT                              
MONTHT3  DS    CL6                 PRIOR MONTH TEXT                             
*                                                                               
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
*                                                                               
         SPACE 2                                                                
         ENTRY OFFTAB                                                           
*                                                                               
OFFTAB   DS    150CL22             OFFICE TABLE - 150 MAX                       
*                                                                               
OTABCODE EQU   0                   CL2  - OFFICE CODE                           
OTABNAM  EQU   2                   CL20 - OFFICE NAME                           
*                                                                               
OFFTABX  DS    CL2                 END OF TABLE MARKER                          
OTABNUM  EQU   150                 NUMBER OF ENTRIES                            
OTABLEN  EQU   22                  ENTRY LENGTH                                 
*                                                                               
         SPACE 2                                                                
       ++INCLUDE RECOMTAB                                                       
*                                                                               
         SPACE 2                                                                
         ENTRY IOBUFF                                                           
IOBUFF   DS    0D                                                               
         DC    1008X'00'           WORK FILE BUFFER                             
         EJECT                                                                  
*                                                                               
** A SCHEMATIC OF THE ACCUMULATORS FOR ROLLER FOLLOWS-                          
*                                                                               
*                                                                               
* COLUMN  1     =  PRIOR MONTH'S ADJUSTMENT GROSS                               
*         2     =  PRIOR MONTH'S ADJUSTMENT COMMISSION                          
*         3     =  LAST MONTH'S ACTUAL GROSS                                    
*         4     =  LAST MONTH'S ACTUAL COMMISSION                               
*         5     =  CURRENT MONTH'S ESTIMATED GROSS                              
*         6     =  CURRENT MONTH'S ESTIMATED COMMISSION                         
*                                                                               
*                                                                               
* LINE  001     = IS THE WORK AREA                                              
*       002-051 = STATION BY OFFICE (MAX. 50 OFFICES)                           
*       052     = STATION TOTAL                                                 
*       053-102 = GROUP BY OFFICE                                               
*       103     = GROUP TOTAL                                                   
*       104-153 = REPORT BY OFFICE                                              
*       154     = REPORT TOTAL                                                  
*                                                                               
*                                                                               
* --->   ACCUMULATOR SIZE = (LINE*COL*4)+8 = (154*6*4)+8 = 3704 BYTES           
*                                                                               
         ENTRY ACCUMC                                                           
ACCUMC   DS    0D                                                               
         DS    40000C                                                           
ACNUMCOL EQU   6                                                                
ACNUMLIN EQU   154                                                              
ACFR1    EQU   2                                                                
ACTO1    EQU   51                                                               
ACFR2    EQU   53                                                               
ACTO2    EQU   102                                                              
ACFR3    EQU   104                                                              
ACTO3    EQU   153                                                              
         EJECT                                                                  
       ++INCLUDE REMONARCHD                                                     
         EJECT                                                                  
*        PRINT OFF                                                              
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
EXTRAIO  DSECT                                                                  
EXTRAIO1 DS    0F                                                               
       ++INCLUDE REGENCOM                                                       
         EJECT                                                                  
       ++INCLUDE REXADDRD                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'112REREP4702 09/27/02'                                      
         END                                                                    
