*          DATA SET REREP1602  AT LEVEL 154 AS OF 12/16/14                      
*PHASE RE1602A,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE REPVALM2                                                               
         TITLE 'RE1602 - REREP1602 - STATION CLOSE-OUT REPORT'                  
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP1602 --- STATION CLOSEOUT REPORT                      *           
*                                                                   *           
* ----------------------------------------------------------------- *           
*                                                                   *           
*  OPT1/OPT2/OPT3 ARE ON THE REQUEST SCREEN                         *           
*  ---------------------------------------                          *           
* QOPTION1 - UPDATE/SOFT, 'U' IS UPDATE MODE                        *           
* QOPTION2 - CLEAR/DON'T CLEAR INVOICED DOLLARS, 'D' IS DON'T       *           
*        *** OPTION TURNED OFF DEC13/96 - BILL UHR ***              *           
* QOPTION3 - 'Y' = BACK OUT CLOSEOUT:                               *           
*                  RESET DATE TO REQUEST END DATE                   *           
*                  DELETE INV (X'04') ELEMENTS WITH $0 WHICH ARE    *           
*                    -AFTER- THE REQUEST END DATE                   *           
*                                                                   *           
*  OPT3 ALTERNATE USE:  FROM INPUT CARD ONLY (SUPERCEDES BACKOUT):  *           
*  -----------------------------------------                        *           
* QOPTION3 - ALL RECORDS/CONTRACT ONLY, 'C' IS CONTRACTS ONLY       *           
*                                                                   *           
*  OPT4 IS FOR INPUT CARD ONLY USE                                  *           
*  -----------------------------------------                        *           
* QOPTION4 - 'T' = PROGRAM TRACE                                    *           
*            'H' = DON'T REWRITE                                    *           
*                                                                   *           
*                                                                   *           
* QUESTOR     =  DISPCON: DISPLAY PRE/POST CONTRACT RECORDS         *           
*                         VIA PRNTBL                                *           
* QUESTOR     =  DISPSTA: DISPLAY PRE/POST STATION RECORD(S)        *           
*                         VIA PRNTBL                                *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* SEP/89 (MRR)   --- INITIAL RELEASE                                *           
*                                                                   *           
* 10/26/89  PJS  --- FIX HEADHOOK ADDRESSABILITY (TRACE DUMP BUG)   *           
*                                                                   *           
* NOV17/89 (MRR) --- ADD CONTRACT TYPE TO CONTRACT DISPLAY          *           
*                                                                   *           
* JAN03/90 (MRR) --- UPDATE THE STATION RECORD WITH THE CLOSE DATE  *           
*                     ON REQUEST FIRST MODE ON A SINGLE STATION     *           
*                     REQUEST SO THAT THE STATION 'CLOSES' EVEN     *           
*                     IF THERE ARE NO CONTRACTS                     *           
*                                                                   *           
* 02/14/90  PJS  --- ADDED CONTRACT START DATE FILTER TO REJECT     *           
*                    CONTRACTS STARTING AFTER REQUEST ENDS.         *           
*                                                                   *           
* DEC03/91 (BU ) --- UPDATE FOR VALUENEW FACILITY                   *           
*                                                                   *           
* MAR27/92 (BU ) --- MAKE COMPATIBLE WITH NEW VALU2NEW:             *           
*                    REREPRGEQU ---> REREPRGEQA                     *           
*                                                                   *           
* AUG10/92 (BU ) --- PROHIBIT UPDATE OF CLOSEOUT DATE IF FILTERS    *           
*                    OTHER THAN 'STATION' ARE PRESENT.  ALL FILTERS *           
*                    ARE ON FIRST OPTION CARD.                      *           
*                                                                   *           
* NOV06/92 (BU ) --- FIX 'TRACE' AND 'HARD-TEST' OPTIONS            *           
*                                                                   *           
* NOV18/94 (BU ) --- ADD 'BACKOUT' LOGIC                            *           
*                                                                   *           
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                *           
*                                                                   *           
* JUN27/96 (WSB) --- IF '*' IN LAST BYTE OF QUESTOR, DON'T ADD      *           
*                    'CLOSED BY RE16' CONTRACT COMMENT              *           
*                                                                   *           
* NOV07/96 (BU ) --- FIX 'CLEAR INVOICE $' LOGIC                    *           
*                                                                   *           
* NOV14/96 (BU ) --- ADJUST 'CLEAR INVOICE $' LOGIC TO PROPERLY     *           
*                    HANDLE NEGATIVE DOLLARS                        *           
*                                                                   *           
* DEC13/96 (BU ) --- NOOP 'CLEAR INVOICE $' LOGIC                   *           
*                                                                   *           
* MAR14/97 (DBU) ---SUMMARY BY OFFICE                               *           
*                                                                   *           
* MAY07/97 (RHV) --- REMOVE * IN LAST BYTE OF QUESTOR OPTION        *           
*                    WRITE '2B' CLOSEOUT HIST ELEM TO K INSTEAD     *           
*                    OF CLOSEOUT '02' COMMENT                       *           
*                    DELETE SAME ON REVERSE ACTION                  *           
*                                                                   *           
* JUN10/97 (BU ) --- UPGRADE TO SET X'23' ELEMENT                   *           
*                                                                   *           
* NOV09/97 (JRD) --- HANDLE ALTERNATE CALENDAR BUCKETS              *           
*                                                                   *           
* FEB05/98 (BU ) --- DON'T CLOSE STATION IF CONTYPE FILTERED ON     *           
*                                                                   *           
* FEB16/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
* OCT15/07 (SKU) --- CHANGE TO ADD COPY TO RECOVERY OUTPUT          *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
RE1602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1602,R9,RR=RE                                              
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         STM   R2,RC,SAVEREGS      SAVE REGS 2-C                                
         SPACE 2                                                                
*                                                                               
*- NO-OP PUT REC CALL FOR TRACE OPTION (T) OR HARD-TEST/NO REWRITE              
*     OPTION (H)                                                                
*                                                                               
         CLI   QOPTION4,C'T'       TRACE OPTION                                 
         BE    NOOPIT              YES                                          
*                                                                               
         CLI   QOPTION4,C'H'       HARD-TEST/NO REWRITE?                        
         BNE   TRACE010            NO                                           
*                                                                               
NOOPIT   EQU   *                   RESET CODE AT 'PREC' TO RETURN               
         MVC   PREC(2),NOOPCODE                                                 
         B     TRACE010                                                         
NOOPCODE BR    RE                                                               
TRACE010 EQU   *                                                                
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
         GOTO1 REPORT                                                           
         MVC   P(24),=C'>>> PROCESSING ERROR <<<'                               
         GOTO1 REPORT                                                           
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
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
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
         DC    AL1(SUBFRST),AL3(GRUPINIT)   START OF SUBGROUP/GROUP             
         DC    AL1(STAFRST),AL3(STAINIT)    STATION FIRST                       
         DC    AL1(PROCCONT),AL3(POST)      PROCESS A CONTRACT                  
         DC    AL1(STALAST),AL3(STADONE)    STATION BREAK                       
         DC    AL1(SUBLAST),AL3(GRUPDONE)   SUBGROUP BREAK                      
         DC    AL1(REQLAST),AL3(RPTDONE)    END OF REPORT                       
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*                                                                               
*        INITIAL --- PROCESS TO START THE REPORT                                
*                                                                               
*        - SET DATES FOR BUCKETING THE CONTRACT DOLLARS                         
*        - SET INITIAL REPORT VALUES                                            
*                                                                               
INITIAL  NTR1                                                                   
         GOTO1 =A(INITIAL2),DMCB,(RC)                                           
         BAS   RE,INITADDR                                                      
         BNZ   INITBAD                                                          
         BAS   RE,INITVALS                                                      
         BNZ   INITBAD                                                          
*                                                                               
* SET TO GENERATE RECOVERY OFFLINE COPIES                                       
* SINCE DXTRACT TO MO REQUIRES COPIES                                           
*                                                                               
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     R4,MASTC                                                         
         USING MASTD,R4                                                         
         DROP  RF                                                               
         L     R6,MCSSB                                                         
         USING SSBOFFD,R6                                                       
         OI    SSOSTAT2,SSOSROLC                                                
         DROP  R4,R6                                                            
*                                                                               
*                                                                               
         CLC   QSTATION,SPACES     STATION ON REQUEST CARD?                     
         BE    INIT10              NO  - (REQUEST REQUIRES IT, SO               
*                                     THIS IS NOT POSSIBLE)                     
         MVC   CURSTA(5),QSTATION  YES - INSERT STATION CALL LETTERS            
*        BAS   RE,SETSTA           UPDATE STATION RECORD                        
         GOTO1 =A(SETSTA),DMCB,(RC)                                             
         BNZ   INITBAD                                                          
INIT10   EQU   *                                                                
         XC    SORTREC2(76),SORTREC2                                            
         XC    STACONS(8),STACONS                                               
         XC    GRPCONS(8),STACONS                                               
         XC    OFFCONS(8),STACONS                                               
         XC    TOTCONS(8),STACONS                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
*                                                                               
INITGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
INITBAD  EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P(37),=C'>>> ERROR IN INITIALIZING PROGRAM <<<'                  
         GOTO1 REPORT                                                           
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        GRUPINIT --- PROCESS FOR FIRST/CHANGE OF SUBGROUP                      
*                                                                               
GRUPINIT NTR1                                                                   
*                                                                               
         MVC   GRPNAME(10),RGRPNAME                                             
         MVC   SGRPNAME(10),RGRPSBNM                                            
         XC    SORTREC(76),SORTREC              SET UP BASIC KEY                
*                                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        STAINIT --- PROCESS FOR FIRST ENCOUNTER OF A STATION                   
*                                                                               
*        - READ COMMISSION RATE RECORDS FOR A STATION AND BUILD                 
*           A TABLE                                                             
*                                                                               
*                                                                               
STAINIT  NTR1                                                                   
*                                                                               
         CLC   QSTATION,SPACES     STATION ON REQUEST CARD?                     
         BNE   STAI10              YES - UPDATED IN 'INITIAL' RTN               
*                                                                               
*                                  NO  - REQUEST DEMANDS STA CALLS, SO          
*                                     THIS IS NOT POSSIBLE                      
         MVC   CURSTA(5),RSTAKSTA  INSERT STATION CALL LETTERS                  
*        BAS   RE,SETSTA           UPDATE STATION RECORD                        
         GOTO1 =A(SETSTA),DMCB,(RC)                                             
         BNZ   STAIBAD                                                          
         MVI   STAREAD,C'Y'        SET 'STATION REC READ' FLAG                  
STAI10   EQU   *                                                                
*                                                                               
         MVC   P+0(4),RSTAKSTA     STATION-BAND/MARKET ON 1ST LINE              
         LA    R3,P+3                                                           
         CLI   0(R3),0                                                          
         BE    STAI15                                                           
         CLI   0(R3),C' '                                                       
         BNE   STAI20                                                           
STAI15   EQU   *                                                                
         LA    R3,P+2                                                           
STAI20   EQU   *                                                                
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         CLI   RSTAKSTA+4,0                                                     
         BE    STAI30                                                           
         CLI   RSTAKSTA+4,C' '                                                  
         BNE   STAI40                                                           
STAI30   EQU   *                                                                
         MVC   0(2,R3),=C'TV'                                                   
         B     STAI60                                                           
STAI40   EQU   *                                                                
         MVC   0(1,R3),RSTAKSTA+4                                               
         CLI   RSTAKSTA+4,C'T'                                                  
         BNE   STAI50                                                           
         MVI   1(R3),C'V'                                                       
         B     STAI60                                                           
STAI50   EQU   *                                                                
         MVI   1(R3),C'M'                                                       
STAI60   EQU   *                                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),C'/'                                                       
         MVC   1(20,R3),RSTAMKT                                                 
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
*        STAINIT EXIT                                                           
*                                                                               
STAIGOOD EQU   *                                                                
         MVC   SVMKT(20),RSTAMKT                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
STAIBAD  EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P(38),=C'>>> ERROR IN INITIALIZING STATION <<<'                  
         GOTO1 REPORT                                                           
         LA    R0,1                                                             
         B     TESTEXIT                                                         
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
         GOTO1 DOPRINT,DMCB,1                                                   
         BNZ   STADBAD                                                          
*                                                                               
STADGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
STADBAD  EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P(42),=C'>>> ERROR IN PROCESSING STATION TOTALS <<<'             
         GOTO1 REPORT                                                           
         LA    R0,1                                                             
         B     TESTEXIT                                                         
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
         GOTO1 DOPRINT,DMCB,2                                                   
         BNZ   GRPDBAD                                                          
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
GRPDGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
GRPDBAD  EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P(40),=C'>>> ERROR IN PROCESSING GROUP TOTALS <<<'               
         GOTO1 REPORT                                                           
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        RPTDONE --- PROCESS TO FINISH THE REPORT                               
*                                                                               
*                                                                               
*        - PRINT THE REPORT/OFFICE TABLE                                        
*        - FINISH THE REPORT                                                    
*                                                                               
RPTDONE  NTR1                                                                   
         MVC   SGRPNAME(10),SPACES     NO SUB-GROUP NAME ON RPT TOT             
         GOTO1 DOPRINT,DMCB,4                                                   
         BNZ   GRPRBAD                                                          
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     RPTD0008                                                         
GRPRBAD  EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P(40),=C'>>> ERROR IN PROCESSING GROUP TOTALS <<<'               
         GOTO1 REPORT                                                           
RPTD0008 EQU   *                                                                
         XC    OFFACCS(8),OFFACCS                                               
*                                                                               
* REPORT TOTAL HAS ALREADY BEEN GENERATED                                       
* CAN USE ITS MEMORY STORAGE FOR OFFICE TOTALS                                  
         XC    TOTACCS(8),TOTACCS  NEED ONLY TWO BUCKETS                        
*        XC    TOTCONS(8),TOTCONS                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         OI    MYFLAG,ISOFF        SUMMARY BY OFFICE (FOR HOOK ROUTINE)         
*                                                                               
         GOTO1 SORTER,DMCB,=C'GET' IN CASE OF MULTIPLE STATION                  
         L     R6,DMCB+4           GET STATION/OFFICE/MARKET FROM               
         LTR   R6,R6               SORTREC                                      
         BZ    RPTDGOOD            TEST RETURN ZERO=EOF                         
         MVC   SORTREC,0(R6)                                                    
*                                                                               
         GOTO1 =A(PRNTSTA),DMCB,(RC)                                            
         BAS   RE,PRNTHD           PRINT COLUMNS NAMES                          
*                                                                               
RPTD0010 EQU   *                                                                
         MVC   SORTREC,0(R6)                                                    
         OC    SORTREC2(76),SORTREC2   FIRST TIME THROUGH?                      
         BZ    RPTD0080            YES                                          
         CLC   SORTREC+SOFF(2),SORTREC2+SOFF                                    
         BE    RPTD0080                                                         
         GOTO1 DOPRINT,DMCB,3      PRINT OFFICE REPORT                          
         BNZ   SOFFDBAD                                                         
RPTD0080 EQU   *                                                                
         LA    R2,OFFACCS          OFFICE TOTALS                                
*        LA    R6,OFFCONS                                                       
         BAS   RE,CALCTOT          CALCULATE TOTALS                             
         LA    R2,TOTACCS          OFFICE TOTALS                                
*        LA    R6,TOTCONS                                                       
         BAS   RE,CALCTOT          CALCULATE TOTALS                             
*        MVC   P(9),SORTREC        ***TEST***                                   
*        EDIT  SCONFINV,(9,P+12),2 ***TEST***                                   
*        EDIT  SCONOINV,(9,P+24),2 ***TEST***                                   
*        GOTO1 REPORT              ***TEST***                                   
         MVC   SORTREC2(76),SORTREC                                             
         B     RPTD0090                                                         
SOFFDBAD EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P(42),=C'>>> ERROR IN PROCESSING OFFICE TOTALS <<<'              
         GOTO1 REPORT                                                           
RPTD0090 EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    RPTDGOOD            TEST RETURN ZERO=EOF                         
         B     RPTD0010                                                         
*                                                                               
RPTDGOOD EQU   *                                                                
         GOTO1 DOPRINT,DMCB,3      OFFICE                                       
         MVC   P+2(5),=C'TOTAL'                                                 
         GOTO1 MONLN2,DMCB,TOTACCS                                              
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+2(21),=C'*** END OF REPORT ***'                                
         GOTO1 REPORT                                                           
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
RPTDBAD  EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P(41),=C'>>> ERROR IN PROCESSING REPORT TOTALS <<<'              
         GOTO1 REPORT                                                           
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
PRNTHD   NTR1                                                                   
         L     R3,ANEWMON          A(NEW MONTH TABLE)                           
         LA    R4,P                                                             
*                                                                               
PRHD02   EQU   *                                                                
         CLC   0(4,R3),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    PRHD10              FOUND - BEGIN TO PULL FIGURES                
         BH    PRHD20                                                           
         LA    R3,NEXTBUCK(R3)     BUMP TO NEXT BUCKET                          
         B     PRHD02              GO BACK FOR NEXT                             
*                                                                               
PRHD10   EQU   *                   FIRST MONTH                                  
         LA    R4,P                                                             
         GOTO1 DATCON,DMCB,(0,0(R3)),(9,12(R4))                                 
         LA    R3,NEXTBUCK(R3)     NEXT MONTH IN TABLE                          
         LA    R4,31(R4)                                                        
         B     PRHD02                                                           
*                                                                               
PRHD20   EQU   *                   ALL OTHER MONTHS                             
*                                                                               
         GOTO1 DATCON,DMCB,(0,0(R3)),(9,0(R4))  SECOND MONTH                    
PRHD30   EQU   *                                                                
         CLC   0(4,R3),QFAEND                                                   
         BL    PRHD40                                                           
         MVI   6(R4),C'-'                                                       
         GOTO1 DATCON,DMCB,(0,0(R3)),(9,7(R4))  LAST MONTH                      
         LA    R4,30(R4)                                                        
         B     PRHD50                                                           
PRHD40   EQU   *                                                                
         LA    R3,NEXTBUCK(R3)     NEXT MONTH IN TABLE                          
         B     PRHD30                                                           
*                                                                               
PRHD50   EQU   *                                                                
         MVC   0(5,R4),=C'TOTAL'                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
CALCTOT  NTR1                                                                   
         LA    R3,SCONFINV         CONTRACT TOTALS                              
         LA    R5,2                2 BUCKETS                                    
RLNE20   EQU   *                                                                
         L     R4,0(R2)                                                         
         A     R4,0(R3)                                                         
         ST    R4,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,RLNE20                                                        
*        L     R2,0(R6)            COUNT 1 STATION PROCESSED                    
*        A     R2,=F'1'                                                         
*        ST    R2,0(R6)                                                         
*        TM    SFLAG,RECUPD                                                     
*        BZ    RLNE30                                                           
*        L     R2,4(R6)            COUNT 1 STATION PROCESSED                    
*        A     R2,=F'1'                                                         
*        ST    R2,4(R6)                                                         
RLNE30   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        POST --- PROCESS A CONTRACT                                            
*                                                                               
         SPACE 2                                                                
POST     NTR1                                                                   
         XC    SORTREC(76),SORTREC                                              
         CLC   =C'DISPCON',QUESTOR DISPLAY CONTRACT RECORDS?                    
         BNE   POST0010            CONTR COMMENT ADDED, DON'T DUMP REC          
         MVC   P(05),=C'PRE :'                                                  
         GOTO1 REPORT                                                           
         PRINT GEN                                                              
         ZICM  RF,RCONLEN,2                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,RCONREC),RCONREC,C'DUMP',(RF),       X        
               =C'1D'              *** PUT CONTINUE MARK BACK IN                
         PRINT NOGEN                                                            
POST0010 EQU   *                                                                
*                                                                               
         CLI   QOPTION3,C'Y'       BACK OUT CLOSEOUT?                           
         BNE   POST0020            NO                                           
         GOTO1 =A(BACKOUT),DMCB,(RC)                                            
         B     POST0330            GO FINISH RECORD                             
*                                                                               
POST0020 EQU   *                                                                
         CLI   QOPTION4,C'T'                                                    
         BNE   POST0040                                                         
*                                                                               
         MVC   P(15),=CL15'POST CONTRACT'                                       
         MVC   P+20(27),RCONREC                                                 
         GOTO1 HEXOUT,DMCB,RCONREC,P+50,27,=C'N',0                              
         MVC   P+110(3),=C'LEN'                                                 
         GOTO1 HEXOUT,DMCB,RCONLEN,P+115,2,=C'N'                                
         GOTO1 REPORT                                                           
POST0040 EQU   *                                                                
*                                                                               
*- RE-READ CONTRACT RECORD IN CASE INTERNAL DATA MANAGER POINTERS               
*-   WERE CHANGED BY AN INTERVENING READ OF THE STATION RECORD.                 
*                                                                               
         CLI   STAREAD,C'Y'        STATION RECORD READ?                         
         BNE   POST0060            NO                                           
         MVI   STAREAD,C'N'        TURN OFF FLAG                                
         MVC   KEY(27),RCONKEY                                                  
         BAS   RE,READ                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIOAREA          SAVE A(IOAREA)                               
         LA    RF,RCONREC                                                       
         ST    RF,AIOAREA          POINT TO CONTRACT BUFFER                     
         BAS   RE,GREC                                                          
         ST    R4,AIOAREA          PUT BACK A(IOAREA)                           
POST0060 EQU   *                                                                
*                                                                               
*- SKIP CONTRACTS ENDING PRIOR TO REQUEST START                                 
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK)                              
         GOTO1 GETBROAD,DMCB,(0,WORK),WORK+6                                    
*                                                                               
         CLI   QOPTION4,C'T'                                                    
         BNE   POST0080                                                         
*                                                                               
         MVC   P(20),=CL20'WORK VS QFASTART'                                    
         MVC   P+24(4),WORK+12     BC MONTH END                                 
         MVC   P+32(4),QFASTART    RQST START                                   
         GOTO1 REPORT                                                           
POST0080 EQU   *                                                                
*                                                                               
         CLC   WORK+12(4),QFASTART                                              
         BL    POSTGOOD                                                         
*                                                                               
*- SKIP CONTRACTS STARTING AFTER REQUEST END                                    
*                                                                               
*  NOTE: USE CALENDAR END YYMM FROM GETBROAD OF CONTRACT START DATE             
*        TO DETERMINE BROADCAST MONTH.                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,WORK)                                
         GOTO1 GETBROAD,DMCB,(0,WORK),WORK+6                                    
*                                                                               
         CLI   QOPTION4,C'T'                                                    
         BNE   POST0100                                                         
*                                                                               
         MVC   P(20),=CL22'WORK VS QFAEND'                                      
         MVC   P+24(4),WORK+12     BC MONTH START                               
         MVC   P+32(4),QFAEND      RQST END                                     
         GOTO1 REPORT                                                           
POST0100 EQU   *                                                                
*                                                                               
         CLC   WORK+12(4),QFAEND                                                
         BH    POSTGOOD                                                         
*                                                                               
*- CLEAR OUT CONTRACT BUCKETS                                                   
         XC    CONACCS(48),CONACCS                                              
         MVI   CONUPED,0           ASSUME NO UPDATE                             
*                                                                               
         L     R2,STACONS          COUNT 1 STATION PROCESSED                    
         A     R2,=F'1'                                                         
         ST    R2,STACONS                                                       
*                                                                               
*- LOOP THROUGH MONTHLY BUCKETS FROM VALU2NEW.                                  
*                                                                               
*  IF MONTH HAS BEEN INVOICED, ADD AN X'04' ELEMENT OF OPPOSITE SIGN            
*  ELSE ADD X'04' ELEMENT OF 0 DOLLARS                                          
*                                                                               
         L     R2,ANEWMON          A(NEW MONTH TABLE)                           
         LA    R3,CONACCS                                                       
         MVI   BYTE,X'04'          INVOICE ELEMENT CODE                         
*                                                                               
POST0120 EQU   *                                                                
         CLC   0(4,R2),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST0140            FOUND - BEGIN TO PULL FIGURES                
         BH    POST0270            NOT FOUND - SHOULDN'T HAPPEN                 
         LA    R2,NEXTBUCK(R2)     BUMP TO NEXT BUCKET                          
         B     POST0120            GO BACK FOR NEXT                             
*                                                                               
POST0140 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE                                 
         BE    POST0270            YES                                          
         CLC   0(4,R2),QFAEND      TABLE ENTRY VS REQ END   DATE                
         BH    POST0270            TABLE > END DATE - EXIT                      
         LR    R6,R2               SET R6 TO A(BUCKETS IN MONTH)                
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROLS                          
*                                                                               
POST0160 EQU   *                                                                
         CLI   QOPTION4,C'T'                                                    
         BNE   POST0180                                                         
         MVC   P(20),=CL20'BUCKET AT POST20'                                    
         GOTO1 HEXOUT,DMCB,(R2),P+22,40,=C'N',0                                 
         GOTO1 REPORT                                                           
POST0180 EQU   *                                                                
*                                                                               
         SR    R5,R5                                                            
         L     R5,GROSSORD(R6)     GET STATION AMOUNT -                         
*                                  AS THIS IS NEVER USED, I'M NOT               
*                                     SURE WHY IT'S SET HERE.  IF NO            
*                                     INVOICE DATA, POST0200 REUSES             
*                                     R5 RIGHT AWAY.                            
*                                     THIS ROUTINE WANTS INVOICE DATA,          
*                                      NOT GROSS ORDERED.                       
         TM    FLAG6(R2),X'01'     TEST ANY INVOICE DATA                        
         BNO   POST0200                                                         
         L     R5,CUASATIN(R6)     GET CURRENT INVOICE AMOUNT                   
         LTR   R5,R5               INVOICE ALREADY ZERO?                        
         BZ    POST0260            YES                                          
*                                                                               
         CLI   QOPTION2,C'D'       DON'T CLEAR INVOICE?                         
****>>>  BE    POST0260                                                         
*                                                                               
*   TURN OFF 'CLEAR' OPTION:  DEC13/96 - BILL UHR                               
*                                                                               
         B     POST0260                                                         
*---------------------------------------------------------------------*         
*                  <<<<<< BEGIN NOP'D CODE >>>>>                      *         
*---------------------------------------------------------------------*         
*- ADD CREDIT ELEMENT                                                           
         ST    R5,0(R3)            STORE AMOUNT TO BE REMOVED                   
         LTR   R5,R5               TEST FOR POSITIVE/NEGATIVE AMOUNT            
         BM    POST0184            NEGATIVE:  LOAD POSITIVE REGISTER            
         LNR   R5,R5               NEGATE AMOUNT                                
         B     POST0188                                                         
POST0184 EQU   *                                                                
         LPR   R5,R5               'NEGATE' (MAKE POSITIVE) AMOUNT              
POST0188 EQU   *                                                                
         XC    ELAREA(10),ELAREA                                                
         MVC   ELAREA+0(1),BYTE                                                 
         MVI   ELAREA+1,X'0A'                                                   
         GOTO1 DATCON,DMCB,(0,0(R2)),(3,WORK)                                   
         MVC   ELAREA+2(2),WORK                                                 
         MVC   ELAREA+4(2),COMPMON                                              
         STCM  R5,15,ELAREA+6                                                   
         CLI   QOPTION4,C'T'                                                    
         BNE   POST0190                                                         
         MVC   P(13),=C'INVOICE DATA:'                                          
         MVC   P+16(10),ELAREA                                                  
         EDIT  (4,0(R3)),(10,P+30)                                              
         GOTO1 REPORT                                                           
POST0190 EQU   *                                                                
*                                                                               
         B     POST0220                                                         
*---------------------------------------------------------------------*         
*                  <<<<<< END NOP'D CODE >>>>>                        *         
*---------------------------------------------------------------------*         
*                                                                               
*- ADD 0 DOLLAR ELEMENT                                                         
POST0200 EQU   *                                                                
         L     R5,TOTORD(R6)        GET STATION AMOUNT                          
         A     R5,CUASATIN(R6)                                                  
         ST    R5,0(R3)                                                         
         XC    ELAREA(10),ELAREA                                                
         MVC   ELAREA+0(1),BYTE                                                 
         MVI   ELAREA+1,X'0A'                                                   
         GOTO1 DATCON,DMCB,(0,0(R2)),(3,WORK)                                   
         MVC   ELAREA+2(2),WORK                                                 
         MVC   ELAREA+4(2),COMPMON                                              
*                                                                               
*- ADD ELEMENT TO CONTRACT RECORD.  CHECK FOR OVERFLOW                          
*                                                                               
*  ADJUST RECORD LENGTH TO WORK AROUND 'HELLO' 'FEATURE'                        
*  OF ADDING AN EXTRA BYTE OF 0 AT END OF RECORD ON 1ST TIME                    
*  ELEMENT ADD.  ONLY CHANGE RECORD LENGTH BY ELEMENT LENGTH.                   
*                                                                               
POST0220 EQU   *                                                                
         MVI   CONUPED,X'01'       UPDATING THE CONTRACT                        
*                                                                               
         SR    R8,R8                                                            
         ICM   R8,3,RCONLEN        RECORD LENGTH                                
         ZIC   RF,ELAREA+1                                                      
         AR    R8,RF               +ELEM LEN = NEW REC LEN.                     
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE '),RCONREC,ELAREA,0,0,  X        
               RR=RELO                                                          
*                                                                               
         STCM  R8,3,RCONLEN                                                     
*                                                                               
         CLI   QOPTION4,C'T'                                                    
         BNE   POST0240                                                         
         L     R5,DMCB+12                                                       
         MVC   P(20),=CL20'RECORD LENGTH'                                       
         GOTO1 HEXOUT,DMCB,RCONLEN,P+22,2,=C'N',0                               
         GOTO1 REPORT                                                           
         ST    R5,DMCB+12                                                       
*                                                                               
POST0240 EQU   *                                                                
         CLI   DMCB+12,0                                                        
         BE    POST0260                                                         
*                                                                               
         XC    CONACCS(48),CONACCS CLEAR CONTRACT BUCKETS                       
         XC    SCONACCS(8),SCONACCS CLEAR CONTRACT BUCKETS                      
         BAS   RE,RPTLINE          PRINT CONTRACT WE CAN'T CLOSE                
         BAS   RE,SORTGEN                                                       
         MVI   CONUPED,0                                                        
         MVC   P(42),=C'> CONTRACT RECORD TOO LARGE TO CLOSE OUT <'             
         EDIT  RCONLEN,(4,P+45)                                                 
         GOTO1 REPORT                                                           
         B     POSTBAD                                                          
*                                                                               
POST0260 EQU   *                                                                
         LA    R2,NEXTBUCK(R2)     NEXT SET OF BUCKETS                          
         LA    R3,4(R3)                                                         
         BCT   R4,POST0140                                                      
         SPACE 2                                                                
*                                                                               
*- PROCESS ALTERNATE BUCKETS IF NOT DONE ALREADY                                
POST0270 EQU   *                                                                
         CLI   BYTE,X'54'          ALREADY DONE ALTERNATE INVOICE $?            
         BE    POST0280            YES                                          
*                                                                               
         XC    DMCB(6*4),DMCB      BUILD VALU2NEW PARMS                         
*                                                                               
         MVC   DMCB+4(4),ANEWMON                                                
         MVC   DMCB+8(4),AMONINFO                                               
         MVC   DMCB+12(4),AMONFORC                                              
         GOTO1 =V(VALU2NEW),DMCB,(X'80',RCONREC)                                
*                                                                               
         MVI   BYTE,X'54'          ELEMENT CODE                                 
*                                                                               
         L     R2,ANEWMON          A(NEW MONTH TABLE)                           
         LA    R3,CONACCS                                                       
         B     POST0120            PROCESS ALTERNATE BUCKETS                    
*                                                                               
*- END OF BUCKETS....WRITE BACK CONTRACT RECORD IF UPDATED                      
POST0280 EQU   *                                                                
         CLI   QOPTION4,C'T'                                                    
         BNE   POST0300                                                         
         MVC   P(20),=CL20'AT POST0270 UPD='                                    
         LA    RF,X'00F0'                                                       
         IC    RE,CONUPED                                                       
         OR    RF,RE                                                            
         STC   RF,P+20                                                          
         GOTO1 REPORT                                                           
POST0300 EQU   *                                                                
         CLI   CONUPED,0           RECORD UPDATED?                              
         BE    POSTGOOD            NO, ALL DONE                                 
*                                                                               
         XC    ELAREA(16),ELAREA   BUILT '2B' CLOSEOUT HISTORY ELEM             
         MVI   ELAREA+0,X'2B'                                                   
         MVI   ELAREA+1,4                                                       
         GOTO1 DATCON,DMCB,(5,0),(2,ELAREA+2)                                   
*                                                                               
*- COMPENSATE FOR 'HELLO' ADD A ZERO AT THE END FEATURE.                        
         SR    R8,R8                                                            
         ICM   R8,3,RCONLEN        RECORD LENGTH                                
         ZIC   RF,ELAREA+1                                                      
         AR    R8,RF               +ELEM LEN = NEW REC LEN.                     
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE '),RCONREC,ELAREA,0,0,  X        
               RR=RELO                                                          
*                                                                               
         STCM  R8,3,RCONLEN                                                     
*                                                                               
POST0310 EQU   *                                                                
         CLI   QOPTION4,C'T'                                                    
         BNE   POST0320                                                         
         MVC   P(20),=CL20'RECORD LENGTH CMT'                                   
         GOTO1 HEXOUT,DMCB,RCONLEN,P+22,2,=C'N',0                               
         GOTO1 REPORT                                                           
*                                                                               
POST0320 EQU   *                                                                
         L     R2,STACONS+4        BUMP UPDATED RECORD COUNT                    
         A     R2,=F'1'                                                         
         ST    R2,STACONS+4                                                     
*        OI    SFLAG,RECUPD        RECORD HAS BEEN UPDATED                      
*                                                                               
POST0330 EQU   *                                                                
*                                                                               
         CLC   =C'DISPCON',QUESTOR DISPLAY CONTRACT RECORDS?                    
         BNE   POST0335            CONTR COMMENT ADDED, DON'T DUMP REC          
         MVC   P(05),=C'POST:'                                                  
         GOTO1 REPORT                                                           
         PRINT GEN                                                              
         ZICM  RF,RCONLEN,2                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,RCONREC),RCONREC,C'DUMP',(RF),       X        
               =C'1D'              *** PUT CONTINUE MARK BACK IN                
         PRINT NOGEN                                                            
POST0335 EQU   *                                                                
         CLI   QOPTION1,C'U'       UPDATE ON?                                   
         BNE   POST0380            NO  - SKIP UPDATE                            
         L     R2,AIOAREA          SET IOAREA TO CONTRACT REC                   
         LA    R3,RCONREC                                                       
         ST    R3,AIOAREA                                                       
*                                                                               
         CLI   RCONREC,X'0C'                                                    
         BE    *+6                                                              
         DC    H'0'                RECORD NOT A CONTRACT                        
*                                                                               
         CLI   QOPTION4,C'T'                                                    
         BNE   POST0340                                                         
*                                                                               
         MVC   P(10),=CL20'WRIT CON  KEY/DA/REC'                                
         MVC   P+22(27),KEY                                                     
         GOTO1 HEXOUT,DMCB,KEY+28,P+52,4,=C'N',0                                
         MVC   P+70(27),0(R3)      KEY IN RECORD                                
         GOTO1 REPORT                                                           
*                                                                               
POST0340 EQU   *                                                                
         CLI   QOPTION4,C'T'       TRACE OPTION?                                
         BE    POST0360            YES - SKIP REWRITE                           
         CLI   QOPTION4,C'H'       HARD-UPDATE TEST OPTION?                     
         BE    POST0360            YES - SKIP REWRITE                           
*                                                                               
         BAS   RE,PREC             PUT OUT NEW RECORD                           
*                                                                               
POST0360 EQU   *                                                                
*                                                                               
         ST    R2,AIOAREA          RESET ADDR                                   
*                                                                               
         CLI   QOPTION4,C'T'                                                    
         BNE   POST0380                                                         
*                                                                               
         MVC   P(10),=CL20'AFTER CON UPDATE'                                    
         MVC   P+22(27),KEY                                                     
         GOTO1 HEXOUT,DMCB,KEY+28,P+52,4,=C'N',0                                
         MVC   P+70(27),0(R3)      KEY IN RECORD                                
         GOTO1 REPORT                                                           
*                                                                               
POST0380 EQU   *                                                                
*                                                                               
         CLI   QOPTION3,C'Y'       BACKOUT REQUEST?                             
         BNE   POST0390            NO  - PROCEED                                
         CLI   CONUPED,0           YES - CONTRACT UPDATED?                      
         BE    POSTGOOD            NO  - DON'T DISPLAY ANYTHING                 
POST0390 EQU   *                                                                
         BAS   RE,RPTLINE          PRINT OUT DETAILS OF THIS LINE               
         BAS   RE,TWOBUCK          CONVERT 12 MONTHS BUCKETS INTO               
*                                  2 BUCKETS (1 MONTH + OTHER MONTHS)           
         BAS   RE,SORTGEN                                                       
*                                                                               
         LA    R2,CONACCS                                                       
         LA    R3,STAACCS                                                       
         LA    R4,12                                                            
POST0400 EQU   *                                                                
         L     R0,0(R3)                                                         
         A     R0,0(R2)                                                         
         ST    R0,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,POST0400                                                      
*                                                                               
*        POST EXIT                                                              
*                                                                               
POSTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
POSTBAD  EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P(35),=C'>>> ERROR IN POSTING A CONTRACT <<<'                    
         GOTO1 REPORT                                                           
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
         EJECT                                                                  
*                                                                               
TWOBUCK  NTR1                                                                   
         LA    R2,CONACCS                                                       
         LA    R3,SCONACCS                                                      
         MVC   0(4,R3),0(R2)       FIRST MONTH                                  
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,11               MAX 11 BUCKETS LEFT                          
         SR    R5,R5               CLEAR TOTAL                                  
TB10     EQU   *                                                                
         A     R5,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R4,TB10                                                          
         ST    R5,0(R3)                                                         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* GENERATE SORT RECORDS                                                         
*                                                                               
SORTGEN  NTR1                                                                   
         MVC   SORTREC+SOFF(2),RCONKOFF                                         
         MVC   SORTREC+SSTA(5),RCONKSTA                                         
         MVC   SORTREC+SGRP(2),RCONKGRP                                         
         LA    R3,SCONACCS                                                      
         MVC   SCONFINV(L'SCONFINV),0(R3)                                       
         MVC   SCONOINV(L'SCONOINV),4(R3)                                       
*        MVC   SCONNUM(4),RCONKCON                                              
*        MVC   SCONAGY(4),RCONKAGY                                              
*        MVC   SCONAOFF(2),RCONKAOF                                             
*        MVC   SCONADV(4),RCONKADV                                              
*        MVC   SCONTYPE(1),RCONTYPE                                             
         MVC   SSTAMKT(20),SVMKT                                                
*        MVC   SGRPNM(10),GRPNAME                                               
*        MVC   SSGRPNM(10),SGRPNAME                                             
*        MVC   P(9),SORTREC        ***TEST***                                   
*        EDIT  SCONFINV,(6,P+12)   ***TEST***                                   
*        EDIT  SCONOINV,(9,P+24)   ***TEST***                                   
*        GOTO1 REPORT              ***TEST***                                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        RPTLINE --- REPORT ON CURRENT CONTRACT                                 
*                                                                               
RPTLINE  NTR1                                                                   
*                                                                               
         CLI   LINE,51                                                          
         BL    RLNE10                                                           
         MVI   FORCEHED,C'Y'                                                    
RLNE10   EQU   *                                                                
*                                                                               
         MVC   P+02(17),=C'CONTRACT NUMBER ='                                   
         GOTO1 HEXOUT,DMCB,RCONKCON,P+20,4,=C'TOG'                              
         MVC   P+30(8),=C'OFFICE ='                                             
         MVC   P+39(2),RCONKOFF                                                 
         MVC   P+42(16),=C'AGENCY/AGY OFF ='                                    
         MVC   P+59(4),RCONKAGY                                                 
         MVI   P+63,C'/'                                                        
         MVC   P+64(2),RCONKAOF                                                 
         MVC   P+67(12),=C'ADVERTISER ='                                        
         MVC   P+80(4),RCONKADV                                                 
         MVC   P+85(15),=C'CONTRACT TYPE ='                                     
         MVC   P+101(1),RCONTYPE                                                
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 MONLINE,DMCB,CONACCS                                             
*                                                                               
RLNEGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        MONLINE --- PRINT MONTHLY DATA                                         
*                                                                               
*        PARM1 =  A(ACCUMS)                                                     
*                                                                               
MONLINE  NTR1                                                                   
*                                                                               
         L     R2,0(R1)            ADDR OF ACCUMS                               
         L     R3,ANEWMON          A(NEW MONTH TABLE)                           
         LA    R6,2                                                             
         SR    R7,R7                                                            
MLNE02   EQU   *                                                                
         CLC   0(4,R3),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    MLNE10              FOUND - BEGIN TO PULL FIGURES                
         BH    MLNE40              NOT FOUND - SHOULDN'T HAPPEN                 
         LA    R3,NEXTBUCK(R3)     BUMP TO NEXT BUCKET                          
         B     MLNE02              GO BACK FOR NEXT                             
*                                                                               
MLNE10   EQU   *                                                                
         LA    R4,P                                                             
         LA    R5,6                                                             
MLNE20   EQU   *                                                                
         CLI   0(R3),0             SHORT RUN (LESS THAN 12 MONTHS?)             
         BE    MLNE25              NO                                           
         CLC   0(4,R3),QFAEND      TABLE ENTRY VS REQ END   DATE                
         BNH   MLNE40              TABLE > END DATE - EXIT                      
*                                                                               
MLNE25   EQU   *                                                                
*                                                                               
*- CHECK FOR PARTIAL PRINT LINE.                                                
         SR    R6,R6               FORCE END OF LOOP                            
         LA    RF,6                                                             
         CR    RF,R5               IF R5<>6, WE STARTED TO BUILD LINE.          
         BNE   MLNE50              PRINT PARTIAL LINE                           
*                                                                               
         B     MLNE100             NO PARTIAL.  (6 MONTH REQUEST)               
*                                                                               
MLNE40   EQU   *                                                                
         A     R7,0(R2)                                                         
         GOTO1 DATCON,DMCB,(0,0(R3)),(9,0(R4))                                  
         EDIT  (B4,0(R2)),(12,7(R4)),2,FLOAT=-,ZERO=NOBLANK                     
         LA    R2,4(R2)                                                         
         LA    R3,NEXTBUCK(R3)     NEXT MONTH IN TABLE                          
         LA    R4,20(R4)                                                        
         BCT   R5,MLNE20                                                        
MLNE50   GOTO1 REPORT                                                           
         BCT   R6,MLNE10                                                        
*                                                                               
MLNE100  EQU   *                                                                
         MVC   P(7),=C'TOTAL ='                                                 
         LA    R4,P+9                                                           
         EDIT  (R7),(12,0(R4)),2,FLOAT=-,ZERO=NOBLANK                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
MLNEGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
MONLN2   NTR1                                                                   
*                                                                               
         L     R2,0(R1)            ADDR OF ACCUMS                               
         SR    R7,R7               TOTAL                                        
*                                                                               
         LA    R4,P                                                             
         LA    R5,2                TWO BUCKETS                                  
*                                                                               
MONL20   EQU   *                                                                
         A     R7,0(R2)            TOTAL                                        
*                                                                               
         CH    R5,=H'2'            FIRST MONTH?                                 
         BL    MONL100             NO                                           
*                                                                               
         EDIT  (B4,0(R2)),(12,7(R4)),2,FLOAT=-,ZERO=NOBLANK                     
         LA    R2,4(R2)                                                         
         LA    R4,30(R4)                                                        
         BCT   R5,MONL20                                                        
*                                                                               
MONL100  EQU   *                                                                
         EDIT  (B4,0(R2)),(12,0(R4)),2,FLOAT=-,ZERO=NOBLANK                     
         LA    R4,25(R4)                                                        
         EDIT  (R7),(12,0(R4)),2,FLOAT=-,ZERO=NOBLANK                           
         GOTO1 REPORT                                                           
*                                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
MONERR   EQU   *                                                                
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*        DOPRINT --- PRINT BREAK TOTALS                                         
*                                                                               
*        PARM1  =  1 DO STATION                                                 
*                  2 DO GROUP                                                   
*                  3 DO REPORT                                                  
*                                                                               
         SPACE 3                                                                
DOPRINT  NTR1                                                                   
*                                                                               
         ZIC   R2,3(R1)            GET PASSED VALUE INTO R2                     
         STC   R2,PRNTLEVL         STORE VALUE IN PRNTLEVL                      
*                                                                               
         CLI   PRNTLEVL,3          LEVEL 3 REPORTING?                           
         BE    DPNT02              SKIP PRINTING EXTRA LINE                     
         GOTO1 REPORT                                                           
*                                                                               
DPNT02   EQU   *                                                                
         CLI   LINE,51                                                          
         BL    DPNT05                                                           
         MVI   FORCEHED,C'Y'                                                    
DPNT05   EQU   *                                                                
*                                                                               
         CLI   PRNTLEVL,1          LEVEL 1 REPORTING?                           
         BNE   DPNT20              NO, CHECK FURTHER                            
         BAS   RE,DPNTSTA                                                       
         B     DPNT100                                                          
DPNT20   EQU   *                                                                
         CLI   PRNTLEVL,2          LEVEL 2 REPORTING                            
         BNE   DPNT40              NO, CHECK FURTHER                            
         BAS   RE,DPNTGRP                                                       
         B     DPNT100                                                          
DPNT40   EQU   *                                                                
         CLI   PRNTLEVL,3          LEVEL 3 REPORTING?                           
         BNE   DPNT60              NO, CHECK FURTHER                            
         BAS   RE,DPNTOFF                                                       
DPNT60   EQU   *                                                                
         CLI   PRNTLEVL,4          LEVEL 4 REPORTING?                           
         BNE   DPNT100             NO, NOW I'M LOST                             
         BAS   RE,DPNTRPT                                                       
DPNT100  EQU   *                                                                
*                                                                               
         CLI   PRNTLEVL,3          LEVEL 3 REPORTING?                           
         BE    DPNTGOOD            SKIP PRINTING EXTRA LINE                     
         GOTO1 REPORT                                                           
*                                                                               
*        DOREPORT TOTALS                                                        
*                                                                               
DPNTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
DPNTBAD  EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P(41),=C'>>> ERROR IN PRINTING AN OFFICE TABLE <<<'              
         GOTO1 REPORT                                                           
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        DPNTOFF --- DOPRINT OFFICE HOUSEKEEPING                                
*                                                                               
DPNTOFF  NTR1                                                                   
*                                                                               
         MVC   P+2(2),SORTREC2    LABEL OFFICE TOTALS                           
*        MVC   P+2(27),=C'TOTAL CONTRACTS PROCESSED ='                          
*        LA    R2,OFFCONS                                                       
*        EDIT  (B4,0(R2)),(6,P+30),ZERO=NOBLANK                                 
*        MVC   P+38(19),=C'CONTRACTS UPDATED ='                                 
*        EDIT  (B4,4(R2)),(6,P+58),ZERO=NOBLANK                                 
*        GOTO1 REPORT                                                           
*                                                                               
         GOTO1 MONLN2,DMCB,OFFACCS                                              
*                                                                               
         XC    OFFACCS(8),OFFACCS   CLEAR GROUP BUCKETS                         
*        XC    OFFCONS(8),OFFCONS                                               
*                                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        DPNTSTA --- DOPRINT STATION LEVEL HOUSEKEEPING                         
*                                                                               
DPNTSTA  NTR1                                                                   
*                                                                               
         ZIC   RF,LV1PRINT                                                      
         A     RF,=F'1'                                                         
         STC   RF,LV1PRINT                                                      
*                                                                               
         MVC   P(17),=C'* STATION TOTAL *'                                      
         GOTO1 REPORT                                                           
         LA    R2,STACONS                                                       
         MVC   P+2(27),=C'TOTAL CONTRACTS PROCESSED ='                          
         EDIT  (B4,0(R2)),(6,P+30),ZERO=NOBLANK                                 
         MVC   P+38(19),=C'CONTRACTS UPDATED ='                                 
         EDIT  (B4,4(R2)),(6,P+58),ZERO=NOBLANK                                 
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 MONLINE,DMCB,STAACCS                                             
*                                                                               
         LA    R2,STAACCS                                                       
         LA    R3,GRPACCS                                                       
         LA    R4,12                                                            
DSTA10   EQU   *                                                                
         L     R5,0(R3)                                                         
         A     R5,0(R2)                                                         
         ST    R5,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,DSTA10                                                        
         L     R5,GRPCONS                                                       
         A     R5,STACONS                                                       
         ST    R5,GRPCONS                                                       
         L     R5,GRPCONS+4                                                     
         A     R5,STACONS+4                                                     
         ST    R5,GRPCONS+4                                                     
*                                                                               
         XC    STAACCS(48),STAACCS  CLEAR STATION BUCKETS                       
         XC    STACONS(8),STACONS                                               
*                                                                               
DSTAGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        DPNTGRP --- DOPRINT GROUP HOUSEKEEPING                                 
*                                                                               
DPNTGRP  NTR1                                                                   
*                                                                               
         ZIC   RF,LV2PRINT         BUMP LEVEL 2 COUNT                           
         A     RF,=F'1'                                                         
         STC   RF,LV2PRINT                                                      
         MVI   LV1PRINT,0                                                       
         MVC   P(17),=C'** GROUP TOTAL **'                                      
         GOTO1 REPORT                                                           
         MVC   P+2(10),RGRPNAME    LABEL GROUP TOTALS                           
         MVC   P+13(10),RGRPSBNM                                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P+2(27),=C'TOTAL CONTRACTS PROCESSED ='                          
         LA    R2,GRPCONS                                                       
         EDIT  (B4,0(R2)),(6,P+30),ZERO=NOBLANK                                 
         MVC   P+38(19),=C'CONTRACTS UPDATED ='                                 
         EDIT  (B4,4(R2)),(6,P+58),ZERO=NOBLANK                                 
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 MONLINE,DMCB,GRPACCS                                             
*                                                                               
DGRP10   EQU   *                                                                
         LA    R2,GRPACCS                                                       
         LA    R3,TOTACCS                                                       
         LA    R4,12                                                            
DGRP20   EQU   *                                                                
         L     R5,0(R3)                                                         
         A     R5,0(R2)                                                         
         ST    R5,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,DGRP20                                                        
         L     R5,TOTCONS                                                       
         A     R5,GRPCONS                                                       
         ST    R5,TOTCONS                                                       
         L     R5,TOTCONS+4                                                     
         A     R5,GRPCONS+4                                                     
         ST    R5,TOTCONS+4                                                     
*                                                                               
         XC    GRPACCS(48),GRPACCS   CLEAR GROUP BUCKETS                        
         XC    GRPCONS(8),GRPCONS                                               
*                                                                               
DGRPGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        DPNTPRT --- DOPRINT REPOR HOUSEKEEPING                                 
*                                                                               
DPNTRPT  NTR1                                                                   
*                                                                               
         MVC   P(20),=C'*** REPORT TOTAL ***'                                   
         GOTO1 REPORT                                                           
         MVC   P+0(10),RGRPNAME    LABEL GROUP TOTALS                           
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P+2(27),=C'TOTAL CONTRACTS PROCESSED ='                          
         LA    R2,TOTCONS                                                       
         EDIT  (B4,0(R2)),(6,P+30),ZERO=NOBLANK                                 
         MVC   P+38(19),=C'CONTRACTS UPDATED ='                                 
         EDIT  (B4,4(R2)),(6,P+58),ZERO=NOBLANK                                 
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 MONLINE,DMCB,TOTACCS                                             
*                                                                               
DRPTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        INITADDR --- INITIALIZE LINK AND SPACE ADDRESSES                       
*                                                                               
INITADDR NTR1                                                                   
*                                                                               
         LA    R0,HHOOK                                                         
         ST    R0,HEADHOOK                                                      
*                                                                               
         L     R0,=V(IOBUFF)                                                    
         A     R0,RELO                                                          
         ST    R0,AIOBUFF          USE IT FOR INTERNAL I/O'S                    
         ST    R0,AIOAREA                                                       
*                                                                               
IADRGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        INITVALS --- INITIALIZE REPORT VALUES                                  
*                                                                               
INITVALS NTR1                                                                   
*                                                                               
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVI   QACCTOPT,C'P'                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(5,PRNTLEVL),(0,WORK) TODAY'S DATE YYMMDD            
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         CLI   DMCB,1                        MONDAY?                            
         BE    IVAL10                        YES                                
         ZIC   R2,DMCB                       ELSE, MAKE A MONDAY DATE           
         BCTR  R2,0                                                             
         LNR   R2,R2                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         MVC   WORK(6),WORK+6                                                   
IVAL10   EQU   *                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(2,COMPMON)                                 
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(3,WORK),0                                
         MVC   QSTRTBIN(2),WORK    YYMM START DATE (BINARY)                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,QEND),(3,WORK),0                                  
         MVC   QENDBIN(2),WORK     YYMM END DATE (BINARY)                       
*                                                                               
         CLI   QOPTION4,C'T'                                                    
         BNE   IVALGOOD                                                         
*                                                                               
         MVC   P(20),=CL20'START/END/MON DATES'                                 
         GOTO1 HEXOUT,DMCB,QSTRTBIN,P+22,2,=C'N'                                
         GOTO1 HEXOUT,DMCB,QENDBIN,P+32,2,=C'N'                                 
         GOTO1 HEXOUT,DMCB,COMPMON,P+42,2,=C'N'                                 
         GOTO1 REPORT                                                           
*                                                                               
IVALGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        HHOOK --- HEADHOOK ROUTINE                                             
*                                                                               
HHOOK    NTR1                                                                   
*                                                                               
         USING HHOOK,RF            ESTABLISH ADDRESSABILITY                     
         LA    R1,SAVEREGS-HHOOK                                                
         AR    R1,RF               A(SAVED REGISTERS)                           
         LM    R2,RC,0(R1)                                                      
         DROP  RF                                                               
*                                                                               
         TM    MYFLAG,ISOFF                                                     
         BZ    *+16                                                             
         XC    HEAD6+3(35),HEAD6+3                                              
         MVC   HEAD6+3(17),=C'SUMMARY BY OFFICE'                                
         MVC   HEAD3+1(7),=C'GROUP -'                                           
         MVC   HEAD3+9(10),GRPNAME                                              
         MVC   HEAD3+54(6),=C'PERIOD'                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(9,HEAD3+61)                              
         MVI   HEAD3+68,C'-'                                                    
         GOTO1 DATCON,DMCB,(0,QEND),(9,HEAD3+70)                                
*                                                                               
         LA    R2,HEAD3+99                                                      
         CLI   QOPTION1,C'U'                                                    
         BE    HHOOK10                                                          
         MVC   0(12,R2),=C'NO UPDATING,'                                        
         LA    R2,13(R2)                                                        
         B     HHOOK20                                                          
HHOOK10  EQU   *                                                                
         MVC   0(14,R2),=C'*UPDATE MODE*,'                                      
         LA    R2,15(R2)                                                        
HHOOK20  EQU   *                                                                
         CLI   QOPTION3,C'Y'       BACKOUT REQUEST?                             
         BNE   HHOOK25             NO                                           
         MVC   0(16,R2),=C'REVERSE CLOSEOUT'                                    
         B     HHOOK40                                                          
HHOOK25  EQU   *                                                                
         CLI   QOPTION2,C'D'                                                    
****>>>> BNE   HHOOK30                                                          
*                                                                               
*   TURN OFF 'CLEAR' OPTION:  DEC13/96 - BILL UHR                               
*                                                                               
         MVC   0(10,R2),=C'INV $ HELD'                                          
         B     HHOOK40                                                          
HHOOK30  EQU   *                                                                
         MVC   0(13,R2),=C'*INV CLEARED*'                                       
HHOOK40  EQU   *                                                                
*                                                                               
HHOOKEXT EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        COMMON ROUTINES FOR SAVING CODE SPACE                                  
*                                                                               
         SPACE 3                                                                
TESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*                                                                               
*        LOCAL CONSTANTS AND PATCH AREA                                         
*                                                                               
CALLTOT  DS    CL20                                                             
PATCH    DC    80X'00'                                                          
*                                                                               
*        PROGRAM LITERAL AREA                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        LOCAL WORK AREA                                                        
*                                                                               
         SPACE 2                                                                
SAVEGRP  DS    CL2                                                              
ANYPRINT DC    C'N'                                                             
         SPACE 2                                                                
RELO     DS    A                                                                
SAVEREGS DS    11F                 REGS 2 THRU C FOR HEADHOOK                   
AIOBUFF  DS    A                                                                
AIOAREA  DS    A                   A(I/O AREA)                                  
*                                                                               
CONACCS  DS    12F                 CONTRACT ACCUMULATORS                        
STAACCS  DS    12F                                                              
GRPACCS  DS    12F                                                              
TOTACCS  DS    12F                                                              
STACONS  DS    2F                  PROCESSED/UPDATED                            
GRPCONS  DS    2F                      "    /    "                              
TOTCONS  DS    2F                      "    /    "                              
SCONACCS DS    2F                  SORTREC (1 MONTH + OTHER MONTHS)             
OFFACCS  DS    2F                                                               
OFFCONS  DS    2F                                                               
*                                                                               
QSTRTBIN DS    H                                                                
QENDBIN  DS    H                                                                
COMPMON  DS    H                   COMPRESSED MONDAY DATE                       
*                                                                               
COMMAND  DS    CL8                 IO COMMAND                                   
KEY2     DS    CL32                EXTRA KEY HOLDER                             
CURSTA   DS    CL5                 CURRENT STATION CALLS                        
CONUPED  DS    CL1                 THIS CONTRACT UPDATED FLAG                   
LV1PRINT DS    CL1                 NUM LEVEL 1 GROUPS PRINTED                   
LV2PRINT DS    CL1                 NUM LEVEL 2 GROUPS PRINTED                   
GRPNAME  DS    CL10                CURRENT GROUP NAME                           
SGRPNAME DS    CL10                CURRENT SUB-GROUP NAME                       
PRNTLEVL DS    X                   PRINT LEVEL (1,2,3) IN DOPRINT               
STAREAD  DS    X                   STATION RECORD READ FLAG                     
MYFLAG   DS    X                                                                
*                                                                               
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
*                                                                               
ISOFF    EQU   X'80'               SUMMARY BY OFFICE                            
*                                                                               
SORTREC  DS    0CL76                                                            
         DS    CL9                                                              
SCONFINV DS    F                   FIRST MONTH OF THE REQUEST PERIOD            
SCONOINV DS    F                   ALL OTHER MONTHS                             
*CONNUM  DS    CL4                 CONT #                                       
*CONAGY  DS    CL4                 AGY                                          
*CONAOFF DS    CL2                 AGY OFFICE                                   
*CONADV  DS    CL4                 ADVERTISER                                   
*CONTYPE DS    CL1                                                              
SSTAMKT  DS    CL20                STATION MARKET                               
*GRPNM   DS    CL10                GROUP NAME                                   
*SGRPNM  DS    CL10                SUB GROUP NAME                               
*FLAG    DS    X                                                                
*ECUPD   EQU   X'80'               RECORD UPDATED                               
         DS    CL39                SPARE                                        
*                                                                               
*  KEY BUILDING EQUATES:  DISPLACEMENTS INTO SORTREC                            
*                                                                               
SOFF     EQU   0                   OFFICE                                       
SSTA     EQU   2                   STATION                                      
SGRP     EQU   7                   GROUP                                        
*GNAME   EQU   52                  GROUP NAME                                   
*SNAME   EQU   62                  SUB GROUP NMAE                               
*                                                                               
SORTREC2 DS    CL76                                                             
SVMKT    DS    CL20                SAVE STATION MARKET                          
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,9,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=76'                                    
*                                                                               
ELAREA   DS    CL100               ELEMENT BUILD AREA                           
         EJECT                                                                  
*                                                                               
*        1K IO BUFFER                                                           
*                                                                               
         ENTRY IOBUFF                                                           
IOBUFF   DS    0D                                                               
         DC    1008X'00'          WORK FILE BUFFER                              
         EJECT                                                                  
       ++INCLUDE REMONARCHD                                                     
         EJECT                                                                  
*        PRINT OFF                                                              
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE DDMASTD                                                        
*- SSB DSECT                                                                    
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         EJECT                                                                  
         CSECT                                                                  
*                                                                               
*   INITIAL2:  SET UP SOME DATE VALUES HERE TO GAIN ADDRESSABILITY              
*                                                                               
INITIAL2 NMOD1 0,*INI2*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         GOTO1 DATCON,DMCB,(3,WORK+6),(0,QFASTART)                              
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         GOTO1 DATCON,DMCB,(3,WORK+6),(0,QFAEND)                                
*                                  CONVERT ORIGINAL EBCDIC DATE TO              
*                                     SPECIAL YR 2000 FORMAT                    
*                                                                               
*   TEST                                                                        
*        MVC   P+1(05),=C'FAS/E'                                                
*        MVC   P+10(12),QFASTART                                                
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   BACKOUT:  EXAMINE THE CONTRACT RECORD.  IF INVOICE ELEMENT OF               
*     $0 EXISTS, AND ITS DATE IS AFTER - REPEAT, AFTER - THE REQUEST            
*     END DATE, DELETE THE ELEMENT.                                             
*                                                                               
BACKOUT  NMOD1 0,*BACK*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   CONUPED,0           TURN OFF CONTRACT UPDATE FLAG                
         L     R2,STACONS          INCREMENT CONTRACT COUNT                     
         LA    R2,1(R2)                                                         
         ST    R2,STACONS                                                       
         LA    R2,RCONELEM         FIND X'04' ELEMENT                           
BOUT0010 EQU   *                                                                
         ZIC   R1,1(R2)            GET ELEMENT LENGTH                           
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         CLI   0(R2),0             END OF RECORD?                               
         BE    BOUT0040            YES - FINISHED                               
         CLI   0(R2),X'04'                                                      
         BE    *+12                X'04' ELEMENT                                
         CLI   0(R2),X'54'                                                      
         BNE   BOUT0010            NOT X'54' ELEMENT                            
         USING RCONSTEL,R2                                                      
*                                                                               
*   X'04' ELEMENT FOUND:  CHECK MONTH AGAINST REQUEST END MONTH                 
*                                                                               
         CLC   RCONSTYR(2),QENDBIN BUCKET DATE VS REQUEST END DATE              
         BNH   BOUT0010            BCKT NOT AFTER END DATE: GET NEXT            
*                                                                               
*   BUCKET DATE AFTER REQUEST END DATE:  IS THERE MONEY IN BUCKET?              
*                                                                               
         OC    RCONSTAM,RCONSTAM   MONEY IN BUCKET?                             
         BNZ   BOUT0010            YES - SKIP IT                                
         MVI   RCONSTEL,X'FF'      RESET ELEMENT CODE TO X'FF'                  
*                                     FOR DELETION                              
*                                                                               
*   NO MONEY IN BUCKET/BUCKET AFTER NEW CLOSE DATE:                             
*     BUCKET MUST BE DROPPED.                                                   
*                                                                               
*                                                                               
         CLI   CONUPED,1           FIRST BUCKET BEING DROPPED?                  
         BE    BOUT0020            NO  - HEADER ALREADY DISPLAYED               
*                                                                               
         MVC   P+5(10),=C'CONTRACT= '                                           
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
         MVC   P+25(32),=C'$0 INVOICE MONTHS TO BE DROPPED:'                    
         GOTO1 REPORT                                                           
BOUT0020 EQU   *                                                                
         XC    WORK(3),WORK                                                     
         MVC   WORK(2),RCONSTYR    INSERT DATE FROM BUCKET                      
         GOTO1 DATCON,DMCB,(3,WORK),(5,P+15)                                    
         MVC   P+25(9),=C'EFFECTIVE'                                            
         GOTO1 DATCON,DMCB,(2,RCONSTWK),(5,P+37)                                
         GOTO1 REPORT                                                           
         MVI   CONUPED,1           SET FLAG TO 'UPDATE'                         
         B     BOUT0010            GO BACK FOR NEXT ELEMENT                     
BOUT0040 EQU   *                                                                
         CLI   CONUPED,1           RECORD TO BE UPDATED?                        
         BNE   BOUT0060            NO  - DON'T CALL DELETE                      
         L     RF,STACONS+4        BUMP UPDATED RECORD COUNT                    
         A     RF,=F'1'                                                         
         ST    RF,STACONS+4                                                     
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE '),(X'FF',RCONREC),0,0, X        
               RR=RELO                                                          
         CLI   QOPTION4,C'T'                                                    
         BNE   BOUT0060                                                         
*                                                                               
         MVC   P+1(28),=C'INV ELTS DROPPED FROM RECORD'                         
         GOTO1 REPORT                                                           
BOUT0060 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*        SETSTA --- SET CLOSE DATE IN THE STATION RECORD                        
*                                                                               
*                   CURSTA SET BY CALLER                                        
*                                                                               
SETSTA   NMOD1 0,*SETSTA*                                                       
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLI   QOPTION4,C'T'                                                    
         BE    SSTA0010                                                         
         CLI   QOPTION4,C'H'                                                    
         BNE   SSTA0020                                                         
SSTA0010 EQU   *                                                                
         MVC   P(20),=CL20'STATION INITIAL RTN'                                 
         GOTO1 REPORT                                                           
SSTA0020 EQU   *                                                                
*                                                                               
         L     R4,AIOAREA          SAVE CALLERS A(IO BUFFER)                    
*                                                                               
         CLI   QOPTION1,C'U'       SKIP UPDATE UNLESS IN UPDATE MODE            
         BNE   SSTA0190                                                         
         CLI   QOPTION3,C'C'       SKIP UPDATE IF CONTRACT ONLY                 
         BE    SSTA0190                                                         
*                                                                               
         L     R3,AIOBUFF                                                       
         ST    R3,AIOAREA                                                       
         XC    KEY(27),KEY                                                      
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),QREP                                                   
         MVC   KEY+22(5),CURSTA                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    SSTA0040                                                         
         DC    H'0'                                                             
SSTA0040 EQU   *                                                                
         BAS   RE,GREC                                                          
*                                                                               
         CLI   QOPTION4,C'T'                                                    
         BE    SSTA0050                                                         
         CLI   QOPTION4,C'H'                                                    
         BNE   SSTA0060                                                         
SSTA0050 EQU   *                                                                
         MVC   P(20),=CL20'STA LAST CLOSE DATE'                                 
         LA    RF,(RSTACLDT-RSTAREC)(R3)                                        
         ST    RF,DMCB                                                          
         GOTO1 HEXOUT,DMCB,,P+25,2,=C'N',0                                      
         GOTO1 REPORT                                                           
SSTA0060 EQU   *                                                                
*                                                                               
         CLC   QREGION(4),SPACES   OTHER FILTERS ON OPTION CARD?                
         BNE   SSTA0080            YES - DON'T UPDATE CLOSEOUT DATE             
         CLI   QCONTYPE,C' '       CONTRACT TYPE FILTER APPLIED?                
         BNE   SSTA0080            YES - DON'T UPDATE CLOSEOUT DATE             
         CLC   QDIV(22),SPACES                                                  
         BE    SSTA0100            YES - DON'T UPDATE CLOSEOUT DATE             
SSTA0080 EQU   *                                                                
         MVC   P(34),=C'ADDITIONAL FILTERS PREVENT UPDATE '                     
         MVC   P+34(34),=C'OF CLOSEOUT DATE FOR THIS STATION!'                  
         GOTO1 REPORT                                                           
         B     SSTA0180                                                         
*                                                                               
SSTA0100 EQU   *                                                                
*                                                                               
         CLI   QOPTION3,C'Y'       BACK OUT CLOSE?                              
         BE    SSTA0120            YES - IGNORE TEST OF DATE, AND               
*                                     RESET STATION TO REQUEST                  
*                                        END DATE                               
         CLC   (RSTACLDT-RSTAREC)(2,R3),QENDBIN                                 
         BH    SSTA0180            STATION CLOSED MORE RECENTLY                 
SSTA0120 EQU   *                                                                
         CLC   =C'DISPSTA',QUESTOR                                              
         BNE   SSTA0130                                                         
         MVC   P(26),=C'STATION RECORD: PRE-UPDATE'                             
         GOTO1 REPORT                                                           
         ZICM  RF,(RSTALEN-RSTAREC)(R3),2                                       
                                                                                
         GOTO1 =V(PRNTBL),DMCB,(0,(R3)),(R3),C'DUMP',(RF),             X        
               =C'1D'              *** PUT CONTINUE MARK BACK IN                
SSTA0130 EQU   *                                                                
         MVC   (RSTACLDT-RSTAREC)(2,R3),QENDBIN                                 
         BAS   RE,SET23ELT         SET INDICATOR ELEMENT FOR CLOSE              
         CLC   =C'DISPSTA',QUESTOR                                              
         BNE   SSTA0140                                                         
         MVC   P(27),=C'STATION RECORD: POST-UPDATE'                            
         GOTO1 REPORT                                                           
         ZICM  RF,(RSTALEN-RSTAREC)(R3),2                                       
         GOTO1 =V(PRNTBL),DMCB,(0,(R3)),(R3),C'DUMP',(RF),             X        
               =C'1D'              *** PUT CONTINUE MARK BACK IN                
SSTA0140 EQU   *                                                                
*                                                                               
         CLI   QOPTION4,C'T'                                                    
         BE    SSTA0150                                                         
         CLI   QOPTION4,C'H'                                                    
         BNE   SSTA0160                                                         
SSTA0150 EQU   *                                                                
         MVC   P(20),=CL20'WRIT STA  KEY/DA/REC'                                
         MVC   P+22(27),KEY                                                     
         GOTO1 HEXOUT,DMCB,KEY+28,P+52,4,=C'N',0                                
         MVC   P+70(27),0(R3)      KEY IN RECORD                                
         GOTO1 REPORT                                                           
SSTA0160 EQU   *                                                                
*                                                                               
         L     RE,=V(IOBUFF)                                                    
         L     RF,AIOAREA                                                       
         CR    RE,RF                                                            
         BE    *+6                                                              
         DC    H'0'                POINTING AT WRONG PLACE                      
         CLI   0(RE),X'02'                                                      
         BE    *+6                                                              
         DC    H'0'                RECORD NOT A STATION                         
*                                                                               
*   TEST                                                                        
****     CLI   QOPTION1,C'U'       SKIP UPDATE UNLESS IN UPDATE MODE            
****     BNE   SSTA0169                                                         
*   TEST END                                                                    
*                                                                               
         BAS   RE,PREC                                                          
*                                                                               
SSTA0169 EQU   *                                                                
         CLI   QOPTION4,C'T'                                                    
         BE    SSTA0175                                                         
         CLI   QOPTION4,C'H'                                                    
         BNE   SSTA0180                                                         
SSTA0175 EQU   *                                                                
         MVC   P(10),=CL20'AFTER STA PREC'                                      
         MVC   P+22(27),KEY                                                     
         GOTO1 HEXOUT,DMCB,KEY+28,P+52,4,=C'N',0                                
         MVC   P+70(27),0(R3)      KEY IN RECORD                                
         GOTO1 REPORT                                                           
SSTA0180 EQU   *                                                                
         CLI   QOPTION4,C'T'                                                    
         BE    SSTA0185                                                         
         CLI   QOPTION4,C'H'                                                    
         BNE   SSTA0190                                                         
SSTA0185 EQU   *                                                                
         MVC   P(20),=CL20'NEW LAST CLOSE DATE'                                 
         LA    RF,(RSTACLDT-RSTAREC)(R3)                                        
         ST    RF,DMCB                                                          
         GOTO1 HEXOUT,DMCB,,P+25,2,=C'N',0                                      
         GOTO1 REPORT                                                           
SSTA0190 EQU   *                                                                
         ST    R4,AIOAREA          ALWAYS RESTORE CALLER'S AIOAREA              
*                                                                               
*        SETSTA EXIT                                                            
*                                                                               
SSTA0200 EQU   *                                                                
         SR    R0,R0                                                            
TESTEX2  EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SET23ELT:  UPDATE THE CLOSE ACTIVITY ELEMENT OF THE STATION                 
*        RECORD                                                                 
*                                                                               
SET23ELT NTR1                                                                   
         USING RSTAREC,R3                                                       
         LA    R2,RSTAELEM                                                      
*                                  SET A(DESCRIPTOR ELEMENT)                    
SE230020 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         CLI   0(R2),0             END OF RECORD?                               
         BE    SE230060            YES - USE NEW ELEMENT                        
         CLI   0(R2),X'23'         NO  - OLD 23 ELEMENT?                        
         BNE   SE230020            NO  - GO BACK FOR NEXT                       
         ZIC   RF,1(R2)            YES - GET ELEMENT LENGTH                     
         BCTR  RF,0                -1 FOR EX                                    
         EX    RF,SE230800         MOVE BY LENGTH                               
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE '),(X'23',RSTAREC),0,0, X        
               RR=RELO                                                          
*                                  DELETE ORIGINAL X'23' ELEMENT                
         CLC   =C'DISPSTA',QUESTOR                                              
         BNE   SE230060                                                         
         MVC   P(27),=C'STATION RECORD: POST-DELETE'                            
         GOTO1 REPORT                                                           
         ZICM  RF,RSTALEN,2                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,RSTAREC),RSTAREC,C'DUMP',(RF),       X        
               =C'1D'              *** PUT CONTINUE MARK BACK IN                
SE230060 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,WORK),(3,NEW23ELT+2)                              
         MVI   NEW23ELT+5,X'20'    RESET WHO CLOSED                             
         MVC   NEW23ELT+7(8),=C'RE16 RUN'                                       
*                                  SET 'CLOSED BY'                              
*                                                                               
*- ADD ELEMENT TO STATION RECORD.                                               
*                                                                               
*  ADJUST RECORD LENGTH TO WORK AROUND 'HELLO' 'FEATURE'                        
*  OF ADDING AN EXTRA BYTE OF 0 AT END OF RECORD ON 1ST TIME                    
*  ELEMENT ADD.  ONLY CHANGE RECORD LENGTH BY ELEMENT LENGTH.                   
*                                                                               
         SR    R8,R8                                                            
         ICM   R8,3,RSTALEN        RECORD LENGTH                                
         ZIC   RF,NEW23ELT+1       ELEMENT LENGTH                               
         AR    R8,RF               +ELEM LEN = NEW REC LEN.                     
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE '),RSTAREC,NEW23ELT,0,0,X        
               RR=RELO                                                          
*                                  REPLACE X'23' ELEMENT AFTER UPDATE           
         STCM  R8,3,RSTALEN                                                     
*                                  REPLACE CORRECT RECORD LENGTH                
*                                                                               
         XIT1                                                                   
         DROP  R3                                                               
*                                                                               
SE230800 MVC   NEW23ELT(0),0(R2)   MOVE X'23' ELEMENT BY LENGTH                 
*                                                                               
*                   .0.1.2.3.4.5.6.7.8.9.0.1.2.3.4.5.6.7.8.9                    
NEW23ELT DC    XL20'2314FFFFFF200040404040404040400000000000'                   
*                                                                               
         EJECT                                                                  
*                                                                               
PRNTSTA  NMOD1 0,*PRSTA*                                                        
         MVC   P+0(4),SORTREC+SSTA   STATION-BAND/MARKET ON 1ST LINE            
         LA    R3,P+3                                                           
         CLI   0(R3),0                                                          
         BE    PRST15                                                           
         CLI   0(R3),C' '                                                       
         BNE   PRST20                                                           
PRST15   EQU   *                                                                
         LA    R3,P+2                                                           
PRST20   EQU   *                                                                
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         CLI   SORTREC+4+SSTA,0                                                 
         BE    PRST30                                                           
         CLI   SORTREC+4+SSTA,C' '                                              
         BNE   PRST40                                                           
PRST30   EQU   *                                                                
         MVC   0(2,R3),=C'TV'                                                   
         B     PRST60                                                           
PRST40   EQU   *                                                                
         MVC   0(1,R3),SORTREC+4+SSTA                                           
         CLI   SORTREC+4+SSTA,C'T'                                              
         BNE   PRST50                                                           
         MVI   1(R3),C'V'                                                       
         B     PRST60                                                           
PRST50   EQU   *                                                                
         MVI   1(R3),C'M'                                                       
PRST60   EQU   *                                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),C'/'                                                       
         MVC   1(20,R3),SSTAMKT                                                 
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'154REREP1602 12/16/14'                                      
         END                                                                    
