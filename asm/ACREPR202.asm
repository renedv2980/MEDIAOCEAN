*          DATA SET ACREPR202  AT LEVEL 031 AS OF 12/11/09                      
*PHASE ACR202A,+0                                                               
*INCLUDE ACGETSTD                                                               
*INCLUDE CONVMOS                                                                
*INCLUDE PERCALL                                                                
*INCLUDE PERVERT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE UNDERLIN                                                               
         TITLE 'REALIZATION - EMPLOYEE REPORT'                                  
**********************************************************************          
*        REQUESTED OFF THE 1R LEDGER                                 *          
*        READS 3 DIFFERENT TYPES OF BUDGETS:                         *          
*               1) TARGET BUDGET                                     *          
*               2) STANDARD RATE BUDGET                              *          
*               3) STANDARD HOURS BUDGET  = STDLIST( 36 OFFICE MAX)  *          
*                                                                    *          
*        READS 1R ACCOUNT RECORD - GETS NAME & HIRE & FIRE DATE      *          
*        READS 1R HISTORY (MONACC) CLIENT HOURS  = HRSLIST           *          
*        READS SJ TRANS FOR BILLING VALUE, AND TIME BILLED.          *          
*                                                                    *          
*        START END MONTHS - MMM/YY  MMM/YY                           *          
*                           READ BY BUDGET, BILLING VALUE AND HOURS  *          
*                                                                    *          
*        MOA RANGE          MMM/YY       TIME BILLED FOR MMM/YY ONLY *          
*                           -MMM/YY      TIME BILLED THRU MMM/YY     *          
*                           MMM/YY-MMM/YY BILLED MMM/YY THRU MMM/YY  *          
*                           BLANK        TIME BILLED ETERNITY        *          
*                           READ BY TIME BILLED.                     *          
*                                                                    *          
*        OPTION 1           BLANK - JOB DETAIL BY EMPLOYEE           *          
*                           C     - CLIENT DETAIL BY EMPLOYEE        *          
*                           P     - PRODUCT DETAIL BY EMPLOYEE       *          
*                           E     - LINE LINE PER EMPLOYEE           *          
*                                                                    *          
*        OPTION 2           BLANK - ALL LEVELS OF 1R                 *          
*                           S     - EMPLOYEE'S WITHIN OFFICE         *          
*                                   WITH DEPT AND CAT SUPPRESSED.    *          
*        OPTION 3           FORMAT TYPE                              *          
*                           BLANK - BILLABLE VALUE, TIME BILLED,     *          
*                                   WRITE-OFF, TIME HELD, BILLED VS  *          
*                                   VALUE, BILLED VS STANDARD        *          
*                           1     - BILLABLE VALUE, TIME BILLED,     *          
*                                   BILLED VS STANDARD               *          
*                                                                    *          
*        OPTION 10          OPTION TO SHOW DOLLARS                   *          
*                           N     - DOLLARS ARE SUPPRESSED BECAUSE   *          
*                                   REQUESTOR DOES NOT HAVE PRIV     *          
*                                   TO SEE THE DOLLARS               *          
*                           Y     - REPORT DISPLAYED WITHOUT         *          
*                                   SURPRESSING DOLLARS              *          
*                                                                    *          
*       STANDARD HOURS      ANNUAL HRS INPUT INTO BUDGETS  BY EMPL.  *          
*                                                                    *          
*       STANDARD RATES      ANNUAL RATES INPUT INTO BUDGETS BY EMPL. *          
*                                                                    *          
*       UTIL. ACTUAL PCT    EMPLOYEE HOURS YTD,DIVIDED BY STANDARD   *          
*                           NUMBER OF HOURS YTD.                     *          
*                           EMPLOYEE HOURS AND STANDARD HOURS ARE    *          
*                           KEPT MONTHLY THEN ADDED TOGETHER TO      *          
*                           GET YTD.                                 *          
*                           IF AN EMPLOYEE HAD NO CLIENT HOURS IN A  *          
*                           GIVEN MONTH, THEN THE STANDARD HRS FOR   *          
*                           THAT MONTH ARE NOT USED IN THAT EMPLOYEE *          
*                           CALCULATIONS.                            *          
*                                                                    *          
*                           EMPLOYEE CLIENT HOURS IN OR BEFORE THE   *          
*                           HIRE DATE  AND IN OF AFTER THE TERM DATE *          
*                           ARE ALSO ELIMINATED. STANDARD HRS FOR    *          
*                           THOSE MONTHS ARE ALSO NOT USED.          *          
*                           THE X'56' EL HAS HIRE AND TERM DATES.    *          
*                           TERMINATION DURING MONTH RESULTS IN      *          
*                           PRORATION.                               *          
**********************************************************************          
* PROGRAM DUMPS ARE CAUSED BY THE FOLLOWING:                         *          
*  1) INVALID TARGET, RATE OR STANDARD HRS BUDGET ( A MESSAGE IS     *          
*     PRINTED IN BUDERR TO TELL YOU WHICH BUDGET IS INVALID)         *          
*  2) NO NAME ELEMENT                                                *          
*  3) DON'T GET MATCH FOR HRS IN STD OR ACTUAL HRS TABLE             *          
*  4) INCORRECT FORMAT OPTION(OPTION 3)                              *          
*  5) BAD READ OR WRITE                                              *          
*  6) # OF EMPLOYEES EXCEED EMPLIST LIMIT(5000)                      *          
**********************************************************************          
*  GETBUD   P1 = ACCT ADDRESS     P2 = BUDGET #(PROFILES)            *          
*           P3 = TYPE OF BUDGET (0-TARGET, 1-STD RATES, 2-STD HRS    *          
*           P4 = (0 = VALIDATION,   1 = EMPLOYEE READ)               *          
*  RETURN CONDITION CODE = GOOD (0) OR BAD (<> 0)                    *          
**********************************************************************          
         EJECT                                                                  
ACR202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACR2**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         L     R7,VBIGPRNT                                                      
         USING BIGPRNTD,R7                                                      
         USING ACR2D,RC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LEDGF                                                            
         CLI   MODE,PROCLEVA                                                    
         BE    LEVRD                                                            
         CLI   MODE,PROCLEVB                                                    
         BE    LEVRD                                                            
         CLI   MODE,PROCLEVC                                                    
         BE    LEVRD                                                            
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,PROCHIST                                                    
         BE    PHST                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                                                             
         CLI   MODE,PROCTIME                                                    
         BE    PTIME                                                            
         CLI   MODE,ACCLAST                                                     
         BE    ACCL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* ROUTINE FOR RUN FIRST                                              *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
*                                                                               
         LA    RE,RELOTAB          RELOCATE MY A TYPES                          
         LA    R1,ATYPES                                                        
         MVC   0(ATYPLNQ,R1),0(RE)                                              
*                                                                               
         USING ACCRECD,RE                                                       
         L     RE,AIO                                                           
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         L     RE,AIO              IO AREA                                      
         LA    RF,IOLNQ            IO AREA LENGTH                               
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)            SAVE RC                                      
*                                                                               
         GOTO1 =A(GETBUFF),DMCB,(RC) GETMAIN CALL                               
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
*                                                                               
         GOTO1 =A(CLTNP),DMCB,(RC) CHECK CLIENTS FOR NEW BUS/PRO BONO           
*                                                                               
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTINE FOR REQUEST FIRST                                          *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         LA    RE,ACR2DATA                                                      
         LA    RF,ACR2DLEN                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   PROFILES,PROGPROF   SAVE PROFILES AT COMPANY LEVEL               
         MVI   NUMCOLS,0           CLEAR COLUMNS                                
         BAS   RE,BLDCOLS          BUILD HEADER COLUMNS                         
*                                                                               
         MVI   RCSUBPRG,0                                                       
         L     R4,ADCMPNAM         ** COMPANY NAME FOR HEADLINES**              
         LA    R5,COMPNAM                                                       
         USING NAMELD,R4                                                        
         MVC   0(L'COMPNAM,R5),SPACES                                           
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   0(0,R5),NAMEREC                                                  
         DROP  R4                                                               
*                                                                               
         USING BIND,R5                                                          
         L     R5,AEMPLST          CLEAR CODE/NAME TABLES                       
         XC    BININ,BININ         CLEAR THE TABLE                              
         L     R5,ACLTLST                                                       
         XC    BININ,BININ                                                      
         DROP  R5                                                               
*                                                                               
         XC    ALSORT,ALSORT       CLEAR A(LAST SORT) - INIT FOR SORT           
*                                                                               
         MVC   WORK(4),QSTART      ** PACK PERIOD DATES **                      
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,START)   START=YMD PACKED                
         MVC   WORK(4),QEND                                                     
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,ENDATE)  ENDATE=YMD PACKED               
*                                                                               
* BUILD TABLE OF MONTHLY  DATES AND STANDARD HRS PER MONTH.                     
* PUT PACKED MONTHS INTO EMPL CLI HOUR TBL WHICH IS USED AT PROCACC             
*                                                                               
         LA    R2,STDLIST                 STD HOURS TABLE                       
         LA    R3,TMPLIST                 STD HOURS TABLE                       
         LA    R4,HRSLIST                 MONTHLY TAB EMPL CLI HOURS            
         LA    R5,24                      14 MONTH LIMIT                        
         L     R0,=F'31'                  DAYS FOR ADDAY NEXT MONTH             
         MVC   1(2,R2),START              PACKED YM                             
         MVC   0(2,R3),START                                                    
         MVC   0(2,R4),START                                                    
         MVC   WORK(4),QSTART             EBCDIC START FOR WK DAYS CALC         
         MVC   3(4,R2),QSTART                                                   
         B     REQF20                                                           
*                                                                               
REQF10   MVC   WORK+4(2),=C'01'                                                 
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         MVC   WORK(6),WORK+6                                                   
         MVC   3(4,R2),WORK                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)    YMD PACKED                    
         MVC   0(2,R4),WORK+6              YM INTO CLI HOUR TABLE               
         MVC   0(2,R3),WORK+6              YM INTO TMPLIST                      
         MVC   1(2,R2),WORK+6              YM INTO STD HRS TABLE                
*                                                                               
REQF20   MVC   WORK+4(2),=C'01'           STANDARD HRS & RATE TABLE             
         ZAP   7(4,R2),=P'0'                                                    
         ZAP   2(6,R4),=P'0'                                                    
         ZAP   2(4,R3),=P'0'                                                    
         ZAP   6(6,R3),=P'0'                                                    
         MVI   0(R2),X'40'                                                      
*                                                                               
         LA    R4,8(R4)            FOR CLI HOUR TAB TOO                         
         LA    R2,11(R2)                                                        
         LA    R3,12(R3)                                                        
         MVI   0(R2),X'FF'                                                      
         MVI   0(R3),X'FF'                                                      
         MVI   0(R4),X'FF'                                                      
         CLC   WORK(4),QEND        EQUAL TO END MONTH                           
         BE    *+10                YES - TAB COMPLETE                           
         BCT   R5,REQF10                                                        
         DC    H'0'                                                             
*                                                                               
         MVC   BDGSTR(1),START     TAKE YEAR FROM START DATE                    
         MVI   BDGSTR+1,X'01'      START JANUARY                                
         MVC   BDGEND(1),START                                                  
         MVI   BDGEND+1,X'12'      END DECEMBER                                 
         XC    OFFSWIT(4),OFFSWIT  1ST TIME COMP/U/L/OFF =0                     
*                                                                               
* SET UP MOA RANGE DATES                                                        
*                                                                               
         L     R5,AMONACC          CLEAR MOA RANGE DATES SO                     
         USING ACMD,R5             MONACC WILL PRINT DATES IN                   
         MVC   M1START,ACMMSTR     DETAILS OF REQUEST BUT WILL                  
         MVC   M1END,ACMMEND       NOT HAVE DATES TO FILTER                     
         XC    ACMMSTR,ACMMSTR     OUT INFORMATION FROM 1C                      
         MVC   ACMMEND,=2X'FF'                                                  
*                                                                               
         MVC   BILLTHRU,SPACES     HEADLINE WORK AREA                           
         CLI   M1END,X'FF'                                                      
         BE    REQF50                                                           
*                                                                               
         MVC   BILLTHRU(11),=C'TIME BILLED'                                     
         CLI   QOPT3,C' '                                                       
         BNE   *+10                                                             
         MVC   BILLTHRU+11(4),=C'-W/O'                                          
         CLC   ACMCMSTR,SPACES                                                  
         BNE   REQF30                                                           
         MVC   BILLTHRU+16(4),=C'THRU'                                          
         MVC   BILLTHRU+21(6),ACMCMEND                                          
         B     REQF50                                                           
REQF30   MVC   BILLTHRU+16(3),=C'FOR'                                           
         MVC   BILLTHRU+20(6),ACMCMSTR                                          
         CLC   M1START,M1END                                                    
         BNE   REQF40                                                           
         MVC   BILLTHRU+27(4),=C'ONLY'                                          
         B     REQF50                                                           
REQF40   MVC   BILLTHRU+27(4),=C'THRU'                                          
         MVC   BILLTHRU+32(6),ACMCMEND                                          
*                                                                               
REQF50   GOTO1 ADSQUASH,DMCB,BILLTHRU,38                                        
         ZAP   TARGET,=P'0'                                                     
*                                                                               
         XC    BUDSTAT,BUDSTAT                                                  
         CLI   PROFILES,0          NO TRG BUD DEFINED                           
         BNE   *+8                                                              
         OI    BUDSTAT,NOPROF                                                   
*                                                                               
         LA    R2,BUTRGPCT                                                      
         MVC   1(1,R2),PROFILES                                                 
         GOTO1 =A(SETBUD),DMCB,(RC)                                             
*                                                                               
         LA    R2,BUSTDRAT                                                      
         MVC   1(1,R2),PROFILES+1                                               
         GOTO1 =A(SETBUD),DMCB,(RC)                                             
*                                                                               
         LA    R2,BUSTDHRS                                                      
         MVC   1(1,R2),PROFILES+2                                               
         GOTO1 =A(SETBUD),DMCB,(RC)                                             
*                                                                               
* NOTE: CONTINUE ON WITH AN INVALID BUDGET ADDED AS PER LDES 3/90               
*                                                                               
         XC    PERSTAT,PERSTAT     ASSUME NO PERSON RECORDS                     
         L     R1,APERBLK                                                       
         USING PERD,R1                                                          
         MVC   PERDMGR,DATAMGR                                                  
         MVC   PERCOMP,RCCOMPFL                                                 
         OI    PERFLAGS,PERRECCK                                                
         GOTO1 =V(PERCALL)         GET PERSON BLOCK                             
         TM    PERERR,PERNOREC     USING TMS                                    
         BO    *+8                                                              
         OI    PERSTAT,PERCOST     USING PERSON RECORDS                         
*                                                                               
REQFX    B     EXIT                        EXIT                                 
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTINES TO READ BUDGETS AT THE LEVEL THE USER DEFINED THEM        *          
*          AT LEDGER FIRST - GET LEVELS                              *          
**********************************************************************          
         SPACE 1                                                                
LEDGF    DS    0H                                                               
         BAS   RE,GETLEVS          GET LEVEL STRUCTURE                          
*                                                                               
LEVRD    XC    HIREDATE,HIREDATE            SET HIRE TO '000000'                
         MVC   FIREDATE,=X'FFFFFF'          TERM DATE TO ETERNITY               
         BAS   RE,READBUD                                                       
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ROUTINE FOR PROCESSING AND ACCOUNT                                 *          
*         BUILD SORT RECORD KEY  FROM U/L 1R                         *          
*         LOOKUP NAME AND TARGET UTIL BUDGETS                        *          
**********************************************************************          
         SPACE 1                                                                
PACC     DS    0H                                                               
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCRDTIME,C'N'                                                    
         MVI   FCRDHIST,C'Y'                                                    
         CLI   PROFILES+3,C'Y'     INCLUDE NB/PB TIME                           
         BE    PACC10              YES                                          
         MVI   FCRDTRNS,C'Y'       YES, READ TRANSACTIONS TO GET TIME           
         MVI   FCRDTIME,C'Y'       SO I CAN FILTER BY CLIENT                    
         MVI   FCRDHIST,C'N'                                                    
*                                                                               
PACC10   XC    HIREDATE,HIREDATE   SET HIRE TO '000000'                         
         MVC   FIREDATE,=X'FFFFFF' TERM DATE TO ETERNITY                        
*                                                                               
         TM    PERSTAT,PERCOST     IS THIS PERSON ON NEW COST                   
         BZ    PACC15                                                           
*                                                                               
         USING PERD,R1                                                          
         L     R1,APERBLK                                                       
         MVC   PERALEDG,ADLEDGER                                                
         MVC   PERADACC,ADACC                                                   
         GOTO1 =V(PERCALL)         GET PERSON BLOCK                             
         MVC   HIREDATE,PERHIR            HIRE DATE                             
         OC    PERTRM,PERTRM            IS THERE A TERM DATE                    
         BZ    PACC20                                                           
         MVC   FIREDATE,PERTRM              TERM DATE                           
         B     PACC20                                                           
         DROP  R1                                                               
*                                                                               
PACC15   L     R4,ADACC                                                         
         MVI   ELCODE,EMPELQ       X'56' - EMPLOYEE HISTORY ELEMENT             
         BAS   RE,GETEL                                                         
         BNE   PACC20              NO X'56' ELEMENT                             
         USING EMPELD,R4                                                        
         MVC   HIREDATE,EMPHIR     HIRE DATE                                    
         OC    EMPTRM,EMPTRM       IS THERE A TERM DATE                         
         BZ    PACC20                                                           
         MVC   FIREDATE,EMPTRM     TERM DATE                                    
*                                                                               
PACC20   BAS   RE,READBUD          READ ANY BUDGETS DEFINED AT THIS LEV         
*                                                                               
         L     R4,ADACC                                                         
         LA    R2,HRSLIST          CLEAR CLI HRS TABLE                          
         LA    R0,24               12 MONTH MAX                                 
         ZAP   2(6,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         USING SRTD,R6                                                          
         LA    R6,SRTWRK                                                        
         BAS   RE,CLRSRT           CLEAR SORT WORK AREA                         
*                                                                               
* BUILD THE 1R SORT KEY                                                         
*                                                                               
         USING ACTRECD,R4                                                       
         L     R4,ADACC            ADDR OF ACCOUNT                              
         SR    R1,R1               FIRST LEVEL                                  
         IC    R1,LEVLNQA          LENGTH OF 1ST LEVEL                          
         BCTR  R1,0                DECREMENT FOR EXMVC                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACTKACT(0),OVRHED   ELIMINATE ALL OVERHEAD - 999999'S            
         BE    PACCX               ACCOUNTS                                     
*                                                                               
         LA    R3,ACTKACT          SECOND LEVEL                                 
         SR    R0,R0                                                            
         IC    R0,LEVA             DISPLACEMENT PAST FIRST LEVEL                
         SR    R1,R1                                                            
         IC    R1,LEVLNQB          LENGTH OF 2ND LEVEL                          
         AR    R3,R0               ADD DISPLACEMENT TO START OF ACCOUNT         
         BCTR  R1,0                DECREMENT FOR EXMVC                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),OVRHED      ELIMINATE ALL OVERHEAD - 999999'S            
         BE    PACCX               ACCOUNTS                                     
*                                                                               
* FIND LAST LEVEL                                                               
*                                                                               
         LA    R2,LEVD             START AT LOWEST LEVEL                        
         LA    R3,LEVLNQD          INDIVIDUAL LENGTH OF LOWEST                  
         LA    R0,LEVELQ           ASSUME IT IS A 4 LEVEL STRUCTURE             
         SR    R1,R1                                                            
         IC    R1,LEVNUM           ACTUAL LEVEL NUMBER                          
         SR    R0,R1                                                            
         BZ    PACC30              IF ZERO - 4 LEVEL STRUCTURE                  
*                                                                               
         SH    R2,=Y(L'LEVELS)     BUMP UP ONE LEVEL OF HEIRARCHY               
         SH    R3,=Y(L'LEVLNQS)    BUMP UP TO PREV INDIVIDUAL LENGTH            
         BCT   R0,*-8                                                           
*                                                                               
PACC30   SR    R0,R0                                                            
         IC    R0,0(R2)            R0 = COMBINED LENGTHS OF LEVELS              
         SR    R1,R1                                                            
         IC    R1,0(R3)            R1 = INDIVIDUAL LENGTH OF LEVEL              
         SR    R0,R1               GET DISPLACEMENT                             
         LA    R3,ACTKACT                                                       
         AR    R3,R0               BUMP IN KEY                                  
         CLC   0(3,R3),OVRHED      ELIMINATE ALL OVERHEAD - 999999'S            
         BE    PACCX               ACCOUNTS                                     
*                                                                               
         MVC   SRTEMPL,ACTKACT     1R OFF/DPT/CAT/STAFF                         
         USING EMP1RD,R5           1R CODE AND NAME INTO TAB                    
         LA    R5,WORK                                                          
         MVC   WORK,SPACES                                                      
         MVC   EMP1RKEY,ACTKEY     SAVE KEY                                     
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,NAMELQ       X'20' - NAME ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                 NO NAME ELEMENT                              
         DC    H'0'                                                             
*                                                                               
         USING NAMELD,R4                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   EMP1RNME(0),NAMEREC         1R NAME INTO TABLE                   
         MVC   EMP1RHIR,HIREDATE                                                
         MVC   EMP1RFIR,FIREDATE                                                
         GOTO1 =A(BINADD),DMCB,(RC),(R5),AEMPLST                                
*                                                                               
         MVC   SRTENAME,EMP1RNME            1R NAME INTO SORTREC                
         MVC   SRTHIRE,HIREDATE             HIRE/FIRE DATES TO SORT REC         
         MVC   SRTFIRE,FIREDATE                                                 
         MVC   SRTSTAT,BUDSTAT                                                  
PACCX    B     EXIT                                                             
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
**********************************************************************          
* ROUTINE FOR PROCESSING HISTORY                                     *          
*         GET THE 1C HOURS WORKED FROM U/L 1C                        *          
**********************************************************************          
         SPACE 1                                                                
PHST     DS    0H                                                               
         CLI   PROFILES+3,C'Y'     INCLUDE  NB/PB TIME                          
         BNE   EXIT                NO, READ TRANSACTIONS                        
*                                                                               
         USING CACELD,R4                                                        
         L     R4,ADSUBAC          ADDR OF SUB ACCT                             
         USING BUKELD,R5                                                        
         L     R5,ADTRANS                                                       
         CLI   BUKEL,BUKELQ        X'45' - BUCKET ELEMENT                       
         BNE   EXIT                                                             
         CLC   CACCNTU(2),=C'1C'   MUST BE 1C CLIENT TIME ONLY                  
         BNE   EXIT                                                             
         CLI   BUCKTYPE,C'H'       HOURS ONLY                                   
         BNE   EXIT                                                             
*                                  WE DON'T WANT IT IF:                         
         CLC   BUKYEAR(2),START    LOWER THAN START YYMM                        
         BL    EXIT                                                             
         CLC   BUKYEAR(2),ENDATE   HIGHER THAN END YYMM                         
         BH    EXIT                                                             
*                                                                               
         LA    R1,HRSLIST          EMPL CLIENT HRS TABLE PER MONTH              
         LA    R0,24                                                            
PHST10   CLC   0(2,R1),BUKYEAR     MATCH THE YYMM                               
         BE    PHST20                                                           
         LA    R1,8(R1)            NEXT MONTH IN TABLE                          
         BCT   R0,PHST10                                                        
         DC    H'0'                IT HAS TO FIT SOMEWHERE                      
PHST20   AP    2(6,R1),BUKCR       HOURS INTO TABLE                             
         B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
**********************************************************************          
* ROUTINE FOR PROCESSING A TRANSACTION                               *          
*         BUILD HRSLIST FROM TRANSACTION DATA                        *          
**********************************************************************          
         SPACE 1                                                                
PTRN     DS    0H                                                               
         CLI   PROFILES+3,C'Y'     INCLUSE NB/PN                                
         BE    PTRNX               YES, USE HRS LIST BUILD WITH BUCKETS         
*                                                                               
         L     R4,ADTRANS                                                       
         USING TRNRECD,R5                                                       
         LR    R5,R4                                                            
         SH    R5,DATADISP                                                      
         CLC   TRNKCUNT(2),=C'1C'  MUST BE 1C CLIENT TIME                       
         BNE   PTRNX                                                            
*                                  WE DON'T WANT IT IF:                         
         L     R2,AMONACC                                                       
         USING ACMD,R2                                                          
         CLC   ACMMDTE,START       LOWER THAN START YYMM                        
         BL    PTRNX                                                            
         CLC   ACMMDTE,ENDATE      HIGHER THAN END YYMM                         
         BH    PTRNX                                                            
*                                                                               
         USING PCIELD,R4                                                        
         MVI   ELCODE,PCIELQ       X'51' - PROJECT CONTROL INFO ELEMENT         
         BAS   RE,NEXTEL                                                        
         BNE   PTRN10              NO 51 EL IS OK                               
         MVC   WORK(CLILNQ),SPACES                                              
         MVC   WORK(CLILNQ),PCICLI+3                                            
         BAS   RE,CHKCLI           CHECK CLIENT                                 
         TM    CLISTAT,CLINBPB     IS CLIENT NEW BUSINES/PRO BONO?              
         BO    PTRNX               CLIENT FOUND, REJECT                         
*                                                                               
PTRN10   L     R4,ADTRANS                                                       
         MVI   ELCODE,PRTELQ       X'40' - PERSONNEL RATE ELEMENT               
         BAS   RE,NEXTEL                                                        
         BNE   PTRN20              TRY FOR N TIME                               
*                                                                               
         USING PRTELD,R4                                                        
         ZAP   DUB,PRTHOUR                                                      
         B     PTRN40                                                           
*                                                                               
PTRN20   L     R4,ADTRANS          IS THIS N TIME?                              
         MVI   ELCODE,SCIELQ       X'50' - SUBSIDIARY CASH INFO ELEMENT         
PTRN30   BAS   RE,NEXTEL                                                        
         BNE   PTRNX               NOT N TIME                                   
*                                                                               
         USING SCIELD,R4                                                        
         CLI   SCITYPE,SCITHOUR    C'H' - HOURS HERE                            
         BNE   PTRN30              GET NEXT 50 EL                               
         ZAP   DUB,SCIAMNT                                                      
*                                                                               
PTRN40   LA    R1,HRSLIST          EMPL CLIENT HRS TABLE PER MONTH              
         LA    R0,24                                                            
PTRN50   CLC   0(2,R1),ACMMDTE     MATCH THE YYMM OF THE TRANS                  
         BE    PTRN60                                                           
         LA    R1,8(R1)            NEXT MONTH IN TABLE                          
         BCT   R0,PTRN50                                                        
         DC    H'0'                IT HAS TO FIT SOMEWHERE                      
PTRN60   AP    2(6,R1),DUB         HOURS INTO TABLE                             
PTRNX    B     EXIT                                                             
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
* PROCESS TIME RECORDS (TMS)                                         *          
**********************************************************************          
         SPACE 1                                                                
PTIME    DS    0H                                                               
         CLI   PROFILES+3,C'Y'     INCLUDE NB/PN                                
         BE    PTIMEX              YES, USE BUCKET HOURS                        
*                                                                               
         USING TIMELD,R5                                                        
         L     R5,ADTRANS                                                       
         CLI   TIMEL,TIMELQ                                                     
         BNE   PTIMEX                                                           
         CLI   TIMETYP,TIMEINP     IS THIS INPUT DETAIL                         
         BNE   PTIMEX                                                           
         CLI   TIMLN,TIMILN1Q      AT LEAST INPUT LENGTH                        
         BL    PTIMEX                                                           
*                                                                               
         MVC   WORK(CLILNQ),SPACES                                              
         MVC   WORK(CLILNQ),TIMACC+2                                            
         BAS   RE,CHKCLI           CHECK CLIENT                                 
         TM    CLISTAT,CLINBPB     IS CLIENT NEW BUSINES/PRO BONO?              
         BO    PTIMEX              CLIENT FOUND, REJECT                         
*                                                                               
         CLI   TIMTTYP,TIMTCB      CLIENT BILLABLE TIME                         
         BE    PTIME10                                                          
         CLI   TIMTTYP,TIMTCR      CLIENT REALIZATION                           
         BE    PTIME10                                                          
         CLI   TIMTTYP,TIMTCN      CLIENT NON BILLABLE                          
         BE    PTIME10                                                          
         CLI   TIMTTYP,TIMTNC      NON CLIENT                                   
         BE    PTIMEX              NOT ON THIS REPORT                           
*                                                                               
PTIME10  LA    R1,HRSLIST          EMPL CLIENT HRS TABLE PER MONTH              
         LA    R0,24                                                            
PTIME20  CLC   0(2,R1),TIMMOA      MATCH THE YYMM OF THE TRANS                  
         BE    PTIME30                                                          
         LA    R1,8(R1)             NEXT MONTH IN TABLE                         
         BCT   R0,PTIME20                                                       
         B     PTIMEX              MONACCS DATE FILTERING IS SHAKEY             
*                                                                               
PTIME30  AP    2(6,R1),TIMHRS      HOURS INTO TABLE                             
*                                                                               
PTIMEX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ROUTINE FOR ACCOUNT LAST                                           *          
*         ADD THE RECORD TO SORTER                                   *          
**********************************************************************          
         SPACE 1                                                                
ACCL     DS    0H                                                               
         USING ACTRECD,R4                                                       
         L     R4,ADACC            ADDR OF ACCOUNT                              
         SR    R1,R1               FIRST LEVEL                                  
         IC    R1,LEVLNQA          LENGTH OF 1ST LEVEL                          
         BCTR  R1,0                DECREMENT FOR EXMVC                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACTKACT(0),OVRHED   ELIMINATE ALL OVERHEAD - 999999'S            
         BE    ACCLX               ACCOUNTS                                     
*                                                                               
         LA    R3,ACTKACT          SECOND LEVEL                                 
         SR    R0,R0                                                            
         IC    R0,LEVA             DISPLACEMENT PAST FIRST LEVEL                
         SR    R1,R1                                                            
         IC    R1,LEVLNQB          LENGTH OF 2ND LEVEL                          
         AR    R3,R0               ADD DISPLACEMENT TO START OF ACCOUNT         
         BCTR  R1,0                DECREMENT FOR EXMVC                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),OVRHED      ELIMINATE ALL OVERHEAD - 999999'S            
         BE    ACCLX               ACCOUNTS                                     
*                                                                               
* FIND LAST LEVEL                                                               
*                                                                               
         LA    R2,LEVD             START AT LOWEST LEVEL                        
         LA    R3,LEVLNQD          INDIVIDUAL LENGTH OF LOWEST                  
         LA    R0,LEVELQ           ASSUME IT IS A 4 LEVEL STRUCTURE             
         SR    R1,R1                                                            
         IC    R1,LEVNUM           ACTUAL LEVEL NUMBER                          
         SR    R0,R1                                                            
         BZ    ACCL10              IF ZERO - 4 LEVEL STRUCTURE                  
*                                                                               
         SH    R2,=Y(L'LEVELS)     BUMP UP ONE LEVEL OF HEIRARCHY               
         SH    R3,=Y(L'LEVLNQS)    BUMP UP TO PREV INDIVIDUAL LENGTH            
         BCT   R0,*-8                                                           
*                                                                               
ACCL10   SR    R0,R0                                                            
         IC    R0,0(R2)            R0 = COMBINED LENGTHS OF LEVELS              
         SR    R1,R1                                                            
         IC    R1,0(R3)            R1 = INDIVIDUAL LENGTH OF LEVEL              
         SR    R0,R1               GET DISPLACEMENT                             
         LA    R3,ACTKACT                                                       
         AR    R3,R0               BUMP IN KEY                                  
         CLC   0(3,R3),OVRHED      ELIMINATE ALL OVERHEAD - 999999'S            
         BE    ACCLX               ACCOUNTS                                     
*                                                                               
         LA    R4,TMPLIST          LIST FOR STD VALUE CAL                       
         LA    R0,24               SIZE OF TMPLIST                              
*                                                                               
ACCL20   CLC   0(2,R4),HIREDATE    IS IT BEFORE                                 
         BL    *+14                YES - EXCLUDE                                
         CLC   0(2,R4),FIREDATE    FIRED BEFORE?                                
         BNH   *+10                NO, KEEP                                     
         ZAP   2(4,R4),=P'0'       NO STANDARD HOURS                            
*                                                                               
         LA    R4,12(R4)           INCREMENT STD LIST                           
         BCT   R0,ACCL20                                                        
         MVI   0(R4),X'FF'         END OF TABLE                                 
*                                                                               
         LA    R4,TMPLIST          PT TO BEG OF TABLE                           
         USING SRTD,R6                                                          
         LA    R6,SRTWRK                                                        
         LA    R1,HRSLIST          EMPL CLI HOURS BY MONTH                      
         LA    R0,24               24 MONTH MAX                                 
         ZAP   SRTSTRAT,=P'0'      CALCULATE STANDARD VALUE                     
         ZAP   SRTSTAND,=P'0'      AND STD HOURS                                
*                                                                               
ACCL30   CLI   0(R1),X'FF'         END OF TABLE                                 
         BE    ACCL120             YES - PUT TO SORT                            
         CP    2(6,R1),=P'0'       ZERO CLIENT HRS FOR MONTH                    
         BE    *+10                YES - DONT ADD IN STAND                      
         AP    SRTHRACT,2(6,R1)    ADD HRS TO SORT REC                          
*                                                                               
         CLC   0(2,R4),HIREDATE    IS HIRE DATE IN THIS MONTH?                  
         BNE   ACCL60              NO - CHECK FIREDATE                          
*                                                                               
         CLI   HIREDATE+2,X'01'    HIRED ON FIRST DAY?                          
         BE    ACCL60              YES - CHECK FIREDATE                         
*                                                                               
         L     R5,=A(MMDAYS)       DAYS IN MONTH TABLE                          
         ZAP   DAYSINMM,=P'0'                                                   
ACCL40   CLC   0(1,R5),HIREDATE+1  DO MONTHS MATCH?                             
         BE    *+12                YES                                          
         LA    R5,3(R5)            INCREMENT TABLE                              
         B     ACCL40                                                           
         ZAP   DAYSINMM,1(2,R5)    LOAD IN DAYS IN MONTH                        
*                                                                               
         CLC   HIREDATE(2),FIREDATE    DOES HIREDATE = FIREDATE?                
         BNE   ACCL50                  NO                                       
*                                                                               
         ZAP   DUB,=P'0'                                                        
         ZAP   BUDGAMT,=P'0'                                                    
         MVO   DUB,FIREDATE+2(1)       CONVERT TO PACK                          
         MVO   BUDGAMT,HIREDATE+2(1)   CONVERT TO PACK                          
         B     ACCL90                  FIREDATE - HIREDATE                      
*                                                                               
ACCL50   ZAP   DUB,DAYSINMM            MATCH ON HIRE DATE ONLY                  
         ZAP   BUDGAMT,=P'0'                                                    
         MVO   BUDGAMT,HIREDATE+2(1)   CONVERT TO PACK                          
         B     ACCL90                                                           
*                                                                               
ACCL60   CLC   0(2,R4),FIREDATE    MATCH ON FIRE DATE?                          
         BNE   ACCL100             NO                                           
*                                                                               
         L     R5,=A(MMDAYS)       DAYS IN MONTH TABLE                          
         ZAP   DAYSINMM,=P'0'                                                   
ACCL70   CLC   0(1,R5),FIREDATE+1  DO MONTHS MATCH?                             
         BE    *+12                YES                                          
         LA    R5,3(R5)            INCREMENT TABLE                              
         B     ACCL70                                                           
         ZAP   DAYSINMM,1(2,R5)    LOAD IN DAYS IN MONTH                        
*                                                                               
         CLC   HIREDATE(2),FIREDATE   DOES HIREDATE = FIREDATE?                 
         BNE   ACCL80                 NO                                        
*                                                                               
         ZAP   DUB,=P'0'                                                        
         ZAP   BUDGAMT,=P'0'                                                    
         MVO   DUB,FIREDATE+2(1)      CONVERT TO PACK                           
         MVO   BUDGAMT,HIREDATE+2(1)  CONVERT TO PACK                           
         B     ACCL90                                                           
*                                                                               
ACCL80   ZAP   DUB,DAYSINMM        MATCH ON FIRE DATE ONLY                      
         ZAP   BUDGAMT,=P'0'                                                    
         MVO   DUB,FIREDATE+2(1)   DATE FIRED                                   
*                                                                               
ACCL90   ZAP   RATEXHR,=P'0'                                                    
         SP    DUB,BUDGAMT         DAYS-IN-MONTH -                              
         MP    DUB,=P'100000'                                                   
         DP    DUB,DAYSINMM        /DAYS IN MONTH                               
         ZAP   RATEXHR,2(4,R4)                                                  
         MP    RATEXHR,DUB+2(4)    STD HRS X PRORATION                          
         SRP   RATEXHR,64-5,5      ROUND TO 2 DECIMALS                          
         ZAP   2(4,R4),RATEXHR+4(4)    MOVE BACK TO TABLE                       
*                                                                               
* CALCULATE STD VALUE                                                           
* STD HOURS X RATE X TRG PCT                                                    
*                                                                               
ACCL100  ZAP   RATEXHR,6(6,R4)     STD RATE   FOR MONTH                         
         MP    RATEXHR,2(4,R4)     STANDARD RATE X STD HRS                      
         SRP   RATEXHR,64-2,5      ROUND TO 2 DECIMAL PLACES                    
*                                                                               
         CP    TARGET,=P'0'                                                     
         BE    ACCL110                                                          
*                                                                               
         ZAP   DUB,TARGET          MULTIPLY BY TARGET                           
         DP    DUB,=P'100'                                                      
         MP    RATEXHR,DUB+4(2)                                                 
         SRP   RATEXHR,64-2,5      ROUND TO 2 DECIMAL PLACES                    
*                                                                               
ACCL110  ZAP   6(6,R4),RATEXHR     UPDATE TABLE                                 
         AP    SRTSTAND,2(4,R4)    ADD STAND HRS TO SORT REC                    
         AP    SRTSTRAT,6(6,R4)    ADD STAND VAL TO SORT REC                    
         LA    R1,8(R1)            BUMP CLI HRS TABLE                           
         LA    R4,12(R4)           INCREMENT PRORATE TABLE                      
         BCT   R0,ACCL30                                                        
*                                                                               
         USING BUDD,R2                                                          
ACCL120  LA    R2,BUTRGPCT                                                      
         ZAP   SRTTARGT,BUDAMNT    GET BUD AMOUNT                               
         BAS   RE,PUTSRT           PUT TO SORT                                  
ACCLX    B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
**********************************************************************          
* ROUTINE FOR REQUEST LAST                                           *          
*         LOOKUP SJ ACCOUNTS CLI/PRD/JOB - COSTING ACCT FROM X'24' EL*          
*         READ TRANS FOR BILLING VALUE AND TIME BILLED               *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         OC    ALSORT,ALSORT       ANY EMPLOYEES IN SORT                        
         BZ    EXIT                NO, I'M DONE                                 
*                                                                               
         USING SRTD,R6                                                          
         LA    R6,SRTWRK                                                        
         BAS   RE,CLRSRT                                                        
         USING TRNRECD,R3                                                       
         LA    R3,SVKEY                                                         
         MVC   TRNKCULA(ACCORFST),SPACES   CLEAR KEY                            
         MVC   TRNKCPY,RCCOMPFL            MOVE IN COMPANY                      
         MVC   TRNKUNT(2),=C'SJ'           U/L SJ                               
         MVI   TRNKACT,X'41'                                                    
REQL10   GOTO1 =A(DMHIGHDR),DMCB,(RC)      READ HIGH                            
         B     REQL30                                                           
*                                                                               
REQL20   GOTO1 =A(DMSEQDR),DMCB,(RC)       READ SEQ                             
REQL30   CLC   IOKEY(TRNKACT-TRNKEY),SVKEY                                      
         BE    REQL40                      YES CONTINUE                         
         CLC   SRTWRK(SRTDLNQ),SPACES                                           
         BE    *+8                                                              
         BAS   RE,PUTSRT                   PUT LAST ONE TO SORT                 
         B     REPORT                                                           
*                                                                               
REQL40   GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         L     R3,AIO                                                           
*                                                                               
         CLI   PROFILES+3,C'Y'     INCLUED NEW BUISNESS/PB CLIENT               
         BE    REQL50              YES                                          
         CLC   TRNKACT(L'CLICLI),SVKEY+3       SAME ACCOUNT                     
         BE    REQL50                          YES                              
         MVC   SVKEY,IOKEY         UPDATE SVKEY                                 
         MVC   WORK(CLILNQ),SPACES                                              
         MVC   WORK(CLILNQ),TRNKACT                                             
         BAS   RE,CHKCLI           CHECK IF ACCOUNT IS NBPB                     
         TM    CLISTAT,CLINBPB                                                  
         BZ    REQL50              NOPE, NOT IN LIST                            
*                                                                               
         LA    R3,SVKEY            RESET R3 TO BUMP SVKEY AND READHI            
         MVI   TRNKACT+6,X'FF'                                                  
         B     REQL10              GET NEXT PRODUCT                             
*                                                                               
REQL50   CLC   TRNKCUNT(2),=C'1R'  IS CONTRA UL 1R                              
         BNE   REQL20              NO - READ NEXT                               
*                                                                               
         L     R5,AEMPLST                                                       
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ    MOVE IN PARMS 3,4,5,6                        
         LA    R0,BINTAB                                                        
         GOTO1 BINSRCH,DMCB,TRNKCULC,(R0)                                       
         CLI   DMCB,0                                                           
         BNE   REQL20              NOT FOUND                                    
*                                                                               
         L     R5,DMCB                                                          
         USING EMP1RD,R5                                                        
*                                                                               
         MVC   SRTEMPL,TRNKCACT    1R EMPL INTO SORTREC                         
         MVC   SRTSJ,TRNKACT       SJ CLI/PRD/JOB INTO SORT                     
         MVC   SRTENAME,EMP1RNME   1R EMP NAME INTO SRTREC                      
         MVC   SRTHIRE,EMP1RHIR                                                 
         MVC   SRTFIRE,EMP1RFIR                                                 
         MVC   SRTSTAT,BUDSTAT                                                  
*                                                                               
         USING TRNELD,R4                                                        
         L     R4,AIO                                                           
         MVI   ELCODE,TRNELQ       X'44' - TRANSACTION ELEMENTS                 
         BAS   RE,GETEL2                                                        
         BNE   REQL120                                                          
*                                                                               
* CALL PRORATA TO CONVERT TO X'77' - ELM RETURNED IN ACMAPRO2                   
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         GOTO1 ACMAPRAT,DMCB,(X'40',AIO),ADGOBLOC,ADCOMFAC,0,          X        
               ACMAPROB,ACMAPRO2                                                
         DROP  R2                                                               
*                                                                               
         GOTO1 CONVMOS,DMCB,(R4),MOS          MOS FROM TRANSACTION              
         CLI   TRNTYPE,57                     IS THIS A WRITE-OFF?              
         BE    REQL80                         YES                               
         CLI   TRNTYPE,58                                                       
         BE    REQL80                                                           
*                                                                               
* FILTER TYPE 49'S ON QSTART/QEND V.S. MOS                                      
*                                                                               
         MVI   NOTIMESH,C'Y'       ASSUME REJECTION                             
         CLC   MOS,START           IS MOS LOWER THAN START                      
         BL    REQL120             YES - GET NEXT                               
         CLC   MOS,ENDATE          OR HIGHER THAN END DATE                      
         BH    REQL120             YES - GET NEXT                               
*                                                                               
         MVI   NOTIMESH,C'N'       ACCEPT W/O'S FOR THIS                        
         AP    SRTBLVAL,TRNAMNT    BILLING VALUE IN SRTREC                      
         MVI   BILLED,C'N'         SET BILLED SWITCH TO NO                      
         OC    TRNRSUSE,TRNRSUSE   FULLY BILLED                                 
         BZ    REQL60              NO - CHECK FOR X'40'                         
         GOTO1 DATCON,DMCB,(2,TRNRSUSE),(1,BILDATE) DATE BLD =YMD PKED          
         GOTO1 =A(GET77DTE),DMCB,(RC)  10/92 USE '77' DATE AS BIL DATE          
*                                      09/96 - '4B' CHANGES TO '77'             
*                                                                               
* FILTER BILLED/BILLABLE DATA VS MSTR/MEND OF THE REQUEST                       
*                                                                               
         CLC   BILDATE(2),M1START  LOWER THAN START MOA YM                      
         BL    REQL60              CHECK FOR X'40'                              
         CLC   BILDATE(2),M1END    HIGHER THAN END MOA YM                       
         BH    REQL60                                                           
*                                                                               
         MVI   BILLED,C'Y'         SET BILLED SWITCH                            
         AP    SRTBILLD,TRNAMNT    FEE'S BILLED IN SRTREC                       
*                                                                               
         USING PRTELD,R4                                                        
REQL60   MVI   ELCODE,PRTELQ       X'40' - GET BILLABLE HOURS                   
         BAS   RE,NEXTEL2          GET  X'40' EL                                
         BNE   REQL110                                                          
         ZAP   SRTBLHRS,PRTHOUR    SAVE BILLABLE HOURS                          
         CLI   BILLED,C'Y'         IF NOT FULLY BILLED                          
         BNE   *+14                LOOKUP UP THE X'77' S                        
         ZAP   SRTHRSBL,PRTHOUR    SAVE HOURS BILLED                            
         B     REQL110             PUT TO SORT                                  
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         USING PTAELD,R4                                                        
         L     R4,ACMAPRO2                                                      
         CLI   0(R4),PTAELQ        X'77' - PTA ELEMENT                          
         BNE   REQL110             ALL DONE                                     
*                                                                               
         MVI   ELCODE,PTAELQ                                                    
         B     *+12                                                             
REQL70   BAS   RE,NEXTEL2          GET  X'77' EL                                
         BNE   REQL110             PUT TO SORT                                  
         TM    PTASTAT1,PTASPEND   PENDING? - SKIP                              
         BO    REQL70                                                           
*                                                                               
         OC    PTANET,PTANET       SKIP ZERO BALANCE                            
         BZ    REQL70                                                           
*                                                                               
         CLI   PTATYPE,PTATWOF     IGNORE W/O'S                                 
         BE    REQL70                                                           
         CLI   PTATYPE,PTATWOFR    AND W/O RECOVERY                             
         BE    REQL70                                                           
*                                                                               
         CLI   PTATYPE,PTATRAL     ALLOCATED TO BILL? - SKIP                    
         BNE   REQL70                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(2,PTARBLDT),(1,BILDATE) DATE BLD =YMD PKED          
         CLC   BILDATE(2),M1START  LOWER THAN START MOA YM                      
         BL    REQL70              LOOK FOR ANOTHER X'77'                       
         CLC   BILDATE(2),M1END    HIGHER THAN END MOA YM                       
         BH    REQL70              LOOK FOR ANOTHER X'77'                       
         AP    SRTBILLD,PTANET     ADD INTO SORT REC                            
         LH    RF,PTAHOURS         GET BILLED HOURS                             
         CVD   RF,DUB              MAKE IT DECIMAL                              
         AP    SRTHRSBL,DUB        ADD INTO SORT REC                            
         B     REQL70                                                           
*                                                                               
* BUILD WRITE OFF HRS & AMT SORT RECORD                                         
* FILTER TYPE 57/58 TRANSACTIONS VS MOS OF THE REQUEST                          
*                                                                               
REQL80   CLC   MOS,M1START         LOWER THAN START MOA YM                      
         BL    REQL110                                                          
         CLC   MOS,M1END           HIGHER THAN END MOA YM                       
         BH    REQL110                                                          
*                                                                               
         CLI   NOTIMESH,C'N'       DOES THIS HAVE A 49                          
         BNE   REQL110                                                          
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         USING PTAELD,R4                                                        
         L     R4,ACMAPRO2                                                      
         CLI   0(R4),PTAELQ        X'77' - PTA ELEMENT                          
         BNE   REQL100                                                          
*                                                                               
         MVI   ELCODE,PTAELQ       ISOLATE WRITEOFFS                            
         B     *+12                                                             
REQL90   BAS   RE,NEXTEL2          GET X'77'                                    
         BNE   REQL100             PUT TO SORT                                  
*                                                                               
         CLI   PTATYPE,PTATWOF     W/O'S?                                       
         BE    *+12                                                             
         CLI   PTATYPE,PTATWOFR    AND W/O RECOVERY                             
         BNE   REQL90              NO                                           
*                                                                               
         ZAP   DUB,PTANET          GET NET AMOUNT                               
         LH    RF,PTAHOURS         GET HOURS                                    
         CVD   RF,DOUBLE                                                        
*                                                                               
* FIX PROBLEM W/RECOVERIES HAVING NEGATIVE HOURS                                
*                                                                               
         XC    HALF,HALF                                                        
         MVN   HALF(1),DUB+7       MOVE SIGN NIBBLE INTO HALF                   
         MVN   HALF+1(1),DOUBLE+7                                               
         CLC   HALF(1),HALF+1      SAME SIGN                                    
         BE    *+10                YES                                          
         MP    DOUBLE,=P'-1'       NO, HOURS HAS THE WRONG SIGN                 
*                                                                               
         AP    SRTWODO,DUB         WRITE OFF AMOUNT                             
         AP    SRTWOHR,DOUBLE      WRITE OFF HOURS                              
         B     REQL90                                                           
*                                                                               
REQL100  CLI   BILLED,C'Y'         DID I CONSIDER THIS FULLY BILLED             
         BNE   REQL110             NO, SUM OF 77'S IS OK                        
*                                                                               
         AP    SRTBILLD,SRTWODO    ADJUST BILLED FIGURES                        
         AP    SRTHRSBL,SRTWOHR                                                 
*                                                                               
REQL110  BAS   RE,PUTSRT                                                        
REQL120  BAS   RE,CLRSRT                                                        
         B     REQL20                                                           
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* PRINTED REPORT                                                     *          
**********************************************************************          
         SPACE 1                                                                
REPORT   DS    0H                                                               
         LA    R1,ACCUMS                  CLEAR ALL REPORT TOT ACCUMS           
         LA    R0,BUKCOUNT                                                      
         ZAP   0(7,R1),=P'0'                                                    
         LA    R1,7(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         OC    ALSORT,ALSORT              IS THERE A LAST SORT ADDR             
         BZ    RPT50                      NO DATA                               
         MVC   LSTWRK(SRTLNQ),XSPACES     CLEAR SAVE AREA FOR PREV REC          
         MVC   PAGE,=H'1'                 SET PAGE TO ONE                       
         MVI   TOTSW,TOTEMP               SET FOR EMPLOYEE TOTAL                
*                                                                               
         MVC   EMPLOYEE(1),RCCOMPFL       MOVE IN COMPANY                       
         MVC   EMPLOYEE+1(2),=C'1R'       U/L 1R                                
         MVC   EMPCODE,SPACES             1R SAVE                               
         MVC   SJACCT(1),RCCOMPFL         MOVE IN COMPANY                       
         MVC   SJACCT+1(2),=C'SJ'         U/L SJ                                
         MVC   SJCODE,SPACES              SJ SAVE                               
         MVC   NMEFLD(NMELN1Q),XSPACES                                          
         MVC   CLINME(NMELN2Q),XSPACES                                          
*                                                                               
RPT10    GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   R6,15,DMCB+4                                                     
         ST    R6,ALSORT                  ADDRESS OF LAST SORT                  
         BZ    RPT20                      END OF RECORDS FROM SORT              
         MVC   SRTWRK(SRTLNQ),0(R6)       SAVE CURRENT SORT RECORD              
         CLC   LSTWRK(SRTLNQ),XSPACES     DO I HAVE ONE SAVED                   
         BNE   *+14                       YES - CONTINUE                        
         MVC   LSTWRK(SRTLNQ),SRTWRK      NO  - SAVE THIS ONE                   
         B     RPT10                      AND GET NEXT                          
*                                                                               
         CLC   LSTWRK(SRTDLNQ),SRTWRK     SAME KEY                              
         BNE   RPT20                      NO - PROCESS SAVED ONE                
*                                                                               
         LA    R5,LSTWRK                  YES - ADD'EM UP                       
         AP    SRTTARGT-SRTD(L'SRTTARGT,R5),SRTTARGT                            
         AP    SRTHRACT-SRTD(L'SRTHRACT,R5),SRTHRACT                            
         AP    SRTSTAND-SRTD(L'SRTSTAND,R5),SRTSTAND                            
         AP    SRTHRSBL-SRTD(L'SRTHRSBL,R5),SRTHRSBL                            
         AP    SRTBLHRS-SRTD(L'SRTBLHRS,R5),SRTBLHRS                            
         AP    SRTBLVAL-SRTD(L'SRTBLVAL,R5),SRTBLVAL                            
         AP    SRTBILLD-SRTD(L'SRTBILLD,R5),SRTBILLD                            
         AP    SRTSTRAT-SRTD(L'SRTSTRAT,R5),SRTSTRAT   RATE X STD HR            
         AP    SRTWOHR-SRTD(L'SRTWOHR,R5),SRTWOHR      WO HRS                   
         AP    SRTWODO-SRTD(L'SRTWODO,R5),SRTWODO      WO DOLLARS               
*                                                                               
         CLC   SRTFIRE-SRTD(L'SRTFIRE,R5),SRTFIRE      TAKE THE HIGHEST         
         BH    *+10                                    FIRE DATE                
         MVC   SRTFIRE-SRTD(L'SRTFIRE,R5),SRTFIRE                               
         B     RPT10                      AND GET NEXT                          
*                                                                               
RPT20    BAS   RE,REFRESH                 PROCESS SAVED RECORD                  
         OC    ALSORT,ALSORT              IS IT END OF FILE                     
         BZ    RPT40                      YES - PRINT FINAL TOTALS              
*                                                                               
         LA    R5,LSTWRK                  YES - ADD'EM UP                       
         CLC   SRTEMPL-SRTD(12,R5),SRTEMPL  SAME EMPLOYEE                       
         BE    RPT30                        YES - CONTINUE                      
         MVI   TOTSW,TOTEMP                 NO  - SET FOR EMP TOTAL             
         BAS   RE,ACCTOT                                                        
*                                                                               
RPT30    SR    R1,R1                                                            
         IC    R1,LEVC                    LENGTH OF THIRD LEVEL                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRTEMPL-SRTD(0,R5),SRTEMPL  SAME CAT                             
         BE    *+12                        YES - CONTINUE                       
         MVI   TOTSW,TOTCAT                NO  - SET FOR CAT TOTAL              
         BAS   RE,ACCTOT                                                        
*                                                                               
         SR    R1,R1                                                            
         IC    R1,LEVB                    LENGTH OF SECOND LEVEL                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRTEMPL-SRTD(0,R5),SRTEMPL  SAME DPT                             
         BE    *+12                        YES - CONTINUE                       
         MVI   TOTSW,TOTDPT                NO  - SET FOR DPT TOTAL              
         BAS   RE,ACCTOT                                                        
*                                                                               
         SR    R1,R1                                                            
         IC    R1,LEVA                    LENGTH OF FIRST LEVEL                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRTEMPL-SRTD(0,R5),SRTEMPL  SAME OFF                             
         BE    *+12                        YES - CONTINUE                       
         MVI   TOTSW,TOTOFF                NO  - SET FOR OFF TOTAL              
         BAS   RE,ACCTOT                                                        
*                                                                               
         MVC   LSTWRK(SRTLNQ),SRTWRK       SAVE THIS ONE                        
         B     RPT10                       AND GET NEXT.                        
*                                                                               
* FINAL TOTALS                                                                  
*                                                                               
RPT40    BAS   RE,ACCTOT                                                        
         MVI   TOTSW,TOTCAT               CAT TOTAL  (1R)                       
         BAS   RE,ACCTOT                                                        
         MVI   TOTSW,TOTDPT               DPT TOTAL  (1R)                       
         BAS   RE,ACCTOT                                                        
         MVI   TOTSW,TOTOFF               OFF TOTAL  (1R)                       
         BAS   RE,ACCTOT                                                        
*                                                                               
RPT50    MVI   TOTSW,TOTRPT               SET FOR REPORT TOTAL                  
         BAS   RE,ACCTOT                                                        
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         B     EXIT                                                             
         DROP  R3,R5,R6                                                         
         EJECT                                                                  
**********************************************************************          
* ROUTINE FOR RUN LAST                                               *          
**********************************************************************          
         SPACE 1                                                                
RUNL     DS    0H                                                               
         GOTO1 =A(RELBUFF),DMCB,(RC) RELEASE GETMAINED SPACE                    
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET HEIRARCHY LEVEL STRUCTURE                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING ACLELD,R5                                                        
GETLEVS  NTR1                                                                   
         L     R5,ADLDGHIR         GET HEIRARCHY LEVELS                         
         MVC   LEVELS(LEVLNQ),SPACES                                            
         SR    R1,R1                                                            
         IC    R1,ACLLN            R1 = LENGTH OF ELEMENT                       
         SH    R1,=Y(ACLLN1Q+1)    SUBTRACT OVERHEAD                            
         EX    R1,*+4                                                           
         MVC   LEVELS(0),ACLVALS   MVC LEVEL LENGTHS/NAMES INTO STORAGE         
*                                                                               
* CONVERT 1,3,5,12 -> 1,2,2,7                                                   
*                                                                               
         LA    R0,LEVELQ           R0 = MAXIMUM NUMBER OF LEVELS                
         STC   R0,LEVNUM           ASSUME 4 LEVEL STRUCTURE                     
         LA    R1,LEVELS           R1 = FIRST LEVEL LENGTH                      
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R4,R4                                                            
GLEV10   ICM   R4,1,0(R1)          CURRENT LEVEL LENGTH                         
         BZ    GLEV20              NO MORE LEVELS - ADJUST LEVNUM               
         SR    R4,R3               SUBTRACT CURRENT FROM PREVIOUS               
         STC   R4,0(R2)            CURRENT INDIVIDUAL LENGTH                    
         IC    R3,0(R1)            UPDATE R3                                    
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         BCT   R0,GLEV10                                                        
         B     GLEVX                                                            
*                                                                               
GLEV20   LA    R1,LEVELQ           R1 = MAXIMUM NUMBER OF LEVELS                
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GLEVX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* SET LEVEL CODES                                                    *          
*     R1 - INDIVIDUAL LEVELS OF 1R                                   *          
*     R2 - FULL 1R ACCOUNT                                           *          
**********************************************************************          
         SPACE 1                                                                
SETCDE   NTR1                                                                   
         MVC   LEVSCDE(LVCDLNQ),SPACES     CLEAR LEVEL CODES AREA               
         SR    R0,R0                                                            
         IC    R0,LEVNUM           NUMBER OF LEVELS/LOOPS                       
         LA    R3,LEVLNQS          FIRST LEVEL LENGTHS                          
*                                                                               
SETCDE10 SR    R4,R4                                                            
         IC    R4,0(R3)            CURRENT INDIVIDUAL LEV LENGTH                
         BCTR  R4,0                                                             
         EX    R4,*+4                                                           
         MVC   0(0,R1),0(R2)                                                    
         LA    R2,1(R4,R2)         BUMP TO NEXT LEVEL IN ACCOUNT CODE           
         LA    R1,12(R1)           BUMP TO NEXT LEVEL CODE AREA                 
         LA    R3,1(R3)            BUMP TO NEXT INDIVIUAL LEV LENGTH            
         BCT   R0,SETCDE10                                                      
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REFRESH NAMES WHERE NECESSARY                                      *          
* REPORT LEVEL TITLES                                                *          
**********************************************************************          
         SPACE 1                                                                
REFRESH  NTR1                                                                   
         USING SRTD,R6                                                          
         LA    R6,LSTWRK                                                        
         MVC   XP,XSPACES                                                       
         MVC   XPSECOND,XSPACES                                                 
         USING ACTRECD,R3                                                       
         LA    R3,SVKEY                                                         
*                                                                               
* ** 1R NAMES **                                                                
*                                                                               
         CLC   EMPCODE,SRTEMPL            SAME EMPLOYEE                         
         BE    REFSH40                    YES- DONT REFRESH 1R NAMES            
*                                                                               
         MVC   EMPLVCDS(EMPLVLNQ),SPACES  CLEAR IDIVIDUAL LEVELS                
         LA    R1,EMPLVCDS                EMPLOYEE INDIVIDUAL LEVELS            
         LA    R2,EMPCODE                 FULL 1R ACCOUNT                       
         BAS   RE,SETCDE                  R1 AND R2 MUST BE SET                 
*                                                                               
         MVC   SRTLVCDS(SRTLVLNQ),SPACES  CLEAR IDIVIDUAL LEVELS                
         LA    R1,SRTLVCDS                EMPLOYEE INDIVIDUAL SORT LEVS         
         LA    R2,SRTEMPL                 FULL 1R ACCOUNT                       
         BAS   RE,SETCDE                  R1 AND R2 MUST BE SET                 
*                                                                               
         CLC   EMPOFFC,SRTOFFC            SAME 1R OFFICE                        
         BE    REFSH10                                                          
         MVC   ACTKCULA(ACCORFST),SPACES  CLEAR KEY                             
         MVC   ACTKCULA(3),EMPLOYEE       CO/U/L                                
         SR    R1,R1                                                            
         IC    R1,LEVA                    LENGTH OF FIRST LEVEL                 
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   ACTKACT(0),SRTEMPL         MOVE IN OFFICE                        
         GOTO1 =A(DMREADDR),DMCB,(RC)     READ RECORD                           
         BE    *+6                 CAN'T FIND THIS BUD-CC SET IN CALL           
         DC    H'0'                DIE                                          
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         BAS   RE,GETNME                                                        
         MVC   OFNAME,WORK         SAVE 1R OFFICE NAME                          
         MVC   OFCODE,SRTOFFC      SAVE 1R OFFICE CODE                          
         BAS   RE,HEADUP                                                        
*                                                                               
REFSH10  CLI   QOPT2,C'S'          SUPRESS DEP/CAT                              
         BE    REFSH30             YES,                                         
         CLC   EMPDEPT,SRTDEPT            SAME 1R DEPT                          
         BE    REFSH20                                                          
         MVC   ACTKCULA(ACCORFST),SPACES   CLEAR KEY                            
         MVC   ACTKCULA(3),EMPLOYEE       CO/U/L                                
         SR    R1,R1                                                            
         IC    R1,LEVB                    LENGTH OF SECOND LEVEL                
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   ACTKACT(0),SRTEMPL         OFFICE/DEPT                           
         GOTO1 =A(DMREADDR),DMCB,(RC)     READ RECORD                           
         BE    *+6                 CAN'T FIND THIS BUD-CC SET IN CALL           
         DC    H'0'                DIE                                          
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         BAS   RE,GETNME                                                        
         MVC   DPNAME,WORK         SAVE 1R DEPT NAME                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,LINE             PRESENT LINE                                 
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         IC    RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   *+8                                                              
         BAS   RE,HEADUP           HEAD UP NEW PAGE                             
*                                                                               
         MVC   XP+1(4),=C'DEPT'                                                 
         LA    R2,SRTEMPL                                                       
         SR    R0,R0                                                            
         IC    R0,LEVA             LENGTH OF FIRST LEVEL                        
         AR    R2,R0               BUMP TO SECOND LEVEL OF ACCOUNT              
         SR    R1,R1                                                            
         IC    R1,LEVLNQB                 LENGTH OF SECOND LEVEL                
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   XP+6(0),0(R2)       NEW DEPT TITLE                               
         MVC   XP+13(L'DPNAME),DPNAME                                           
         GOTO1 UNDERLIN,DMCB,(44,XP+1),(X'BF',XPSECOND+1)                       
         GOTO1 ACREPORT                                                         
*                                                                               
REFSH20  DS    0H                                                               
         CLC   EMPCAT,SRTCAT              SAME 1R SUB-DEPT                      
         BE    REFSH30                                                          
         MVC   ACTKCULA(ACCORFST),SPACES  CLEAR KEY                             
         MVC   ACTKCULA(3),EMPLOYEE       CO/U/L                                
         SR    R1,R1                                                            
         IC    R1,LEVC                    LENGTH OF THIRD LEVEL                 
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   ACTKACT(0),SRTEMPL         OFFICE/DEPT/SUB-DEPT                  
         GOTO1 =A(DMREADDR),DMCB,(RC)     READ RECORD                           
         BE    *+6                 CAN'T FIND THIS BUD-CC SET IN CALL           
         DC    H'0'                DIE                                          
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         BAS   RE,GETNME                                                        
*                                                                               
         MVC   CATNAME,WORK        SAVE 1R CATEGORY NAME                        
         SR    RF,RF                                                            
         IC    RF,LINE             PRESENT LINE                                 
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         IC    RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   *+8                                                              
         BAS   RE,HEADUP           HEAD UP NEW PAGE                             
*                                                                               
         MVC   XP+1(8),=C'SUB DEPT'                                             
         SR    R1,R1                                                            
         IC    R1,LEVLNQC                 LENGTH OF THIRD LEVEL                 
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   XP+10(0),SRTCAT     NEW CATEGORY TITLE                           
         MVC   XP+13(L'CATNAME),CATNAME                                         
         GOTO1 UNDERLIN,DMCB,(44,XP+1),(X'BF',XPSECOND+1)                       
         GOTO1 ACREPORT                                                         
*                                                                               
REFSH30  CLC   EMPSTAF,SRTEMPL     1R STAFF SAME                                
         BE    REFSH40                                                          
         MVC   EMPLNAME,SRTENAME                                                
         MVC   HIREDATE,SRTHIRE                                                 
         MVC   FIREDATE,SRTFIRE                                                 
         MVC   BUDSTAT,SRTSTAT                                                  
         SR    RF,RF                                                            
         IC    RF,LINE             PRESENT LINE                                 
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         IC    RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   *+8                                                              
         BAS   RE,HEADUP           HEAD UP NEW PAGE                             
*                                                                               
         MVC   XP+1(5),=C'STAFF'                                                
         SR    R1,R1                                                            
         IC    R1,LEVLNQD                 LENGTH OF FOURTH LEVEL                
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   XP+7(0),SRTSTAF     NEW EMPLOYEE TITLE                           
         MVC   XP+13(L'SRTENAME),SRTENAME                                       
         GOTO1 UNDERLIN,DMCB,(44,XP+1),(X'BF',XPSECOND+1)                       
*                                                                               
         CLI   QOPT1,C'E'          EMPLOYEE DETAIL REPORT                       
         BNE   *+12                PRINT IT                                     
         LA    R2,XP+48                                                         
         BAS   RE,HIREFIRE                                                      
         GOTO1 ACREPORT                                                         
         MVC   EMPCODE,SRTEMPL     MAKE NEW EMPLOYEE CURRENT                    
*                                                                               
REFSH40  CLC   SRTSJ,SJCODE        SJ CODE SAME AS SAVED                        
         BE    PRNT                YES - DONT REFRESH SJ NAMES                  
         CLC   SRTSJ,SPACES        IF NO JOB ITS BUD OR HRS REC                 
         BE    PRNT                                                             
*                                                                               
* LOOK IN TABLE FIRST                                                           
*                                                                               
         USING BIND,R5                                                          
         L     R5,ACLTLST                 ADDR OF CLTLIST                       
         MVC   DMCB+8(16),BININ           MOVE IN PARMS 3,4,5,6                 
         LA    R0,BINTAB                                                        
         GOTO1 BINSRCH,DMCB,SRTSJ,(R0)                                          
         CLI   DMCB,0                                                           
         BNE   REFSH50                    NOT FOUND IN TABLE                    
         USING CLTCDE,R5                                                        
         L     R5,DMCB                                                          
         MVC   CLINME,CLTNAME             SJ CLIENT NAME FROM TAB               
         MVC   SJACCT+3(12),CLTKEY        SJ CLIENT CODE FROM TAB               
         B     PRNT                                                             
*                                                                               
* NOT IN TAB - LOOK IT UP                                                       
*                                                                               
REFSH50  MVC   ACTKCULA(ACCORFST),SPACES   CLEAR KEY                            
         MVC   SJCODE,SRTSJ               MOVE IN SJ CLI CODE                   
         MVC   ACTKCULA,SJACCT            KEY FOR READ                          
         GOTO1 =A(DMREADDR),DMCB,(RC)     READ RECORD                           
         BE    *+6                 CAN'T FIND THIS BUD-CC SET IN CALL           
         DC    H'0'                DIE                                          
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         BAS   RE,GETNME                                                        
         MVC   CLINME,WORK                SAVE SJ CLIENT NAME                   
*                                                                               
         MVC   WORK,SPACES                CLEAR TAB ENTRY WORK AREA             
         USING CLTCDE,R5                                                        
         LA    R5,WORK                                                          
         MVC   CLTNAME,CLINME             UPDATE TABLE                          
         MVC   CLTKEY,SRTSJ                                                     
         GOTO1 =A(BINADD),DMCB,(RC),(R5),ACLTLST                                
         DROP  R3,R5                                                            
         EJECT                                                                  
**********************************************************************          
* CHECK AND SEE IF CLIENT CODE IS IN IN NEW BUSINESS/PRO BONO TABLE  *          
**********************************************************************          
* PRINT THE DETAIL LINE                                              *          
* UPDATE TOTALS                                                      *          
**********************************************************************          
         SPACE 1                                                                
PRNT     MVC   XP,XSPACES              CLEAR FIRST PRINT LINE                   
         MVC   XPSECOND,XSPACES        CLEAR 2ND PRINT LINE                     
         BAS   RE,CLRBK                CLEAR PRINT BLOCK                        
         MVC   AMNTTAB(AMNTLNQ),XSPACES      CLEAR TABLE                        
*                                                                               
         CLC   SRTSJ,SPACES               IS THERE AN SJ ACCOUNT                
         BE    PRNT30                     NO- ITS AN BUD OR HRS REC             
         MVC   PRNTBLOC+1(12),SRTSJ       SJCODE                                
         MVC   PRNTBLOC+14(30),CLINME     NAME                                  
*                                                                               
         LA    R2,SRTBLHRS                                                      
         LA    R3,BILHR                                                         
         BAS   RE,EDITHR                                                        
         LA    R2,SRTBLVAL                                                      
         LA    R3,BILDOL                                                        
         BAS   RE,EDITAMT                                                       
*                                                                               
         CP    SRTSTAND,=P'0'                                                   
         BE    PRNT10                                                           
         CP    SRTBLHRS,=P'0'                                                   
         BE    PRNT10                                                           
         CP    SRTTARGT,=P'0'                                                   
         BE    PRNT10                                                           
         ZAP   ANSWER,SRTBLHRS     BILLABLE HRS                                 
         ZAP   DOUBLE,SRTSTAND                                                  
         MP    DOUBLE,SRTTARGT                                                  
         DP    ANSWER,DOUBLE       STANDARD HRS                                 
         SRP   ANSWER(7),64-1,5                                                 
*                                                                               
         LA    R3,BILSTHR                                                       
         BAS   RE,EDITPCT                                                       
PRNT10   LA    R2,SRTHRSBL                                                      
         LA    R3,BILDHR                                                        
         BAS   RE,EDITHR                                                        
         LA    R2,SRTBILLD                                                      
         LA    R3,BILDDOL                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
         LA    R2,SRTWOHR                                                       
         LA    R3,WOHR                                                          
         BAS   RE,EDITHR                                                        
         LA    R2,SRTWODO                                                       
         LA    R3,WODOL                                                         
         BAS   RE,EDITAMT                                                       
*                                                                               
* TIME HELD = BILLABLE HRS + WRITE-OFF HRS - HOURS BILLED                       
*                                                                               
         ZAP   ANSWER,SRTBLHRS     BILLABLE HRS                                 
         AP    ANSWER,SRTWOHR      HRS WRITTEN-OFF                              
         SP    ANSWER,SRTHRSBL     HOURS BILLED                                 
         LA    R2,ANSWER+7                                                      
         LA    R3,HELDHR                                                        
         BAS   RE,EDITHR                                                        
*                                                                               
* TIME $ HELD = BILLABLE $ + WRITE-OFF $ + BILLED $                             
*                                                                               
         ZAP   ANSWER,SRTBLVAL     BILLABLE $                                   
         AP    ANSWER,SRTWODO      $ WRITE-OFF                                  
         SP    ANSWER,SRTBILLD     $ BILLED                                     
         LA    R2,ANSWER+7                                                      
         LA    R3,HELDDOL                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
* BILLED VS VALUE HOURS = HOURS BILLED - BILLABLE HOURS                         
*                                                                               
         ZAP   ANSWER,SRTHRSBL     HOURS BILLED                                 
         SP    ANSWER,SRTBLHRS     BILLABLE HOURS                               
         LA    R2,ANSWER+7                                                      
         LA    R3,BVVHR                                                         
         BAS   RE,EDITHR                                                        
*                                                                               
         ZAP   ANSWER,SRTBILLD     $ BILLED                                     
         SP    ANSWER,SRTBLVAL     BILLABLE $                                   
         LA    R2,ANSWER+7                                                      
         LA    R3,BVVDOL                                                        
         BAS   RE,EDITAMT                                                       
*                                                                               
         CP    SRTBLHRS,=P'0'                                                   
         BE    PRNT20                                                           
         CP    SRTHRSBL,=P'0'                                                   
         BE    PRNT20                                                           
         ZAP   ANSWER,SRTHRSBL     VALUE VS BILLED                              
         MP    ANSWER,=P'10000'    (PCT)                                        
         DP    ANSWER,SRTBLHRS                                                  
         SRP   ANSWER(7),64-1,5                                                 
         LA    R3,BVVPER                                                        
         BAS   RE,EDITPCT                                                       
*                                                                               
* BILLED VS STAND HOURS = HOURS BILLED - STANDARD HOURS                         
*                                                                               
         CP    SRTSTAND,=P'0'                                                   
         BE    PRNT20                                                           
         ZAP   ANSWER,SRTHRSBL     HOURS BILLED                                 
         SP    ANSWER,DOUBLE       STANDATD HOURS                               
         LA    R2,ANSWER+7                                                      
         LA    R3,BVSHR                                                         
         BAS   RE,EDITHR                                                        
*                                                                               
         ZAP   ANSWER,SRTBILLD     $ BILLED                                     
         SP    ANSWER,SRTSTRAT     BILLABLE $                                   
         LA    R2,ANSWER+7                                                      
         LA    R3,BVSDOL                                                        
         BAS   RE,EDITAMT                                                       
*                                                                               
         MVC   BVSPER,SPACES                                                    
         CP    SRTSTAND,=P'0'                                                   
         BE    PRNT20                                                           
         CP    SRTHRSBL,=P'0'                                                   
         BE    PRNT20                                                           
         ZAP   ANSWER,SRTHRSBL     VALUE VS STAND                               
         MP    ANSWER,=P'10000'    (PCT)                                        
         DP    ANSWER,SRTSTAND                                                  
         SRP   ANSWER(7),64-1,5                                                 
         LA    R3,BVSPER                                                        
         BAS   RE,EDITPCT                                                       
*                                                                               
PRNT20   SR    RF,RF                                                            
         IC    RF,LINE             PRESENT LINE                                 
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         IC    RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   *+8                                                              
         BAS   RE,HEADUP           HEAD UP NEW PAGE                             
*                                                                               
         BAS   RE,BLDBK            BUILD PRINT BLOCK                            
         OC    PRNTBLOC(165),XSPACES                                            
         OC    PRNTBLOC+165(165),XSPACES                                        
         MVC   XP(165),PRNTBLOC                                                 
         MVC   XPSECOND(165),PRNTBLOC+165                                       
         GOTO1 ACREPORT            AND PRINT IT                                 
         CLC   PRNTBLOC+165(165),XSPACES                                        
         BNE   PRNT30                                                           
         GOTO1 ACREPORT                                                         
*                                                                               
* UPDATE TOTALS                                                                 
*                                                                               
PRNT30   LA    R0,LEVCOUNT         NUMBER OF REPORT LEVEL TOTALS                
         USING TOTALD,R1                                                        
         LA    R1,ACCUMS                                                        
PRNT40   AP    TOTTAR,SRTTARGT     ADD TARGET TO TOTALS                         
         AP    TOTHRS,SRTHRACT     HOURS                                        
         AP    TOTSTAND,SRTSTAND   STANDARD HOURS                               
         AP    TOTBLHRS,SRTBLHRS   BILLABLE HOURS                               
         AP    TOTHRSBL,SRTHRSBL   HOURS BILLED                                 
         AP    TOTBLVAL,SRTBLVAL   BILLING VALUE                                
         AP    TOTBILLD,SRTBILLD   TIME BILLED                                  
         AP    TOTRATE,SRTSTRAT    STAND RATE $                                 
         AP    TOTWOHR,SRTWOHR     WO HOURS                                     
         AP    TOTWODO,SRTWODO     WO DOLLARS                                   
         ZAP   PL16,SRTSTAND       CALC STD HRS X TRG PCT                       
         MP    PL16,SRTTARGT                                                    
         SRP   PL16,64-4,5                                                      
         AP    TOTHRXPC,PL16       ADJUSTED STANDARD HOURS                      
         LA    R1,TOTLEN(R1)       NEXT LEVEL OF ACCUMS                         
         BCT   R0,PRNT40                                                        
         B     EXIT                                                             
         DROP  R1,R6                                                            
         EJECT                                                                  
**********************************************************************          
* BUILD PRINT BLOCK COLUMNS                                          *          
*     R2 = FIELDS INPUT INTO COLUMN  FROM OPTION3 TABLE              *          
*     R4 = AMNTTTBLE - OUTPUT VARIABLES                              *          
*     R5 = PRINT BUFFER                                              *          
*     R6 = COLUMN TABLE                                              *          
*     COLSWIT = SET FOR 1ST FIELD IN COLUMN & CLEARED AFTER LAST     *          
*               FIELD IN COLUMN ( DENOTED BY '77')                   *          
**********************************************************************          
        SPACE 1                                                                 
BLDBK   NTR1                                                                    
        XC     CNTNUM,CNTNUM                                                    
        ZAP    COLSWIT,=P'0'                                                    
        LA     R2,OPTION3                  PT TO OPTION3 TABLE                  
        LA     R5,PRNTBLOC+64                                                   
        LA     R1,OPTLNQ                   # FORMATS AVAILABLE                  
*                                                                               
BLDBK10 CLC    QOPT3(1),0(R2)              OPTION MATCH?                        
        BE     BLDBK20                     YES                                  
        LA     R2,TLENGTH(R2)              BUMP TO NEXT OPTION                  
        BCT    R1,BLDBK10                                                       
        DC     H'0'                        DUMP IF NO MATCH                     
*                                                                               
        USING  COLUMND,R6                                                       
BLDBK20 LA     R6,COLUMN                                                        
        LA     R4,AMNTTAB                  OUTPUT TABLE                         
        LA     R2,1(R2)                    1ST COL POSITION                     
*                                                                               
        CLI    0(R2),ENDOFROW              END OF LINE                          
        BE     BLCXIT                      YES                                  
*                                                                               
        CLI    0(R2),ENDOFCOL              END OF COLUMN?                       
        BNE    BLC24                       YES                                  
*                                                                               
        CP     COLSWIT,=P'0'               ANY ITEMS IN COLUMN                  
        BE     BLDBK20                     NO                                   
*                                                                               
        CP     COLSWIT,=P'2'               2 ITEMS/COL?                         
        BE     BLC22                       YES                                  
*                                                                               
        LA     R5,17(R5)                   INCREMENT LINE BY 17                 
        B      BLC23                                                            
*                                                                               
BLC22   LA     R5,8(R5)                    INCREMENT LINE BY 8                  
BLC23   ZAP    COLSWIT,=P'0'                                                    
        B      BLDBK20                                                          
*                                                                               
BLC24   CLI    0(R2),SKIP                  PRINT COLUMN?                        
        BE     BLDBK20                     NO                                   
*                                                                               
        ZIC    R1,0(R2)                    OPTION3 EQUATE                       
        LA     R0,COLEND                   END OF COLUMN TABLE                  
BLC27   CLM    R1,1,COLNUM                 DID WE GET MATCH?                    
        BE     BLC30                       YES                                  
        LA     R6,8(R6)                    NEXT ROW IN COLNUMD                  
        BCT    R0,BLC27                                                         
        DC     H'0'                        INVALID INPUT IN TABL                
*                                                                               
BLC30   CP     COLSWIT,=P'0'               MORE THAN 1 ITEM/COL?                
        BE     BLC32                       NO                                   
        LA     R5,9(R5)                    YES                                  
*                                                                               
BLC32   ZIC    RF,COLOFF                   OFFSET INTO AMNTTABLE                
        AR     R4,RF                       PT TO LOC IN AMNTTABL                
*                                                                               
        ZIC    R1,COLLEN                   SIZE OF OUTPUT FIELD                 
        SH     R1,=H'1'                                                         
        EX     R1,BLC33                                                         
        AP     COLSWIT,=P'1'               ITEM IN COLUMN COUNTE                
        B      BLDBK20                                                          
BLC33   MVC    0(0,R5),0(R4)               MOVE TO PRINT BUFFER                 
*                                                                               
BLCXIT  ZIC    R1,CNTNUM                                                        
        LA     R1,1(R1)                                                         
        STC    R1,CNTNUM                                                        
        LA     R5,PRNTBLOC+231                                                  
        ZAP    COLSWIT,=P'0'               CLEAR COLUMN COUNTER                 
        CH     R1,=H'2'                    IS THERE SECOND LINE                 
        BL     BLDBK20                     YES                                  
        MVC    AMNTTAB,XSPACES                                                  
        B      EXIT                                                             
        DROP   R6                                                               
        EJECT                                                                   
**********************************************************************          
* REPORT TOTALS                                                      *          
**********************************************************************          
         SPACE 1                                                                
ACCTOT   NTR1                                                                   
         USING TOTALD,R5                                                        
         BAS   RE,CLRBK            CLEAR PRINT BLOCK                            
         MVC   PRN2BLOC(165),XSPACES                                            
         MVC   XP(165),XSPACES                                                  
         MVC   XPSECOND(165),XSPACES                                            
*                                                                               
         MVC   EMPLVCDS(EMPLVLNQ),SPACES  CLEAR IDIVIDUAL LEVELS                
         LA    R1,EMPLVCDS                EMPLOYEE INDIVIDUAL LEVELS            
         LA    R2,EMPCODE                 FULL 1R ACCOUNT                       
         BAS   RE,SETCDE                  R1 AND R2 MUST BE SET                 
*                                                                               
         CLI   QOPT1,C'E'          EMPLOYEE LEVEL AS DETAIL                     
         BNE   ACTOT10             NO                                           
         CLI   TOTSW,TOTEMP        DON'T PRINT "TOTALS FOR" ON A DETAIL         
         BE    ACTOT20                                                          
*                                                                               
ACTOT10  MVC   PRN2BLOC(11),=C' TOTALS FOR'                                     
         CLI   TOTSW,TOTEMP              IS IT A 1R EMPLOYEE TOTAL              
         BE    ACTOT20                                                          
         CLI   TOTSW,TOTCAT                         CATEGORY TOTAL              
         BE    ACTOT30                                                          
         CLI   TOTSW,TOTDPT                         DEPT TOTAL                  
         BE    ACTOT40                                                          
         CLI   TOTSW,TOTOFF                         OFFICE TOTAL                
         BE    ACTOT50                                                          
         CLI   TOTSW,TOTRPT                         REPORT TOTAL                
         BE    ACTOT60                                                          
*                                                                               
* ***EMPLOYEE TOTAL***                                                          
*                                                                               
ACTOT20  LA    R5,EMTOT                  ADDR OF EMPLOYEE ACCUMS                
         LA    R6,BUKCONT1               NUMBER ACCUM BUCKETS TO CLEAR          
         CLC   EMPSTAF,SPACES            IF BLANK, LEV TOT NOT NEEDED           
         BE    ACTOT160                                                         
*                                                                               
         CLI   QOPT1,C'E'                IS THIS AN EMPLOYEE LEVEL REP          
         BE    ACTOT70                   YES, DONT NEED "TOTALS FOR"            
*                                                                               
         MVC   PRN2BLOC+12(7),EMPSTAF     STAFF CODE                            
         MVC   PRN2BLOC+20(25),EMPLNAME   NAME                                  
         LA    R2,PRN2BLOC+48                                                   
         BAS   RE,HIREFIRE                PRINT HIRE FIRE DATES                 
         B     ACTOT70                    PUT OUT TOTALS                        
*                                                                               
* ***CATEGORY TOTAL***                                                          
*                                                                               
ACTOT30  LA    R5,CTTOT                  ADDR OF CATEGORY ACCUMS                
         LA    R6,BUKCONT2               NUMBER ACCUM BUCKETS TO CLEAR          
         CLC   EMPCAT,SPACES             IF BLANK, LEVEL TOT NOT NEEDED         
         BE    ACTOT160                                                         
         MVC   PRN2BLOC+12(2),EMPCAT     CATEGORY CODE                          
         MVC   PRN2BLOC+15(30),CATNAME   NAME                                   
         B     ACTOT70                   PUT OUT TOTALS                         
*                                                                               
* ***DEPARTMENT TOTAL***                                                        
*                                                                               
ACTOT40  LA    R5,DPTTOT                 ADDR OF DEPT ACCUMS                    
         LA    R6,BUKCONT3               NUMBER ACCUM BUCKETS TO CLEAR          
         CLC   EMPDEPT,SPACES            IF BLANK,LEVEL TOT NOT NEEDED          
         BE    ACTOT160                                                         
         MVC   PRN2BLOC+12(2),EMPDEPT    DEPT CODE                              
         MVC   PRN2BLOC+15(30),DPNAME    NAME                                   
         B     ACTOT70                   PUT OUT TOTALS                         
*                                                                               
* ***OFFICE TOTAL***                                                            
*                                                                               
ACTOT50  LA    R5,OFTOT                  ADDR OF OFFICE ACCUMS                  
         LA    R6,BUKCONT4               NUMBER ACCUM BUCKETS TO CLEAR          
         MVC   PRN2BLOC+12(2),OFCODE     OFFICE CODE                            
         MVC   PRN2BLOC+15(30),OFNAME    NAME                                   
         B     ACTOT70                   PUT OUT TOTALS                         
*                                                                               
* ***REPORT TOTAL***                                                            
*                                                                               
ACTOT60  LA    R5,RPTOT                  ADDR OF REPORT TOTAL ACCUMS            
         LA    R6,BUKCOUNT               NUMBER ACCUM BUCKETS TO CLEAR          
         MVC   PRN2BLOC+12(6),=C'REPORT' REPORT TOTAL                           
*                                                                               
ACTOT70  MVC   STDHR,SPACES        CLEAR OUT PRINT FIELDS                       
         MVC   STDVALUE,SPACES                                                  
         MVC   BILSTHR,SPACES                                                   
         SRP   TOTTAR,64-1,0                                                    
*                                                                               
* 1R TOTALS                                                                     
* PRINT THE TARGET PERCENTAGE                                                   
*                                                                               
         MVC   TRGT,SPACES                                                      
         CLI   TOTSW,TOTEMP        EMPLOYEE LEVEL TOTAL                         
         BNE   ACTOT90             NO, DONT PRINT TARGET PCT                    
*                                                                               
         CLI   PROFILES,0          NO TRG BUD DEFINED                           
         BNE   ACTOT80             YES, USE IT                                  
*                                                                               
         CLI   PROFILES+4,0        IS THERE A DEFAULT                           
         BE    ACTOT90             NO, DON'T PRINT ANYTHING                     
*                                                                               
ACTOT80  EDIT  (P7,TOTTAR),(5,TRGT),1,MINUS=YES                                 
*                                                                               
         TM    BUDSTAT,NOBUD       WAS THIS THE DEFAULT BUD                     
         BNO   ACTOT90             NO,                                          
*                                                                               
         MVI   TRGT+5,C'*'         PUT AN * AFTER IT                            
*                                                                               
ACTOT90  LA    R2,TOTHRS                                                        
         LA    R3,ACTHR                                                         
         BAS   RE,EDITHR           ACTUAL HOURS                                 
*                                                                               
         CP    TOTSTAND,=P'0'                                                   
         BE    ACTOT100                                                         
*                                                                               
         LA    R2,TOTSTAND                                                      
         LA    R3,STDHR                                                         
         BAS   RE,EDITHR           STAND HRS  *                                 
*                                                                               
         LA    R2,TOTRATE                                                       
         LA    R3,STDVALUE                                                      
         BAS   RE,EDITAMT          STAND RATE                                   
*                                                                               
ACTOT100 MVC   AMNTTAB(AMNTLNQ),XSPACES      CLEAR TABLE                        
*                                                                               
* BILLABLE VALUE COL, HOURS AND DOLLARS AND PERCENT                             
*                                                                               
         LA    R2,TOTBLHRS                                                      
         LA    R3,BILHR                                                         
         BAS   RE,EDITHR                                                        
*                                                                               
         LA    R2,TOTBLVAL                                                      
         LA    R3,BILDOL                                                        
         BAS   RE,EDITAMT                                                       
*                                                                               
         CP    TOTSTAND,=P'0'                                                   
         BE    ACTOT110                                                         
         ZAP   ANSWER,TOTBLHRS     BILLABLE HRS/STAND HRS (PCT)                 
         MP    ANSWER,=P'1000'                                                  
         DP    ANSWER,TOTSTAND                                                  
         LA    R3,BILSTHR                                                       
         BAS   RE,EDITPCT                                                       
*                                                                               
* TIME BILLED COL, HOURS AND DOLLARS                                            
*                                                                               
ACTOT110 LA    R2,TOTHRSBL                                                      
         LA    R3,BILDHR                                                        
         BAS   RE,EDITHR                                                        
*                                                                               
         LA    R2,TOTBILLD                                                      
         LA    R3,BILDDOL                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
* TIME WRITE OFF COL, HOURS AND DOLLARS                                         
*                                                                               
         LA    R2,TOTWOHR                                                       
         LA    R3,WOHR                                                          
         BAS   RE,EDITHR                                                        
*                                                                               
         LA    R2,TOTWODO                                                       
         LA    R3,WODOL                                                         
         BAS   RE,EDITAMT                                                       
*                                                                               
* TIME HELD COL, HOURS AND DOLLARS                                              
* TIME HELD = BILLABLE HR + WRITE-OFF HRS - HRS BILLED)                         
*                                                                               
         ZAP   ANSWER,TOTBLHRS         BILLABLE HOURS                           
         AP    ANSWER,TOTWOHR        - WRITE-OFFS HOURS                         
         SP    ANSWER,TOTHRSBL       - HOURS BILLED                             
         LA    R2,ANSWER+7                                                      
         LA    R3,HELDHR                                                        
         BAS   RE,EDITHR                                                        
*                                                                               
*  TIME HELD = BILLABLE $ - (WRITE-OFF $ + $ BILLED)                            
         ZAP   ANSWER,TOTBLVAL         BILLABLE $                               
         AP    ANSWER,TOTWODO        - WRITE-OFF $                              
         SP    ANSWER,TOTBILLD       - $BILLED                                  
         LA    R2,ANSWER+7                                                      
         LA    R3,HELDDOL                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
* BILLED VS VALUE COL                                                           
* BILLED HOURS - BILLABLE HOURS                                                 
*                                                                               
         ZAP   ANSWER,TOTHRSBL          HOURS BILLED                            
         SP    ANSWER,TOTBLHRS          - BILLABLE HOURS                        
         LA    R2,ANSWER+7                                                      
         LA    R3,BVVHR                                                         
         BAS   RE,EDITHR                                                        
*                                                                               
* BILLED $ - BILLABLE $                                                         
         ZAP   ANSWER,TOTBILLD          $ BILLED                                
         SP    ANSWER,TOTBLVAL          - BILLABLE $                            
         LA    R2,ANSWER+7                                                      
         LA    R3,BVVDOL                                                        
         BAS   RE,EDITAMT                                                       
*                                                                               
* BILLED HRS/ BILLABLE HRS                                                      
*                                                                               
         CP    TOTBLHRS,=P'0'                                                   
         BE    ACTOT120                                                         
         CP    TOTHRSBL,=P'0'                                                   
         BE    ACTOT120                                                         
         ZAP   ANSWER,TOTHRSBL          HOURS BILLED                            
         MP    ANSWER,=P'10000'         X 100                                   
         DP    ANSWER,TOTBLHRS          /BILLABLE HOURS                         
         SRP   ANSWER(7),64-1,5         BILLED VS VALUE %                       
         LA    R3,BVVPER                                                        
         BAS   RE,EDITPCT                                                       
*                                                                               
* BILLED VS STAND COL         BILLED HRS - STANDARD HRS                         
*                                                                               
ACTOT120 ZAP   ANSWER,TOTHRSBL          BILLED HRS - STANDARD HRS               
         SP    ANSWER,TOTHRXPC          HOURS BILLED                            
         LA    R2,ANSWER+7              - STANDARD HOURS X TARG PCT             
         LA    R3,BVSHR                                                         
         BAS   RE,EDITHR                                                        
*                                                                               
         ZAP   ANSWER,TOTBILLD          BILLED $ - STANDARD $                   
         SP    ANSWER,TOTRATE           $ BILLED                                
         LA    R2,ANSWER+7              - STANDARD RATE X HR $                  
         LA    R3,BVSDOL                                                        
         BAS   RE,EDITAMT                                                       
*                                                                               
         CP    TOTHRXPC,=P'0'           BILLED HRS/STANDARD HRS                 
         BE    ACTOT130                 STAND HOURS X TARG PCT ZERO?            
         CP    TOTHRSBL,=P'0'                                                   
         BE    ACTOT130                                                         
*                                                                               
         ZAP   ANSWER,TOTHRSBL          HOURS BILLED                            
         MP    ANSWER,=P'10000'                                                 
         DP    ANSWER,TOTHRXPC                                                  
         SRP   ANSWER(7),64-1,5         BILLED VS STANDARD(%)                   
         LA    R3,BVSPER                                                        
         BAS   RE,EDITPCT                                                       
*                                                                               
ACTOT130 BAS   RE,CLRBK            CLEAR PRINT BLOCK                            
         BAS   RE,BLDBK            BUILD PRINT BLOCK                            
         OC    PRNTBLOC(165),XSPACES                                            
         OC    PRNTBLOC+165(165),XSPACES                                        
*                                                                               
         MVC   PRNTBLOC+1(9),=C'STD HRS  '                                      
         MVC   PRNTBLOC+12(9),=C'STD VALUE'                                     
         MVC   PRNTBLOC+27(5),=C'TRG %'                                         
         MVC   PRNTBLOC+35(7),=C'TOT HRS  '                                     
*                                                                               
         MVC   PRNTBLOC+166(9),STDHR                                            
         MVC   PRNTBLOC+177(13),STDVALUE                                        
         MVC   PRNTBLOC+192(6),TRGT                                             
         MVC   PRNTBLOC+200(9),ACTHR                                            
*                                                                               
         LA    R2,XP                                                            
         CLC   PRN2BLOC,XSPACES                                                 
         BE    ACTOT140                                                         
         MVC   0(L'PRN2BLOC,R2),PRN2BLOC                                        
         LA    R2,L'XP(R2)                                                      
*                                                                               
ACTOT140 MVC   0(L'PRNTBLOC,R2),PRNTBLOC                                        
         LA    R2,L'XP(R2)                                                      
         LA    R1,L'PRNTBLOC                                                    
         CLI   QOPT10,C'N'         ARE WE ALLOWING DOLLARS TO SHOW?             
         BNE   *+8                                                              
         LA    R1,50               INCLUDE STD HRS INFO BUT NOT $               
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R2),PRNTBLOC+165                                             
         GOTO1 ACREPORT                 SKIP A LINE                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,LINE                  PRESENT LINE                            
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         IC    RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   *+8                                                              
         BAS   RE,HEADUP               HEAD UP NEW PAGE                         
*                                                                               
         CLI   TOTSW,TOTEMP             EMPLOYEE LEVEL TOTAL                    
         BNE   ACTOT150                 NO - CONTINUE                           
         CLI   QOPT1,C'E'               PRINT EMPLOYEE AS DETAIL LINE           
         BE    *+8                      YES - DONT SKIP LINES                   
ACTOT150 MVI   SPACING,2                                                        
         GOTO1 ACREPORT                AND PRINT IT                             
*                                                                               
ACTOT160 ZAP   0(7,R5),=P'0'           CLEAR LEVEL ACCUMS                       
         LA    R5,7(R5)                                                         
         BCT   R6,ACTOT160                                                      
*                                                                               
         MVI   TOTSW,TOTEMP            RESET TO EMPLOYEE TOTAL                  
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
*  BUILD COLUMN HEADINGS BY USING TABLES                             *          
*  COLUMN WIDTH = 17 ( 15 + BOX EACH SIDE                            *          
*  MAX COLUMNS = 6                                                   *          
**********************************************************************          
         SPACE 1                                                                
BLDCOLS  NTR1                                                                   
         ZAP   COLSWIT,=P'0'                COLUMN SWITCH                       
         LA    R2,OPTION3                   PT TO OPTION3 TABLE                 
         LA    R5,HEADBLOC                                                      
         MVC   HEADBLOC(130),SPACES         XHEAD10                             
         MVC   HEADBLOC+130(130),SPACES     XHEAD11                             
         MVC   HEADBLOC+260(130),SPACES     XHEAD12                             
         LA    R1,OPTLNQ                    # FORMATS AVAILABLE                 
BLD10    CLC   QOPT3(1),0(R2)               MATCH?                              
         BE    BLD20                        YES                                 
         LA    R2,TLENGTH(R2)               NEXT OPTION                         
         BCT   R1,BLD10                                                         
         DC    H'0'                                                             
*                                                                               
         USING COLUMND,R4                                                       
BLD20    LA    R4,COLUMN                                                        
         LA    R2,1(R2)                                                         
*                                                                               
BLD25    CLI   0(R2),ENDOFROW              END OF LINE?                         
         BE    EXIT                        YES                                  
*                                                                               
         CLI   0(R2),ENDOFCOL               END OF COLUMN                       
         BNE   BLD26                                                            
         ZAP   COLSWIT,=P'0'               NEW COLUMN                           
         B     BLD20                                                            
*                                                                               
BLD26    CP    COLSWIT,=P'0'                                                    
         BNE   BLD20                                                            
         CLI   0(R2),SKIP                   PRINT COLUMN?                       
         BE    BLD20                        NO                                  
         CLI   0(R2),BLANK                  SKIP COLUMN?                        
         BE    BLD45                                                            
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
*                                                                               
BLD27    SR    R0,R0                                                            
         IC    R0,COLNUM                    GET # OF COL                        
         CR    R1,R0                                                            
         BE    BLD30                                                            
         LA    R4,8(R4)                     NEXT ROW OF COL TABLE               
         B     BLD27                                                            
*                                                                               
BLD30    L     R6,COLNAME                   LOAD IN ADD OF HEADING              
         MVC   0(15,R5),0(R6)               LINE 1 OF HEAD                      
         MVC   130(15,R5),15(R6)            LINE 2 OF HEAD                      
         MVC   260(15,R5),30(R6)            LINE 3 OF HEAD                      
         AP    COLSWIT,=P'1'                SET COLUMN SWITCH                   
*                                                                               
BLD40    LA    R5,17(R5)                    BUMP TO NEXT COL                    
BLD45    SR    R1,R1                                                            
         IC    R1,NUMCOLS                   KEEP TRACK OF NUM OF COL            
         LA    R1,1(R1)                                                         
         STC   R1,NUMCOLS                                                       
         CH    R1,=H'6'                     MAX COL ACROSS IS 6                 
         BE    EXIT                                                             
         B     BLD20                                                            
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT HEADLINES                                                    *          
**********************************************************************          
         SPACE 1                                                                
HEADUP   NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   XHEAD3+26(36),COMPNAM          COMPNAY NAME                      
         MVC   XHEAD4+17(2),OFCODE            1R OFFICE CODE                    
         MVC   XHEAD4+26(36),OFNAME                     NAME                    
         MVC   XHEAD7+124(34),BILLTHRU        BILLED THRU HEADLINE              
*                                                                               
         MVC   XHEAD10+63(102),HEADBLOC          COLUMN HEADING 1               
         MVC   XHEAD11+63(102),HEADBLOC+130      COLUMN HEADING 2               
         MVC   XHEAD12+63(102),HEADBLOC+260      COLUMN HEADING 3               
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* NAME LOOKUP                                                        *          
**********************************************************************          
         SPACE 1                                                                
         USING NAMELD,R4                                                        
GETNME   NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,NAMELQ              X'20' - NAME ELEMENT                  
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                       NO X'20'EL -SOMETHINGS WRONG          
         MVC   WORK(36),SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   WORK(0),NAMEREC                                                  
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PUT RECORD TO SORT                                                 *          
**********************************************************************          
         SPACE 1                                                                
PUTSRT   NTR1                             ** PUT RECORD TO SORT **              
         USING SRTD,R6                                                          
         LA    R6,SRTWRK                                                        
         LR    R1,R6                         ADDR OF SORT REC INTO R1           
         LA    R1,SBUKLOC(R1)                START OF COL BUCKETS               
         LA    R0,SBUKCONT                   NUMBER OF BUCKETS INTO R0          
*                                                                               
PUTS10   CP    0(7,R1),=P'0'                 CONTAIN PACKED ZEROS               
         BNE   PUTS20                        NOT ZERO-WE WANT IT                
         LA    R1,7(R1)                      BUMP TO NEXT BUCKET                
         BCT   R0,PUTS10                                                        
         B     EXIT                                                             
*                                                                               
PUTS20   CLI   QOPT2,C'S'          1R EMP,DEPT/CAT SUPPRESSED                   
         BNE   PUTS30              NO - CHECK NEXT LEVEL                        
         LA    R2,SRTEMPL          R2 = A(FULL ACCOUNT)                         
         SR    R1,R1                                                            
         IC    R1,LEVC             LENGTH OF OFF+DEPT+CAT                       
         SR    R0,R0                                                            
         IC    R0,LEVLNQA          LENGTH OF LEVEL A INDIVIDUALLY               
         AR    R2,R0               BUMP R2 PAST OFFICE                          
         SR    R1,R0               R1 = LENGTH OF DEPT + CAT                    
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R2),SPACES      CLEAR DEPT/CAT CODE                          
*                                                                               
PUTS30   CLI   QOPT1,C' '          SJ JOB LEVEL REPORT                          
         BE    PUTS50                        YES - PUT TO SORT                  
         CLI   QOPT1,C'P'                    PRODUCT LEVEL REPORT               
         BNE   *+14                                                             
         MVC   SRTJOB,SPACES                                                    
         B     PUTS50                                                           
*                                                                               
         CLI   QOPT1,C'C'                    CLIENT LEVEL REPORT                
         BNE   *+14                                                             
         MVC   SRTPROD(9),SPACES                                                
         B     PUTS50                                                           
*                                                                               
         CLI   QOPT1,C'E'                    EMPLOYEE LEVEL REPORT              
         BNE   PUTS50                                                           
         MVC   SRTSJ,SPACES                                                     
*                                                                               
PUTS50   OC    ALSORT,ALSORT       IS SORTER OPENED                             
         BNZ   PUTSX               YES                                          
*                                                                               
         LA    R1,SRTDLNQ                    SORT KEY LENGTH                    
         CVD   R1,DUB                        CONVERT KEY LEN TO CHARS           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
*                                                                               
         LA    R1,SRTLNQ                     SORT RECORD LENGTH                 
         CVD   R1,DUB                        CONVERT REC LEN TO CHARS           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
*                                                                               
PUTSX    GOTO1 ADSORTER,DMCB,=C'PUT',(R6)                                       
         MVI   ALSORT,1                      ACTIVITY SWITCH                    
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CLEAR SORT RECORD AREA                                             *          
**********************************************************************          
         SPACE 1                                                                
CLRSRT   NTR1                                                                   
         USING SRTD,R6                                                          
         LA    R6,SRTWRK                                                        
         MVC   SRTWRK(SRTLNQ),XSPACES                                           
         XC    SRTHIRE,SRTHIRE                                                  
         MVC   SRTFIRE,=X'FFFFFF'                                               
         LR    R1,R6                         ADDR OF SORT REC INTO R1           
         LA    R1,SBUKLOC(R1)                START OF COL BUCKETS               
         LA    R0,SBUKCONT                   NUMBER OF BUCKETS INTO R0          
*                                                                               
CLER03   ZAP   0(7,R1),=P'0'                 CLEAR TO PACKED ZEROS              
         LA    R1,7(R1)                      BUMP TO NEXT BUCKET                
         BCT   R0,CLER03                                                        
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CHECK AND SEE IF CLIENT CODE IS IN IN NEW BUSINESS/PRO BONO TABLE  *          
**********************************************************************          
         SPACE 1                                                                
CHKCLI   NTR1                                                                   
         XC    CLISTAT,CLISTAT     CLEAR CLIENT STATUS FIELD                    
         USING BIND,R5                                                          
         L     R5,ACLITAB          R5 = CLIENT TABLE                            
         SR    R0,R0                                                            
         ICM   R0,15,BININ                                                      
         BZ    CHKCX               NOTHING IN TABLE EXIT                        
         USING CLITBLD,R3                                                       
         LA    R3,BINTAB                                                        
*                                                                               
CHKC10   CLC   CLICLI(CLILNQ),WORK COMPARE ON CLIENT CODE                       
         BNE   CHKC20                                                           
         OI    CLISTAT,CLINBPB     SET NEW BUSINESS/PRO BONO SWITCH             
         B     CHKCX                                                            
*                                                                               
CHKC20   LA    R3,CLILNQ(R3)       BUMP TO NEXT RECORD                          
         BCT   R0,CHKC10                                                        
*                                                                               
CHKCX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* CLEAR PRINTBLOCK                                                   *          
**********************************************************************          
         SPACE 1                                                                
CLRBK    NTR1                                                                   
         LA    R1,PRNTBLOC                                                      
         LA    RE,2                                                             
         MVC   0(165,R1),XSPACES                                                
         LA    R1,165(R1)                                                       
         BCT   RE,*-10                                                          
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT HIREDATE, FIRE DATE AT 0(R2)                                 *          
**********************************************************************          
         SPACE 1                                                                
HIREFIRE NTR1                                                                   
         CLI   FIREDATE,X'FF'                                                   
         BE    HFX                                                              
         MVC   0(5,R2),=C'TERM='                                                
         GOTO1 DATCON,DMCB,(1,FIREDATE),(5,5(R2))                               
HFX      B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* EDIT ROUTINES                                                      *          
**********************************************************************          
         SPACE 1                                                                
EDITPCT  NTR1                                                                   
         LA    R2,ANSWER                                                        
         EDIT  (P7,(R2)),(6,(R3)),1,TRAIL=C'%'                                  
         B     EXIT                                                             
*                                                                               
EDITHR   NTR1                                                                   
         EDIT  (P7,(R2)),(9,(R3)),2,MINUS=YES                                   
         B     EXIT                                                             
*                                                                               
EDITAMT  NTR1                                                                   
         EDIT  (P7,(R2)),(13,(R3)),2,MINUS=YES,FLOAT=$                          
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* READBUD, IF A BUDGET IS DEFINED AT THIS MODE, READ IT              *          
**********************************************************************          
         SPACE 1                                                                
         USING BUDD,R2                                                          
READBUD  NTR1  ,                                                                
         L     R4,ADACC                                                         
         LA    R2,BUSTDHRS                                                      
         CLC   BUDMODE,MODE                                                     
         BNE   READB30                                                          
         GOTO1 =A(GETBUD),DMCB,(RC),(R4),PROFILES+2,2,1    STD HRS              
*                                                                               
READB30  EQU   *                                                                
         LA    R2,BUTRGPCT                                                      
         ZAP   BUDAMNT,=P'0'      SAVE TARGET PCT FROM GETBUD                   
         ZAP   TARGET,=P'0'                                                     
*                                                                               
         CLI   PROFILES,0          NO TRG BUD DEFINED                           
         BNE   READB40                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,PROFILES+4       USE DEFAULT TRG PCT FROM PROFILE             
         CVD   R1,DUB                                                           
         ZAP   BUDGAMT,DUB                                                      
         MP    BUDGAMT,=P'100'                                                  
         OI    BUDSTAT,NOBUD                                                    
         B     READB45                                                          
*                                                                               
READB40  CLC   BUDMODE,MODE                                                     
         BNE   READB50                                                          
*                                                                               
         GOTO1 =A(GETBUD),DMCB,(RC),(R4),PROFILES+0,0,1 TARGET                  
*                                                                               
READB45  ZAP   BUDAMNT,BUDGAMT    SAVE TARGET PCT FROM GETBUD                   
         ZAP   TARGET,BUDAMNT                                                   
*                                                                               
READB50  EQU   *                                                                
         CLI   MODE,PROCACC        READ STD RATES AT EMPLOYEE                   
         BNE   READBX              LEVEL, BUT PASS GETBUD THE CORRECT           
         LA    R2,BUSTDRAT         KEY                                          
         L     R4,ADLEDGER                                                      
         CLI   BUDMODE,LEDGFRST                                                 
         BE    READB70                                                          
         L     R4,ADHEIRA                                                       
         CLI   BUDMODE,PROCLEVA                                                 
         BE    READB70                                                          
         L     R4,ADHEIRB                                                       
         CLI   BUDMODE,PROCLEVB                                                 
         BE    READB70                                                          
         L     R4,ADHEIRC                                                       
         CLI   BUDMODE,PROCLEVC                                                 
         BE    READB70                                                          
         L     R4,ADACC                                                         
         CLI   BUDMODE,PROCACC                                                  
         BNE   READBX                                                           
*                                                                               
READB70  GOTO1 =A(GETBUD),DMCB,(RC),(R4),PROFILES+1,1,1 RATE                    
         ZAP   BUDAMNT,BUDGAMT    SAVE STD RATE FROM GETBUD                     
*                                                                               
READBX   L     RE,LASTIO                                                        
         MVC   SVKEY,0(RE)         RE-READ THE LAST ACCOUNT                     
         GOTO1 =A(DMREADDR),DMCB,(RC)                                           
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LOCAL STORAGE                                                      *          
**********************************************************************          
         SPACE 1                                                                
BILLTHRU DS    CL38                HEADLINE TO PRINT MOA RANGE                  
COMPNAM  DS    CL36                COMPANY NAME FOR HEADLINES                   
*                                                                               
PL16     DS    PL16                                                             
ANSWER   DS    PL14                FOR DIVIDE                                   
*                                                                               
SRTLVCDS DS    0CL12               SORT EMPLOYEE'S INDIVIDUAL LEVELS            
SRTOFFC  DS    CL12                         -OFFICE                             
SRTDEPT  DS    CL12                         -DEPARTMENT                         
SRTCAT   DS    CL12                         -SUB-DEPT (CATGORY)                 
SRTSTAF  DS    CL12                         -STAFF NUMBER                       
SRTLVLNQ EQU   *-SRTLVCDS                                                       
*                                                                               
EMPLOYEE DS    0CL15               1R EMPLOYEE                                  
         DS    CL3                 CO/U/L                                       
EMPCODE  DS    CL12                EMPLOYEE ACCOUNT CODE                        
EMPLVCDS DS    0CL12               EMPLOYEE'S INDIVIDUAL LEVELS                 
EMPOFFC  DS    CL12                         -OFFICE                             
EMPDEPT  DS    CL12                         -DEPARTMENT                         
EMPCAT   DS    CL12                         -SUB-DEPT (CATGORY)                 
EMPSTAF  DS    CL12                         -STAFF NUMBER                       
EMPLVLNQ EQU   *-EMPLVCDS                                                       
*                                                                               
PRNTHED  DS    CL6                                                              
*                                                                               
SJACCT   DS    0CL15               SJ ACCOUNT                                   
         DS    CL3                 CO/U/L                                       
SJCODE   DS    0CL12                                                            
SJCLIENT DS    CL3                          -CLIENT                             
SJPROD   DS    CL3                          -PRODUCT                            
SJJOB    DS    CL6                          -JOB                                
*                                                                               
CLISTAT  DS    XL1                 CLIENT STATUS BYTE                           
CLINBPB  EQU   X'80'               CLIENT IS NEW BUSINESS OR PRO BONO           
*                                                                               
CLIWRK   DS    CL(CLILNQ)          CLIENT TABLE WORK AREA                       
BILLED   DS    CL1                 FULLY BILLED SWITCH                          
*                                                                               
*                                  EMPLOYEE MONTHLY CLIENT HOURS TABLE          
*                                  2 YM PACKED, PL6 HRS                         
HRSLIST  DS    CL192                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
RELOTAB  DS    0A                                                               
         DC    A(IO)               IO AREA                                      
         DC    A(CLITAB)           CLIENT TABLE OF NEW BUS/PRO BONO             
         DC    V(CONVMOS)                                                       
         DC    V(PRINT)            PRINT FOR PRNTBL                             
         DC    V(PRNTBL)           PRINTABLE                                    
         DC    V(UNDERLIN)                                                      
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
*                                                                               
OVRHED   DC    C'999999999999'     OVERHEAD FOR LEVELS                          
         EJECT                                                                  
**********************************************************************          
* PRINT FIELD EQUATES                                                *          
**********************************************************************          
         SPACE 1                                                                
*   POSSIBLE COLUMN COMBINATIONS AVAILABLE                                      
*                                                                               
IBILHR   EQU   1                    BILLABLE HOURS                              
IBILSTHR EQU   2                    BILLABLE/STANDARD HRS                       
IBILDOL  EQU   3                    BILLABLE DOLLARS                            
IBILDHR  EQU   4                    HOURS BILLED                                
IBILDDOL EQU   5                    DOLLARS BILLED                              
IWOHR    EQU   6                    WRITE-OFF HOURS                             
IWODOL   EQU   7                    WRITE-OFF DOLLARS                           
IHELDHR  EQU   8                    TIME HELD HOURS                             
IHELDDOL EQU   9                    TIME HELD DOLLARS                           
IBVVHR   EQU  10                    BILLED VS VALUE HOURS                       
IBVVPER  EQU  11                    BILLED VS VALUE PERCENT                     
IBVVDOL  EQU  12                    BILLED BS VALUE DOLLARS                     
IBVSHR   EQU  13                    BILLED VS STAND HOURS                       
IBVSPER  EQU  14                    BILLED VS STAND PERCENT                     
IBVSDOL  EQU  15                    BILLED BS STAND DOLLARS                     
ENDOFCOL EQU  77                    END OF COLUMN                               
SKIP     EQU  88                    SKIP COLUMN                                 
BLANK    EQU  99                    BLANK COLUMN                                
ENDOFROW EQU  X'FF'                 ENDOFROW                                    
         EJECT                                                                  
**********************************************************************          
* PRINT COLUMN TABLE - CONVERTS WHAT OPT3 INPUT MEANS                *          
**********************************************************************          
         SPACE 1                                                                
* THIS TABLE CONTAINS WHAT THE COLUMNS OF THE PRINTED REPORT                    
* WILL BE FOR EACH OPTION3 INPUT.                                               
* EACH ENTRY LINE SHOWS WHAT WILL PRINT ON THE 2 AVAILABLE                      
* LINES.   FIRST BYTE OF EACH LINE IS THE INPUT IN QOPT3.                       
*                                                                               
OPTION3  DC    C' '                    OPTION LEFT BLANK                        
         DC    AL1(IBILHR),AL1(IBILSTHR),AL1(ENDOFCOL)                          
         DC    AL1(IBILDHR),AL1(ENDOFCOL)                                       
         DC    AL1(IWOHR),AL1(ENDOFCOL)                                         
         DC    AL1(IHELDHR),AL1(ENDOFCOL)                                       
         DC    AL1(IBVVHR),AL1(IBVVPER),AL1(ENDOFCOL)                           
         DC    AL1(IBVSHR),AL1(IBVSPER),AL1(ENDOFROW)    16                     
*                                                                               
         DC    AL1(IBILDOL),AL1(ENDOFCOL)                                       
         DC    AL1(IBILDDOL),AL1(ENDOFCOL)                                      
         DC    AL1(IWODOL),AL1(ENDOFCOL)                                        
         DC    AL1(IHELDDOL),AL1(ENDOFCOL)                                      
         DC    AL1(IBVVDOL),AL1(ENDOFCOL)                                       
         DC    AL1(IBVSDOL),AL1(SKIP),AL1(ENDOFROW)     14                      
TLENGTH  EQU   (*-OPTION3)                                                      
         EJECT                                                                  
**********************************************************************          
* SUPPRESS  WRITE-OFFS, TIME HELD                                    *          
**********************************************************************          
         SPACE 1                                                                
         DC    C'1'                    QOPT3=1                                  
         DC    AL1(IBILHR),AL1(IBILSTHR),AL1(ENDOFCOL)                          
         DC    AL1(IBILDHR),AL1(ENDOFCOL)                                       
         DC    AL1(SKIP),AL1(ENDOFCOL)                                          
         DC    AL1(SKIP),AL1(SKIP),AL1(ENDOFCOL)                                
         DC    AL1(IBVVHR),AL1(IBVVPER),AL1(ENDOFCOL)                           
         DC    AL1(IBVSHR),AL1(IBVSPER),AL1(ENDOFROW)        16                 
*                                                                               
         DC    AL1(IBILDOL),AL1(ENDOFCOL)                                       
         DC    AL1(IBILDDOL),AL1(ENDOFCOL)                                      
         DC    AL1(SKIP),AL1(ENDOFCOL)                                          
         DC    AL1(SKIP),AL1(ENDOFCOL)                                          
         DC    AL1(IBVVDOL),AL1(ENDOFCOL)                                       
         DC    AL1(IBVSDOL),AL1(SKIP),AL1(ENDOFROW)         14                  
OPTLNQ   EQU   (*-OPTION3)/(TLENGTH)                                            
         EJECT                                                                  
**********************************************************************          
* THIS TABLE CONTAINS ALL DATA FOR A SPECIFIC COL                    *          
**********************************************************************          
         SPACE 1                                                                
* PARAMETER 1 = COLUMN TO PROCESS                                               
* PARAMETER 2 = OFFSET OF COLUMN AMT IS LOCATED IN AMNTTAB                      
* PARAMETER 3 = LENGTH OF OUTPUT FIELD                                          
* PARAMETER 4 = SPARE                                                           
* PARAMETER 5 = ADDRESS OF COLUMN HEADING NAME                                  
*                                                                               
         DS    0F                                                               
COLUMN   DC    AL1(IBILHR),AL1(0),AL1(9),AL1(0),A(BILHEAD)                      
CLENGTH  EQU   (*-COLUMN)                                                       
         DC    AL1(IBILSTHR),AL1(9),AL1(6),AL1(0),A(BILHEAD)                    
         DC    AL1(IBILDHR),AL1(15),AL1(9),AL1(0),A(BILDHEAD)                   
         DC    AL1(IWOHR),AL1(24),AL1(9),AL1(0),A(WOHEAD)                       
         DC    AL1(IHELDHR),AL1(33),AL1(9),AL1(0),A(HELDHEAD)                   
         DC    AL1(IBVVHR),AL1(42),AL1(9),AL1(0),A(BVVHEAD)                     
         DC    AL1(IBVVPER),AL1(51),AL1(6),AL1(0),A(BVVHEAD)                    
         DC    AL1(IBVSHR),AL1(57),AL1(9),AL1(0),A(BVSHEAD)                     
         DC    AL1(IBVSPER),AL1(66),AL1(6),AL1(0),A(BVSHEAD)                    
         DC    AL1(IBILDOL),AL1(72),AL1(13),AL1(0),A(BILHEAD)                   
         DC    AL1(IBILDDOL),AL1(85),AL1(13),AL1(0),A(BILDHEAD)                 
         DC    AL1(IWODOL),AL1(98),AL1(13),AL1(0),A(WOHEAD)                     
         DC    AL1(IHELDDOL),AL1(111),AL1(13),AL1(0),A(HELDHEAD)                
         DC    AL1(IBVVDOL),AL1(124),AL1(13),AL1(0),A(BVVHEAD)                  
         DC    AL1(IBVSDOL),AL1(137),AL1(13),AL1(0),A(BVSHEAD)                  
COLEND   EQU   (*-COLUMN)/(CLENGTH)                                             
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
**********************************************************************          
* GETEL # 2                                                          *          
**********************************************************************          
         SPACE 1                                                                
         GETELN R4,DISP2,ELCODE,2                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* HEADINGS FOR PRINT COLUMNS                                         *          
**********************************************************************          
         SPACE 1                                                                
BILHEAD  DC    CL15'BILLABLE VALUE '      BILLABLE COLUMN                       
         DC    CL15'HOURS     %    '                                            
         DC    CL15'  VALUE IN $   '                                            
BILDHEAD DC    CL15' TIME BILLED   '      BILLED COLUMN                         
         DC    CL15'HOURS          '                                            
         DC    CL15'  VALUE IN $   '                                            
WOHEAD   DC    CL15'TIME WRITE-OFF '      WRITE OFF COLUMN                      
         DC    CL15'HOURS          '                                            
         DC    CL15'  VALUE IN $   '                                            
HELDHEAD DC    CL15'  TIME HELD    '      TIME HELD COLUMN                      
         DC    CL15'HOURS          '                                            
         DC    CL15'  VALUE IN $   '                                            
BVVHEAD  DC    CL15'BILLED VS VALUE'      BILLED VS VALUE COLUMN                
         DC    CL15' OVER (UNDER)  '                                            
         DC    CL15'HOURS $   %    '                                            
BVSHEAD  DC    CL15'BILLED VS STAND'      BILLED VS STANDARD COL                
         DC    CL15' OVER (UNDER)  '                                            
         DC    CL15'HOURS $   %    '                                            
         EJECT                                                                  
**********************************************************************          
* BUFFER AND TABLES                                                  *          
**********************************************************************          
         SPACE 1                                                                
         DS    0F                                                               
MMDAYS   DC    X'01',PL2'31'                                                    
         DC    X'02',PL2'28'                                                    
         DC    X'03',PL2'31'                                                    
         DC    X'04',PL2'30'                                                    
         DC    X'05',PL2'31'                                                    
         DC    X'06',PL2'30'                                                    
         DC    X'07',PL2'31'                                                    
         DC    X'08',PL2'31'                                                    
         DC    X'09',PL2'30'                                                    
         DC    X'10',PL2'31'                                                    
         DC    X'11',PL2'30'                                                    
         DC    X'12',PL2'31'                                                    
         DC    X'FF'                                                            
*                                                                               
IO       DS    0C                                                               
IOKEY    DS    CL49                                                             
IOAREA   DS    CL2000                                                           
IOLNQ    EQU   *-IO                                                             
*                                                                               
* CLIENT TABLE OF NEW BUSINESS/PRO BONO ACCOUNTS                                
*                                                                               
CLITAB   DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(CLILNQ)         LENGTH OF ENTRY                              
         DC    AL1(0)              DISP. TO KEY                                 
         DC    AL3(CLILNQ)         KEY LENGTH                                   
         DC    AL4(CLIMAX)         MAX IN TABLE                                 
         DC    AL1(0)              NUMBER OF BUCKETS - NO BUCKETS               
         DC    AL1(0)              DISPLACEMENT TO FIRST BUCKET                 
CLIHDLN  EQU   *-CLITAB            CLIENT TABLE HEADER LENGTH                   
         DS    (CLIMAX*CLILNQ)XL1  TABLE                                        
*                                                                               
CLISIZE  EQU   (CLIMAX*CLILNQ)+CLIHDLN                                          
CLIMAX   EQU   250                                                              
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  DS    0D                                                               
         NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCDIR ',SVKEY,AIO,0                      
         B     DMX                                                              
*                                                                               
DMHIGHDR DS    0D                                                               
         NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCDIR ',SVKEY,AIO,0                      
         B     DMX                                                              
*                                                                               
DMREADDR DS    0D                                                               
         NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCDIR ',SVKEY,AIO,0                      
         CLI   8(R1),0             EXIT WITH CC SET                             
         B     DMX                                                              
*                                                                               
DMGETREC DS    0D                                                               
         NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         L     R3,AIO                                                           
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',ACCKDA,AIO,DMWORK                 
DMX      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* ADD ITEM TO BINSRCH TABLE                                          *          
*     P1 - A(ITEM TO BE ADDED)                                       *          
*     P2 - A(TABLE)                                                  *          
**********************************************************************          
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   DS    0D                                                               
         NMOD1 0,*BIN*                                                          
         L     RC,0(R1)            RESET RC                                     
         L     R5,8(R1)                                                         
         MVC   DMCB+8(16),BININ              NUMBER LENGTH,KEY,MAX              
         LA    R6,BINTAB                     A(TABLE)                           
         L     R4,4(R1)                      A(ITEM)                            
         GOTO1 BINSRCH,DMCB,(X'01',(R4)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                          TABLE IS FULL                      
         MVC   BININ,DMCB+8                  UPDATE COUNT                       
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* GETBUD - READS BUDGET RECORD  AT EMPLOYEE LEVEL OF 1R              *          
*   GETB00           -    VALIDATES BUDGETS                          *          
*   GETB05-GETB35    -    BUILD STD HRS TABLE FOR OFFICE             *          
*   GETB50-GETB115   -    TARGET AND RATE BUDGETS                    *          
**********************************************************************          
*          PASS PARAMETER                                            *          
*               PARAMETER 1 = U/L ACCOUNT CODE FOR 1R RECORD         *          
*               PARAMETER 2 = BUDGET NUMBER                          *          
*  BUDTP        PARAMETER 3 = 0 = TARGET,  1 = STD RATE, 2 = STD HRS *          
*               PARAMETER 4 = VALIDATION (0), NON-VALIDATION (<> 0)  *          
*          PASS BACK - CONDITION CODE (GOOD = 0, BAD = NOT 0)        *          
*               BUDGAMT OR STANDARD RATE                             *          
* NOTE - FOR STD HRS GO RIGHT FROM VALIDATION TO COMPUTING STD HRS   *          
*        TARGET BUDGET = SUM OF 1D ELEMENTS                          *          
*        RATE   BUDGET = RATE FOR MONTH.  IF NO RATE FOR MONTH       *          
*                        CASCADE BACK TO PREVIOUS MONTH OR FURTHER   *          
*                        UNTIL RATE IS FOUND.  IF NO RATE IS FOUND   *          
*                        RATE BECOMES ZERO                           *          
**********************************************************************          
GETBUD   DS    0D                                                               
         NMOD1 0,*GETBUD                                                        
         L     RC,0(R1)                LOAD BACK RC                             
         L     R5,4(R1)                P1 = 1R KEY    - R2                      
         L     R2,8(R1)                P2 = BUDGET #  - R4                      
         MVC   BUDTP,15(R1)            P3 = BUDGET TYPE  (0,1,2)                
*                                                                               
* BUILD STD HRS BY MONTH TABLE                                                  
*        MONTHS IN TABLE ARE SET IN REQFRST                                     
*                                                                               
GETB05   CLI   BUDTP,2                IS IT STD HRS BUDGET?                     
         BNE   GETB50                 NO                                        
*                                                                               
         LA    R1,STDLIST             CLEAR OLD STD HRS FROM TABLE              
         LA    R0,24                                                            
GETB11   MVI   0(R1),X'40'                                                      
         ZAP   7(4,R1),=P'0'                                                    
         LA    R1,11(R1)                                                        
         BCT   R0,GETB11                                                        
*                                                                               
         USING BUDRECD,R4                                                       
         LA    R4,SVKEY                                                         
         MVC   BUDKEY(ACCORFST),SPACES   CLEAR KEY                              
         MVI   BUDKTYP,BUDKTYPQ          BUDGET RECORD                          
         MVC   BUDKCULA(4),0(R5)         COMP/UNIT/LEDGER/OFFICE                
         MVC   BUDKWORK,SPACES                                                  
         MVC   BUDKCCPY,SPACES                                                  
         MVC   BUDKCCPY(1),QCOMPANY        COMPANY                              
         MVC   BUDKCCPY+1(2),=C'1C'        1C CONTRA                            
         XC    BUDKBUDN(9),BUDKBUDN        CLEAR W/ BINARY ZEROES               
         MVC   BUDKBUDN+1(1),0(R2)         BUDGET #                             
         GOTO1 =A(DMHIGHDR),DMCB,(RC)    READ HIGH                              
         CLC   SVKEY((BUDKBUDN-BUDKEY)+L'BUDKBUDN),IOKEY                        
         BE    GETB15                  NO MATCH, BUDGET IS INCOMPLETE           
*                                                                               
         CLI   BUDTP,0             TRGPCT                                       
         BNE   *+8                 NO                                           
         OI    BUDSTAT,NOBUD       SET TARGET PCT NOT FOUND                     
         B     GETBX                                                            
*                                                                               
* GET X'1D' ELEMENT                                                             
*                                                                               
GETB15   GOTO1 =A(DMGETREC),DMCB,(RC)    GET RECORD                             
         USING BAMELD,R6                                                        
         L     R6,AIO                                                           
         MVC   OFFSWIT(4),0(R6)        SAVE COMP/UNIT/LED/OFFICE                
         AH    R6,DISP2                                                         
*                                                                               
GETB20   CLI   0(R6),0                 IS IT END OF ELEMENTS?                   
         BE    GETBX                   YES - NEXT BUDGET                        
         CLI   BAMEL,BAMELQ            X'1D' - BUDGET AMOUNT ELEMENT            
         BE    GETB25                  YES                                      
*                                                                               
GETB22   SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     GETB20                  GET NEXT ELEMENT                         
*                                                                               
GETB25   CLC   BAMMNTH,START            LOWER THAN START YYMM                   
         BL    GETB22                       YES - GO GET NEXT X'1D'             
         CLC   BAMMNTH,ENDATE           HIGHER THAN END YYMM                    
         BH    GETB22                       YES - GO GET NEXT X'1D'             
*                                                                               
         LA    R1,STDLIST                                                       
         LA    R0,24                     MAX # OF MONTHS                        
GETB30   CLC   1(2,R1),BAMMNTH           DOES YM MATCH?                         
         BE    GETB35                    YES                                    
         LA    R1,11(R1)                 NO - INCREMENT TABLE                   
         BCT   R0,GETB30                 LOOP                                   
         B     GETB22                    NO MATCH IN TABLE                      
*                                                                               
GETB35   AP    7(4,R1),BAMBUDG           ADD STD HOURS                          
         MVC   0(1,R1),BUDKACT           MOVE IN OFFICE CODE                    
         B     GETB22                    GET NEXT ELEMENT                       
         EJECT                                                                  
**********************************************************************          
*  READ LOW LEVEL 1R EMPLOYEE AND RETURN EITHER TARGET BUDGET AMT    *          
*  OR STANDARD VALUE( RATE X STD HRS)                                *          
**********************************************************************          
GETB50   ZAP   BUDGAMT,=P'0'           CLEAR AMOUNT                             
         ZAP   RATEXHR,=P'0'          CLEAR RATE X HOURS                        
*                                                                               
         CLI   BUDTP,1             RATE CALL                                    
         BNE   GETB60              NO, MUST BE TARG PCT                         
*                                                                               
         LA    R0,24                                                            
         LA    R1,STDLIST          INIT TMPLIST WITH  STDLIST                   
         LA    R3,TMPLIST                                                       
GETB55   MVC   0(2,R3),1(R1)               YYMM                                 
         ZAP   2(4,R3),=P'0'               INIT HRS TO ZERO                     
         CLC   0(2,R3),HIREDATE                                                 
         BL    GETB56                                                           
         CLC   0(2,R3),FIREDATE                                                 
         BH    GETB56                                                           
         MVC   2(4,R3),7(R1)               STD HRS PLL4                         
*                                                                               
GETB56   ZAP   6(6,R3),=P'0'               MAKE RATES ZERO PACKED               
         LA    R1,11(R1)                   INCREMENT STD LIST                   
         LA    R3,12(R3)                   INCREMENT TMP LIST                   
         BCT   R0,GETB55                                                        
         MVI   0(R3),X'FF'                 END OF TMPLIST TABLE                 
*                                                                               
         USING BUDRECD,R4                                                       
GETB60   LA    R4,SVKEY                                                         
         XC    BUDKEY(ACCORFST),BUDKEY CLEAR KEY                                
         MVI   BUDKTYP,BUDKTYPQ        BUDGET RECORD                            
         MVC   BUDKCULA,0(R5)          1R ACCOUNT                               
         MVC   BUDKWORK,SPACES                                                  
         MVC   BUDKCCPY,SPACES                                                  
         MVC   BUDKCCPY(1),QCOMPANY        COMPANY                              
         MVC   BUDKCCPY+1(2),=C'1C'        1C CONTRA                            
         MVC   BUDKBUDN+1(1),0(R2)         BUDGET NUMBER                        
         GOTO1 =A(DMHIGHDR),DMCB,(RC)    READ HIGH                              
         CLC   SVKEY((BUDKBUDN-BUDKEY)+L'BUDKBUDN),IOKEY                        
         BE    GETB69                  YES                                      
*                                                                               
         CLI   BUDTP,0             TRGPCT                                       
         BNE   GETBX               NO                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,PROFILES+4       UES DEFAULT TRG PCT FROM PROFILE             
         CVD   R1,DUB                                                           
         ZAP   BUDGAMT,DUB                                                      
         MP    BUDGAMT,=P'100'                                                  
         OI    BUDSTAT,NOBUD                                                    
         B     GETBX                                                            
*                                                                               
GETB69   GOTO1 =A(DMGETREC),DMCB,(RC)    GET RECORD                             
         CLI   BUDTP,0             TRGPCT                                       
         BNE   GETB69A             NO                                           
         OI    BUDSTAT,NOBUD       ASSUME TRG NOT FOUND                         
         SR    R1,R1                                                            
         IC    R1,PROFILES+4       USE DEFAULT TRG PCT FROM PROFILE             
         CVD   R1,DUB                                                           
         ZAP   BUDGAMT,DUB                                                      
         MP    BUDGAMT,=P'100'                                                  
*                                                                               
         USING BAMELD,R6                                                        
GETB69A  L     R6,AIO                                                           
         AH    R6,DISP2                                                         
         SR    R1,R1                                                            
*                                                                               
GETB70   CLI   0(R6),0             ANY ELEMENTS LEFT?                           
         BE    GETB83              NO                                           
         CLI   BAMEL,BAMELQ        X'1D' - BUDGET AMT ELEMENT                   
         BE    GETB85              YES                                          
*                                                                               
GETB80   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                        IN SEACH OF X'1D'                   
         B     GETB70                                                           
*                                                                               
GETB83   CLI   BUDTP,1                   IS IT RATE BUDGET?                     
         BE    GETB150                   YES -                                  
         B     GETBX                     NO - TARGET BUDGET                     
*                                                                               
GETB85   MVC   TRGDTS(6),START           RPT START & END YYMM                   
         CLI   BUDTP,1                   IS IT A RATE BUDGET?                   
         BE    GETB90                    YES                                    
*                                                                               
         MVC   TRGDTS(6),START                                                  
         MVI   TRGDTS+1,1                START MONTHS FOR TARGET                
         MVI   TRGDTE+1,X'12'            END   MONTH  FOR TARGET                
*                                                                               
GETB90   CLC   BAMMNTH,TRGDTS           REPORT START DATE?                      
         BL    GETB80                    EXCLUDE IF BEFORE                      
         CLC   BAMMNTH,TRGDTE           REPORT END DATE?                        
         BH    GETB80                    EXCLUDE IF AFTER                       
*                                                                               
         CLI   BUDTP,0                   IS IT A TARGET BUDGET?                 
         BE    GETB100                   YES                                    
*                                                                               
         CLC   BAMMNTH,HIREDATE         HIRE DATE?                              
         BL    GETB80                    EXCLUDE IF BEFORE                      
         CLI   FIREDATE,X'FF'            NO TERMINATION DATE?                   
         BE    GETB102                   NO PRORATION NEEDED                    
         CLC   BAMMNTH,FIREDATE         FIRE DATE?                              
         BH    GETB80                    NO PRORATION NEEDED                    
         B     GETB102                                                          
*                                                                               
GETB100  CLI   BUDTP,0             TRGPCT                                       
         BNE   GETB101             NO                                           
*                                                                               
         TM    BUDSTAT,NOBUD       DOES BUDGAMNT STILL HAVE THE DEFAULT         
         BNO   GETB101             NO                                           
*                                                                               
         ZAP   BUDGAMT,=P'0'       ZAP OUT DEFAULT                              
         NI    BUDSTAT,X'FF'-NOBUD SET TARGET PCT FOUND                         
GETB101  AP    BUDGAMT,BAMBUDG          SUM TARGET YYMM                         
         B     GETB80                    NEXT 1D ELEMENT                        
*                                                                               
GETB102  LA    R3,TMPLIST                PUT RATE IN TABLE                      
*                                                                               
GETB105  CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    GETB80              YES, GET NEXT 1D EL                          
         CLC   BAMMNTH,0(R3)            IS THERE A MATCH ON YM?                 
         BNE   GETB110                   NO                                     
*                                                                               
         ZAP   6(6,R3),BAMBUDG          ADD RATE TO TABLE                       
         B     GETB80                    GET NEXT 1D                            
*                                                                               
GETB110  LA    R3,12(R3)                INCREMENT TABLE                         
         B     GETB105                                                          
********************************************                                    
*  FILL BLANK FIELDS WITH LAST RATE AMOUNT *                                    
********************************************                                    
GETB150  LA    R3,TMPLIST                                                       
         ZAP   DUB,=P'0'                                                        
         ZAP   MMCNT,=P'0'                                                      
*                                                                               
GETB155  CLI   0(R3),X'FF'               END OF TABLE                           
         BE    GETB170                                                          
*                                                                               
         CP    6(6,R3),=P'0'             IS IT ZERO?                            
         BNE   GETB160                   NO                                     
*                                                                               
         ZAP   6(6,R3),DUB               YES -                                  
         B     GETB165                                                          
*                                                                               
GETB160  ZAP   DUB,6(6,R3)                                                      
*                                                                               
GETB165  LA    R3,12(R3)               INCREMENT TABLE                          
         AP    MMCNT,=P'1'                                                      
         B     GETB155                  NEXT TABLE ITEM                         
*                                                                               
GETB170  ZAP   BUDGAMT,=P'0'            CLEAR BUDGAMT                           
GETBX    XMOD1 1                                                     *          
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* SETBUD - FIND OUT WHAT LEVELS THE USER HAS DEFINED HIS             *          
*          BUDGETS AT SO YOU CAN READ FOR THEM WITH THE CORRECT      *          
*          ACCOUNT (WHEN YOU HAVE TO)                                *          
* BUDGET NUMBER IS AT 0(R2), RETURS MODE TO READ IT AT IN 1(R2)      *          
**********************************************************************          
         SPACE 1                                                                
SETBUD   DS    0D                                                               
         NMOD1 0,SETBUD                                                         
         L     RC,0(R1)           RESTORE REG C                                 
         USING BUDD,R2                                                          
         MVI   BUDMODE,X'FF'       INIT MODE TO INVALID                         
         ZAP   BUDAMNT,=P'0'                                                    
*                                                                               
         USING BUDRECD,R4                                                       
         LA    R4,SVKEY                                                         
         XC    BUDKEY(ACCORFST),BUDKEY   CLEAR KEY                              
         MVI   BUDKTYP,BUDKTYPQ        BUDGET RECORD                            
         MVC   BUDKCPY,QCOMPANY                                                 
         MVC   BUDKNO1+1(1),BUDNUM     BUD NUMBER                               
*                                                                               
* 1/11/93                                                                       
* I DONT KNOW WHY I WOULD HAVE TO CONVERT AN F3 TO A 03, BUT                    
* APPARENTLY I DID.  THIS WAS LIMITING THEM TO BUDS 1-9. I WILL                 
* CHECK FOR A VAL GT 200, THE LIMIT ON THE CTL/PRO, BEFORE I DO                 
* THE NI (X'F0' IS 240 DECIMAL)                                                 
*                                                                               
         SR    RF,RF                                                            
         IC    RF,BUDNUM                                                        
         CH    RF,=H'200'          IS THIS SOMEHOW CHARACTER                    
         BNH   *+8                 NO                                           
         NI    BUDKNO1+1,X'0F'     ISOLATE BINARY VALUE                         
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   READ HIGH                               
         CLC   IOKEY((BUDKNO1-BUDKEY)+L'BUDKNO1),SVKEY                          
         BNE   STBXIT                 CAN'T FIND THIS BUD                       
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         MVI   ELCODE,BIVELQ       X'1C' - GET BUDGET VALID'TN ELEMENT          
         LA    R4,IO                                                            
         AH    R4,DISP2            ADD DISPLACEMENT TO FIRST ELEMENT            
SETB30   BAS   RE,NEXTEL3                                                       
         BNE   STBXIT                                                           
*                                                                               
         USING BIVELD,R4                                                        
         CLC   BIVAUNT(2),=C'1R'                                                
         BNE   SETB30                                                           
         CLC   =C'1C',BIVVCUNT                                                  
         BNE   SETB30                                                           
         CLI   BIVACLV,0                                                        
         BNE   SETB40                                                           
         MVI   BUDMODE,LEDGFRST                                                 
         B     STBXIT                                                           
*                                                                               
SETB40   CLI   BIVACLV,1          SET MODE AT WHICH TO READ THIS BUD            
         BNE   SETB50                                                           
         MVI   BUDMODE,PROCLEVA                                                 
         B     STBXIT                                                           
*                                                                               
SETB50   CLI   BIVACLV,2                                                        
         BNE   SETB60                                                           
         MVI   BUDMODE,PROCLEVB                                                 
         B     STBXIT                                                           
*                                                                               
SETB60   CLI   BIVACLV,3                                                        
         BNE   SETB70                                                           
         MVI   BUDMODE,PROCLEVC                                                 
         B     STBXIT                                                           
*                                                                               
SETB70   CLI   BIVACLV,4                                                        
         BNE   STBXIT                                                           
         MVI   BUDMODE,PROCACC                                                  
*                                                                               
STBXIT   XIT                                                                    
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* GETEL # 3                                                          *          
**********************************************************************          
         SPACE 1                                                                
         GETELN R4,DISP2,ELCODE,3                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* BOX ROUTINES (HOOK)                                                *          
**********************************************************************          
         SPACE 1                                                                
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC           RESTORE REG C                                 
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXCOLS(198),XSPACES                                             
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+8,C'T'      SET ROWS                                     
         MVI   BOXROWS+12,C'M'                                                  
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS,C'L'        SET LH MARGIN                                
         SR    R2,R2                                                            
         IC    R2,NUMCOLS                                                       
         LA    R1,BOXCOLS+62       1ST LINE AT 62                               
         LA    R0,10               ?                                            
         CR    R2,R0                                                            
         BL    BOXIT                                                            
         LA    R2,6                MAX # OF COLUMNS = 6                         
BOXIT    MVI   0(R1),C'C'                                                       
         LA    R1,17(R1)           EACH COLUMN = 16                             
         BCT   R2,BOXIT                                                         
         MVI   0(R1),C'R'                                                       
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO BUILD A LIST OF NEW BUSINESS/PROBONO ACCOUNTS       *          
**********************************************************************          
         SPACE 1                                                                
CLTNP    DS    0D                                                               
         NMOD1 0,**CLTN**                                                       
         L     RC,0(R1)                                                         
         USING ACTRECD,R6                                                       
         LA    R6,SVKEY                                                         
         USING BIND,R5                                                          
         L     R5,ACLITAB                                                       
         XC    BININ,BININ            CLEAR TABLE                               
         USING CLITBLD,R2                                                       
         LA    R2,CLIWRK           CLIENT TABLE WORK AREA                       
         XC    CLIWRK,CLIWRK                                                    
         MVC   ACTKEY(ACTRFST-ACTKEY),SPACES                                    
         MVC   ACTKCPY,RCCOMPFL                                                 
         L     R4,ADCMPEL                                                       
         USING CPYELD,R4                                                        
         MVC   ACTKUNT(2),CPYPROD     UL FOR CLI/PRO/JOB                        
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY      SAME C/U/L?                    
         BNE   CLTNPX                                                           
         DROP  R4                                                               
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         USING ACLELD,R4                                                        
         L     R4,AIO                                                           
         MVI   ELCODE,ACLELQ                                                    
         BAS   RE,GETEL4                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CLILEN,ACLVALS                GET CLIENT LENGTH                  
         DROP  R4                                                               
*                                                                               
CLTNP10  LA    R6,SVKEY            BUMP TO NEXT ACCOUNT                         
         SR    R1,R1                                                            
         IC    R1,CLILEN                                                        
         LA    R1,ACTKACT(R1)      BUMP TO JOB FIELD                            
         MVI   0(R1),X'FF'                                                      
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY      SAME C/U/L?                    
         BNE   CLTNPX                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   SVKEY,IOKEY         UPDATE SVKEY                                 
*                                                                               
         USING GOBLOCKD,R3                                                      
         L     R3,ADGOBLOC                                                      
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELCUL,ACTKCULA           CUL                                  
         SR    R1,R1                                                            
         IC    R1,CLILEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   GOSELCLI(0),ACTKACT       MOVE IN CLIENT CODE                    
*                                                                               
         MVC   GOADM,DATAMGR                                                    
         L     R6,AIO                                                           
         ST    R6,GOAKEY                                                        
         GOTO1 GETOPT,DMCB,GOBLOCKD                                             
*                                                                               
         CLI   GOCTYPE,C'B'        NEW BUSINESS                                 
         BE    CLTNP20                                                          
         CLI   GOCTYPE,C'P'        PRO BONO??                                   
         BNE   CLTNP10                                                          
*                                                                               
CLTNP20  MVC   CLICLI,GOSELCLI                                                  
         GOTO1 =A(BINADD),DMCB,(RC),CLIWRK,ACLITAB                              
         B     CLTNP10                                                          
*                                                                               
CLTNPX   L     RE,LASTIO                                                        
         MVC   SVKEY,0(RE)         RE-READ THE LAST ACCOUNT                     
         GOTO1 =A(DMREADDR),DMCB,(RC)                                           
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         XIT1                                                                   
         DROP  R2,R3,R5,R6                                                      
         EJECT                                                                  
**********************************************************************          
* GETEL # 4                                                          *          
**********************************************************************          
         SPACE 1                                                                
         GETELN R4,DISP2,ELCODE,4                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GET SPACE FOR THE NAME TABLES                                      *          
**********************************************************************          
         SPACE 1                                                                
GETBUFF  DS    0D                                                               
         NMOD1 0,*GETBUFF                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R0,=A(BUFFSIZE)    SUM OF ALL TABLE SIZES                        
         GETMAIN R,LV=(0)                                                       
         LA    R0,MAINNUM                                                       
         LR    R5,R1               R5 IS BUFFER POINTER                         
         ST    R1,ABUFF            SAVE BUFF START                              
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         STCM  R5,15,MCUSRDMP                                                   
         LR    RF,R5                                                            
         A     RF,=A(BUFFSIZE)                                                  
         STCM  RF,15,MCUSRDMP+4                                                 
*                                                                               
         L     R2,=A(MAINTAB)                                                   
         USING MAIND,R2                                                         
*                                                                               
GETBF10  MVC   *+8(2),MAINST      SCON OF WHERE TO STORE BUFF LOCATION          
         ST    R5,FULL             FULL IS A DUMMY FOR THE ASSEMBLER            
         L     R4,MAINHEAD         WRITE TABLE HEADER TO BUFF                   
         SR    R1,R1                                                            
         ICM   R1,3,MAINHDLN                                                    
         BZ    GETBF20             HEADER NOT DEFINED                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)                                                    
*                                                                               
GETBF20  A     R5,MAINSIZE         BUMP R5                                      
*                                                                               
         LA    R2,MAINLN(R2)                                                    
         BCT   R0,GETBF10                                                       
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* RELEASE GETMAINED SPACE                                            *          
**********************************************************************          
         SPACE 1                                                                
RELBUFF  DS    0D                                                               
         NMOD1 0,*RELBUFF                                                       
         L     RC,0(R1)                                                         
         L     R1,ABUFF                                                         
         L     R0,=A(BUFFSIZE)    SUM OF ALL TABLE SIZES                        
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         XC    MCUSRDMP,MCUSRDMP   CLEAR XTRA DUMP ADDRESS                      
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* SET BILDATE TO 77 DATE, IF SET                                     *          
**********************************************************************          
         SPACE 1                                                                
GET77DTE DS    0D                                                               
         NMOD1 0,*GET77                                                         
         L     RC,0(R1)                                                         
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         USING PTAELD,R4                                                        
         L     R4,ACMAPRO2                                                      
         DROP  R2                                                               
*                                                                               
         CLI   0(R4),PTAELQ        X'77' - PTA ELEMENT                          
         BNE   GET77X              ALL DONE                                     
*                                                                               
         MVI   ELCODE,PTAELQ                                                    
         B     *+8                                                              
GET7710  BAS   RE,NEXTEL5          GET  X'77' EL                                
         BNE   GET77X              NONE                                         
*                                                                               
         TM    PTASTAT1,PTASPEND   PENDING?                                     
         BO    GET7710                                                          
*                                                                               
         CLI   PTATYPE,PTATWOF     W/O'S?                                       
         BE    GET7710                                                          
         CLI   PTATYPE,PTATWOFR    AND W/O RECOVERY                             
         BE    GET7710             SKIP                                         
*                                                                               
         CLI   PTATYPE,PTATRAL     ALLOCATE TO BILL?                            
         BNE   GET7710                                                          
*                                                                               
         OC    PTARBLDT,PTARBLDT                                                
         BZ    GET7710                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,PTARBLDT),(1,BILDATE) DATE BLD =YMD PKED          
GET77X   XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* GETEL # 5                                                          *          
**********************************************************************          
         SPACE 1                                                                
         GETELN R4,DATADISP,ELCODE,5                                            
         EJECT                                                                  
**********************************************************************          
* TABLE DESCRIBING HOW TO CHOP UP GETMAINED CORE                     *          
* COVERED BY MAIND, USED BY SETMAIN                                  *          
**********************************************************************          
         SPACE 1                                                                
MAINTAB  DS    0A                                                               
         DC    S(AEMPLST)                                                       
         DC    AL2(EMPHDLN)                                                     
         DC    A(EMPHEAD)                                                       
         DC    A(EMPSIZE)                                                       
*                                                                               
         DC    S(ACLTLST)                                                       
         DC    AL2(CLTHDLN)                                                     
         DC    A(CLTHEAD)                                                       
         DC    A(CLTSIZE)                                                       
*                                                                               
         DC    S(ACALREC)                                                       
         DC    AL2(0)                                                           
         DC    A(0)                                                             
         DC    A(IOLNQ)                                                         
*                                                                               
         DC    S(ACALKEYS)                                                      
         DC    AL2(0)                                                           
         DC    A(0)                                                             
         DC    A(GPBUFSZQ)                                                      
*                                                                               
         DC    S(ASTDKEYS)                                                      
         DC    AL2(0)                                                           
         DC    A(0)                                                             
         DC    A(GSBUFSZQ)                                                      
*                                                                               
         DC    S(ASTDHRS)                                                       
         DC    AL2(0)                                                           
         DC    A(0)                                                             
         DC    A(GSOPLENQ)         GETSTD OUTPUT BUFFER                         
*                                                                               
         DC    S(AACCUMS)                                                       
         DC    AL2(0)                                                           
         DC    A(0)                                                             
         DC    A(BUKCOUNT*BUCKLN)                                               
*                                                                               
         DC    S(APERBLK)                                                       
         DC    AL2(0)                                                           
         DC    A(0)                                                             
         DC    A(LPERBLK)                                                       
MAINNUM  EQU   (*-MAINTAB)/MAINLN                                               
         EJECT                                                                  
**********************************************************************          
* TABLE HEADER, SETMAIN MOVES THESE OUT INTO GETMAINED CORE          *          
**********************************************************************          
         SPACE 1                                                                
EMPHEAD  DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(EMP1RLNQ)       RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(L'EMP1RKEY)     KEY LENGTH                                   
         DC    A(EMPMAX)           MAX IN TABLE                                 
EMPHDLN  EQU   *-EMPHEAD                                                        
EMPSIZE  EQU   (EMPMAX*EMP1RLNQ)+EMPHDLN                                        
EMPMAX   EQU   15000                                                            
*                                                                               
CLTHEAD  DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(CLTLEN)         RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(L'CLTKEY)       KEY LENGTH                                   
         DC    A(CLTMAX)           MAX IN TABLE                                 
CLTHDLN  EQU   *-CLTHEAD                                                        
CLTSIZE  EQU   (CLTMAX*CLTLEN)+CLTHDLN                                          
CLTMAX   EQU   8000                                                             
         EJECT                                                                  
**********************************************************************          
* DSECT FOR STORAGE AREA                                             *          
**********************************************************************          
         SPACE 1                                                                
ACR2D    DSECT                                                                  
ATYPES   DS    0A                                                               
AIO      DS    A                   IO AREA                                      
ACLITAB  DS    A                   CLIENT TABLE                                 
CONVMOS  DS    V                                                                
APRINT   DS    V                   PRINT FOR PRINTABLE                          
APRNTBL  DS    V                   PRINTABLE                                    
UNDERLIN DS    V                                                                
ATYPLNQ  EQU   *-ATYPES                                                         
*                                                                               
ADBOX    DS    A                                                                
SAVER4   DS    F                                                                
*                                                                               
ABUFF    DS    A                                                                
AEMPLST  DS    A                                                                
ACLTLST  DS    A                                                                
ACALREC  DS    A                                                                
ACALKEYS DS    A                                                                
ASTDKEYS DS    A                                                                
ASTDHRS  DS    A                                                                
AACCUMS  DS    A                                                                
APERBLK  DS    A                                                                
*                                                                               
DISP2    DS    H                   DISPLACEMENT TO FIRST ELEMENT                
*                                                                               
ACR2DATA DS    0C                  START ADDRESS FOR MVCL CLEAR                 
LEVELS   DS    0XL16                                                            
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVADSC  DS    CL15                LEVEL A NAME                                 
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVBDSC  DS    CL15                LEVEL B NAME                                 
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVCDSC  DS    CL15                LEVEL C NAME                                 
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVDDSC  DS    CL15                LEVEL D NAME                                 
LEVLNQ   EQU   *-LEVELS                                                         
*                                                                               
LEVLNQS  DS    0XL4                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
LEVSCDE  DS    0CL6                LEVEL CODES                                  
LEVACDE  DS    CL6                 LEVEL A CODE                                 
LEVSVGP  DS    0CL1                SAVE GROUP                                   
LEVBCDE  DS    CL6                 LEVEL B CODE                                 
LEVCCDE  DS    CL6                 LEVEL C CODE                                 
LEVDCDE  DS    CL7                 LEVEL D CODE                                 
LVCDLNQ  EQU   *-LEVSCDE                                                        
*                                                                               
LEVANME  DS    CL36                LEVEL A NAME                                 
LEVSDSC  EQU   *-LEVSCDE           TOTAL LEVEL CODES AND NAMES                  
*                                                                               
PROFILES DS    CL12                SAVED COMPANY LEVEL PROGPROF                 
NUMNON   DS    CL1                                                              
COMMAND  DS    CL6                                                              
ELCODE   DS    CL1                                                              
START    DS    CL3                 START DATE PACKED YMD                        
ENDATE   DS    CL3                 END DATE PACKED YMD                          
HIREDATE DS    CL3                 HIRE DATE PACKED YMD                         
FIREDATE DS    CL3                 TERM DATE PACKED YMD                         
HOLIDAYS DS    PL4                 NUMBER OF HOLIDAYS IN A MONTH                
BDGSTR   DS    CL2                 START DATE FOR BUDGETS PACKED YM             
BDGEND   DS    CL2                 END DATE FOR BUDGETS PACKED YM               
BUDSTAT  DS    XL1                 STATUS FOR TRG PCT BULLSHIT                  
NOPROF   EQU   X'80'                                                            
NOBUD    EQU   X'40'                                                            
*                                                                               
PERSTAT  DS    XL1                                                              
PERCOST  EQU   X'80'               PERSON IS ON NEW COST RECORDS                
PERHASTS EQU   X'40'               PERSON HAS TIME SHEET DATA FOR REQ           
*                                                                               
PERINOUT DS    XL(PERLMAX*6)       SETS OF INOUTS ( 2 XL3 DATES)                
*                                                                               
BILDATE  DS    CL3                 BILLING DATE PACKED YMD                      
MOS      DS    CL2                 MONTH OF SERVICE FROM SJ TRANS               
M1START  DS    PL2                 MOS START DATE PACKED                        
M1END    DS    PL2                 MOS END DATE PACKED                          
NOTIMESH DS    CL1                 Y= REJECT ALL TYPE 57'S FOR THIS 49          
CLILEN   DS    CL1                 LEVEL A LEN                                  
PROLEN   DS    CL1                       B                                      
*                                                                               
BUSTKEY  DS    CL15                READ BUDGETS FOR THIS ACCOUNT                
BUSTDHRS DS    XL2                 BUD NUMBER, MODE TO READ AT                  
         DS    PL8                                                              
BUSTDRAT DS    XL2                                                              
         DS    PL8                                                              
BUTRGPCT DS    XL2                                                              
         DS    PL8                                                              
*                                                                               
*                                  STANDARD MONTHLY HOURS TABLE                 
*                                  1 OFFICE CODE, 2YM PACKED, 4 YYMM            
STDLIST  DS    CL264               EBCDIC, PL4 HRS (11 X 24 = 264)              
         DC    X'FF'                                                            
*                                                                               
SVKEY    DS    CL42                                                             
LASTKEY  DS    CL42                                                             
BUDERR   DS    CL30                MSG AREA FOR BAD BUDGETS                     
DAYSINMM DS    PL2                 PRORATED DAYS AREA                           
RATEXHR  DS    CL8                 RATE X STD HRS                               
BUDGAMT  DS    CL8                 CUMULATOR FOR BUDGET AMTS                    
TARGET   DS    CL7                 TARGET UTILIZATION PERCENTAGE                
BUDTP    DS    X                   TYPE OF BUDGET                               
BUDGET#  DS    CL1                 BUDGET #                                     
MATCHKEY DS    CL42                AREA TO SAVE KEY                             
TRGT     DS    CL6                 TARGET BUDGET PERCENT AND MAYBE A *          
ACTHR    DS    CL9                 ACTUAL HRS                                   
STDHR    DS    CL9                 STANDARD HRS                                 
STDVALUE DS    CL12                STANDARD VALUE                               
COLSWIT  DS    CL2                 COLUMN SWITCH(1 OR 2 ITEMS)                  
OFFSWIT  DS    CL4                 COMP/U/L/OFFICE                              
TRGDTS   DS    CL3                 START DATE                                   
TRGDTE   DS    CL3                 END DATE                                     
MMCNT    DS    CL2                 MONTH COUNTER                                
TMPLIST  DS    CL289               2 YM PKED, PL4 STD HRS, PL6 RATEX24          
*                                                                               
OFCODE   DS    CL2                                                              
*                                                                               
NMEFLD   DS    0CL36               NAME FIELDS                                  
OFNAME   DS    CL36                1R OFFICE NAME                               
DPNAME   DS    CL36                1R DEPT NAME                                 
CATNAME  DS    CL36                1R SUB-DEPT NAME                             
EMPLNAME DS    CL36                1R STAFF NAME                                
NMELN1Q  EQU   *-NMEFLD                                                         
CLINME   DS    CL36                SJ CLIENT NAME                               
PRDNME   DS    CL36                SJ PRODUCT NAME                              
JOBNME   DS    CL36                SJ JOB NAME                                  
NMELN2Q  EQU   *-CLINME                                                         
*                                                                               
ALSORT   DS    A                   A(LAST SORT RECORD)                          
SRTWRK   DS    (SRTLNQ)C           WORK AREA FOR SORT RECORD                    
LSTWRK   DS    (SRTLNQ)C           WORK AREA FOR LAST RECORD                    
*                                                                               
TOTSW    DS    CL1                 LEVEL OF TOTAL SWITCH                        
TOTEMP   EQU   C'1'                TOTALS FOR EMPLOYEE LEVEL                    
TOTCAT   EQU   C'2'                TOTALS FOR CATEGORY LEVEL                    
TOTDPT   EQU   C'3'                TOTALS FOR DEPTARTMENT LEVEL                 
TOTOFF   EQU   C'4'                TOTALS FOR OFFICE LEVEL                      
TOTRPT   EQU   C'5'                TOTALS FOR REPORT                            
*                                                                               
ACCUMS   DS    0C                                                               
RPTOT    DS    (TOTLEN)C           OVERALL REPORT TOTAL                         
OFTOT    DS    (TOTLEN)C           (1R) OFFICE ACCUMS                           
DPTTOT   DS    (TOTLEN)C           (1R) DEPT ACCUMS                             
CTTOT    DS    (TOTLEN)C           (1R) CAT ACCUMS                              
EMTOT    DS    (TOTLEN)C           (1R) EMPL ACCUMS                             
*                                                                               
BUKCOUNT EQU   (*-ACCUMS)/(BUCKLN)     # OF ACCUMS                              
BUKCONT1 EQU   (*-EMTOT)/(BUCKLN)                   EMPL                        
BUKCONT2 EQU   (*-CTTOT)/(BUCKLN)                   CAT                         
BUKCONT3 EQU   (*-DPTTOT)/(BUCKLN)                  DPT                         
BUKCONT4 EQU   (*-OFTOT)/(BUCKLN)                   OFF                         
LEVCOUNT EQU   (*-ACCUMS)/(TOTLEN)     # OF LEVEL TOTALS                        
BUCKLN   EQU   7                                                                
*                                                                               
AMNTTAB  DS    0CL150              TABLE OF AMOUNTS                             
BILHR    DS    CL9                 BILLABLE HOURS                               
BILSTHR  DS    CL6                 BILLABLE/STANDARD HRS                        
BILDHR   DS    CL9                 BILLED HOURS                                 
WOHR     DS    CL9                 WRITE-OFF HOURS                              
HELDHR   DS    CL9                 HELD HOURS                                   
BVVHR    DS    CL9                 BILLED VS VALUE  HRS                         
BVVPER   DS    CL6                 BILLED VS VALUE PERCENT                      
BVSHR    DS    CL9                 BILLED VS STAND  HRS                         
BVSPER   DS    CL6                 BILLED VS STAND PERCENT                      
BILDOL   DS    CL13                BILLABLE DOLLARS                             
BILDDOL  DS    CL13                BILLED DOLLARS                               
WODOL    DS    CL13                WRITE-OFF DOLLARS                            
HELDDOL  DS    CL13                TIME HELD DOLLARS                            
BVVDOL   DS    CL13                BILLED VS VALUE DOLLARS                      
BVSDOL   DS    CL13                BILLED VS STAND DOLLARS                      
AMNTLNQ  EQU   *-AMNTTAB           LENGTH OF AMOUNT TABLE                       
*                                                                               
CNTNUM   DS    XL1                                                              
NUMCOLS  DS    XL1                 # OF COLUMNS IN RPT                          
WKAREA   DS    CL60                TEMP WORK AREA                               
HEADBLOC DS    3CL130              CONTAINS COLUMN HEADINGS                     
PRNTBLOC DS    2CL165              PRINT AREA                                   
PRN2BLOC DS    CL165               USED FOR STD HRS LINE                        
ACR2DLEN EQU   *-ACR2DATA          LENGTH FOR MVCL CLEAR                        
         EJECT                                                                  
**********************************************************************          
* DSECT FOR SORT RECORD                                              *          
**********************************************************************          
         SPACE 1                                                                
SRTD     DSECT                                                                  
SRTKEY   DS    0C                                                               
SRTEMPL  DS    CL12               EMPLOYEE'S  1R ACCOUNT                        
SRTKLNQ  EQU   *-SRTKEY            SORT KEY LENGTH                              
*                                                                               
SRTSJ    DS    0CL12               SJ  ACCOUNT                                  
SRTCLT   DS    CL3                          -CLIENT                             
SRTPROD  DS    CL3                          -PRODUCT                            
SRTJOB   DS    CL6                          -JOB                                
SRTDLNQ  EQU   *-SRTKEY            RECORD DESCRIPTION LENGTH                    
SRTENAME DS    CL36                1R EMPLOYEE NAME                             
SRTHIRE  DS    CL3                                                              
SRTFIRE  DS    CL3                                                              
SRTSTAT  DS    CL1                 STATUS OF BUDS FOR THIS RECORD               
SBUKLOC  EQU   *-SRTD              LOCATION OF BUCKETS                          
SRTTARGT DS    PL7                 UTILIZATION TARGET PCT (1R)                  
SRTHRACT DS    PL7                 THIS EMPL'S ACTUAL TOTAL PERIOD HRS          
SRTSTAND DS    PL7                 STANDARD HOURS                               
SRTBLHRS DS    PL7                 BILLABLE HOURS (X'40')                       
SRTHRSBL DS    PL7                 HOURS BILLED   (X'77')                       
SRTBLVAL DS    PL7                 BILLING VALUE IN SJ                          
SRTBILLD DS    PL7                 FEES BILLED                                  
SRTSTRAT DS    PL7                 STANDARD DOLLARS                             
SRTWOHR  DS    PL7                 WRITE-OFF HOURS                              
SRTWODO  DS    PL7                 WRITE-OFF DOLLARS                            
SBUKCONT EQU   (*-SRTTARGT)/(L'SRTTARGT)    # OF BUCKETS                        
SRTLNQ   EQU   *-SRTKEY            RECORD LENGTH                                
         EJECT                                                                  
**********************************************************************          
* DSECT FOR COLUMN TABLE                                             *          
**********************************************************************          
         SPACE 1                                                                
COLUMND  DSECT                                                                  
COLNUM   DS    CL1                                                              
COLOFF   DS    CL1                 OFFSET OF COLUMN                             
COLLEN   DS    CL1                 COL LENGTH                                   
         DS    CL1                 SPARE                                        
COLNAME  DS    F                   A(COLUMN HEADER)                             
         EJECT                                                                  
**********************************************************************          
* DSECT FOR BINSRCH PARAMETERS                                       *          
**********************************************************************          
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINTAB   DS    0CL1                                                             
         EJECT                                                                  
**********************************************************************          
* DSECT FOR 1R CODE LIST TABLE                                       *          
**********************************************************************          
         SPACE 1                                                                
EMP1RD   DSECT                                                                  
EMP1RKEY DS    CL15                                                             
EMP1RNME DS    CL36                                                             
EMP1RHIR DS    CL3                 HIRE DATE                                    
EMP1RFIR DS    CL3                 FIRE DATE                                    
EMP1RLNQ EQU   *-EMP1RKEY                                                       
         EJECT                                                                  
**********************************************************************          
* DSECT FOR CLT LIST TABLE                                           *          
**********************************************************************          
         SPACE 1                                                                
CLTCDE   DSECT                                                                  
CLTKEY   DS    CL12                                                             
CLTNAME  DS    CL36                                                             
CLTLEN   EQU   *-CLTKEY                                                         
         EJECT                                                                  
**********************************************************************          
* DSECT FOR TOTAL LINE ACCUMS                                        *          
**********************************************************************          
         SPACE 1                                                                
TOTALD   DSECT                                                                  
TOTTAR   DS    PL7          TOTAL TARGET PCT                                    
TOTHRS   DS    PL7          TOTAL FOR HOURS                                     
TOTSTAND DS    PL7                    STAND                                     
TOTBLHRS DS    PL7             BILLABLE HRS                                     
TOTHRSBL DS    PL7             HOURS BILLED                                     
TOTBLVAL DS    PL7            BILLING VALUE                                     
TOTBILLD DS    PL7              TIME BILLED                                     
TOTRATE  DS    PL7            STANDARD DOLLARS                                  
TOTWOHR  DS    PL7            WRITE-OFF HOURS                                   
TOTWODO  DS    PL7            WRITE-OFF DOLLARS                                 
TOTHRXPC DS    PL7            ADJUSTED STANDARD HOURS (STDHRS X TRG %)          
TOTLEN   EQU   *-TOTALD                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT FOR CLIENT TABLE                                              *         
***********************************************************************         
         SPACE 1                                                                
CLITBLD  DSECT                                                                  
CLICLI   DS    CL3                 CLIENT CODE                                  
CLILNQ   EQU   *-CLICLI                                                         
**********************************************************************          
* DSECT FOR BUDGET PARAMETERS                                        *          
**********************************************************************          
         SPACE 1                                                                
BUDD     DSECT                                                                  
BUDMODE  DS    CL1                                                              
BUDNUM   DS    CL1                                                              
BUDAMNT  DS    PL8                                                              
         EJECT                                                                  
**********************************************************************          
* DSECT FOR TABLE DESCRIBING HOW TO CHOP UP GETMAINED CORE           *          
**********************************************************************          
         SPACE 1                                                                
MAIND    DSECT                                                                  
MAINST   DS    S                   WHERE TO STORE THE ADDRESS TABLE             
MAINHDLN DS    AL2                 LENGTH OF HEADER DATA                        
MAINHEAD DS    A                   ADDRESS OF HEADER DATA                       
MAINSIZE DS    A                   TOTAL SIZE OF THIS TABLE                     
MAINLN   EQU   *-MAIND                                                          
         EJECT                                                                  
**********************************************************************          
* BUFFER - TOTAL CORE I'LL NEED                                      *          
**********************************************************************          
         SPACE 1                                                                
BUFFSIZE EQU   IOLNQ+EMPSIZE+CLTSIZE+CLISIZE+IOLNQ+GPBUFSZQ+GSBUFSZQ+GSX        
               OPLENQ+(BUKCOUNT*BUCKLN)+LPERBLK                                 
*                                                                               
LPERBLK  EQU   1000                                                             
         EJECT                                                                  
**********************************************************************          
* ++INCLUDES                                                         *          
**********************************************************************          
         SPACE 1                                                                
* DDLOGOD                                                                       
* ACGENFILE                                                                     
* ACGENPOST                                                                     
* ACREPWORKD                                                                    
* ACGENMODES                                                                    
* ACMASTD                                                                       
* ACBIGPRNTD                                                                    
* DDBIGBOX                                                                      
* DDCNTRL                                                                       
* DDREPXTRAD                                                                    
* DDREPMASTD                                                                    
* DDBOXEQUS                                                                     
* DDREMOTED                                                                     
*                                                                               
GOBLOCKD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE ACGETSTDD                                                      
       ++INCLUDE ACPERCALLD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACREPR202 12/11/09'                                      
         END                                                                    
