*          DATA SET ACREPR902  AT LEVEL 005 AS OF 12/11/09                      
*PHASE ACR902A                                                                  
*INCLUDE UNDERLIN                                                               
*INCLUDE ACGETSTD                                                               
*INCLUDE PERVERT                                                                
*INCLUDE PERCALL                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'REALIZATION - EMPLOYEE REPORT'                                  
ACR902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACR9**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
         USING ACR9D,RC                                                         
         LA    RC,SPACEND                                                       
*------------------------------------------------------------------*            
*        REQUESTED OFF THE 1R LEDGER                                *           
*        READS 3 DIFFERENT TYPES OF BUDGETS:                        *           
*               1) TARGET BUDGET                                    *           
*               2) STANDARD RATE BUDGET                             *           
*               3) STANDARD HOURS BUDGET  = STDLIST( 36 OFFICE MAX) *           
*                                                                   *           
*        READS 1R ACCOUNT RECORD - GETS NAME & HIRE & FIRE DATE     *           
*        READS 1R HISTORY (MONACC) CLIENT HOURS  = HRSLIST          *           
*        READS SJ TRANS FOR BILLING VALUE, AND TIME BILLED.         *           
*                                                                   *           
*        START END MONTHS - MMM/YY  MMM/YY                          *           
*                           READ BY BUDGET, BILLING VALUE AND HOURS *           
*                                                                   *           
*        MOA RANGE          MMM/YY       TIME BILLED FOR MMM/YY ONLY*           
*                           -MMM/YY      TIME BILLED THRU MMM/YY    *           
*                           MMM/YY-MMM/YY BILLED MMM/YY THRU MMM/YY *           
*                           BLANK        TIME BILLED ETERNITY       *           
*                           READ BY TIME BILLED.                    *           
*                                                                   *           
*        OPTION 1           BLANK - JOB DETAIL BY EMPLOYEE          *           
*                           C     - CLIENT DETAIL BY EMPLOYEE       *           
*                           P     - PRODUCT DETAIL BY EMPLOYEE      *           
*                           E     - LINE LINE PER EMPLOYEE          *           
*                                                                   *           
*        OPTION 2           BLANK - ALL LEVELS OF 1R                *           
*                           S     - EMPLOYEE'S WITHIN OFFICE        *           
*                                   WITH DEPT AND CAT SUPPRESSED.   *           
*        OPTION 3           FORMAT TYPE                             *           
*                           BLANK - BILLABLE VALUE, TIME BILLED,    *           
*                                   WRITE-OFF, TIME HELD, BILLED VS *           
*                                   VALUE, BILLED VS STANDARD       *           
*                           1     - BILLABLE VALUE, TIME BILLED,    *           
*                                   BILLED VS STANDARD              *           
*                                                                   *           
*       STANDARD HOURS      ANNUAL HRS INPUT INTO BUDGETS  BY EMPL. *           
*                                                                   *           
*       STANDARD RATES      ANNUAL RATES INPUT INTO BUDGETS BY EMPL.*           
*                                                                   *           
*       UTIL. ACTUAL PCT    EMPLOYEE HOURS YTD,DIVIDED BY STANDARD  *           
*                           NUMBER OF HOURS YTD.                    *           
*                           EMPLOYEE HOURS AND STANDARD HOURS ARE   *           
*                           KEPT MONTHLY THEN ADDED TOGETHER TO     *           
*                           GET YTD.                                *           
*                           IF AN EMPLOYEE HAD NO CLIENT HOURS IN A *           
*                           GIVEN MONTH, THEN THE STANDARD HRS FOR  *           
*                           THAT MONTH ARE NOT USED IN THAT EMPLOYEE*           
*                           CALCULATIONS.                           *           
*                                                                               
*                           EMPLOYEE CLIENT HOURS IN OR BEFORE THE  *           
*                           HIRE DATE  AND IN OF AFTER THE TERM DATE*           
*                           ARE ALSO ELIMINATED. STANDARD HRS FOR   *           
*                           THOSE MONTHS ARE ALSO NOT USED.         *           
*                           THE X'56' EL HAS HIRE AND TERM DATES.   *           
*                           TERMINATION DURING MONTH RESULTS IN     *           
*                           PRORATION.                              *           
**********************************************************************          
* PROGRAM DUMPS ARE CAUSED BY THE FOLLOWING:                        *           
*  1) INVALID TARGET, RATE OR STANDARD HRS BUDGET ( A MESSAGE IS    *           
*     PRINTED IN BUDERR TO TELL YOU WHICH BUDGET IS INVALID)        *           
*  2) NO NAME ELEMENT                                               *           
*  3) DON'T GET MATCH FOR HRS IN STD OR ACTUAL HRS TABLE            *           
*  4) INCORRECT FORMAT OPTION(OPTION 3)                             *           
*  5) BAD READ OR WRITE                                             *           
*  6) # OF EMPLOYEES EXCEED EMPLIST LIMIT(5000)                     *           
**********************************************************************          
*  GETBUD   P1 = ACCT ADDRESS     P2 = BUDGET #(PROFILES)                       
*           P3 = TYPE OF BUDGET (0 - TARGET, 1 - STD RATES, 2 - STD HRS         
*           P4 = (0 = VALIDATION,   1 = EMPLOYEE READ)                          
*  RETURN CONDITION CODE = GOOD (0) OR BAD (<> 0)                               
***********************************************************************         
         EJECT                                                                  
*************************                                                       
* ROUTINE FOR RUN FIRST *                                                       
*************************                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
*                                                                               
         LA    RE,RELOTAB          RELOCATE MY A TYPES                          
         LA    R1,ATYPES                                                        
RELOOP   L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
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
         GOTO1 =A(NBPB),DMCB,(RC)                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FOR REQUEST FIRST                                        
*                                                                               
REQF     CLI   MODE,REQFRST                                                     
         BNE   LEVELS                                                           
*                                                                               
         LA    RE,ACR9DATA                                                      
         LA    RF,ACR9DLEN                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   PROFILES,PROGPROF   SAVE PROFILES AT COMPANY LEVEL               
*                                                                               
         MVI   NUMCOLS,0                CLEAR COLUMNS                           
         GOTO1 =A(BLDCOLS),DMCB,(RC)                                            
         SPACE 2                                                                
         MVI   RCSUBPRG,0                                                       
         L     R4,ADCMPNAM              ** COMPANY NAME FOR HEADLINES**         
         LA    R5,COMPNAM                                                       
         USING ACNAMED,R4                                                       
         MVC   0(36,R5),SPACES                                                  
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ACNMNAME                                                 
*                                                                               
         L     R5,AEMPLST                    CLEAR CODE/NAME TABLES             
         USING BIND,R5                                                          
         XC    BININ,BININ                   CLEAR THE TABLE                    
         L     R5,ACLTLST                                                       
         XC    BININ,BININ                                                      
*                                       ** INIT FOR SORT **                     
*                                                                               
         XC    ALSORT,ALSORT                 CLEAR A(LAST SORT)                 
*                                                                               
*                                       ** PACK PERIOD DATES **                 
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,START)   START=YMD PACKED                
         MVC   WORK(4),QEND                                                     
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,ENDATE)  ENDATE=YMD PACKED               
         EJECT                                                                  
*                                       ** BUILD TABLE OF MONTHLY **            
*                                       ** DATES AND STANDARD HRS **            
*                                       ** PER MONTH .            **            
*                                       ** PUT PACKED MONTHS INTO **            
*                                       ** EMPL CLI HOUR TABLE    **            
*                                       ** WHICH IS USED AT PROCACC*            
*                                                                               
REQF01   LA    R4,HRSLIST                 MONTHLY TAB EMPL CLI HOURS            
         LA    R2,STDLIST                 STD HOURS TABLE                       
         LA    R3,TMPLIST                 STD HOURS TABLE                       
         LA    R6,TRGPLIST                TARGET PCT BY MONTH FOR BURS          
         LA    R5,24                      14 MONTH LIMIT                        
         L     R0,=F'31'                  DAYS FOR ADDAY NEXT MONTH             
         MVC   1(2,R2),START              PACKED YM                             
         MVC   0(2,R3),START                                                    
         MVC   0(2,R4),START                                                    
         MVC   0(2,R6),START                                                    
         MVC   WORK(4),QSTART             EBCDIC START FOR WK DAYS CALC         
         MVC   3(4,R2),QSTART                                                   
         B     REQF02A                                                          
*                                                                               
REQF02   MVC   WORK+4(2),=C'01'                                                 
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         MVC   WORK(6),WORK+6                                                   
         MVC   3(4,R2),WORK                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)    YMD PACKED                    
         MVC   0(2,R4),WORK+6              YM INTO CLI HOUR TABLE               
         MVC   0(2,R6),WORK+6              YM INTO TRG PCT TABLE                
         MVC   0(2,R3),WORK+6              YM INTO TMPLIST                      
         MVC   1(2,R2),WORK+6              YM INTO STD HRS TABLE                
*                                                                               
REQF02A  DS    0H                                                               
         MVC   WORK+4(2),=C'01'           STANDARD HRS & RATE TABLE             
         ZAP   7(4,R2),=P'0'                                                    
         ZAP   2(6,R4),=P'0'                                                    
         ZAP   2(6,R6),=P'0'                                                    
         ZAP   2(4,R3),=P'0'                                                    
         ZAP   6(6,R3),=P'0'                                                    
         MVI   0(R2),X'40'                                                      
*                                                                               
REQF02D  LA    R4,8(R4)                    FOR CLI HOUR TAB TOO                 
         LA    R6,8(R6)                    TARG PCT LIST                        
         LA    R2,11(R2)                                                        
         LA    R3,12(R3)                                                        
         MVI   0(R2),X'FF'                                                      
         MVI   0(R3),X'FF'                                                      
         MVI   0(R4),X'FF'                                                      
         MVI   0(R6),X'FF'                                                      
         CLC   WORK(4),QEND                EQUAL TO END MONTH                   
         BE    REQF10                      YES - TAB COMPLETE                   
         BCT   R5,REQF02                                                        
         DC    H'0'                                                             
         EJECT                                                                  
REQF10   MVC   BDSTRT(1),START             TAKE YEAR FROM START DATE            
         MVI   BDSTRT+1,X'01'              START JANUARY                        
         MVC   BDEND(1),START                                                   
         MVI   BDEND+1,X'12'               END DECEMBER                         
         XC    OFFSWIT(4),OFFSWIT          1ST TIME COMP/U/L/OFF =0             
         SPACE 2                                                                
*                                       ** SET UP MOA RANGE DATES **            
         L     R5,AMONACC                  CLEAR MOA RANGE DATES SO             
         USING ACMD,R5                 MONACC WILL PRINT DATES IN               
         MVC   M1START,ACMMSTR              DETAILS OF REQUEST BUT WILL         
         MVC   M1END,ACMMEND                  NOT HAVE DATES TO FILTER          
         XC    ACMMSTR,ACMMSTR               OUT INFORMATION FROM 1C            
         MVC   ACMMEND,=2X'FF'                                                  
*                                                                               
         MVC   BILLTHRU,SPACES             HEADLINE WORK AREA                   
         CLI   M1END,X'FF'                                                      
         BE    REQF100                                                          
*                                                                               
*                                                                               
REQF20   MVC   BILLTHRU(11),=C'TIME BILLED'                                     
         CLI   QOPT3,C' '                                                       
         BNE   REQF25                                                           
         MVC   BILLTHRU+11(4),=C'-W/O'                                          
REQF25   CLC   ACMCMSTR,SPACES                                                  
         BNE   REQF30                                                           
         MVC   BILLTHRU+16(4),=C'THRU'                                          
         MVC   BILLTHRU+21(6),ACMCMEND                                          
         B     REQF100                                                          
REQF30   MVC   BILLTHRU+16(3),=C'FOR'                                           
         MVC   BILLTHRU+20(6),ACMCMSTR                                          
         CLC   M1START,M1END                                                    
         BNE   REQF40                                                           
         MVC   BILLTHRU+27(4),=C'ONLY'                                          
         B     REQF100                                                          
REQF40   MVC   BILLTHRU+27(4),=C'THRU'                                          
         MVC   BILLTHRU+32(6),ACMCMEND                                          
*                                                                               
REQF100  GOTO1 ADSQUASH,DMCB,BILLTHRU,38                                        
         ZAP   TARGET,=P'0'                                                     
*                                                                               
         XC    BUDSTAT,BUDSTAT                                                  
         CLI   PROFILES,0          NO TRG BUD DEFINED                           
         BNE   REQF110                                                          
         OI    BUDSTAT,NOPROF                                                   
*                                                                               
REQF110  LA    R2,BUTRGPCT                                                      
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
*                                                                               
*        NOTE: CONTINUE ON WITH AN INVALID BUDGET ADDED AS PER LDES             
*              3/90                                                             
         BAS   RE,SETBURSN         SET ISBURSON IF IS BURSON                    
         GOTO1 =A(SETTITLE)                                                     
         XC    PERSTAT,PERSTAT     ASSUME NO PERSON RECORDS                     
         L     R1,APERBLK                                                       
         USING PERD,R1                                                          
         MVC   PERDMGR,DATAMGR                                                  
         MVC   PERCOMP,RCCOMPFL                                                 
         OI    PERFLAGS,PERRECCK                                                
         GOTO1 =V(PERCALL)         GET PERSON BLOCK                             
         TM    PERERR,PERNOREC     USING TMS                                    
         BO    *+8                                                              
         OI    PERSTAT,PERISNEW    USING PERSON RECORDS                         
*                                                                               
REQFX    B     XIT                         EXIT                                 
         DROP  R5                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        READ BUDGETS AT THE LEVEL THE USER DEFINED THEM                        
*----------------------------------------------------------------------         
*                                                                               
LEVELS   CLI   MODE,LEDGFRST                                                    
         BE    LEVREAD                                                          
         CLI   MODE,PROCLEVA                                                    
         BE    LEVREAD                                                          
         CLI   MODE,PROCLEVB                                                    
         BE    LEVREAD                                                          
         CLI   MODE,PROCLEVC                                                    
         BNE   PROCAC                                                           
LEVREAD  XC    HIREDATE,HIREDATE            SET HIRE TO '000000'                
         MVC   FIREDATE,=X'FFFFFF'          TERM DATE TO ETERNITY               
         BAS   RE,READBUD                                                       
         B     XIT                                                              
         EJECT                                                                  
*****************************************                                       
*  BUILD SORT RECORD KEY  FROM U/L 1R  **                                       
*  LOOKUP NAME AND TARGET UTIL BUDGETS **                                       
*****************************************                                       
*                                                                               
PROCAC   CLI   MODE,PROCACC                                                     
         BNE   ACTUAL                                                           
*                                                                               
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCRDTIME,C'N'                                                    
         MVI   FCRDHIST,C'Y'                                                    
         CLI   PROFILES+3,C'Y'     INCLUDE NB/PB TIME                           
         BE    PRAC10              YES                                          
         MVI   FCRDTRNS,C'Y'       NO, READ TRANSACTIONS TO GET TIME            
         MVI   FCRDTIME,C'Y'       SO I CAN FILTER BY CLIENT                    
         MVI   FCRDHIST,C'N'                                                    
*                                                                               
PRAC10   XC    HIREDATE,HIREDATE            SET HIRE TO '000000'                
         MVC   FIREDATE,=X'FFFFFF'          TERM DATE TO ETERNITY               
*                                                                               
         TM    PERSTAT,PERISNEW    IS THIS PERSON ON NEW COST                   
         BZ    PRAC30                                                           
*                                                                               
         L     R1,APERBLK                                                       
         USING PERD,R1                                                          
         MVC   PERALEDG,ADLEDGER                                                
         MVC   PERADACC,ADACC                                                   
         GOTO1 =V(PERCALL)         GET PERSON BLOCK                             
         MVC   HIREDATE,PERHIR            HIRE DATE                             
         OC    PERTRM,PERTRM            IS THERE A TERM DATE                    
         BZ    *+10                                                             
         MVC   FIREDATE,PERTRM              TERM DATE                           
         BAS   RE,SETINOUT         SET INOUTS                                   
         B     PROCAC00                                                         
         DROP  R1                                                               
*                                                                               
PRAC30   L     R4,ADACC                                                         
         MVI   ELCODE,X'56'                 EMPLOYEE HISTORY ELEMENT            
         BAS   RE,GETEL                                                         
         BNE   PROCAC00                     NO X'56' ELEMENT                    
         USING ACEMPD,R4                                                        
         MVC   HIREDATE,ACEMPHIR            HIRE DATE                           
         OC    ACEMPTRM,ACEMPTRM            IS THERE A TERM DATE                
         BZ    PROCAC00                                                         
         MVC   FIREDATE,ACEMPTRM            TERM DATE                           
*                                                                               
PROCAC00 BAS   RE,READBUD          READ ANY BUDGETS DEFINED AT THIS LEV         
*                                                                               
         LA    R2,HRSLIST                  CLEAR CLI HRS TABLE                  
         LA    R0,24                       12 MONTH MAX                         
PROCAC0  ZAP   2(6,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R0,PROCAC0                                                       
         LA    R6,SRTWRK                                                        
         USING SRTD,R6                                                          
         BAS   RE,CLERSORT                 CLEAR SORT WORK AREA                 
         SPACE 3                                                                
*                                  *BUILD THE 1R SORT KEY*                      
*                                                                               
         L     R4,ADACC                     ADDR OF ACCOUNT                     
         CLI   3(R4),C'9'                                                       
         BE    XIT                                                              
         CLC   4(2,R4),=C'999'     ELIMINATE ALL OVERHEAD                       
         BE    XIT                 ACCOUNTS                                     
         CLC   8(3,R4),=C'999'                                                  
         BE    XIT                                                              
*                                                                               
PROCAC1A MVC   SRTEMPL,3(R4)                1R OFF/DPT/CAT/STAFF                
         USING EMPCDE,R5                    1R CODE AND NAME INTO TAB           
         L     R5,ACREC            BUILD EMPREC IN ACREC                        
         MVC   WORK,SPACES                                                      
         MVC   EMPCDE(15),0(R4)                                                 
         SPACE 3                                                                
         MVI   ELCODE,X'20'                 NAME ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                          NO NAME ELEMENT                     
         DC    H'0'                                                             
*                                                                               
         MVC   EMPNAME,XSPACES                                                  
         USING ACNAMED,R4                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   EMPNAME(0),ACNMNAME          1R NAME INTO TABLE                  
         MVC   EMPHIRE,HIREDATE                                                 
         MVC   EMPFIRE,FIREDATE                                                 
         MVC   EMPTRGP,TRGPLIST             SAVE TARGET PCTS                    
         GOTO1 =A(BINADD),DMCB,(R5),AEMPLST                                     
*                                                                               
         MVC   SRTENAME,EMPNAME             1R NAME INTO SORTREC                
         MVC   SRTHIRE,HIREDATE             HIRE/FIRE DATES TO SORT REC         
         MVC   SRTFIRE,FIREDATE                                                 
         MVC   SRTSTAT,BUDSTAT                                                  
*                                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*****************************************                                       
*  GET THE 1C HOURS WORKED FROM U/L 1C **                                       
*****************************************                                       
ACTUAL   CLI   MODE,PROCHIST                                                    
         BNE   TRANS                                                            
         CLI   PROFILES+3,C'Y'     INCLUDE  NB/PB TIME                          
         BNE   XIT                 NO, READ TRANSACTIONS                        
ACT01    LA    R6,SRTWRK               SORT WORK AREA                           
         L     R4,ADSUBAC              ADDR OF SUB ACCT                         
         USING TRSUBHD,R4                                                       
         L     R5,ADTRANS                                                       
         CLI   0(R5),X'45'                                                      
         BNE   XIT                                                              
         CLC   TRSBACNT+1(2),=C'1C'    MUST BE 1C CLIENT TIME ONLY              
         BNE   XIT                                                              
         CLI   BUCKTYPE,C'H'           HOURS ONLY                               
         BNE   XIT                                                              
         USING TRHISTD,R5                                                       
*                                      WE DON'T WANT IT IF:                     
         CLC   TRHSYEAR(2),START       LOWER THAN START YYMM                    
         BL    XIT                                                              
         CLC   TRHSYEAR(2),ENDATE      HIGHER THAN END YYMM                     
         BH    XIT                                                              
*                                                                               
         LA    R1,HRSLIST              EMPL CLIENT HRS TABLE PER MONTH          
         LA    R0,24                                                            
ACT02    CLC   0(2,R1),TRHSYEAR        MATCH THE YYMM                           
         BE    ACT03                                                            
         LA    R1,8(R1)                NEXT MONTH IN TABLE                      
         BCT   R0,ACT02                                                         
         DC    H'0'                    IT HAS TO FIT SOMEWHERE                  
ACT03    AP    2(6,R1),TRHSCR          HOURS INTO TABLE                         
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
*        BUILD HRSLIST FROM TRANSACTION DATA                                    
*--------------------------------------------------------------------           
TRANS    CLI   MODE,PROCTRNS                                                    
         BNE   TIME                                                             
         CLI   PROFILES+3,C'Y'     INCLUDE NB/PN                                
         BE    XIT                 YES, USE HRS LIST BUILD WITH BUCKETS         
         L     R4,ADTRANS                                                       
         USING TRSUBHD,R4                                                       
         L     R5,ADTRANS                                                       
         LR    R4,R5                                                            
         SH    R5,DATADISP                                                      
         USING ACKEYD,R5                                                        
         CLC   ACKEYCON+1(2),=C'1C'    MUST BE 1C CLIENT TIME                   
         BNE   XIT                                                              
*                                      WE DON'T WANT IT IF:                     
         L     R2,AMONACC                                                       
         USING ACMD,R2                                                          
         CLC   ACMMDTE,START             LOWER THAN START YYMM                  
         BL    XIT                                                              
         CLC   ACMMDTE,ENDATE            HIGHER THAN END YYMM                   
         BH    XIT                                                              
*                                                                               
         USING ACPCD,R4                                                         
         MVI   ELCODE,X'51'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   TRN20               NO 51 EL IS OK                               
         MVC   WORK(L'NBPBACC),SPACES                                           
         MVC   WORK(L'NBPBACC),ACPCCLI+3                                        
         BAS   RE,CHKNBPB          IS THIS CLIENT NB/PB                         
         LTR   R0,R0                                                            
         BNZ   XIT                 CLIENT FOUND, REJECT                         
*                                                                               
TRN20    L     R4,ADTRANS                                                       
         MVI   ELCODE,X'40'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   TRN50               TRY FOR N TIME                               
*                                                                               
         USING ACPERSD,R4                                                       
         ZAP   DUB,ACPSHOUR                                                     
         B     TRN60                                                            
*                                                                               
TRN50    L     R4,ADTRANS          IS THIS N TIME?                              
         MVI   ELCODE,X'50'                                                     
TRN55    BAS   RE,NEXTEL                                                        
         BNE   XIT                 NOT N TIME                                   
*                                                                               
         USING TRCASHD,R4                                                       
         CLI   TRCSTYPE,C'H'       HOURS HERE                                   
         BNE   TRN55               GET NEXT 50 EL                               
*                                                                               
         ZAP   DUB,TRCSAMNT                                                     
*                                                                               
TRN60    LA    R1,HRSLIST              EMPL CLIENT HRS TABLE PER MONTH          
         LA    R0,24                                                            
TRN02    CLC   0(2,R1),ACMMDTE              MATCH THE YYMM OF THE TRANS         
         BE    TRN03                                                            
         LA    R1,8(R1)                   NEXT MONTH IN TABLE                   
         BCT   R0,TRN02                                                         
         DC    H'0'                    IT HAS TO FIT SOMEWHERE                  
TRN03    AP    2(6,R1),DUB             HOURS INTO TABLE                         
         B     XIT                                                              
*--------------------------------------------------------------------           
         EJECT                                                                  
*                                                                               
* -------------------------------------------------------------------           
* PROCESS TMS RECORDS (ELEMENTS)                                                
* -------------------------------------------------------------------           
*                                                                               
TIME     CLI   MODE,PROCTIME                                                    
         BNE   ACLAST                                                           
         CLI   PROFILES+3,C'Y'     INCLUDE NB/PN                                
         BE    TIMEX               YES, USE BUCKET HOURS                        
*                                                                               
         USING TIMELD,R5                                                        
         L     R5,ADTRANS                                                       
         CLI   TIMEL,TIMELQ                                                     
         BNE   TIMEX                                                            
         CLI   TIMETYP,TIMEINP     IS THIS INPUT DETAIL                         
         BNE   TIMEX                                                            
         CLI   TIMLN,TIMILN1Q      AT LEAST INPUT LENGTH                        
         BL    TIMEX                                                            
*                                                                               
         MVC   WORK(L'NBPBACC),SPACES                                           
         MVC   WORK(L'NBPBACC),TIMACC+2                                         
         BAS   RE,CHKNBPB          IS THIS CLIENT NB/PB                         
         LTR   R0,R0                                                            
         BNZ   TIMEX               CLIENT FOUND, REJECT                         
*                                                                               
         CLI   TIMTTYP,TIMTCB      CLIENT BILLABLE TIME                         
         BE    TIME40                                                           
         CLI   TIMTTYP,TIMTCR      CLIENT REALIZATION                           
         BE    TIME40                                                           
         CLI   TIMTTYP,TIMTCN      CLIENT NON BILLABLE                          
         BE    TIME40                                                           
         CLI   TIMTTYP,TIMTNC      NON CLIENT                                   
         BE    TIMEX               NOT ON THIS REPORT                           
*                                                                               
*                                                                               
TIME40   LA    R1,HRSLIST          EMPL CLIENT HRS TABLE PER MONTH              
         LA    R0,24                                                            
TIME60   CLC   0(2,R1),TIMMOA      MATCH THE YYMM OF THE TRANS                  
         BE    TIME70                                                           
         LA    R1,8(R1)             NEXT MONTH IN TABLE                         
         BCT   R0,TIME60                                                        
         B     TIMEX               MONACCS DATE FILTERING IS SHAKEY             
*                                                                               
TIME70   AP    2(6,R1),TIMHRS      HOURS INTO TABLE                             
*                                                                               
TIMEX    B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------           
*  ADD THE RECORD TO SORTER  **                                                 
*--------------------------------------------------------------------           
*                                                                               
ACLAST   CLI   MODE,ACCLAST                                                     
         BNE   SJBILD                                                           
*                                                                               
         L     R4,ADACC                     ADDR OF ACCOUNT                     
         CLI   3(R4),C'9'                                                       
         BE    XIT                                                              
         CLC   4(2,R4),=C'999'     ELIMINATE ALL OVERHEAD                       
         BE    XIT                 ACCOUNTS                                     
         CLC   8(3,R4),=C'999'                                                  
         BE    XIT                                                              
*                                                                               
         CLI   ISBURSON,C'Y'       IS THIS BURSON OR THEIR ILK                  
         BNE   ALAST00A                                                         
         GOTO1 =A(RESETTMP)        RESET LISTS W/=COST STUFF                    
*                                                                               
         LA    R6,SRTWRK                                                        
         ZAP   SRTSTRAT,=P'0'      CALCULATE STANDARD VALUE                     
         ZAP   SRTSTAND,=P'0'      AND STD HOURS                                
         LA    R4,TMPLIST                                                       
         LA    R0,24                                                            
ACL20    CLI   0(R4),X'FF'                                                      
         BE    ACL30                                                            
         ZAP   RATEXHR,6(6,R4)     STD RATE   FOR MONTH                         
         MP    RATEXHR,2(4,R4)     STANDARD RATE X STD HRS                      
         SRP   RATEXHR,64-2,5      ROUND TO 2 DECIMAL PLACES                    
         ZAP   6(6,R4),RATEXHR     UPDATE TABLE                                 
         AP    SRTSTAND,2(4,R4)    ADD STAND HRS TO SORT REC                    
         AP    SRTSTRAT,6(6,R4)    ADD STAND VAL TO SORT REC                    
         LA    R4,12(R4)                                                        
         BCT   R0,ACL20                                                         
*                                                                               
ACL30    LA    R0,24               CALCULATE TOTAL HOURS                        
         LA    R4,HRSLIST                                                       
ACL50    CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    ALAST04             YES - PUT TO SORT                            
         AP    SRTHRACT,2(6,R4)                                                 
         LA    R4,8(R4)            BUMP CLI HRS TABLE                           
         BCT   R0,ACL50                                                         
         B     ALAST04                                                          
*                                                                               
ALAST00A LA    R4,TMPLIST                  LIST FOR STD VALUE CAL               
         LA    R0,24                       SIZE OF TMPLIST                      
*                                                                               
ALAST0A  CLC   0(2,R4),HIREDATE            IS IT BEFORE                         
         BL    ALAST0AA                    YES - EXCLUDE                        
         CLC   0(2,R4),FIREDATE            FIRED BEFORE?                        
         BNH   ALAST0AB                    NO, KEEP                             
*                                                                               
ALAST0AA ZAP   2(4,R4),=P'0'               NO STANDARD HOURS                    
*                                                                               
ALAST0AB LA    R4,12(R4)                   INCREMENT STD LIST                   
         BCT   R0,ALAST0A                                                       
*                                                                               
         MVI   0(R4),X'FF'                 END OF TABLE                         
*                                                                               
         LA    R4,TMPLIST                  PT TO BEG OF TABLE                   
         LA    R6,SRTWRK                                                        
         LA    R1,HRSLIST                  EMPL CLI HOURS BY MONTH              
         LA    R0,24                       24 MONTH MAX                         
         ZAP   SRTSTRAT,=P'0'      CALCULATE STANDARD VALUE                     
         ZAP   SRTSTAND,=P'0'      AND STD HOURS                                
*                                                                               
ALAST01  CLI   0(R1),X'FF'                 END OF TABLE                         
         BE    ALAST04                     YES - PUT TO SORT                    
*                                                                               
         AP    SRTHRACT,2(6,R1)            ADD HRS TO SORT REC                  
*                                                                               
         CLC   0(2,R4),HIREDATE            IS HIRE DATE IN THIS MONTH?          
         BNE   ALAST02D                    NO - CHECK FIREDATE                  
*                                                                               
         CLI   HIREDATE+2,X'01'            HIRED ON FIRST DAY?                  
         BE    ALAST02D                    YES - CHECK FIREDATE                 
*                                                                               
         L     R5,=A(MMDAYS)               DAYS IN MONTH TABLE                  
         ZAP   DAYSINMM,=P'0'                                                   
ALAST02A CLC   0(1,R5),HIREDATE+1          DO MONTHS MATCH?                     
         BE    ALAST02B                    YES                                  
         LA    R5,3(R5)                    INCREMENT TABLE                      
         B     ALAST02A                                                         
*                                                                               
ALAST02B ZAP   DAYSINMM,1(2,R5)            LOAD IN DAYS IN MONTH                
*                                                                               
         CLC   HIREDATE(2),FIREDATE        DOES HIREDATE = FIREDATE?            
         BNE   ALAST02C                    NO                                   
*                                                                               
         ZAP   DUB,=P'0'                                                        
         ZAP   BUDGAMT,=P'0'                                                    
         MVO   DUB,FIREDATE+2(1)           CONVERT TO PACK                      
         MVO   BUDGAMT,HIREDATE+2(1)        CONVERT TO PACK                     
         B     ALAST02H                    FIREDATE - HIREDATE                  
*                                                                               
ALAST02C ZAP   DUB,DAYSINMM                MATCH ON HIRE DATE ONLY              
         ZAP   BUDGAMT,=P'0'                                                    
         MVO   BUDGAMT,HIREDATE+2(1)        CONVERT TO PACK                     
         B     ALAST02H                                                         
*                                                                               
ALAST02D CLC   0(2,R4),FIREDATE            MATCH ON FIRE DATE?                  
         BNE   ALAST03                     NO                                   
*                                                                               
         L     R5,=A(MMDAYS)              DAYS IN MONTH TABLE                   
         ZAP   DAYSINMM,=P'0'                                                   
ALAST02E CLC   0(1,R5),FIREDATE+1          DO MONTHS MATCH?                     
         BE    ALAST02F                    YES                                  
         LA    R5,3(R5)                    INCREMENT TABLE                      
         B     ALAST02E                                                         
*                                                                               
ALAST02F ZAP   DAYSINMM,1(2,R5)            LOAD IN DAYS IN MONTH                
*                                                                               
         CLC   HIREDATE(2),FIREDATE        DOES HIREDATE = FIREDATE?            
         BNE   ALAST02G                    NO                                   
*                                                                               
         ZAP   DUB,=P'0'                                                        
         ZAP   BUDGAMT,=P'0'                                                    
         MVO   DUB,FIREDATE+2(1)           CONVERT TO PACK                      
         MVO   BUDGAMT,HIREDATE+2(1)        CONVERT TO PACK                     
         B     ALAST02H                                                         
*                                                                               
ALAST02G ZAP   DUB,DAYSINMM                MATCH ON FIRE DATE ONLY              
         ZAP   BUDGAMT,=P'0'                                                    
         MVO   DUB,FIREDATE+2(1)            DATE FIRED                          
*                                                                               
ALAST02H ZAP   RATEXHR,=P'0'                                                    
         SP    DUB,BUDGAMT                 DAYS-IN-MONTH -                      
         MP    DUB,=P'100000'                                                   
         DP    DUB,DAYSINMM               /DAYS IN MONTH                        
         ZAP   RATEXHR,2(4,R4)                                                  
         MP    RATEXHR,DUB+2(4)           STD HRS X PRORATION                   
         SRP   RATEXHR,64-5,5            ROUND TO 2 DECIMALS                    
         ZAP   2(4,R4),RATEXHR+4(4)       MOVE BACK TO TABLE                    
*                                                                               
*---------------------------------------------------------------------          
*        CALCULATE STD VALUE                                                    
*        STD HOURS X RATE X TRG PCT                                             
*---------------------------------------------------------------------          
*                                                                               
ALAST03  ZAP   RATEXHR,6(6,R4)         STD RATE   FOR MONTH                     
         MP    RATEXHR,2(4,R4)         STANDARD RATE X STD HRS                  
         SRP   RATEXHR,64-2,5          ROUND TO 2 DECIMAL PLACES                
*                                                                               
         CLI   ISBURSON,C'Y'       IS THIS A BURSON                             
         BE    ALAST03D            YES, TRG% ALREADY IN TMPLIST                 
*                                  VIA RESETTMP ROUTINE                         
         CP    TARGET,=P'0'                                                     
         BE    ALAST03D                                                         
*                                                                               
         ZAP   DUB,TARGET              MULTIPLY BY TARGET                       
         DP    DUB,=P'100'                                                      
         MP    RATEXHR,DUB+4(2)                                                 
         SRP   RATEXHR,64-2,5          ROUND TO 2 DECIMAL PLACES                
*                                                                               
ALAST03D ZAP   6(6,R4),RATEXHR         UPDATE TABLE                             
*                                                                               
         AP    SRTSTAND,2(4,R4)            ADD STAND HRS TO SORT REC            
         AP    SRTSTRAT,6(6,R4)            ADD STAND VAL TO SORT REC            
         LA    R1,8(R1)                    BUMP CLI HRS TABLE                   
         LA    R4,12(R4)                   INCREMENT PRORATE TABLE              
         BCT   R0,ALAST01                                                       
*                                                                               
         USING BUDD,R2                                                          
ALAST04  DS    0H                                                               
         LA    R2,BUTRGPCT                                                      
         ZAP   SRTTARGT,BUDAMNT                  GET BUD AMOUNT                 
*                                                                               
         BAS   RE,PUTSORT                      PUT TO SORT                      
         B     XIT                                                              
         EJECT                                                                  
***********************************                                             
* LOOKUP SJ ACCOUNTS CLI/PRD/JOB **                                             
* COSTING ACCT FROM X'24' EL     **                                             
* READ TRANS FOR BILLING VALUE   **                                             
* AND TIME BILLED                **                                             
***********************************                                             
SJBILD   CLI   MODE,REQLAST                                                     
         BNE   LASTRUN                                                          
*                                                                               
         OC    ALSORT,ALSORT       ANY EMPLOYEES IN SORT                        
         BZ    XIT                 NO, I'M DONE                                 
*                                                                               
         LA    R6,SRTWRK                                                        
         USING SRTD,R6                                                          
         BAS   RE,CLERSORT                                                      
         USING TRNRECD,R7                                                       
         L     R7,ACREC                                                         
         MVC   TRNKEY,SPACES       CLEAR KEY                                    
         MVC   TRNKCPY,RCCOMPFL            MOVE IN COMPANY                      
         MVC   TRNKUNT(2),=C'SJ'            U/L SJ                              
*                                                                               
         MVC   DATADISP,=Y(TRNRFST-TRNRECD)                                     
*                                                                               
SJBILD01 BAS   RE,HIGH                                                          
         B     *+8                                                              
SJBILD03 BAS   RE,SEQ                                                           
*                                                                               
         L     R7,ACREC                                                         
*                                                                               
         CLC   TRNKCULA(3),SAVEKEY           SAME CO/U/L                        
         BE    SJBILD05                        YES CONTINUE                     
         CLC   SRTWRK(SRTDLNQ),SPACES                                           
         BE    *+8                                                              
         BAS   RE,PUTSORT                      PUT LAST ONE TO SORT             
         MVC   DATADISP,=Y(49)                                                  
         B     REPORT                                                           
*                                                                               
SJBILD05 CLI   PROFILES+3,C'Y'     INCLUED NEW BUISNESS/PB CLIENT               
         BE    SJBILD06            YES                                          
         CLC   TRNKACT(L'NBPBACC),SAVEKEY+3     SAME ACCOUNT                    
         BE    SJBILD06                        YES                              
         MVC   WORK(L'NBPBACC),SPACES                                           
         MVC   WORK(L'NBPBACC),TRNKACT                                          
         BAS   RE,CHKNBPB          CHECK IF ACCOUNT IS NBPB                     
         LTR   R0,R0                                                            
         BZ    SJBILD06            NOPE, NOT IN LIST                            
*                                                                               
         MVI   TRNKACT+6,X'FF'                                                  
         B     SJBILD01            GET NEXT PRODUCT                             
*                                                                               
SJBILD06 CLC   TRNKCUNT(2),=C'1R'            IS CONTRA UL 1R                    
         BNE   SJBILD03                        NO - READ NEXT                   
*                                                                               
         CLC   TRNKREF,SPACES      IS THE REFERENCR # SET,                      
         BNH   SJBILD03            NOT A TRAN                                   
*                                                                               
         L     R5,AEMPLST                                                       
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ                MOVE IN PARMS 3,4,5,6            
         LA    R0,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,TRNKCULC,(R0)                                       
         CLI   DMCB,0                                                           
         BNE   SJBILD03                        NOT FOUND                        
*                                                                               
         L     R5,DMCB                                                          
         USING EMPCDE,R5                                                        
*                                                                               
         MVC   SRTEMPL,TRNKCACT                1R EMPL INTO SORTREC             
         MVC   SRTSJ,TRNKACT                   SJ CLI/PRD/JOB INTO SORT         
         MVC   SRTENAME,EMPNAME                1R EMP NAME INTO SRTREC          
         MVC   SRTHIRE,EMPHIRE                                                  
         MVC   SRTFIRE,EMPFIRE                                                  
         MVC   SRTSTAT,BUDSTAT                                                  
*                                                                               
         BAS   RE,GET              GET THE RECORD                               
*                                                                               
         LR    R4,R7                                                            
         MVI   ELCODE,X'44'                                                     
         BAS   RE,GETEL                                                         
         BNE   SJBILD99            NO, GET NEXT                                 
*                                                                               
         LR    R4,R7                                                            
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         BNE   SJBILD99            NO, GET NEXT                                 
         USING PRTELD,R4                                                        
         TM    PRTSTAT,PRTSBILQ                                                 
         BZ    SJBILD99                                                         
         DROP  R4                                                               
*                                                                               
         L     R4,ACREC                                                         
         USING ACMD,R5                                                          
         L     R5,AMONACC                                                       
         GOTO1 ACMAPRAT,DMCB,(X'40',(R4)),0,ADCOMFAC,0,                X        
               ACMAPROB,ACMAPRO2                                                
*                                                                               
         BAS   RE,SETMOS                                                        
*                                                                               
*        FILTER TYPE 49'S ON QSTART/QEND V.S. MOS                               
*                                                                               
         CLC   MOS,START                       IS MOS LOWER THAN START          
         BL    SJBILD99                        YES - CLEAR AMOUNTS              
         CLC   MOS,ENDATE                      OR HIGHER THAN END DATE          
         BH    SJBILD99                        NO, DETERMINE BILLED             
*                                                                               
         CLI   TRNRSTYP,57         SKIP THESE ALTOGETHER                        
         BE    SJBILD99            WILL GET INFO FROM X'77'S                    
         CLI   TRNRSTYP,58                                                      
         BE    SJBILD99                                                         
*                                                                               
         USING PRORATAD,R3                                                      
         L     R3,ACMAPROB                                                      
*                                                                               
         ZAP   SRTBLVAL,PA$NET                                                  
         ZAP   SRTBLHRS,PA$HOURS                                                
*                                                                               
         USING PTAELD,R4                                                        
         L     R4,ACMAPRO2         GET BILLS AND WRITE OFFS                     
*                                                                               
SJBILD26 CLI   PTAEL,PTAELQ        IS THIS A PTA ELEMENT                        
         BNE   SJBILD98                        PUT TO SORT                      
*                                                                               
         TM    PTASTAT1,PTASPEND   ACTIVITY PENDING (DRAFT)?                    
         BO    SJBILD29            YES, NEXT PTA                                
         CLI   PTATYPE,PTATRAL     BILLING?                                     
         BNE   SJBILD30            NO, SEE IF WRITE-OFF                         
*                                                                               
         MVC   BILDATE,PTAMOA                                                   
         OC    PTAMOA,PTAMOA       HAS THE MOA BEEN SET                         
         BNZ   SJBILD27            YES, USE IT                                  
*                                                                               
         GOTO1 DATCON,DMCB,(2,PTARBLDT),(1,BILDATE)                             
*                                                                               
SJBILD27 CLC   BILDATE(2),M1START  LOWER THAN START MOA YM                      
         BL    SJBILD29            LOOK FOR ANOTHER PTA ELEMENT                 
         CLC   BILDATE(2),M1END    HIGHER THAN END MOA YM                       
         BH    SJBILD29            LOOK FOR ANOTHER PTA ELEMENT                 
*                                                                               
         AP    SRTBILLD,PTANET                                                  
         LH    RF,PTAHOURS                                                      
         CVD   RF,DUB                                                           
         AP    SRTHRSBL,DUB                                                     
*                                                                               
SJBILD29 XR    R1,R1                                                            
         IC    R1,PTALN                                                         
         LA    R4,0(R1,R4)                                                      
         B     SJBILD26                                                         
*                                                                               
SJBILD30 CLI   PTATYPE,PTATWOF     LOOK FOR WRITE-OFFS                          
         BE    SJBILD32                                                         
         CLI   PTATYPE,PTATWOFR    AND RECOVERIES                               
         BNE   SJBILD29                                                         
*                                                                               
SJBILD32 CLC   PTAMOA,M1START      LOWER THAN START MOA YM                      
         BL    SJBILD29            LOOK FOR ANOTHER PTA ELEMENT                 
         CLC   PTAMOA,M1END        HIGHER THAN END MOA YM                       
         BH    SJBILD29            LOOK FOR ANOTHER PTA ELEMENT                 
*                                                                               
         AP    SRTWODO,PTANET                                                   
         LH    RF,PTAHOURS                                                      
         CVD   RF,DUB                                                           
         AP    SRTWOHR,DUB                                                      
         B     SJBILD29                                                         
*                                                                               
SJBILD98 BAS   RE,PUTSORT                                                       
*                                                                               
SJBILD99 BAS   RE,CLERSORT                                                      
         B     SJBILD03                                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
* SET TRANSACTION MONTH OF SERVIXE                                              
*----------------------------------------------------------------------         
SETMOS   SR    R0,R0                                                            
         L     RF,ACREC                                                         
         AH    RF,DATADISP                                                      
SETMOS3  IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RF),TRSELQ                                                     
         BNE   SETMOS3                                                          
         USING TRSELD,RF                                                        
         MVC   MOS,TRSPMOS                                                      
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
*                                      ** PRINTED REPORT **                     
*                                                                               
REPORT   DS    0H                                                               
         L     R1,AACCUMS                 CLEAR ALL REPORT TOT ACCUMS           
         LA    R0,BUKCOUNT                                                      
RPT0     ZAP   0(7,R1),=P'0'                                                    
         LA    R1,7(R1)                                                         
         BCT   R0,RPT0                                                          
*                                                                               
         OC    ALSORT,ALSORT              IS THERE A LAST SORT ADDR             
         BZ    RPT99                      NO DATA                               
         MVC   LSTWRK(SRTLNQ),XSPACES     CLEAR SAVE AREA FOR PREV REC          
         MVC   PAGE,=H'1'                 SET PAGE TO ONE                       
         MVI   TOTSW,C'1'                 SET FOR CATEGORY TOTAL                
         SPACE 2                                                                
         MVC   EMPLOYEE(1),RCCOMPFL       MOVE IN COMPANY                       
         MVC   EMPLOYEE+1(2),=C'1R'       U/L 1R                                
         MVC   EMPCODE,SPACES             1R SAVE                               
         MVC   SJACCT(1),RCCOMPFL         MOVE IN COMPANY                       
         MVC   SJACCT+1(2),=C'SJ'         U/L SJ                                
         MVC   SJCODE,SPACES              SJ SAVE                               
         MVC   RNAMES,XSPACES                                                   
         MVC   CNAMES,XSPACES                                                   
*                                                                               
RPT2     GOTO1 ADSORTER,DMCB,=C'GET'                                            
         L     R6,DMCB+4                                                        
         ST    R6,ALSORT                  ADDRESS OF LAST SORT                  
         LTR   R6,R6                                                            
         BZ    RPT9                       END OF RECORDS FROM SORT              
         MVC   SRTWRK(SRTLNQ),0(R6)       SAVE CURRENT SORT RECORD              
         CLC   LSTWRK(SRTLNQ),XSPACES     DO I HAVE ONE SAVED                   
         BNE   RPT5                       YES - CONTINUE                        
         MVC   LSTWRK(SRTLNQ),SRTWRK      NO  - SAVE THIS ONE                   
         B     RPT2                       AND GET NEXT                          
*                                                                               
RPT5     CLC   LSTWRK(SRTDLNQ),SRTWRK     SAME KEY                              
         BNE   RPT9                       NO - PROCESS SAVED ONE                
*                                                                               
         LA    R5,LSTWRK                  YES - ADD'EM UP                       
*                                                                               
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
         BH    RPT6                                    FIRE DATE                
         MVC   SRTFIRE-SRTD(L'SRTFIRE,R5),SRTFIRE                               
*                                                                               
RPT6     B     RPT2                       AND GET NEXT                          
*                                                                               
RPT9     BAS   RE,REFRES                  PROCESS SAVED RECORD                  
         OC    ALSORT,ALSORT              IS IT END OF FILE                     
         BZ    RPT90                      YES - PRINT FINAL TOTALS              
         SPACE 2                                                                
         LA    R5,LSTWRK                                                        
*                                                                               
         CLC   SRTEMPL-SRTD(12,R5),SRTEMPL  SAME EMPLOYEE                       
         BE    RPT11                       YES - CONTINUE                       
         MVI   TOTSW,C'1'                  NO  - SET FOR EMP TOTAL              
         BAS   RE,ACCTOT                                                        
*                                                                               
RPT11    LA    R4,SRTEMPL                                                       
         MVI   BYTE,EMPLEVC                                                     
         GOTO1 =A(COMP1R)                                                       
         BE    RPT12                       YES - CONTINUE                       
         MVI   TOTSW,C'2'                  NO  - SET FOR CAT TOTAL              
         BAS   RE,ACCTOT                                                        
*                                                                               
RPT12    MVI   BYTE,EMPLEVB                                                     
         GOTO1 =A(COMP1R)                                                       
         BE    RPT15                       YES - CONTINUE                       
         MVI   TOTSW,C'3'                  NO  - SET FOR DPT TOTAL              
         BAS   RE,ACCTOT                                                        
*                                                                               
RPT15    MVI   BYTE,EMPLEVA                                                     
         GOTO1 =A(COMP1R)                                                       
         BE    RPT20                       YES - CONTINUE                       
         MVI   TOTSW,C'4'                  NO  - SET FOR OFF TOTAL              
         BAS   RE,ACCTOT                                                        
*                                                                               
RPT20    MVC   LSTWRK(SRTLNQ),SRTWRK       SAVE THIS ONE                        
         B     RPT2                        AND GET NEXT.                        
         SPACE 2                                                                
*                                      *FINAL TOTALS*                           
RPT90    BAS   RE,ACCTOT                                                        
         MVI   TOTSW,C'2'                 CAT TOTAL  (1R)                       
         BAS   RE,ACCTOT                                                        
         MVI   TOTSW,C'3'                 DPT TOTAL  (1R)                       
         BAS   RE,ACCTOT                                                        
         MVI   TOTSW,C'4'                 OFF TOTAL  (1R)                       
         BAS   RE,ACCTOT                                                        
*                                                                               
RPT99    MVI   TOTSW,C'5'                        SET FOR REPORT TOTAL           
         BAS   RE,ACCTOT                                                        
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         B     XIT                                                              
         EJECT                                                                  
LASTRUN  CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         GOTO1 =A(RELBUFF),DMCB,(RC) RELEASE GETMAINED SPACE                    
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SET THE IN OUT DATES FOR A PERSON                                      
*        SET HIREDATE AND FIREDATE                                              
*----------------------------------------------------------------------         
SETINOUT NTR1                                                                   
*                                                                               
         LA    R0,PERLMAX        CLEAR IN OUT DATES                             
         LA    R1,PERINOUT                                                      
SIO05    MVC   0(3,R1),=XL3'FFFFFF' ASSURE FAILURE IF NOT SET                   
         XC    3(3,R1),3(R1)                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,SIO05                                                         
*                                                                               
         TM    PERSTAT,PERISNEW    IS THIS PERSON ON NEW COST                   
         BZ    SIOX                                                             
*                                                                               
         USING PERD,R2                                                          
         L     R2,APERBLK                                                       
         XR    R1,R1                                                            
         ICM   R1,1,PERLNUM        PERSON WAS NEVER ANYWHERE!!                  
         BZ    SIOX                                                             
         LA    R0,PERLMAX                                                       
         CR    R1,R0                                                            
         BNH   *+6                                                              
         DC    H'0'                MORE THAN MAX INOUTS!!                       
*                                                                               
         LA    R4,PERINOUT                                                      
         LA    R3,PERLVALS                                                      
         USING PERLVALS,R3                                                      
SIO10    MVC   0(3,R4),PERLSTD                                                  
         MVC   3(3,R4),PERLENDD                                                 
         LA    R4,6(R4)                                                         
         LA    R3,PERLVLEN(R3)                                                  
         BCT   R1,SIO10                                                         
*                                                                               
SIOX     XIT1                                                                   
         DROP  R2,R3                                                            
         EJECT                                                                  
************************************                                            
*  REFRESH NAMES WHERE NECESSARY   *                                            
*  REPORT LEVEL TITLES             *                                            
************************************                                            
REFRES   NTR1                                                                   
         GOTO1 =A(REFEXT)          REFRESH HEADER INFO                          
***************************                                                     
*  PRINT THE DETAIL LINE  *                                                     
*  UPDATE TOTALS          *                                                     
***************************                                                     
PRNT     MVC   XP,XSPACES              CLEAR FIRST PRINT LINE                   
         MVC   XPSECOND,XSPACES        CLEAR 2ND PRINT LINE                     
         LA    R6,LSTWRK                                                        
         BAS   RE,CLEARBLK             CLEAR PRINT BLOCK                        
         MVC   AMNTTBLE(AMNTLENQ),XSPACES    CLEAR TABLE                        
*                                                                               
         CLC   SRTSJ,SPACES               IS THERE AN SJ ACCOUNT                
         BE    PRNT28                     NO- ITS AN BUD OR HRS REC             
         MVC   PRNTBLOC+1(12),SRTSJ       SJCODE                                
         MVC   PRNTBLOC+14(30),CLNAME     NAME                                  
*                                                                               
         LA    R2,SRTBLHRS                                                      
         LA    R3,BILHR                                                         
         BAS   RE,EDITHR                                                        
         LA    R2,SRTBLVAL                                                      
         LA    R3,BILDOL                                                        
         BAS   RE,EDITAMT                                                       
*                                                                               
         CP    SRTSTAND,=P'0'                                       *           
         BE    PRNT05                                               *           
         CP    SRTBLHRS,=P'0'                                       *           
         BE    PRNT05                                               *           
         CP    SRTTARGT,=P'0'                                       *           
         BE    PRNT05                                               *           
         ZAP   ANSWER,SRTBLHRS                         BILLABLE HRS *           
*                                                                               
         ZAP   DOUBLE,SRTSTAND                                                  
         MP    DOUBLE,SRTTARGT                                                  
*                                                                               
*                                                                               
         DP    ANSWER,DOUBLE                           STANDARD HRS *           
*                                                                               
         SRP   ANSWER(7),64-1,5                                     *           
*                                                                               
         LA    R3,BILSTHR                                                       
         BAS   RE,EDITPCT                                                       
*                                                                   *           
PRNT05   LA    R2,SRTHRSBL                                                      
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
*** TIME HELD = BILLABLE HRS -(WRITE-OFF HRS + HOURS BILLED)        *           
         ZAP   ANSWER,SRTBLHRS                 BILLABLE HRS         *           
         SP    ANSWER,SRTWOHR                HRS WRITTEN-OFF        *           
         SP    ANSWER,SRTHRSBL               HOURS BILLED           *           
         LA    R2,ANSWER+7                                                      
         LA    R3,HELDHR                                                        
         BAS   RE,EDITHR                                                        
*                                                                               
*** TIME $ HELD = BILLABLE $ -(WRITE-OFF $ + BILLED $)              *           
         ZAP   ANSWER,SRTBLVAL                 BILLABLE $           *           
         SP    ANSWER,SRTWODO                $ WRITE-OFF            *           
         SP    ANSWER,SRTBILLD               $ BILLED               *           
         LA    R2,ANSWER+7                                                      
         LA    R3,HELDDOL                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
*** BILLED VS VALUE HOURS = HOURS BILLED - BILLABLE HOURS           *           
         ZAP   ANSWER,SRTHRSBL                 HOURS BILLED         *           
         SP    ANSWER,SRTBLHRS                 BILLABLE HOURS       *           
         LA    R2,ANSWER+7                                                      
         LA    R3,BVVHR                                                         
         BAS   RE,EDITHR                                                        
*                                                                   *           
         ZAP   ANSWER,SRTBILLD                 $ BILLED             *           
         SP    ANSWER,SRTBLVAL               BILLABLE $             *           
         LA    R2,ANSWER+7                                                      
         LA    R3,BVVDOL                                                        
         BAS   RE,EDITAMT                                                       
*                                                                               
         CP    SRTBLHRS,=P'0'                                       *           
         BE    PRNT24                                               *           
         CP    SRTHRSBL,=P'0'                                       *           
         BE    PRNT24                                               *           
         ZAP   ANSWER,SRTHRSBL                 VALUE VS BILLED      *           
         MP    ANSWER,=P'10000'                (PCT)                *           
         DP    ANSWER,SRTBLHRS                                      *           
         SRP   ANSWER(7),64-1,5                                     *           
         LA    R3,BVVPER                                                        
         BAS   RE,EDITPCT                                                       
*                                                                               
*** BILLED VS STAND HOURS = HOURS BILLED - STANDARD HOURS           *           
         CP    SRTSTAND,=P'0'                                       *           
         BE    PRNT24                                               *           
         ZAP   ANSWER,SRTHRSBL                HOURS BILLED          *           
         SP    ANSWER,DOUBLE                  STANDATD HOURS        *           
         LA    R2,ANSWER+7                                                      
         LA    R3,BVSHR                                                         
         BAS   RE,EDITHR                                                        
*                                                                   *           
         ZAP   ANSWER,SRTBILLD                $ BILLED              *           
         SP    ANSWER,SRTSTRAT              BILLABLE $              *           
         LA    R2,ANSWER+7                                                      
         LA    R3,BVSDOL                                                        
         BAS   RE,EDITAMT                                                       
*                                                                   *           
         MVC   BVSPER,SPACES                                                    
         CP    SRTSTAND,=P'0'                                       *           
         BE    PRNT24                                               *           
         CP    SRTHRSBL,=P'0'                                       *           
         BE    PRNT24                                               *           
         ZAP   ANSWER,SRTHRSBL                VALUE VS STAND        *           
         MP    ANSWER,=P'10000'               (PCT)                 *           
         DP    ANSWER,SRTSTAND                                      *           
         SRP   ANSWER(7),64-1,5                                     *           
         LA    R3,BVSPER                                                        
         BAS   RE,EDITPCT                                                       
*                                                                   *           
PRNT24   ZIC   RF,LINE                    PRESENT LINE                          
         AH    RF,=H'5'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   PRNT25                                                           
         GOTO1 =A(HEADUP)                 HEAD UP NEW PAGE                      
*                                                                               
PRNT25   DS    0H                                                               
         GOTO1 =A(BLDBLC),DMCB,(RC)                                             
         OC    PRNTBLOC(165),XSPACES                                            
         OC    PRNTBLOC+165(165),XSPACES                                        
         MVC   XP(165),PRNTBLOC                                                 
         MVC   XPSECOND(165),PRNTBLOC+165                                       
         GOTO1 ACREPORT                   AND PRINT IT                          
         CLC   PRNTBLOC+165(165),XSPACES                                        
         BNE   PRNT28                                                           
         GOTO1 ACREPORT                                                         
*********************                                                           
*   UPDATE TOTALS   *                                                           
*********************                                                           
PRNT28   LA    R0,LEVCOUNT                NUMBER OF REPORT LEVEL TOTALS         
         L     R1,AACCUMS                                                       
PRNT30   AP    0(7,R1),SRTTARGT           ADD TARGET TO TOTALS                  
         AP    7(7,R1),SRTHRACT               HOURS                             
         AP    14(7,R1),SRTSTAND              STANDARD HOURS                    
         AP    21(7,R1),SRTBLHRS              BILLABLE HOURS                    
         AP    28(7,R1),SRTHRSBL              HOURS BILLED                      
         AP    35(7,R1),SRTBLVAL              BILLING VALUE                     
         AP    42(7,R1),SRTBILLD              TIME BILLED                       
         AP    49(7,R1),SRTSTRAT              STAND RATE $                      
         AP    56(7,R1),SRTWOHR               WO HOURS                          
         AP    63(7,R1),SRTWODO               WO DOLLARS                        
         ZAP   PL16,SRTSTAND             CALC STD HRS X TRG PCT                 
         MP    PL16,SRTTARGT                                                    
         SRP   PL16,64-4,5                                                      
         AP    70(7,R1),PL16                                                    
         LA    R1,TOTLEN(R1)              NEXT LEVEL OF ACCUMS                  
         BCT   R0,PRNT30                                                        
         B     XIT                                                              
         EJECT                                                                  
******************                                                              
*  REPORT TOTALS *                                                              
******************                                                              
ACCTOT   NTR1                                                                   
         USING TOTALD,R5                                                        
         BAS   RE,CLEARBLK                                                      
         MVC   PRN2BLOC(165),XSPACES                                            
         MVC   XP(165),XSPACES                                                  
         MVC   XPSECOND(165),XSPACES                                            
         CLI   QOPT1,C'E'          EMPLOYEE LEVEL AS DETAIL                     
         BNE   ACTOTA              NO                                           
         CLI   TOTSW,C'1'          DON'T PRINT "TOTALS FOR" ON A DETAIL         
         BE    ACTOT01                                                          
*                                                                               
ACTOTA   MVC   PRN2BLOC(11),=C' TOTALS FOR'                                     
         CLI   TOTSW,C'1'                 IS IT A 1R EMPLOYEE TOTAL             
         BE    ACTOT01                                                          
         CLI   TOTSW,C'2'                            CATEGORY TOTAL             
         BE    ACTOT02                                                          
         CLI   TOTSW,C'3'                            DEPT TOTAL                 
         BE    ACTOT03                                                          
         CLI   TOTSW,C'4'                            OFFICE TOTAL               
         BE    ACTOT04                                                          
         CLI   TOTSW,C'5'                            REPORT TOTAL               
         BE    ACTOT05                                                          
         SPACE 2                                                                
*                                    ***EMPLOYEE TOTAL***                       
*                                                                               
         USING ACCUMSD,R5                                                       
ACTOT01  L     R5,AACCUMS                                                       
         LA    R5,EMTOT                  ADDR OF EMPLOYEE ACCUMS                
         LA    R6,BUKCONT1               NUMBER ACCUM BUCKETS TO CLEAR          
         MVI   BYTE,EMPLEVD                                                     
         GOTO1 =A(NOLEVEL)        IF BLANK, LEV TOT NOT NEEDED                  
         BE    ACTOT35                                                          
*                                                                               
ACTOT01A CLI   QOPT1,C'E'                IS THIS AN EMPLOYEE LEVEL REP          
         BE    ACTOT20                   YES, DONT NEED "TOTALS FOR"            
*                                                                               
         LA    R4,PRN2BLOC+12                                                   
         LA    R5,EMPCODE                                                       
         MVI   BYTE,EMPLEVD                                                     
         GOTO1 =A(MOVE1R)                                                       
         MVC   PRN2BLOC+20(35),EMPLNAME   NAME                                  
         L     R5,AACCUMS          RESTORE A(EMPLOYEE ACCUMS)                   
         LA    R5,EMTOT                                                         
         B     ACTOT20                   PUT OUT TOTALS                         
*                                                                               
*                                    ***CATEGORY TOTAL***                       
         USING ACCUMSD,R5                                                       
ACTOT02  L     R5,AACCUMS                ADDR OF CATEGORY ACCUMS                
         LA    R5,CTTOT                                                         
         LA    R6,BUKCONT2               NUMBER ACCUM BUCKETS TO CLEAR          
         MVI   BYTE,EMPLEVC                                                     
         GOTO1 =A(NOLEVEL)                                                      
         BE    ACTOT35                                                          
         MVI   BYTE,3                                                           
         LA    R5,EMPCODE          MOVE CAT TO PRINT                            
         LA    R4,PRN2BLOC+12                                                   
         GOTO1 =A(MOVE1R)                                                       
         MVC   PRN2BLOC+15(30),CATNAME    NAME                                  
         L     R5,AACCUMS                ADDR OF CATEGORY ACCUMS                
         LA    R5,CTTOT                                                         
         B     ACTOT20                   PUT OUT TOTALS                         
*                                                                               
*                                    ***DEPARTMENT TOTAL***                     
*                                                                               
         USING ACCUMSD,R5                                                       
ACTOT03  L     R5,AACCUMS                                                       
         LA    R5,DPTTOT                 ADDR OF DEPT ACCUMS                    
         LA    R6,BUKCONT3               NUMBER ACCUM BUCKETS TO CLEAR          
         MVI   BYTE,EMPLEVB                                                     
         GOTO1 =A(NOLEVEL)                                                      
         BE    ACTOT35                                                          
         MVI   BYTE,EMPLEVB        PRINT DEPARTMENT                             
         LA    R5,EMPCODE                                                       
         LA    R4,PRN2BLOC+12                                                   
         GOTO1 =A(MOVE1R)                                                       
         MVC   PRN2BLOC+15(30),DPNAME     NAME                                  
         L     R5,AACCUMS                ADDR OF CATEGORY ACCUMS                
         LA    R5,DPTTOT                 ADDR OF DEPT ACCUMS                    
         B     ACTOT20                   PUT OUT TOTALS                         
*                                                                               
*                                    ***OFFICE TOTAL***                         
*                                                                               
         USING ACCUMSD,R5                                                       
ACTOT04  L     R5,AACCUMS                                                       
         LA    R5,OFTOT                  ADDR OF OFFICE ACCUMS                  
         LA    R6,BUKCONT4               NUMBER ACCUM BUCKETS TO CLEAR          
         MVI   BYTE,EMPLEVA        PRINT OFFICE                                 
         LA    R5,EMPCODE                                                       
         LA    R4,PRN2BLOC+12                                                   
         GOTO1 =A(MOVE1R)                                                       
         MVC   PRN2BLOC+15(30),OFNAME     NAME                                  
         L     R5,AACCUMS                                                       
         LA    R5,OFTOT                  ADDR OF OFFICE ACCUMS                  
         B     ACTOT20                   PUT OUT TOTALS                         
*                                                                               
*                                        ***REPORT TOTAL***                     
*                                                                               
         USING ACCUMSD,R5                                                       
ACTOT05  L     R5,AACCUMS                ADDR OF CATEGORY ACCUMS                
         LA    R5,RPTOT                  ADDR OF REPORT TOTAL ACCUMS            
         LA    R6,BUKCOUNT               NUMBER ACCUM BUCKETS TO CLEAR          
         MVC   PRN2BLOC+12(6),=C'REPORT'  REPORT TOTAL                          
         SPACE 2                                                                
         USING TOTALD,R5                                                        
ACTOT20  DS    0H                                                 *             
         MVC   STDHR,SPACES        CLEAR OUT PRINT FIELDS                       
         MVC   STDVALUE,SPACES                                                  
         MVC   BILSTHR,SPACES                                                   
         SRP   TOTTAR,64-1,0                                      *             
*                                                                               
*-------------------------------------------------------------------            
*        1R TOTALS                                                              
*                                                                               
*-------------------------------------------------------------------            
*        PRINT THE TARGET PERCENTAGE                                            
*-------------------------------------------------------------------            
         MVC   TRGT,SPACES                                                      
         CLI   TOTSW,C'1'          EMPLOYEE LEVEL TOTAL                         
         BNE   ACTOT20D            NO, DONT PRINT TARGET PCT                    
*                                                                               
         CLI   PROFILES,0          NO TRG BUD DEFINED                           
         BNE   ACTOT20B            YES, USE IT                                  
*                                                                               
         CLI   PROFILES+4,0        IS THERE A DEFAULT                           
         BE    ACTOT20D            NO, DON'T PRINT ANYTHING                     
*                                                                               
ACTOT20B EDIT  (P7,TOTTAR),(5,TRGT),1,MINUS=YES                                 
*                                                                               
         TM    BUDSTAT,NOBUD       WAS THIS THE DEFAULT BUD                     
         BNO   ACTOT20D            NO,                                          
*                                                                               
         MVI   TRGT+5,C'*'         PUT AN * AFTER IT                            
*                                                                               
ACTOT20D LA    R2,TOTHRS                                                        
         LA    R3,ACTHR                                                         
         BAS   RE,EDITHR           ACTUAL HOURS                                 
*                                                                               
         CP    TOTSTAND,=P'0'                                                   
         BE    ACTOT22                                                          
*                                                                               
         LA    R2,TOTSTAND                                                      
         LA    R3,STDHR                                                         
         BAS   RE,EDITHR           STAND HRS  *                                 
*                                                                               
         LA    R2,TOTRATE                                                       
         LA    R3,STDVALUE                                                      
         BAS   RE,EDITAMT          STAND RATE                                   
*                                                                               
*                                                                               
ACTOT22  EQU   *                                                                
         MVC   AMNTTBLE(AMNTLENQ),XSPACES    CLEAR TABLE                        
*                                                                               
*-------------------------------------------------------------------            
*        BILLABLE VALUE COL, HOURS AND DOLLARS AND PERCENT                      
*                                                                               
*-------------------------------------------------------------------            
         LA    R2,TOTBLHRS                                                      
         LA    R3,BILHR                                                         
         BAS   RE,EDITHR                                                        
*                                                                               
         LA    R2,TOTBLVAL                                                      
         LA    R3,BILDOL                                                        
         BAS   RE,EDITAMT                                                       
*                                                                               
         CP    TOTSTAND,=P'0'                                                   
         BE    ACTOT24                                                          
         ZAP   ANSWER,TOTBLHRS     BILLABLE HRS/STAND HRS (PCT)   *             
         MP    ANSWER,=P'1000'                                                  
         DP    ANSWER,TOTSTAND                                                  
         LA    R3,BILSTHR                                                       
         BAS   RE,EDITPCT                                                       
*                                                                               
*-------------------------------------------------------------------            
*        TIME BILLED COL, HOURS AND DOLLARS                                     
*-------------------------------------------------------------------            
ACTOT24  LA    R2,TOTHRSBL                                                      
         LA    R3,BILDHR                                                        
         BAS   RE,EDITHR                                                        
*                                                                               
         LA    R2,TOTBILLD                                                      
         LA    R3,BILDDOL                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
*-------------------------------------------------------------------            
*        TIME WRITE OFF COL, HOURS AND DOLLARS                                  
*-------------------------------------------------------------------            
         LA    R2,TOTWOHR                                                       
         LA    R3,WOHR                                                          
         BAS   RE,EDITHR                                                        
*                                                                               
         LA    R2,TOTWODO                                                       
         LA    R3,WODOL                                                         
         BAS   RE,EDITAMT                                                       
*                                                                               
*-------------------------------------------------------------------            
*        TIME HELD COL, HOURS AND DOLLARS                                       
*-------------------------------------------------------------------            
*  TIME HELD = BILLABLE HR -(WRITE-OFF HRS + HRS BILLED)          *             
         ZAP   ANSWER,TOTBLHRS         BILLABLE HOURS             *             
         SP    ANSWER,TOTWOHR        - WRITE-OFFS HOURS           *             
         SP    ANSWER,TOTHRSBL       - HOURS BILLED               *             
         LA    R2,ANSWER+7                                                      
         LA    R3,HELDHR                                                        
         BAS   RE,EDITHR                                                        
*                                                                               
*  TIME HELD = BILLABLE $ - (WRITE-OFF $ + $ BILLED)              *             
         ZAP   ANSWER,TOTBLVAL         BILLABLE $                 *             
         SP    ANSWER,TOTWODO        - WRITE-OFF $                *             
         SP    ANSWER,TOTBILLD       - $BILLED                    *             
         LA    R2,ANSWER+7                                                      
         LA    R3,HELDDOL                                                       
         BAS   RE,EDITAMT                                                       
*                                                                               
*-------------------------------------------------------------------            
*        BILLED VS VALUE COL                                                    
*-------------------------------------------------------------------            
* BILLED HOURS - BILLABLE HOURS                                   *             
         ZAP   ANSWER,TOTHRSBL          HOURS BILLED              *             
         SP    ANSWER,TOTBLHRS          - BILLABLE HOURS          *             
         LA    R2,ANSWER+7                                                      
         LA    R3,BVVHR                                                         
         BAS   RE,EDITHR                                                        
*                                                                               
* BILLED $ - BILLABLE $                                           *             
         ZAP   ANSWER,TOTBILLD          $ BILLED                  *             
         SP    ANSWER,TOTBLVAL          - BILLABLE $              *             
         LA    R2,ANSWER+7                                                      
         LA    R3,BVVDOL                                                        
         BAS   RE,EDITAMT                                                       
*                                                                               
* BILLED HRS/ BILLABLE HRS                                        *             
         CP    TOTBLHRS,=P'0'                                                   
         BE    ACTOT26                                                          
         CP    TOTHRSBL,=P'0'                                                   
         BE    ACTOT26                                                          
         ZAP   ANSWER,TOTHRSBL          HOURS BILLED              *             
         MP    ANSWER,=P'10000'         X 100                     *             
         DP    ANSWER,TOTBLHRS          /BILLABLE HOURS           *             
         SRP   ANSWER(7),64-1,5         BILLED VS VALUE %         *             
         LA    R3,BVVPER                                                        
         BAS   RE,EDITPCT                                                       
*                                                                               
*-------------------------------------------------------------------            
*        BILLED VS STAND COL                                                    
*-------------------------------------------------------------------            
* BILLED HRS - STANDARD HRS                                       *             
ACTOT26  ZAP   ANSWER,TOTHRSBL          HOURS BILLED              *             
         SP    ANSWER,TOTHRXPC          - STANDARD HOURS X TARG PCT             
         LA    R2,ANSWER+7                                                      
         LA    R3,BVSHR                                                         
         BAS   RE,EDITHR                                                        
*                                                                               
* BILLED $ - STANDARD $                                           *             
         ZAP   ANSWER,TOTBILLD          $ BILLED                  *             
         SP    ANSWER,TOTRATE           - STANDARD RATE X HR $    *             
         LA    R2,ANSWER+7                                                      
         LA    R3,BVSDOL                                                        
         BAS   RE,EDITAMT                                                       
*                                                                               
* BILLED HRS/STANDARD HRS                                         *             
         CP    TOTHRXPC,=P'0'           STAND HOURS X TARG PCT ZERO?            
         BE    ACTOT28                                                          
         CP    TOTHRSBL,=P'0'                                                   
         BE    ACTOT28                                                          
*                                                                               
         ZAP   ANSWER,TOTHRSBL          HOURS BILLED                            
         MP    ANSWER,=P'10000'                                                 
         DP    ANSWER,TOTHRXPC                                                  
         SRP   ANSWER(7),64-1,5         BILLED VS STANDARD(%)                   
         LA    R3,BVSPER                                                        
         BAS   RE,EDITPCT                                                       
*                                                                               
ACTOT28  DS    0H                                                               
         BAS   RE,CLEARBLK                                                      
         GOTO1 =A(BLDBLC),DMCB,(RC)                                             
         OC    PRNTBLOC(165),XSPACES                                            
         OC    PRNTBLOC+165(165),XSPACES                                        
*                                                                               
         MVC   PRNTBLOC+1(9),=C'STD HRS  '                       *              
         MVC   PRNTBLOC+12(9),=C'STD VALUE'                      *              
         CLI   ISBURSON,C'Y'                                                    
         BE    *+10                                                             
         MVC   PRNTBLOC+27(5),=C'TRG %'                          *              
         MVC   PRNTBLOC+35(7),=C'TOT HRS  '                      *              
*                                                                               
         MVC   PRNTBLOC+166(9),STDHR                             *              
         MVC   PRNTBLOC+177(13),STDVALUE                         *              
         MVC   PRNTBLOC+200(9),ACTHR                             *              
         CLI   ISBURSON,C'Y'                                                    
         BE    *+10                                                             
         MVC   PRNTBLOC+192(6),TRGT                              *              
*                                                                               
         LA    R2,XP                                                            
         CLC   PRN2BLOC,XSPACES                                                 
         BE    ACTOT28A                                                         
         MVC   0(L'PRN2BLOC,R2),PRN2BLOC                                        
*                                                                               
         CLI   TOTSW,C'1'          EMPLOYEE TOTAL                               
         BNE   *+8                 NO                                           
         BAS   RE,HIREFIRE         PRINT HIRE FIRE DATES                        
         LA    R2,L'XP(R2)                                                      
*                                                                               
ACTOT28A MVC   0(L'PRNTBLOC,R2),PRNTBLOC                                        
         LA    R2,L'XP(R2)                                                      
         MVC   0(L'PRNTBLOC,R2),PRNTBLOC+165                                    
         GOTO1 ACREPORT                 SKIP A LINE                             
*                                                                               
         CLI   ISBURSON,C'Y'                                                    
         BNE   ACTOT28C                                                         
*                                                                               
         CLI   TOTSW,C'1'          EMPLOYEE TOTAL                               
         BNE   ACTOT28C            NO                                           
         GOTO1 =A(BURTPC)          PRINT THEIR TARG PCT                         
*                                                                               
ACTOT28C ZIC   RF,LINE                  PRESENT LINE                            
         AH    RF,=H'5'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   ACTOT30                                                          
         GOTO1 =A(HEADUP)              HEAD UP NEW PAGE                         
*                                                                               
ACTOT30  CLI   TOTSW,C'1'               EMPLOYEE LEVEL TOTAL                    
         BNE   ACTOT30A                 NO - CONTINUE                           
         CLI   QOPT1,C'E'               PRINT EMPLOYEE AS DETAIL LINE           
         BE    *+8                      YES - DONT SKIP LINES                   
ACTOT30A MVI   SPACING,2                                                        
*                                                                               
         GOTO1 ACREPORT                AND PRINT IT                             
*                                                                               
ACTOT35  ZAP   0(7,R5),=P'0'           CLEAR LEVEL ACCUMS                       
         LA    R5,7(R5)                                                         
         BCT   R6,ACTOT35                                                       
*                                                                               
         MVI   TOTSW,C'1'              RESET TO CAT TOTAL                       
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
PUTSORT  NTR1                             ** PUT RECORD TO SORT **              
*----------------------------------------------------------------------         
         USING SRTD,R6                                                          
         LA    R6,SRTWRK                                                        
         LR    R1,R6                         ADDR OF SORT REC INTO R1           
         LA    R1,SBUKLOC(R1)                START OF COL BUCKETS               
         LA    R0,SBUKCONT                   NUMBER OF BUCKETS INTO R0          
*                                                                               
PUT00    CP    0(7,R1),=P'0'                 CONTAIN PACKED ZEROS               
         BNE   PUT01                         NOT ZERO-WE WANT IT                
         LA    R1,7(R1)                      BUMP TO NEXT BUCKET                
         BCT   R0,PUT00                                                         
         B     XIT                                                              
*                                                                               
PUT01    CLI   QOPT2,C'S'                    1R EMP,DEPT/CAT SUPPRESSED         
         BNE   PUT02                         NO - CHECK NEXT LEVEL              
*                                                                               
         MVI   BYTE,EMPLEVB                                                     
         GOTO1 =A(CLEAR1R)                   YES - CLEAR DEPT/CAT CODE          
         MVI   BYTE,EMPLEVC                                                     
         GOTO1 =A(CLEAR1R)                                                      
*                                                                               
PUT02    CLI   QOPT1,C' '                    SJ JOB LEVEL REPORT                
         BE    PUT99                         YES - PUT TO SORT                  
         CLI   QOPT1,C'P'                    PRODUCT LEVEL REPORT               
         BNE   PUT03                                                            
         MVC   SRTJOB,SPACES                                                    
         B     PUT99                                                            
PUT03    CLI   QOPT1,C'C'                    CLIENT LEVEL REPORT                
         BNE   PUT04                                                            
         MVC   SRTPROD(9),SPACES                                                
         B     PUT99                                                            
PUT04    CLI   QOPT1,C'E'                    EMPLOYEE LEVEL REPORT              
         BNE   PUT99                                                            
         MVC   SRTSJ,SPACES                                                     
*                                                                               
PUT99    OC    ALSORT,ALSORT       IS SORTER OPENED                             
         BNZ   PUT100              YES                                          
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
PUT100   GOTO1 ADSORTER,DMCB,=C'PUT',(R6)                                       
         MVI   ALSORT,1                      ACTIVITY SWITCH                    
*                                                                               
         CLC   QUESTOR(4),=C'DUMP'                                              
         BNE   XIT                                                              
         LA    R6,=C'SORT'                                                      
         LA    R2,SRTLNQ                                                        
         LA    R4,SRTWRK                                                        
         GOTO1 =V(PRNTBL),DMCB,(4,(R6)),(R4),C'DUMP',(R2),=C'2D',(C'P',X        
               PRINT)                                                           
         LA    R6,=C'I_O '                                                      
         LA    R2,PERLMAX*6                                                     
         LA    R4,PERINOUT                                                      
         GOTO1 =V(PRNTBL),DMCB,(4,(R6)),(R4),C'DUMP',(R2),=C'2D',(C'P',X        
               PRINT)                                                           
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
CLERSORT NTR1                    **  CLEAR SORT RECORD AREA **                  
*----------------------------------------------------------------------         
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
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*  CHECK IF A CLIENT IS NEW BUSINESS OR PRO BONO                                
*        CLIENT IS PASSED IN WORK                                               
*        R0 IS NONZERO IF FOUND                                                 
*----------------------------------------------------------------------         
CHKNBPB  NTR1                                                                   
         USING BIND,R5                                                          
         SR    R0,R0                                                            
         CLI   PROFILES+3,C'Y'     INCLUDE NEW BUS/PRO BONO ACCOUNTS            
         BE    CHKCX               YES                                          
         L     R5,ACLITAB          TABLE OF NB/PB ACCOUNTS                      
         L     R0,BININ                                                         
         LA    R2,BINTABLE                                                      
         LTR   R0,R0                                                            
         BZ    CHKCX                                                            
         GOTO1 BINSRCH,DMCB,(0,WORK),(R2),(R0),NBPBLEN,(0,NBPNKLN),(R0)         
         CLI   DMCB,0              RECORD FOUND?                                
         BE    CHKCX               YES                                          
         SR    R0,R0               NO                                           
*                                                                               
CHKCX    XIT1  REGS=(R0)           ZERO IN R0 IF NOT FOUND                      
         EJECT                                                                  
*----------------------------------------------------------------------         
*  CLEAR PRINTBLOCK *                                                           
*----------------------------------------------------------------------         
CLEARBLK NTR1                                                                   
         LA    R1,PRNTBLOC                                                      
         LA    RE,2                                                             
         MVC   0(165,R1),XSPACES                                                
         LA    R1,165(R1)                                                       
         BCT   RE,*-10                                                          
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------             
*        PRINT FIRE DATE AT WHEREEVER IT WILL FIT                               
*------------------------------------------------------------------             
HIREFIRE EQU   *                                                                
         CLI   FIREDATE,X'FF'                                                   
         BER   RE                                                               
*                                                                               
         ST    RE,SAVERE                                                        
         CLC   41(5,R2),XSPACES                                                 
         BNH   *+8                                                              
         LA    R2,L'XP(R2)                                                      
*                                                                               
         MVC   42(5,R2),=C'TERM='                                               
         GOTO1 DATCON,DMCB,(1,FIREDATE),(5,47(R2))                              
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
**************************************************************                  
*              DATAMGR INTERFACE                                                
**************************************************************                  
HIGH     MVC   COMMAND,=C'DMRDHI'            READ HIGH                          
         MVC   FILE,=C'ACCDIR'                                                  
         MVC   SAVEKEY,0(R7)                                                    
         B     GTREC                                                            
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'            READ SEQUENTIAL                    
         MVC   FILE,=C'ACCDIR'                                                  
         MVC   SAVEKEY,0(R7)                                                    
         B     GTREC                                                            
*                                                                               
GET      NTR1  WORK=(R2,12)                                                     
         MVC   COMMAND,=C'GETREC'  GET A RECORD                                 
         MVC   FILE,=CL8'ACCMST'                                                
         L     R7,ACREC                                                         
         MVC   SAVEKEY,0(R7)                                                    
         USING ACCRECD,R3                                                       
         LA    R3,SAVEKEY                                                       
         LA    R3,ACCKDA           ADDRESS DISK ADDRESS FOR GETREC              
         GOTO1 DATAMGR,DMCB,COMMAND,FILE,(R3),(R7),(R2)                         
         B     XIT                                                              
*                                                                               
GTREC    NTR1  ,                                                                
         L     R7,ACREC                                                         
         GOTO1 DATAMGR,DMCB,COMMAND,FILE,(R7),(R7),(R2)                         
         CLI   DMCB+8,0                      TEST FOR ERRORS                    
         BE    *+6                                                              
         DC    H'0'                          DIE IF ERRORS FOUND                
         B     XIT                                                              
         SPACE 4                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 4                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------------          
*              EDITS                                                            
*---------------------------------------------------------------------          
*                                                                               
EDITPCT  ST    RE,SAVERE                                                        
         LA    R2,ANSWER                                                        
         EDIT  (P7,(R2)),(6,(R3)),1,TRAIL=C'%'                                  
         B     EDITBR                                                           
*                                                                               
EDITHR   ST    RE,SAVERE                                                        
         EDIT  (P7,(R2)),(10,(R3)),2,MINUS=YES                                  
         B     EDITBR                                                           
*                                                                               
EDITAMT  ST    RE,SAVERE                                                        
         EDIT  (P7,(R2)),(13,(R3)),2,MINUS=YES,FLOAT=$                          
EDITBR   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
**************************************************************                  
*        READBUD, IF A BUDGET IS DEFINED AT THIS MODE, READ IT                  
**************************************************************                  
         USING BUDD,R2                                                          
READBUD  NTR1  ,                                                                
         MVC   MYKEYSV,KEY                                                      
         L     R4,ADACC                                                         
         LA    R2,BUSTDHRS                                                      
         CLC   BUDMODE,MODE                                                     
         BNE   READB30                                                          
         GOTO1 =A(GETBUD),DMCB,(R4),PROFILES+2,2,1,(RC)    STD HRS              
*                                                                               
READB30  EQU   *                                                                
         LA    R2,BUTRGPCT                                                      
         ZAP   BUDAMNT,=P'0'      SAVE TARGET PCT FROM GETBUD                   
         ZAP   TARGET,=P'0'                                                     
*                                                                               
         CLI   PROFILES,0          NO TRG BUD DEFINED                           
         BNE   READB40                                                          
*                                                                               
         ZIC   R1,PROFILES+4       USE DEFAULT TRG PCT FROM PROFILE             
         CVD   R1,DUB                                                           
         ZAP   BUDGAMT,DUB                                                      
         MP    BUDGAMT,=P'100'                                                  
         OI    BUDSTAT,NOBUD                                                    
         B     READB45                                                          
*                                                                               
READB40  CLC   BUDMODE,MODE                                                     
         BNE   READB50                                                          
*                                                                               
         GOTO1 =A(GETBUD),DMCB,(R4),PROFILES+0,0,1,(RC) TARGET                  
*                                                                               
READB45  ZAP   BUDAMNT,BUDGAMT    SAVE TARGET PCT FROM GETBUD                   
         ZAP   TARGET,BUDAMNT                                                   
*                                                                               
READB50  EQU   *                                                                
         CLI   MODE,PROCACC        READ STD RATES AT EMPLOYEE                   
         BNE   READB80             LEVEL, BUT PASS GETBUD THE CORRECT           
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
         BNE   READB80                                                          
*                                                                               
READB70  GOTO1 =A(GETBUD),DMCB,(R4),PROFILES+1,1,1,(RC) RATE                    
         ZAP   BUDAMNT,BUDGAMT    SAVE STD RATE FROM GETBUD                     
*                                                                               
READB80  MVC   KEY,MYKEYSV                                                      
         B     XIT                                                              
*                                                                               
*        USING ACKEYD,R7                                                        
*        L     R7,ACREC                                                         
*        MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES                               
*        MVC   ACKEYACC(42),MYKEYSV                                             
*        BAS   RE,HIGH                                                          
*        B     XIT                                                              
         EJECT                                                                  
SETBURSN LA    RF,BURAGYS                                                       
         XC    ISBURSON,ISBURSON                                                
         LA    R0,NBURSONS                                                      
SETBUR10 CLC   RCCOMPFL,0(RF)                                                   
         BNE   *+8                                                              
         MVI   ISBURSON,C'Y'                                                    
         LA    RF,1(RF)                                                         
         BCT   R0,SETBUR10                                                      
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
**************************************************************                  
*              CONSTANTS AND LITERALS                                           
**************************************************************                  
*                                                                               
RELOTAB  DS    0A                                                               
         DC    V(UNDERLIN)                                                      
         DC    V(GETSTD)                                                        
         DC    X'FF'                                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
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
BURAGYS  DC    AL1(BURSON,DDS3,DDSB,BURTST)                                     
NBURSONS EQU   *-BURAGYS                                                        
BURSON   EQU   X'94'                                                            
BURTST   EQU   X'6C'                                                            
DDS3     EQU   X'EF'                                                            
DDSB     EQU   X'DB'                                                            
         LTORG                                                                  
*--------------------------------------------------------------------           
* END OF BASE ADDRESSABILITY                                                    
*--------------------------------------------------------------------           
*                                                                               
         DROP  RB,R9                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*              ADD ITEM TO BINSRCH TABLE                                        
*              P1                  A(ITEM TO BE ADDED)                          
*              P2                  A(TABLE)                                     
*----------------------------------------------------------------------         
         USING BIND,R5                                                          
BINADD   NMOD1 0,**BINA**                                                       
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ              NUMBER LENGTH,KEY,MAX              
         LA    R6,BINTABLE                   A(TABLE)                           
         L     R4,0(R1)                      A(ITEM)                            
         GOTO1 BINSRCH,DMCB,(X'01',(R4)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                          TABLE IS FULL                      
         MVC   BININ,DMCB+8                  UPDATE COUNT                       
         XIT1                                                                   
         EJECT                                                                  
BURTPC   LR    R1,RC                                                            
         NMOD1 0,**BUTP**                                                       
         LR    RC,R1                                                            
*                                  GET TARG PCTS FROM EMPLIST                   
         L     R5,AEMPLST                                                       
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ                MOVE IN PARMS 3,4,5,6            
         LA    R0,BINTABLE                                                      
         LA    R6,LSTWRK           SORT REC DOING TOTALS FOR                    
         LA    R6,SRTEMPL-SRTD(R6) ADDRESS EMPLOYEE CODE                        
         MVC   WORK,SPACES                                                      
         MVC   WORK(1),RCCOMPFL                                                 
         MVC   WORK+1(2),=C'1R'    FOR SOME GODDAM REASON, UNIT LEDGER          
         MVC   WORK+3(12),SRTEMPL-SRTD(R6)                                      
         LA    R6,WORK                                                          
         GOTO1 BINSRCH,DMCB,(R6),(R0)                                           
         CLI   DMCB,0                                                           
         BNE   BTX                             NOT FOUND                        
*                                                                               
         L     R5,DMCB                                                          
         USING EMPCDE,R5                                                        
         MVC   TRGPLIST,EMPTRGP    RESTORE TARG PCT LIST                        
*                                                                               
         MVI   PRNTBLOC,C' '                                                    
         MVC   PRNTBLOC+1(L'PRNTBLOC-1),PRNTBLOC                                
         LA    R5,PRNTBLOC                                                      
         MVC   0(5,R5),=C'TRG %'                                                
         MVI   5(R5),C':'                                                       
         LA    R5,7(R5)                                                         
         LA    R6,TRGPLIST                                                      
*                                                                               
BT20     CLI   0(R6),X'FF'         END OF LIST?                                 
         BE    BT50                YES                                          
         MVC   FULL(2),0(R6)       EXTRACT YM                                   
         MVI   FULL+2,1            D IS 1                                       
         GOTO1 DATCON,DMCB,(1,FULL),(6,0(R5))                                   
         MVI   6(R5),C'='                                                       
         ZAP   DOUBLE,2(6,R6)                                                   
         SRP   DOUBLE,64-1,0                                                    
         EDIT  (P8,DOUBLE),(4,7(R5)),1,FILL=0                                   
*                                                                               
         CLI   8(R6),X'FF'         IS THIS THE LAST TABLE ENTRY                 
         BE    BT50                YES                                          
         MVI   11(R5),C','                                                      
         LA    R5,13(R5)           M                                            
         LA    R6,8(R6)                                                         
         B     BT20                                                             
*                                                                               
BT50     DS    0H                                                               
         GOTO1 CHOPPER,DMCB,(L'PRNTBLOC,PRNTBLOC),(57,XP+1),(L'XP,4)            
         ZIC   RF,LINE                    PRESENT LINE                          
         AH    RF,=H'2'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   BT70                                                             
         GOTO1 =A(HEADUP)                 HEAD UP NEW PAGE                      
BT70     GOTO1 ACREPORT                                                         
*                                                                               
BTX      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
REFEXT   LR    R1,RC               SAVE RC                                      
         NMOD1 0,**RFXX**                                                       
         LR    RC,R1               RESTORE RC                                   
         LA    R6,LSTWRK                                                        
         MVC   XP,XSPACES                                                       
         MVC   XPSECOND,XSPACES                                                 
*                                                                               
         USING ACKEYD,R7                                                        
         L     R7,ACREC                                                         
*                                         ** 1R NAMES **                        
         CLC   EMPCODE(SRTEMPLN),SRTEMPL  SAME EMPLOYEE                         
         BE    RFRS25                     YES- DONT REFRESH 1R NAMES            
*                                                                               
         LA    R4,ACKEYACC+3                                                    
         LA    R5,SRTEMPL                                                       
         MVI   BYTE,1                    SAME 1R LEVEL A                        
         GOTO1 =A(COMP1R)                                                       
         BE    RFRS16                                                           
*                                                                               
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES   CLEAR KEY                   
         MVC   ACKEYACC(3),EMPLOYEE       CO/U/L                                
         GOTO1 =A(MOVEK1R),DMCB,(R4),SRTEMPL,EMPLEVA                            
         BAS   RE,RFREAD                  READ FOR THAT RECORD                  
         BAS   RE,NAMEOUT                                                       
         MVC   OFNAME,WORK                SAVE 1R OFFICE NAME                   
         LA    R4,OFCODE                  SAVE 1R OFFICE CODE                   
         LA    R5,SRTEMPL                                                       
         MVI   BYTE,EMPLEVA                                                     
         GOTO1 =A(MOVE1R)                                                       
         GOTO1 =A(HEADUP)                                                       
*                                                                               
RFRS16   CLI   QOPT2,C'S'                 SUPRESS DEP/CAT                       
         BE    RFRS20                     YES,                                  
*                                                                               
         MVI   BYTE,2                    SAME 1R LEVEL B                        
         GOTO1 =A(COMP1R)                                                       
         BE    RFRS18                                                           
*                                                                               
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES   CLEAR KEY                   
         MVC   ACKEYACC(3),EMPLOYEE       CO/U/L                                
         LA    R4,ACKEYACC+3                                                    
         GOTO1 =A(MOVEK1R),DMCB,(R4),SRTEMPL,EMPLEVB                            
         BAS   RE,RFREAD                  READ FOR THAT RECORD                  
         BAS   RE,NAMEOUT                                                       
         MVC   DPNAME,WORK                SAVE 1R DEPT NAME                     
*                                                                               
         ZIC   RF,LINE                    PRESENT LINE                          
         AH    RF,=H'5'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   RFRS17                                                           
         GOTO1 =A(HEADUP)                 HEAD UP NEW PAGE                      
*                                                                               
RFRS17   MVC   XP+1(4),LVBTITLE                                                 
         MVI   BYTE,EMPLEVB                                                     
         LA    R5,SRTEMPL                                                       
         LA    R4,XP+6                                                          
         GOTO1 =A(MOVE1R)                                                       
         MVC   XP+13(32),DPNAME                                                 
         GOTO1 UNDERLIN,DMCB,(44,XP+1),(X'BF',XPSECOND+1)                       
         GOTO1 ACREPORT                                                         
*                                                                               
RFRS18   MVI   BYTE,3                    SAME 1R LEVEL C                        
         GOTO1 =A(COMP1R)                                                       
         BE    RFRS20                                                           
*                                                                               
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES   CLEAR KEY                   
         MVC   ACKEYACC(3),EMPLOYEE       CO/U/L                                
         LA    R4,ACKEYACC+3                                                    
         GOTO1 =A(MOVEK1R),DMCB,(R4),SRTEMPL,EMPLEVC                            
         BAS   RE,RFREAD                  READ FOR THAT RECORD                  
         BAS   RE,NAMEOUT                                                       
*                                                                               
         MVC   CATNAME,WORK               SAVE 1R CATEGORY NAME                 
         ZIC   RF,LINE                    PRESENT LINE                          
         AH    RF,=H'5'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   RFRS19                                                           
         GOTO1 =A(HEADUP)                 HEAD UP NEW PAGE                      
*                                                                               
RFRS19   MVC   XP+1(8),LVCTITLE                                                 
         MVI   BYTE,EMPLEVC                                                     
         LA    R5,SRTEMPL                                                       
         LA    R4,XP+10                                                         
         GOTO1 =A(MOVE1R)                                                       
         MVC   XP+13(32),CATNAME                                                
         GOTO1 UNDERLIN,DMCB,(44,XP+1),(X'BF',XPSECOND+1)                       
         GOTO1 ACREPORT                                                         
*                                                                               
RFRS20   MVI   BYTE,4                    SAME 1R LEVEL D                        
         GOTO1 =A(COMP1R)                                                       
         BE    RFRS25                                                           
         MVC   EMPLNAME,SRTENAME                                                
         MVC   HIREDATE,SRTHIRE                                                 
         MVC   FIREDATE,SRTFIRE                                                 
         MVC   BUDSTAT,SRTSTAT                                                  
         ZIC   RF,LINE                    PRESENT LINE                          
         AH    RF,=H'5'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   RFRS22                                                           
         GOTO1 =A(HEADUP)                 HEAD UP NEW PAGE                      
*                                                                               
RFRS22   MVC   XP+1(5),LVDTITLE                                                 
         MVI   BYTE,EMPLEVD                                                     
         LA    R5,SRTEMPL                                                       
         LA    R4,XP+7                                                          
         GOTO1 =A(MOVE1R)                                                       
         MVC   XP+14(32),SRTENAME                                               
         GOTO1 UNDERLIN,DMCB,(44,XP+1),(X'BF',XPSECOND+1)                       
*                                                                               
         CLI   QOPT1,C'E'                 EMPLOYEE DETAIL REPORT                
         BNE   RFRS23                                                           
         LA    R2,XP+48                                                         
         BAS   RE,RFHIFI                                                        
RFRS23   GOTO1 ACREPORT                                                         
*                                                                               
RFRS24   MVC   EMPCODE,SRTEMPL            MAKE NEW EMPLOYEE CURRENT             
*                                                                               
RFRS25   CLC   SRTSJ,SJCODE               SJ CODE SAME AS SAVED                 
         BE    RFX                        YES - DONT REFRESH SJ NAMES           
         CLC   SRTSJ,SPACES               IF NO JOB ITS BUD OR HRS REC          
         BE    RFX                                                              
*                                                                               
*                                         LOOK IN TABLE FIRST                   
         L     R5,ACLTLST                 ADDR OF CLTLIST                       
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ           MOVE IN PARMS 3,4,5,6                 
         LA    R0,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,SRTSJ,(R0)                                          
         CLI   DMCB,0                                                           
         BNE   RFRS27                     NOT FOUND IN TABLE                    
         L     R5,DMCB                                                          
         USING CLTCDE,R5                                                        
         MVC   CLNAME,CLTNAME             SJ CLIENT NAME FROM TAB               
         MVC   SJACCT+3(12),CLTKEY        SJ CLIENT CODE FROM TAB               
         B     RFX                                                              
*                                                                               
*                                         NOT IN TAB - LOOK IT UP               
RFRS27   MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES   CLEAR KEY                   
         MVC   SJCODE,SRTSJ               MOVE IN SJ CLI CODE                   
         MVC   ACKEYACC,SJACCT            KEY FOR READ                          
         BAS   RE,RFREAD                  READ FOR THAT RECORD                  
         BAS   RE,NAMEOUT                                                       
         MVC   CLNAME,WORK                SAVE SJ CLIENT NAME                   
*                                                                               
         MVC   WORK,SPACES                CLEAR TAB ENTRY WORK AREA             
         USING CLTCDE,R5                                                        
         LA    R5,WORK                                                          
         MVC   CLTNAME,CLNAME             UPDATE TABLE                          
         MVC   CLTKEY,SRTSJ                                                     
         GOTO1 =A(BINADD),DMCB,(R5),ACLTLST                                     
*                                                                               
RFX      XIT1                                                                   
         EJECT                                                                  
*RFSH SUPPORT                                                                   
*                                                                               
*------------------------------------------------------------------             
*        PRINT HIREDATE, FIRE DATE AT 0(R2)                                     
*------------------------------------------------------------------             
RFHIFI   NTR1                                                                   
         CLI   FIREDATE,X'FF'                                                   
         BE    RFX                                                              
         MVC   0(5,R2),=C'TERM='                                                
         GOTO1 DATCON,DMCB,(1,FIREDATE),(5,5(R2))                               
         B     RFX                                                              
         EJECT                                                                  
*                                                                               
RFREAD   NTR1                                                                   
         L     R7,ACREC                                                         
         MVC   COMMAND,=C'DMREAD'            A SPECIFIC READ                    
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R7),(R7)                       
         CLI   DMCB+8,0                      TEST FOR ERRORS                    
         BE    *+6                                                              
         DC    H'0'                          DIE IF ERRORS FOUND                
         B     RFX                                                              
*----------------------------------------------------------------------         
NAMEOUT  NTR1                             ** NAME LOOKUP **                     
*----------------------------------------------------------------------         
         L     R4,ACREC                                                         
         MVI   ELCODE,X'20'                                                     
         AH    R4,DATADISP                                                      
         XR    R1,R1                                                            
NO10     CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'20'                                                      
         BE    NO50                                                             
*                                                                               
         IC    R1,1(R4)                                                         
         LA    R4,0(R1,R4)                                                      
         B     NO10                                                             
*                                                                               
         USING ACNAMED,R4                                                       
NO50     MVC   WORK(36),SPACES                                                  
         CLI   1(R4),L'WORK        WILL THIS NAME FIT                           
         BNH   *+6                                                              
         DC    H'0'                NOPE                                         
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ACNMNAME                                                 
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
HEADUP   LR    R1,RC                                                            
         NMOD1 0,**HDUP**                                                       
         LR    RC,R1                                                            
*----------------------------------------------------------------------         
         MVI   FORCEHED,C'Y'                                                    
         MVC   XHEAD3+26(36),COMPNAM          COMPNAY NAME                      
         MVC   XHEAD4+17(2),OFCODE            1R OFFICE CODE                    
         MVC   XHEAD4+26(36),OFNAME                     NAME                    
         MVC   XHEAD7+124(38),BILLTHRU        BILLED THRU HEADLINE              
*                                                                               
         MVC   XHEAD10+56(107),HEADBLOC          COLUMN HEADING 1               
         MVC   XHEAD11+56(107),HEADBLOC+130      COLUMN HEADING 2               
         MVC   XHEAD12+56(107),HEADBLOC+260      COLUMN HEADING 3               
         GOTO1 ACREPORT                                                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------                        
*       RESET TMPLIST TO DO IT THE BURSON WAY                                   
*       STD VAL = STD HRS*BILLING RATE*TARG %                                   
*-------------------------------------------------------                        
RESETTMP LR    R1,RC                                                            
         DS    0H                                                               
         NMOD1 0,**STMP**                                                       
         LR    RC,R1                                                            
*                                                                               
         TM    PERSTAT,PERISNEW    IS THIS AGENCY ON NEW COST RECORDS           
         BNO   RTX                 NO, CAN'T GET HOURS                          
*                                                                               
         LA    R0,24                                                            
         LA    R4,TMPLIST                                                       
*                                                                               
RT20     CLI   0(R4),X'FF'         EOT                                          
         BE    RT100                                                            
         ZAP   2(4,R4),=P'0'                                                    
         ZAP   6(6,R4),=P'0'                                                    
         OC    0(2,R4),0(R4) IS A MONTH DEFINED IN TABLE                        
         BZ    RT80                   NOPE, CHECK NEXT                          
*                                                                               
         GOTO1 SETAHRS,DMCB,DOUBLE,1,(R4)                                       
*                                                                               
         ZAP   2(4,R4),DOUBLE      RESET STD HOURS                              
*                                                                               
RT80     LA    R4,12(R4)                                                        
         BCT   R0,RT20                                                          
*                                                                               
RT100    BAS   RE,BLDRATKY         BUILD RATE KEY                               
         LA    R4,TMPLIST                                                       
         LA    R0,24                                                            
*                                                                               
         USING GRPARMSD,R1                                                      
RT110    OC    0(2,R4),0(R4)       IS THERE A DATE IN THE TABLE NTRY            
         BZ    RTX                 NO                                           
         LA    R1,DMCB             GETRATE                                      
         LA    RE,DIRKEY                                                        
         ST    RE,GRAKEYIN                                                      
         ST    R4,GRADATIN                                                      
         MVC   GRADMGR,DATAMGR                                                  
         LA    RE,RATEXHR          BUCKET TO RETURN RATE IN                     
         ST    RE,GRAOPRAT                                                      
         GOTO1 =A(GETRATE)                                                      
         OC    RATEXHR,RATEXHR     WAS A RATE RETURNED?                         
         BNZ   *+10                YES                                          
         ZAP   RATEXHR,=P'0'                                                    
         BAS   RE,RATEXTRG         SET RATE TIMES TARG IN TMPLIST               
         LA    R4,12(R4)                                                        
         BCT   R0,RT110                                                         
*                                                                               
RTX      XIT1                                                                   
         DROP  R1                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
*        BUILD A RATE KEY FROM THE 1R ACCOUNT IN ADACC                          
*--------------------------------------------------------------------           
BLDRATKY NTR1                                                                   
         USING PCHRECD,R2                                                       
         LA    R2,DIRKEY                                                        
         XC    PCHKEY,PCHKEY                                                    
         MVI   PCHKTYP,PCHKTYPQ                                                 
         MVC   PCHKCPY,RCCOMPFL                                                 
         MVI   BYTE,1                                                           
         MVC   PCHKDOF,SPACES                                                   
         L     R5,ADACC                                                         
         LA    R5,3(R5)                                                         
         LA    R4,PCHKDOF                                                       
         GOTO1 =A(MOVE1R)                                                       
*                                                                               
         MVI   BYTE,2                                                           
         MVC   PCHKDEP,SPACES                                                   
         LA    R4,PCHKDEP                                                       
         GOTO1 =A(MOVE1R)                                                       
*                                                                               
         MVI   BYTE,3                                                           
         MVC   PCHKSUB,SPACES                                                   
         LA    R4,PCHKTSK                                                       
         GOTO1 =A(MOVE1R)                                                       
         XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------------          
* MULTIPLY THE RATE IN DUB WITH THE TARGET PCT FOR THE MONTH IN 0(R4)           
* WILL USE PROFILES+4 AS THE DEFAULT IF MONTH NOT IN TABLE, OR ZERO             
*---------------------------------------------------------------------          
RATEXTRG NTR1                                                                   
         ZAP   TARGET,=P'0'                                                     
         CLI   PROFILES+4,0        ANY DEFAULT DEFINED                          
         BE    RXT10               NO, USE 75%                                  
*                                                                               
         ZIC   R1,PROFILES+4       USE DEFAULT TRG PCT FROM PROFILE             
         CVD   R1,DUB                                                           
         ZAP   TARGET,DUB                                                       
         MP    TARGET,=P'100'                                                   
*                                                                               
RXT10    LA    R6,TRGPLIST                                                      
RXT20    CLI   0(R6),X'FF'         END OF TABLE                                 
         BE    RXT50               YES, USE DEFAULT                             
         CLC   0(2,R4),0(R6)       IS THERE A MATCH ON YM?                      
         BNE   RXT30               NO                                           
*                                                                               
         ZAP   TARGET,2(6,R6)     EXTRACT PCT                                   
         B     RXT50               MULT BY RATE                                 
*                                                                               
RXT30    LA    R6,8(R6)           INCREMENT TABLE                               
         B     RXT20                                                            
*                                                                               
RXT50    ZAP   PL16,TARGET                                                      
         MP    PL16,RATEXHR                                                     
         ZAP   DUB,PL16                                                         
         SRP   DUB,64-4,5                                                       
         ZAP   6(6,R4),DUB         SET RATE * TARG % IN TMPLIST                 
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* INITIALIZE PERBLK                                                             
*                                                                               
         EJECT                                                                  
*-------------------------------------------------------------------            
* SET AGENCY HOURS                                                              
* NOTE:  PERINOUTS MUST BE SET                                                  
* P1 = PL8 BUCKET TO SET                         (R2)                           
* P2  NUMBER OF BUCKS (1 HERE)                                                  
* P3  = A(X'YYMM') MONTH  (OR MONTH LIST)        (R4)                           
*-------------------------------------------------------------------            
SETAHRS  NTR1  WORK=(R5,GPOPLENQ)                                               
         LM    R2,R4,0(R1)                                                      
         ZAP   0(8,R2),=P'0'                                                    
*                                                                               
         GOTO1 =V(GETPER),DMCB,(R4),ACALKEYS,(R5),0,ACALREC                     
         USING GPOPD,R5                                                         
         SR    R1,R1                                                            
         ICM   R1,1,GPONPER                                                     
         BZ    SETAHX                                                           
         LA    RF,GPOPERS                                                       
*                                                                               
         USING GPOPERD,RF                                                       
SETAH20  BAS   RE,NWRK             ADJUST GPOPNWRK FOR IN-OUT                   
*                                                                               
         ZIC   RE,GPOPNWRK         NUMBER OF WORKING DAYS                       
         CVD   RE,DUB                                                           
         MP    DUB,=P'800'                                                      
         AP    0(8,R2),DUB                                                      
*                                                                               
         LA    RF,GPOPLN1Q(RF)                                                  
         BCT   R1,SETAH20          GET NEXT PERIOD                              
*                                                                               
SETAHX   XIT1                                                                   
         DROP  R5,RF                                                            
         EJECT                                                                  
*--------------------------------------------------------------------           
* FOR THE PERIOD IN THE GPOPERD BLOCK AT 0(RF), CALCULATE THE                   
* NUMBER OF WORKING DAYS THE EMPLOYEE IN PERDATA WAS ACTIVE                     
*--------------------------------------------------------------------           
NWRK     NTR1                                                                   
         LA    R2,PERINOUT         EMPLOYEES INOUTS                             
         LA    R0,PERLMAX          NUMBER OF INOUTS IN PERDATA                  
         LR    R4,RF               ADDRESS OF GPOPERD DATA                      
         XR    R3,R3               ACCUM IN R3                                  
         USING GPOPERD,R4                                                       
*                                                                               
*                                                                               
* FOR THE 2 START DATES, TAKE THE HIGHEST                                       
*                                                                               
NWRK40   LA    R5,GPOPSTRT                                                      
         CLC   GPOPSTRT,0(R2)                                                   
         BH    *+8                                                              
         LA    R5,0(R2)                                                         
*                                                                               
* FOR THE 2 END DATES, TAKE THE LOWEST                                          
*                                                                               
         LA    R6,GPOPEND                                                       
         CLC   GPOPEND,3(R2)                                                    
         BL    *+8                                                              
         LA    R6,3(R2)                                                         
*                                                                               
         CLC   0(3,R5),0(R6)       IS THIS A VALID PERIOD (ST <= END)           
         BH    NWRK50                                                           
*                                                                               
         GOTO1 =V(GETNWRK),DMCB,(R5),(R6),0                                     
         L     R1,DMCB                                                          
         AR    R3,R1               ACCUMULATE NWORKING                          
*                                                                               
NWRK50   LA    R2,6(R2)            NEXT INOUT GROUP                             
         BCT   R0,NWRK40                                                        
         STC   R3,GPOPNWRK         SAVE IN GPOP BLOCK                           
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*-------------------------------------------------------                        
*       IS THERE DATA IN EMPCODE FOR THE LEVEL IN BYTE                          
*-------------------------------------------------------                        
NOLEVEL  DS    0D                                                               
         LR    R1,RC                                                            
         NMOD1 0,**ISEM**                                                       
         LR    RC,R1                                                            
         LA    R5,EMPCODE                                                       
         LA    R4,WORK                                                          
         MVC   WORK,SPACES                                                      
         GOTO1 =A(MOVE1R)                                                       
         CLC   WORK,SPACES                                                      
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*-------------------------------------------------------                        
*       MOVE A PIECE OF THE 1R ACCOUNT AT 0(R5) TO THE STORAGE AT 0(R4)         
*       THE LEVEL TO MOVE IS IN BYTE X'1-4'                                     
*-------------------------------------------------------                        
MOVE1R   LR    R1,RC                                                            
         NMOD1 0,**MV1R**                                                       
         LR    RC,R1                                                            
*                                                                               
         BAS   RE,SET1ROFF         GET LENGTH/OFFSET INTO R1/R2                 
         LA    R5,0(R2,R5)                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R5)                                                    
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------------         
*       SET R2 TO THE OFFSET OF BYTE INTO A 12 BYTE ACCOUNT FIELD               
*       AND R1 TO THE LENGTH OF THAT FIELD                                      
*---------------------------------------------------------------------          
SET1ROFF EQU   *                                                                
         XR    R2,R2                                                            
         XR    R1,R1                                                            
         XR    R0,R0                                                            
         L     RF,ADLDGHIR                                                      
         LA    RF,2(RF)            BUMP PAST ELCODE/LEN                         
         IC    R1,0(RF)                                                         
         CLI   BYTE,1                                                           
         BER   RE                                                               
*                                                                               
         IC    R0,0(RF)            SAVE LENGTH OF PREV                          
         IC    R2,0(RF)            SET OFFSET TO LEVEL 2                        
         LA    RF,16(RF)                                                        
         IC    R1,0(RF)                                                         
         SR    R1,R0               SET LEN OF LEVEL 2                           
         CLI   BYTE,2                                                           
         BER   RE                                                               
*                                                                               
         IC    R0,0(RF)                                                         
         IC    R2,0(RF)                                                         
         LA    RF,16(RF)                                                        
         IC    R1,0(RF)                                                         
         SR    R1,R0               SET LEN OF LEVEL 3                           
         CLI   BYTE,3                                                           
         BER   RE                                                               
*                                                                               
         IC    R0,0(RF)                                                         
         IC    R2,0(RF)                                                         
         LA    RF,16(RF)                                                        
         IC    R1,0(RF)                                                         
         SR    R1,R0               SET LEN OF LEVEL 3                           
         CLI   BYTE,4                                                           
         BER   RE                                                               
         DC    H'0'                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*       COMPARE THE 12 BYTES AT 0(R4) WITH THE 12 BYTES AT 0(R5)                
* FOR THE LEVEL IN BYTE                                                         
*----------------------------------------------------------------------         
COMP1R   LR    R1,RC                                                            
         NMOD1 0,**CM1R**                                                       
         LR    RC,R1                                                            
*                                                                               
         BAS   RE,SETLEVEL         ADDRESS HEIRARCHY DATA W/R7                  
*                                                                               
         ZIC   R2,0(R7)                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R5)                                                    
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*       SET R7 TO THE A(THE HEIRARCHY DATA OF THE LEVEL IN BYTE)                
*---------------------------------------------------------------------          
SETLEVEL EQU   *                                                                
         ZIC   RF,BYTE                                                          
         BCTR  RF,0                                                             
         MH    RF,=H'16'                                                        
         L     R7,ADLDGHIR                                                      
         LA    R7,2(R7)            BUMP TO ELEMENT DATA                         
         LA    R7,0(RF,R7)         ADDRESS THIS LEVELS DATA                     
         BR    RE                                                               
*                                                                               
*----------------------------------------------------------------------         
*       SET R1 TO THE LENGTH OF THE LEVEL IN BYTE                               
*---------------------------------------------------------------------          
SET1RLEN EQU   *                                                                
         XR    R1,R1                                                            
         ST    R7,DUB                                                           
         ST    RE,DUB+4                                                         
         BAS   RE,SETLEVEL                                                      
         IC    R1,0(R7)                                                         
         L     R7,DUB                                                           
         L     RE,DUB+4                                                         
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*       CLEAR THE LEVEL OF SRTEMPL IN BYTE ASSUMES R6 IS A(SORTRECORD)          
*----------------------------------------------------------------------         
CLEAR1R  LR    R1,RC                                                            
         NMOD1 0,*CLEAR*                                                        
         LR    RC,R1                                                            
         BAS   RE,SET1ROF2         GET OFFSET/LENGTH OF THIS LEVEL              
         LA    R2,SRTEMPL(R2)      CONVERT OFFSET INTO ADDRESS                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------------         
*       SET R2 TO THE OFFSET OF BYTE INTO A 12 BYTE ACCOUNT FIELD               
*       AND R1 TO THE LENGTH OF THAT FIELD                                      
*---------------------------------------------------------------------          
SET1ROF2 EQU   *                                                                
         XR    R2,R2                                                            
         XR    R1,R1                                                            
         XR    R0,R0                                                            
         L     RF,ADLDGHIR                                                      
         LA    RF,2(RF)            BUMP PAST ELCODE/LEN                         
         IC    R1,0(RF)                                                         
         CLI   BYTE,1                                                           
         BER   RE                                                               
*                                                                               
         IC    R0,0(RF)            SAVE LENGTH OF PREV                          
         IC    R2,0(RF)            SET OFFSET TO LEVEL 2                        
         LA    RF,16(RF)                                                        
         IC    R1,0(RF)                                                         
         SR    R1,R0               SET LEN OF LEVEL 2                           
         CLI   BYTE,2                                                           
         BER   RE                                                               
*                                                                               
         IC    R0,0(RF)                                                         
         IC    R2,0(RF)                                                         
         LA    RF,16(RF)                                                        
         IC    R1,0(RF)                                                         
         SR    R1,R0               SET LEN OF LEVEL 3                           
         CLI   BYTE,3                                                           
         BER   RE                                                               
*                                                                               
         IC    R0,0(RF)                                                         
         IC    R2,0(RF)                                                         
         LA    RF,16(RF)                                                        
         IC    R1,0(RF)                                                         
         SR    R1,R0               SET LEN OF LEVEL 3                           
         CLI   BYTE,4                                                           
         BER   RE                                                               
         DC    H'0'                                                             
         EJECT                                                                  
*-------------------------------------------------------                        
*       MOVE INTO P1, THE 1R ACCOUNT IN P2,                                     
*       FOR THE LEVEL IN P3, P4 IS RC                                           
*-------------------------------------------------------                        
MOVEK1R  NMOD1 0,*MOK1R                                                         
*                                                                               
*                                                                               
         L     R3,0(R1)            P1 = TO FIELD                                
         L     R4,4(R1)            P2 = FROM FIELD                              
         L     R5,8(R1)            P3 = LEVEL                                   
*                                                                               
         STC   R5,BYTE                                                          
         BAS   RE,SET1RKLN         GET LENGTH R1/R2                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)                                                    
         XMOD1 1                                                                
*                                                                               
*                                                                               
*---------------------------------------------------------------------          
*       SET R1 TO THE LENGTH OF THE 12 BYTE ACCOUNT IN BYTE                     
*---------------------------------------------------------------------          
SET1RKLN EQU   *                                                                
         XR    R1,R1                                                            
         XR    R0,R0                                                            
         L     RF,ADLDGHIR                                                      
         LA    RF,2(RF)            BUMP PAST ELCODE/LEN                         
         IC    R1,0(RF)            LEV A                                        
         CLI   BYTE,1                                                           
         BER   RE                                                               
*                                                                               
         LA    RF,16(RF)                                                        
         IC    R1,0(RF)            LEV B LEN                                    
         CLI   BYTE,2                                                           
         BER   RE                                                               
*                                                                               
         LA    RF,16(RF)                                                        
         IC    R1,0(RF)            LEV C LEN                                    
         CLI   BYTE,3                                                           
         BER   RE                                                               
*                                                                               
         LA    RF,16(RF)                                                        
         IC    R1,0(RF)            LEV D LEN                                    
         CLI   BYTE,4                                                           
         BER   RE                                                               
         DC    H'0'                                                             
         LTORG                                                                  
*----------------------------------------------------------------------         
* GET THE TITLES THE USER HAS DEFINED FOR THEIR 1R LEVELS             -         
* ON ENTRY, P1=RC                                                     -         
*----------------------------------------------------------------------         
         USING ACR9D,RC                                                         
SETTITLE NMOD1 0,**STIT**                                                       
         L     RC,0(R1)                                                         
         USING ACLELD,R6                                                        
         L     R6,ADLDGHIR                                                      
         MVC   LVATITLE,ACLVDESC                                                
         LA    R6,L'ACLVALS(R6)                                                 
         MVC   LVBTITLE,ACLVDESC                                                
         LA    R6,L'ACLVALS(R6)                                                 
         MVC   LVCTITLE,ACLVDESC                                                
         XIT1                                                                   
         DROP R6                                                                
         LTORG                                                                  
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
*  BDTYPE       PARAMETER 3 = 0 = TARGET,  1 = STD RATE, 2 = STD HRS *          
*               PARAMETER 4 = VALIDATION (0), NON-VALIDATION (<> 0)  *          
*          PASS BACK - CONDITION CODE (GOOD = 0, BAD = NOT 0)        *          
*               BUDGAMT OR STANDARD RATE                             *          
* NOTE - FOR STD HRS GO RIGHT FROM VALIDATION TO COMPUTING STD HRS   *          
*        TARGET BUDGET = SUM OF 1D ELEMENTS                          *          
*        RATE   BUDGET = RATE FOR MONTH.  IF NO RATE FOR MONTH       *          
*                        CASCADE BACK TO PREVIOUS MONTH OR FURTHER   *          
*                        UNTIL RATE IS FOUND.  IF NO RATE IS FOUND   *          
*                        RATE BECOMES ZERO                                      
**********************************************************************          
GETBUD   DS    0D                                                               
         NMOD1 0,*GETBUD                                                        
         L     RC,16(R1)               LOAD BACK RC                             
*                                                                               
         USING ACKEYD,R7                                                        
         L     R7,ACREC                                                         
*                                                                               
         L     R5,0(R1)                P1 = 1R KEY    - R5           *          
         L     R4,4(R1)                P2 = BUDGET #  - R4           *          
         MVC   BDTYPE(1),11(R1)        P3 = BUDGET TYPE  (0,1,2)     *          
*                                                                               
*--------------------------------------------------------------------*          
* BUILD STD HRS BY MONTH TABLE                                                  
*        MONTHS IN TABLE ARE SET IN REQFRST                                     
*--------------------------------------------------------------------*          
*                                                                               
GETB05   CLI   BDTYPE,2               IS IT STD HRS BUDGET?          *          
         BNE   GETB50                 NO                             *          
*                                                                               
         LA    R2,STDLIST               CLEAR OLD STD HRS FROM TABLE *          
         LA    R0,24                                                 *          
GETB11   MVI   0(R2),X'40'                                           *          
         ZAP   7(4,R2),=P'0'                                         *          
         LA    R2,11(R2)                                             *          
         BCT   R0,GETB11                                             *          
*                                                                               
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES   CLEAR KEY        *          
         MVI   ACBTKTYP,X'1B'           BUDGET RECORD                *          
         MVC   ACBTKACC(4),0(R5)        COMP/UNIT/LEDGER/OFFICE      *          
         MVC   ACBTKWRK,SPACES                                       *          
         MVC   ACBTKCON,SPACES                                       *          
         MVC   ACBTKCON(1),QCOMPANY        COMPANY                   *          
         MVC   ACBTKCON+1(2),=C'1C'        1C CONTRA                 *          
         XC    ACBTKBNO(9),ACBTKBNO        CLEAR W/ BINARY ZEROES    *          
         MVC   ACBTKBNO+1(1),0(R4)         BUDGET #                  *          
         MVC   MATCHKEY(42),ACKEYACC                                 *          
         BAS   RE,GETHIGH             READ HIGH                      *          
GETB12   CLC   MATCHKEY(42),ACKEYACC    COMP/UNIT/LEDGER/OFFICE      *          
         BE    GETB15                   SO FAR, SO GOOD                         
*                                                                               
         CLI   BDTYPE,0            TRGPCT                                       
         BNE   *+8                 NO                                           
         OI    BUDSTAT,NOBUD       SET TARGET PCT NOT FOUND                     
         B     GETBX                                                            
*                                                                    *          
*                                                                    *          
**********************                                               *          
*  GET X'1D' ELEMENT *                                               *          
**********************                                               *          
GETB15   L     R5,ACREC                                              *          
         MVC   OFFSWIT(4),0(R5)        SAVE COMP/UNIT/LED/OFFICE     *          
         AH    R5,DATADISP                                           *          
*                                                                    *          
GETB20   CLI   0(R5),0                 IS IT END OF ELEMENTS?        *          
         BE    GETBX                   YES - NEXT BUDGET             *          
         CLI   0(R5),X'1D'             IS IT 1D ELEMENT?             *          
         BE    GETB25                  YES                           *          
*                                                                    *          
GETB22   ZIC   RF,1(R5)                                              *          
         AR    R5,RF                                                 *          
         B     GETB20                  GET NEXT ELEMENT              *          
*                                                                    *          
         USING ACBAD,R5                BUDGET ELEMENT                *          
GETB25   CLC   ACBAMNTH,START            LOWER THAN START YYMM       *          
         BL    GETB22                       YES - GO GET NEXT X'1D'  *          
         CLC   ACBAMNTH,ENDATE           HIGHER THAN END YYMM        *          
         BH    GETB22                       YES - GO GET NEXT X'1D'  *          
*                                                                    *          
         LA    R2,STDLIST                                            *          
         LA    R0,24                     MAX # OF MONTHS             *          
*                                                                    *          
GETB30   CLC   1(2,R2),ACBAMNTH          DOES YM MATCH?              *          
         BE    GETB35                    YES                         *          
         LA    R2,11(R2)                 NO - INCREMENT TABLE        *          
         BCT   R0,GETB30                 LOOP                        *          
         B     GETB22                    NO MATCH IN TABLE           *          
GETB35   AP    7(4,R2),ACBABUDG          ADD STD HOURS               *          
         MVC   0(1,R2),ACKEYACC+4        MOVE IN OFFICE CODE         *          
         B     GETB22                    GET NEXT ELEMENT            *          
         EJECT                                                       *          
**********************************************************************          
*  READ LOW LEVEL 1R EMPLOYEE AND RETURN EITHER TARGET BUDGET AMT    *          
*  OR STANDARD VALUE( RATE X STD HRS)                                *          
**********************************************************************          
GETB50   ZAP   BUDGAMT,=P'0'           CLEAR AMOUNT                  *          
         ZAP   RATEXHR,=P'0'          CLEAR RATE X HOURS            *           
*                                                                               
         CLI   BDTYPE,1            RATE CALL                                    
         BNE   GETB60              NO, MUST BE TARG PCT                         
*                                                                               
         LA    R0,24                                                            
         LA    R2,STDLIST          INIT TMPLIST WITH  STDLIST                   
         LA    R3,TMPLIST                                                       
GETB55   MVC   0(2,R3),1(R2)               YYMM                                 
         ZAP   2(4,R3),=P'0'               INIT HRS TO ZERO                     
         CLC   0(2,R3),HIREDATE                                                 
         BL    GETB56                                                           
         CLC   0(2,R3),FIREDATE                                                 
         BH    GETB56                                                           
         MVC   2(4,R3),7(R2)               STD HRS PLL4                         
*                                                                               
GETB56   ZAP   6(6,R3),=P'0'               MAKE RATES ZERO PACKED               
         LA    R2,11(R2)                   INCREMENT STD LIST                   
         LA    R3,12(R3)                   INCREMENT TMP LIST                   
         BCT   R0,GETB55                                                        
         MVI   0(R3),X'FF'                 END OF TMPLIST TABLE                 
*                                                                               
GETB60   XC    ACKEYACC(ACRECORD-ACKEYACC),ACKEYACC CLEAR KEY        *          
         MVI   ACBTKTYP,X'1B'           BUDGET RECORD                *          
         MVC   ACBTKACC,0(R5)          1R ACCOUNT                    *          
         MVC   ACBTKWRK,SPACES                                       *          
         MVC   ACBTKCON,SPACES                                       *          
         MVC   ACBTKCON(1),QCOMPANY        COMPANY                   *          
         MVC   ACBTKCON+1(2),=C'1C'        1C CONTRA                 *          
         MVC   ACBTKBNO+1(1),0(R4)         BUDGET NUMBER             *          
         MVC   MATCHKEY(42),ACKEYACC                                 *          
         BAS   RE,GETHIGH             READ HIGH                      *          
         CLC   MATCHKEY(35),ACKEYACC   MATCH ON 1R ACCOUNT BUDGET?   *          
         BE    GETB69                  YES                           *          
*                                                                               
         CLI   BDTYPE,0            TRGPCT                                       
         BNE   GETBX               NO                                           
*                                                                               
         ZIC   R1,PROFILES+4       UES DEFAULT TRG PCT FROM PROFILE             
         CVD   R1,DUB                                                           
         ZAP   BUDGAMT,DUB                                                      
         MP    BUDGAMT,=P'100'                                                  
         OI    BUDSTAT,NOBUD                                                    
         B     GETBX                                                            
*                                                                    *          
GETB69   CLI   BDTYPE,0            TRGPCT                                       
         BNE   GETB69A             NO                                           
         OI    BUDSTAT,NOBUD       ASSUME TRG NOT FOUND                         
         ZIC   R1,PROFILES+4       USE DEFAULT TRG PCT FROM PROFILE             
         CVD   R1,DUB                                                           
         ZAP   BUDGAMT,DUB                                                      
         MP    BUDGAMT,=P'100'                                                  
*                                                                               
GETB69A  L     R5,ACREC                                              *          
         AH    R5,DATADISP                                           *          
         SR    R1,R1                                                            
*                                                                    *          
GETB70   CLI   0(R5),0                      ANY ELEMENTS LEFT?       *          
         BE    GETB83                       NO                       *          
         CLI   0(R5),X'1D'                  BUDGET AMT ELEMENT       *          
         BE    GETB85                       YES                      *          
*                                                                               
GETB80   IC    R1,1(R5)                                              *          
         AR    R5,R1                        IN SEACH OF X'1D'        *          
         B     GETB70                                                *          
*                                                                               
GETB83   CLI   BDTYPE,1                  IS IT RATE BUDGET?                     
         BE    GETB150                   YES -                                  
         B     GETBX                     NO - TARGET BUDGET                     
*                                                                               
         USING ACBAD,R5                                              *          
*                                                                    *          
GETB85   CLI   ISBURSON,C'Y'       IS THIS A BURSON AGENCY                      
         BNE   GETB86              NO, CONTINUE                                 
         CLI   BDTYPE,0            TARG PCT FOR BURSON                          
         BE    GETB200             YES, KEEP MONTHLY                            
*                                                                               
GETB86   MVC   TRGDTS(6),START           RPT START & END YYMM                   
         CLI   BDTYPE,1                  IS IT A RATE BUDGET?                   
         BE    GETB90                    YES                                    
*                                                                               
         MVC   TRGDTS(6),START                                                  
         MVI   TRGDTS+1,1                START MONTHS FOR TARGET                
         MVI   TRGDTE+1,X'12'            END   MONTH  FOR TARGET                
*                                                                               
GETB90   CLC   ACBAMNTH,TRGDTS           REPORT START DATE?          *          
         BL    GETB80                    EXCLUDE IF BEFORE           *          
         CLC   ACBAMNTH,TRGDTE           REPORT END DATE?                       
         BH    GETB80                    EXCLUDE IF AFTER                       
*                                                                               
         CLI   BDTYPE,0                  IS IT A TARGET BUDGET?                 
         BE    GETB100                   YES                                    
*                                                                               
         CLC   ACBAMNTH,HIREDATE         HIRE DATE?                             
         BL    GETB80                    EXCLUDE IF BEFORE                      
         CLI   FIREDATE,X'FF'            NO TERMINATION DATE?                   
         BE    GETB102                   NO PRORATION NEEDED                    
         CLC   ACBAMNTH,FIREDATE         FIRE DATE?                             
         BH    GETB80                    NO PRORATION NEEDED                    
         B     GETB102                                                          
*                                                                               
GETB100  CLI   BDTYPE,0            TRGPCT                                       
         BNE   GETB101             NO                                           
*                                                                               
         TM    BUDSTAT,NOBUD       DOES BUDGAMNT STILL HAVE THE DEFAULT         
         BNO   GETB101             NO                                           
*                                                                               
         ZAP   BUDGAMT,=P'0'       ZAP OUT DEFAULT                              
         NI    BUDSTAT,X'FF'-NOBUD SET TARGET PCT FOUND                         
GETB101  AP    BUDGAMT,ACBABUDG          SUM TARGET YYMM                        
         B     GETB80                    NEXT 1D ELEMENT                        
*                                                                               
GETB102  LA    R6,TMPLIST                PUT RATE IN TABLE                      
*                                                                               
GETB105  CLI   0(R6),X'FF'         END OF TABLE                                 
         BE    GETB80              YES, GET NEXT 1D EL                          
         CLC   ACBAMNTH,0(R6)            IS THERE A MATCH ON YM?                
         BNE   GETB110                   NO                                     
*                                                                               
         ZAP   6(6,R6),ACBABUDG          ADD RATE TO TABLE                      
         B     GETB80                    GET NEXT 1D                            
*                                                                               
GETB110  LA    R6,12(R6)                INCREMENT TABLE                         
         B     GETB105                                                          
********************************************                                    
*  FILL BLANK FIELDS WITH LAST RATE AMOUNT *                                    
********************************************                                    
GETB150  LA    R6,TMPLIST                                                       
         ZAP   DUB,=P'0'                                                        
         ZAP   MMCNT,=P'0'                                                      
*                                                                               
GETB155  CLI   0(R6),X'FF'               END OF TABLE                           
         BE    GETB170                                                          
*                                                                               
         CP    6(6,R6),=P'0'             IS IT ZERO?                            
         BNE   GETB160                   NO                                     
*                                                                               
         ZAP   6(6,R6),DUB               YES -                                  
         B     GETB165                                                          
*                                                                               
GETB160  ZAP   DUB,6(6,R6)                                                      
*                                                                               
GETB165  LA    R6,12(R6)               INCREMENT TABLE                          
         AP    MMCNT,=P'1'                                                      
         B     GETB155                  NEXT TABLE ITEM                         
*                                                                               
GETB170  ZAP   BUDGAMT,=P'0'            CLEAR BUDGAMT                           
*                                                                               
*-------------------------------------------------------------------            
* KEEP MONTHLY TARGET PERCENTAGES FOR BURSON                                    
*-------------------------------------------------------------------            
GETB200  LA    R6,TRGPLIST         KEEP MONTHLY TARGET PCT BUCKETS              
GETB205  CLI   0(R6),X'FF'         END OF TABLE                                 
         BE    GETB80              YES, GET NEXT 1D EL                          
         CLC   ACBAMNTH,0(R6)      IS THERE A MATCH ON YM?                      
         BNE   GETB210             NO                                           
*                                                                               
         ZAP   2(6,R6),ACBABUDG    ADD RATE TO TABLE                            
         B     GETB80              GET NEXT 1D                                  
*                                                                               
GETB210  LA    R6,8(R6)           INCREMENT TABLE                               
         B     GETB205                                                          
*--------------------------------------------------------------------*          
*  RETURN                                                                       
*--------------------------------------------------------------------*          
GETBX    XMOD1 1                                                     *          
**********************************************************************          
*                                                                               
**************************************************************                  
*              DATAMGR INTERFACE FOR GETBUD                                     
**************************************************************                  
GETHIGH  ST    RE,FULL                                                          
         MVC   COMMAND,=C'DMRDHI'            READ HIGH                          
         MVC   SAVEKEY,0(R7)                                                    
         L     R7,ACREC                                                         
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R7),(R7)                       
         CLI   DMCB+8,0                      TEST FOR ERRORS                    
         BE    *+6                                                              
         DC    H'0'                          DIE IF ERRORS FOUND                
         L     RE,FULL                                                          
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*        SETBUD - FIND OUT WHAT LEVELS THE USER HAS DEFINED HIS                 
*                 BUDGETS AT SO YOU CAN READ FOR THEM WITH THE CORRECT          
*                 ACCOUNT (WHEN YOU HAVE TO)                                    
*        BUDGET NUMBER IS AT 0(R2), RETURS MODE TO READ IT AT IN 1(R2)          
**********************************************************************          
*                                                                               
SETBUD   NMOD1 0,SETBUD                                                         
         L     RC,0(R1)           RESTORE REG C                                 
         USING BUDD,R2                                                          
         MVI   BUDMODE,X'FF'       INIT MODE TO INVALID                         
         ZAP   BUDAMNT,=P'0'                                                    
*                                                                               
         USING ACKEYD,R7                                                        
         L     R7,ACREC                                                         
         XC    ACKEYACC(ACRECORD-ACKEYACC),ACKEYACC CLEAR KEY        *          
         MVI   ACBTKTYP,ACBTKTEQ        BUDGET RECORD                *          
         MVC   ACBTKCMP,QCOMPANY                                                
         MVC   ACBTKNO1+1(1),BUDNUM     BUD NUMBER                              
*        2/17/93                   BUDNUM IS NOW I/P BINARY                     
         MVC   MATCHKEY,ACKEYACC                                                
         BAS   RE,SETHIGH          READ HIGH                                    
         CLC   MATCHKEY((ACBTKNO1-ACBTKEY)+L'ACBTKNO1),ACKEYACC                 
         BNE   SETBX                  CAN'T FIND THIS BUD                       
*                                                                               
         LR    R4,R7               WHAT LEVEL IS THIS BUD DEFINED?              
         AH    R4,DATADISP                                                      
*                                                                               
SETB30   CLI   0(R4),0                                                          
         BE    SETBX                                                            
         CLI   0(R4),X'1C'                                                      
         BE    SETB35                                                           
SETB30A  ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     SETB30                                                           
*                                                                               
         USING ACBVD,R4                                                         
SETB35   CLC   ACBVACUL,=C'1R'                                                  
         BNE   SETB30A                                                          
         CLC   =C'1C',ACBVCAUN                                                  
         BNE   SETB30A                                                          
         CLI   ACBVACLV,0                                                       
         BNE   SETB40                                                           
         MVI   BUDMODE,LEDGFRST                                                 
         B     SETBX                                                            
*                                                                               
SETB40   CLI   ACBVACLV,1          SET MODE AT WHICH TO READ THIS BUD           
         BNE   SETB50                                                           
         MVI   BUDMODE,PROCLEVA                                                 
         B     SETBX                                                            
*                                                                               
SETB50   CLI   ACBVACLV,2                                                       
         BNE   SETB60                                                           
         MVI   BUDMODE,PROCLEVB                                                 
         B     SETBX                                                            
*                                                                               
SETB60   CLI   ACBVACLV,3                                                       
         BNE   SETB70                                                           
         MVI   BUDMODE,PROCLEVC                                                 
         B     SETBX                                                            
*                                                                               
SETB70   CLI   ACBVACLV,4                                                       
         BNE   SETBX                                                            
         MVI   BUDMODE,PROCACC                                                  
*                                                                               
SETBX    XIT1                                                                   
         DROP  R4,R7                                                            
         EJECT                                                                  
**************************************************************                  
*              DATAMGR INTERFACE FOR SETBUD                                     
**************************************************************                  
SETHIGH  ST    RE,FULL                                                          
         MVC   COMMAND,=C'DMRDHI'            READ HIGH                          
         MVC   SAVEKEY,0(R7)                                                    
         L     R7,ACREC                                                         
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R7),(R7)                       
         CLI   DMCB+8,0                      TEST FOR ERRORS                    
         BE    *+6                                                              
         DC    H'0'                          DIE IF ERRORS FOUND                
         L     RE,FULL                                                          
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*  BUILD COLUMN HEADINGS BY USING TABLES                             *          
*  COLUMN WIDTH = 17 ( 15 + BOX EACH SIDE                            *          
*  MAX COLUMNS = 6                                                   *          
*----------------------------------------------------------------------         
BLDCOLS  NMOD1 0,BLDCOLS                                             *          
         L     RC,0(R1)                                                         
         ZAP   COLSWIT,=P'0'                COLUMN SWITCH            *          
         LA    R2,OPTION3                   PT TO OPTION3 TABLE      *          
         LA    R5,HEADBLOC                                           *          
         MVC   HEADBLOC(130),SPACES         XHEAD10                  *          
         MVC   HEADBLOC+130(130),SPACES     XHEAD11                  *          
         MVC   HEADBLOC+260(130),SPACES     XHEAD12                  *          
         LA    R1,OPTLNQ                    # FORMATS AVAILABLE      *          
BLD10    CLC   QOPT3(1),0(R2)               MATCH?                   *          
         BE    BLD20                        YES                      *          
         LA    R2,TLENGTH(R2)               NEXT OPTION              *          
         BCT   R1,BLD10                                              *          
         DC    H'0'                                                  *          
*                                                                    *          
         USING COLUMND,R4                                            *          
BLD20    LA    R4,COLUMN                                             *          
         LA    R2,1(R2)                                              *          
*                                                                    *          
BLD25    CLI   0(R2),ENDOFROW              END OF LINE?              *          
         BE    BLDCOLX                     YES                       *          
*                                                                    *          
         CLI   0(R2),ENDOFCOL               END OF COLUMN            *          
         BNE   BLD26                                                 *          
         ZAP   COLSWIT,=P'0'               NEW COLUMN                *          
         B     BLD20                                                 *          
*                                                                    *          
BLD26    CP    COLSWIT,=P'0'                                         *          
         BNE   BLD20                                                 *          
         CLI   0(R2),SKIP                   PRINT COLUMN?            *          
         BE    BLD20                        NO                       *          
         CLI   0(R2),BLANK                  SKIP COLUMN?             *          
         BE    BLD45                                                 *          
         ZIC   R1,0(R2)                                              *          
*                                                                    *          
BLD27    ZIC   R0,COLNUM                    GET # OF COL             *          
         CR    R1,R0                                                 *          
         BE    BLD30                                                 *          
         LA    R4,8(R4)                     NEXT ROW OF COL TABLE    *          
         B     BLD27                                                 *          
*                                                                    *          
BLD30    L     R6,COLNAME                   LOAD IN ADD OF HEADING   *          
         MVC   0(17,R5),0(R6)               LINE 1 OF HEAD           *          
         MVC   130(17,R5),17(R6)            LINE 2 OF HEAD           *          
         MVC   260(17,R5),34(R6)            LINE 3 OF HEAD           *          
         AP    COLSWIT,=P'1'                SET COLUMN SWITCH        *          
*                                                                    *          
BLD40    LA    R5,18(R5)                    BUMP TO NEXT COL         *          
BLD45    ZIC   R1,NUMCOLS                   KEEP TRACK OF NUM OF COL *          
         LA    R1,1(R1)                                              *          
         STC   R1,NUMCOLS                                            *          
         CH    R1,=H'6'                     MAX COL ACROSS IS 6      *          
         BNE   BLD20                                                 *          
BLDCOLX  XIT1                                                        *          
         LTORG                                                                  
         DROP  R4                                                    *          
         EJECT                                                       *          
******************************************************************              
* BUILD PRINT BLOCK COLUMNS *                                    *              
*     R2 = FIELDS INPUT INTO COLUMN  FROM OPTION3 TABLE          *              
*     R4 = AMNTTTBLE - OUTPUT VARIABLES                          *              
*     R5 = PRINT BUFFER                                          *              
*     R6 = COLUMN TABLE                                          *              
*     COLSWIT = SET FOR 1ST FIELD IN COLUMN & CLEARED AFTER LAST *              
*               FIELD IN COLUMN ( DENOTED BY '77')               *              
******************************************************************              
BLDBLC   NMOD1 0,BLDBLC                                              *          
         L     RC,0(R1)                                                         
        XC     CNTNUM,CNTNUM                                     *              
        ZAP    COLSWIT,=P'0'                                     *              
        LA     R2,OPTION3                  PT TO OPTION3 TABLE   *              
        LA     R5,PRNTBLOC+56                                    *              
        LA     R1,OPTLNQ                   # FORMATS AVAILABLE   *              
*                                                                *              
BLC10   CLC    QOPT3(1),0(R2)              OPTION MATCH?         *              
        BE     BLC20                       YES                   *              
        LA     R2,TLENGTH(R2)              BUMP TO NEXT OPTION   *              
        BCT    R1,BLC10                                          *              
        DC     H'0'                        DUMP IF NO MATCH      *              
*                                                                *              
        USING  COLUMND,R6                                        *              
BLC20   LA     R6,COLUMN                                         *              
        LA     R4,AMNTTBLE                 OUTPUT TABLE          *              
        LA     R2,1(R2)                    1ST COL POSITION      *              
*                                                                *              
        CLI    0(R2),ENDOFROW              END OF LINE           *              
        BE     BLCXIT                      YES                   *              
*                                                                *              
        CLI    0(R2),ENDOFCOL              END OF COLUMN?        *              
        BNE    BLC24                       YES                   *              
*                                                                *              
        CP     COLSWIT,=P'0'               ANY ITEMS IN COLUMN   *              
        BE     BLC20                       NO                    *              
*                                                                *              
        CP     COLSWIT,=P'2'               2 ITEMS/COL?          *              
        BE     BLC22                       YES                   *              
*                                                                *              
        LA     R5,18(R5)                   INCREMENT LINE BY 17  *              
        B      BLC23                                             *              
*                                                                *              
BLC22   LA     R5,8(R5)                   INCREMENT LINE BY 7    *              
BLC23   ZAP    COLSWIT,=P'0'                                     *              
        B      BLC20                                             *              
*                                                                *              
BLC24   CLI    0(R2),SKIP                  PRINT COLUMN?         *              
        BE     BLC20                       NO                    *              
*                                                                *              
        ZIC    R1,0(R2)                     OPTION3 EQUATE       *              
        LA     R7,COLEND                    END OF COLUMN TABLE  *              
BLC27   CLM    R1,1,COLNUM                  DID WE GET MATCH?    *              
        BE     BLC30                        YES                  *              
        LA     R6,8(R6)                     NEXT ROW IN COLNUMD  *              
        BCT    R7,BLC27                                          *              
        DC     H'0'                         INVALID INPUT IN TABL*              
*                                                                *              
BLC30   CP     COLSWIT,=P'0'                MORE THAN 1 ITEM/COL?*              
        BE     BLC32                        NO                   *              
        LA     R5,10(R5)                   YES                  *               
*                                                                *              
BLC32   ZIC    RF,COLOFF                    OFFSET INTO AMNTTABLE*              
        AR     R4,RF                        PT TO LOC IN AMNTTABL*              
*                                                                *              
        ZIC    R1,COLLEN                    SIZE OF OUTPUT FIELD *              
        SH     R1,=H'1'                                          *              
        EX     R1,BLC33                                          *              
        AP     COLSWIT,=P'1'                ITEM IN COLUMN COUNTE*              
        B      BLC20                                             *              
BLC33   MVC    0(0,R5),0(R4)                MOVE TO PRINT BUFFER *              
*                                                                *              
BLCXIT  ZIC    R1,CNTNUM                                         *              
        LA     R1,1(R1)                                          *              
        STC    R1,CNTNUM                                         *              
        LA     R5,PRNTBLOC+225                                   *              
        ZAP    COLSWIT,=P'0'                 CLEAR COLUMN COUNTER*              
        CH     R1,=H'2'                      IS THERE SECOND LINE*              
        BL     BLC20                         YES                 *              
        MVC    AMNTTBLE,XSPACES                                  *              
        XIT1                                                        *           
        LTORG                                                                   
        DROP   R6                                                *              
        EJECT                                                    *              
*****************************************************************               
* PRINT FIELD EQUATES                                           *               
*****************************                                   *               
*                                                               *               
*   POSSIBLE COLUMN COMBINATIONS AVAILABLE                      *               
*                                                               *               
IBILHR   EQU   1                    BILLABLE HOURS              *               
IBILSTHR EQU   2                    BILLABLE/STANDARD HRS       *               
IBILDOL  EQU   3                    BILLABLE DOLLARS            *               
IBILDHR  EQU   4                    HOURS BILLED                *               
IBILDDOL EQU   5                    DOLLARS BILLED              *               
IWOHR    EQU   6                    WRITE-OFF HOURS             *               
IWODOL   EQU   7                    WRITE-OFF DOLLARS           *               
IHELDHR  EQU   8                    TIME HELD HOURS             *               
IHELDDOL EQU   9                    TIME HELD DOLLARS           *               
IBVVHR   EQU  10                    BILLED VS VALUE HOURS       *               
IBVVPER  EQU  11                    BILLED VS VALUE PERCENT     *               
IBVVDOL  EQU  12                    BILLED BS VALUE DOLLARS     *               
IBVSHR   EQU  13                    BILLED VS STAND HOURS       *               
IBVSPER  EQU  14                    BILLED VS STAND PERCENT     *               
IBVSDOL  EQU  15                    BILLED BS STAND DOLLARS     *               
ENDOFCOL EQU  77                    END OF COLUMN               *               
SKIP     EQU  88                    SKIP COLUMN                 *               
BLANK    EQU  99                    BLANK COLUMN                *               
ENDOFROW EQU  X'FF'                 ENDOFROW                                    
*                                                               *               
*****************************************************************               
*  PRINT COLUMN TABLE - CONVERTS WHAT OPT3 INPUT MEANS          *               
*****************************************************************               
*     THIS TABLE CONTAINS WHAT THE COLUMNS OF THE PRINTED REPORT*               
*     WILL BE FOR EACH OPTION3 INPUT.                           *               
*     EACH ENTRY LINE SHOWS WHAT WILL PRINT ON THE 2 AVAILABLE  *               
*     LINES.   FIRST BYTE OF EACH LINE IS THE INPUT IN QOPT3.   *               
*                                                               *               
OPTION3  DC    C' '                    OPTION LEFT BLANK        *               
         DC    AL1(IBILHR),AL1(IBILSTHR),AL1(ENDOFCOL)          *               
         DC    AL1(IBILDHR),AL1(ENDOFCOL)                       *               
         DC    AL1(IWOHR),AL1(ENDOFCOL)                         *               
         DC    AL1(IHELDHR),AL1(ENDOFCOL)                       *               
         DC    AL1(IBVVHR),AL1(IBVVPER),AL1(ENDOFCOL)           *               
         DC    AL1(IBVSHR),AL1(IBVSPER),AL1(ENDOFROW)    16     *               
*                                                               *               
         DC    AL1(IBILDOL),AL1(ENDOFCOL)                       *               
         DC    AL1(IBILDDOL),AL1(ENDOFCOL)                      |               
         DC    AL1(IWODOL),AL1(ENDOFCOL)                        |               
         DC    AL1(IHELDDOL),AL1(ENDOFCOL)                      |               
         DC    AL1(IBVVDOL),AL1(ENDOFCOL)                       |               
         DC    AL1(IBVSDOL),AL1(SKIP),AL1(ENDOFROW)     14      |               
TLENGTH  EQU   (*-OPTION3)                                      |               
****************************************************************|               
*         SUPPRESS  WRITE-OFFS, TIME HELD                       |               
****************************************************************|               
         DC    C'1'                    QOPT3=1                                  
         DC    AL1(IBILHR),AL1(IBILSTHR),AL1(ENDOFCOL)          |               
         DC    AL1(IBILDHR),AL1(ENDOFCOL)                       |               
         DC    AL1(SKIP),AL1(ENDOFCOL)                          |               
         DC    AL1(SKIP),AL1(SKIP),AL1(ENDOFCOL)                |               
         DC    AL1(IBVVHR),AL1(IBVVPER),AL1(ENDOFCOL)           *               
         DC    AL1(IBVSHR),AL1(IBVSPER),AL1(ENDOFROW)        16 |               
*                                                               |               
         DC    AL1(IBILDOL),AL1(ENDOFCOL)                       |               
         DC    AL1(IBILDDOL),AL1(ENDOFCOL)                      |               
         DC    AL1(SKIP),AL1(ENDOFCOL)                          |               
         DC    AL1(SKIP),AL1(ENDOFCOL)                          *               
         DC    AL1(IBVVDOL),AL1(ENDOFCOL)                       *               
         DC    AL1(IBVSDOL),AL1(SKIP),AL1(ENDOFROW)         14  *               
OPTLNQ   EQU   (*-OPTION3)/(TLENGTH)                                            
         EJECT                                                  *               
*****************************************************************               
*  THIS TABLE CONTAINS ALL DATA FOR A SPECIFIC COL              *               
*****************************************************************               
*                                                               *               
*    PARAMETER 1 = COLUMN TO PROCESS                            *               
*    PARAMETER 2 = OFFSET OF COLUMN AMT IS LOCATED IN AMNTTBLE  *               
*    PARAMETER 3 = LENGTH OF OUTPUT FIELD                       *               
*    PARAMETER 4 = SPARE                                        *               
*    PARAMETER 5 = ADDRESS OF COLUMN HEADING NAME               *               
*************************************************************** *               
*                                                               *               
HRLEN    EQU   10                                               *               
DOLLEN   EQU   13                                               *               
PCTLEN   EQU   6                                                *               
         DS    0F                                               *               
COLUMN   DC    AL1(IBILHR),AL1(BHRO),AL1(HRLEN),AL1(0),A(BILHEAD)               
CLENGTH  EQU   (*-COLUMN)                                                       
         DC    AL1(IBILSTHR),AL1(BSHRO),AL1(PCTLEN),AL1(0),A(BILHEAD)           
         DC    AL1(IBILDHR),AL1(BDHRO),AL1(HRLEN),AL1(0),A(BILDHEAD)            
         DC    AL1(IWOHR),AL1(WHRO),AL1(HRLEN),AL1(0),A(WOHEAD)                 
         DC    AL1(IHELDHR),AL1(HHRO),AL1(HRLEN),AL1(0),A(HELDHEAD)             
         DC    AL1(IBVVHR),AL1(BVVHRO),AL1(HRLEN),AL1(0),A(BVVHEAD)             
         DC    AL1(IBVVPER),AL1(BVVPO),AL1(PCTLEN),AL1(0),A(BVVHEAD)            
         DC    AL1(IBVSHR),AL1(BVSHRO),AL1(HRLEN),AL1(0),A(BVSHEAD)             
         DC    AL1(IBVSPER),AL1(BVVSPO),AL1(PCTLEN),AL1(0),A(BVSHEAD)           
*                                                                               
         DC    AL1(IBILDOL),AL1(BADOLO),AL1(DOLLEN),AL1(0),A(BILHEAD)           
         DC    AL1(IBILDDOL),AL1(BDDOLO),AL1(DOLLEN),AL1(0),A(BILDHEAD)         
         DC    AL1(IWODOL),AL1(WODOLO),AL1(DOLLEN),AL1(0),A(WOHEAD)             
         DC    AL1(IHELDDOL),AL1(HEDOLO),AL1(DOLLEN),AL1(0),A(HELDHEAD)         
         DC    AL1(IBVVDOL),AL1(BVVDOLO),AL1(DOLLEN),AL1(0),A(BVVHEAD)          
         DC    AL1(IBVSDOL),AL1(BVSDOLO),AL1(DOLLEN),AL1(0),A(BVSHEAD)          
COLEND   EQU   (*-COLUMN)/(CLENGTH)                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
*  HEADINGS FOR PRINT COLUMNS                                   *               
*****************************************************************               
*                                                               *               
BILHEAD  DC    CL17' BILLABLE VALUE '      BILLABLE COLUMN                      
         DC    CL17' HOURS     %    '                                           
         DC    CL17'     VALUE IN $  '                                          
*                                                                               
BILDHEAD DC    CL17'  TIME BILLED   '      BILLED COLUMN                        
         DC    CL17' HOURS          '                                           
         DC    CL17'     VALUE IN $  '                                          
*                                                                               
WOHEAD   DC    CL17' TIME WRITE-OFF '      WRITE OFF COLUMN                     
         DC    CL17' HOURS          '                                           
         DC    CL17'     VALUE IN $  '                                          
*                                                                               
HELDHEAD DC    CL17'   TIME HELD    '      TIME HELD COLUMN                     
         DC    CL17' HOURS          '                                           
         DC    CL17'     VALUE IN $  '                                          
*                                                                               
BVVHEAD  DC    CL17' BILLED VS VALUE'      BILLED VS VALUE COLU                 
         DC    CL17'  OVER (UNDER)  '                                           
         DC    CL17' HOURS $    %   '                                           
*                                                                               
BVSHEAD  DC    CL17' BILLED VS STAND'      BILLED VS STANDARD COL               
         DC    CL17'  OVER (UNDER)  '                                           
         DC    CL17' HOURS $    %   '                                           
*                                                                               
*****************************************************************               
*              BOX ROUTINES (HOOK)                                              
*****************                                                               
BXHOOK   DS    0D                                                               
*****************                                                               
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
         ZIC   R2,NUMCOLS                                                       
         LA    R1,BOXCOLS+55       1ST COL AT 55                                
         LA    R0,10               ?                                            
         CR    R2,R0                                                            
         BL    BOXIT                                                            
         LA    R2,6                MAX # OF COLUMNS = 6                         
BOXIT    MVI   0(R1),C'C'                                                       
         LA    R1,18(R1)           EACH COLUMN = 17                             
         BCT   R2,BOXIT                                                         
         MVI   0(R1),C'R'                                                       
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A LIST OF NEW BUSINESS/PROBONO ACCOUNTS                  
*                                                                               
NBPB     NMOD1 0,*NBPB*                                                         
         L     RC,0(R1)                                                         
         L     R7,ACREC                                                         
         USING BIND,R3                                                          
         L     R3,ACLITAB                                                       
         XC    BININ,BININ         CLEAR TABLE                                  
         LA    R5,BINTABLE                                                      
         USING ACKEYD,R7                                                        
         XC    ACKEYACC(ACRECORD-ACKEYACC),ACKEYACC                             
         MVC   ACKEYACC(1),RCCOMPFL                                             
         L     R6,ADCMPEL                                                       
         USING ACCOMPD,R6                                                       
         MVC   ACKEYACC+1(2),ACMPJOB  UL FOR CLI/PRO/JOB                        
         BAS   RE,NBPBHIGH                                                      
*                                                                               
NBPB2    L     R4,ACREC                                                         
         AH    R4,DATADISP                                                      
         XR    R1,R1                                                            
NBPB310  CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),ACHRELQ                                                    
         BE    NBPB350                                                          
         IC    R1,1(R4)                                                         
         LA    R4,0(R1,R4)                                                      
         B     NBPB310                                                          
*                                                                               
         USING ACHEIRD,R4                                                       
NBPB350  MVC   CLILEN,ACHRLEVA    GET CLIENT LENGTH                             
         MVC   PROLEN,ACHRLEVB    GET PRODUCT LENGTH                            
         SR    R2,R2               R2=ENTRY COUNTER                             
*                                                                               
NBPB4    L     R7,ACREC            BUMP TO NEXT ACCOUNT                         
         LA    R1,NBPBLEN                                                       
         LA    R1,ACKEYACC+3(R1)                                                
         MVI   0(R1),X'FF'                                                      
         BAS   RE,NBPBHIGH                                                      
         L     R7,ACREC                                                         
         CLC   ACKEYACC(3),SAVEKEY SAME CUL                                     
         BNE   NBPB10              NO-ALL DONE                                  
*                                                                               
         BAS   RE,NBPBDUMP                                                      
*                                                                               
         USING GOBLOCKD,R4                                                      
         L     R4,ADGOBLOC                                                      
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELCUL,ACKEYACC           CUL                                  
         LA    R6,GOSELCLI                                                      
         LA    R7,ACKEYACC+3                                                    
         ZIC   R1,CLILEN                                                        
         BCTR  R1,0                                                             
         EX    R1,NBPBMVC                                                       
*                                                                               
         LA    R6,GOSELPRO                                                      
         LA    R1,1(R1)                                                         
         AR    R7,R1                                                            
         ZIC   R1,PROLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,NBPBMVC                                                       
*                                                                               
         MVC   GOADM,DATAMGR                                                    
         L     R7,ACREC                                                         
         ST    R7,GOAKEY                                                        
         GOTO1 GETOPT,DMCB,GOBLOCKD                                             
*                                                                               
         CLI   GOCTYPE,C'N'        NEW BUSINESS??                               
         BE    NBPB6                                                            
         CLI   GOCTYPE,C'O'        OTHER TO EXCLUDE                             
         BE    NBPB6                                                            
         CLI   GOCTYPE,C'B'        NEW BUSINESS                                 
         BE    NBPB6                                                            
         CLI   GOCTYPE,C'P'        PRO BONO??                                   
         BNE   NBPB4               NO                                           
*                                                                               
*                                                                               
NBPB6    MVC   0(L'GOSELCLI,R5),GOSELCLI                                        
         MVC   3(3,R5),GOSELPRO                                                 
         LA    R2,1(R2)                                                         
         LA    R5,L'GOSELCLI(R5)                                                
         C     R2,BINMAX           TEST FOR TABLE OVERFLOW                      
         BL    NBPB4               NO                                           
         DC    H'0'                                                             
*                                                                               
NBPB10   ST    R2,BININ           SAVE ENTRY COUNT                              
*                                                                               
NBPBX    XIT1                                                                   
*                                                                               
NBPBMVC  MVC   0(0,R6),0(R7)                                                    
*                                                                               
NBPBDUMP NTR1                                                                   
         CLC   QUESTOR(4),=C'DUMP'                                              
         BNE   NBPBX                                                            
         LA    R6,=C'NBPB'                                                      
         LA    R2,49                                                            
         LR    R4,R7                                                            
         GOTO1 =V(PRNTBL),DMCB,(4,(R6)),(R4),C'DUMP',(R2),=C'2D',(C'P',X        
               PRINT)                                                           
         B     NBPBX                                                            
         DROP  R4,R5,R6                                                         
*                                                                               
**************************************************************                  
*              DATAMGR INTERFACE FOR CLI                                        
**************************************************************                  
NBPBHIGH ST    RE,FULL                                                          
         MVC   COMMAND,=C'DMRDHI'            READ HIGH                          
         MVC   SAVEKEY,0(R7)                                                    
         L     R7,ACREC                                                         
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R7),(R7)                       
         CLI   DMCB+8,0                      TEST FOR ERRORS                    
         BE    *+6                                                              
         DC    H'0'                          DIE IF ERRORS FOUND                
         L     RE,FULL                                                          
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
* GET BILLING RATE                                                              
* ON ENTRY, P1=A(PCHKEY, FILLED IN WITH ALL THE DATA YOU HAVE)                  
*           P2=A(X'YYMM' EFFECTIVE DATE)                                        
*           P3=A(DATAMGR)                                                       
*           P4=A(PL8 OUTPUT AREA)                                               
*           RETURNS THE RATE IN 0(P4) AS PL8 OR XL8'00' IF NOT FOUND            
*----------------------------------------------------------------------         
*                                                                               
GETRATE  NMOD1 GRWRKLN,**RATE**                                                 
         USING GRWORKD,RC                                                       
         USING GRPARMSD,R8                                                      
         LR    R8,R1               SAVE A(PARMS)                                
         L     RE,GRADATIN                                                      
         MVC   GREDATE,0(RE)                                                    
         L     RE,GRAOPRAT                                                      
         L     R4,GRAKEYIN                                                      
         XC    0(8,RE),0(RE)       ASSUME RATE NOT FOUND                        
         USING SRCH1D,R7                                                        
         LA    R7,SRCHTBL1                                                      
         XC    GRSTAT,GRSTAT                                                    
         XC    GRHIDATE,GRHIDATE                                                
*                                                                               
         USING PCHRECD,R3          BUILD KEY FROM 0(R4) INTO GRKEY              
GTR50    LA    R3,GRKEY                                                         
         XC    PCHKEY,PCHKEY                                                    
         MVI   PCHKTYP,PCHKTYPQ    X'2A' RECORD                                 
         MVC   PCHKCPY,PCHKCPY-PCHRECD(R4)   COMPANY CODE                       
*                                                                               
         TM    S1STAT,S1DPT                                                     
         BZ    GTR200                                                           
         MVC   PCHKDOF,PCHKDOF-PCHRECD(R4)                                      
         MVC   PCHKDEP,PCHKDEP-PCHRECD(R4)                                      
*                                                                               
GTR200   TM    S1STAT,S1SUB        MOVE IN SUBDEPT                              
         BZ    *+10                                                             
         MVC   PCHKSUB,PCHKSUB-PCHRECD(R4)                                      
*                                                                               
GTR300   TM    S1STAT,S1STF        MOVE IN STAFF                                
         BZ    GTR400                                                           
         MVC   PCHKSTF,PCHKSTF-PCHRECD(R4)                                      
*                                                                               
GTR400   TM    S1STAT,S1TSK        MOVE IN TASK CODE                            
         BZ    *+10                                                             
         MVC   PCHKTSK,PCHKTSK-PCHRECD(R4)                                      
*                                                                               
         TM    S1STAT,S1OFF        MOVE IN OFFICE CODE                          
         BZ    *+10                                                             
         MVC   PCHKOFF,PCHKOFF-PCHRECD(R4)                                      
*                                                                               
         TM    S1STAT,S1CLI        MOVE IN CLIENT                               
         BZ    *+10                                                             
         MVC   PCHKCLI,PCHKCLI-PCHRECD(R4)                                      
*                                                                               
         TM    S1STAT,S1PRO        MOVE IN PRODUCT CODE                         
         BZ    *+10                                                             
         MVC   PCHKPRO,PCHKPRO-PCHRECD(R4)                                      
*                                                                               
         BAS   RE,GRRDHI                                                        
         BE    GTR600              KEY WAS FOUND                                
*                                                                               
         LA    R7,1(R7)                                                         
         CLI   0(R7),0                                                          
         BNE   GTR50                                                            
         B     GTRX                                                             
*                                                                               
GTR600   BAS   RE,GRGETREC                                                      
         LA    R5,GRIO                                                          
         AH    R5,=Y(ACTRFST-ACTRECD)                                           
GTR625   CLI   0(R5),0                                                          
         BE    GTRX                                                             
         CLI   0(R5),X'53'         RATE ELEMENT                                 
         BE    GTR700                                                           
GTR650   ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     GTR625                                                           
*                                                                               
         USING TCIELD,R5                                                        
GTR700   CLC   TCIDTE(2),GREDATE   ONLY GET PASSED A YM                         
         BH    GTR650                                                           
         CLC   TCIDTE,GRHIDATE                                                  
         BL    GTR650                                                           
         MVC   GRHIDATE,TCIDTE     SAVE HIGHEST DATE                            
         ZAP   GRRATE,TCIAMT       SAVE RATE                                    
         OI    GRSTAT,GRFOUND                                                   
         B     GTR650                                                           
*                                                                               
GTRX     TM    GRSTAT,GRFOUND      DID I FIND A RATE                            
         BNO   GTRXX               NO                                           
         L     RE,GRAOPRAT         YES, SEND IT BACK                            
         ZAP   0(8,RE),GRRATE                                                   
GTRXX    XMOD1 1                                                                
GTRXIT   XIT1                                                                   
*                                                                               
GRRDHI   NTR1                                                                   
         MVC   GRKEYSV,GRKEY                                                    
         GOTO1 GRADMGR,GRDMCB,=C'DMRDHI',=C'ACCDIR',GRKEY,GRKEY                 
         CLC   GRKEYSV,GRKEY                                                    
         B     GTRXIT                                                           
*                                                                               
GRGETREC NTR1  WORK=(R2,20)                                                     
         USING ACCRECD,R3                                                       
         LA    R3,GRKEY                                                         
         LA    R3,ACCKDA           ADDRESS DISK ADDRESS FOR GETREC              
         GOTO1 GRADMGR,GRDMCB,=C'GETREC  ',=C'ACCMST ',(R3),GRIO,(R2)           
         B     GTRXIT                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              RATE SEARCH TABLE                                      *         
***********************************************************************         
*                                                                               
SRCHTBL1 DS    0CL1                                                             
         DC    B'11111110'     PRO/CLI,OFF,TASK,STF/SUB/DEP                     
         DC    B'11101110'     PRO/CLI,OFF,     STF/SUB/DEP                     
         DC    B'01111110'         CLI,OFF,TASK,STF/SUB/DEP                     
         DC    B'01101110'         CLI,OFF,     STF/SUB/DEP                     
         DC    B'00111110'             OFF,TASK,STF/SUB/DEP                     
         DC    B'00101110'             OFF,     STF/SUB/DEP                     
         DC    B'00011110'                 TASK,STF/SUB/DEP                     
         DC    B'00001110'                      STF/SUB/DEP                     
         DC    B'11110110'     PRO/CLI,OFF,TASK,    SUB/DEP                     
         DC    B'11100110'     PRO/CLI,OFF,         SUB/DEP                     
         DC    B'01110110'         CLI,OFF,TASK,    SUB/DEP                     
         DC    B'01100110'         CLI,OFF,         SUB/DEP                     
         DC    B'00110110'             OFF,TASK,    SUB/DEP                     
         DC    B'00100110'             OFF,         SUB/DEP                     
         DC    B'00010110'                 TASK,    SUB/DEP                     
         DC    B'00000110'                          SUB/DEP                     
         DC    B'11110010'     PRO/CLI,OFF,TASK,        DEP                     
         DC    B'11100010'     PRO/CLI,OFF,             DEP                     
         DC    B'01110010'         CLI,OFF,TASK,        DEP                     
         DC    B'01100010'         CLI,OFF,             DEP                     
         DC    B'00110010'             OFF,TASK,        DEP                     
         DC    B'00100010'             OFF,             DEP                     
         DC    B'00010010'                 TASK,        DEP                     
         DC    B'00000010'                              DEP                     
         DC    X'00'           END OF TABLE OF POSSIBLE SEARCHES                
         EJECT                                                                  
*----------------------------------------------------------------------         
*        GET SPACE FOR THE NAME TABLES                                          
*----------------------------------------------------------------------         
GETBUFF  NMOD1 0,*GETBUFF                                                       
         USING ACR9D,RC                                                         
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
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*        RELEASE GETMAINED SPACE                                                
*----------------------------------------------------------------------         
RELBUFF  NMOD1 0,*RELBUFF                                                       
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
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*        TABLE DESCRIBING HOW TO CHOP UP GETMAINED CORE                         
*        COVERED BY MAIND, USED BY SETMAIN                                      
*---------------------------------------------------------------------          
MAINTAB  DS    0A                                                               
         DC    S(ACREC)                                                         
         DC    AL2(0)                                                           
         DC    A(0)                                                             
         DC    A(RECSIZE)                                                       
RECSIZE  EQU   2000+49                                                          
*                                                                               
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
         DC    S(ACLITAB)                                                       
         DC    AL2(CLIHDLN)                                                     
         DC    A(CLIHEAD)                                                       
         DC    A(CLISIZE)                                                       
*                                                                               
         DC    S(ACALREC)                                                       
         DC    AL2(0)                                                           
         DC    A(0)                                                             
         DC    A(RECSIZE)                                                       
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
*                                                                               
MAINNUM  EQU   (*-MAINTAB)/MAINLN                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
*        TABLE HEADER, SETMAIN MOVES THESE OUT INTO GETMAINED CORE              
*--------------------------------------------------------------------           
*                                                                               
EMPHEAD  DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(EMPLEN)         RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(L'EMPKEY)       KEY LENGTH                                   
         DC    A(EMPMAX)           MAX IN TABLE                                 
EMPHDLN  EQU   *-EMPHEAD                                                        
EMPSIZE  EQU   (EMPMAX*EMPLEN)+EMPHDLN                                          
EMPMAX   EQU   7000                                                             
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
*                                                                               
*----------------------------------------------------------------------         
*        TABLE OF NEW BUSINESS AND PROBONO CLIENTS                              
*----------------------------------------------------------------------         
CLIHEAD  DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(NBPBLEN)        RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(NBPBLEN)        KEY LENGTH                                   
         DC    A(MAXACC)           MAX IN TABLE                                 
CLIHDLN  EQU   *-CLIHEAD                                                        
CLISIZE  EQU   (MAXACC*NBPBLEN)+CLIHDLN                                         
MAXACC   EQU   2500                                                             
         EJECT                                                                  
*              DSECT FOR STORAGE AREA                                           
ACR9D    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
UNDERLIN DS    V                                                                
VGETSTD  DS    V                                                                
*                                                                               
ADBOX    DS    A                                                                
SAVERE   DS    F                                                                
*                                                                               
ABUFF    DS    A                                                                
ACREC    DS    A                                                                
AEMPLST  DS    A                                                                
ACLTLST  DS    A                                                                
ACLITAB  DS    A                                                                
ACALREC  DS    A                                                                
ACALKEYS DS    A                                                                
ASTDKEYS DS    A                                                                
ASTDHRS  DS    A                                                                
AACCUMS  DS    A                                                                
APERBLK  DS    A                                                                
*                                                                               
ACR9DATA DS    0C                  START ADDRESS FOR MVCL CLEAR                 
PROFILES DS    CL12                SAVED COMPANY LEVEL PROGPROF                 
NUMNON   DS    CL1                                                              
COMMAND  DS    CL6                                                              
FILE     DS    CL8                                                              
ELCODE   DS    CL1                                                              
BILLED   DS    CL1                 FULLY BILLED SWITCH                          
START    DS    CL3                 START DATE PACKED YMD                        
ENDATE   DS    CL3                 END DATE PACKED YMD                          
HIREDATE DS    CL3                 HIRE DATE PACKED YMD                         
FIREDATE DS    CL3                 TERM DATE PACKED YMD                         
HOLIDAYS DS    PL4                 NUMBER OF HOLIDAYS IN A MONTH                
BDSTRT   DS    CL2                 START DATE FOR BUDGETS PACKED YM             
BDEND    DS    CL2                 END DATE FOR BUDGETS PACKED YM               
BUDSTAT  DS    CL1                 STATUS FOR TRG PCT BULLSHIT                  
NOPROF   EQU   1                                                                
NOBUD    EQU   2                                                                
*                                                                               
PERSTAT  DS    XL1                                                              
PERISNEW EQU   1                   PERSON IS ON NEW COST RECORDS                
PERHASTS EQU   2                   PERSON HAS TIME SHEET DATA FOR REQ           
*                                                                               
PERINOUT DS    XL(PERLMAX*6)       SETS OF INOUTS ( 2 XL3 DATES)                
*                                                                               
BILDATE  DS    CL3                 BILLING DATE PACKED YMD                      
MOS      DS    CL2                 MONTH OF SERVICE FROM SJ TRANS               
M1START  DS    PL2                 MOS START DATE PACKED                        
M1END    DS    PL2                 MOS END DATE PACKED                          
NOTIMESH DS    CL1                 Y= REJECT ALL TYPE 57'S FOR THIS 49          
ISBURSON DS    CL1                 Y= BURSON STYPE STD VAL, TRGT %              
CLILEN   DS    CL1                 LEVEL A LEN                                  
PROLEN   DS    CL1                       B                                      
COMPNAM  DS    CL36                COMPANY NAME FOR HEADLINES                   
BILLTHRU DS    CL38                HEADLINE TO PRINT MOA RANGE                  
EMPLEVA  EQU   1                                                                
EMPLEVB  EQU   2                                                                
EMPLEVC  EQU   3                                                                
EMPLEVD  EQU   4                                                                
EMPLOYEE DS    0CL15               1R EMPLOYEE                                  
         DS    CL3                 CO/U/L                                       
EMPCODE  DS    CL12                                                             
*                                                                               
BDKEY    DS    CL15                READ BUDGETS FOR THIS ACCOUNT                
BUSTDHRS DS    XL2                 BUD NUMBER, MODE TO READ AT                  
         DS    PL8                                                              
BUSTDRAT DS    XL2                                                              
         DS    PL8                                                              
BUTRGPCT DS    XL2                                                              
         DS    PL8                                                              
*                                                                               
SJACCT   DS    0CL15               SJ ACCOUNT                                   
         DS    CL3                 CO/U/L                                       
SJCODE   DS    0CL12                                                            
SJCLIENT DS    CL3                          -CLIENT                             
SJPROD   DS    CL3                          -PRODUCT                            
SJJOB    DS    CL6                          -JOB                                
*                                                                               
*                                  STANDARD MONTHLY HOURS TABLE                 
*                                  1 OFFICE CODE, 2YM PACKED, 4 YYMM            
STDLIST  DS    CL264               EBCDIC, PL4 HRS (11 X 24 = 264)              
         DC    X'FF'                                                            
*                                  EMPLOYEE MONTHLY CLIENT HOURS TABLE          
*                                  2 YM PACKED, PL6 HRS                         
HRSLIST  DS    CL192                                                            
         DC    X'FF'                                                            
*                                                                               
TRGPLIST DS    CL192               MONTHLI TARGET PCT FOR BURSON                
         DC    X'FF'                                                            
*                                                                               
PL16     DS    PL16                                                             
ANSWER   DS    PL14                FOR DIVIDE                                   
SUBTRACT DS    PL14                                                             
SAVEKEY  DS    CL(ACCKLEN)         KEY FOR DIRECTORY READ                       
MYKEYSV  DS    CL(ACCKLEN)         KEY FOR DIRECTORY READ                       
DIRKEY   DS    CL(ACCKLEN)         KEY FOR DIRECTORY READ                       
BUDERR   DS    CL30                MSG AREA FOR BAD BUDGETS                     
DAYSINMM DS    PL2                 PRORATED DAYS AREA                           
RATEXHR  DS    CL8                 RATE X STD HRS                               
BUDGAMT  DS    CL8                 CUMULATOR FOR BUDGET AMTS                    
TARGET   DS    CL7                 TARGET UTILIZATION PERCENTAGE                
BDTYPE   DS    B'0'                TYPE OF BUDGET                               
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
LVATITLE DS    CL15                1R LEVEL TITLES                              
LVBTITLE DS    CL15                                                             
LVCTITLE DS    CL15                                                             
LVDTITLE DS    CL15                                                             
*                                                                               
OFCODE   DS    CL2                                                              
RNAMES   DS    0CL144                                                           
OFNAME   DS    CL36                1R OFFICE NAME                               
DPNAME   DS    CL36                1R DEPT NAME                                 
CATNAME  DS    CL36                1R SUB-DEPT NAME                             
EMPLNAME DS    CL36                1R STAFF NAME                                
*                                                                               
CNAMES   DS    0CL108                                                           
CLNAME   DS    CL36                SJ CLIENT NAME                               
PDNAME   DS    CL36                SJ PRODUCT NAME                              
JOBNAME  DS    CL36                SJ JOB NAME                                  
*                                                                               
ALSORT   DS    A                   A(LAST SORT RECORD)                          
SRTWRK   DS    (SRTLNQ)C           WORK AREA FOR SORT RECORD                    
LSTWRK   DS    (SRTLNQ)C           WORK AREA FOR LAST RECORD                    
*                                                                               
TOTSW    DS    CL1                 LEVEL OF TOTAL SWITCH                        
         SPACE 3                   **REPORT TOTAL ACCUMS**                      
*                                                                               
*****************************************************************               
AMNTTBLE DS    0CL150              TABLE OF AMOUNTS             *               
BHRO     EQU   *-AMNTTBLE                                                       
BILHR    DS    CL10                BILLABLE HOURS               *               
*                                                                               
BSHRO    EQU   *-AMNTTBLE                                                       
BILSTHR  DS    CL6                 BILLABLE/STANDARD HRS        *               
*                                                                               
BDHRO    EQU   *-AMNTTBLE                                                       
BILDHR   DS    CL10                BILLED HOURS                 *               
*                                                                               
WHRO     EQU   *-AMNTTBLE                                                       
WOHR     DS    CL10                WRITE-OFF HOURS              *               
*                                                                               
HHRO     EQU   *-AMNTTBLE                                                       
HELDHR   DS    CL10                HELD HOURS                   *               
*                                                                               
BVVHRO   EQU   *-AMNTTBLE                                                       
BVVHR    DS    CL10                BILLED VS VALUE  HRS         *               
*                                                                               
BVVPO    EQU   *-AMNTTBLE                                                       
BVVPER   DS    CL6                 BILLED VS VALUE PERCENT      *               
*                                                                               
BVSHRO   EQU   *-AMNTTBLE                                                       
BVSHR    DS    CL10                BILLED VS STAND  HRS         *               
*                                                                               
BVVSPO   EQU   *-AMNTTBLE                                                       
BVSPER   DS    CL6                 BILLED VS STAND PERCENT      *               
*                                                               *               
BADOLO   EQU   *-AMNTTBLE                                                       
BILDOL   DS    CL13                BILLABLE DOLLARS             *               
*                                                               *               
BDDOLO   EQU   *-AMNTTBLE                                                       
BILDDOL  DS    CL13                BILLED DOLLARS               *               
*                                                               *               
WODOLO   EQU   *-AMNTTBLE                                                       
WODOL    DS    CL13                WRITE-OFF DOLLARS            *               
*                                                               *               
HEDOLO   EQU   *-AMNTTBLE                                                       
HELDDOL  DS    CL13                TIME HELD DOLLARS            *               
*                                                               *               
BVVDOLO  EQU   *-AMNTTBLE                                                       
BVVDOL   DS    CL13                BILLED VS VALUE DOLLARS      *               
*                                                               *               
BVSDOLO  EQU   *-AMNTTBLE                                                       
BVSDOL   DS    CL13                BILLED VS STAND DOLLARS      *               
*                                                               *               
AMNTLENQ EQU   *-AMNTTBLE          LENGTH OF AMOUNT TABLE       *               
*****************************************************************               
CNTNUM   DS    XL1                                                              
NUMCOLS  DS    XL1                 # OF COLUMNS IN RPT                          
WKAREA   DS    CL60                TEMP WORK AREA                               
HEADBLOC DS    3CL130              CONTAINS COLUMN HEADINGS                     
PRNTBLOC DS    2CL165              PRINT AREA                                   
PRN2BLOC DS    CL165               USED FOR STD HRS LINE                        
*                                                                               
EMPIS    DS    CL1                 LEVELS OF 1R TOTALS NEEDED                   
EMPISLVA EQU   X'01'                                                            
EMPISLVB EQU   X'02'                                                            
EMPISLVC EQU   X'04'                                                            
EMPISLVD EQU   X'08'                                                            
MONSTART DS    PL2                 START MONTH FOR REPORT                       
ACR9DLEN EQU   *-ACR9DATA          LENGTH FOR MVCL CLEAR                        
         EJECT                                                                  
*              DSECT FOR SORT RECORD                                            
*                                                                               
SRTD     DSECT                                                                  
SRTKEY   DS    0C                                                               
SRTEMPL  DS    CL12                EMPLOYEE'S  1R ACCOUNT                       
SRTEMPLN EQU   *-SRTEMPL                                                        
SRTKLNQ  EQU   *-SRTKEY            SORT KEY LENGTH                              
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
SRTHRSBL DS    PL7                 HOURS BILLED   (X'4B')                       
SRTBLVAL DS    PL7                 BILLING VALUE IN SJ                          
SRTBILLD DS    PL7                 FEES BILLED                                  
SRTSTRAT DS    PL7                 STANDARD DOLLARS                             
SRTWOHR  DS    PL7                 WRITE-OFF HOURS                              
SRTWODO  DS    PL7                 WRITE-OFF DOLLARS                            
SBUKCONT EQU   (*-SRTTARGT)/(L'SRTTARGT)    # OF BUCKETS                        
SRTLNQ   EQU   *-SRTKEY            RECORD LENGTH                                
         EJECT                                                                  
ACCUMSD  DSECT                     TO COVER AACCUMS                             
*                                  OVERALL REPORT TOTAL                         
RPTOT    DS    (TOTLEN)C                                                        
*                                                                               
*                                  (1R) OFFICE ACCUMS                           
OFTOT    DS    (TOTLEN)C                                                        
*                                                                               
*                                  (1R) DEPT ACCUMS                             
DPTTOT   DS    (TOTLEN)C                                                        
*                                                                               
*                                  (1R) CAT ACCUMS                              
CTTOT    DS    (TOTLEN)C                                                        
*                                                                               
*                                  (1R) EMPL ACCUMS                             
EMTOT    DS    (TOTLEN)C                                                        
*                                                                               
BUKCOUNT EQU   (*-ACCUMSD)/(BUCKLN)    # OF ACCUMS                              
BUKCONT1 EQU   (*-EMTOT)/(BUCKLN)                   EMPL                        
BUKCONT2 EQU   (*-CTTOT)/(BUCKLN)                   CAT                         
BUKCONT3 EQU   (*-DPTTOT)/(BUCKLN)                  DPT                         
BUKCONT4 EQU   (*-OFTOT)/(BUCKLN)                   OFF                         
LEVCOUNT EQU   (*-ACCUMSD)/(TOTLEN)    # OF LEVEL TOTALS                        
BUCKLN   EQU   7                                                                
**************************                                                      
* DSECT FOR COLUMN TABLE *                                                      
**************************                                                      
COLUMND  DSECT                                                                  
COLNUM   DS    CL1                                                              
COLOFF   DS    CL1                 OFFSET OF COLUMN                             
COLLEN   DS    CL1                 COL LENGTH                                   
         DS    CL1                 SPARE                                        
COLNAME  DS    F                   A(COLUMN HEADER)                             
*                                                                               
*****************************                                                   
* DSECT FOR BINSRCH PARAMETERS                                                  
**************************                                                      
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINTABLE DS    0CL1                                                             
         SPACE 2                                                                
**************************                                                      
* DSECT FOR 1R CODE LIST TABLE                                                  
**************************                                                      
EMPCDE   DSECT                                                                  
EMPKEY   DS    CL15                                                             
EMPNAME  DS    CL36                                                             
EMPHIRE  DS    CL3                 HIRE DATE                                    
EMPFIRE  DS    CL3                 FIRE DATE                                    
EMPTRGP  DS    CL192               MONTHLI TARGET PCT FOR BURSON                
EMPLEN   EQU   *-EMPKEY                                                         
         SPACE 2                                                                
**************************                                                      
* DSECT FOR CLT LIST TABLE                                                      
**************************                                                      
CLTCDE   DSECT                                                                  
CLTKEY   DS    CL12                                                             
CLTNAME  DS    CL36                                                             
CLTLEN   EQU   *-CLTKEY                                                         
         SPACE 2                                                                
**************************                                                      
* DSECT FOR TOTAL LINE ACCUMS                                                   
**************************                                                      
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
         SPACE 2                                                                
**************************                                                      
* DSECT FOR BUDGET PARAMETERS                                                   
**************************                                                      
BUDD     DSECT                                                                  
BUDMODE  DS    CL1                                                              
BUDNUM   DS    CL1                                                              
BUDAMNT  DS    PL8                                                              
         SPACE 2                                                                
*---------------------------------------------------------------------          
* DSECT FOR TABLE OF NEW BUSINESS,PRO BONO ACCOUNTS                             
*---------------------------------------------------------------------          
NBPBD    DSECT                                                                  
NBPBACC  DS    CL6                 ###########################                  
NBPNKLN  EQU   *-NBPBD                                                          
NBPBLEN  EQU   *-NBPBD                                                          
         EJECT                                                                  
*---------------------------------------------------------------------          
* DSECT FOR TABLE DESCRIBING HOW TO CHOP UP GETMAINED CORE                      
*---------------------------------------------------------------------          
MAIND    DSECT                                                                  
MAINST   DS    S                   WHERE TO STORE THE ADDRESS TABLE             
MAINHDLN DS    AL2                 LENGTH OF HEADER DATA                        
MAINHEAD DS    A                   ADDRESS OF HEADER DATA                       
MAINSIZE DS    A                   TOTAL SIZE OF THIS TABLE                     
MAINLN   EQU   *-MAIND                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
BUFFSIZE EQU   RECSIZE+EMPSIZE+CLTSIZE+CLISIZE+RECSIZE+GPBUFSZQ+GSBUFSZX        
               Q+GSOPLENQ+(BUKCOUNT*BUCKLN)+LPERBLK                             
*                                                                               
LPERBLK  EQU   1000                                                             
*                                                                               
PRORATAD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACPRORATAD                                                     
         PRINT ON                                                               
*        DDLOGOD                                                                
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        ACGENMODES                                                             
*        ACMD                                                                   
*        ACBIGPRNTD                                                             
*        DDBIGBOX                                                               
*        DDCNTRL                                                                
*        DDREPXTRAD                                                             
*        DDREPMASTD                                                             
*        DDBOXEQUS                                                              
*        DDREMOTED                                                              
*        ACGETSTDD                                                              
*        ACPERCALLD                                                             
GOBLOCKD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
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
*                                                                               
***********************************************************************         
* WORK AREA FOR ACGETRATE                                             *         
***********************************************************************         
*                                                                               
GRPARMSD DSECT                     PARAMETERS                                   
GRAKEYIN DS    A                   INPUT KEY                                    
GRADATIN DS    A                   INPUT DATE                                   
GRADMGR  DS    A                                                                
GRAOPRAT DS    A                   A(PL8 OUTPUT BUFFER                          
*                                                                               
GRWORKD  DSECT                                                                  
GRDMCB   DS    6A                                                               
GREDATE  DS    XL3                 EFFECTIVE DATE                               
GRIO     DS    CL2049              IO AREA                                      
GRKEY    DS    CL(ACCKLEN)                                                      
GRKEYSV  DS    CL(ACCKLEN)                                                      
GRHIDATE DS    XL3                 DATE OF FOUND RATE ELEMENT                   
GRRATE   DS    PL8                                                              
GRSTAT   DS    CL1                                                              
GRFOUND  EQU   1                                                                
GRWRKLN  EQU   *-GRWORKD                                                        
         EJECT                                                                  
***********************************************************************         
*              DSECT FOR SEARCH TABLE 1                               *         
***********************************************************************         
*                                                                               
SRCH1D   DSECT                                                                  
S1STAT   DS    XL1                 DESCRIBES HOW TO BUILD KEY                   
S1PRO    EQU   X'80'               PRODUCT                                      
S1CLI    EQU   X'40'               CLIENT                                       
S1OFF    EQU   X'20'               OFFICE                                       
S1TSK    EQU   X'10'               TASK                                         
S1STF    EQU   X'08'               STAFF                                        
S1SUB    EQU   X'04'               SUBDEPT                                      
S1DPT    EQU   X'02'               DEPT                                         
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPR902 12/11/09'                                      
         END                                                                    
