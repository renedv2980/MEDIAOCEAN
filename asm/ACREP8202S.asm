*          DATA SET ACREP8202S AT LEVEL 044 AS OF 05/01/02                      
*PHASE AC8202A,+0                                                               
*INCLUDE ACLIST                                                                 
***********************************************************************         
*              PROFILES                                               *         
*              --------                                               *         
* 1      SUMMARY BY ACCOUNT                                           *         
* 2      SUMMARY BY CONTRA ACCOUNT                                    *         
* 3      SUPPRESS ZERO BALANCES                                       *         
* 4      SUPPRESS SUB-TOTALS                                          *         
* 5      LEVEL TOTALS                                                 *         
* 6      SUPPRESS CONTRA ACCOUNT (MUTUALLY EXCLUSIVE-NOT IN USE)      *         
* 7      SORT FIELD 1 R-ADV NUMB*-DFLT(ADV NUMB)  N-NO ADV NUMB       *         
* 8      SORT FIELD 2 D-ADV DATE   *-DFLT(ADV DATE)   N-NO ADV DATE   *         
* 9      SORT FIELD 3 C-CONTRA ACC *-DFLT(CONTRA ACC) N-NO CONTRA ACC *         
*                                                                     *         
*        ACREP8202 REPORTS ON CASH ADVANCES MADE TO INDIVIDUALS. DATA *         
*        IS PRESENTED AS DICTATED BY THE SORT FIELD(S) PROFILE INPUT. *         
*        TOTALS AND SUMMARIES ARE AVAILABLE AS PER PROFILE QUESTIONS. *         
***********************************************************************         
*                                                                               
         TITLE 'AC8202 - ADVANCES STATEMENT'                                    
AC8202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC82**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(WORK CSECT)                             
         LA    RC,SPACEND                                                       
         USING AC82D,RC            RC=A(SPACEND)                                
*                                                                               
*              HANDLE MODE SETTINGS                                             
*                                                                               
         CLI   MODE,REQFRST        * FIRST FOR REQUEST                          
         BE    REQ2                                                             
         CLI   MODE,LEDGFRST       * FIRST FOR LEDGER                           
         BE    FRSTLEDG                                                         
         CLI   MODE,PROCACC        * ACCOUNT FIRST                              
         BE    ACCPROC                                                          
         CLI   MODE,SBACFRST       * SUB-ACCOUNT 1ST                            
         BE    SUBA2                                                            
         CLI   MODE,PROCTRNS       * TRANACTIONS                                
         BE    TRNS2                                                            
         CLI   MODE,ACCLAST        * ACCOUNT LAST                               
         BE    LASTACC                                                          
         CLI   MODE,LEVCLAST       * LEVEL C LAST                               
         BE    LASTLEVC                                                         
         CLI   MODE,LEVBLAST       * LEVEL B LAST                               
         BE    LASTLEVB                                                         
         CLI   MODE,LEVALAST       * LEVEL A LAST                               
         BE    LASTLEVA                                                         
         CLI   MODE,REQLAST        * LAST FOR REQUEST                           
         BE    LASTREQ                                                          
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
*              FIRST FOR REQUEST                                                
*                                                                               
REQ2     GOTO1 BUFFALO,DMCB,=C'SET',ADBUFF                                      
         ZAP   COUNT,=P'0'                                                      
         MVC   HEADMON,SPACES                                                   
         L     R4,AMONACC                                                       
         USING ACMD,R4                                                          
         CLI   ACMMEND,X'FF'                                                    
         BE    REQ1A                                                            
         CLI   ACMMSTR,0                                                        
         BE    REQ1B                                                            
         CLC   ACMMSTR,ACMMEND                                                  
         BE    REQ1C                                                            
         SPACE                                                                  
         MVC   HEADMON(16),=C'MMM/YY TO MMM/YY'                                 
         MVC   HEADMON(L'ACMCMSTR),ACMCMSTR                                     
         MVC   HEADMON+10(L'ACMCMEND),ACMCMEND                                  
         B     REQ1D                                                            
         SPACE                                                                  
REQ1A    MVC   HEADMON(8),=C'MONTH OF'                                          
         GOTO1 DATCON,DMCB,(4,RCDATE),(6,HEADMON+9)                             
         B     REQ1D                                                            
         SPACE                                                                  
REQ1B    MVC   HEADMON(13),=C'POSTINGS THRU'                                    
         MVC   HEADMON+14(L'ACMCMEND),ACMCMEND                                  
         B     REQ1D                                                            
         SPACE                                                                  
REQ1C    MVC   HEADMON(20),=C'MMM/YY POSTINGS ONLY'                             
         MVC   HEADMON(L'ACMCMSTR),ACMCMSTR                                     
         SPACE                                                                  
REQ1D    LA    R7,PROGPROF                                                      
         USING PROFDSEC,R7                                                      
         ZAP   MINIMP,=P'0'                                                     
         CLC   QSRTAREA(3),SPACES             MINIMUM REQUESTED                 
         BE    REQ4                                                             
         LA    R1,0                                                             
         CLI   QSRTAREA+1,C' '                                                  
         BE    PACKIT                                                           
         LA    R1,1                                                             
         CLI   QSRTAREA+2,C' '                                                  
         BE    PACKIT                                                           
         LA    R1,2                                                             
PACKIT   EX    R1,*+8                                                           
         B     *+10                                                             
         SPACE                                                                  
         PACK  MINIMP,QSRTAREA(0)                                               
         MP    MINIMP,=P'10000'      PUT IT IN HUNDREDS OF DOLLARS              
REQ4     MVC   REQNAME,SPACES      SET-UP HEDLINE DATA                          
         MVC   REQNAME(9),=C'REQUESTOR'                                         
         MVC   REQNAME+10(12),QUESTOR                                           
REQ6     MVC   COMPNAME,SPACES                                                  
         L     R5,VEXTRAS                                                       
         USING RUNXTRAD,R5                                                      
         CLC   ORIGINAM,SPACES                                                  
         BE    REQ7                                                             
         MVC   COMPNAME(33),ORIGINAM                                            
         MVC   COMPADDR(52),SPACES                                              
         GOTO1 CHOPPER,DMCB,(33,ORIGINAD),(26,COMPADDR),2                       
         B     REQ10                                                            
         SPACE                                                                  
REQ7     DS    0H                                                               
         L     RF,ADCMPNAM                                                      
         ZIC   R1,1(RF)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         SPACE                                                                  
         MVC   COMPNAME(0),2(RF)                                                
REQ8     MVC   COMPADDR,SPACES                                                  
         L     RF,ADCMPADD                                                      
         ZIC   R1,1(RF)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         SPACE                                                                  
         MVC   COMPADDR(0),3(RF)                                                
REQ10    MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         BAS   RE,BUILDATE         SET UP DATE RANGE TABLE                      
         BAS   RE,FORMDATE         FORMAT DATE COLUMN HEADINGS FOR RPT          
*                                                                               
         LA    R5,ADVWK            INIT FINAL WORD OF BIN REC KEY               
         USING ADVANCED,R5         USED TO INDIVIDUALIZE DTL RECS FOR           
         ZAP   ADVRECNO,=P'0'      FURTHER DETAIL REPORTING                     
*                                                                               
         MVC   MASK(3),=C'RDC'                                                  
         MVC   SVACCTBK(40),=5PL8'0'     INIT ACCT TOT HOLD                     
         B     EXIT                                                             
         DROP  R4                                                               
         DROP  R5                                                               
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
*              FIRST FOR LEDGER                                                 
*                                                                               
FRSTLEDG BAS   RE,FORMDTL     FORMAT DETAIL COLM HDGS-SORTSEQ                   
*                                                                               
         XC    SAVLEVA(4),SAVLEVA  MOVE IN LEVEL CODES                          
         L     R6,ADLDGHIR                                                      
         USING ACHEIRD,R6                                                       
         LA    R1,ACHRLEVD                                                      
         LA    RE,4                                                             
         CLI   0(R1),0                                                          
         BNE   *+12                                                             
         SH    R1,=H'16'                                                        
         BCT   RE,*-12                                                          
         STC   RE,LEVELS                                                        
         MVC   SAVLEVA(1),ACHRLEVA L' OF LEVA                                   
         CLI   ACHRLEVB,0                                                       
         BE    EXIT                                                             
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         IC    R2,ACHRLEVA                                                      
         IC    R3,ACHRLEVB                                                      
         SR    R3,R2                                                            
         STC   R3,SAVLEVB                                                       
         CLI   ACHRLEVC,0                                                       
         BE    EXIT                                                             
         IC    R3,ACHRLEVB                                                      
         IC    R2,ACHRLEVC                                                      
         SR    R2,R3                                                            
         STC   R2,SAVLEVC                                                       
         CLI   ACHRLEVD,0                                                       
         BE    EXIT                                                             
         IC    R2,ACHRLEVD                                                      
         IC    R3,ACHRLEVC                                                      
         SR    R2,R3                                                            
         STC   R2,SAVLEVD                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              PROCACC                                                          
*                                                                               
ACCPROC  DS    0H                                                               
***                                                                             
         MVI   SUPLOCK,0                INITIALIZE INDICATOR                    
         CLI   QOPT3,C'Y'               SUPPRESS LOCKED ACCTS?                  
         BNE   ACCP200                                                          
         L     R2,ADACC                 IF YES CHECK AND SEE IF ACCT            
         MVI   ELCODE,X'30'             IS LOCKED                               
         BAS   RE,GETEL                                                         
         BNE   ACCP200                                                          
         USING ACSTATD,R2                                                       
         TM    ACSTSTAT,X'20'           IF ACCT IS LOCKED -                     
         BNO   ACCP200                                                          
         OI    SUPLOCK,X'80'            SET INDICATOR                           
         B     EXIT                                                             
***                                                                             
ACCP200  L     R1,ADACC                                                         
         L     R2,ADACCNAM                                                      
         LA    R3,LEVCACC                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   0(51,R3),SPACES                                                  
         MVC   0(15,R3),0(R1)                                                   
         ZIC   R1,1(R2)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   15(0,R3),2(R2)                                                   
         MVC   THISACCT,0(R3)                                                   
         MVI   RCSUBPRG,0          ADVANCE STMNT BANNER                         
         ZAP   RECNO,=P'0'         CLEAR ADVRECNO VALUE HOLD                    
         B     EXIT                                                             
         EJECT                                                                  
*              FIRST FOR SUB-ACCOUNT                                            
*                                                                               
SUBA2    L     R2,ADSUBAC                                                       
         USING TRSUBHD,R2                                                       
         MVC   THISSRCE,TRSBACNT                                                
         XC    THISREF,THISREF                                                  
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
TRNS2    MVI   BBFSW,C'N'                                                       
***                                                                             
         TM    SUPLOCK,X'80'               IF ACCT IS SUPPRESSED AND            
         BO    EXIT                        LOCKED DO NOT PROCESS TRAN           
***                                                                             
         L     R2,ADTRANS                                                       
         USING TRANSD,R2                                                        
         CLI   TRNSEL,X'44'                                                     
         BNE   EXIT                                                             
         LR    RF,R2                                                            
         SH    RF,DATADISP                                                      
         USING ACKEYD,RF                                                        
         OC    ACDTPEEL,ACDTPEEL                                                
         BNZ   EXIT                SKIP PEELED                                  
         TM    TRNSSTAT,X'20'                                                   
         BO    EXIT                SKIP REVERSALS                               
         ZAP   BILLTOT,=P'0'       CLEAR TOTAL ACCUM                            
TRNS4    MVC   THISREF,TRNSREF                                                  
         MVC   THISDATE,TRNSDATE                                                
         ZAP   THISAMNT,=P'0'      USE MOS DATE                                 
*                                                                               
TRNS4A   L     R4,AMONACC                                                       
         USING ACMD,R4                                                          
         MVC   THISMOS(2),ACMMDTE                                               
         MVI   THISMOS+2,1                                                      
*                                                                               
TRNS6    ZAP   DUB,TRNSAMNT                                                     
         TM    TRNSSTAT,X'80'                                                   
         BNZ   *+10                                                             
         MP    DUB,=P'-1'          CREDIT TRANSACTION HANDLING                  
         AP    THISAMNT,DUB                                                     
         ZAP   BILLTOT,DUB                                                      
TRNSX2   BAS   RE,BILLEND                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              LAST FOR ACCOUNT - PRINT TABLE ENTRY FOR ACCOUNT                 
*                                                                               
LASTACC  DC    0H'0'                                                            
***                                                                             
         TM    SUPLOCK,X'80'               IF ACCT IS SUPPRESSED AND            
         BO    EXIT                        LOCKED DO NOT PROCESS                
***                                                                             
         MVI   BBFSW,C'Y'                                                       
         L     R4,AMONACC                                                       
         USING ACMD,R4                                                          
         CP    ACMABAL,=P'0'       PROCESS BALANCE FORWARD                      
         BE    L04                                                              
         LA    R5,ADVWK                                                         
         USING ADVANCED,R5                                                      
         MVC   ADVBUCK1(40),=5PL8'0'          CLEAR BUCKETS                     
         LA    R2,DATAB            TABLE OF BKTS                                
         LA    R1,ADVBUCK1         1ST BKT                                      
L02      CLI   6(R2),X'00'         LAST BKT CHK                                 
         BE    L03                                                              
         LA    R1,8(R1)            NEXT BKT                                     
         LA    R2,6(R2)            NEXT DATE                                    
         B     L02                                                              
L03      ZAP   0(8,R1),ACMABAL             BBF TO EARLIEST BKT                  
         LA    R2,ADVBUCK5         TOT BKT                                      
         AP    0(8,R2),0(8,R1)     ADD TOT                                      
         MVC   ADVKEY(13),AFLD             MAKE IT FIRST                        
         MVC   ADVKEY+13(15),=CL15'BALANCE FORWARD'                             
         BAS   RE,BILLEND                                                       
         B     L05                                                              
*                                                                               
L04      OC    TRNSW,TRNSW         ANY TRANSACTIONS                             
         BZ    INITOUT             NO                                           
*                                                                               
*ANALYZE BIN TABLE ACCT TOT REC-IF SUPPRESSING 0 BALANCES DELETE                
*ACCORDINGLY. IN EITHER CASE RETURN FROM ROUTINE WITH ACCT TOTS                 
*IN SVACCTBK TO FACILITATE CORRECT POSTING OF SUMMARY RECS TO BUFFALO.          
*IN 0 BALANCE MODE ROUTINE WILL UPDATE CONTRA BUFF RECS PRIOR TO                
*BIN TABLE DELETE.                                                              
*                                                                               
L05      BAS   RE,ANALACCT                                                      
*                                                                               
L07      L     R5,AADVTAB          A(BIN TABLE)                                 
         USING BIND,R5             DSECT TO COVER BIN TABLE PARAMS              
         LA    R7,BINTABLE         A(TABLE ENTRY)                               
         USING ADVANCED,R7         DSECT TO COVER BIN TABLE ENTRIES             
         OC    BININ,BININ         ANY RECS FOR THIS ACCOUNT                    
         BZ    EXIT                NO                                           
*                                                                               
L10      BAS   RE,ACCBUFF          WRITE OUT ACCOUNT SUM RECS                   
         CLI   QOPT2,C'Y'          REQUEST FOR ACCOUNT SUMMARY ONLY             
         BE    INITOUT             YES                                          
*                                                                               
LASTACC4 BAS   RE,PRTDET                                                        
*                                                                               
INITOUT  L     R5,AADVTAB          A(BIN TABLE)                                 
         USING BIND,R5             DSECT TO COVER BIN TABLE PARAMS              
         XC    BININ,BININ         CLEAR TBL FOR NXT ACCT                       
         MVC   TRNSW,=F'0'       CLEAR TRANSACTION INDICATOR FOR ACCOUT         
         MVC   SVACCTBK(40),=5PL8'0'     INIT ACCT TOT HOLD                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              PRINT LEVEL TOTALS IF REQUESTED VIA PROFILE                      
*                                                                               
*              LEVEL C TOTALS                                                   
LASTLEVC LA    R7,PROGPROF                                                      
         USING PROFDSEC,R7                                                      
         CLI   PLEVTOTS,C'N'                                                    
         BE    EXIT                                                             
         CLI   QOPT2,C'Y'                                                       
         BE    EXIT                                                             
         LA    R4,BUFAREA                                                       
         USING BUFFD,R4                                                         
         XC    BUFAREA,BUFAREA                                                  
         MVI   BUFTYP,BUFLEVC      GET LEVEL TOTAL C                            
         MVI   BUFNAME,X'FF'                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',(=C'C',ADBUFF),BUFAREA,1                   
         TM    8(R1),X'80'                                                      
         BO    EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         CLI   PROGPROF+9,3    SAME PAGE FOR EMPLOYEE TOTAL(LVL0-3)             
         BNE   *+12                                                             
         MVI   FORCEHED,C'N'  IF YES JUST SKIP A LINE BEFORE TOTAL              
         BAS   RE,PRINTEM                                                       
         MVC   P+1(11),=C'TOTALS FOR '                                          
         L     R6,ADLVCNAM                                                      
         USING ACNAMED,R6                                                       
         ZIC   R2,ACNMLEN                                                       
         SH    R2,=H'2'                                                         
         GOTO1 CHOPPER,DMCB,((R2),ACNMNAME),(20,P+12),(=C'P',2)                 
         GOTO1 MOVEPRNT,DMCB,BUFBUCK                                            
         BAS   RE,PRINTEM                                                       
CLERCBUF GOTO1 BUFFALO,DMCB,=C'CLEAR',(=C'C',ADBUFF),(X'80',1)                  
         B     EXIT                                                             
         SPACE 3                                                                
*              LEVEL B TOTALS                                                   
LASTLEVB LA    R7,PROGPROF                                                      
         USING PROFDSEC,R7                                                      
         CLI   PLEVTOTS,C'N'                                                    
         BE    EXIT                                                             
         CLI   QOPT2,C'Y'                                                       
         BE    EXIT                                                             
         LA    R4,BUFAREA                                                       
         USING BUFFD,R4                                                         
         XC    BUFAREA,BUFAREA                                                  
         MVI   BUFTYP,BUFLEVB      GET LEVEL TOTAL B                            
         MVI   BUFNAME,X'FF'                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',(=C'B',ADBUFF),BUFAREA,1                   
         TM    8(R1),X'80'                                                      
         BO    EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         CLI   PROGPROF+9,2    SAME PAGE FOR EMPLOYEE TOTAL(LVL0-3)             
         BNE   *+12                                                             
         MVI   FORCEHED,C'N'  IF YES JUST SKIP A LINE BEFORE TOTAL              
         BAS   RE,PRINTEM                                                       
         MVC   P+1(11),=C'TOTALS FOR '                                          
         L     R6,ADLVBNAM         LEVEL B NAME                                 
         USING ACNAMED,R6                                                       
         ZIC   R2,ACNMLEN                                                       
         SH    R2,=H'2'                                                         
         GOTO1 CHOPPER,DMCB,((R2),ACNMNAME),(20,P+12),(=C'P',2)                 
         GOTO1 MOVEPRNT,DMCB,BUFBUCK                                            
         BAS   RE,PRINTEM                                                       
CLERBBUF GOTO1 BUFFALO,DMCB,=C'CLEAR',(=C'B',ADBUFF),(X'80',1)                  
         B     EXIT                                                             
         SPACE 3                                                                
*              LEVEL A TOTALS                                                   
LASTLEVA LA    R7,PROGPROF                                                      
         USING PROFDSEC,R7                                                      
         CLI   PLEVTOTS,C'N'                                                    
         BE    EXIT                                                             
         CLI   QOPT2,C'Y'                                                       
         BE    EXIT                                                             
         LA    R4,BUFAREA                                                       
         USING BUFFD,R4                                                         
         XC    BUFAREA,BUFAREA                                                  
         MVI   BUFTYP,BUFLEVA      GET LEVEL TOTAL A                            
         MVI   BUFNAME,X'FF'                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',(=C'A',ADBUFF),BUFAREA,1                   
         TM    8(R1),X'80'                                                      
         BO    EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         CLI   PROGPROF+9,1    SAME PAGE FOR EMPLOYEE TOTAL(LVL0-3)             
         BNE   *+12                                                             
         MVI   FORCEHED,C'N'  IF YES JUST SKIP A LINE BEFORE TOTAL              
         BAS   RE,PRINTEM                                                       
         MVC   P+1(11),=C'TOTALS FOR '                                          
         L     R6,ADLVANAM                                                      
         USING ACNAMED,R6                                                       
         ZIC   R2,ACNMLEN                                                       
         SH    R2,=H'2'                                                         
         GOTO1 CHOPPER,DMCB,((R2),ACNMNAME),(20,P+12),(=C'P',2)                 
         GOTO1 MOVEPRNT,DMCB,BUFBUCK                                            
         BAS   RE,PRINTEM                                                       
CLERABUF GOTO1 BUFFALO,DMCB,=C'CLEAR',(=C'A',ADBUFF),(X'80',1)                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              LAST FOR REQUEST - ACCOUNT AND CONTRA ACCOUNT SUMMARIES          
*                                                                               
LASTREQ  LA    R7,PROGPROF                                                      
         USING PROFDSEC,R7                                                      
         DROP  R5                                                               
         CLI   QOPT2,C'Y'          SUMMARY ONLY REQUESTED                       
         BE    CHEKACCT                                                         
         CLI   PCASUM,C'Y'         CONTRA ACCOUNT SUMMARY REQUESTED             
         BNE   CHEKACCT                                                         
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         LA    R4,BUFAREA                                                       
         USING BUFFD,R4                                                         
         XC    BUFAREA,BUFAREA                                                  
         MVI   BUFTYP,BUFCA        GET CONTRA ACCOUNT RECORDS                   
         GOTO1 BUFFALO,DMCB,=C'HIGH',(2,ADBUFF),BUFAREA,1                       
         B     LAST1A                                                           
         SPACE                                                                  
LAST1    GOTO1 BUFFALO,DMCB,=C'SEQ',(2,ADBUFF),BUFAREA,1                        
LAST1A   TM    8(R1),X'80'         NO RECORDS                                   
         BO    CHEKACCT                                                         
         CLI   BUFACCT,X'FF'       BUFFALO EOF                                  
         BE    LASTTOT                                                          
         LA    R7,PROGPROF                                                      
         USING PROFDSEC,R7                                                      
         CLI   PSUPZERO,C'N'       SUPPRESS ZERO BALANCES                       
         BE    LAST1B                                                           
         LA    R1,BUFBUCK1                                                      
         ZAP   BUCKFLD(8),=P'0'                                                 
         LA    R2,BUCKFLD          DON'T ADD TO BUCK5 YET                       
         LA    R3,4                                                             
LASTADD  AP    0(8,R2),0(8,R1)     ADD BUCKS 1-4 TO BUCKFLD                     
         LA    R1,8(R1)                                                         
         BCT   R3,LASTADD                                                       
         SPACE                                                                  
         CP    0(8,R2),=P'0'       IF ACCT BAL IS 0 - SKIP IT                   
         BNE   *+8                                                              
         B     LAST1                                                            
*                                                                               
LAST1B   MVC   P+1(14),BUFACCT+1   ACCT NUMBER + 1                              
         GOTO1 MOVEPRNT,DMCB,BUFBUCK                                            
         BAS   RE,PRINTEM                                                       
         B     LAST1                                                            
*                                                                               
LASTTOT  GOTO1 ACREPORT            SKIP A LINE                                  
         MVC   P+16(6),=C'TOTALS'                                               
         GOTO1 MOVEPRNT,DMCB,BUFBUCK                                            
         GOTO1 ACREPORT                                                         
         BAS   RE,PRINTEM                                                       
CHEKACCT EQU   *                                                                
         LA    R7,PROGPROF                                                      
         USING PROFDSEC,R7                                                      
         CLI   QOPT2,C'Y'          SUMMARY ONLY OVERRIDES PROFILE               
         BE    *+12                                                             
         CLI   PACCTSUM,C'N'       ACCOUNT SUMMARY WANTED?                      
         BE    LASTCLER                       NO                                
         LA    R4,BUFAREA                                                       
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         XC    BUFAREA,BUFAREA                                                  
         MVI   BUFTYP,BUFAC                   GO GET ACCT SUMM RECS             
         GOTO1 BUFFALO,DMCB,=C'HIGH',(1,ADBUFF),BUFAREA,1                       
         B     LAST3                                                            
         SPACE                                                                  
LAST2    GOTO1 BUFFALO,DMCB,=C'SEQ',(1,ADBUFF),BUFAREA,1                        
LAST3    TM    8(R1),X'80'         BUFF E-O-F                                   
         BO    LASTCLER                                                         
         CLI   BUFACCT,X'FF'                                                    
         BE    LAST4TOT                                                         
         MVC   P+1(14),BUFACCT+1                                                
         MVC   P+17(36),BUFNAME                                                 
         GOTO1 MOVEPRNT,DMCB,BUFBUCK                                            
         BAS   RE,PRINTEM                                                       
         B     LAST2                                                            
         SPACE                                                                  
LAST4TOT GOTO1 ACREPORT            SKIP A LINE                                  
         MVC   P+16(6),=C'TOTALS'                                               
         GOTO1 MOVEPRNT,DMCB,BUFBUCK                                            
         BAS   RE,PRINTEM                                                       
LASTCLER L     R4,AADVTAB                                                       
         USING BIND,R4                                                          
         XC    BININ,BININ         CLEAR TABLE                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              BUILD DATE RANGE TABLE USING INPUT VALUES                        
*                                                                               
BUILDATE NTR1                                                                   
         XC    DATAB,DATAB                                                      
         LA    R2,DATAB                                                         
         L     R4,AMONACC                                                       
         USING ACMD,R4                                                          
         CLI   ACMMEND,X'FF'                                                    
         BNE   UNPKDT                                                           
         GOTO1 DATCON,DMCB,(5,RCDATE),(1,WORK)                                  
         MVC   ACMMEND(2),WORK                                                  
UNPKDT   UNPK  WORK(5),ACMMEND(3)                                               
GETDATE  MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,0(R2))    1ST START DATE                 
         MVC   WORK+4(2),=C'28'                                                 
         LA    R3,1                                                             
ADD      GOTO1 ADDAY,DMCB,WORK,WORK+6,(R3)                                      
         CLC   WORK+2(2),WORK+8                                                 
         BNE   *+14                                                             
         MVC   WORK(6),WORK+6                                                   
         B     ADD                                                              
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(0,WORK),(1,3(R2))     1ST END DATE                  
         CLC   ACMMSTR,ACMMEND        ONLY 1 MONTH REQUESTED?                   
         BE    FINISHED                       YES - EXIT                        
         LA    R2,6(R2)                                                         
         LNR   R3,R3                                                            
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R3)                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,3(R2))    2ND END DATE                 
         CLC   3(2,R2),ACMMSTR                                                  
         BNL   *+14                                                             
         XC    3(3,R2),3(R2)                                                    
         B     FINISHED                                                         
         SPACE                                                                  
         MVC   WORK(6),WORK+6                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,0(R2))      2ND START DATE               
         CLC   ACMMSTR,0(R2)        ONLY 2 MONTHS REQUESTED                     
         BE    FINISHED                                                         
         LA    R2,6(R2)                                                         
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R3)                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,3(R2))     3RD END DATE                
         CLC   3(2,R2),ACMMSTR                                                  
         BNL   *+14                                                             
         XC    3(3,R2),3(R2)                                                    
         B     FINISHED                                                         
         SPACE                                                                  
         MVC   WORK(6),WORK+6                                                   
         MVC   WORK+4(2),=C'01'                                                 
         MVC   SAVEMON(6),WORK                                                  
         GOTO1 DATCON,DMCB,(0,WORK),(1,0(R2))       3RD START DATE              
         CLC   ACMMSTR,0(R2)                                                    
         BE    FINISHED            ONLY 3 MONTHS REQUESTED                      
         LA    R2,6(R2)                                                         
         CLC   ACMMSTR,SPACES       NO START DATE ENTERED                       
         BE    NOSTART                                                          
         CLC   ACMMSTR,=2X'00'      DITTO                                       
         BE    NOSTART                                                          
         UNPK  WORK(5),ACMMSTR(3)                                               
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,0(R2))       FINAL START                 
         GOTO1 ADDAY,DMCB,SAVEMON,WORK+6,(R3)                                   
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,3(R2))      FINAL END                  
         B     FINISHED                                                         
         SPACE                                                                  
NOSTART  MVC   0(3,R2),=3X'00'             FINAL START = 000000                 
         MVC   3(3,R2),=3X'FF'               FINAL END = FFFFFF                 
FINISHED B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              FORMAT DATE HEADLINES                                            
*                                                                               
FORMDATE NTR1                                                                   
         MVC   DATLIN1,SPACES                 DATE LINES 1 & 2                  
         MVC   DATLIN2,SPACES                                                   
         LA    R2,DATAB                                                         
         LA    R3,DATLIN1+1                                                     
         LA    R4,3                                                             
FORMD2A  CLI   0(R2),0             NO MORE DATES                                
         BE    FORMDX                                                           
         MVC   WORK(3),0(R2)                                                    
         GOTO1 DATCON,DMCB,(1,WORK),(6,WORK+3)                                  
         MVC   5(6,R3),WORK+3                                                   
         MVC   L'DATLIN1+5(6,R3),DASHES                                         
         LA    R2,6(R2)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,FORMD2A                                                       
         SPACE                                                                  
         CLC   0(2,R2),3(R2)                                                    
         BNE   FORMD3                                                           
         CLC   0(3,R2),=3X'00'                                                  
         BE    FORMDX                                                           
         MVC   WORK(3),0(R2)                                                    
         GOTO1 DATCON,DMCB,(1,WORK),(6,WORK+3)                                  
         MVC   5(6,R3),WORK+3                                                   
         MVI   8(R3),C'/'                                                       
         MVC   L'DATLIN1+5(6,R3),DASHES                                         
         B     FORMDX                                                           
         SPACE                                                                  
FORMD3   MVC   6(5,R3),=C'PRIOR'                                                
         MVC   L'DATLIN1+6(5,R3),DASHES                                         
         SPACE                                                                  
FORMDX   B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* PUT TRANSACTION TO APPLICABLE TABLES-EDIT FOR SUPPRESION OF 0 BALANCE         
*                                                                               
BILLEND  NTR1                                                                   
         CLI   BBFSW,C'Y'                                                       
         BE    BILLND5                                                          
         CP    BILLTOT,=P'0'                  ZERO AMOUNT                       
         BE    BILLENDX                                                         
BILL0B   LA    R5,ADVWK                                                         
         USING ADVANCED,R5                                                      
         MVC   ADVBUCKS(40),=5PL8'0'          CLEAR BUCKETS                     
         DROP  R4                                                               
         LA    R4,ADVBUCKS         BUCKETS FOR DETAIL RECS                      
         LA    R6,32(R4)           POINT TO 5TH BKT                             
BILLEND0 LA    R2,DATAB                                                         
         LA    R3,3                                                             
         MVC   WORK(3),THISMOS     USE MOS TO GET CORRECT COLUMN                
BILLEND2 CLC   WORK(2),0(R2)          POST AMOUNT INTO CORRECT BUCKET           
         BH    BILLENDX                                                         
         CLC   WORK(2),3(R2)                                                    
         BNL   BILLEND4                                                         
         LA    R4,8(R4)                                                         
         LA    R5,8(R5)                                                         
         LA    R2,6(R2)                                                         
         BCT   R3,BILLEND2                                                      
*                                                                               
         CLC   WORK(2),3(R2)                                                    
         BH    BILLENDX            LOWER THAN RANGE                             
         CLC   WORK(2),0(R2)                                                    
         BL    BILLENDX            HIGHER THAN RANGE                            
BILLEND4 AP    0(8,R4),THISAMNT                                                 
         AP    0(8,R6),THISAMNT    TOTAL BKT                                    
         MVC   TRNSW,=F'1'         INDICATE TRANSACTION FOR ACCOUNT             
*                                                                               
         LA    R5,ADVWK                                                         
         USING ADVANCED,R5                                                      
*                                                                               
         LA    R7,PROGPROF                                                      
         USING PROFDSEC,R7                                                      
         GOTO1 BUILDKEY                                                         
*                                                                               
BILLND5  GOTO1 BINADD,DMCB,ADVWK,AADVTAB  DETAIL LINE REC                       
*                                                                               
         CLI   BBFSW,C'Y'          BBF PASS                                     
         BE    BILLENDX            YES                                          
*                                                                               
         GOTO1 BLD2KEY,DMCB,ADVKEY BUILD KEY FOR SUB-TOTAL                      
*                                                                               
         GOTO1 BINADD,DMCB,ADVWK,AADVTAB  ADD SUB-TOT LEVEL                     
*                                                                               
*ALWAYS ADD IF REQUESTED                                                        
CONWRT   CLI   PCASUM,C'Y'         CONTRA ACCOUNT SUMMARY REQUESTED             
         BNE   BILLENDX                                                         
         BAS   RE,CONBUFF          ADD TO CONTRA ACCT BUFFALO                   
*                                                                               
BILLENDX B     EXIT                                                             
         EJECT                                                                  
*ADD CONTRA ACCOUNT BUFFALO RECS                                                
CONBUFF  NTR1                                                                   
         LA    R6,BUFAREA                                                       
         USING BUFFD,R6                                                         
         XC    BUFAREA,BUFAREA       ADD ENTRY TO BUFF BY CONTRA                
         MVI   BUFTYP,BUFCA                                                     
         MVC   BUFACCT(15),THISSRCE                                             
         MVC   BUFBUCK(40),ADVBUCKS                                             
         ZAP   BUFBUCK6,=P'1'                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFF,BUFAREA                              
         XC    BUFACCT(51),BUFACCT      ADD CONTRA ACCOUNT TOTAL RECORD         
         MVI   BUFACCT,X'FF'                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFF,BUFAREA                              
         DROP  R6                                                               
         B     EXIT                                                             
*                                                                               
*ADD ACCOUNT SUMMARY BUFFALO RECS                                               
ACCBUFF  NTR1                                                                   
         LA    R5,ADVWK                                                         
         USING ADVANCED,R5                                                      
         LA    R7,BUFAREA                                                       
         USING BUFFD,R7                                                         
         XC    BUFAREA,BUFAREA        ADD ENTRY TO BUFF BY ACCOUNT              
         MVI   BUFTYP,BUFAC                                                     
         MVC   BUFACCT(15),THISACCT                                             
         MVC   BUFNAME,LEVCACC+15                                               
         ZAP   BUFBUCK6,=P'1'                                                   
         MVC   BUFBUCK(40),SVACCTBK  INITIALIZEDIN ANALACCT                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFF,BUFAREA                              
         XC    BUFACCT(51),BUFACCT     ADD ENTRY FOR ACCOUNTS TOTAL REC         
         MVI   BUFACCT,X'FF'                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFF,BUFAREA                              
         CLI   QOPT2,C'Y'                                                       
         BE    EXIT                                                             
*                                                                               
         LA    R7,PROGPROF                                                      
         USING PROFDSEC,R7                                                      
         CLI   PLEVTOTS,C'N'       LEVEL TOTALS REQUESTED?                      
         BE    ACCTEND2                                                         
         DROP  R7                                                               
         LA    R7,BUFAREA                                                       
         USING BUFFD,R7                                                         
         XC    BUFAREA,BUFAREA                                                  
         MVI   BUFTYP,BUFLEVA      BUFTYP = A                                   
         MVI   BUFNAME,X'FF'                                                    
         MVC   BUFBUCK(40),SVACCTBK  INITIALIZEDIN ANALACCT                     
         ZAP   BUFBUCK6,=P'1'                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFF,BUFAREA                              
         MVI   BUFTYP,BUFLEVB      BUFTYP = B                                   
         BASR  RE,RF                                                            
         MVI   BUFTYP,BUFLEVC      BUFTYP = C                                   
         BASR  RE,RF                                                            
         MVI   BUFTYP,BUFLEVD      BUFTYP = D                                   
         BASR  RE,RF                                                            
ACCTEND2 B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE TO BUILD SORT KEY                                        
*                                                                               
BUILDKEY NTR1                                                                   
         LA    R6,SORTSEQ          'THISDATA' VALUE POINTERS                    
         XC    ADVKEY(28),ADVKEY   CLEAR KEY                                    
         CLI   PROGPROF+5,C'Y'     INDIVIDUALIZE DETAIL RECS                    
         BNE   BA                  NO                                           
         AP    RECNO,=P'1'                                                      
         MVC   ADVRECNO,RECNO      INIT END OF DTL KEY                          
*                                                                               
BA       LA    R8,3                                                             
         LA    R7,ADVKEY           A(TO BUILD KEY)                              
BLDLOOP  LA    R1,THISDATA                                                      
         CLC   0(2,R6),=H'0'    0=PROCESS ON ONE OR TWO ITEMS                   
         BE    EXIT                                                             
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         IC    R2,0(R6)                       DISPLACEMENT                      
         IC    R3,1(R6)                       LENGTH                            
         AR    R1,R2                          DISP TO DATA FLD                  
         EX    R3,KEYMOVE                                                       
         LA    R6,2(R6)                       POINT TO NEXT PARAM               
         AR    R7,R3                          BUMP KEY TO NEXT POS.             
         LA    R7,1(R7)                                                         
         BCT   R8,BLDLOOP                                                       
         SPACE                                                                  
         B     EXIT                                                             
         SPACE 2                                                                
KEYMOVE  MVC   0(0,R7),0(R1)                                                    
         EJECT                                                                  
*                                                                               
*    BUILDS KEY FOR BINADD FOR SUB-TOTALS ONLY                                  
*                                                                               
BLD2KEY  NTR1                                                                   
         L     R3,0(R1)                                                         
         SR    R7,R7                                                            
         IC    R7,SORTSEQ+1 LENGTH OF 1ST SORT FLD                              
         SR    R1,R1                                                            
         IC    R1,SORTSEQ   DISPLACEMENT OF 1ST SORT FLD IN THISDATA            
         AR    R3,R7        ADDRESS TO XC KEY FROM                              
         LA    R3,1(R3)                                                         
         SR    R2,R2                                                            
         LA    R2,27                          LENGTH OF KEY - 1                 
         SR    R2,R7                          MINUS LENGTH OF 1ST FLD           
         BCTR  R2,0                                                             
         EX    R2,ZAPKEY                      = LENGTH TO XC                    
         MVI   0(R3),X'FF'                                                      
         B     EXIT                                                             
         SPACE 2                                                                
ZAPKEY   XC    0(0,R3),0(R3)                                                    
         EJECT                                                                  
*                                                                               
*              CONTROLS PRINTING OF DETAIL-DETAIL TOT LINES                     
*                                                                               
PRTDET   NTR1                                                                   
         L     R4,AADVTAB                                                       
         USING BIND,R4                                                          
         LA    R5,BINTABLE                                                      
         USING ADVANCED,R5                                                      
         L     R0,BININ            NUM OF ENTRIES IN TABLE                      
         LTR   R0,R0                                                            
         BZ    EXIT                NO ENTRIES FOR THIS ACCT                     
*                                                                               
         CP    MINIMP,=P'0'        MINIMUM AMOUNT REQUESTED                     
         BE    PD10                NO                                           
         LR    R6,R5               SAVE A(START OF TABLE)                       
*                                                                               
PD05     CLI   ADVKEY,X'FF'        ACCT TOTS RECORD                             
         BE    PD07                                                             
         LA    R5,ADVLEQ(R5)                                                    
         B     PD05                LOOP FOR ACCT TOT REC                        
PD07     LA    R5,ADVTOTLN(R5)     TO ACCT TOT BKT                              
         CP    MINIMP,0(8,R5)      MINIMUM TO ACCT TOTAL                        
         BH    EXIT                ACCT TOT BELOW MINIMUM                       
*                                                                               
         LR    R5,R6               RESTORE                                      
PD10     CLC   ADVKEY(13),AFLD     BBF REC                                      
         BNE   BUCKLOOP                                                         
         MVC   P+1(25),=CL25'BALANCE BROUGHT FORWARD'                           
         GOTO1 MOVEPRNT,DMCB,ADVBUCKS                                           
         B     PRNTBBF                                                          
*                                                                               
BUCKLOOP CLI   ADVKEY,X'FF'                   ACCT TOTS RECORD                  
         BE    PRTATONL                                                         
*                                                                               
         LA    R6,SORTSEQ                     SORT SEQ VALUES                   
         LA    R7,ADVKEY                                                        
         SR    R2,R2                                                            
         IC    R2,1(R6)                       LENGTH OF SORT FLD                
         AR    R7,R2                          ADDED TO ADDR OF KEY              
         LA    R7,1(R7)                       PLUS 1 = 2ND SORT FLD             
         CLI   0(R7),X'FF'    IS IT SOURCE TOTALS RECORD                        
         BNE   B10                 NO                                           
         CLI   PROGPROF+3,C'Y'     ARE SUB-TOTS SUPPRESSED                      
         BE    BUMPBUCK            YES                                          
         B     PRTATST                                                          
*                                                                               
B10      CLI   PROGPROF+5,C'Y'     DETAIL REPORTING                             
         BNE   B10A                NO                                           
         OC    CONTRASW,CONTRASW   SUPPRESSING CONTRA                           
         BZ    B10A                NO                                           
         BAS   RE,ACCUMDTL         READS AHEAD TO ACCUM ALIKE ENTS              
         BZ    B10A                CC=NO ACCUMALATION TOOK PLACE                
         L     R7,FULL        =A(ENTRY CONTAINING ACCUMED AMMOUNTS)             
         L     R5,FULL        =A(ENTRY CONTAINING ACCUMED AMMOUNTS)             
*                                                                               
B10A     LA    R7,ADVLEQ(R7)       TO A POSSIBLE SUB TOT FF                     
         ST    R7,WORD             SAVE TO CHECK                                
*                                                                               
*DROP THROUGH FOR DETAIL LINE PROCESS                                           
*                                                                               
         LA    R3,P+1                         DETAIL RECORD                     
         LA    R8,3                                                             
         LA    R7,ADVKEY                                                        
PLOOP    SR    R2,R2                                                            
         IC    R2,1(R6)                       LENGTH OF SORT FIELD              
         CH    R2,=H'2'       IS IT DATE FIELD?                                 
         BNE   CONTLOOP           NO                                            
*                                                                               
         OC    0(3,R7),0(R7)                                                    
         BZ    CONTLOOP            NO DATE                                      
         GOTO1 DATCON,DMCB,(1,0(R7)),(8,0(R3))                                  
         LA    R3,11(R3)   TO NEXT PRINT POSITION                               
         B     CONT4        ADJUST ACCORDINGLY                                  
*                                                                               
CONTLOOP CH    R2,=H'14'     IS IT CONTRA ACCOUNT?                              
         BNE   CONT2          NO                                                
         OC    CONTRASW,CONTRASW   IF SET DON'T PRINT                           
         BNZ   CONT2                                                            
         LR    R4,R3                                                            
         BCTR  R2,0           ADJUST LENGTH OF MOVE FOR CONTRA +1 L'14          
         EX    R2,MVCCA                                                         
         IC    R2,1(R6)       RESET LENGTH FOR BUMP TO NEXT FIELD               
         LR    R3,R4                                                            
         LA    R3,16(R3)           TO NEXT PRINT POSITION                       
         B     CONT4                                                            
*                                                                               
CONT2    CH    R2,=H'5'                                                         
         BNE   CONT4                                                            
         LR    R4,R3                                                            
         EX    R2,MVCP        USED FOR REF# FLD ONLY                            
         LR    R3,R4                                                            
         LA    R3,11(R3)           TO NEXT PRINT POSITION                       
*                                                                               
CONT4    CLC   2(2,R6),=H'0'       IS THAT IT                                   
         BE    CONT5               YES                                          
         AR    R7,R2         BUMP KEY TO END OF SORT FIELD                      
         LA    R7,1(R7)      ADD 1 TO POINT TO NEXT AVAILABLE POS.              
         LA    R6,2(R6)      POINT R6 TO NEXT SORT PARAMETER                    
         BCT   R8,PLOOP                                                         
*                                                                               
CONT5    CP    COUNT,=P'0'         FIRST PASS THIS DTL STRING                   
         BNE   CONT5A                                                           
         CLI   PROGPROF+3,C'Y'     ARE SUB-TOTS SUPPRESSED                      
         BE    CONT5A              YES                                          
         L     R1,WORD       STORED A(NEXT POSSIBLE SUB-TOT X'FF')              
         CLI   0(R1),X'FF'                                                      
         BNE   CONT5A                                                           
         MVI   SUBSW,X'01'                                                      
CONT5A   AP    COUNT,=P'1'                                                      
PRNTBBF  GOTO1 MOVEPRNT,DMCB,ADVBUCKS                                           
         BAS   RE,PRINTEM                                                       
         GOTO1 ACREPORT            SKIP A LINE                                  
BUMPBUCK LA    R5,ADVLEQ(R5)     BUMP TO NEXT BINSRCH TABLE ENTRY               
         B     BUCKLOOP                                                         
*                                                                               
PRTATST  CP    COUNT,=P'1'                                                      
         BNH   PRTANEXT                                                         
         MVC   P+16(9),=C'SUB-TOTAL'                                            
         MVI   SUBSW,X'01'                                                      
         GOTO1 MOVEPRNT,DMCB,ADVBUCKS                                           
         BAS   RE,PRINTEM     GO PRINT THIS ENTRY                               
         GOTO1 ACREPORT            SKIP A LINE                                  
PRTANEXT ZAP   COUNT,=P'0'                                                      
         LA    R5,ADVLEQ(R5)     POINT TO NEXT BINSRCH TABLE ENTRY              
         B     BUCKLOOP                                                         
*                                                                               
PRTATONL GOTO1 ACREPORT                       SKIP A LINE                       
         MVC   P+16(14),=C'ACCOUNT TOTALS'                                      
         MVI   TOTSW,X'01'                                                      
         GOTO1 MOVEPRNT,DMCB,ADVBUCKS                                           
         BAS   RE,PRINTEM     GO PRINT ACCT TOTS REC                            
         B     EXIT                                                             
         SPACE 2                                                                
MVCP     MVC   0(0,R3),0(R7)                                                    
         SPACE                                                                  
MVCCA    MVC   0(0,R3),1(R7)                                                    
*                                                                               
*ROUTINE READS FORWARD ACCUMALATING AMOUNTS FOR SAME KEY ENTRIES                
ACCUMDTL NTR1                                                                   
         XC    FULL,FULL           ACCUM INDICATOR                              
         LA    R7,ADVKEY           CURRENT ENTRY                                
         LA    R3,SORTSEQ          DETERMINE LEN TO COMPARE                     
         CLC   2(2,R3),=X'000E'    IN 2ND POS MEANS JUST 1 KEY VAL              
         BNE   ACD05                                                            
         ZIC   R3,1(R3)            =LEN FOR DATE OR NUMB                        
         B     ACD05A                                                           
ACD05    LA    R3,7                =LEN FOR DATE AND NUMB                       
ACD05A   LA    R1,ADVLEQ(R7)       NEXT ENTRY                                   
ACDEX    EX    R3,ACD10                                                         
         B     ACD12                                                            
ACD10    CLC   0(0,R7),0(R1)                                                    
ACD12    BNE   ACD30                                                            
         MVC   FULL,=F'1'          INDICATE ACCUMALATION                        
         LA    R1,ADVLEKY(R1)      TO 1ST BKT                                   
         LA    R7,ADVLEKY(R7)      TO 1ST BKT                                   
         LA    R8,5                                                             
ACD20    AP    0(8,R1),0(8,R7)     ADD FIRST TO SECOND                          
         AH    R1,=H'8'            NEXT BKT                                     
         AH    R7,=H'8'            NEXT BKT                                     
         BCT   R8,ACD20            POINT TO UPDATED ENTRY                       
         B     ACD05A                                                           
*                                                                               
ACD30    OC    FULL,FULL           ANY ACCUM                                    
         BZ    NOACCUM             NO                                           
         ST    R7,FULL             RETURN WITH A(BKT TO PRINT)                  
         LTR   RB,RB               ACCUM CC                                     
         B     ACUMEX                                                           
NOACCUM  SR    R0,R0                                                            
ACUMEX   XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
*              PRINT A LINE                                                     
*                                                                               
PRINTEM  NTR1                                                                   
         CLI   RCSUBPRG,0                                                       
         BNE   HED15                                                            
         MVC   HEAD6+1(36),LEVCACC+15      ACCOUNT NAME                         
         MVC   HEAD7+1(7),=C'ACCOUNT'                                           
         MVC   HEAD7+9(12),THISACCT+3                                           
HED15    MVC   HEAD4+42(36),COMPNAME       COMPANY NAME                         
         MVC   HEAD5+42(26),COMPADDR          ADDRESS                           
         MVC   HEAD5+89(L'HEADMON),HEADMON                                      
         MVC   HEAD6+42(26),COMPADDR+26                                         
         MVC   HEAD7+42(26),COMPADDR+52                                         
         MVC   HEAD8+42(26),COMPADDR+78                                         
         MVC   HEAD7+89(22),REQNAME                                             
         MVC   HEAD10+36(L'DATLIN1),DATLIN1                                     
         MVC   HEAD11+36(L'DATLIN2),DATLIN2                                     
         CLI   RCSUBPRG,1                                                       
         BE    P10                                                              
         CLI   RCSUBPRG,2                                                       
         BE    P10                                                              
         MVC   HEAD10(37),BANNER1                                               
         MVC   HEAD11(37),BANNER2                                               
P10      L     R4,AMONACC                                                       
         USING ACMD,R4                                                          
         CLC   ACMMSTR,ACMMEND      NO TOTALS IF SHOWING ONLY 1 MONTH           
         BE    *+16                                                             
         DROP  R4                                                               
         MVC   HEAD10+99(5),=C'TOTAL'                                           
         MVC   HEAD11+99(5),=C'-----'                                           
         CLI   MODE,REQLAST        NO LEVELS PRINTED ON REQLAST                 
         BE    THRU                                                             
         CLI   LEVELS,1            OR IF ONLY 1 LEVEL                           
         BE    THRU                                                             
         CLI   MODE,LEVALAST                                                    
         BE    PRINCONT                                                         
         CLI   MODE,LEVBLAST                                                    
         BE    PRINCONT                                                         
         CLI   MODE,LEVCLAST                                                    
         BE    PRINCONT                                                         
         B     THRU                                                             
PRINCONT MVC   HEAD6+1(36),SPACES                                               
         MVC   HEAD7+1(20),SPACES                                               
         MVC   HEAD10(37),SPACES                                                
         MVC   HEAD11(37),SPACES                                                
         SR    R2,R2                                                            
         MVC   HEAD4+1(8),=C'ACCOUNT '                                          
         LA    R3,THISACCT+3                                                    
         LA    R1,HEAD4+9                                                       
         IC    R2,SAVLEVA                                                       
         BCTR  R2,0                                                             
         EX    R2,MVCLEV                                                        
         CLI   MODE,LEVALAST                                                    
         BE    THRU                                                             
         LA    R3,1(R2,R3)         BUMP R3 BY L' OF LEVEL + 1                   
         LA    R1,1(R2,R1)         DITTO FOR R1                                 
         IC    R2,SAVLEVB                                                       
         BCTR  R2,0                                                             
         EX    R2,MVCLEV                                                        
         CLI   MODE,LEVBLAST                                                    
         BE    THRU                                                             
         LA    R3,1(R2,R3)                                                      
         LA    R1,1(R2,R1)                                                      
         IC    R2,SAVLEVC                                                       
         BCTR  R2,0                                                             
         EX    R2,MVCLEV                                                        
THRU     GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         SPACE 3                                                                
MVCLEV   MVC   0(0,R1),0(R3)                                                    
         EJECT                                                                  
*                                                                               
*              ROUTINE TO MOVE BUCKETS TO PRINT LINE                            
*                                                                               
MOVEPRNT NTR1                                                                   
         L     R4,0(R1)       POINTS TO 1ST BUCKET                              
         L     R5,AMONACC                                                       
         USING ACMD,R5                                                          
         LA    R3,P+38                                                          
         LA    R6,4                                                             
EDITIT   CP    0(8,R4),=P'0'                                                    
         BE    EDITBUMP     DON'T PRINT ZEROES                                  
         EDIT  (P8,0(R4)),(11,0(R3)),2,MINUS=YES                                
EDITBUMP LA    R4,8(R4)                                                         
         LA    R3,14(R3)                                                        
         BCT   R6,EDITIT                                                        
*                                                                               
         CP    0(8,R4),=P'0'      IS ACCT BAL ZERO?                             
         BNE   M12                                                              
         CLC   ACMMSTR,ACMMEND                                                  
         BNE   M11                                                              
         LA    R3,P+38          BACK TO 1ST COLM                                
M11      MVC   8(3,R3),=C'NIL'     YES - MOVE 'NIL'                             
         B     SWOFF                                                            
M12      CLC   ACMMSTR,ACMMEND      NO TOTALS IF SHOWING ONLY 1 MONTH           
         BE    SWOFF                                                            
*                                                                               
         EDIT  (P8,0(R4)),(11,0(R3)),2,MINUS=YES                                
         CLI   SUBSW,X'01'                                                      
         BNE   CKTOT                                                            
         MVI   11(R3),C'*'                                                      
         B     SWOFF                                                            
CKTOT    CLI   TOTSW,X'01'                                                      
         BNE   EXIT                                                             
         MVC   11(2,R3),=2C'*'                                                  
SWOFF    MVI   TOTSW,X'00'                                                      
         MVI   SUBSW,X'00'                                                      
         DROP  R5                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              ADD BINSRCH ENTRIES TO TABLE                                     
*                                                                               
BINADD   NTR1                                                                   
         USING BIND,R5                                                          
         L     R5,4(R1)            BINSRCH PARAMETERS                           
         L     R3,0(R1)            A(RECORD)                                    
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R3)),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1                                                           
         BE    EXIT                NOT FOUND - ADDED                            
         L     R4,DMCB             A(RECORD FOUND)                              
         ZIC   R6,BINFRST          DISP. TO FIRST BUCKET                        
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         ZIC   R7,BINNUMB          NUMBER OF BUCKETS                            
         LTR   R7,R7                                                            
         BZ    EXIT                NO BUCKETS                                   
         TM    BINSTAT,X'80'                                                    
         BO    BINBIN              DATA IS BINARY                               
         AP    0(8,R4),0(8,R3)     ADD NEW TO OLD                               
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R7,*-14                                                          
*                                                                               
         B     EXIT                                                             
*                                                                               
BINBIN   L     RE,0(R3)                                                         
         L     RF,0(R4)                                                         
         AR    RF,RE                                                            
         ST    RF,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R7,BINBIN                                                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*          DELETE 0 BALANCE ENTRIES FROM BINSRCH TABLE                          
*                                                                               
ANALACCT NTR1                                                                   
         L     R5,AADVTAB          A(BIN TABLE)                                 
         USING BIND,R5             DSECT TO COVER BIN TABLE PARAMS              
         LA    R7,BINTABLE         A(TABLE ENTRY)                               
         USING ADVANCED,R7         DSECT TO COVER BIN TABLE ENTRIES             
         L     R0,BININ            NUMBER OF ENTRIES                            
         LTR   R0,R0               ANY                                          
         BZ    DELEXT              NO                                           
*                                                                               
         CLC   ADVKEY(13),AFLD             CHK FOR BBF REC                      
         BNE   ANALLOOP            ITS NOT                                      
         LA    R3,SVACCTBK         UPDATE ACCT TOT WITH BBF BKTS                
         LA    R4,ADVLEKY(R7)      BUMP TO FST BBF BKT                          
         LA    R2,32(R3)           SVACCT TOT BKT                               
         LA    R1,4                NUMBER OF BKTS TO TOTAL                      
BBFLOOP  AP    0(8,R3),0(8,R4)                                                  
         AP    0(8,R2),0(8,R4)     ACCUM SVACCTBK TOT                           
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R1,BBFLOOP                                                       
         S     R0,=F'1'            DECREMENT COUNT OF RECS FOR ACCT             
         LTR   R0,R0               JUST A BBF REC                               
         BZ    WRTACCT             YES, GO WRITE ACCT TOT                       
         LA    R7,ADVLEQ(R7)       IT IS BUMP TO NEXT                           
*                                                                               
ANALLOOP ST    R7,FULL             SAVE START POINT CUR SUB-ACCT                
         LA    R2,0                COUNT RECS FOR CUR SUB-ACCT                  
         ZIC   R3,SORTSEQ+1        LENGTH OF 1ST KEY VAL MINUS 1                
         A     R3,=F'1'            TRUE LEN                                     
SCANLOOP LR    R4,R7               SAVE                                         
         AR    R4,R3               BUMP R4 TO X'FF' SUB-TOT INDICATION          
         CLI   0(R4),X'FF'         IS IT THE SUB-TOT                            
         BE    AL05                                                             
         A     R2,=F'1'            ADD TO COUNT TO POSSIBLY DELETE              
         LA    R7,ADVLEQ(R7)       BUMP TO NEXT REC                             
         B     SCANLOOP                                                         
*                                                                               
AL05     A     R2,=F'1'            INCLUDE SUB-TOT IN COUNT                     
         ZAP   BUCKFLD,=P'0'       CLEAR ACCUM FLD                              
         LR    R6,R7               SAVE FOR RETURN                              
         LA    R7,ADVLEKY(R7)      A(1ST SUB-TOT BKT)                           
         LR    R4,R7               SAVE                                         
         LA    R1,4                NUMBER OF BKTS TO TOTAL                      
         LA    R8,BUCKFLD          A(TO ACCUM)                                  
CHKLOOP  AP    0(8,R8),0(8,R7)                                                  
         LA    R7,8(R7)                                                         
         BCT   R1,CHKLOOP                                                       
         CP    BUCKFLD,=P'0'       IS IT 0                                      
         BE    AL15                                                             
AL10     LA    R3,SVACCTBK         ACCUM ACCT TOT WITH NON-ZERO BALS            
         LA    R8,32(R3)           SVACCT TOT BKT                               
         LA    R1,4                                                             
CLP2     AP    0(8,R3),0(8,R4)                                                  
         AP    0(8,R8),0(8,R4)                                                  
         LA    R4,8(R4)            NEXT SUB-TOT BKT                             
         LA    R3,8(R3)            NEXT ACC-TOT BKT                             
         BCT   R1,CLP2                                                          
         SR    R0,R2               DECREMENT TOTAL REC COUNT                    
         LTR   R0,R0               ARE THERE ANY MORE                           
         BZ    WRTACCT             NO, GO WRITE ACCT SUMS                       
         LR    R7,R6                                                            
         LA    R7,ADVLEQ(R7)       TO 1ST DTL RECNEXT SUB-ACCT                  
         B     ANALLOOP                                                         
*                                                                               
AL15     CLI   PROGPROF+2,0        IF NOT ADDED-SUPPRESS                        
         BE    AL20                                                             
         CLI   PROGPROF+2,C'Y'     SUPPRESSING 0 BALS                           
         BNE   AL10                NO INCLUDE IN ACCT TOT                       
*                                                                               
AL20     BAS   RE,UPDTCNTR         UPDATE CONTRA WITH ACC 0 BAL RECS            
*                                                                               
         L     R7,FULL             A(TO DELETE RECS FROM)                       
DELETE   MVC   STORKEY(28),0(R7)    FILL KEY WITH REC KEYS TO DELETE            
         MVC   DMCB+8(16),BININ                                                 
         GOTO1 BINSRCH,DMCB,(X'80',STORKEY),BINTABLE DELETE DTL-RECS            
         MVC   BININ,DMCB+8        UPDATE REC COUNT                             
         CLI   DMCB,1              IF NOT FOUND                                 
         BNE   AL30                                                             
         DC    H'0'                DUMP                                         
AL30     S     R0,=F'1'            DECREMENT ACCT REC COUNT                     
         BCT   R2,DELETE           LOOP TO DELETE THE REST                      
         LTR   R0,R0               IS THIS THE END                              
         BNZ   ANALLOOP                                                         
         CLC   BININ,=F'0'         IF THATS IT                                  
         BE    DELEXT              EXIT                                         
*WRITE OUT THE ACCT TOT                                                         
WRTACCT  EQU   *                                                                
         DROP  R7                                                               
         DROP  R5                                                               
         LA    R5,ADVWK                                                         
         USING ADVANCED,R5                                                      
         XC    ADVKEY(28),ADVKEY                                                
         MVI   ADVKEY,X'FF'                                                     
         MVC   ADVBUCKS(40),SVACCTBK                                            
         GOTO1 BINADD,DMCB,ADVWK,AADVTAB  ADD ACCOUNT LEVEL                     
*                                                                               
DELEXT   B     EXIT                                                             
*                                                                               
*                                                                               
UPDTCNTR NTR1       UPDATE CONTRA SUMMARY RECS FOR 0 BALANCE OCCURANCE          
         L     R5,FULL  =A(1ST DETL REC TO BE DELETED THIS SUB-ACCT)            
         USING ADVANCED,R5                                                      
         S     R2,=F'1'            SUBTRACT FROM COUNT FOR SUB-TOT              
*                                                                               
*PUT CONTRA ACCT IN THISSRCE                                                    
URLOOP   LR    R1,R5                                                            
         LH    R0,SORTSEQ                                                       
         CH    R0,=H'14'             1ST POS CONTRA                             
         BE    UR10                                                             
         ZIC   R3,SORTSEQ+1        ADD DISP 1ST POS                             
         AR    R1,R3                                                            
         A     R1,=F'1'            =TRUE DISP                                   
         LH    R0,SORTSEQ+2                                                     
         CH    R0,=H'14'              2ND POS CONTRA                            
         BE    UR10                                                             
*                                                                               
UR05     ZIC   R3,SORTSEQ+3        3RD POS CONTRA                               
         AR    R1,R3                                                            
         A     R1,=F'1'                                                         
UR10     MVC   THISSRCE,0(R1)      DTL CONTRA TO HOLD FOR CONBUFF               
*                                                                               
         LA    R3,ADVBUCK1         CONVERT TO UPDATE VALS                       
         LA    R0,5                                                             
UR15     MP    0(8,R3),=P'-1'                                                   
         LA    R3,8(R3)                                                         
         BCT   R0,UR15                                                          
*                                                                               
         BAS   RE,CONBUFF          UPDATE EXSISTENT CONTRA BUFF RECS            
*                                                                               
UR17     LA    R5,ADVLEQ(R5)                                                    
         BCT   R2,URLOOP           IF ACCT REC EXIT                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
FORMDTL  NTR1       CALCULATE HEADING-PRINT DETAIL LOCS-SAVE SORT ENTRY         
         MVC   BANNER1,SPACES                                                   
         MVC   BANNER2,SPACES                                                   
         XC    SORTSEQ,SORTSEQ                                                  
         XC    CONTRASW,CONTRASW                                                
*                                                                               
         LA    R1,PROGPROF                                                      
         USING PROFDSEC,R1                                                      
         A     R1,=F'6'            POINT TO FIRST SORT VALUE                    
         LA    R2,BANNER1          A(DETAIL HEADER 1)                           
         LA    R3,BANNER2          A(DETAIL HEADER 2)                           
         LA    R8,SORTSEQ          HOLDER FOR PSRT VALUES                       
         LA    R0,3                COUNT                                        
*                                                                               
LOOPFRMT L     R6,AINITTAB         A(TABLE TO INITIAL BANNER)                   
         LA    R5,MASK        A(MASK USED TO MAINTAIN RQST INTEGRITY)           
ST00     CLI   0(R1),C'*'     IF, TAKE DEFAULT                                  
         BE    ST01                                                             
         CLI   0(R1),0        IF, TAKE DEFAULT                                  
         BNE   ILAB07                                                           
ST01     C     R0,=F'3'                                                         
         BNE   ST05                                                             
         MVI   0(R1),C'R'                                                       
         B     ILAB10                                                           
ST05     C     R0,=F'2'                                                         
         BNE   ST07                                                             
         MVI   0(R1),C'D'                                                       
         B     ILAB10                                                           
ST07     MVI   0(R1),C'C'                                                       
         B     ILAB10                                                           
*                                                                               
ILAB07   CLI   0(R1),C'N'                                                       
         BNE   ILAB10                                                           
         C     R0,=F'3'            FIRST PASS                                   
         BNE   N10                                                              
         MVI   MASK,X'FF'          NO ADV NUMB                                  
         B     ILAB10                                                           
N10      C     R0,=F'2'            SECOND PASS                                  
         BNE   N20                                                              
         MVI   MASK+1,X'FF'        NO ADV DATE                                  
         B     ILAB10                                                           
N20      MVI   MASK+2,X'FF'        NO CONTRA ACCT                               
*                                                                               
ILAB10   LA    R4,3                COUNT                                        
ILAB12   CLC   0(1,R6),0(R1)                                                    
         BE    ILAB20                                                           
         LA    R6,INITLEN(R6)      TO NEXT INITTAB ENT                          
         BCT   R4,ILAB12                                                        
         B     LOOPLAB                                                          
*                                                                               
ILAB20   LA    R4,3                CHECK MASK FOR REQUEST INTEGRITY             
ILAB22   CLC   0(1,R5),0(R1)                                                    
         BNE   ILAB24                                                           
         MVI   0(R5),X'FF'                                                      
         B     ILAB27                                                           
ILAB24   A     R5,=F'1'                                                         
         BCT   R4,ILAB22                                                        
         B     LOOPLAB                                                          
*                                                                               
ILAB27   A     R6,=F'1'            POINT TO HALF WORD OF SORTSEQ VALUE          
         MVC   0(2,R8),0(R6)                                                    
         A     R6,=F'2'            POINT TO LENGTH TO INIT BANNER               
         ZIC   R7,0(R6)                                                         
         A     R6,=F'1'            POINT TO BANNER 1 LIT                        
         EX    R7,MOVEA                                                         
         A     R6,=F'14'           POINT TO BANNER 2 LIT                        
         EX    R7,MOVEB                                                         
         A     R6,=F'14'           POINT TO NEXT TABLE ENTRY                    
         AR    R3,R7               ADVANCE A(BANNERS)                           
         AR    R2,R7                                                            
         A     R3,=F'4'            SPACE BETWEEN FIELDS                         
         A     R2,=F'4'                                                         
         A     R8,=F'2'            POINT TO NEXT HAF WORD SORT SEQ              
LOOPLAB  A     R1,=F'1'            TO NEXT PROF SORT VALUE                      
         BCT   R0,LOOPFRMT         LOOP                                         
*                                                                               
*CHECK FOR CONTRA IN SORTSEQ-INSERT IF NOT THERE-SET INDICATOR TO               
*MAKE EXISTENCE OF CONTRA TRANSPARENT IN PRINT                                  
         LA    R2,3                COUNT                                        
         LA    R1,SORTSEQ                                                       
CHKA     CLC   0(2,R1),=X'000E'    CONTRA                                       
         BE    CHKC                                                             
         CLC   0(2,R1),=X'0000'    1 OR 2 SORT SEQUENCE REQUEST                 
         BE    CHKB                                                             
         LA    R1,2(R1)            TO NEXT VALUE                                
         BCT   R2,CHKA                                                          
         S     R1,=F'2'            BACK TO LAST POSITION                        
CHKB     MVC   0(2,R1),=X'000E'    INSERT CONTRA VALUE                          
         MVC   CONTRASW,=F'1'      SET INDICATION TO NOT PRINT                  
CHKC     B     EXIT                                                             
*                                                                               
MOVEA    MVC   0(0,R2),0(R6)       BANNER1                                      
MOVEB    MVC   0(0,R3),0(R6)       BANNER2                                      
PATCH    DC    20F'0'                                                           
*                                                                               
         GETEL R2,DATADISP,ELCODE                                               
         EJECT                                                                  
*              LITERALS                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              CONSTANTS                                                        
*                                                                               
MASK     DC    CL3'RDC'                                                         
DASHES   DC    10C'-'                                                           
BUFAREA  DS    CL100                                                            
AADVTAB  DC    A(ADVTAB)                                                        
ADBUFF   DC    A(BUFFALOC)                                                      
AINITTAB DC    A(INITTAB)                                                       
ADLIST   DC    V(ACLIST)                                                        
AFLD     DC    13X'0'                                                           
         EJECT                                                                  
*                                                                               
*              BINSRCH TABLE - USED FOR DETAIL, SOURCE TOTAL,                   
*                              AND ACCOUNT TOTAL RECORDS                        
ADVTAB   DS    0D                                                               
         DC    F'0'                           NUMBER IN TABLE                   
         DC    AL4(ADVLEQ)                    RECORD LENGTH                     
         DC    AL4(ADVLEKY)                   DISP.TO KEY/KEY LENGTH            
         DC    AL4(ADVMAX)                    MAX NUMBER OF RECORDS             
         DC    AL1(ADVBKCNT)                  NUMBER OF BUCKETS                 
         DC    AL1(ADVBUCDS)                  DISP TO BUCKETS                   
         DC    AL2(0)                                                           
         DS    (ADVMAX*ADVLEQ)C               TABLE                             
ADVMAX   EQU   5000                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
* INITTAB IS USED TO INITIALIZE SORTSEQ(SORT SEQUENCE OF DATA), BANNER          
*TABLE ENTRIES: 1 BYTE PSORT RQST INPUT                                         
*               2 BYTES SORT VALUE                                              
*               1 BYTES HEADING LINE MOVE LENGTH                                
*               14 BYTES BANNER 1                                               
*               14 BYTES BANNER 2                                               
* 6 BYTES TO INDICATE SORT SEQ-RELATION TO 'THISDATA'                           
INITTAB  DC    C'R',X'0F05',X'07',CL14'ADV NUMB',CL14'--------'                 
*                                                                               
         DC    C'D',X'1502',X'07',CL14'ADV DATE',CL14'--------'                 
*                                                                               
         DC    C'C',X'000E',X'0D',CL14'CONTRA ACCOUNT'                          
         DC    CL14'--------------'                                             
*                                                                               
         DC    X'FF'                                                            
*                                                                               
*              DSECT TO COVER INITTAB ENTRIES                                   
*                                                                               
INITD    DSECT                                                                  
INITMEMB DS    CL1               MIRROR OF PSRT                                 
INITMVLN DS    CL2               SORTSEQ VALUE                                  
INITSEQ  DS    CL1               LENGTH OF BANNER MOVE USING EX INST            
INITBAN1 DS    CL14              BANNER VALUE                                   
INITBAN2 DS    CL14              BANNER VALUE                                   
INITLEN  EQU   *-INITD           TABLE ENTRY LENGTH                             
         EJECT                                                                  
TESTSW   DC    X'00'                                                            
*                                                                               
*              DSECT TO COVER SPACEND                                           
*                                                                               
AC82D    DSECT                                                                  
MINIMP   DS    D                                                                
SAVE4    DS    F                                                                
STORKEY  DS    CL28                                                             
HEADMON  DS    CL30                                                             
BUCKFLD  DS    PL8                                                              
SAVFLD   DS    PL8                                                              
COUNT    DS    PL2                                                              
SAVENAME DS    CL36                                                             
THISACCT DS    CL51                                                             
THISDATA DS    0CL24               CURRENT TRANSACTION VALUES                   
THISSRCE DS    CL15                CONTRA ACCOUNT                               
THISREF  DS    CL6                 REFERENCE (ADVANCE NUMBER)                   
THISDATE DS    CL3                 DATE                                         
THISMOS  DS    CL3                 MOS                                          
THISAMNT DS    PL6                 AMOUNT                                       
*                                                                               
DATLIN1  DS    CL61                                                             
DATLIN2  DS    CL61                                                             
REQNAME  DS    CL22                                                             
COMPNAME DS    CL36                                                             
COMPADDR DS    4CL26                                                            
DATAB    DS    CL50                                                             
TOTSW    DS    C                                                                
SUBSW    DS    X                                                                
LEVELS   DS    C                                                                
SAVEMON  DS    CL6                                                              
LEVACCS  DS    0CL51               NAMES OF LEVELS                              
LEVCACC  DS    CL51                                                             
LEVBACC  DS    CL51                                                             
LEVAACC  DS    CL51                                                             
BILLTOT  DS    PL8                                                              
ADVWK    DS    CL(ADVLEQ)                                                       
BBFSW    DS    CL1                                                              
SAVLEVA  DS    X                                                                
SAVLEVB  DS    X                                                                
SAVLEVC  DS    X                                                                
SAVLEVD  DS    X                                                                
BANNER1  DS    CL37                DETAIL HEADING LINE 1                        
BANNER2  DS    CL37                DETAIL HEADING LINE 2                        
FILL     DS    CL1                                                              
SORTSEQ  DS    CL6                 POINTERS TO THISDATA VALUES                  
SVACCTBK DS    PL8                                                              
SVACCTB2 DS    PL8                                                              
SVACCTB3 DS    PL8                                                              
SVACCTB4 DS    PL8                                                              
SVACCTB5 DS    PL8                                                              
TRNSW    DS    F                   TRANS INDICATOR FOR ACCT                     
CONTRASW DS    F                   CONTRA INDICATOR TO PRINT                    
RECNO    DS    PL4             HOLDER FOR ADVRECNO VALUE WITHIN ACCT            
SUPLOCK  DS    CL1             X'80' SUPPRESS LOCKED ACCOUNTS                   
ELCODE   DS    CL1             FOR GETEL                                        
         EJECT                                                                  
*                                                                               
*              DSECT TO COVER PROFILE                                           
*                                                                               
PROFDSEC DSECT                                                                  
PACCTSUM DS    CL1                 ACCOUNT SUMMARY (DEFAULT=Y)                  
PCASUM   DS    CL1                 CONTRA ACCOUNT SUMMARY (DEFAULT=N)           
PSUPZERO DS    CL1                 SUPPRESS 0 BALANCE ACCTS (DEFAULT=Y)         
PSUPSTOT DS    CL1                 SUPPRESS SUB-TOTALS (DEFAULT=N)              
PLEVTOTS DS    CL1                 PRINT LEVEL TOTALS (DEFAULT=Y)               
PDTLON   DS    CL1                 BREAK DTL RECS OUT TO MORE DTL               
PSRT1    DS    CL1                 SORT FIELD 1 - R,D,C OR *                    
PSRT2    DS    CL1                 SORT FIELD 2 - R,D,C OR *                    
PSRT3    DS    CL1                 SORT FIELD 3 - R,D,C OR *                    
PROFLEN  EQU   *-PROFDSEC                                                       
         EJECT                                                                  
*                                                                               
*              DSECT TO COVER BINSRCH TABLE                                     
*                                                                               
BIND     DSECT                                                                  
BININ    DS    F                              NUMBER IN TABLE                   
BINLEN   DS    F                              RECORD LENGTH                     
BINDISP  DS    CL1                            DISPLACEMENT IN RECORD            
BINKEY   DS    CL3                            KEY LENGTH                        
BINMAX   DS    F                              MAXIMUM NUMBER IN TABLE           
BINNUMB  DS    CL1                            NUMBER OF BUCKETS                 
BINFRST  DS    CL1                            DISP. TO FIRST BUCKET             
BINSTAT  DS    CL1                            X'80' BINARY DATA                 
         DS    CL1                            SPARE                             
BINTABLE DS    CL1                                                              
         EJECT                                                                  
*                                                                               
*              DSECT TO COVER BINSRCH TABLE ENTRIES                             
*                                                                               
ADVANCED DSECT                                                                  
ADVKEY   DS    CL24                           KEY                               
ADVRECNO DS    PL4        FLD TO IMPLEMENT FURTHER DETAIL                       
ADVLEKY  EQU   *-ADVANCED                                                       
ADVBUCDS EQU   *-ADVANCED                                                       
ADVBUCKS DS    0CL40                          BUCKETS                           
ADVBUCK1 DS    PL8                                                              
ADVBUCK2 DS    PL8                                                              
ADVBUCK3 DS    PL8                                                              
ADVBUCK4 DS    PL8                                                              
ADVTOTLN EQU   *-ADVANCED          LENGTH TO TOT BKT                            
ADVBUCK5 DS    PL8                                                              
ADVBKCNT EQU   (*-ADVBUCKS)/8                 NUMBER OF BUCKETS                 
ADVBKLEN EQU   (*-ADVBUCKS)                   LENGTH OF BUCKETS                 
ADVLEQ   EQU   *-ADVANCED                     LENGTH OF 1 TABLE ENTRY           
         EJECT                                                                  
*                                                                               
*              DSECT TO COVER SORTTAB ENTRIES                                   
*                                                                               
SRTD     DSECT                                                                  
SRTORDER DS    CL3               ORDER OF SORT FIELDS                           
SRTDISP1 DS    X                 DISPLACEMENT INTO THISDATA OF FIELD 1          
SRTLNTH1 DS    X                 LENGTH OF SORT FIELD 1                         
SRTDISP2 DS    X                 DISPLACEMENT INTO THISDATA OF FIELD 2          
SRTLNTH2 DS    X                 LENGTH OF SORT FIELD 2                         
SRTDISP3 DS    X                 DISPLACEMENT INTO THISDATA OF FIELD 3          
SRTLNTH3 DS    X                 LENGTH OF SORT FIELD 3                         
SRTSPROG DS    X                 SPROG FOR THIS ENTRY FOR HEADINGS              
SRTSPRO2 DS    X                   SPROG FOR PROFILE OPTION 6                   
SRTLEN   EQU   *-SRTD                                                           
*                                                                               
*              DSECT TO COVER BUFFALO RECORDS                                   
*                                                                               
BUFFD    DSECT                                                                  
BUFTYP   DS    CL1               RECORD TYPE                                    
BUFAC    EQU   1                 TYPE 1 = ACCOUNT SUMMARY RECORD                
BUFCA    EQU   2                 TYPE 2 = CONTRA/ACCT SUMMARY RECORD            
BUFLEVA  EQU   C'A'              TYPE A = LEVEL A TOTALS RECORD                 
BUFLEVB  EQU   C'B'              TYPE B = LEVEL B TOTALS RECORD                 
BUFLEVC  EQU   C'C'              TYPE C = LEVEL C TOTALS RECORD                 
BUFLEVD  EQU   C'D'              TYPE D = LEVEL D TOTALS RECORD                 
BUFACCT  DS    CL15              ACCOUNT NUMBER                                 
BUFNAME  DS    CL36              ACCOUNT NAME                                   
BUFBUCK  DS    0CL40             5 REGULAR BUCKETS                              
BUFBUCK1 DS    PL8                                                              
BUFBUCK2 DS    PL8                                                              
BUFBUCK3 DS    PL8                                                              
BUFBUCK4 DS    PL8                                                              
BUFBUCK5 DS    PL8                                                              
BUFBUCK6 DS    PL8               USED TO ALLOW RETRIEVAL OF 0 BALANCES          
         EJECT                                                                  
* ACMASTD                                                                       
* ACREPWORKD                                                                    
* ACGENMODES                                                                    
* ACGENBOTH                                                                     
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
         EJECT                                                                  
         BUFF  LINES=400,ROWS=1,COLUMNS=6,FLAVOR=PACKED,COMMENT=36,    X        
               KEYLIST=(16,A)                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044ACREP8202S05/01/02'                                      
         END                                                                    
