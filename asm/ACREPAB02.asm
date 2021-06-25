*          DATA SET ACREPAB02  AT LEVEL 007 AS OF 03/24/20                      
*PHASE ACAB02C,*                                                                
*INCLUDE AUTOAB                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE BUFFERIN                                                               
*INCLUDE BINSR31                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE ACLIST                                                                 
*INCLUDE DLFLD                                                                  
*INCLUDE ACABVNDR                                                               
         TITLE 'AUTO APPROVE BY VENDOR'                                         
**********************************************************************          
* PID  LVL DATE    COMMENTS                                          *          
* ---------------------------------                                  *          
* VGUP 005 18JUN19 SPEC-36093 FIX THE TOTALING ISSUE                 *          
* RGUP 006 01DEC19 SPEC-30974 SUPPORT DOLLAR TOLERANCE               *          
**********************************************************************          
* PROGRAM OPTIONS :                                                  *          
*                                                                    *          
*         QOPT1 - 'Y'  DISPLAY SR ACCOUNT DTLS (DOESN'T APPLY IN AB) *          
*         QOPT2 - 'Y'  EXPAND MULTIPLE TRANSACTIONS ALWAYS SET FOR AB*          
*         QOPT3 - 'Y'  DISPLAY DISK ADDRESS (MEDIAOCEAN ONLY)        *          
*         QOPT4 - 'Y'  MARK APPROVED (LIVE RUN)                      *          
*         QOPT5 - 'Y'  RUN OFF SS/SR TRANSACTIONS NOT POINTERS       *          
*         QOPT6 - 'Y'  CALC CASH AVAILABILITY BY MOS NOT BY ESTIMATE *          
*         QOPT7 - 'Y'  PRINTABLES (INTERNAL USE ONLY)                *          
*         QOPT8 - 'Y'  SUPPRESS DISBURSED DETAILS/TRANSACTIONS       *          
*         QOPT9 - 'Y'  APPROVE UP TO CASH POSITION                   *          
*--------------------------------------------------------------------*          
*         QSELECT(3)   CLIENT CODE                                   *          
*           OR                                                       *          
*         QSELECT(4)   CLIENT LIST (PRODUCT NOT ALLOWED IF LIST)     *          
*         QSELECT+3(3) PRODUCT CODE                                  *          
*         QAPPL(2)     TOLERANCE LEVEL (99 MAX)                      *          
*         QAPPL+2(6)   ESTIMATE                                      *          
*         QAPPL+8(2)   MEDIA OR +N, +S, +P FOR SYSTEM                *          
**********************************************************************          
ACAB02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACAB**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACABD,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                      INITIALIZATIONS FOR THE RUN.           
         CLI   MODE,REQFRST              REQUEST FIRST                          
         BE    REQF                                                             
         CLI   MODE,LEDGFRST             LEDGER FIRST                           
         BE    LDGF                                                             
         CLI   MODE,REQLAST              LAST FOR REQUEST                       
         BE    REQL                                                             
         CLI   MODE,RUNLAST              LAST FOR RUN                           
         BE    RUNL                                                             
         CLI   MODE,PROCRQST             SET MEDIA MOS RANGE                    
         BE    RQST                                                             
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
         XIT1                                                                   
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
*                                                                               
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'     SET WIDTH FOR REPORT                        
*                                                                               
         CLI   MCTSTRUN,X'FF'      TEST RUN=TEST                                
         BNE   *+8                                                              
         OI    RUNOPT,TESTRUN                                                   
*                                                                               
         MVC   UPSI,MCUPSI         TEST OPTIONS                                 
         CLI   MCRECOVR,C'W'       IS THIS A SOON RUN                           
         BE    RUNF05              NO                                           
         OC    MCREMPQK,MCREMPQK   IS THIS A SOON RUN?                          
         BZ    RUNF10              NO                                           
RUNF05   OI    RUNOPT,SOONRUN      YES                                          
         MVC   USID,MCUSERID       GET USER ID                                  
         MVC   PQID,MCREMPQK       PQ ID                                        
         MVC   VSSB,MCSSB          AND ADDRESS OF SSB                           
*                                                                               
RUNF10   DS    0H                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(3,TODAY3)                                
RUNFX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
*                                                                               
         L     RF,ADWNBUF             RF=A(DOWNLOAD BUFFER)                     
         XC    0(L'DWNBUF,RF),0(RF)   CLEAR DOWNLOAD BUFFER                     
         XC    DWNSTAT,DWNSTAT        CLEAR DOWNSTAT BYTE                       
*                                                                               
         USING BOXD,R3                                                          
         L     R3,ADBOX                                                         
         MVI   BOXOFF,C'Y'         TURN OFF BOXES                               
*                                                                               
         MVI   RCSUBPRG,9          SET PRINT RTE AS INVALID                     
         GOTO1 ADWNL,DMCB,(RC),DWNINIT      INITIALIZE DOWNLOAD RTE             
*                                                                               
*        CLI   QOPT2,C'Y'                                                       
*        BE    *+8                                                              
         MVI   QOPT2,C'Y'          NEEDS THIS TO BE Y FOR AB REPORT             
         MVI   FLAG,0              INITIALIZE FLAG                              
         MVI   RCSUBPRG,1                                                       
*                                                                               
         ZAP   REQPCNT,=P'1000000' DEFAULT IS FULLY MATCH                       
*                                                                               
         L     R7,APROFILE                                                      
         USING ACPROFSD,R7                                                      
*                                                                               
         CLI   QOPT10,C' '                                                      
         BNE   REQF07                                                           
         MVC   QOPT10,ACPPFC06     USE IT FROM PROFILE.                         
*                                                                               
REQF07   DS    0H                                                               
         CLI   QOPT10,C'Y'       AA PROFILE, "USE TOLERANCE" FIELD              
         BE    REQF08                                                           
         CLI   QOPT10,C'D'       AA PROFILE, "USE TOLERANCE" FIELD              
         BE    REQF08                                                           
         MVC   QTOLRNCE,SPACES                                                  
         B     REQF50    NOT ENABLED, IGNORE BOTH PROFILE AND OVERRIDE          
*                                                                               
* REQUEST CARD OVERRIDES AA PROFILE, CHECK QTOLRNCE FIRST                       
*                                                                               
REQF08   CLC   QTOLRNCE,SPACES          REQ CARD OVERRIDE EMPTY?                
         BNE   REQF10                   NO - USE THE OVERRIDE VALUE             
*                                  REQ CARD EMPTY, USE AA PROFILE               
         LLC   RF,ACPPFC07         TOLERANCE, INTEGER PART                      
         MHI   RF,100                                                           
         LLC   RE,ACPPFC08         TOLERANCE, DECIMAL PART                      
         AR    RF,RE                                                            
         STCM  RF,3,QTOLRNCE                                                    
*                                                                               
REQF10   DS    0H                                                               
         CLC   QTOLRNCE,=X'63FF'        CAN'T BE MORE THAN 255.99$              
         JH    *+2                      SOMETHING IS WRONG                      
*                                                                               
         LHI   RF,10000                 100.00%                                 
         XR    R0,R0                                                            
         ICM   R0,3,QTOLRNCE            RANGE: 0.00-10.00%                      
*                                       RANGE: 0.00-255.99$                     
         CLI   QOPT10,C'Y'              PERCENTAGE                              
         BE    REQF15                                                           
         CLI   QOPT10,C'D'              DOLLAR                                  
         BNE   REQF15                                                           
         CVD   R0,REQPCNT                                                       
         B     REQF50                                                           
*                                                                               
REQF15   SR    RF,R0                    100%-QTOLRNCE                           
         CVD   RF,REQPCNT               90.00% -> X'09000C' PACKED              
         SRP   REQPCNT(8),2,0           MAKE IT 4 DECIMAL DIGITS                
*        PACK  REQPCNT(8),QTOLRNCE(2)   REQPCNT=100C IF 100%                    
*        SRP   REQPCNT(8),4,0           MAKE IT 10000C                          
*                                                                               
REQF50   CLC   QOPT10,ACPPFC06     HAS USE TOLERANCE CHANGED?                   
         BNE   ERRINCST                                                         
*                                                                               
         DROP  R7                                                               
*                                                                               
REQF60   GOTO1 ASETMOS,DMCB,(RC)   SET START AND END MOS DATES                  
*                                                                               
REQFX    B     EXIT                                                             
*                                                                               
ERRINCST DS    0H                                                               
         MVI   RCSUBPRG,5                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
         MVC   XP+1(46),=CL46'** ERROR: USE TOLERANCE INCONSISTENT'             
         GOTO1 ACREPORT                                                         
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVI   ACMMODE,REQLAST     FORCE REQ LAST                               
         B     REQFX                                                            
         EJECT                                                                  
**********************************************************************          
* PROCRQST                                                                      
**********************************************************************          
RQST     DS    0H                                                               
*                                                                               
         BRAS  RE,SETPROF             READ AA PROFILES FOR AB REPORT            
*                                                                               
         L     R3,ADQSTACK                                                      
         USING ACQD,R3                                                          
         MVC   WORK(4),ACQDTEND                                                 
         MVC   WORK+4(2),=C'01'                                                 
*                                                                               
         L     RE,APROFILE                                                      
         USING ACPROFSD,RE         PROFILES PASSED BY MONACC                    
         LLC   R0,ACPPFC04                                                      
         DROP  RE                                                               
         CHI   R0,0                VALUE NOT SET?                               
         BNE   *+8                                                              
         LHI   R0,11               SET TO 11 (DEFAULT)                          
         LNR   R0,R0                                                            
*                                                                               
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,(R0)                               
         MVC   ACQDTSTR(4),WORK+6                                               
*                                                                               
RQSTX    B     EXIT                                                             
         DROP  R3                                                               
**********************************************************************          
* LEDGER FIRST                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING LDGRECD,R5                                                       
LDGF     DS    0H                                                               
         L     RE,APROFILE                                                      
         USING ACPROFSD,RE                                                      
         MVC   PROFILES,ACPPFCP1   PROFILES PASSED BY MONACC                    
         DROP  RE                                                               
*                                                                               
         USING CPYELD,R1                                                        
         L     R1,ADCMPEL          GET X'10' ELEMENT COMPANY ELEM               
         MVC   SVCLOGO,CPYLOGO     GET LOGIN ID / CONNECT ID                    
         DROP  R1                                                               
*                                                                               
         L     R5,ADLEDGER         GET LEDGER RECORD                            
         L     RF,ALDSYTAB         CHECK TABLE OF LEDGER/SYSTEM B/C             
LDGF05   CLI   0(RF),X'FF'         ONLY WANT CERTAIN LEDGERS                    
         BE    LDGFX               DON'T LOCK THIS LEDGER?                      
         CLC   LDGKLDG,0(RF)                                                    
         BE    LDGF08                                                           
         LA    RF,2(RF)                                                         
         B     LDGF05                                                           
*                                                                               
LDGF08   DS    0H                                                               
*                                                                               
         TM    RUNOPT,SOONRUN                                                   
         BNO   LDGFX                                                            
*                                                                               
         CLI   QOPT4,C'Y'          IS THIS A LIVE RUN?                          
         BNE   LDGFX                                                            
LDGF10   L     R5,ADLEDGER         GET LEDGER RECORD                            
         MVI   ELCODE,LGLELQ       LEDGER LOCK ELEMENT X'EA'                    
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING LGLELD,R5                                                        
         TM    RUNOPT,TESTRUN      DO WE HAVE RUN=TEST IN JCL                   
         BO    LDGF20                                                           
         TM    LGLSTAT,LGLSLOCK    TEST LEDGER IS LOCKED                        
         BO    *+6                                                              
         DC    H'0'                LEDGER MUST BE LOCKED FOR SOON               
         CLC   LGLDATE,TODAY3      MAKE SURE IT WAS LOCKED TODAY                
         BE    *+6                                                              
         DC    H'0'                                                             
LDGF20   MVC   LUID,LGLLUID        SAVE LUID                                    
*                                                                               
LDGFX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         GOTOR INITABLK            INITIALIZE AUTO APPROVE BLOCK                
         JNE   EXIT                                                             
         GOTO1 AUTOAB,AUTBLK       BUILD PAYABLE AND APPLY CASH                 
*                                                                               
         L     R7,APROFILE                                                      
         USING ACPROFSD,R7                                                      
         CLC   QOPT10,ACPPFC06     PROFILE CHANGED AFTER REQ?                   
         JE    REQL10              ALL GOOD THEN CONTINUE...                    
         ICM   R3,15,AUTPBUFF      GET BUFFERIN CONTROL BLOCK ADDRESS           
         USING BUFFD,R3            R3=A(BUFFERIN CONTROL BLOCK)                 
         GOTO1 ABFRIN,DMCB,(RC),BUFFAINI  INIT BUFFER                           
         DROP  R3,R7                                                            
*                                                                               
REQL10   BAS   RE,PRNL             PRINT REPORT                                 
*        BAS   RE,PRNBUFF          PRINT BUFFERS AND IOS ETC                    
         BAS   RE,MARKIT           MARK ITEMS APPROVED                          
*        BAS   RE,PRTOTAL          PRINT TOTALS PAGE                            
REQLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* RUN LAST                                                           *          
**********************************************************************          
         SPACE 1                                                                
RUNL     DS    0H                                                               
         TM    RUNOPT,SOONRUN      ARE WE RUNNING SOON?                         
         BZ    RUNL10              NO                                           
         CLI   QOPT4,C'Y'          IS THIS A LIVE RUN?                          
         BNE   RUNL10                                                           
*                                                                               
         L     R5,ADLEDGER         GET LEDGER RECORD                            
         MVI   ELCODE,LGLELQ       LEDGER LOCK ELEMENT X'EA'                    
         BAS   RE,GETEL2                                                        
         BNE   RUNL05                                                           
         USING LGLELD,R5                                                        
         XC    LGLDATE,LGLDATE     UNLOCK THE LEDGER                            
         NI    LGLSTAT,X'FF'-LGLSLOCK                                           
         MVI   MODE,WRITLEDG       WRITE THE LEDGER BACK                        
*                                                                               
RUNL05   L     RF,AMONACC                                                       
         TM    UPSI,KPFACWK        X'80'                                        
         BNO   *+8                                                              
         OI    ACMINDS7-ACMD(RF),ACMIKPFW  KEEP FACWK FILE                      
         BRAS  RE,SETFWRK                                                       
RUNL10   DS    0H                                                               
RUNLX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT LAST                                                         *          
**********************************************************************          
         SPACE 1                                                                
PRNL     NTR1                                                                   
*                                                                               
         USING PLINED,R5                                                        
         BRAS  RE,CLEAROUT         CLEAR OUTPUT BUFFER                          
*                                                                               
         LA    R0,4                                                             
         LA    R1,CLITBKT                                                       
         BRAS  RE,ZAPTOTBK         CLEAR SYS/MED/CLI/PRD TOT BUCKETS            
         LA    R1,CLITBKCT*CLITBKLN(R1) BUMP TO NEXT TOTALS ENTRY               
         BCT   R0,*-8                                                           
*                                                                               
         XC    LASTS(LASTL),LASTS  INITIALIZE LAST TIME VALUES                  
         NI    FLAG,X'FF'-FLGPRNT                                               
         L     R5,AOUTBUF          ADDRESS OF THE OUTPUT BUFFER                 
*                                                                               
         XC    PSBAWRK,PSBAWRK     INITIALIZE PAYABLE ENTRY                     
         XC    PSSAWRK,PSSAWRK                                                  
         GOTO1 ABFRIN,DMCB,(RC),BUFFARDH                                        
         TM    BCTSERRS,BUFFEEOF            IS THIS END OF FILE                 
         BO    PRNLX                                                            
*                                                                               
         USING PSSAD,R2                                                         
         LA    R2,PSSAWRK                                                       
*                                                                               
PRNL10   DS    0H                                                               
         MVC   SVPSSWRK,PSSAWRK    SAVE OFF CURRENT RECORD                      
*                                                                               
         CLI   PSSKTYP,X'00'       IS THIS A GOOD ESTIMATE?                     
         BE    PRNL50              YES PROCESS                                  
         CLI   PSSKTYP,AAVPNEST    NO ESTIMATE RECORD?                          
         BNE   PRNL30              NO IT IS A BAD ESTIMATE RECORD               
         CLI   RCSUBPRG,2                                                       
         BE    PRNL50                                                           
         BRAS  RE,PRINT06          PRINT PREVIOUS 06S IF ANY                    
         BRAS  RE,PSMCPTOT          PRINT SYS/MED/CLI/PROD TOTALS               
         MVI   RCSUBPRG,2                                                       
         B     PRNL40                                                           
PRNL30   CLI   RCSUBPRG,3          BAD ESTIMATE                                 
         BE    PRNL50                                                           
         MVI   RCSUBPRG,3                                                       
PRNL40   MVI   FORCEHED,C'Y'       INITIALIZE FOR FIRST TIME PRINTING           
         BRAS  RE,CLEAROUT         CLEAR OUTPUT BUFFER                          
         XC    LASTS(LASTL),LASTS  INITIALIZE LAST TIME VALUES                  
         NI    FLAG,X'FF'-FLGPRNT                                               
*                                                                               
PRNL50   CLI   PSSKRTYP,X'00'      ARE WE AT A NEW ENTRY                        
         BNE   PRNL55                                                           
         XC    SVVENDOR,SVVENDOR   RESET ALL VALUES UP TO VENDOR                
         NI    FLAG,X'FF'-FLGPRN03 DONT WANT TO PRNT FULLY DISB 03              
         BRAS  RE,PRINT06          PRINT PREVIOUS 06S IF ANY                    
*                                                                               
PRNL55   CLI   PSSKRTYP,X'03'      RESET IF YOU GET FUTURE OR DETL RECD         
         BNH   *+8                                                              
         NI    FLAG,X'FF'-FLGPRN03 DONT WANT TO PRNT FULLY DISB 03              
         TM    FLAG,FLGPRN03       WANT TO PRNT FULLY DISB MOS TOT LIN?         
         BO    PRNL60              YES, DON'T CHECK IF FULLY DISBURSED          
         TM    PSSFLAG,PSSDISB     IS THIS DISBURSED ONLY ITEM                  
         BNO   PRNL60              DON'T PRINT IT                               
         GOTO1 ABFRIN,DMCB,(RC),BUFFASEQ       GET NXT RECORD                   
         TM    BCTSERRS,BUFFEEOF                                                
         BNO   PRNL10                                                           
         BRAS  RE,PRINT06          PRINT PREVIOUS 06S IF ANY                    
         BRAS  RE,PSMCPTOT          PRINT SYS/MED/CLI/PROD TOTALS               
         B     PRNL500                                                          
*                                                                               
*                        *****SKIP PRINTING TOTALS IF NO DTLS*****              
*                                                                               
PRNL60   CLI   PSSKRTYP,X'00'      ARE WE AT A NEW ENTRY, TOTAL RECD            
         BNE   PRNL120                                                          
         OI    FLAG,FLGPRN03       PRINT 03 EVEN IF FULLY DISBURSED             
         B     PRNL80                                                           
PRNL70   CLI   PSSKRTYP,X'00'                                                   
         BE    PRNL10              TOTL RECD BEFORE DETAILS FOR PREV            
         CLI   PSSKRTYP,X'05'      DO WE HAVE DETAILS FOR THIS SET              
         BE    PRNL100             DETAILS FOUND, GO AND PRINT THIS SET         
PRNL80   DS    0H                                                               
         GOTO1 ABFRIN,DMCB,(RC),BUFFASEQ       GET NXT RECORD                   
         TM    BCTSERRS,BUFFEEOF                                                
         BO    PRNL500                                                          
         B     PRNL70              LAST RECORD OF THE TABLE.                    
*                        *****SKIP PRINTING TOTALS IF NO DTLS*****              
*                                                                               
PRNL100  DS    0H                                                               
         MVC   PSSAWRK,SVPSSWRK                                                 
         GOTO1 ABFRIN,DMCB,(RC),BUFFARDH                                        
         CLI   BCTSERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRNL120  DS    0H                                                               
         TM    FLAG,FLGPRNT        FIRST TIME PRINTING SOMETHING?               
         BZ    PRNL180             YES                                          
*                                                                               
*        CLI   PSSKRTYP,X'00'      ARE WE AT A NEW ENTRY, TOTAL RECD            
*        BNE   PRNL180                                                          
*                                                                               
         CLC   LSTSYS,PSSKSYS      LAST SYSTEM SAME AS CURRENT ?                
         BE    PRNL140                                                          
         BAS   RE,PSMCPTOT        PRINT SYS/MED/CLI/PROD TOTALS                 
         B     PRNL180             CHECK IF LAST RECORD AND EXIT                
*                                                                               
PRNL140  DS    0H                                                               
         MVC   PSYS,LSTSYS                                                      
         MVC   PMED,LSTMED                                                      
         MVC   PCLI,LSTCLI                                                      
         MVC   PPRO,LSTPRO         MOVE PRODUCT CODE TO PRINT                   
*                                                                               
         CLC   LSTMED,PSSKMED                                                   
         BE    PRNL150                                                          
*                                                                               
*        MVC   PPRO,LSTPRO         MOVE PRODUCT CODE TO PRINT                   
         BRAS  RE,PRINT06          PRINT PREVIOUS 06S IF ANY                    
         LA    R1,PROTBKT                                                       
         BRAS  RE,CKTOTBK0         CHECK TOTAL BUCKETS FOR PRDCT                
         TM    FLAG2,FLGTOTZR                                                   
         BO    PRNL142                                                          
         MVC   PMOS,SPACES         CLEAR PRODUCT PRINT FIELD                    
         MVC   PEST,=C'TOTALS'     MOVE TOT IN NEXT FIELD                       
         LA    R1,PROTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR PRDCT                
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
         MVC   PPRO,SPACES         CLEAR PRODUCT PRINT FIELD                    
*                                                                               
*        MVC   PCLI,LSTCLI                                                      
PRNL142  LA    R1,CLITBKT                                                       
         BRAS  RE,CKTOTBK0         CHECK TOTAL BUCKETS FOR CLIENT               
         TM    FLAG2,FLGTOTZR                                                   
         BO    PRNL144                                                          
         MVC   PMOS,SPACES         CLEAR PRODUCT PRINT FIELD                    
         MVC   PEST,=C'TOTALS'                                                  
         LA    R1,CLITBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR CLIENT               
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
         MVC   PCLI,SPACES         CLEAR CLIENT  PRINT FIELD                    
*                                                                               
*        MVC   PMED,LSTMED                                                      
PRNL144  LA    R1,MEDTBKT                                                       
         BRAS  RE,CKTOTBK0         CHECK TOTAL BUCKETS FOR CLIENT               
         TM    FLAG2,FLGTOTZR                                                   
         BO    PRNL180                                                          
         MVC   PMOS,SPACES         CLEAR PRODUCT PRINT FIELD                    
         MVC   PEST,=C'TOTALS'                                                  
         LA    R1,MEDTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR CLIENT               
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
         MVC   PMED,SPACES                                                      
         B     PRNL180                                                          
*                                                                               
PRNL150  CLC   LSTCLI,PSSKCLI                                                   
         BE    PRNL160                                                          
*                                                                               
*        MVC   PPRO,LSTPRO         MOVE PRODUCT CODE TO PRINT                   
         BRAS  RE,PRINT06          PRINT PREVIOUS 06S IF ANY                    
         LA    R1,PROTBKT                                                       
         BRAS  RE,CKTOTBK0         CHECK TOTAL BUCKETS FOR PRDCT                
         TM    FLAG2,FLGTOTZR                                                   
         BO    PRNL152                                                          
         MVC   PMOS,SPACES         CLEAR PRODUCT PRINT FIELD                    
         MVC   PEST,=C'TOTALS'     MOVE TOT IN NEXT FIELD                       
         LA    R1,PROTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR PRDCT                
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
         MVC   PPRO,SPACES         MOVE PRODUCT CODE TO PRINT                   
*                                                                               
*        MVC   PCLI,LSTCLI                                                      
PRNL152  LA    R1,CLITBKT                                                       
         BRAS  RE,CKTOTBK0         CHECK TOTAL BUCKETS FOR CLIENT               
         TM    FLAG2,FLGTOTZR                                                   
         BO    PRNL180                                                          
         MVC   PMOS,SPACES         CLEAR PRODUCT PRINT FIELD                    
         MVC   PEST,=C'TOTALS'                                                  
         LA    R1,CLITBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR CLIENT               
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
         MVC   PCLI,SPACES                                                      
         B     PRNL180                                                          
*                                                                               
PRNL160  CLC   LSTPRO,PSSKPRO      LAST PRODUCT SAME AS CURRENT                 
         BE    PRNL180                                                          
*                                                                               
*        MVC   PPRO,LSTPRO         MOVE PRODUCT CODE TO PRINT                   
         BRAS  RE,PRINT06          PRINT PREVIOUS 06S IF ANY                    
         LA    R1,PROTBKT                                                       
         BRAS  RE,CKTOTBK0         CHECK TOTAL BUCKETS FOR PRDCT                
         TM    FLAG2,FLGTOTZR                                                   
         BO    PRNL180                                                          
         MVC   PMOS,SPACES         CLEAR PRODUCT PRINT FIELD                    
         MVC   PEST,=C'TOTALS'     MOVE TOT IN NEXT FIELD                       
         LA    R1,PROTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR PRDCT                
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
         MVC   PPRO,SPACES         MOVE PRODUCT CODE TO PRINT                   
*                                                                               
PRNL180  DS    0H                                                               
         CLI   PSSKRTYP,X'00'      ARE WE AT A NEW ENTRY, TOTAL RECD            
         BE    PRNL190                                                          
*                                                                               
         MVC   PSYS,PSSKSYS        SYSTEM                                       
         MVC   PMED,PSSKMED        MEDIA                                        
         MVC   PCLI,PSSKCLI        CLIENT                                       
         MVC   PPRO,PSSKPRO        PRODUCT                                      
         MVC   PEST,PSSKEST                                                     
*                                                                               
         CLC   LSTSYS,PSSKSYS      IS LAST SYSTEM=CURRENT                       
         BNE   PRNL190             YES DON'T PRINT AGAIN                        
         CLC   LSTMED,PSSKMED      IS LAST MEDIA=CURRENT                        
         BNE   PRNL200             YES DON'T PRINT AGAIN                        
         CLC   LSTCLI,PSSKCLI                                                   
         BNE   PRNL210                                                          
         CLC   LSTPRO,PSSKPRO                                                   
         BNE   PRNL220                                                          
         CLC   LSTEST,PSSKEST                                                   
         BNE   PRNL230                                                          
         CLC   LSTMOS,PSSKMOS                                                   
         BNE   PRNL240                                                          
         B     PRNL270                                                          
*                                                                               
PRNL190  DS    0H                                                               
         LA    R0,4                                                             
         LA    R1,CLITBKT                                                       
         BRAS  RE,ADDTOTBK         ADD TO SYS/MED/CLI/PRD TOT BUCKETS           
         LA    R1,CLITBKCT*CLITBKLN(R1) BUMP TO NEXT TOTALS ENTRY               
         BCT   R0,*-8                                                           
*                                                                               
         MVC   PSYS,PSSKSYS        SYSTEM                                       
         MVC   LSTSYS,PSSKSYS                                                   
PRNL200  DS    0H                                                               
         CLI   PSSKSYS,C'N'        IS THIS NET SYSTEM ?                         
         BNE   *+12                                                             
         CLI   AUTPRF2,C'S'        RUN BY SYSTEM OR SYS/MEDIA ?                 
         BE    *+10                                                             
         MVC   PMED,PSSKMED        MEDIA                                        
*                                                                               
         MVC   LSTMED,PSSKMED                                                   
PRNL210  DS    0H                                                               
         MVC   PCLI,PSSKCLI        CLIENT                                       
         MVC   LSTCLI,PSSKCLI                                                   
PRNL220  DS    0H                                                               
         MVC   PPRO,PSSKPRO        PRODUCT                                      
         MVC   LSTPRO,PSSKPRO                                                   
PRNL230  DS    0H                                                               
         MVC   PEST,PSSKEST                                                     
         MVC   LSTEST,PSSKEST                                                   
*                                                                               
PRNL240  DS    0H                                                               
         CLI   PSSKRTYP,X'00'                                                   
         BNE   *+14                                                             
         MVC   PMOS,=C'TOTAL '                                                  
         B     PRNL250                                                          
         CLI   PSSKRTYP,X'01'                                                   
         BNE   *+14                                                             
         MVC   PMOS,=C'PRIOR '                                                  
         B     PRNL250                                                          
         CLI   PSSKRTYP,X'04'                                                   
         BNE   *+14                                                             
         MVC   PMOS,=C'FUTURE'                                                  
         B     PRNL250                                                          
         CLI   PSSKRTYP,X'02'     SR DETAILS                                    
         BNE   PRNL260                                                          
         MVC   PMOS,=C'N/A   '    ONLY IF PRINTING SR DETAILS                   
PRNL250  MVI   LSTMOS,X'FF'       NOT A VALID DATE                              
         B     PRNL270                                                          
*                                                                               
PRNL260  MVC   WORK(2),PSSKMOS   YM                                             
         MVI   WORK+2,X'01'      DAY                                            
         GOTO1 DATCON,DMCB,(1,WORK),(6,PMOS)                                    
         MVC   LSTMOS,PSSKMOS                                                   
         CLI   PSSKRTYP,X'05'     DATE CHANGED PRINT STATION & INV              
         BE    PRNL490                                                          
         CLI   PSSKRTYP,X'06'     DATE CHANGED PRINT STATION & INV              
         BE    PRNL272                                                          
         B     PRNL280            AND READ NEXT                                 
*                                                                               
PRNL270  MVC   PSTA,SPACES                                                      
         MVC   PINV,SPACES                                                      
         CLI   PSSKRTYP,X'05'                                                   
         BE    PRNL490                                                          
         CLI   PSSKRTYP,X'06'                                                   
         BNE   PRNL280                                                          
PRNL272  BRAS  RE,BLDPSBTB        BUILD TYPE 06 BINTABLE                        
         B     PRNL490                                                          
*                                                                               
PRNL280  XC    LSTSTN,LSTSTN      CLEAR STATION FROM LAST CLI/PRO/EST           
         B     PRNL320                                                          
*                                                                               
*PRNL290  DS    0H                                                              
*         CLI   AUTPRF3,C'Y'                                                    
*         BNE   *+12                                                            
*         CLI   PSSKSTAT,PSSKTOT                                                
*         BE    PRNL490                                                         
*                                                                               
*PRNL300  NI    FLAG1,X'FF'-FLGSTN THIS STATION HAS NOT BEEN PRINTD YET         
*         OC    PSBAWRK,PSBAWRK    DO WE HAVE PREVIOUSLY SAVED 05?              
*         BZ    *+8                                                             
*         BRAS  RE,PRINT06         PRINT PREVIOUSLY SAVED PAYABLE 05            
*         MVC   PSBAWRK,PSSAWRK    SAVE OFF 05 PAYBLE TO PRINT AFTER SR         
*         B     PRNL490                                                         
*                                                                               
PRNL320  DS    0H                 PRINT CLEARED,BILLED,APPLIED BUCKETS          
         LA    R4,PSSABKT         POINT TO START OF BUCKETS                     
         LA    R6,PBUCKET         POINT WHERE TO START PRINTING                 
         LHI   R1,PSSBKCT2        NUMBER OF BUCKETS TO LOOP TO PRINT            
         CLI   PSSKRTYP,X'05'     R V PRINTING SS DETAIL LINE BUCKETS           
*        BE    PRNL370            YES DO REGULAR PRINTING                       
         JE    *+2                                                              
*                                                                               
         OI    FLAG,FLGHIGH       PRINTING TOTAL BUCKETS                        
*                                                                               
         TM    PSSFLAG,PSSBZERO                                                 
         BNO   PRNL330             DON'T PRINT ZERO'S                           
         EDIT  (P8,PSSBILLB),(PBUCLEN,PBILLED),2,MINUS=YES,            +        
               COMMAS=YES                                                       
         B     PRNL340                                                          
PRNL330  EDIT  (P8,PSSBILLB),(PBUCLEN,PBILLED),2,MINUS=YES,ZERO=BLANK, +        
               COMMAS=YES                                                       
*                                                                               
PRNL340  TM    PSSFLAG,PSSRZERO                                                 
         BNO   PRNL350             DON'T PRINT ZERO'S                           
         EDIT  (P8,PSSRCVDB),(PBUCLEN,PRCVD),2,MINUS=YES,              +        
               COMMAS=YES                                                       
         B     PRNL360             DON'T PRINT ZERO'S                           
PRNL350  EDIT  (P8,PSSRCVDB),(PBUCLEN,PRCVD),2,MINUS=YES,ZERO=BLANK,   +        
               COMMAS=YES                                                       
*                                  PRINT REST OF THE BUCKETS                    
PRNL360  LA    R4,PSSPBKT          POINT TO START OF BUCKETS                    
         LA    R6,PBUCKET2         POINT WHERE TO START PRINTING                
         LHI   R1,PSSPBKCT         NUMBER OF BUCKETS TO LOOP                    
PRNL370  DS    0H                                                               
         TM    FLAG,FLGHIGH        ARE WE PRINTING TOTAL BUCKETS                
         BO    PRNL380                                                          
         EDIT  (P8,(R4)),(PBUCLEN-1,(R6)),2,ZERO=BLANK,MINUS=YES,      +        
               COMMAS=YES                                                       
         B     PRNL390                                                          
PRNL380  EDIT  (P8,(R4)),(PBUCLEN,(R6)),2,ZERO=BLANK,MINUS=YES,        +        
               COMMAS=YES                                                       
PRNL390  LA    R4,PSSABKLN(R4)                                                  
*                                                                               
         LA    RE,PUNDISB                                                       
         CR    R6,RE                                                            
         BNE   *+8                                                              
         LA    R6,L'PMARKA(R6)     BUMP TO PRINT 'A' IN APPROVED ITEMS          
*                                                                               
         LA    R6,L'PBUCKET(R6)                                                 
         BCT   R1,PRNL370                                                       
*                                                                               
         NI    FLAG,X'FF'-FLGHIGH   RESET TOTAL BUCKET PRINTING                 
*                                                                               
         CLI   PSSKRTYP,X'02'     SKIP CHECKS FOR SR ENTRIES                    
         BE    PRNL480                                                          
         CLI   PSSKRTYP,X'01'     SKIP PRIORS                                   
         BE    PRNL480                                                          
         CLI   PSSKRTYP,X'04'     SKIP FUTURES                                  
         BE    PRNL480                                                          
         CLI   PSSKRTYP,X'05'     ALWAYS PRINT SS DETAILS                       
*        BE    PRNL480                                                          
         JE    *+2                                                              
*                                                                               
         CLI   QOPT6,C'Y'         DO WE WANT TOTALS BY MOS                      
         BNE   PRNL400                                                          
         CLI   PSSKRTYP,X'00'                                                   
         BE    PRNL480            DOING CALC BY MOS SKIP GRAND TOTS             
         B     PRNL410            PRINT STATUS OF X'03' TOT BY MOS              
*                                                                               
PRNL400  CLI   PSSKRTYP,X'03'     SKIP TOTALS BY MOS                            
         BE    PRNL480                                                          
*                                                                               
PRNL410  TM    PSSFLAG,PSSCSH     IS THERE CASH AVAILABLE                       
         BO    PRNL480                                                          
         TM    PSSFLAG,PSSMAN     MANUAL APPRVL NEEDED NOT ENOUGH CASH          
         BO    PRNL480            PRINT MANUAL ON X'04' ONLY                    
         MVC   PMARK,=C'******'                                                 
         B     PRNL480                                                          
PRNL480  BRAS  RE,PRINTIT                                                       
         OI    FLAG,FLGPRNT                                                     
         BRAS  RE,CLEAROUT         CLEAR OUTPUT BUFFER                          
*                                                                               
PRNL490  GOTO1 ABFRIN,DMCB,(RC),BUFFASEQ       GET NXT RECORD                   
         TM    BCTSERRS,BUFFEEOF                                                
         BNO   PRNL10              CHECK FOR EOF                                
         BRAS  RE,PRINT06          PRINT LAST SET OF 06S IF ANY                 
         BRAS  RE,PSMCPTOT         PRINT SYS/MED/CLI/PROD TOT AND EXIT          
*                                                                               
PRNL500  DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         GOTO1 ADWNL,DMCB,(RC),DWNEOR     DOWNLOAD EOR MARKER                   
*                                                                               
PRNLX    B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
**********************************************************************          
* PRINT BUFFERS IOS ETC                                              *          
**********************************************************************          
         SPACE 1                                                                
PRNBUFF  NTR1                                                                   
         MVC   LASTS(LASTL),XSPACES   CLEAR LAST TIME VALUES                    
         TM    UPSI,DSPNIOS        DO WE WANT TO PRINT NO. OF IOS               
         BNO   PRNBX                                                            
*                                                                               
         USING BOXD,R3                                                          
         L     R3,ADBOX                                                         
         MVI   BOXOFF,C'Y'         TURN OFF BOXES                               
*                                                                               
         MVI   RCSUBPRG,8          SOME INVALID SUBPRG                          
         MVI   FORCEHED,C'Y'                                                    
         BRAS  RE,CLEAROUT         CLEAR OUTPUT BUFFER                          
         BRAS  RE,PRINTIT                                                       
         ICM   R3,15,AUTPBUFF                                                   
         USING BUFFD,R3            R3=A(BUFFERIN CONTROL BLOCK)                 
         MVC   XP+5(7),=C'PAYBUFF'                                              
         BRAS  RE,PRINTIT                                                       
         MVC   XP+5(30),=C'BUFFNIOS NO. OF IOS EXECUTED= '                      
         EDIT  BUFFNIOS,(6,XP+50),ZERO=NOBLANK                                  
         BRAS  RE,PRINTIT                                                       
         MVC   XP+5(30),=C'BUFFNREC NO. OF RECS IN BUFF= '                      
         EDIT  BUFFNREC,(10,XP+50),ZERO=NOBLANK                                 
         BRAS  RE,PRINTIT                                                       
*                                                                               
         USING BOXD,R3                                                          
         L     R3,ADBOX                                                         
         MVI   BOXOFF,C'N'         TURN ON BOXES FOR NEXT REQUEST               
*                                                                               
PRNBX    B     EXIT                                                             
         DROP  R3                                                               
**********************************************************************          
* MARK ITEMS APPROVED READ TABLE READ DISK ADDRESS AND MARK APPROVE  *          
**********************************************************************          
         SPACE 1                                                                
MARKIT   NTR1                                                                   
*                                                                               
         ZAP   PTOTAPP,=P'0'                                                    
         ZAP   PTOTPRV,=P'0'                                                    
         XC    PSSAWRK,PSSAWRK                                                  
         GOTO1 ABFRIN,DMCB,(RC),BUFFARDH                                        
         TM    BCTSERRS,BUFFEEOF            IS THIS END OF FILE                 
         BO    MRKITX                                                           
         USING PSSAD,R2                                                         
         LA    R2,PSSAWRK                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAYP) TODAY'S DATE                   
*                                                                               
         TIME  DEC                                                              
         SRL   R0,8                SHIFT OUT TENTHS & HUNDREDTHS                
         SLL   R0,4                MAKE ROOM FOR SIGN                           
         XC    DUB,DUB                                                          
         STCM  R0,15,DUB+4                                                      
         OI    DUB+7,X'0F'                                                      
         AP    DUB,=P'60000'       BUMP UP HOURS FROM DDS TO ACTUAL             
         ICM   R0,15,DUB+4                                                      
         SRL   R0,4                SHIFT OUT SIGN                               
         STCM  R0,7,CURTIME        SAVE OFF CURRENT TIME                        
*                                                                               
MRKIT10  DS    0H                                                               
         CLI   PSSKTYP,X'00'       IS THIS A GOOD ESTIMATE                      
         BNE   MRKITX              BAD ESTIMATES COMING UP, EXIT                
         CLI   PSSKSTAT,PSSKPREV   ARE THESE PREVIOUSLY APPROVED ITEMS?         
         BNE   *+10                NO                                           
         AP    PTOTPRV,PSSAPPRB    SUM UP PREVIOUSLY APPROVED AMOUNT            
         TM    PSSFLAG,PSSCSH                                                   
         BNO   MRKIT70                                                          
*                                                                               
         CLI   PSSKRTYP,X'05'      ARE THESE SS DETAILS                         
         BNE   MRKIT70                                                          
         CLC   PSSSRAC,SPACES      IS THIS SR LEVEL 05S                         
         BH    MRKIT70             SKIP AND READ NEXT                           
         CLC   PSSKSTN,SPACES      IS THIS VENDOR LEVEL 05S                     
         BNH   MRKIT70             SKIP IT                                      
*                                                                               
         OC    PSSKDA,PSSKDA       NO DISK ADDRS MEANS NOT A VALID 05           
         BZ    MRKIT70             FOR APPROVAL                                 
*                                                                               
         CLI   PSSKSTAT,PSSKUNAP   ONLY THOSE THAT WE ARE AUTOAPPROVING         
         BNE   MRKIT70                                                          
         AP    PTOTAPP,PSSUNAPB    SUM UP AMOUNT THAT IS BEING APPROVED         
*                                                                               
         XC    DSKADDR,DSKADDR                                                  
         MVC   DSKADDR,PSSKDA                                                   
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC),DSKADDR                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO                                                            
         USING TRNELD,R5                                                        
         LR    R5,R4                                                            
         AH    R5,=Y(TRNRFST-TRNKEY)                                            
         MVC   DISP2,=Y(TRNRFST-TRNKEY)                                         
         CLI   0(R5),TRNELQ        IS IT X'44' ELEMENT                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MSG,=CL10'DISKADDR  '                                            
         GOTO1 ADUMP,DMCB,(RC),DSKADDR,L'DSKADDR                                
*                                                                               
*        MVC   MSG,=CL10'GETREC    '                                            
*        GOTO1 ADUMP,DMCB,(RC),(R4),L'IO                                        
*                                                                               
         OI    TRNSTAT,TRNSAPPR    MARK THEM APPROVED                           
*                                                                               
*        SR    RE,RE                                                            
*        IC    RE,TRNLN                                                         
*        MVC   MSG,=CL10'44 AFTER '                                             
*        GOTO1 ADUMP,DMCB,(RC),(R5),(RE)                                        
*                                                                               
*                                                                               
         USING GDAELD,R5                                                        
         LR    R5,R4                                                            
         MVI   ELCODE,GDAELQ       X'E5'                                        
         BAS   RE,GETEL                                                         
         BE    MRKIT50                                                          
         B     MRKIT40                                                          
MRKIT30  BAS   RE,NEXTEL                                                        
         BE    MRKIT50                                                          
MRKIT40  BAS   RE,BLDE5            BUILD DATE AND TIME ELEMENT                  
         B     MRKIT60                                                          
*                                                                               
MRKIT50  DS    0H                                                               
         CLI   GDATYPE,GDAAPP      IS THIS APPROVAL DATE TYPE                   
         BNE   MRKIT30                                                          
         CLI   GDATSUB,GDAAPPAA    WAS THIS MARKED BY AUTO APPROVE?             
         BNE   MRKIT30                                                          
         MVC   GDAAPPDT,TODAYP     TODAY'S DATE                                 
         MVC   GDAAPTIM,CURTIME    CURRENT TIME                                 
*                                                                               
MRKIT60  MVC   MSG,=CL10'PUTREC'                                                
         GOTO1 ADUMP,DMCB,(RC),(R4),L'IO                                        
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    MRKIT65                                                          
         CLI   QOPT4,C'Y'          DO WE WANT TO MARK RECORD                    
         BNE   MRKIT65                                                          
*                                                                               
         GOTO1 =A(DMPUTREC),DMCB,(RC)   PUT RECORD                              
         BE    *+6                                                              
         DC    H'0'                                                             
MRKIT65  BRAS  RE,UPDVPAS          TURN ON APPR BIT IN VENDOR PASSIVES          
*                                                                               
MRKIT70  DS    0H                                                               
         GOTO1 ABFRIN,DMCB,(RC),BUFFASEQ       GET NXT RECORD                   
         TM    BCTSERRS,BUFFEEOF                                                
         BNO   MRKIT10                                                          
*                                                                               
MRKITX   B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
**********************************************************************          
* BUILD E5 ELEMENT (IF NOT ON RECORD)                                *          
*                                                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING GDAELD,R5                                                        
BLDE5    NTR1                                                                   
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   GDAEL,GDAELQ        X'E5' ELEMENT                                
         MVI   GDALN,GDALN3Q                                                    
         MVI   GDATYPE,GDAAPP      APPROVAL DATE TYPE                           
         MVI   GDATSUB,GDAAPPAA    APPROVED BY AUTO APPROVE                     
         MVC   GDAAPPDT,TODAYP     SAVE OFF APPROVAL DATE                       
         MVC   GDAAPTIM,CURTIME    SAVE OFF CURRENT TIME                        
         BAS   RE,ADDL                                                          
*                                                                               
BLDE5X   B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*&&DO                                                                           
**********************************************************************          
* PRINT TOTALS PAGE                                                  *          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R5                                                        
PRTOTAL  NTR1                                                                   
*                                                                               
PRTOT10  BRAS  RE,CLEAROUT         CLEAR OUTPUT BUFFER                          
         MVI   RCSUBPRG,4          TOTALS PAGE SUBPROG                          
         MVI   FORCEHED,C'Y'                                                    
         LA    R5,XP                                                            
*                                                                               
         MVC   PTOTMSG,=CL30'APPROVED IN THIS RUN'                              
         EDIT  (P8,PTOTAPP),(L'PTOT,PTOT),2,MINUS=YES,                 +        
               COMMAS=YES                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVC   PTOTMSG,=CL30'PREVIOUSLY APPROVED'                               
         EDIT  (P8,PTOTPRV),(L'PTOT,PTOT),2,MINUS=YES,                 +        
               COMMAS=YES                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         AP    PTOTPRV,PTOTAPP                                                  
*                                                                               
         MVC   PTOTMSG,=CL30'TOTAL APPROVED'                                    
         EDIT  (P8,PTOTPRV),(L'PTOT,PTOT),2,MINUS=YES,                 +        
               COMMAS=YES                                                       
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
PRTOTX   B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* ADD ELEMENT                                                         *         
*     R5 = A(ELEMENT)                                                 *         
***********************************************************************         
         SPACE 1                                                                
ADDL     NTR1                                                                   
         LA    R2,IO                                                            
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),(R2),(R5)                              
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
*                                                                               
ACCMST   DC    CL8'ACCMST'                                                      
PKROUND  DC    XL2'010C'                                                        
         SPACE 2                                                                
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(DWNL)             DOWNLOAD ROUTINE                             
         DC    A(DWNBUF)           DOWNLOAD BUFFER                              
         DC    A(OUTBUF)           DOWNLOAD OUTPUT BUFFER                       
         DC    A(DWNRTE)           DOWNLOAD TOTALS ROUTINE                      
         DC    A(HD1TAB)           FIRST HEADLINE DWNLD FIELDS TABLE            
         DC    A(STATTAB)           CASH AVAILABILITY STATUS TABLE              
         DC    A(TSRTAB)           SS TRANSACTIONS TABLE                        
         DC    A(PSSATAB)          SS TRANSACTIONS TABLE                        
         DC    A(PSBATAB)          SS TRANSACTIONS TBL 06 ENTRIES ONLY          
         DC    A(SETMOS)           SET START AND END MOS DATES                  
         DC    A(DUMP)             ROUTINE TO DITTO RECORDS                     
         DC    A(MAINTAB)          MAIN TABLE                                   
         DC    A(LDSYTAB)          LEDGER SYSTEM TABLE                          
         DC    A(MBUFF)            BUFFERIN ROUTINE                             
         DC    A(BINADD)           ROUTINE TO ADD TO BINSEARCH TABLE            
         DC    V(AUTOAB)           AUTO APPROVE MODULE                          
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(BINSRCH)          BINSRCH (31 BIT MODE)                        
         DC    V(HELLO)            HELLO                                        
         DC    V(ACLIST)           CLIENT LIST                                  
         DC    V(BUFFERIN)         BUFFERIN                                     
         DC    V(DLFLD)            DOWNLOAD MODULE                              
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R5,56,ELCODE                                                     
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL2 R5,DATADISP,ELCODE,2                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE UPDATES VENOR PASSIVE POINTER WITH THE APPROVED BIT        *          
* R2=(ADDRESS OF THE CURRENT TABLE ENTRY)                            *          
**********************************************************************          
         USING PSSAD,R2                                                         
         SPACE 1                                                                
UPDVPAS  NTR1  BASE=*,LABEL=*                                                   
         USING AAVPASD,R3          AUTO APPROVE VENDOR PASSIVE PTR              
         XC    SVKEY,SVKEY         BUILD KEY TO READ SR RECDS                   
         LA    R3,SVKEY                                                         
         MVC   AAVPKEY,SPACES                                                   
         MVI   AAVPTYP,AAVPTYPQ    X'24'                                        
         MVI   AAVPSUB,AAVPSUBQ    X'01'                                        
         MVC   AAVPCPY,RCCOMPFL                                                 
         MVC   AAVPCLT,PSSKCLI     CLIENT CODE                                  
         MVC   AAVPPRD,PSSKPRO     PRODUCT CODE                                 
         MVC   AAVPEST,PSSKEST     ESTIMATE                                     
         MVC   AAVPMOS,PSSKMOS     MOS                                          
         MVC   AAVPSYS,PSSKSYS     SYSTEM                                       
         MVC   AAVPOFF,PSSKOFF     OFFICE                                       
         MVC   AAVPACCT,PSSKSTN+1  ACCOUNT                                      
         MVC   AAVPKDA,DSKADDR     DISK ADDRESS                                 
         MVI   AAVPFLG1,0          WANT ALL TYPES OF ESTIMATES                  
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   SVKEY(AAVPFLG1-AAVPKEY),IOKEY                                    
         BNE   UPDVX                                                            
         LA    R3,IOKEY                                                         
*                                                                               
         MVC   MSG,=CL10'VENDOR BEF'                                            
         GOTO1 ADUMP,DMCB,(RC),(R3),L'IOKEY+10                                  
*                                                                               
         OI    AAVPSTAT,AAVPAPP    TURN ON APPROVE BIT                          
*                                                                               
         MVC   MSG,=CL10'VENDOR AFT'                                            
         GOTO1 ADUMP,DMCB,(RC),(R3),L'IOKEY+10                                  
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    UPDVX                                                            
         CLI   QOPT4,C'Y'          DO WE WANT TO MARK RECORD                    
         BNE   UPDVX                                                            
         GOTO1 =A(DMWRTDR),DMCB,(RC)                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDVX    J     EXIT                                                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS UPDVPAS                                                   *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE INITIALIZES AUTO APPROVE BLOCK TO PASS TO ROUTINE          *          
**********************************************************************          
         SPACE 1                                                                
INITABLK NTR1  BASE=*,LABEL=*                                                   
         XC    AUTBLK(AUTBLKL),AUTBLK                                           
         LAY   RF,AAEXTRA          EXTRA PARMS FOR AUTBLK                       
         XC    0(AUTBLKXLQ,RF),0(RF)                                            
*                                                                               
         MVC   AUTALST,ADLSTREC                                                 
         OC    ADLSTREC,ADLSTREC                                                
         BNZ   INITAB10                                                         
         MVC   AUTCLNT,QCLI        CLIENT CODE                                  
         MVC   AUTPROD,QPROD       PRODUCT CODE                                 
         MVC   AUTEST,QEST         PRODUCT CODE                                 
INITAB10 MVC   AUTSMOS,MMOSSTR     START MOS DATE                               
         MVC   AUTEMOS,MMOSEND     END MOS DATE                                 
         MVC   AUTCOMF,ADCOMFAC    COMFACS ADDRESS                              
         MVC   AUTBFRIN,VBUFFRIN   ADDRESS OF BUFFERIN                          
         MVC   AUTABIN,BINSRC31    ADDRESS OF 31 BIT BINSEARCH RTN.             
         MVC   ADYNALOC,DYNALLOC   ADDRESS OF DYNAMIC ALLOCATION                
         MVI   AUTOFSW,C'Y'        RUNNING OFFLINE TSAR                         
         MVC   AUTCPY,RCCOMPFL     COMPANY CODE                                 
         MVC   AUTALPHA,ALPHAID    COMPANY ALPHAID                              
         MVC   AUTLDGR,QLEDGER     LEDGER                                       
*                                  ALL MEDIAS OR SPECIFIC MEDIA                 
         CLI   QMED,C'+'           DO WE WANT SPECIFIC SYSTEM                   
         BNE   INITAB20                                                         
         MVC   AUTSYSTM,QSYSTEM    SYSTEM CODE S/P/N                            
         B     INITAB30                                                         
INITAB20 MVC   AUTMEDIA,QMED       MEDIA CODE FOR A GIVEN SYSTEM                
*                                                                               
INITAB30 DS    0H                                                               
         MVC   AUTOPT1,QOPT1                                                    
         MVC   AUTOPT2,QOPT2                                                    
         MVC   AUTOPT5,QOPT5                                                    
         MVC   AUTOPT6,QOPT6                                                    
         MVC   AUTOPT8,QOPT8                                                    
         MVC   AUTPROF,PROFILES    PASS PROGRAM CONTROL PROFILES                
         ZAP   AUTPCNT,REQPCNT                                                  
*                                                                               
         L     R0,=A(TRCHK)                                                     
         STCM  R0,15,TRCHOOK                                                    
*                                                                               
         CLI   QOPT9,C' '                                                       
         BNH   *+10                                                             
         MVC   AUTPRF3,QOPT9                                                    
*                                                                               
*        CLI   AUTPRF3,C'Y'                                                     
*        BNE   *+8                                                              
*        MVI   QOPT2,C'Y'                                                       
*                                                                               
         LAY   RF,AAEXTRA          EXTRA PARMS FOR AUTBLK                       
         ST    RF,AUTEXTRA         SAVE IN AUTBLK                               
         L     RE,AMONACC          A(ACMD)                                      
         LA    RE,ACMOFCN-ACMD(RE) ADDRESS OF REQUESTED OFFICE LIST             
         ST    RE,AACMOFCN-AUTBLKX(RF)                                          
*                                                                               
         LAY   RF,AAEXTRA          EXTRA PARMS FOR AUTBLK                       
         GOTO1 =A(GETOPOS),DMCB,QLEDGER,LDGOPOSP-AUTBLKX(RF)                    
         LAY   RF,AAEXTRA          EXTRA PARMS FOR AUTBLK                       
         CLI   LDGOPOSP-AUTBLKX(RF),C' '                                        
         BH    *+8                                                              
         MVI   LDGOPOSP-AUTBLKX(RF),C'T' DEFAULT TO 'T' IF NOT SET              
*                                                                               
         LAY   RF,AAEXTRA          EXTRA PARMS FOR AUTBLK                       
         GOTO1 =A(GETOPOS),DMCB,=C'R',LDGOPOSR-AUTBLKX(RF)                      
         LAY   RF,AAEXTRA          EXTRA PARMS FOR AUTBLK                       
         CLI   LDGOPOSR-AUTBLKX(RF),C' '                                        
         JH    EQXIT                                                            
*                                                                               
* OFFPOS VALUE MISSING FROM SR LEDGER                                           
*                                                                               
         XC    HEADHOOK,HEADHOOK                                                
         MVC   XP(40),=CL40'OFFPOS VALUE MISSING FROM SR LEDGER'                
         GOTO1 ACREPORT                                                         
         MVC   XP(40),=CL40'REPORT NOT RUN'                                     
         GOTO1 ACREPORT                                                         
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
* READ LEDGER AND GET OFFICE DISPLACEMENT (LDGOPOS)                             
* P1 EXPECTED TO ADDRESS LEDGER                                                 
* P2 EXPECTED TO ADDRESS OUTPUT BYTE FOR LDGOPOS                                
GETOPOS  NTR1  BASE=*,LABEL=*                                                   
         L     R2,4(R1)            DMCB PARM2, A(OUTPUT), SAVE IN R2            
         MVI   0(R2),X'00'         INITIALIZE OUTPUT TO NULL                    
*                                                                               
         MVC   SVKEY,SPACES                                                     
         LA    R5,SVKEY                                                         
         USING LDGRECD,R5                                                       
         MVC   LDGKCPY,RCCOMPFL    COMPANY                                      
         MVI   LDGKUNT,C'S'        UNIT                                         
*                                                                               
         L     RF,0(R1)            DMCB PARM1 (LEDGER)                          
         MVC   LDGKLDG,0(RF)       LEDGER                                       
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   SVKEY(L'LDGKEY),IOKEY                                            
         JNE   *+2                 LEDGER NOT FOUND                             
*                                                                               
         LA    R5,IOKEY                                                         
         GOTO1 =A(DMGETREC),DMCB,(RC),LDGKDA                                    
         JNE   *+2                 LEDGER NOT FOUND                             
         DROP  R5                                                               
*                                                                               
         LA    R5,IO               LEDGER RECORD                                
         MVI   ELCODE,LDGELQ       LEDGER ELEMENT X'14'                         
         BRAS  RE,GETEL                                                         
         JNE   *+2                 LEDGER ELEMENT NOT FOUND                     
*                                                                               
         USING LDGELD,R5                                                        
*                                                                               
         CLI   LDGOPOS,C' '        ANYTHING IN OFFICE POSITION?                 
         JNH   EXIT                                                             
*                                                                               
         MVC   0(1,R2),LDGOPOS                                                  
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
**********************************************************************          
* CLEAR OUTPUT BUFFER TO SPACES                                                 
**********************************************************************          
         SPACE 1                                                                
CLEAROUT NTR1  BASE=*,LABEL=*                                                   
         L     RE,AOUTBUF                                                       
         LA    RF,PLINLN1Q   CLEAR DOWNLOAD BUFFER                              
         XR    R0,R0                                                            
         LA    R1,X'40'                                                         
         SLL   R1,24                                                            
         MVCL  RE,R0         CLEAR TO SPACES                                    
         J     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
**********************************************************************          
* PRINT/DOWNLOAD SAVED PAYABLE SS 05 ENTRY                                      
**********************************************************************          
         SPACE 1                                                                
BLDPSBTB NTR1   BASE=*,LABEL=*                                                  
                                                                                
         USING PSBTABD,R5                                                       
         LA    R5,PSBAWRK    ADDRESS OF THE PSBATAB WORK AREA                   
*                                                                               
         USING PSSAD,R2                                                         
         LA    R2,PSSAWRK    POINT TO WORK AREA                                 
*                                                                               
         CLC   SVVENDOR,PS6KTYP                                                 
         BE    BLDSB10                                                          
*                                                                               
         MVC   SVVENDOR,PS6KTYP    SAVE OFF UPTO CURRENT VENDOR CODE            
*                                                                               
         USING BIND,R1                                                          
         L     R1,APSBATAB                                                      
         ICM   R0,15,BININ         NUMBER OF ENTRIES IN TABLE                   
         LTR   R0,R0                                                            
         BZ    BLDSB10             START ADDING TO BINTABLE                     
         BRAS  RE,PRINT06          PRINT OUT ALL OF 06'S                        
*                                                                               
         XC    BININ,BININ         CLEAR PSBATAB TABLE                          
         DROP  R1                                                               
*                                                                               
BLDSB10  DS    0H                                                               
         XC    PSBAWRK,PSBAWRK     CLEAR TABLE BUILD WORK AREA                  
         LA    R1,PSBABKT          START OF BUCKETS                             
         BRAS  RE,ZAPWRKBK         ZAP ALL BUCKETS                              
*                                                                               
         MVI   PSBTYP,X'06'                                                     
         MVI   PSBSTYP,X'00'     CREATE SR ENTRY                                
         MVC   PSBKSYS,PS6KSYS  SYSTEM                                          
         MVC   PSBKMED,PS6KMED  MEDIA                                           
         MVC   PSBKCLI,PS6KCLI  CLIENT                                          
         MVC   PSBKPRO,PS6KPRO  PRODUCT                                         
         MVC   PSBKEST,PS6KEST  ESTIMATE                                        
         MVC   PSBKMOS,PS6KMOS  MOS                                             
*                                                                               
         MVC   PSBKVEN,PSSKVEN     VENDOR CODE                                  
         MVC   PSBSRAC,PS6SRAC     SR ACCOUNT CODE                              
         MVC   PSBSRCAC,PS6SRCAC   SR CONTRA ACCOUNT                            
         MVC   PSBSRBNO,PS6SRBNO   SR BILL NUMBER                               
         MVC   PSBSRBDA,PS6SRBDA   BILL DATE                                    
         MVC   PSBSRDDA,PSSSRDDA   CLT INV DUE DATE                             
*                                                                               
         MVC   PSBSRDPD,PSSSRDPD   CLT DEPOSIT DATE                             
         MVC   PSBVNAM,PSSVNAM     VENDOR NAME                                  
         ZAP   PSBBILLB,PSSBILLB   BILLED AMOUNT SR DEBITS                      
         ZAP   PSBRCVDB,PSSRCVDB   RCVD AMOUNT SR CREDITS                       
         ZAP   PSBRPCNT,PSSRPCNT   PERCENT RCVD                                 
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),PSBAWRK,APSBATAB   ADD TABLE ENTRY             
*                                                                               
         XC    PSBAWRK,PSBAWRK                                                  
         LA    R1,PSBABKT          START OF BUCKETS                             
         BRAS  RE,ZAPWRKBK         ZAP ALL BUCKETS                              
*                                                                               
         MVI   PSBTYP,X'06'                                                     
         MVI   PSBSTYP,X'01'     CREATE SS/PAYABLE ENTRY                        
         MVC   PSBKSYS,PS6KSYS  SYSTEM                                          
         MVC   PSBKMED,PS6KMED  MEDIA                                           
         MVC   PSBKCLI,PS6KCLI  CLIENT                                          
         MVC   PSBKPRO,PS6KPRO  PRODUCT                                         
         MVC   PSBKEST,PS6KEST  ESTIMATE                                        
         MVC   PSBKMOS,PS6KMOS  MOS                                             
*                                                                               
         MVC   PSBKVEN,PSSKVEN  VENDOR CODE                                     
         MVC   PSBVNAM,PSSVNAM   VENDOR NAME                                    
*                                                                               
         MVC   PSBKSTN,PS6KSTN    STATION                                       
         MVC   PSBKINV,PS6KINV    INVOICE                                       
*                                                                               
         MVC   PSBINVD,PSSINVD    INVOICE DATE                                  
*                                                                               
         MVC   PSBINVCD,PSSINVCD   INVOICE CLEARED DATE                         
*                                                                               
         MVC   PSBPNAM,PSSPNAM   PAYEE NAME                                     
         MVC   PSBKDA,PSSKDA     DISK ADDRESS                                   
         MVC   PSBKSTAT,PSSKSTAT APPROVAL STATUS                                
         MVC   PSBFLAG,PSSFLAG   CASH AVAILABILITY FLAGS                        
*                                                                               
         LA    R4,PSSDISBB        POINT TO START OF BUCKETS                     
         LA    R6,PSBDISBB        POINT WHERE TO START MOVING TO                
         LHI   R1,8               NUMBER OF BUCKETS TO LOOP TO MOVE TO          
*        LHI   R1,PSBPBKCT        NUMBER OF BUCKETS TO LOOP TO MOVE TO          
BLDSB30  DS    0H                                                               
         MVC   0(PSBABKLN,R6),0(R4)                                             
*                                                                               
         LA    R4,PSSABKLN(R4)    POINT TO NEXT SOURCE BUCKET                   
         LA    R6,L'PSBABKT(R6)   POINT TO NEXT DESTINATION BUCKET              
         BCT   R1,BLDSB30                                                       
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),PSBAWRK,APSBATAB   ADD TABLE ENTRY             
*                                                                               
BLDSBTBX XIT1                                                                   
         DROP  R2,R5                                                            
         EJECT                                                                  
         LTORG                                                                  
**********************************************************************          
* PRINT/DOWNLOAD SAVED PAYABLE SS 05 ENTRY                                      
**********************************************************************          
         SPACE 1                                                                
PRINT06  NTR1   BASE=*,LABEL=*                                                  
         USING PLINED,R5                                                        
         L     R5,AOUTBUF    ADDRESS OF THE OUTPUT BUFFER                       
         USING BIND,R3                                                          
         L     R3,APSBATAB                                                      
         ICM   R7,15,BININ                                                      
         LTR   R7,R7                                                            
         BZ    PRINT06X                                                         
         USING PSBTABD,R2                                                       
         LA    R2,BINTAB     POINT TO TABLE ENTRY                               
*                                                                               
PRPY02   DS    0H                                                               
*                                                                               
         MVC   PVENCODE,PSBKVEN   VENDOR CODE                                   
         MVC   PVENNAME,PSBVNAM   VENDOR NAME                                   
*                                                                               
         MVC   PSYS,PSBKSYS  SYSTEM                                             
         MVC   PMED,PSBKMED  MEDIA                                              
         MVC   PCLI,PSBKCLI  CLIENT                                             
         MVC   PPRO,PSBKPRO  PRODUCT                                            
         MVC   PEST,PSBKEST  ESTIMATE                                           
*                                                                               
         MVC   WORK(2),PSBKMOS   YM                                             
         MVI   WORK+2,X'01'      DAY                                            
         GOTO1 DATCON,DMCB,(1,WORK),(6,PMOS)                                    
*                                                                               
         CLI   PSBSTYP,X'01'      PAYABLE ENTRY ?                               
         BE    PRPY05                                                           
*                                                                               
         MVC   PSTA,PSBSRAC       SR ACCOUNT CODE                               
         MVC   PINV(L'PSBSRBNO),PSBSRBNO      SR BILL NUMBER                    
*                                                                               
         MVC   WORK(3),PSBSRBDA   BILLED/INVOICE DATE SR                        
         GOTO1 DATCON,DMCB,(1,WORK),(10,PINVDATE)                               
*                                                                               
         MVC   WORK(3),PSBSRDDA   CLT INV DUE DATE                              
         GOTO1 DATCON,DMCB,(1,WORK),(10,PINVDUE)                                
*                                                                               
         MVC   WORK(3),PSBSRDPD   CLT DEPOSIT DATE                              
         GOTO1 DATCON,DMCB,(1,WORK),(10,PDEPDATE)                               
         B     PRPY10                                                           
*                                                                               
PRPY05   MVC   PSTA,PSBKSTN                                                     
         MVC   PINV,PSBKINV                                                     
         MVC   PPAYNAME,PSBPNAM   PAYEE NAME                                    
*                                                                               
         MVC   WORK(3),PSBINVD    INVOICE DATE                                  
         GOTO1 DATCON,DMCB,(1,WORK),(10,PINVDATE)                               
*                                                                               
         MVC   WORK(3),PSBINVCD   INVOICE CLEARED DATE                          
         GOTO1 DATCON,DMCB,(1,WORK),(10,PINVCDAT)                               
         GOTO1 HEXOUT,DMCB,PSBKDA,PDISKADD,L'PDISKADD                           
*                                                                               
PRPY10   DS    0H                 PRINT CLEARED,BILLED,APPLIED BUCKETS          
         LA    R4,PSBABKT         POINT TO START OF BUCKETS                     
         LA    R6,PBUCKET         POINT WHERE TO START PRINTING                 
         LHI   R1,PSBBKCT2        NUMBER OF BUCKETS TO LOOP TO PRINT            
PRPY20   DS    0H                                                               
         EDIT  (P8,(R4)),(PBUCLEN-1,(R6)),2,ZERO=BLANK,MINUS=YES,      +        
               COMMAS=YES                                                       
         LA    R4,PSBABKLN(R4)                                                  
*                                                                               
         LA    RE,PUNDISB                                                       
         CR    R6,RE                                                            
         BNE   *+8                                                              
         LA    R6,L'PMARKA(R6)     BUMP TO PRINT 'A' IN APPROVED ITEMS          
*                                                                               
         LA    R6,L'PBUCKET(R6)                                                 
         BCT   R1,PRPY20                                                        
*                                                                               
PRPY30   DS    0H                                                               
         CLI   PSBSTYP,X'01'       IS THIS PAYABLE ENTRY                        
         BNE   PRPY90              SR LEVEL ENTRY PRINT AND EXIT                
*                                                                               
         USING STATTABD,RE                                                      
         L     RE,ASTATTAB      PRINT A=APPROVE,U=UNAPROVE,H=HELD ETC           
PRPY40   CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   STATRTYP,PSBKSTAT   IS THIS APP/UNAPP/HELD/TOT ETC               
         BNE   PRPY60                                                           
         CLI   STATCSH,X'FF'                                                    
         BE    PRPY50                                                           
         MVC   BYTE,STATCSH        CHECK CASH AVAILABILITY                      
         NC    BYTE,PSBFLAG                                                     
         BZ    PRPY60                                                           
         CLC   STATOPT,QOPT2                                                    
         BNE   PRPY60                                                           
PRPY50   CLI   STATSKIP,C'Y'       SKIP PRINTING THIS LINE?                     
         BE    PRPY100                                                          
         B     PRPY70                                                           
PRPY60   LA    RE,STATLNQ(RE)                                                   
         B     PRPY40                                                           
PRPY70   OI    FLAG1,FLGSTN        PRINTING STATION                             
         MVC   PMARKA,STATMRKA                                                  
         CLI   PSBKSTAT,PSSKTOT                                                 
         BNE   PRPY80                                                           
         CP    PSBUNAPB,=P'0'                                                   
         BE    PRPY90                                                           
PRPY80   MVC   PMARK,STATMRK                                                    
         B     PRPY90                                                           
PRPY90   BRAS  RE,PRINTIT                                                       
*        OI    FLAG,FLGPRNT                                                     
PRPY100  DS    0H                                                               
         BRAS  RE,CLEAROUT         CLEAR OUTPUT BUFFER                          
*                                                                               
         LA    R2,PSBALNQ(R2)                                                   
         BCT   R7,PRPY02           PRINT REST OF THE BUFFER                     
         XC    BININ,BININ         CLEAR TABLE                                  
*                                                                               
PRINT06X XIT1                                                                   
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
         LTORG                                                                  
**********************************************************************          
* CLEAR XP TO SPACES                                                            
**********************************************************************          
         SPACE 1                                                                
CLEARXP  NTR1  BASE=*,LABEL=*                                                   
         LA    R5,XP                                                            
         MVI   XP,C' '             FILL XP WITH SPACES                          
         MVC   XP+1(L'XP-1),XP                                                  
         J     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
*              PASS SOON INFO TO FACWK                                *         
***********************************************************************         
*                                                                               
SETFWRK  NTR1  BASE=*,LABEL=*                                                   
         USING FWRECD,R3                                                        
         LA    R3,CHOPWRK          USE ANY BUFFER                               
         XC    CHOPWRK,CHOPWRK                                                  
         MVC   FWRLEN,=Y(FWRLNQ)   SET UP USER INFO FOR SRUPD00                 
         MVC   FWRUSER,=C'USER='                                                
         MVC   FWRUSID,USID                                                     
         MVC   FWRLUID,LUID                                                     
         MVC   FWRPQID,PQID                                                     
*        MVC   FWRWKID,WKID                                                     
*                                                                               
         USING SSOOFF,RF                                                        
         L     RF,VSSB                                                          
         L     R2,SSOFWNDX         INDEX                                        
         L     R0,SSOFWBUF         BUFFER                                       
         GOTO1 DATAMGR,DMCB,(0,FACADD),(0,FACWRK),(R2),(R3),(R0)                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
         DROP  R3,RF                                                            
         EJECT                                                                  
FACADD   DC    C'ADD '                                                          
FACWRK   DC    C'FACWRK '                                                       
**********************************************************************          
* LITERALS FOR SETFWRK  ROUTINE                                      *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* PRINT ROUTINE                                                      *          
**********************************************************************          
         SPACE 1                                                                
PRINTIT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        BRAS  RE,HEADUP                                                        
*        GOTO1 ACREPORT                                                         
         GOTO1 ADWNRTE,DMCB,(RC)   IF SO - POINT TO DOWNLOAD                    
*                                                                               
         J     EXIT                                                             
**********************************************************************          
* LITERALS FOR PRINTIT  ROUTINE                                      *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* SETUP HEADLINES                                                    *          
**********************************************************************          
         SPACE 1                                                                
HEADUP   NTR1  BASE=*,LABEL=*                                                   
         MVC   XHEAD3+1(10),=CL10'USER ID:  '                                   
         MVC   XHEAD3+12(L'SVCLOGO),SVCLOGO  COMPANY'S MAIN ID                  
*                                                                               
         MVC   XHEAD3+88(4),=C'MOS='                                            
         MVC   XHEAD3+92(6),PRTSTART                                            
         MVI   XHEAD3+98,C'-'                                                   
         MVC   XHEAD3+99(6),PRTEND                                              
*                                                                               
         MVC   XHEAD4+88(17),=C'*** DRAFT RUN ***'                              
         CLI   QOPT4,C'Y'                                                       
         BNE   *+10                                                             
         MVC   XHEAD4+88(17),=C'*** LIVE RUN *** '                              
*                                                                               
         CLC   QTOLRNCE,SPACES                                                  
         BE    HEADX                                                            
*                                                                               
         MVC   XHEAD4+159(10),=C'TOLERANCE='                                    
*        MVC   XHEAD4+169(2),QTOLRNCE                                           
*        MVI   XHEAD4+171,C'%'                                                  
         EDIT  (B2,QTOLRNCE),(6,XHEAD4+169),2,ALIGN=LEFT                        
         LA    RF,XHEAD4+174                                                    
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,1(RF)                                                         
         MVI   0(RF),C'%'                                                       
         CLI   QOPT10,C'D'                                                      
         BNE   HEADX                                                            
         MVI   0(RF),C'$'                                                       
*                                                                               
HEADX    J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS FOR HEADUP   ROUTINE                                      *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE ZAPS SYS/MED/CLI/PROD TOTAL BUCKETS                        *          
* PASS ADDRESS OF BUCKET TO ZAP IN R1   R1=A(SYS/MED/CLI/PROD BUCKET)*          
**********************************************************************          
         SPACE 1                                                                
ZAPTOTBK NTR1  BASE=*,LABEL=*                                                   
         LA    R0,CLITBKCT         NO. OF BUCKETS                               
         ZAP   0(CLITBKLN,R1),=P'0'     CLEAR FIELDS                            
         LA    R1,CLITBKLN(R1)                                                  
         BCT   R0,*-10             NUMBER OF BUCKETS TO LOOP                    
ZAPTBX   J     EXIT                                                             
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO CHECH IF SYS/MED/CLI/PROD TOTAL BUCKETS IN NOT ZERO     *          
* PASS ADDRESS OF BUCKET TO ZAP IN R1   R1=A(SYS/MED/CLI/PROD BUCKET)*          
**********************************************************************          
         SPACE 1                                                                
CKTOTBK0 NTR1  BASE=*,LABEL=*                                                   
         NI    FLAG2,X'FF'-FLGTOTZR                                             
         LA    R0,CLITBKCT         NO. OF BUCKETS                               
CKTOTBK1 CP    0(CLITBKLN,R1),=P'0'     CLEAR FIELDS                            
         JNE   CKTOTX                                                           
         LA    R1,CLITBKLN(R1)                                                  
         BCT   R0,CKTOTBK1         NUMBER OF BUCKETS TO LOOP                    
         OI    FLAG2,FLGTOTZR                                                   
CKTOTX   J     EXIT                                                             
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE ZAPS WORK AREAS BUCKETS                                    *          
* PASS ADDRESS OF BUCKET TO ZAP IN R1   R1=A(WORK AREA BUCKET)       *          
**********************************************************************          
         SPACE 1                                                                
ZAPWRKBK NTR1  BASE=*,LABEL=*                                                   
         LA    R0,PSBABKCT         NO. OF BUCKETS                               
         ZAP   0(PSBABKLN,R1),=P'0'  ZERO OUT THE BUCKETS                       
         LA    R1,PSBABKLN(R1)                                                  
         BCT   R0,*-10             NUMBER OF BUCKETS TO LOOP                    
ZAPWBX   J     EXIT                                                             
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO PRINT TOTALS AT SYS/MED/CLI/PROD LEVEL CHANGES          *          
**********************************************************************          
         SPACE 1                                                                
PSMCPTOT NTR1  BASE=*,LABEL=*                                                   
         USING PLINED,R5                                                        
         L     R5,AOUTBUF          OUTPUT BUFFER                                
*                                                                               
         TM    FLAG,FLGPRNT        WAS ANYTHING PRINTED ?                       
         BNO   PSMCPTX                                                          
*                                                                               
         MVC   PPRO,LSTPRO         MOVE PRODUCT CODE TO PRINT                   
         MVC   PCLI,LSTCLI                                                      
         MVC   PMED,LSTMED                                                      
         MVC   PSYS,LSTSYS                                                      
*                                                                               
*        MVC   PPRO,LSTPRO         MOVE PRODUCT CODE TO PRINT                   
         MVC   PMOS,SPACES         CLEAR PRODUCT PRINT FIELD                    
         MVC   PEST,=C'TOTALS'     MOVE TOT IN NEXT FIELD                       
         LA    R1,PROTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR PRDCT                
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
         MVC   PPRO,SPACES                                                      
*                                                                               
*        MVC   PCLI,LSTCLI                                                      
         MVC   PMOS,SPACES         CLEAR PRODUCT PRINT FIELD                    
         MVC   PEST,=C'TOTALS'                                                  
         LA    R1,CLITBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR CLIENT               
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
         MVC   PCLI,SPACES                                                      
*                                                                               
*        MVC   PMED,LSTMED                                                      
         MVC   PMOS,SPACES         CLEAR PRODUCT PRINT FIELD                    
         MVC   PEST,=C'TOTALS'                                                  
         LA    R1,MEDTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR MEDIA                
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
         MVC   PMED,SPACES                                                      
*                                                                               
*        MVC   PSYS,LSTSYS                                                      
         MVC   PMOS,SPACES         CLEAR PRODUCT PRINT FIELD                    
         MVC   PEST,=C'TOTALS'                                                  
         LA    R1,SYSTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR SYSTEM               
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
PSMCPTX  J     EXIT                                                             
         DROP  R5                                                               
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE ADDS TO SYS/MED/CLI/PROD TOTAL BUCKETS                     *          
* PASS ADDRESS OF BUCKET TO ADD IN R1   R1=A(SYS/MED/CLI/PROD BUCKET)*          
**********************************************************************          
         SPACE 1                                                                
         USING PSSAD,R2                                                         
ADDTOTBK NTR1  BASE=*,LABEL=*                                                   
         LA    R0,CLITBKCT         NO. OF BUCKETS                               
         LA    R3,PSSABKT                                                       
         AP    0(CLITBKLN,R1),0(PSSABKLN,R3)    ADD TO TOTALS BUCKET            
         LA    R1,CLITBKLN(R1)                                                  
         LA    R3,PSSABKLN(R3)                                                  
         BCT   R0,*-14             NUMBER OF BUCKETS TO LOOP                    
ADDTBX   J     EXIT                                                             
         DROP  R2                                                               
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE PRINTS  SYS/MED/CLI/PROD TOTAL BUCKETS                     *          
* PASS ADDRESS OF BUCKET TO ADD IN R1   R1=A(SYS/MED/CLI/PROD BUCKET)*          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R5                                                        
PRNTOTBK NTR1  BASE=*,LABEL=*                                                   
         LA    R0,CLITBKCT         NO. OF BUCKETS                               
         LA    R6,PBUCKET                                                       
PRNTB10  EDIT  (P8,(R1)),(PBUCLEN,(R6)),2,ZERO=BLANK,MINUS=YES,        +        
               COMMAS=YES                                                       
         LA    R1,CLITBKLN(R1)                                                  
*                                                                               
         LA    RE,PUNDISB          BUMP LITTLE EXTRA FOR THIS FLD               
         CR    R6,RE                                                            
         BNE   *+8                                                              
         LA    R6,L'PMARKA(R6)     BUMP TO PRINT 'A' IN APPROVED ITEMS          
*                                                                               
         LA    R6,L'PBUCKET(R6)                                                 
         BCT   R0,PRNTB10          NUMBER OF BUCKETS TO LOOP                    
PRNTTBX  J     EXIT                                                             
         DROP  R5                                                               
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET CONTROL PROFILES TO READD PROGRAM AA (AUTO APPROVE) PROFILES    *         
***********************************************************************         
*                                                                               
SETPROF  NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         MVI   WORK,C'A'                                                        
         MVC   WORK+2(2),=C'AA'                                                 
         MVC   WORK+12(2),ALPHAID                                               
         L     R2,APROFILE                                                      
         LA    R2,(ACPPFCP1-ACPROFSD)(R2)                                       
         GOTO1 GETPROF,DMCB,WORK,(R2),DATAMGR                                   
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
***********************************************************************         
* SET START AND END MOS DATES                                         *         
***********************************************************************         
         SPACE 1                                                                
SETMOS   DS    0D                                                               
         NMOD1 0,*SETMOS*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         MVC   PRTSTART,SPACES                                                  
         MVC   PRTEND,SPACES                                                    
         XC    MMOSSTR,MMOSSTR                                                  
         XC    MMOSEND,MMOSEND                                                  
*                                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         L     R3,ADQSTACK                                                      
         USING ACQD,R3                                                          
         CLI   ACQCONT1,C'C'       DO WE HAVE CONTINUATION CHARACTER            
         BNE   SETMS60                                                          
         CLI   ACQCONT2,C'C'                                                    
         BNE   SETMS60             AND MOS DATE FILTER IN TYPE FIELDS           
*                                                                               
         LHI   R0,2                CHECK 3RD AND 4TH CARDS FOR MOS              
         LHI   R2,4                4 ENTRIES TO CHECK                           
         LA    R4,ACQTYP1          POINT TO 1ST TYPE                            
SETMS10  DS    0H                                                               
         CLI   0(R4),ACQDATE       DATE FILTER?                                 
         BNE   SETMS20                                                          
         CLI   1(R4),ACQDTMOS      ACQDTTYP                                     
         BE    SETMS30             WHAT FOLLOWS IS A MOS                        
SETMS20  LA    R4,L'ACQTYP1+L'ACQFLT1(R4)   BUMP TO NEXT ENTRY                  
         BCT   R2,SETMS10                                                       
*                                                                               
         CLI   ACQCONT3,C'C'                                                    
         BNE   SETMS60                                                          
         LHI   R2,4                                                             
         LA    R4,ACQCARD4                                                      
         BCT   R0,SETMS10           CHECK 4TH REQUEST CARD FOR MOS              
         B     SETMS60                                                          
*                                                                               
SETMS30  CLC   (ACQDTSTR-ACQTYP1)(L'ACQDTSTR,R4),SPACES                         
         BE    SETMS40                                                          
         MVC   WORK(L'ACQDTSTR),(ACQDTSTR-ACQTYP1)(R4)                          
         CLC   WORK+4(2),SPACES                                                 
         BNE   *+10                                                             
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,MMOSSTR)                                 
         GOTO1 DATCON,DMCB,(0,WORK),(6,PRTSTART)                                
*                                                                               
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,-1  MOS START MINUS 1 YEAR         
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,LASTYEAR)                              
*                                                                               
SETMS40  MVC   MMOSEND,=X'FFFFFF'                                               
         CLC   (ACQDTEND-ACQTYP1)(L'ACQDTEND,R4),SPACES                         
         BE    SETMS20                                                          
         MVC   WORK(L'ACQDTEND),(ACQDTEND-ACQTYP1)(R4)                          
         CLC   WORK+4(2),SPACES                                                 
         BNE   SETMS50                                                          
         MVC   WORK+4(2),=C'31'                                                 
         GOTO1 DATCON,DMCB,(X'30',WORK),(0,WORK),(1,0) LAST DAY OF MO           
*                                                                               
SETMS50  GOTO1 DATCON,DMCB,(0,WORK),(1,MMOSEND)                                 
         GOTO1 DATCON,DMCB,(0,WORK),(6,PRTEND) FOR PRINTING ON REPORT           
*                                                                               
         OC    MMOSSTR,MMOSSTR     IF REQUEST COMES FROM RLP THEN               
         BNZ   SETMS20             THERE IS NO START DATE SPECIFIED             
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,-11                                
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,MMOSSTR)                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(6,PRTSTART)                              
         B     SETMS20                                                          
*                                                                               
SETMS60  DS    0H                                                               
         OC    MMOSSTR,MMOSSTR                                                  
         BNZ   *+6                                                              
         DC    H'0'                NO MOS START DATE PROVIDED                   
         OC    MMOSEND,MMOSEND                                                  
         BNZ   *+6                                                              
         DC    H'0'                NO MOS END DATE PROVIDED                     
SETMSX   XIT1                                                                   
**********************************************************************          
* LITERALS SETMOS  NMOD1                                             *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* TRACE HOOK                                                          *         
***********************************************************************         
TRCHK    NMOD1 0,*TRCHK                                                         
         LR    R2,R1               A(SRC)                                       
         LA    R2,0(R2)                                                         
         LR    R3,R1               A(SRC)                                       
*                                                                               
         BRAS  RE,CLEAROUT         CLEAR OUTPUT BUFFER                          
         GOTO1 ACREPORT                                                         
*                                                                               
         SRL   R3,24                                                            
         CHI   R3,L'XP                                                          
         BNH   *+8                                                              
         LHI   R3,L'XP                                                          
*                                                                               
         LTR   R3,R3                                                            
         BNZ   *+14                                                             
         MVC   XP(30),0(R2)                                                     
         B     TRCHK20                                                          
*                                                                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   XP(0),0(R2)                                                      
         OC    XP(L'SPACES),SPACES                                              
         OC    XP+L'SPACES(L'XP-L'SPACES),SPACES                                
         GOTO1 ACREPORT                                                         
         GOTO1 HEXOUT,DMCB,0(R2),XP,(R3),=C'N'                                  
*                                                                               
TRCHK20  GOTO1 ACREPORT                                                         
*                                                                               
         XMOD1 1                                                                
*&&                                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* TRACE HOOK                                                                    
* P1 HOB - TYPE - HEX, CHAR, OR BOTH                                            
*    LOB - STRING LENGTH                                                        
* P2     - A(STRING)                                                            
* P3 HOB - NARRATION LENGTH (IF PRESENT)                                        
*        - OPTIONAL ADDRESS OF NARRATION, 30 CHAR MAX                           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
*                                                                               
TRCHEXQ  EQU   X'01'                                                            
TRCCHARQ EQU   X'02'                                                            
TRCNARRQ EQU   30                  NARRATION LENGTH                             
*                                                                               
TRCHK    NMOD1 0,*TRCHK                                                         
* CALLER'S DMCB IS OVERWRITTEN BY "GOTO1 REPORT" CALL                           
* SO, SAVE CALLER'S DMCB                                                        
         MVC   SVDMCB,0(R1)                                                     
*                                                                               
         MVC   SVBXHOOK,HEADHOOK                                                
         XC    HEADHOOK,HEADHOOK                                                
*                                                                               
         OC    SVDM3,SVDM3         NARRATION?                                   
         JZ    TRACEK10            NOT PRESENT                                  
*                                                                               
* PRINT NARRATION FIRST                                                         
*                                                                               
         BRAS  RE,CLEARXP          CLEAR XP TO SPACES                           
         ICM   R1,15,SVDM3         A(NARRATION)                                 
         LLC   RF,SVDM3            L'NARRATION STRING                           
*                                                                               
         CHI   RF,TRCNARRQ         LIMIT TO 30 CHARACTERS                       
         JNH   *+8                                                              
         LHI   RF,TRCNARRQ                                                      
*                                                                               
         BCTR  RF,0                                                             
         EXRL  RF,*+10                                                          
         J     *+10                                                             
         MVC   XP(0),0(R1)                                                      
         GOTO1 ACREPORT                                                         
*                                                                               
TRACEK10 DS    0H                                                               
         TM    SVDM1,TRCCHARQ      PRINTABLE TRACE?                             
         JZ    TRACEK30                                                         
*                                                                               
         BRAS  RE,CLEARXP          CLEAR XP TO SPACES                           
         ICM   RE,15,SVDM2         A(STRING)                                    
         LLC   RF,SVDM1+3          STRING LENGTH                                
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   XP(0),0(RE)                                                      
*                                                                               
         EX    RF,*+8                                                           
         J     *+10                                                             
         TR    XP(0),TRTAB                                                      
*                                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
TRACEK30 DS    0H                                                               
         TM    SVDM1,TRCHEXQ       HEX TRACE?                                   
         JZ    TRACEK40                                                         
*                                                                               
         BRAS  RE,CLEARXP          CLEAR XP TO SPACES                           
         ICM   R2,15,SVDM2         A(STRING)                                    
         LLC   R3,SVDM1+3          STRING LENGTH                                
         GOTO1 HEXOUT,DMCB,(R2),XP,(R3),=C'N'                                   
         GOTO1 ACREPORT                                                         
*                                                                               
TRACEK40 DS    0H                                                               
         MVC   HEADHOOK,SVBXHOOK                                                
         XMOD1 1                                                                
*                    0 1 2 3 4 5 6 7 8 9 A B C D E F                            
TRTAB    DC    XL16'40404040404040404040404040404040' 0                         
         DC    XL16'40404040404040404040404040404040' 1                         
         DC    XL16'40404040404040404040404040404040' 2                         
         DC    XL16'40404040404040404040404040404040' 2                         
         DC    XL16'40404040404040404040404040404040' 4                         
         DC    XL16'40404040404040404040404040404040' 5                         
         DC    XL16'60404040404040404040404040404040' 6                         
         DC    XL16'40404040404040404040404040404040' 7                         
         DC    XL16'40404040404040404040404040404040' 8                         
         DC    XL16'40404040404040404040404040404040' 9                         
         DC    XL16'40404040404040404040404040404040' A                         
         DC    XL16'40404040404040404040404040404040' B                         
         DC    XL16'40C1C2C3C4C5C6C7C8C9404040404040' C                         
         DC    XL16'40D1D2D3D4D5D6D7D8D9404040404040' D                         
         DC    XL16'4040E2E3E4E5E6E7E8E9404040404040' E                         
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040' F                         
*                                                                               
*                                                                               
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 APRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),        +        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS  DUMP NMOD1                                               *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUFFERIN INTERFACE ROUTINE                                          *         
*                                                                     *         
* NTRY - P1 = (RC)                                                    *         
*      - P2 = BUFFERIN ACTION (DEFINED IN DDBUFFD)                    *         
*                                                                     *         
* EXIT - CC=EQU - ACTION COMPLETED WITHOUT ERRORS                     *         
*        CC=NEQ - BCTSERRS=BUFFERIN ERROR CODE                        *         
***********************************************************************         
         SPACE 1                                                                
MBUFF    DS    0D                                                               
         NMOD1 0,**MBUFF*                                                       
         L     RC,0(R1)            P1=A(LOCAL WORKING)                          
         SR    R0,R0                                                            
         IC    R0,7(R1)            P2=BUFFERIN ACTION                           
*                                                                               
         LA    R1,PSSAWRK          PAYABLE TABLE WORK AREA                      
         ST    R1,BUFFREC          SET A(INPUT RECORD)                          
         ICM   R3,15,AUTPBUFF                                                   
         MVC   TSARKSAV,0(R1)      SAVE CURRENT RECORD KEY                      
         USING BUFFD,R3            R3=A(BUFFERIN CONTROL BLOCK)                 
*                                                                               
         CHI   R0,BUFFAINI         TEST INITIALIZATION CALL                     
         JNE   BUFFER10                                                         
         USING BIND,R5                                                          
         L     R5,APSSATAB         POINT TO PAYABLES TABLE                      
         SR    RE,RE                                                            
         ICM   RE,7,BINKEY         KEY LENGTH                                   
         SR    RF,RF                                                            
         IC    RF,BINFST           RECORD LENGTH=RECLEN-BUCKETS                 
*        L     RF,BINLEN           RECORD LENGTH                                
         SR    RF,RE               DATA LENGTH                                  
         STCM  RE,3,BUFFLKEY       SET KEY LENGTH                               
         STCM  RF,3,BUFFLCOM       SET COMMENT LENGTH                           
         MVC   BUFFNCOL,BINNUM     NUMBER OF BUCKETS/COLUMNS                    
*                                                                               
BUFFER10 GOTOR AUTBFRIN,DMCB,((R0),BUFFD),BUFFREC,AUTCOMF                       
         MVC   BUFFRET,BUFFERRS-BUFFPARM(R1)                                    
         CHI   R0,BUFFARDH         EMULATE TSAR NOT FOUND ON READ HIGH          
         JNE   BUFFER20                                                         
         L     RF,BUFFREC                                                       
         LH    R1,BUFFLKEY         LENGTH OF THE KEY                            
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         JE    BUFFER20                                                         
         CLC   TSARKSAV(0),0(RF)                                                
         OI    BUFFRET,BUFFERNF                                                 
                                                                                
BUFFER20 MVC   BCTSERRS,BUFFRET    PASS BACK ERRORS                             
         CLI   BUFFRET,0           SET CONDITION CODE FOR CALLER                
         J     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD ROUTINE                                                   *          
**********************************************************************          
         SPACE 1                                                                
DWNRTE   DS    0D                                                               
         NMOD1 0,**DWNR**                                                       
         L     RC,0(R1)                                                         
         MVI   RCSUBPRG,9                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         USING BOXD,R3                                                          
         L     R3,ADBOX                                                         
         MVI   BOXOFF,C'Y'         NO BOXES IF DOWNLOADING                      
         DROP  R3                                                               
*                                                                               
         TM    DWNSTAT,DWNHDLN     WERE THE HEADLINES DOWNLOADED?               
         BO    *+8                                                              
         BAS   RE,DWNHEAD          DOWNLOAD HEADLINES                           
*                                                                               
         USING PLINED,R5                                                        
         L     R5,AOUTBUF                 ADDRESS OF THE OUTPUT BUFFER          
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PSYS),PSYS        MOVE SYSTEM TO DWN FLD                
         LA    R1,L'PSYS                  SYSTEM LENGTH                         
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PMED),PMED        MOVE MEDIA TO DWN FLD                 
         LA    R1,L'PMED                                                        
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PCLI),PCLI        MOVE CLIENT TO DWN FLD                
         LA    R1,L'PCLI                                                        
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PPRO),PPRO        MOVE PRODUCT TO DWN FLD               
         LA    R1,L'PPRO                                                        
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PEST),PEST        MOVE ESTIMATE TO DWN FLD              
         LA    R1,L'PEST                                                        
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PMOS),PMOS        MOVE MOS TO DWN FLD                   
         LA    R1,L'PMOS                                                        
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PSTA),PSTA        MOVE STATION TO DOWNLOAD FLD          
         LA    R1,L'PSTA                                                        
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PPAYNAME),PPAYNAME PAYEE NAME                           
         LA    R1,L'PPAYNAME                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PINV),PINV        MOVE INV# TO DOWNLOAD FLD             
         LA    R1,L'PINV                                                        
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PINVDATE),PINVDATE                                      
         LA    R1,L'PINVDATE                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PINVDUE),PINVDUE  DUE DATE                              
         LA    R1,L'PINVDUE                                                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PDEPDATE),PDEPDATE CLT DEPOSIT DATE                     
         LA    R1,L'PDEPDATE                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PVENCODE),PVENCODE VENDOR CODE                          
         LA    R1,L'PVENCODE                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PVENNAME),PVENNAME VENDOR NAME                          
         LA    R1,L'PVENNAME                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PINVCDAT),PINVCDAT CLEARED DATE                         
         LA    R1,L'PINVCDAT                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         LA    R6,PBUCKET                                                       
         LHI   R3,PSSBKCT2                                                      
DWNR05   MVC   DWNFLD(PBUCLEN),0(R6)      MOVE BUCKET AMT TO DWN FLD            
         LA    R1,PBUCLEN                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD AS NUMBER                    
*                                                                               
         LA    RE,PUNDISB                                                       
         CR    R6,RE                                                            
         BNE   *+8                                                              
         LA    R6,L'PMARKA(R6)     BUMP TO PRINT 'A' IN APPROVED ITEMS          
*                                                                               
         LA    R6,L'PBUCKET(R6)                                                 
         BCT   R3,DWNR05                                                        
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PMARKA),PMARKA    TRANSACTION STATUS                    
         LA    R1,L'PMARKA                                                      
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PMARK),PMARK      APPROVAL STATUS                       
         LA    R1,L'PMARK                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         CLI   QOPT3,C'Y'                 DOWNLOAD DISK ADDRESS?                
         BNE   DWNR07                                                           
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PDISKADD),PDISKADD DISK ADDRESS                         
         LA    R1,L'PDISKADD                                                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
DWNR07   MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOL     DOWNLOAD EOL MARKER                   
*                                                                               
*                                                                               
DWNXIT   XMOD1                                                                  
*        DROP  R2                                                               
                                                                                
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HEADLINES (ONCE PER REPORT)                               *          
**********************************************************************          
         SPACE 1                                                                
DWNHEAD  NTR1                                                                   
         OI    DWNSTAT,DWNHDLN     SET SWITCH TO SHOW HDLNS WERE DWNLD          
*                                                                               
         LA    R0,HD1LNQ                 NUMBER OF HEADINGS IN LINE 1           
         L     R2,AHD1TAB                FIRST HEADLINE TABLE                   
DWNH10   MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         MVC   DWNFLD(L'HD1TAB),0(R2)    FIRST HEADLINE FIELDS                  
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                          
         LA    R2,L'HD1TAB(R2)                                                  
         BCT   R0,DWNH10                                                        
*                                                                               
         CLI   QOPT3,C'Y'                DOWNLOAD DISK ADDRESS ?                
         BNE   DWNH15                                                           
         MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         MVC   DWNFLD(L'HD1TAB),=CL30'DISK ADDRESS'                             
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                          
*                                                                               
DWNH15   MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         GOTO1 ADWNL,DMCB,(RC),DWNEOL    DOWNLOAD EOL MARKER                    
*                                                                               
DWNHX    B     DWNXIT                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD MODULE                                                    *          
*          PARM1 - RC                                                *          
*          PARM2 - ACTION                                            *          
**********************************************************************          
         SPACE 1                                                                
DWNL     DS    0D                                                               
         NMOD1 0,**DOWN**                                                       
         L     RC,0(R1)                                                         
         L     RF,4(R1)                                                         
         STC   RF,DWNMODE          SAVE CURRENT MODE                            
         USING DLCBD,R5                                                         
         L     R5,ADWNBUF                                                       
*                                                                               
         CLI   DWNMODE,DWNINIT     INITIALIZE                                   
         BE    DWNL10                                                           
         CLI   DWNMODE,DWNTEXT     DOWN-LOAD TEXT                               
         BE    DWNL20                                                           
         CLI   DWNMODE,DWNNUM      DOWN-LOAD NUMBER                             
         BE    DWNL30                                                           
         CLI   DWNMODE,DWNPACK     DOWN-LOAD NUMBER (PACKED)                    
         BE    DWNL40                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         CLI   DWNMODE,DWNEOL      END OF LINE                                  
         BE    DWNL50                                                           
         MVI   DLCBACT,DLCBEOR                                                  
         MVC   DLCBFLD,SPACES      MUST CLEAR FIRST TIME IN                     
         CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    DWNL50                                                           
         DC    H'0'                                                             
*                                                                               
* INITIALIZATION                                                                
*                                                                               
DWNL10   TM    DWNSTAT,DWNINTZ     HAS IT ALREADY BEEN INITIALIZED?             
         BO    DWNLX               YES - EXIT                                   
         MVI   DLCBACT,DLCBINIT    DOWN LOAD ACTION IS START                    
         LA    RE,XP               PRINT LINE                                   
         ST    RE,DLCBAPL                                                       
         LA    RE,DWNHOOK          POINT TO HOOK FOR APPLICATION                
         ST    RE,DLCBAPR                                                       
         MVC   DLCXMAXL,=Y(L'XP)                                                
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         GOTO1 DLFLD,(R5)                                                       
         MVI   FORCEHED,C'Y'       EXCEPT FIRST TIME IN                         
         GOTO1 ACREPORT                                                         
         MVC   DLCBFLD,SPACES      MUST CLEAR FIRST TIME IN                     
*                                  TURN OFF DOWN-LOAD ROW FLDS AS C' '          
         OI    DWNSTAT,DWNINTZ     TURN ON INITIALIZED BYTE                     
         B     DWNLX               EXIT                                         
*                                                                               
* DOWNLOAD A RECORD - TEXT                                                      
*                                                                               
DWNL20   MVC   DLCBLEN,PRTSIZE     LEN OF FIELD-SET TO 0 (NO PADDING)           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE   IS TEXT                               
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER                                                    
*                                                                               
DWNL30   MVI   DLCBLEN,0           LEN OF FIELD-SET TO 0 (NO PADDING)           
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBNUM     TYPE   IS NUMBER                             
         MVI   DLCBLEN,16          YES, USE MAXIMUM LENGTH OF NUMERICS          
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER (PACKED)                                           
*                                                                               
DWNL40   MVI   DLCBTYP,DLCBPACF    PACKED DATA                                  
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBLEN,L'PKFLDS    YES, USE MAXIMUM LENGTH OF NUMERICS          
         XC    DLCBFLD,DLCBFLD     CLEAN DWNLOAD FIELD TO 0'S                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         NC    DLCBFLD,DLCBFLD     YES, MAKE SURE NUMERIC FLD NOT ZEROS         
         BNZ   DWNL50              NOT  ZERO, DOWN-LOAD FIELD                   
         MVI   DLCBLEN,1           ZERO, SET LENGTH 1 TO DOWN-LOAD A 0          
*                                                                               
* END OF LINE/END OF RECORD                                                     
*                                                                               
DWNL50   GOTO1 DLFLD,(R5)          DOWN-LOAD FIELD                              
*                                                                               
DWNLX    XMOD1                                                                  
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HOOK                                                      *          
**********************************************************************          
         SPACE 1                                                                
DWNHOOK  MVI   FORCEHED,C'N'       NEVER HEAD UP                                
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         L     RF,ACREPORT                                                      
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   DS    0D                                                               
         NMOD1 0,**BINA**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,8(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,4(R1)            A(ITEM)                                      
BINA10   GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
*                                                                               
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
*                                                                               
*        C     R5,APSBATAB         ARE WE ADDING TO PSBATAB                     
*        BE    BINXIT              EXIT NO NEED TO UPDATE ANY BUCKETS           
*                                                                               
*BINA35   L     R4,DMCB             A(RECORD FOUND)                             
*                                                                               
*        CLI   (PSBSTYP-PSBKEY)(R4),X'01'                                       
*        BE    BINXIT              DON'T UPDATE PAYABLE BUCKETS                 
*                                                                               
*        SR    R0,R0                                                            
*        ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
*        BZ    BINXIT              NO BUCKETS - EXIT                            
*        SR    R6,R6                                                            
*        IC    R6,BINFST           DISPLACEMENT TO FIRST BUCKET                 
*        AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
*        AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
*INA40   AP    0(L'PSBABKT,R4),0(L'PSBABKT,R3)   ADD TO BUCKET                  
*        LA    R3,L'PSBABKT(R3)    BUMP TO NEXT ENTRY IN NEW ITEM               
*        LA    R4,L'PSBABKT(R4)    BUMP TO NEXT ENTRY IN TABLE                  
*        BCT   R0,BINA40                                                        
*                                                                               
         B     BINXIT                                                           
*                                                                               
BINXIT   XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS  BINADD NMOD1                                             *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRSEQ),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         L     R2,4(R1)                                                         
         LTR   R2,R2               DO WE HAVE DISK ADDRESS                      
         BNZ   DMGETR10            YES                                          
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         LA    R2,ACCKDA                                                        
DMGETR10 GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',(R2),IO,DMWORK                    
         B     DMX                                                              
*                                                                               
DMWRTDR  NMOD1 0,WRT               WRITE BACK TO DIR                            
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',IOKEY,IOKEY            
         B     DMX                                                              
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',ACCKDA,IO,DMWORK            
*                                                                               
DMX      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
*                                                                               
* BINTABLE 1 - SR TRANSACTION TABLE BUILT IN PROC TRANSACTION                   
*                                                                               
MAINTAB  DS    0F                                                               
         DC    C'**TSRACCT*'                                                    
TSRTAB   DS    0C                                                               
         DC    AL4(0)                  NUMBER IN TABLE                          
         DC    AL4(TSRALNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(TSRKLNQ)            KEY LENGTH                               
         DC    AL4(TSRAMAX)            MAX IN TABLE                             
         DC    AL1(TSRABKCT)           NUMBER OF BUCKETS                        
         DC    AL1(TSRABKT-TSRAD)      DISPLACEMENT TO FIRST BUCKET             
         DC    AL4(TSRASIZE)                                                    
*                                                                               
TSRAMAX  EQU   50000                                                            
         SPACE 1                                                                
*                                                                               
* BINTABLE 2 - SS ACCOUNT TABLE BUILT IN ACCOUNT LAST                           
*                                                                               
         DC    C'**PSSACCT*'                                                    
PSSATAB  DS    0C                                                               
         DC    AL4(0)                  NUMBER IN TABLE                          
         DC    AL4(PSSALNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(PSSAKLNQ)           KEY LENGTH                               
         DC    AL4(PSSAMAX)            MAX IN TABLE                             
         DC    AL1(PSSABKCT)           NUMBER OF BUCKETS                        
         DC    AL1(PSSABKT-PSSAD)      DISPLACEMENT TO FIRST BUCKET             
         DC    AL4(PSSASIZE)                                                    
*                                                                               
*MAINNUM  EQU   (*-MAINTAB)/MAINLNQ NUMBER OF BINARY TABLES                     
*                                                                               
*SSAMAX  EQU   99000                                                            
*                                                                               
* BINTABLE 3 - SS ACCOUNT TABLE WITH 06 ENTRIES ONLY                            
*                                                                               
         DC    C'**PSBACCT*'                                                    
PSBATAB  DS    0C                                                               
         DC    AL4(0)                  NUMBER IN TABLE                          
         DC    AL4(PSBALNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(PSBAKLNQ)           KEY LENGTH                               
         DC    AL4(PSBAMAX)            MAX IN TABLE                             
         DC    AL1(PSBABKCT)           NUMBER OF BUCKETS                        
         DC    AL1(PSBABKT-PSBTABD)    DISPLACEMENT TO FIRST BUCKET             
         DS    (PSBASIZE)XL1                                                    
*                                                                               
         SPACE 1                                                                
TSRASIZE EQU   (TSRAMAX*TSRALNQ)                                                
LENBUFF1 EQU   TSRASIZE                                                         
LENBUFF  EQU   LENBUFF1+LENBUFF2                                                
*                                                                               
*                                  TABLE OF LEDGER AND SYSTEM                   
LDSYTAB  DC    C'SS'               LEDGER S - SYS SPOT (BUT MAYBE NET)          
         DC    C'TS'               LEDGER T - SYS SPOT                          
         DC    C'PP'               LEDGER P - SYS PRINT                         
         DC    C'QP'               LEDGER Q - SYS PRINT                         
         DC    C'UN'               LEDGER U - SYS NET                           
         DC    X'FF'                                                            
         SPACE 2                                                                
*                                                                               
         DS    0D                  DOWNLOAD BUFFER                              
DWNBUF   DS    CL250                                                            
*                                                                               
         DC    C'**OUTBUF**'                                                    
         DS    0D                  DOWNLOAD OUTPUT BUFFER                       
OUTBUF   DS    CL(PLINLN1Q)                                                     
*                                                                               
***********************************************************************         
* HEADLINE TABLES                                                     *         
***********************************************************************         
         SPACE 1                                                                
HD1TAB   DS    0CL30               FIRST HEADLINE TABLE                         
         DC    CL30'S'                                                          
         DC    CL30'M'                                                          
         DC    CL30'CLT'                                                        
         DC    CL30'PRD'                                                        
         DC    CL30'EST'                                                        
         DC    CL30'MOS'                                                        
         DC    CL30'ACCOUNT (RCV/PAYEE)'                                        
         DC    CL30'PAYEE NAME'                                                 
         DC    CL30'INVOICE # (CLT/VDR)'                                        
         DC    CL30'INVOICE DATE (CLT/VDR)'                                     
         DC    CL30'CLT INVOICE DUE DATE'                                       
         DC    CL30'CLT DEPOSIT DATE'                                           
         DC    CL30'VENDOR CODE'                                                
         DC    CL30'VENDOR NAME'                                                
         DC    CL30'CLEARED DATE'                                               
         DC    CL30'NET BILLED'                                                 
         DC    CL30'CASH RECEIVED'                                              
         DC    CL30'% RECEIVED'                                                 
         DC    CL30'CHECKS DISBURSED'                                           
         DC    CL30'CASH POSITION (RECEIVED-DISB)'                              
         DC    CL30'TOTAL CLEARED'                                              
         DC    CL30'CLEARED BUT UNDISBURSED'                                    
         DC    CL30'CASH AVAILABLE (CASH-APPROVED)'                             
         DC    CL30'TRANSACTION STATUS'                                         
         DC    CL30'APPROVAL STATUS'                                            
HD1LNQ   EQU   (*-HD1TAB)/L'HD1TAB                                              
*                                                                               
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
STATTAB  DS    0C                                                               
         DC    XL1'00',XL1'80',CL1'N',CL1'N',CL6'AUTO  ',CL2'A'                 
         DC    XL1'00',XL1'40',CL1'N',CL1'Y',CL6'      ',CL2' '                 
         DC    XL1'00',XL1'80',CL1'Y',CL1'Y',CL6'      ',CL2' '                 
         DC    XL1'00',XL1'40',CL1'Y',CL1'Y',CL6'      ',CL2' '                 
         DC    XL1'00',XL1'FF',CL1' ',CL1'Y',CL6'      ',CL2' '                 
*                                                                               
         DC    XL1'01',XL1'80',CL1'N',CL1'Y',CL6'      ',CL2' '                 
         DC    XL1'01',XL1'40',CL1'N',CL1'N',CL6'MANUAL',CL2'P'                 
         DC    XL1'01',XL1'80',CL1'Y',CL1'N',CL6'      ',CL2'P'                 
         DC    XL1'01',XL1'40',CL1'Y',CL1'N',CL6'MANUAL',CL2'P'                 
         DC    XL1'01',XL1'FF',CL1' ',CL1'N',CL6'      ',CL2'P'                 
*                                                                               
         DC    XL1'02',XL1'80',CL1'N',CL1'Y',CL6'      ',CL2' '                 
         DC    XL1'02',XL1'40',CL1'N',CL1'N',CL6'MANUAL',CL2'U'                 
         DC    XL1'02',XL1'80',CL1'Y',CL1'N',CL6'AUTO  ',CL2'A'                 
         DC    XL1'02',XL1'40',CL1'Y',CL1'N',CL6'MANUAL',CL2'U'                 
         DC    XL1'02',XL1'FF',CL1' ',CL1'N',CL6'      ',CL2'U'                 
*                                                                               
         DC    XL1'03',XL1'FF',CL1' ',CL1'N',CL6'      ',CL2'H'                 
         DC    XL1'04',XL1'FF',CL1' ',CL1'N',CL6'      ',CL2' '                 
         DC    X'FF'                                                            
*                                                                               
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACABD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
VTYPES   DS    0A                                                               
ADWNL    DS    A                   DOWNLOAD ROUTINE                             
ADWNBUF  DS    A                   DOWNLOAD BUFFER                              
AOUTBUF  DS    A                   DOWNLOAD OUTPUT BUFFER                       
ADWNRTE  DS    A                   DOWNLOAD TOTALS ROUTINE                      
AHD1TAB  DS    A                   FIRST HEADLINE DWNLD FIELDS TABLE            
ASTATTAB DS    A                   CASH AVAILABILITY STATUS TABLE               
ATSRTAB  DS    A                   SS TRANSACTION TABLE                         
APSSATAB DS    A                   SS ACC TABLE BUILT FROM FILE                 
APSBATAB DS    A                   SS ACC TABLE 06 ENTRIES ONLY                 
ASETMOS  DS    A                   SETS START AND END MOS DATES                 
ADUMP    DS    A                   ROUTINE TO DITTO RECORDS                     
AMAINTAB DS    A                   ADDRESS OF MAIN TABLE                        
ALDSYTAB DS    A                   LEDGER SYSTEM TABLE                          
ABFRIN   DS    A                   ADDRESS OF TSAR MAINTENANCE ROUTINE          
ABINADD  DS    A                   ROUTINE TO ADD TO BINSEARCH TABLE            
AUTOAB   DS    V                   AUTO APPROVE MODULE                          
APRNTBL  DS    V                   PRINT DATA                                   
BINSRC31 DS    V                   BINSRCH (31 BIT MODE)                        
VHELLO   DS    V                                                                
ACLIST   DS    V                                                                
VBUFFRIN DS    V                   ADDRESS OF BUFFERIN                          
DLFLD    DS    V                   DOWNLOAD MODULE                              
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
VSSB     DS    V                   NEEDED FOR SOON                              
AELMNTS  DS    0A                                                               
AGDAELD  DS    A                   X'E5'                                        
AMBIELD  DS    A                   X'F3'                                        
AOTHELD  DS    A                   X'23'                                        
AXPYELD  DS    A                   X'46'                                        
AMDTELD  DS    A                   X'1A'                                        
AMDPELD  DS    A                   X'6A'                                        
AELMNTSQ EQU   *-AELMNTS                                                        
*                                                                               
         DS    0D                                                               
REQPCNT  DS    PL8        IF THIS % OF UNAPPROVED IS AVL THEN AUTO APPR         
DSKADDR  DS    XL4                 DISK ADDRESS OF CURRENT TRANSACTION          
DISP2    DS    H                                                                
PTOTAPP  DS    PL8        TOTALS APPROVED IN THIS RUN.                          
PTOTPRV  DS    PL8        PREVIOUSLY APPROVED TOTALS.                           
***********************************************************************         
* ORDER OF THE NEXT FOUR BUCKET TABLES SHOULD NOT CHANGE OR PROGRAM   *         
* WILL FAIL CLITBKT-> PROTBKT-> SYSTBKT-> MEDTBKT.                    *         
***********************************************************************         
         DC    C'****CLI TOT TAB****'                                           
CLITBKT  DS    0PL8            ****CLIENT TOTALS BUCKET****                     
CLIBILLB DS    PL8         TOTAL   AMOUNT BILLED                                
CLITBKLN EQU   *-CLITBKT           BUCKET LENGTH                                
CLIRCVDB DS    PL8         TOTAL   CASH RECEIVED                                
CLIPCNT  DS    PL8         TOTAL   PERCENT                                      
CLIDISBB DS    PL8         TOTAL   DISBURSED AMOUNT                             
CLICASHB DS    PL8         TOTAL   CASH POSITION                                
CLICLRB  DS    PL8         TOTAL   CLEARED AMOUNT TRANSACTION AMOUNT            
CLIUDISB DS    PL8         TOTAL   ALL OF UNDISBURSED AMOUNT                    
CLIAVLB  DS    PL8         TOTAL   CASH AVAILABLE TO APPROVE                    
CLITBKCT EQU   (*-CLITBKT)/CLITBKLN NUMBER OF BUCKETS                           
*                                                                               
PROTBKT  DS    (CLITBKCT)PL8   ****PRODUCT TOTALS BUCKET****                    
SYSTBKT  DS    (CLITBKCT)PL8   ****SYSTEM TOTALS BUCKET****                     
MEDTBKT  DS    (CLITBKCT)PL8   ****MEDIA TOTALS BUCKET****                      
***********************************************************************         
*                                                                               
LASTS    DS    0X                  **LAST TIME VALUES**                         
LSTSYS   DS    CL1                 LAST SYSTEM CODE                             
LSTMED   DS    CL1                 LAST MEDIA CODE                              
LSTCLI   DS    CL3                 LAST CLIENT                                  
LSTPRO   DS    CL3                 LAST PRODUCT                                 
LSTEST   DS    CL6                 LAST ESTIMATE                                
LSTMOS   DS    XL2                 LAST MOS                                     
LSTSTN   DS    CL14                LAST STATION                                 
LSTSYSL  EQU   *-LSTSYS                                                         
*                                                                               
LASTL    EQU   *-LASTS                                                          
*                                                                               
BCFLAG1  DS    XL1                                                              
BCFULL   DS    F                                                                
BCDUB    DS    D                                                                
BCTSERRS DS    CL1                 TEMP TSAR ERRORS                             
*                                                                               
PROFILES DS    0XL16                                                            
PROF1    DS    XL1                                                              
PROF2    DS    XL1                                                              
PROF3    DS    XL1                                                              
PROF4    DS    XL1                                                              
PROF5    DS    XL1                                                              
PROF6    DS    XL1                                                              
PROF7    DS    XL1                                                              
PROF8    DS    XL1                                                              
PROF9    DS    XL1                                                              
PROF10   DS    XL1                                                              
PROF11   DS    XL1                                                              
PROF12   DS    XL1                                                              
PROF13   DS    XL1                                                              
PROF14   DS    XL1                                                              
PROF15   DS    XL1                                                              
PROF16   DS    XL1                                                              
*                                                                               
MSG      DS    CL10                DUMP MESSAGE                                 
ELCODE   DS    CL1                                                              
ELEM     DS    CL255                                                            
TODAYP   DS    PL3                 TODAY'S DATE-PACKED                          
CURTIME  DS    XL3                 CURRENT TIME                                 
TODAY3   DS    XL3                                                              
*                                                                               
RUNOPT   DS    XL1                                                              
SOONRUN  EQU   X'80'                RUNNING SOON                                
TESTRUN  EQU   X'40'                RUN=TEST IS SET                             
UPSI     DS    XL1                                                              
KPFACWK  EQU   X'80'                KEEP WORKER FILE                            
DSPNIOS  EQU   X'40'                DISPLAY NO. OF IOS (BUFFERIN)               
*                                                                               
USID     DS    XL(L'FWRUSID)        ID                                          
PQID     DS    XL(L'FWRPQID)        PQID                                        
LUID     DS    XL(L'FWRLUID)        LUID                                        
*                                                                               
FLAG     DS    XL1                                                              
FLGPRNT  EQU   X'10'               MARK SOMETHING WAS PRINTED                   
FLGHIGH  EQU   X'08'               PRINTING TOTAL BUCKETS                       
FLGPRN03 EQU   X'01'               PRINT FULLY DISB MOS TOTAL LINE              
*                                                                               
FLAG1    DS    XL1                                                              
FLGSTN   EQU   X'08'               SKIPPED PRINTING STATION                     
FLAG2    DS    XL1                                                              
FLGTOTZR EQU   X'10'               ALL TOTAL AMOUNT FIELD ZERO                  
*                                                                               
SVKEY    DS    CL42                                                             
*                                                                               
SVCLOGO  DS    CL7                 COMPANY'S LOGIN ID (LOGO)                    
*                                                                               
SVMOS    DS    XL3                                                              
DTE1     DS    CL6                 YYMMDD (EBCDIC)                              
DTE2     DS    CL6                 YYMMDD (EBCDIC)                              
MMOSSTR  DS    XL3                 MEDIA MONTH OF SVC START FROM REQ            
MMOSEND  DS    XL3                 MEDIA MONTH OF SVC END FROM REQ              
START    DS    XL3                                                              
END      DS    XL3                                                              
PRTSTART DS    CL6                                                              
PRTEND   DS    CL6                                                              
LASTYEAR DS    XL3                 MMOSSTR-(ONE YEAR)                           
*                                                                               
CHOPWRK  DS    CL120               WORK AREA FOR SQUASHER                       
TSRAWRK  DS    CL(TSRALNQ)      BINSEARCH WORK AREA - SS TRNS TABLE             
PSSAWRK  DS    CL(PSSALNQ)      BINSEARCH WORK AREA - SS TABLE                  
SVPSSWRK DS    CL(L'PSSAWRK)    BINSEARCH WORK AREA - SS TABLE                  
SVVENDOR DS    CL(PS6KLNQ)      SAVED VLAUES SYS/MED... VENDOR                  
PSBAWRK  DS    CL(PSBALNQ)      STATION/PAYABLE LEVEL ENTRY                     
TEMPWRK  DS    CL255               TSAR WORK AREA                               
BUFFREC  DS    A                   BUFFER S/R A(RECORD)                         
BUFFRET  DS    X                   BUFFER S/R RETURN INDICATORS                 
TSARKSAV DS    XL(PSSALNQ)         TSAR RECORD KEY SAVE AREA                    
*                                                                               
DWNFLD   DS    CL36                SAVED AREA FOR FIELD TO BE DWNLOADED         
PRTSIZE  DS    CL1                 DOWNLOAD FLD PRINT SIZE FOR PADDING          
*                                                                               
DWNSTAT  DS    XL1                 DOWNLOAD STATUS                              
DWNINTZ  EQU   X'80'               DOWNLOAD INITIALIZED                         
DWNHDLN  EQU   X'40'               DOWNLOAD HEADLINES                           
*                                                                               
DWNMODE  DS    XL1                 DOWNLOAD MODE                                
DWNINIT  EQU   1                      DOWN-LOAD INITIALIZATION                  
DWNEOL   EQU   2                      MARK END OF LINE                          
DWNEOR   EQU   3                      MARK END OF REPORT                        
DWNTEXT  EQU   4                      DOWN-LOAD TEXT                            
DWNNUM   EQU   5                      DOWN-LOAD NUMBER                          
DWNPACK  EQU   6                      DOWN-LOAD NUMBER (PACKED)                 
PKFLDS   DS    PL8                 PACKED FIELDS FOR CALCULATIONS               
         DS    0D                                                               
SVDMCB   DS    0XL24                                                            
SVDM1    DS    F                                                                
SVDM2    DS    F                                                                
SVDM3    DS    F                                                                
SVDM4    DS    F                                                                
SVDM5    DS    F                                                                
SVDM6    DS    F                                                                
*                                                                               
SVBXHOOK DS    F                                                                
* ACAUTOAPD                                                                     
       ++INCLUDE ACAUTOAPD                                                      
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
*                                                                               
         DS    0F                                                               
AAEXTRA  DS    XL(AUTBLKXLQ)       EXTRA PARAMS FOR AUTBLK (AUTEXTRA)           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINTING STATUSES FOR APPROVED,UNAPPROVED,HELD ETC        *         
***********************************************************************         
STATTABD DSECT                                                                  
STATRTYP DS    XL1                 RECORD TYPE PSSKSTAT                         
STATCSH  DS    XL1                 CASH AVAILABILITY X'80'=AVLBL                
STATOPT  DS    CL1                 QOPT2 =Y (BREAK DOWN RECTYPE=00)             
STATSKIP DS    CL1                 SKIP THIS RECORD Y=YES,N=NO                  
STATMRK  DS    CL6                 MARK AUTO,MANUAL ETC                         
STATMRKA DS    CL2                 MARK STATUS A=APPROVED,U=UNAPPROV            
*                                  P=PREVIOUSLY APPROVED,H=HELD                 
STATLNQ  EQU   *-STATTABD                                                       
***********************************************************************         
* DSECT FOR INTERNAL 06 TYPES WITH VENDOR HIGH                        *         
***********************************************************************         
PSBTABD  DSECT                                                                  
PSBKEY   DS    0C                                                               
PSBTYP   DS    CL1                 06=SS DLTS WITH VENDOR HIGH                  
PSBSTYP  DS    CL1                 00=VENDOR/SR, 01=VENDOR/PAYABLE              
*                                                                               
PSBKSYS  DS    CL1                 SYSTEM                                       
PSBKMED  DS    CL1                 MEDIA/SUBMEDIA                               
PSBKCLI  DS    CL3                 CLEINT FROM CONTRA OF SS                     
PSBKPRO  DS    CL3                 PRODUCT FROM 1ST 3 BYTES OF REF              
PSBKEST  DS    CL6                 ESTIMATE                                     
PSBKRTYP DS    X                   SPARE                                        
*                                                                               
PSBKMOS  DS    PL2                 TRANSACTION MOS PACKED (YYMM)                
PSBKVEN  DS    CL12                VENDOR (STATION, PUB, ETC)                   
*                                                                               
PSBSRAC  DS    CL14                CORRESPONDING SR ACCOUNT                     
PSBSRCAC DS    CL14                SR CONTRA ACCOUNT                            
PSBSRBNO DS    CL6                 SR BILL NUMBER                               
PSBSRBDA DS    PL3                 SR BILL DATE                                 
*                                                                               
         ORG   PSBSRAC                                                          
PSBKSTN  DS    CL14                SS ACCOUNT (STATION)                         
PSBKINV  DS    CL11                INVOICE NUMBER                               
         ORG                                                                    
*                                                                               
*                                                                               
PSBKSTAT DS    XL1                 STATUS ONLY ON PSBKRTYP=05                   
PSBKTOT  EQU   X'00'               ALL OF APPROVED+UNAPPROVED                   
PSBKPREV EQU   X'01'               PREVIOUSLY APPROVED                          
PSBKUNAP EQU   X'02'               UNAPPROVED                                   
PSBKHELD EQU   X'03'               HELD                                         
PSBKDISB EQU   X'04'               DISBURSED ITEMS                              
*                                                                               
PSBKDA   DS    XL4                 DISK ADDRESS IN KEY TO EXPAND                
*                                                                               
PSBKLNQ1 EQU   *-PSBKRTYP                                                       
PSBKLNQ2 EQU   *-PSBKSTAT          STATION ONWARDS                              
*                                                                               
PSBAKLNQ EQU   *-PSBTABD           LENGTH OF KEY                                
*                                                                               
* DATA FIELDS                                                                   
*                                                                               
PSBFLAG  DS    XL1                 SOME FLAGS                                   
PSBCSH   EQU   X'80'               CASH IS AVAILABLE TO AUTO APPROVE            
PSBMAN   EQU   X'40'               CASH AVLBL FOR MANUAL APPROVAL               
PSBDISB  EQU   X'20'               DISBURSED ONLY TABLE ENTRIES                 
PSBPCNT  EQU   X'10'               APPLYING PERCENT TO APPROVE                  
PSBBZERO EQU   X'04'               PRINT ZERO IF BILLED=0                       
PSBRZERO EQU   X'02'               PRINT ZERO IF APPLIED=0                      
PSBPREV  EQU   X'01'               UNDISB ITEM OLDER THAN REQUESTED MOS         
PSBFLAG1 DS    XL1                 SOME FLAGS                                   
PSBFLNQ  EQU   *-PSBFLAG                                                        
*                                                                               
PSBVNAM  DS    CL24                VENDOR NAME                                  
PSBPNAM  DS    CL24                PAYEE NAME                                   
PSBINVD  DS    PL3                 INVOICE DATE                                 
PSBINVCD DS    PL3                 INVOICE CLEARED DATE                         
*                                                                               
PSBSRDDA DS    PL3                 CLIENT INVOICE DUE DATE                      
PSBSRDPD DS    PL3                 CLIENT DEPOSIT DATE                          
*                                                                               
PSBKLNQ3 EQU   *-PSBKSTN                                                        
PSBKLVENFWQ EQU *-PSBKVEN          LENGTH OF KEY VENDOR AND FORWARD             
PSBKLSTNFWQ EQU *-PSBKSTN          LENGTH OF KEY SS ACC AND FORWARD             
*                                                                               
* BUCKETS                                                                       
*                                                                               
PSBABKT  DS    0PL8                BUCKET                                       
*                                                                               
PSBBILLB DS    PL8                 AMOUNT BILLED                                
PSBABKLN EQU   *-PSBABKT           BUCKET LENGTH                                
PSBRCVDB DS    PL8                 CASH RECEIVED                                
PSBPBKT  DS    0PL8                SPECIAL FOR PRINTING                         
PSBRPCNT DS    PL8                 PERCENTAGE RECEIVED FOR SR ENTRY             
PSBDISBB DS    PL8                 DISBURSED AMOUNT                             
PSBCASHB DS    PL8                 CASH POSITION                                
PSBCLRB  DS    PL8                 CLEARED AMOUNT TRANSACTION AMOUNT            
PSBUDISB DS    PL8                 ALL OF UNDISBURSED AMOUNT                    
PSBAVLB  DS    PL8                 CASH AVAILABLE TO APPROVE                    
PSBPBKCT EQU   (*-PSBPBKT)/PSBABKLN NUMBER OF BUCKETS TO PRINT                  
PSBBKCT2 EQU   (*-PSBABKT)/PSBABKLN NUMBER OF BUCKETS TO PRINT                  
*                                  NEXT THREE ARE FOR CALCULATIONS              
PSBAPPRB DS    PL8                 APPROVED AMOUNT                              
PSBHELDB DS    PL8                 HELD AMOUNT                                  
PSBUNAPB DS    PL8                 UNAPPROVED AMOUNT                            
*                                                                               
PSBABKCT EQU   (*-PSBABKT)/PSBABKLN NUMBER OF BUCKETS                           
PSBALNQ  EQU   *-PSBTABD           LENGTH OF ENTRY                              
*                                                                               
* BUFFER SIZE EQUATES                                                           
*                                                                               
PSBAMAX  EQU   2000                                                             
PSBASIZE EQU   (PSBAMAX*PSBALNQ)                                                
LENBUF2  EQU   PSBASIZE                                                         
*                                                                               
*                                                                               
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLINE  DS    0C                                                               
         DS    CL2                                                              
PSYS     DS    CL1                 SYSTEM PSSKSYS                               
         DS    CL1                                                              
PMED     DS    CL1                 MEDIA  PSSKMED                               
         DS    CL1                                                              
PCLI     DS    CL3                 CLIENT CODE PSSKCLI                          
         DS    CL1                                                              
PPRO     DS    CL3                 PRODUCT CODE PSSKPRO                         
         DS    CL1                                                              
PEST     DS    CL6                 ESTIMATE NUMBER PSSKEST                      
         DS    CL1                                                              
PMOS     DS    CL6                 MONTH OF SERVICE PSSKMOS                     
         DS    CL1                                                              
PSTA     DS    CL14                STATION SS/SP/SU PSSKSTN                     
         DS    CL1                                                              
PINV     DS    CL11                INVOICE NUMBER PSSKINV                       
         DS    CL1                                                              
*                                                                               
PBUCKET  DS    0CL17                                                            
*                                                                               
PBILLED  DS    CL16                BILLED AMOUNT FROM SR PSSBILLB               
PBUCLEN  EQU   *-PBUCKET                                                        
         DS    CL1                                                              
PRCVD    DS    CL16                CASH RECEIVED FROM SR PSSRCVDB               
         DS    CL1                                                              
PBUCKET2 DS    0CL17                                                            
PPCNT    DS    CL16                SR PERCENTAGE APPLIED                        
         DS    CL1                                                              
PDISB    DS    CL16                DISBURSED AMOUNT PSSDISBB                    
         DS    CL1                                                              
PCASH    DS    CL16                CASH AVAILABLE PSSCASHB                      
         DS    CL1                                                              
PCLEAR   DS    CL16                CLEARD AMOUNT PSSCLRB                        
         DS    CL1                                                              
PUNDISB  DS    CL16                APPROVED AMMOUNT PSSUDISB                    
PMARKA   DS    CL2                 MARK ITEMS CURRENTLY APPROVING 'A'           
         DS    CL1                                                              
PAVAIL   DS    CL16                CASH AVAILABLE TO APPROVE PSSAVLB            
         DS    CL1                                                              
PUNAPPR  DS    0CL16               UNAPPROVED AMOUNT                            
         DS    CL1                                                              
*                                                                               
PLINELNQ EQU   *-PRTLINE                                                        
         DS    CL1                                                              
PMARK    DS    CL6                                                              
PINVDATE DS    CL8       MM/DD/YY  INV DATE (CLT/VDR) PSSSRBDA/PSSINVD          
PDEPDATE DS    CL8                 CLT DEPOSIT DATE PSSSRDPD                    
PINVDUE  DS    CL8                 CLT INV DUE DATE PSSSRDDA                    
PINVCDAT DS    CL8                 INVOICE CLEARED DATE   PSSINVCD              
PVENCODE DS    CL12                VENDOR CODE PSSKVEN                          
PVENNAME DS    CL24                VENDOR NAME PSSVNAM                          
PPAYNAME DS    CL24                PAYEE NAME  PSSPNAM                          
PDISKADD DS    CL8                 DISK ADDRESS                                 
PLINLN1Q EQU   *-PRTLINE                                                        
*                                                                               
         ORG   PLINED                                                           
         DS    CL2                                                              
PTOTMSG  DS    CL30                                                             
         DS    CL2                                                              
PTOT     DS    CL16                                                             
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN SS TRANSACTION TABLE                             *         
***********************************************************************         
         SPACE 1                                                                
TSRAD    DSECT                                                                  
TSRKEY   DS    0CL36                                                            
TSRKSYS  DS    CL1                                                              
TSRKACT  DS    CL14                SR ACCOUNT                                   
TSRKCAC  DS    CL14                SR CONTRA U/L/AC                             
TSRKBNO  DS    CL6                 SR REF/BILL NO.                              
TSRKBDA  DS    PL3                 SR TRNDATE/BILL DATE                         
TSRKCLI  DS    CL3                 CLEINT FROM CONTRA OF SS                     
TSRKPRO  DS    CL3                 PRODUCT FROM 1ST 3 BYTES OF REF              
         DS    CL1                 SPARE                                        
TSRKEST  DS    CL6                 ESTIMATE                                     
TSRKRTYP DS    X                   REC TYPE 00=BILL#TOTALS,01=EST TOTS          
*                                  02=DETAILS BY MOS                            
TSRKMOS  DS    PL2                 TRANSACTION MOS PACKED (YYMM)                
TSRKLNQ2 EQU   *-TSRKRTYP                                                       
TSRFLAG  DS    XL1                 FLAG IN KEY PART WILL MAKE SURE THAT         
*                                  DR IS ALWAYS AFTER CR IN TABLE               
TSRFLGDR EQU   X'80'               DEBIT TRANSACTION                            
TSRKLNQ1 EQU   *-TSRKCLI                                                        
TSRKLNQ  EQU   *-TSRAD             LENGTH OF KEY                                
*                                                                               
TSRABKT  DS    0PL8                BUCKET                                       
*                                                                               
TSRADR   DS    PL8                 DEBIT AMOUNT                                 
TSRABKLN EQU   *-TSRABKT           BUCKET LENGTH                                
TSRACR   DS    PL8                 CREDIT AMOUNT                                
TSRAPCNT DS    PL8                 CALCULATED PERCENT                           
TSRAWRTF DS    PL8                 WRITE OFF                                    
TSRATXFR DS    PL8                 TRANSFER                                     
TRAOFFST DS    PL8                 OFFSET                                       
*                                                                               
TSRABKCT EQU   (*-TSRABKT)/TSRABKLN NUMBER OF BUCKETS                           
TSRALNQ  EQU   *-TSRAD             LENGTH OF ENTRY                              
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
* ACABAUTOD                                                                     
       ++INCLUDE ACABAUTOD                                                      
* ACBIGPRINTD                                                                   
       ++INCLUDE ACBIGPRNTD                                                     
* ACREPWORKD                                                                    
       ++INCLUDE ACREPWORKD                                                     
         ORG   QSELECT                                                          
QCLI     DS    CL3                 CLIENT                                       
QPROD    DS    CL3                 PRODUCT                                      
         ORG                                                                    
         ORG   QAPPL                                                            
QTOLRNCE DS    XL2     TOLERANCE LEVEL, BINARY, 2 DECIMALS 0.01-10.00%          
*                      E.G. 0.01% = X'0001', 9.55% = X'03BB', ETC               
QEST     DS    CL6                 ESTIMATE                                     
QMED     DS    CL1                 MEDIA CODE OR '+' FOR SYSTEM                 
QSYSTEM  DS    CL1                 SYTEM S/N/P OR BLANK                         
         ORG                                                                    
* ACGENFILE                                                                     
       ++INCLUDE ACGENFILE                                                      
* ACGENMODES                                                                    
       ++INCLUDE ACGENMODES                                                     
* DDLOGOD                                                                       
       ++INCLUDE DDLOGOD                                                        
* ACMASTD                                                                       
       ++INCLUDE ACMASTD                                                        
* DDMASTD                                                                       
       ++INCLUDE DDMASTD                                                        
* DDBIGBOX                                                                      
       ++INCLUDE DDBIGBOX                                                       
* DDBOXEQUS                                                                     
       ++INCLUDE DDBOXEQUS                                                      
* ACREPPROFD                                                                    
       ++INCLUDE ACREPPROFD                                                     
* ACQD                                                                          
       ++INCLUDE ACQD                                                           
*SRUPDD                                                                         
       ++INCLUDE SRUPDD                                                         
*                                                                               
*FASSBOFF                                                                       
       ++INCLUDE FASSBOFF                                                       
*                                                                               
* DDBUFFD                                                                       
       ++INCLUDE DDBUFFD                                                        
*                                                                               
* DDDLCB                                                                        
       ++INCLUDE DDDLCB                                                         
*                                                                               
* AUTBLKX                                                                       
       ++INCLUDE ACAUTOAPDX                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPAB02 03/24/20'                                      
         END                                                                    
