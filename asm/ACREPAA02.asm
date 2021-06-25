*          DATA SET ACREPAA02  AT LEVEL 030 AS OF 03/24/20                      
*PHASE ACAA02C,*                                                                
*INCLUDE AUTOAPP                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE BUFFERIN                                                               
*INCLUDE BINSR31                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE ACLIST                                                                 
         TITLE 'AUTO APPROVE PAYABLES'                                          
**********************************************************************          
* PROGRAM OPTIONS :                                                  *          
*                                                                    *          
*         QOPT1 - 'Y'  DISPLAY SR ACCOUNT DETAILS                    *          
*         QOPT2 - 'Y'  EXPAND MULTIPLE TRANSACTIONS                  *          
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
*RGUP 028 10JUL18  <SPEC-20692> NEED ADDITIONAL MEDIA FOR DIGIAL AUDIO          
*RKEJ 030 28FEB20  <SPEC-30973> SUPPORT DOLLAR TOLERANCE                        
**********************************************************************          
ACAA02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACAA**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACAAD,RC                                                         
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
         XC    AAEXTRA,AAEXTRA                                                  
*                                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
         L     R2,ABOXRC                     SET UP BOX ROUTINE                 
         ST    RC,0(R2)                                                         
         L     R2,ABXHOOK                                                       
         ST    R2,HEADHOOK                                                      
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
         CLI   QOPT2,C'Y'                                                       
         BE    *+8                                                              
         MVI   QOPT2,C'N'                                                       
         MVI   FLAG,0              INITIALIZE FLAG                              
         MVI   RCSUBPRG,1                                                       
*                                                                               
         ZAP   REQPCNT,=P'1000000' DEFAULT IS FULLY MATCH                       
*                                                                               
         L     R7,APROFILE                                                      
         USING ACPROFSD,R7                                                      
*                                                                               
REQF05   CLI   QOPT10,C' '                                                      
         BNE   REQF07                                                           
         MVC   QOPT10,ACPPFC06     USE IT FROM PROFILE                          
REQF07   CLI   QOPT10,C'Y'         AA PROFILE, "USE TOLERANCE" FIELD            
         BE    REQF08                                                           
         CLI   QOPT10,C'D'                                                      
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
*        CLC   QTOLRNCE,=X'044B'        CAN'T BE MORE THAN 10.99%               
         CLC   QTOLRNCE,=X'63FF'        CAN'T BE MORE THAN 255.99$              
         JH    *+2                      SOMETHING IS WRONG                      
*                                                                               
         LHI   RF,10000                 100.00%                                 
         XR    R0,R0                                                            
         ICM   R0,3,QTOLRNCE            RANGE: 0.00-10.00%                      
         CLI   QOPT10,C'Y'              PERCENTAGE                              
         BE    REQF15                                                           
         CLI   QOPT10,C'D'              DOLLAR                                  
         BNE   REQF15                                                           
REQF13   CVD   R0,REQPCNT                                                       
         B     REQF50                                                           
*                                                                               
REQF15   SR    RF,R0                    100%-QTOLRNCE                           
         CVD   RF,REQPCNT               90.00% -> X'09000C' PACKED              
         SRP   REQPCNT(8),2,0           MAKE IT 4 DECIMAL DIGITS                
*        PACK  REQPCNT(8),QTOLRNCE(2)   REQPCNT=100C IF 100%                    
*        SRP   REQPCNT(8),4,0           MAKE IT 10000C                          
*                                                                               
REQF50   CLC   QOPT10,ACPPFC06     HAS USE TOLERANCE CHANGED?                   
         BE    REQF60                                                           
*        CLI   ACPPFC06,C'N'                                                    
*        BE    REQF60                                                           
         B     ERRINCST            INCONSISTENT WITH PROFILE                    
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
         L     R3,ADQSTACK                                                      
         USING ACQD,R3                                                          
         MVC   WORK(4),ACQDTEND                                                 
         MVC   WORK+4(2),=C'01'                                                 
*                                                                               
         L     R7,APROFILE                                                      
         USING ACPROFSD,R7         PROFILES PASSED BY MONACC                    
         LLC   R0,ACPPFC04                                                      
         DROP  R7                                                               
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
         L     R7,APROFILE                                                      
         USING ACPROFSD,R7                                                      
         MVC   PROFILES,ACPPFCP1   PROFILES PASSED BY MONACC                    
         DROP  R7                                                               
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
*                                                                               
         GOTO1 AUTOAPP,AUTBLK      BUILD PAYABLE AND APPLY CASH                 
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
         BAS   RE,PRNBUFF          PRINT BUFFERS AND IOS ETC                    
         BAS   RE,MARKIT           MARK ITEMS APPROVED                          
         BAS   RE,PRTOTAL          PRINT TOTALS PAGE                            
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
         BRAS  RE,CLEARXP          CLEAR XP TO SPACES                           
*                                                                               
         LA    R0,4                                                             
         LA    R1,CLITBKT                                                       
         BRAS  RE,ZAPTOTBK         CLEAR SYS/MED/CLI/PRD TOT BUCKETS            
         LA    R1,CLITBKCT*CLITBKLN(R1) BUMP TO NEXT TOTALS ENTRY               
         BCT   R0,*-8                                                           
*                                                                               
         XC    LASTS(LASTL),LASTS  INITIALIZE LAST TIME VALUES                  
         NI    FLAG,X'FF'-FLGPRNT                                               
         LA    R5,XP                                                            
*                                                                               
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
         BRAS  RE,PSMCPTOT          PRINT SYS/MED/CLI/PROD TOTALS               
         MVI   RCSUBPRG,2                                                       
         B     PRNL40                                                           
PRNL30   CLI   RCSUBPRG,3          BAD ESTIMATE                                 
         BE    PRNL50                                                           
         MVI   RCSUBPRG,3                                                       
PRNL40   MVI   FORCEHED,C'Y'       INITIALIZE FOR FIRST TIME PRINTING           
         BRAS  RE,CLEARXP          CLEAR XP TO SPACES                           
         XC    LASTS(LASTL),LASTS  INITIALIZE LAST TIME VALUES                  
         NI    FLAG,X'FF'-FLGPRNT                                               
*                                                                               
PRNL50   CLI   PSSKRTYP,X'00'      ARE WE AT A NEW ENTRY                        
         BNE   *+8                                                              
         NI    FLAG,X'FF'-FLGPRN03 DONT WANT TO PRNT FULLY DISB 03              
         CLI   PSSKRTYP,X'03'      RESET IF YOU GET FUTURE OR DETL RECD         
         BNH   *+8                                                              
         NI    FLAG,X'FF'-FLGPRN03 DONT WANT TO PRNT FULLY DISB 03              
         TM    FLAG,FLGPRN03       WANT TO PRNT FULLY DISB MOS TOT LIN?         
         BO    PRNL60              YES, DON'T CHECK IF FULLY DISBURSED          
         TM    PSSFLAG,PSSDISB     IS THIS DISBURSED ONLY ITEM                  
         BNO   PRNL60              DON'T PRINT IT                               
         GOTO1 ABFRIN,DMCB,(RC),BUFFASEQ       GET NXT RECORD                   
         TM    BCTSERRS,BUFFEEOF                                                
         BNO   PRNL10                                                           
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
         CLI   PSSKRTYP,X'00'      ARE WE AT A NEW ENTRY, TOTAL RECD            
         BNE   PRNL180                                                          
*                                                                               
         CLC   LSTSYS,PSSKSYS      LAST SYSTEM SAME AS CURRENT ?                
         BE    PRNL140                                                          
         BAS   RE,PSMCPTOT        PRINT SYS/MED/CLI/PROD TOTALS                 
         B     PRNL170             CHECK IF LAST RECORD AND EXIT                
*                                                                               
PRNL140  CLC   LSTMED,PSSKMED                                                   
         BE    PRNL150                                                          
*                                                                               
         MVC   PPRO,LSTPRO         MOVE PRODUCT CODE TO PRINT                   
         MVC   PEST,=C'TOTALS'     MOVE TOT IN NEXT FIELD                       
         LA    R1,PROTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR PRDCT                
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PCLI,LSTCLI                                                      
         MVC   PEST,=C'TOTALS'                                                  
         LA    R1,CLITBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR CLIENT               
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PMED,LSTMED                                                      
         MVC   PEST,=C'TOTALS'                                                  
         LA    R1,MEDTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR CLIENT               
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
         B     PRNL170                                                          
*                                                                               
PRNL150  CLC   LSTCLI,PSSKCLI                                                   
         BE    PRNL160                                                          
*                                                                               
         MVC   PPRO,LSTPRO         MOVE PRODUCT CODE TO PRINT                   
         MVC   PEST,=C'TOTALS'     MOVE TOT IN NEXT FIELD                       
         LA    R1,PROTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR PRDCT                
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PCLI,LSTCLI                                                      
         MVC   PEST,=C'TOTALS'                                                  
         LA    R1,CLITBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR CLIENT               
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
         B     PRNL170                                                          
*                                                                               
PRNL160  CLC   LSTPRO,PSSKPRO      LAST PRODUCT SAME AS CURRENT                 
         BE    PRNL170                                                          
*                                                                               
         MVC   PPRO,LSTPRO         MOVE PRODUCT CODE TO PRINT                   
         MVC   PEST,=C'TOTALS'     MOVE TOT IN NEXT FIELD                       
         LA    R1,PROTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR PRDCT                
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
*                                                                               
PRNL170  BAS   RE,PRNTLIN                                                       
*                                                                               
PRNL180  DS    0H                                                               
         CLI   PSSKRTYP,X'00'      ARE WE AT A NEW ENTRY, TOTAL RECD            
         BE    PRNL190                                                          
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
PRNL210  MVC   PCLI,PSSKCLI        CLIENT                                       
         MVC   LSTCLI,PSSKCLI                                                   
PRNL220  MVC   PPRO,PSSKPRO        PRODUCT                                      
         MVC   LSTPRO,PSSKPRO                                                   
PRNL230  MVC   PEST,PSSKEST                                                     
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
         BE    PRNL300                                                          
*                                                                               
PRNL270  MVC   PSTA,SPACES                                                      
         MVC   PINV,SPACES                                                      
         CLI   PSSKRTYP,X'05'                                                   
         BE    PRNL290                                                          
*                                                                               
         CLI   PSSKRTYP,X'02'                                                   
         BNE   PRNL280                                                          
         MVC   PSTA,PSSSRAC                                                     
         MVC   PINV(6),PSSSRBNO                                                 
*                                                                               
PRNL280  XC    LSTSTN,LSTSTN      CLEAR STATION FROM LAST CLI/PRO/EST           
         B     PRNL320                                                          
*                                                                               
PRNL290  DS    0H                                                               
         CLI   AUTPRF3,C'Y'                                                     
         BNE   *+12                                                             
         CLI   PSSKSTAT,PSSKTOT                                                 
         BE    PRNL490                                                          
*                                                                               
         CLC   LSTSTN,PSSKSTN                                                   
         BNE   PRNL300                                                          
         TM    FLAG1,FLGSTN       DID WE PRINT THIS STATION BEFORE              
         BO    PRNL310                                                          
PRNL300  NI    FLAG1,X'FF'-FLGSTN THIS STATION HAS NOT BEEN PRINTED YET         
         MVC   PSTA,PSSKSTN                                                     
         MVC   LSTSTN,PSSKSTN                                                   
PRNL310  MVC   PINV,PSSKINV                                                     
*                                                                               
PRNL320  DS    0H                 PRINT CLEARED,BILLED,APPLIED BUCKETS          
         LA    R4,PSSABKT         POINT TO START OF BUCKETS                     
         LA    R6,PBUCKET         POINT WHERE TO START PRINTING                 
         LHI   R1,PSSBKCT2        NUMBER OF BUCKETS TO LOOP TO PRINT            
         CLI   PSSKRTYP,X'05'     R V PRINTING SS DETAIL LINE BUCKETS           
         BE    PRNL370            YES DO REGULAR PRINTING                       
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
         BE    PRNL420                                                          
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
*                                                                               
PRNL420  DS    0H                                                               
         USING STATTABD,RE                                                      
         LA    RE,STATTAB         PRINT A=APPROVE,U=UNAPROVE,H=HELD ETC         
PRNL430  CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   STATRTYP,PSSKSTAT   IS THIS APP/UNAPP/HELD/TOT ETC               
         BNE   PRNL450                                                          
         CLI   STATCSH,X'FF'                                                    
         BE    PRNL440                                                          
         MVC   BYTE,STATCSH        CHECK CASH AVAILABILITY                      
         NC    BYTE,PSSFLAG                                                     
         BZ    PRNL450                                                          
         CLC   STATOPT,QOPT2                                                    
         BNE   PRNL450                                                          
PRNL440  CLI   STATSKIP,C'Y'       SKIP PRINTING THIS LINE?                     
         BE    PRNL490                                                          
         B     PRNL460                                                          
PRNL450  LA    RE,STATLNQ(RE)                                                   
         B     PRNL430                                                          
PRNL460  OI    FLAG1,FLGSTN        PRINTING STATION                             
         MVC   PMARKA,STATMRKA                                                  
         CLI   PSSKSTAT,PSSKTOT                                                 
         BNE   PRNL470                                                          
         CP    PSSUNAPB,=P'0'                                                   
         BE    PRNL480                                                          
PRNL470  MVC   PMARK,STATMRK                                                    
         B     PRNL480                                                          
PRNL480  BRAS  RE,PRINTIT                                                       
         OI    FLAG,FLGPRNT                                                     
         BRAS  RE,CLEARXP          CLEAR XP TO SPACES                           
*                                                                               
PRNL490  GOTO1 ABFRIN,DMCB,(RC),BUFFASEQ       GET NXT RECORD                   
         TM    BCTSERRS,BUFFEEOF                                                
         BNO   PRNL10              CHECK FOR EOF                                
         BRAS  RE,PSMCPTOT         PRINT SYS/MED/CLI/PROD TOT AND EXIT          
*                                                                               
PRNL500  DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
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
         BRAS  RE,CLEARXP          CLEAR XP TO SPACES                           
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
         CLI   PSSKRTYP,X'05'      ARE THESE SS DETAILS                         
         BNE   MRKIT70                                                          
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
**********************************************************************          
* PRINT TOTALS PAGE                                                  *          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R5                                                        
PRTOTAL  NTR1                                                                   
*                                                                               
PRTOT10  BRAS  RE,CLEARXP          CLEAR XP TO SPACES                           
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
* PRINT A LINE                                                                  
**********************************************************************          
         USING PLINED,R5                                                        
         SPACE 1                                                                
PRNTLIN  NTR1                                                                   
         ZIC   RF,MAXLINES                                                      
         BCTR  RF,0                MAXLINE MINUS 1                              
         CLM   RF,1,LINE                                                        
         BH    *+12                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     PRNTLNX                                                          
         CLI   LINE,X'01'             ARE WE AT LINE 1                          
         BE    PRNTLNX                                                          
         LA    R5,XP                                                            
         MVI   PSYS-1,X'BF'           FILL XP WITH DASHES                       
         MVC   PSYS(PLINELNQ-2),PSYS-1                                          
         BRAS  RE,PRINTIT                                                       
PRNTLNX  B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
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
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
*                                                                               
ABOXRC   DC    A(BOXRC)                                                         
ABXHOOK  DC    A(BXHOOK)                                                        
ACCMST   DC    CL8'ACCMST'                                                      
PKROUND  DC    XL2'010C'                                                        
         SPACE 2                                                                
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(TSRTAB)           SS TRANSACTIONS TABLE                        
         DC    A(PSSATAB)          SS TRANSACTIONS TABLE                        
         DC    A(SETMOS)           SET START AND END MOS DATES                  
         DC    A(DUMP)             ROUTINE TO DITTO RECORDS                     
         DC    A(MAINTAB)          MAIN TABLE                                   
         DC    A(LDSYTAB)          LEDGER SYSTEM TABLE                          
         DC    A(MBUFF)            BUFFERIN ROUTINE                             
         DC    V(AUTOAPP)          AUTO APPROVE MODULE                          
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(BINSRCH)          BINSRCH (31 BIT MODE)                        
         DC    V(HELLO)            HELLO                                        
         DC    V(ACLIST)           CLIENT LIST                                  
         DC    V(BUFFERIN)         BUFFERIN                                     
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
         XC    AAEXTRA(AUTBLKXLQ),AAEXTRA                                       
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
         CLI   AUTPRF3,C'Y'                                                     
         BNE   *+8                                                              
         MVI   QOPT2,C'Y'                                                       
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
**********************************************************************          
* CLEAR XP TO SPACES                                                            
**********************************************************************          
         SPACE 1                                                                
CLEARXP  NTR1  BASE=*,LABEL=*                                                   
         LA    R5,XP                                                            
         MVI   XP,C' '             FILL XP WITH SPACES                          
         MVC   XP+1(L'XP-1),XP                                                  
         J     EXIT                                                             
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
         BRAS  RE,HEADUP                                                        
         GOTO1 ACREPORT                                                         
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
* ROUTINE TO PRINT TOTALS AT SYS/MED/CLI/PROD LEVEL CHANGES          *          
**********************************************************************          
         SPACE 1                                                                
PSMCPTOT NTR1  BASE=*,LABEL=*                                                   
         USING PLINED,R5                                                        
         LA    R5,XP                                                            
*                                                                               
         TM    FLAG,FLGPRNT        WAS ANYTHING PRINTED ?                       
         BNO   PSMCPTX                                                          
         MVC   PPRO,LSTPRO         MOVE PRODUCT CODE TO PRINT                   
         MVC   PEST,=C'TOTALS'     MOVE TOT IN NEXT FIELD                       
         LA    R1,PROTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR PRDCT                
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PCLI,LSTCLI                                                      
         MVC   PEST,=C'TOTALS'                                                  
         LA    R1,CLITBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR CLIENT               
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PMED,LSTMED                                                      
         MVC   PEST,=C'TOTALS'                                                  
         LA    R1,MEDTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR MEDIA                
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PSYS,LSTSYS                                                      
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
***********************************************************************         
* TRACE HOOK                                                          *         
***********************************************************************         
TRCHK    NMOD1 0,*TRCHK                                                         
         LR    R2,R1               A(SRC)                                       
         LA    R2,0(R2)                                                         
         LR    R3,R1               A(SRC)                                       
*                                                                               
         BRAS  RE,CLEARXP          CLEAR XP TO SPACES                           
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
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R8                                                      
         USING BOXD,R4                                                          
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R8,VBIGPRNT                                                      
         L     R4,ADBOX                                                         
         USING PLINED,R5                                                        
         LA    R5,XP                                                            
*                                                                               
         CLI   RCSUBPRG,4          IS THIS TOTALS PAGE?                         
         BE    BXHK30                                                           
*                                                                               
         CLC   PEST,=C'TOTALS'     PRINTING CLI/PROD/EST/SYS/MED TOTALS         
         BE    BXHK20                                                           
*                                                                               
         MVC   PSYS,LSTSYS                                                      
*                                                                               
         CLI   PSYS,C'N'           IS THIS NET SYSTEM ?                         
         BNE   *+12                                                             
         CLI   AUTPRF2,C'S'        RUN BY SYSTEM OR SYS/MEDIA ?                 
         BE    *+10                                                             
         MVC   PMED,LSTMED                                                      
*                                                                               
         MVC   PCLI,LSTCLI                                                      
         MVC   PPRO,LSTPRO                                                      
         MVC   PEST,LSTEST                                                      
*                                                                               
BXHK10   CLC   LSTSTN,SPACES                                                    
         BNH   BXHK20                                                           
         MVC   PSTA,LSTSTN                                                      
         DROP  R5                                                               
*                                                                               
BXHK20   MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
         MVI   BOXCOLS+(PMED-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PCLI-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PPRO-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PEST-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PMOS-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PSTA-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PINV-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PCLEAR-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PBILLED-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PRCVD-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PDISB-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PCASH-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PUNDISB-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PAVAIL-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+PLINELNQ,C'R'                                            
         B     BXXIT                                                            
*                                                                               
BXHK30   DS    0H                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
*        MVI   BOXCOLS+(PTOTMSG-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PTOT-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+PLINELNQ,C'R'                                            
         B     BXXIT                                                            
*                                                                               
BXXIT    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS BXHOOK  NMOD1                                             *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
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
*                                                                               
*                                                                               
*                                                                               
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACAAD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
VTYPES   DS    0A                                                               
ATSRTAB  DS    A                   SS TRANSACTION TABLE                         
APSSATAB DS    A                   SS ACC TABLE BUILT FROM FILE                 
ASETMOS  DS    A                   SETS START AND END MOS DATES                 
ADUMP    DS    A                   ROUTINE TO DITTO RECORDS                     
AMAINTAB DS    A                   ADDRESS OF MAIN TABLE                        
ALDSYTAB DS    A                   LEDGER SYSTEM TABLE                          
ABFRIN   DS    A                   ADDRESS OF TSAR MAINTENANCE ROUTINE          
AUTOAPP  DS    V                   AUTO APPROVE MODULE                          
APRNTBL  DS    V                   PRINT DATA                                   
BINSRC31 DS    V                   BINSRCH (31 BIT MODE)                        
VHELLO   DS    V                                                                
ACLIST   DS    V                                                                
VBUFFRIN DS    V                   ADDRESS OF BUFFERIN                          
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
TEMPWRK  DS    CL255               TSAR WORK AREA                               
BUFFREC  DS    A                   BUFFER S/R A(RECORD)                         
BUFFRET  DS    X                   BUFFER S/R RETURN INDICATORS                 
TSARKSAV DS    XL150               TSAR RECORD KEY SAVE AREA                    
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
* ACAUTOAPD                                                                     
       ++INCLUDE ACAUTOAPD                                                      
*                                                                               
*                                                                               
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
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLINE  DS    0C                                                               
         DS    CL2                                                              
PSYS     DS    CL1                 SYSTEM                                       
         DS    CL1                                                              
PMED     DS    CL1                 MEDIA                                        
         DS    CL1                                                              
PCLI     DS    CL3                 CLIENT CODE                                  
         DS    CL1                                                              
PPRO     DS    CL3                 PRODUCT CODE                                 
         DS    CL1                                                              
PEST     DS    CL6                 ESTIMATE NUMBER                              
         DS    CL1                                                              
PMOS     DS    CL6                 MONTH OF SERVICE                             
         DS    CL1                                                              
PSTA     DS    CL14                STATION                                      
         DS    CL1                                                              
*PINV     DS    CL10                INVOICE NUMBER                              
PINV     DS    CL11                INVOICE NUMBER                               
         DS    CL1                                                              
*                                                                               
PBUCKET  DS    0CL17                                                            
*                                                                               
PBILLED  DS    CL16                BILLED AMOUNT FROM SR                        
PBUCLEN  EQU   *-PBUCKET                                                        
         DS    CL1                                                              
PRCVD    DS    CL16                CASH RECEIVED FROM SR                        
         DS    CL1                                                              
PBUCKET2 DS    0CL17                                                            
PDISB    DS    CL16                DISBURSED AMOUNT                             
         DS    CL1                                                              
PCASH    DS    CL16                CASH AVAILABLE                               
         DS    CL1                                                              
PCLEAR   DS    CL16                CLEARD AMOUNT                                
         DS    CL1                                                              
PUNDISB  DS    CL16                APPROVED AMMOUNT                             
PMARKA   DS    CL2                 MARK ITEMS CURRENTLY APPROVING 'A'           
         DS    CL1                                                              
PAVAIL   DS    CL16                CASH AVAILABLE TO APPROVE                    
         DS    CL1                                                              
PUNAPPR  DS    0CL16               UNAPPROVED AMOUNT                            
         DS    CL1                                                              
*                                                                               
PLINELNQ EQU   *-PRTLINE                                                        
         DS    CL1                                                              
PMARK    DS    CL6                                                              
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
*TSRKOFF  DS    CL2                                                             
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
*SRACCR  DS    PL8                 CALCULATED CREDIT                            
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
* ACAUTOD                                                                       
       ++INCLUDE ACAUTOD                                                        
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
* DDBUFFD                                                                       
       ++INCLUDE DDBUFFD                                                        
* AUTBLKX                                                                       
       ++INCLUDE ACAUTOAPDX                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACREPAA02 03/24/20'                                      
         END                                                                    
