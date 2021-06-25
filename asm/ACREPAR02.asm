*          DATA SET ACREPAR02  AT LEVEL 048 AS OF 05/01/02                      
*PHASE ACAR02A,+0                                                               
*INCLUDE SORTER                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'NEW ACCOUNTS RECEIVABLE'                                        
ACAR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACAR**,RA                                                    
         L     R8,0(R1)                                                         
         USING ACWORKD,R8                                                       
         SPACE 1                                                                
         USING ACARD,R7                                                         
         LA    R7,SPACEND                                                       
         MVC   PROFS,PROGPROF                                                   
         CLI   PROFBRKS,0                                                       
         BNE   *+8                                                              
         MVI   PROFBRKS,5                                                       
         SPACE 3                                                                
*******************************************************************             
*        REPORT PROFILES                                          *             
*                                                                 *             
*        PROFILE 1    AGE BY INVOICE OR DUE DATE (DEFAULT INV)    *             
*        PROFILE 2    PRINT JOB NUM OR DUE DATE (DEFAULT JOB)     *             
*        PROFILE 3    AGEING INCREMENT (DEFAULT 30 DAYS)          *             
*        PROFILE 4    PRNT BILL SOURCE OR DESCRIP(DEFAULT DESCRIP)*             
*        PROFILE 5    FLAG PARTIAL AMOUNTS Y OR N (DEFAULT NO)    *             
*        PROFILE 6    TREAT CURRENT BALANCE AS PAST DUE. THIS     *             
*                     WILL ELIMINATE THE CURRENT BALANCE LINE.    *             
*                     'CURRENT' ITEMS WILL NOW SHOW ON THE        *             
*                     PAST DUE'1 - XX DAYS' LINE. Y OR (DEFAULT N)*             
*******************************************************************             
         EJECT                                                                  
*              ROUTINE FOR RUN FIRST                                            
         SPACE 1                                                                
RUNF     CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
         SPACE 1                                                                
         RELOC RELO                                                             
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
         SPACE 1                                                                
         CLC   RCPROG,=C'BR'               BILLING RECAP                        
         BE    RUNBR                                                            
         MVC   RPTLNES,SPROGAR                                                  
         MVI   RCSUBPRG,0                                                       
         MVC   ARPWK1(ARPLNQ),SPACES       INITIALIZE PRINT LINE 1              
         MVC   ARPWK2(ARPLNQ),SPACES                             2              
         MVC   ARPWK3(ARPLNQ),SPACES                             3              
         MVC   ARPWK4(ARPLNQ),SPACES                             4              
         ZIC   R1,LSTMAX                                                        
         STC   R1,MAXLINES              SET MAX LINES PER PAGE                  
         STC   R1,PGEMAX                                                        
         MVC   PAGE,=H'1'                                                       
         LA    R0,2                                                             
RUNF3    GOTO1 PRINT,DMCB,P,=C'BC01'    SKIP TO CHANNEL 1                       
         MVI   LINE,1                   PRINT LINE UP                           
         MVI   P+1,C'X'                                                         
         MVC   P+2(81),P+1                                                      
         GOTO1 ACREPORT                                                         
         MVC   GETLNE,MIDLNE            SKIP TO DETAIL LINE - 2                 
         BAS   RE,SKIPLNE                                                       
         MVI   P+1,C'X'                                                         
         MVC   P+2(81),P+1                                                      
         MVC   PSECOND,P                                                        
         GOTO1 ACREPORT                                                         
         BCT   R0,RUNF3                                                         
         B     XIT                                                              
         SPACE 1                                                                
*              LINE-UP FOR RECAP                                                
*                                                                               
RUNBR    MVC   RPTLNES,SPROGBR                                                  
         MVI   RCSUBPRG,1                                                       
         MVC   ARPWK1(ARPLNQ),SPACES       INITIALIZE PRINT LINE 1              
         MVC   ARPWK2(ARPLNQ),SPACES                             2              
         MVC   ARPWK3(ARPLNQ),SPACES                             3              
         MVC   ARPWK4(ARPLNQ),SPACES                             4              
         ZIC   R1,LSTMAX                                                        
         STC   R1,MAXLINES              SET MAX LINES PER PAGE                  
         STC   R1,PGEMAX                                                        
         MVC   PAGE,=H'1'                                                       
         LA    R0,2                                                             
RUNBR3   GOTO1 PRINT,DMCB,P,=C'BC01'    SKIP TO CHANNEL 1                       
         MVI   LINE,1                   PRINT LINE UP                           
         MVI   P+1,C'X'                                                         
         MVC   P+2(81),P+1                                                      
         GOTO1 ACREPORT                                                         
         MVC   GETLNE,MIDLNE            SKIP TO MIDLINE                         
         BAS   RE,SKIPLNE                                                       
         MVC   P+1(L'BRHEAD1),BRHEAD1                                           
         MVC   PSECOND+1(L'BRHEAD2),BRHEAD2                                     
         GOTO1 ACREPORT                                                         
         BCT   R0,RUNBR3                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FOR REQUEST FIRST                                        
         SPACE 1                                                                
REQF     CLI   MODE,REQFRST                                                     
         BNE   PRCT                                                             
*                           HANDLE OLD AR UNTIL BR IS LIVE                      
         CLI   QOPT1,C'B'                                                       
         BNE   *+14                                                             
         MVC   RPTLNES,SPROGAR                                                  
         MVI   RCSUBPRG,1                                                       
*                                                                               
         SPACE 1                                                                
         XC    ALSORT,ALSORT       CLEAR A(LAST SORT)                           
         LA    R1,SRTKLNQ                                                       
         CVD   R1,DUB              CONVERT KEY LENGTH TO CHARACTER              
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         LA    R1,SRTLNQ                                                        
         CVD   R1,DUB              CONVERT RECORD LENGTH TO CHARACTER           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         SPACE 1                                                                
         XC    STDATE1,STDATE1     NO START DATE                                
         CLC   QSTART,SPACES       UNLESS SPECIFIED                             
         BE    REQF2                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(1,STDATE1)                               
*                                                                               
REQF2    GOTO1 DATCON,DMCB,(4,RCDATE),(0,ENDATE0)                               
         CLC   QEND,SPACES         USE TODAY AS END                             
         BE    *+10                                                             
         MVC   ENDATE0,QEND        UNLESS END SPECIFIED                         
         GOTO1 DATCON,DMCB,(0,ENDATE0),(1,ENDATE1)                              
         SPACE 1                                                                
         MVC   WORK(6),ENDATE0     ESTBLISH AGEING DATES                        
         LA    R2,PERDTE1                                                       
         LA    R0,4                                                             
         L     R6,=F'-30'          SET FOR 30 DAY AGEING (DEFAULT)              
         CLI   PROFDAYS,0          PROFILE CAN OVERRIDE                         
         BE    REQF3                                                            
         ZIC   R6,PROFDAYS                                                      
         LNR   R6,R6                                                            
         SPACE 1                                                                
REQF3    GOTO1 ADDAY,DMCB,WORK,WORK+6,(R6)                                      
         MVC   WORK(6),WORK+6                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(1,0(R2))                                   
         LA    R2,3(R2)                                                         
         BCT   R0,REQF3                                                         
         SPACE 1                                                                
         LA    R5,ACCTOTS          CLEAR ACCOUNT                                
         LA    R6,REQTOTS          AND REQUESTS TOTALS                          
         LA    R0,PERACNT                                                       
REQF5    ZAP   0(8,R5),=P'0'                                                    
         ZAP   0(8,R6),=P'0'                                                    
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         BCT   R0,REQF5                                                         
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FOR PROCESS TRANSACTION                                  
         SPACE 1                                                                
PRCT     CLI   MODE,PROCTRNS                                                    
         BNE   REQL                                                             
         USING TRANSD,R4                                                        
         L     R4,ADTRANS                                                       
         CLI   TRNSEL,X'44'                                                     
         BNE   XIT                                                              
         CLC   TRNSDATE,STDATE1                                                 
         BL    XIT                 BEFORE START                                 
         CLC   TRNSDATE,ENDATE1                                                 
         BH    XIT                 AFTER END DATE                               
         SPACE 1                                                                
         USING SRTD,R5                                                          
         LA    R5,SRTWRK                                                        
         XC    SRTWRK(SRTLNQ),SRTWRK                                            
         ZAP   SRTDBS,=P'0'                                                     
         ZAP   SRTCRD,=P'0'                                                     
         SPACE 1                                                                
         L     R2,ADTRANS                                                       
         SH    R2,DATADISP         GET TO BEGINNING OF THE KEY                  
         USING ACKEYD,R2                                                        
         MVC   SRTACC,ACKEYACC     ACCOUNT CODE                                 
         MVC   SRTCON,ACKEYCON     BILLING SOURCE                               
         MVC   SRTDTE,TRNSDATE     TRANSACTION DATE                             
         MVC   SRTINV,TRNSREF      INVOICE NUMBER                               
*                                                                               
         SR    RF,RF                                                            
         L     R2,ADTRANS                                                       
         XC    AELEMS(AELEMSQ),AELEMS                                           
PRCT001  CLI   0(R2),0             END OF RECORD                                
         BE    PRCT005             FINISHED                                     
         LA    RE,AELEM1A          A(ELEMENT X'1A')                             
         CLI   0(R2),ACMTELQ       X'1A' (MEDIA TRANSFER ELEMENT)               
         BE    PRCT004                                                          
         LA    RE,AELEM23          A(ELEMENT X'23')                             
         CLI   0(R2),ACOTELQ       X'23' (OTHER ELEMENT)                        
         BE    PRCT004                                                          
         LA    RE,AELEM4F          A(ELEMENT X'4F')                             
         CLI   0(R2),TRCPELQ       X'4F' (CLI/PRO/JOB ELEMENT)                  
         BE    PRCT004                                                          
         LA    RE,AELEMD9          A(ELEMENT X'D9')                             
         CLI   0(R2),ACRAELQ       X'D9'                                        
         BNE   *+8                                                              
PRCT004  ST    R2,0(RE)                                                         
         IC    RF,1(R2)            LENGTH OF ELEMENT                            
         AR    R2,RF                                                            
         B     PRCT001                                                          
*                                                                               
PRCT005  CLI   TRNSTYPE,9                                                       
         BE    PRCT006                                                          
         CLI   TRNSTYPE,20                                                      
         BE    PRCT006                                                          
         CLI   TRNSTYPE,26                                                      
         BE    PRCT006                                                          
         CLI   TRNSTYPE,30                                                      
         BNE   PRCT5                                                            
         ICM   R2,15,AELEMD9                                                    
         BZ    PRCT5                                                            
         CLI   ACRATYPE-ACRAD(R2),ACRATTFR     TRANSFERED FROM                  
         BNE   PRCT5                                                            
*                                                                               
PRCT006  ICM   R2,15,AELEM23                                                    
         BZ    PRCT008             NO GOOD, TRY NEXT                            
         CLI   1(R2),ACOTLNQ1      MUST BE RIGHT LENGTH                         
         BNE   PRCT008             NO GOOD, TRY NEXT                            
         LA    RE,ACOTNUM-ACOTHERD(R2)  POINT TO JOB IN X'23'                   
         CLC   ACOTNUM-ACOTHERD+6(3,R2),SPACES                                  
         BNH   *+8                                                              
         LA    RE,ACOTNUM-ACOTHERD+3(R2)                                        
         CLI   ACOTPROF-ACOTHERD(R2),C'J'                                       
         BE    PRCT020             FOUND JOB                                    
*                                                                               
PRCT008  ICM   R2,15,AELEM4F                                                    
         BZ    PRCT010                                                          
         LA    RE,TRCPJOB-TRCPJD(R2)     POINT TO JOB IN X'4F'                  
         CLI   TRCPTYPE-TRCPJD(R2),C'J'                                         
         BE    PRCT020                                                          
*                                                                               
PRCT010  ICM   R2,15,AELEM1A                                                    
         BZ    PRCT030                                                          
         LA    RE,ACMTJOB-ACMTD(R2)      POINT TO JOB IN X'1A'                  
*        CLI   TRNSTYPE,30                                                      
*        BE    PRCT020                                                          
*        CLI   ACMTSYS-ACMTD(R2),C'J'                                           
*        BNE   PRCT030                                                          
*                                                                               
PRCT020  MVC   SRTJOB,0(RE)        MOVE IN JOB                                  
         LA    R1,L'SRTJOB         FIGURE OUT JOB LENGTH                        
         LA    RE,SRTJOB+L'SRTJOB-1                                             
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         LA    R2,TRNSNARR                                                      
         ZIC   R3,TRNSLEN                                                       
         SH    R3,=H'28'            R3 TO MAXIMUM NARRATIVE                     
         BNP   PRCT5                NO NARRATIVE                                
         BCTR  R1,0                                                             
         EXCLC R1,0(R2),SRTJOB                                                  
         BNE   PRCT2                                                            
         LA    R2,1(R1,R2)                                                      
         SR    R3,R1                                                            
         SH    R3,=H'01'                                                        
         BNP   PRCT5                                                            
         B     PRCT2                                                            
*                                                                               
PRCT030  ZIC   R3,TRNSLEN                                                       
         SH    R3,=H'28'            R3 TO MAXIMUM NARRATIVE                     
         BNP   PRCT5                NO NARRATIVE                                
         LA    R6,SRTJOB                                                        
         LA    R0,L'SRTJOB                                                      
         LA    R2,TRNSNARR                                                      
PRCT1    MVC   0(1,R6),0(R2)       MOVE JOB CODE                                
         LA    R6,1(R6)                                                         
         LA    R2,1(R2)                                                         
         SH    R3,=H'1'                                                         
         BZ    PRCT5               END OF NARRATIVE                             
         CLI   0(R2),C' '                                                       
         BE    PRCT2               END OF JOB CODE                              
         SH    R0,=H'1'                                                         
         BZ    PRCT2               END OF SRTJOB                                
         B     PRCT1                                                            
PRCT2    CLI   0(R2),C' '                                                       
         BNE   PRCT3                                                            
         LA    R2,1(R2)           R2 1ST CHARACTER OF DESCRIPTION               
         BCT   R3,PRCT2                                                         
         MVC   SRTDSC,SPACES        NO DESCRIPTION UNLESS INPUT                 
         B     PRCT5                                                            
*                                                                               
PRCT3    LA    R6,SRTDSC                                                        
         LA    R0,L'SRTDSC                                                      
PRCT4    MVC   0(1,R6),0(R2)       MOVE JOB CODE                                
         LA    R6,1(R6)                                                         
         LA    R2,1(R2)                                                         
         SH    R0,=H'1'                                                         
         BZ    PRCT5               END OF SRTDSC                                
         SH    R3,=H'1'                                                         
         BZ    PRCT5                                                            
         B     PRCT4                                                            
*                                                                               
PRCT5    LA    R1,SRTDBS                                                        
         TM    TRNSSTAT,X'80'                                                   
         BO    *+8                 BRANCH IF DEBIT                              
         LA    R1,SRTCRD                                                        
         ZAP   0(L'SRTDBS,R1),TRNSAMNT                                          
*                                                                               
         USING TRDUED,R2                                                        
         ICM   R2,15,AELEM61                                                    
         BZ    PRCT7                                                            
         GOTO1 DATCON,DMCB,(2,TRDUDATE),(1,SRTDUE)   DUE DATE                   
*                                                                               
         USING TRCPJD,R2                                                        
PRCT7    ICM   R2,15,AELEM4F       GET X'4F' ELEMENT                            
         BZ    PRCT9                                                            
         CLI   TRCPTYPE,C'J'                                                    
         BNE   PRCT9                                                            
         MVC   SRTCLI,TRCPCLI      CLIENT                                       
         MVC   SRTPRD,TRCPPROD     PRODUCT                                      
         MVC   SRTJOB,TRCPJOB      AND JOB                                      
*                                                                               
PRCT9    CLI   TRNSTYPE,6          IS IT PROD BILLING                           
         BE    PRCT10                                                           
         CLI   TRNSTYPE,7          IS IT W/P BILLING                            
         BE    PRCT10                                                           
         CLI   TRNSTYPE,30         WAS IT TRANSFERED FROM PROD.?                
         BE    PRCT10                                                           
         CLI   RCSUBPRG,1          IS THIS BILLING SUMMARY                      
         BE    XIT                 IF SO SKIP ITEMS THAT ARE NOT PROD           
         B     PRCT12                                                           
*                                                                               
         USING ACMTD,R2                                                         
PRCT10   OC    SRTCLI,SRTCLI                                                    
         BNZ   PRCT11                                                           
         ICM   R2,15,AELEM1A       GET MEDIA TRANSFER ELEMENT                   
         BZ    PRCT11                                                           
         CLI   ACMTSYS,C'J'                                                     
         BNE   PRCT11                                                           
         MVC   SRTCLI(6),ACMTCLI   MOVE IN CLI/PROD                             
         OC    SRTJOB,SRTJOB                                                    
         BNZ   PRCT11                                                           
         MVC   SRTCLI(12),ACMTCLI  MOVE IN CLI/PROD/JOB                         
*                                                                               
         USING ACOTHERD,R2                                                      
PRCT11   CLC   SRTCLI,SPACES                                                    
         BH    PRCT12                                                           
         ICM   R2,15,AELEM23       GET OTHER NUMBER ELEMENT                     
         BZ    PRCT12                                                           
         CLI   ACOTPROF,C'J'                                                    
         BNE   PRCT12                                                           
         CLC   ACOTNUM+6(3),SPACES ONLY JOB                                     
         BNH   PRCT12                                                           
         MVC   SRTPRD(9),ACOTNUM   PROD AND JOB                                 
         MVC   SRTCLI,SRTACC+5     ASSUME SAME CLIENT                           
         SPACE 1                                                                
PRCT12   DS    0H                                                               
         OC    SRTCLI,SRTCLI                                                    
         BZ    PRCT14                                                           
         CLI   TRNSTYPE,30         WAS IT TRANSFERED FROM PROD.?                
         BNE   *+10                                                             
         XC    SRTDSC,SRTDSC                                                    
*                                                                               
PRCT14   CLI   QOPT7,C'D'                                                       
         BNE   PRCT15                                                           
         CLC   QSELECT,SPACES                                                   
         BNH   *+14                                                             
         CLC   QSELECT,SRTINV      FILTER BY INVOICE NUMBER                     
         BNE   PRCT15                                                           
         GOTO1 PRNTBL,DMCB,(3,=C'PUT'),(R5),C'DUMP',SRTLNQ,=CL2'2D'             
PRCT15   GOTO1 SORTER,DMCB,=C'PUT',(R5)                                         
         MVI   ALSORT,1            ACTIVITY SWITCH                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FOR REQUEST LAST                                         
         SPACE 1                                                                
REQL     CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         OC    ALSORT,ALSORT                                                    
         BZ    REQL99              NO DATA                                      
         XC    LSTWRK(SRTLNQ),LSTWRK                                            
         MVI   ACTIVITY,C'N'                                                    
         MVI   TOTSW,C'A'                                                       
         SPACE 1                                                                
REQL2    GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R5,DMCB+4                                                        
         ST    R5,ALSORT           ADDRESS OF LAST SORT                         
         LTR   R5,R5                                                            
         BZ    REQL9                 END OF RECORDS FROM SORT                   
         MVC   SRTWRK(SRTLNQ),0(R5)  SAVE CURRENT SORT RECORD                   
         OC    LSTWRK(SRTLNQ),LSTWRK DO I HAVE ONE SAVED                        
         BNZ   REQL5                 BRANCH IF I DO.                            
REQL3    MVC   LSTWRK(SRTLNQ),SRTWRK SAVE THIS ONE                              
         B     REQL2                 AND GET NEXT.                              
         SPACE 1                                                                
REQL5    CLC   LSTWRK(SRTKLNQ),SRTWRK                                           
         BNE   REQL9               NOT SAME KEY                                 
         LA    R6,LSTWRK                                                        
         AP    SRTDBS-SRTD(L'SRTDBS,R6),SRTDBS   ADD'EM UP                      
         AP    SRTCRD-SRTD(L'SRTCRD,R6),SRTCRD                                  
         OC    SRTCLI(12),SPACES                                                
         CLC   SRTCLI(12),SPACES                 SAVE ANY SIGNIFICANT           
         BE    *+10                              DATA                           
         MVC   SRTCLI-SRTD(12,R6),SRTCLI                                        
         OC    SRTDSC,SPACES                                                    
         CLC   SRTDSC,SPACES                 SAVE ANY SIGNIFICANT               
         BE    *+10                              DATA                           
         MVC   SRTDSC-SRTD(L'SRTDSC,R6),SRTDSC                                  
         B     REQL2                             AND GET NEXT                   
         SPACE 1                                                                
REQL9    BAS   RE,FORMAT           AGE AND SETUP PRINT                          
         OC    ALSORT,ALSORT       IS IT END OF FILE                            
         BNZ   REQL11              NOT END OF FILE                              
         CLI   ACTIVITY,C'N'                                                    
         BE    *+8                 NO ACTIVITY                                  
         BAS   RE,ACCTOT           IF END PRINT ACCOUNT TOTAL                   
         MVI   TOTSW,C'R'                                                       
         MVI   ACTIVITY,C'N'                                                    
         BAS   RE,ACCTOT           REQUEST TOTAL                                
         B     REQL99              AND GET OUT                                  
         SPACE 1                                                                
REQL11   CLC   LSTWRK(L'SRTACC),SRTWRK     IS IT SAME ACCOUNT                   
         BE    REQL3                       IF IT IS GET NEXT                    
         CLI   ACTIVITY,C'N'                                                    
         BE    *+8                 NO ACTIVITY                                  
         BAS   RE,ACCTOT                   IF NOT PRINT ACCOUNT TOTAL           
         B     REQL3                                                            
         SPACE 1                                                                
REQL99   GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO AGE AND FORMAT DETAIL LINE                            
         SPACE 1                                                                
         USING PERAD,R2                                                         
FORMAT   NTR1                                                                   
         LA    R2,ACCTOTS          ACCOUNT TOTALS                               
         LA    R5,LSTWRK                                                        
         ZAP   DUB,SRTDBS          DEBITS                                       
         SP    DUB,SRTCRD          LESS CREDITS EQ AMT DUE                      
         BZ    XIT                                                              
         AP    PERAMT,DUB          ACCOUNT TOTAL                                
         SPACE 1                                                                
         LA    R4,SRTDTE           SET R4 FOR INVOICE DATE FOR AGEING           
         CLI   PROFAGE,C'D'        AGE BY DUE DATE                              
         BNE   FRMT2               NO- USE DEFAULT OF INVOICE DATE              
         SPACE 1                                                                
         LA    R4,SRTDUE           YES - SET R4 FOR DUE DATE                    
         CLC   SRTDUE,ENDATE1      IS DUE DATE HIGHER THAN AS OF DATE           
         BNH   *+14                NO - CONTINUE                                
         AP    PERAMTF,DUB         ADD TO DUE IN FUTURE                         
         B     FRMT5A              SKIP AGEING                                  
         SPACE 1                                                                
FRMT2    LA    R1,PERDTE1          FIND BUCKET FOR AGEING                       
         LA    R3,PERAMT1                                                       
         LA    R0,4                                                             
         CLI   PROFCURR,C'Y'       TREAT 'CURRENT BAL' AS 'PAST DUE'            
         BNE   FRMT3               NO  - WORK OUT CURRENT BALANCE               
         LA    R3,PERAMT2          YES - CURRENT BAL FALLS IN 1-XX DAYS         
         LA    R0,3                                                             
FRMT3    CLC   0(3,R4),0(R1)                                                    
         BNL   FRMT5                                                            
         LA    R1,3(R1)            NEXT DATE                                    
         LA    R3,8(R3)            NEXT BUCKET                                  
         BCT   R0,FRMT3                                                         
FRMT5    AP    0(8,R3),DUB         ADD AMT DUE TO BUCKET                        
         SPACE 1                                                                
         USING ARPD,R6                                                          
FRMT5A   LA    R6,ARPWK1                                                        
         GOTO1 DATCON,DMCB,(1,SRTDTE),(10,ARPDTE)                               
         MVC   ARPINV,SRTINV       INVOICE                                      
         EDIT  (P8,SRTDBS),(14,ARPORG),2,MINUS=YES,COMMAS=YES                   
         MVC   WORK,SPACES                                                      
         CLI   PROFDUE,C'D'        DUE DATE INSTEAD OF JOB NUMBER               
         BNE   FRMT5C              NO - PRINT JOB NUMBER                        
         GOTO1 DATCON,DMCB,(1,SRTDUE),(10,ARPDDAT)                              
         B     *+10                                                             
         SPACE 1                                                                
FRMT5C   MVC   ARPJOB,SRTJOB                                                    
         CLI   PROFDESC,C'B'       PRINT BILLING SOURCE                         
         BNE   *+14                NO -PRINT DESCRIPTION                        
         MVC   ARPDSC+2(12),SRTCON+3                                            
         B     FRMT7                                                            
         SPACE 1                                                                
         MVC   ARPDSC,SRTDSC                                                    
         OC    ARPDSC,ARPDSC                                                    
         BNZ   FRMT7                                                            
         OC    SRTCLI,SRTCLI                                                    
         BNZ   FRMT6                                                            
         MVC   ARPDSC(12),SRTCON+3            BILLING SOURCE                    
*        MVC   ARPDSC(17),=C'MONTHLY STATEMENT'                                 
         B     FRMT7                                                            
         SPACE 1                                                                
FRMT6    MVC   WORK,SPACES         CLEAR WORK TO ACCEPT JOB NAME                
         BAS   RE,GETJOB                                                        
         MVC   ARPDSC,WORK         JOB DESCRIPTION                              
*        MVC   ARPJOB,SRTJOB       JOB                                          
         SPACE 1                                                                
FRMT7    ZAP   DUB,SRTDBS          DEBITS                                       
         SP    DUB,SRTCRD          LESS CREDITS EQ AMT DUE                      
         EDIT  (P8,DUB),(14,ARPDUE),2,MINUS=YES,COMMAS=YES                      
         SPACE 1                                                                
         CLI   PROFPART,C'Y'       FLAG PARTIAL AMTS                            
         BNE   FRMT9               NO - SKIP IT                                 
         CLC   ARPORG,ARPDUE       ORIG AMT SAME AS AMOUNT DUE                  
         BE    *+8                 YES - SKIP IT                                
         MVI   ARPPART,C'*'        FLAG AS PARTIAL AMT                          
         SPACE 1                                                                
FRMT9    CLI   ACTIVITY,C'Y'                                                    
         BE    *+12                                                             
         MVI   ACTIVITY,C'Y'                                                    
         BAS   RE,HEADUP           FIRST FOR ACCOUNT DO HEADS                   
         MVC   PGEMAX,DETMAX       SET MAX FOR DETAIL                           
         BAS   RE,PRNTDET                                                       
         MVC   PGEMAX,LSTMAX       RESET DEFAULT MAX                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT DETAIL LINES                                    
         SPACE 1                                                                
PRNTDET  NTR1                                                                   
         LA    R2,ARPWK1           COMPUTE NUMBER OF LINES TO PRINT             
         SR    R3,R3                                                            
         LA    R0,4                                                             
PRNT3    CLC   0(ARPLNQ,R2),SPACES                                              
         BE    PRNT5                                                            
         AH    R3,=H'1'                                                         
         LA    R2,ARPLNQ(R2)                                                    
         BCT   R0,PRNT3                                                         
         SPACE 1                                                                
PRNT5    DS    0H                                                               
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         AH    R3,=H'1'             WILL PRINT AT LEAST ONE                     
         ZIC   R2,PGEMAX            MAX  ALLOWED FOR PAGE                       
         ZIC   R1,LINE              CURRENT LINE                                
         AR    R1,R3                PLUS NUMBER I WANT TO PRINT                 
         CR    R1,R2                MUST HAVE ENOUGH LINES                      
         BNH   *+8                                                              
         BAS   RE,HEADUP           OR ELSE HEADUP NEW                           
         SPACE 1                                                                
         LA    R0,4                MOVE MY WORK LINES                           
         LA    R1,P                TO P, PSECOND ETC.                           
         LA    R2,ARPWK1                                                        
PRNT7    MVC   0(ARPLNQ,R1),0(R2)                                               
         MVC   0(ARPLNQ,R2),SPACES     CLEAR MY WORK LINES                      
         LA    R1,132(R1)                                                       
         LA    R2,ARPLNQ(R2)                                                    
         BCT   R0,PRNT7                                                         
         GOTO1 ACREPORT                 AND PRINT IT                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT ACCOUNT TOTALS                                  
         SPACE 1                                                                
         USING PERAD,R2                                                         
ACCTOT   NTR1                                                                   
         LA    R2,ACCTOTS          ACCOUNT TOTALS                               
         CLI   TOTSW,C'A'                                                       
         BE    ACCTOT3                                                          
         LA    R2,REQTOTS          OR REQUEST TOTALS                            
         B     ACCTOT4             FORCE NEW PAGE                               
ACCTOT3  CLC   LINE,FUTLNE       AM I PAST START OF FUTURE DUE LINE             
         BNH   *+8                                                              
ACCTOT4  BAS   RE,HEADUP           IF SO HEADUP NEW PAGE                        
         MVC   GETLNE,FUTLNE       AND GET TO FUTURE DUE LINE                   
         BAS   RE,SKIPLNE                                                       
         SPACE 1                                                                
*              NOW PRINT AGEING SUMMARY                                         
         USING ARPD,R6                                                          
         LA    R6,ARPWK1                                                        
         CLI   RCSUBPRG,1                                                       
         BE    ACCTOT5             BILLING SUMMARY / NO AGEING                  
         CLI   PROFAGE,C'D'        AGE BY DUE DATE                              
         BNE   ACCTOT4A            NO - GO TO CURRENT BALANCE                   
         CP    PERAMTF,=P'0'       IS THERE A FUTURE DUE AMT                    
         BE    ACCTOT4A            NO - THEN DONT PRINT THE LINE                
         MVC   ARPDAY,=CL15'DUE IN FUTURE'                                      
         EDIT  (P8,PERAMTF),(14,ARPTOT),2,MINUS=YES,COMMAS=YES                  
         BAS   RE,PRNTDET                                                       
*                                                                               
ACCTOT4A MVC   GETLNE,SUMLNE       AND GET TO SUMMARY                           
         BAS   RE,SKIPLNE                                                       
         CLI   PROFBRKS,4          IF PROFBRKS=4, CHECKING                      
         BE    ACCTOT4B            PROFCURR=Y IS CONTRADICTORY.                 
         CLI   PROFCURR,C'Y'       TREAT 'CURRENT BAL' AS 'PAST DUE'            
         BE    ACCTOT4C            YES - SKIP EDIT OF CURRENT BALANCE           
*                                                                               
*              EDIT CURRENT BALANCE                                             
ACCTOT4B CLI   PROFBRKS,4          IF ONLY 4 BREAKS, ROLL SECOND BRK            
         BNE   *+10                BACK INTO CURRENT BALANCE                    
         AP    PERAMT1,PERAMT2                                                  
         MVC   ARPDAY,=CL15'CURRENT BALANCE'                                    
         EDIT  (P8,PERAMT1),(14,ARPTOT),2,MINUS=YES,COMMAS=YES                  
         BAS   RE,PRNTDET                                                       
*                                                                               
ACCTOT4C MVC   ARPDAY,=CL15'   PAST DUE    '                                    
         BAS   RE,PRNTDET                                                       
*                                                                               
         L     R1,=F'30'           DEFAULT AGEING INCREMENT                     
         CLI   PROFDAYS,0          CAN BE OVERRIDDEN BY PROFILE                 
         BE    *+10                                                             
         ZIC   R1,PROFDAYS                                                      
         LA    R9,PERAMT2          1ST AGEING ACCUM                             
         LA    R3,3                3 AGEING RANGES                              
         LA    R4,1                SET TO ONE                                   
*                                                                               
         CLI   PROFBRKS,4          IF ONLY FOUR BREAKS, PRINT                   
         BNE   ACCTOT4D            ALTERNATE MESSAGE                            
         LA    R9,PERAMT3                                                       
         B     ACCTOT4K                                                         
*                                                                               
ACCTOT4D MVC   ARPDAY,SPACES       CLEAR                                        
         MVC   ARPDAY+7(L'MESSA),MESSA                                          
         EDIT  (R4),(3,ARPDAY+4)   FIRST NUMBER OF RANGE                        
         OI    ARPDAY+6,X'F0'      MAKE SURE PRINTABLE NUMBER                   
         LR    R5,R1               INCREMENT INTO R5                            
         BCTR  R4,0                REDUCE BY 1                                  
         AR    R4,R5               ADD TO RANGE NUMBER                          
         EDIT  (R4),(3,ARPDAY+8),ALIGN=LEFT   SECOND NUMBER IN RANGE            
         EDIT  (P8,0(R9)),(14,ARPTOT),2,MINUS=YES,COMMAS=YES                    
         BAS   RE,PRNTDET                                                       
         SPACE 1                                                                
         LA    R4,1(R4)            ADD ONE TO RANGE NUMBER                      
         LA    R9,8(R9)            NEXT ACCUM                                   
         BCT   R3,ACCTOT4D                                                      
*                                                                               
         BCTR  R4,0                REDUCE BY 1 FOR OVER XX DAYS'                
         LA    R9,PERAMT5                                                       
         LA    R3,1                                                             
ACCTOT4K MVC   ARPDAY,SPACES       CLEAR                                        
         MVC   ARPDAY+3(L'MESSB),MESSB                                          
         CLI   PROFBRKS,4                                                       
         BNE   ACCTOT4M                                                         
         LR    R5,R1               INCREMENT INTO R5                            
         BCTR  R4,0                REDUCE BY 1                                  
         AR    R4,R5               ADD TO RANGE NUMBER                          
ACCTOT4M EDIT  (R4),(3,ARPDAY+8),ALIGN=LEFT   SECOND NUMBER IN RANGE            
         EDIT  (P8,0(R9)),(14,ARPTOT),2,MINUS=YES,COMMAS=YES                    
         BAS   RE,PRNTDET                                                       
         LA    R4,1(R4)            ADD ONE TO RANGE NUMBER                      
         LA    R9,8(R9)            NEXT ACCUM                                   
         BCT   R3,ACCTOT4K                                                      
*                                                                               
ACCTOT5  MVC   GETLNE,TOTLNE       ACCOUNT TOTAL LINE                           
         BAS   RE,SKIPLNE                                                       
         MVC   ARPTOT(13),=C'ACCOUNT TOTAL'                                     
         CLI   TOTSW,C'A'                                                       
         BE    *+10                                                             
         MVC   ARPTOT(13),=C'REQUEST TOTAL'                                     
         EDIT  (P8,PERAMT),(14,ARPDUE),2,MINUS=YES,COMMAS=YES                   
         BAS   RE,PRNTDET                                                       
*                                                                               
         MVC   PAGE,=H'1'          SET PAGE FOR NEXT                            
         SPACE 1                                                                
         CLI   TOTSW,C'A'                                                       
         BNE   ACCTOT9                                                          
         LA    R5,ACCTOTS          ADD ACCOUNT TOTALS                           
         LA    R6,REQTOTS          TO REQUEST TOTALS                            
         LA    R0,PERACNT                                                       
ACCTOT7  AP    0(8,R6),0(8,R5)                                                  
         ZAP   0(8,R5),=P'0'       AND CLEAR ACCOUNT TOTAL                      
         LA    R6,8(R6)                                                         
         LA    R5,8(R5)                                                         
         BCT   R0,ACCTOT7                                                       
ACCTOT9  MVI   ACTIVITY,C'N'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SETUP AND PRINT HEADLINES                             
         SPACE 1                                                                
HEADUP   NTR1                                                                   
         GOTO1 PRINT,DMCB,P,=C'BC01'    SKIP TO CHANNEL 1                       
         MVI   LINE,1              SET LINE NUMBER                              
         SPACE 1                                                                
         MVC   GETLNE,HEADLNE                                                   
         BAS   RE,SKIPLNE          SKIP TO HEADLINES                            
         CLI   TOTSW,C'R'                                                       
         BE    HEADUP15                                                         
*                                                                               
*              GET ACCOUNT RECORD                                               
*                                                                               
         USING ACKEYD,R3                                                        
         L     R3,AIO                                                           
         LA    R5,LSTWRK                                                        
         CLC   ACKEYACC(15),SRTACC       ALREADY HAVE ACCOUNT                   
         BE    HEADUP1                                                          
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(15),SRTACC                                              
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(R3),(R3)                        
         CLC   ACKEYACC(15),SRTACC                                              
         BE    *+6                                                              
         DC    H'0'                CAN'T READ ACCOUNT                           
         SPACE 1                                                                
*                                                                               
*              NOW PRINT THE HEADLINES                                          
*                                                                               
         USING ACNAMED,R4                                                       
HEADUP1  GOTO1 GETEL,DMCB,(X'20',(R3)),0                                        
         CLI   ELERR,0                                                          
         BNE   HEADUP3             NO NAME ELEMENT                              
         L     R4,ELADDR                                                        
         ZIC   R1,ACNMLEN          ACCOUNT NAME                                 
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+9(0),ACNMNAME                                                  
         GOTO1 ACREPORT                                                         
         SPACE 1                                                                
         USING ACADDD,R4                                                        
HEADUP3  GOTO1 GETEL,DMCB,(X'22',(R3)),0                                        
         CLI   ELERR,0                                                          
         BNE   HEADUP7             NO ADDRESS ELEMENT                           
         L     R4,ELADDR                                                        
         ZIC   R0,ACADLNES                                                      
         LA    R1,P+9                                                           
         LA    R2,ACADADD                                                       
HEADUP5  MVC   0(L'ACADADD,R1),0(R2)                                            
         LA    R1,132(R1)                                                       
         LA    R2,L'ACADADD(R2)                                                 
         BCT   R0,HEADUP5                                                       
         GOTO1 ACREPORT                                                         
         SPACE 1                                                                
HEADUP7  MVC   GETLNE,PGELNE       LINE NUMBER FOR PAGE                         
         BAS   RE,SKIPLNE                                                       
         MVC   P+63(4),=C'PAGE'                                                 
         EDIT  (B2,PAGE),(3,P+69),3,ZERO=BLANK                                  
         CLC   RCPROG,=C'BR'                                                    
         BNE   *+10                                                             
         MVC   P+37(13),=C'BILLING RECAP'                                       
         GOTO1 ACREPORT                                                         
         MVC   HALF,PAGE           UPDATE PAGE NUMBER                           
         LH    R3,HALF                                                          
         AH    R3,=H'1'                                                         
         STH   R3,HALF                                                          
         MVC   PAGE,HALF                                                        
         SPACE 1                                                                
         MVC   GETLNE,DTELNE       LINE NUMBER FOR DATE                         
         BAS   RE,SKIPLNE                                                       
         CLI   RCSUBPRG,1                                                       
         BNE   HEADUP8             BILLING SUMMARY                              
         LA    R5,P+63                                                          
         CLC   QSTART(6),SPACES    PERIOD                                       
         BE    HEADUP7C                                                         
         CLC   QEND(6),SPACES                                                   
         BNE   HEADUP7B                                                         
         MVC   0(5,R5),=C'FROM '                                                
         LA    R5,5(R5)                                                         
*                                                                               
HEADUP7B GOTO1 DATCON,DMCB,(0,QSTART),(5,0(R5))                                 
         CLC   QEND(6),SPACES                                                   
         BE    HEADUP10                                                         
         LA    R5,9(R5)                                                         
*                                                                               
HEADUP7C CLC   QEND(6),SPACES                                                   
         BE    HEADUP10                                                         
         MVC   0(3,R5),=C'TO '                                                  
         GOTO1 DATCON,DMCB,(0,QEND),(5,3(R5))                                   
         B     HEADUP10                                                         
*                                                                               
HEADUP8  MVC   P+63(5),=C'AS OF'                                                
         GOTO1 DATCON,DMCB,(1,ENDATE1),(8,P+69)                                 
*                                                                               
HEADUP10 MVC   P+9(7),=C'AC/ NO.'                                               
         L     R3,AIO                                                           
         MVC   P+17(12),3(R3)      ACCOUNT CODE                                 
*                                                                               
         CLI   RCSUBPRG,1                                                       
         BNE   HEADUP15            NOT BILLING RECAP                            
         CLI   QOPT1,C'B'                                                       
         BE    HEADUP15                                                         
         GOTO1 ACREPORT                                                         
         MVC   GETLNE,MIDLNE       GET TO MIDLINE                               
         BAS   RE,SKIPLNE                                                       
         MVC   P+1(L'BRHEAD1),BRHEAD1    BILLNG RECAP HEADLINES                 
         MVC   PSECOND+1(L'BRHEAD2),BRHEAD2                                     
*                                                                               
HEADUP15 GOTO1 ACREPORT                                                         
         MVC   GETLNE,DETLNE                                                    
         BAS   RE,SKIPLNE          SKIP TO DETAIL LINE                          
         B     XIT                                                              
*                                                                               
*              GET JOB NAME                                                     
*                                                                               
GETJOB   NTR1                                                                   
         USING ACKEYD,R3                                                        
         L     R3,AIO2                                                          
         LA    R5,LSTWRK                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'SJ'                                             
         MVC   ACKEYACC+3(12),SRTCLI                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(R3),(R3)                        
         CLC   ACKEYACC+3(12),SRTCLI                                            
         BE    *+8                                                              
         B     XIT                 IF YOU CAN'T FIND JOB DONT WORRY             
*                                  ABOUT THE JOB NAME                           
*        DC    H'0'                CAN'T READ JOB                               
         SPACE 1                                                                
         USING ACNAMED,R4                                                       
         GOTO1 GETEL,DMCB,(X'20',(R3)),0                                        
         CLI   ELERR,0                                                          
         BNE   XIT                 NO NAME ELEMENT                              
         L     R4,ELADDR                                                        
         ZIC   R1,ACNMLEN          JOB NAME                                     
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ACNMNAME                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SKIP TO NEW LINE                                      
         SPACE 1                                                                
SKIPLNE  NTR1                                                                   
         ZIC   R1,GETLNE           LINE NUMBER I WANT TO SKIP TO                
         ZIC   R2,LINE             CURRENT LINE                                 
         SR    R1,R2                                                            
         BZ    XIT                 ALREADY AT THE LINE I WANT                   
         BP    *+6                                                              
         DC    H'0'                LOST TRACK                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SKIP+1(3),DUB+6(2)                                               
         MVC   SKIP(2),=C'BL'                                                   
         AR    R2,R1               ADD SKIP COUNT  TO LINE                      
         STC   R2,LINE                                                          
         GOTO1 PRINT,DMCB,P,SKIP   AND SKIP                                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
GETEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,ELIST,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         B     XIT                                                              
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE 1                                                                
RELOTAB  DS    0A                                                               
         DC    V(SORTER)                                                        
         DC    V(HELLO)                                                         
         DC    V(PRNTBL)                                                        
         DC    A(IO)                                                            
         DC    A(IO2)                                                           
         DC    X'FF'                                                            
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
         SPACE 1                                                                
*              LINE CONTROL                                                     
SPROGAR  DC    AL1(7,12,14,16,18,51,52,58,59,63)                                
SPROGBR  DC    AL1(1,6,7,9,12,45,46,52,53,63)                                   
*                                                                               
BRHEAD1  DC    C'      INVOICE      ORIGINAL INVOICE JOB     JOB       X        
                                 AMOUNT'                                        
BRHEAD2  DC    C'   DATE     NUMBER      AMOUNT      NUMBER   DESCRIPTIX        
               ON                  DUE '                                        
         SPACE 1                                                                
MESSA    DC    C'-   DAYS'                                                      
MESSB    DC    C'OVER    DAYS'                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
IO       DS    1008C                                                            
IO2      DS    1008C                                                            
         EJECT                                                                  
*              DSECT FOR STORAGE AREA                                           
         SPACE 1                                                                
ACARD    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
SORTER   DS    A                                                                
HELLO    DS    A                                                                
PRNTBL   DS    A                                                                
AIO      DS    A                                                                
AIO2     DS    A                                                                
         SPACE 1                                                                
AELEMS   DS    0F                                                               
AELEM1A  DS    A                   X'1A' ELEMENT ADDRESS OF TRANSACTION         
AELEM23  DS    A                   X'23' ELEMENT ADDRESS OF TRANSACTION         
AELEM4F  DS    A                   X'4F' ELEMENT ADDRESS OF TRANSACTION         
AELEM61  DS    A                   X'61' ELEMENT ADDRESS OF TRANSACTION         
AELEMD9  DS    A                   X'D9' ELEMENT ADDRESS OF TRANSACTION         
AELEMSQ  EQU   *-AELEMS                                                         
         SPACE 1                                                                
GETLNE   DS    CL1                                                              
PGEMAX   DS    CL1                                                              
         SPACE 1                                                                
RPTLNES  DS    0CL10               SIGNIFICANT REPORT LINE NUMBERS              
HEADLNE  DS    CL1                 LINE NUMBER FOR FIRST HEADLINE               
PGELNE   DS    CL1                 LINE NUMBER TO PRINT PAGE                    
DTELNE   DS    CL1                 LINE NUMBER TO PRINT DATE                    
MIDLNE   DS    CL1                 LINE NUMBER FOR MIDLINES                     
DETLNE   DS    CL1                 LINE NUMBER FOR DETAIL                       
FUTLNE   DS    CL1                 LINE NUMBER FOR FUTURE DUE                   
SUMLNE   DS    CL1                 LINE NUMBER FOR SUMMARY                      
DETMAX   DS    CL1                 LAST DETAIL LINE                             
TOTLNE   DS    CL1                 LINE NUMBER FOR ACCOUNT TOTAL                
LSTMAX   DS    CL1                 LAST LINE ON PAGE                            
         SPACE 1                                                                
ALSORT   DS    A                   A(LAST SORT RECORD)                          
PROFS    DS    0CL16               PROGRAM PROFILES                             
PROFAGE  DS    CL1                 AGE BY INV OR DUE DATE (DEFAULT INV)         
PROFDUE  DS    CL1                 PRINT JOB NUM OR DUE DATE (JOB)              
PROFDAYS DS    CL1                 AGEING INCREMENT (DEFAULT 30)                
PROFDESC DS    CL1                 PRNT BILL SOURCE OR DESCRIP(DESCRIP)         
PROFPART DS    CL1                 FLAG PARTIAL AMOUNTS Y OR N (N)              
PROFCURR DS    CL1                 CURRENT BAL AS PAST DUE Y OR N (N)           
PROFBRKS DS    CL1                 NUMBER OF AGING BREAKOUTS                    
PROFLEFT DS    CL9                 9 REMAINING PROFILES AVAILABLE               
         SPACE 1                                                                
SRTWRK   DS    (SRTLNQ)C           WORK AREA FOR SORT RECORD                    
LSTWRK   DS    (SRTLNQ)C           WORK AREA FOR LAST RECORD                    
         SPACE 1                                                                
ARPWK1   DS    (ARPLNQ)C           PRINT LINE 1                                 
ARPWK2   DS    (ARPLNQ)C           PRINT LINE 2                                 
ARPWK3   DS    (ARPLNQ)C           PRINT LINE 3                                 
ARPWK4   DS    (ARPLNQ)C           PRINT LINE 4                                 
         SPACE 1                                                                
ENDATE0  DS    CL6                 AS OF DATE YYMMDD                            
ENDATE1  DS    CL3                            PWOS                              
STDATE1  DS    CL3                            PWOS                              
         SPACE 1                                                                
*                                  AGEING DATES (DEFAULT IS 30 DAYS)            
PERDTE1  DS    CL3                           -30 DAYS  CURRENT                  
PERDTE2  DS    CL3                           -60 DAYS  30 PAST DUE              
PERDTE3  DS    CL3                           -90 DAYS  60 PAST DUE              
PERDTE4  DS    CL3                           -120 DAYS 90 PAST DUE              
         SPACE 1                                                                
ACCTOTS  DS    (PERALNQ)C          ACCOUNT TOTALS                               
REQTOTS  DS    (PERALNQ)C          REQUEST TOTALS                               
         SPACE 1                                                                
ELIST    DS    3F                  FOR GETL, ADDEL, DELEL                       
ELERR    DS    CL1                                                              
         ORG   ELERR               ERROR CODE FROM HELLO                        
ELADDR   DS    F                   ADDRESS OF ELEMENT FROM HELLO                
         DS    2F                                                               
         SPACE 1                                                                
ACTIVITY DS    CL1                                                              
TOTSW    DS    CL1                                                              
SKIP     DS    CL4                                                              
         EJECT                                                                  
*              DSECT FOR SORT RECORD                                            
         SPACE 1                                                                
SRTD     DSECT                                                                  
SRTKEY   DS    0C                                                               
SRTACC   DS    CL15                RECEIVABLE ACCOUNT                           
SRTDTE   DS    CL3                 INVOICE DATE (PWOS)                          
SRTINV   DS    CL6                 INVOICE NUMBER                               
SRTKLNQ  EQU   *-SRTKEY            KEY LENGTH                                   
SRTCLI   DS    CL3                 CLIENT                                       
SRTPRD   DS    CL3                 PRODUCT                                      
SRTJOB   DS    CL6                 JOB                                          
SRTDSC   DS    CL22                DESCRIPTION                                  
SRTCON   DS    CL15                BILLING SOURCE                               
SRTDUE   DS    CL3                 DUE DATE X'61'                               
SRTDBS   DS    PL8                 DEBITS(ORIGINAL AMOUNT)                      
SRTCRD   DS    PL8                 CREDITS(PAYMENTS)                            
SRTLNQ   EQU   *-SRTKEY            RECORD LENGTH                                
         EJECT                                                                  
*              DSECT FOR ACCUMS                                                 
         SPACE 1                                                                
PERAD    DSECT                                                                  
PERAMT1  DS    PL8                 CURRENT                                      
PERAMT2  DS    PL8                 30 PAST DUE                                  
PERAMT3  DS    PL8                 60 PAST DUE                                  
PERAMT4  DS    PL8                 90 PAST DUE                                  
PERAMT5  DS    PL8                 OVER 90                                      
PERAMT   DS    PL8                 TOTAL                                        
PERAMTF  DS    PL8                 DUE IN FUTURE                                
PERALNQ  EQU   *-PERAD             LENGTH OF AREA                               
PERACNT  EQU   (*-PERAD)/8         NUMBER OF BUCKETS                            
         EJECT                                                                  
*              DSECT FOR PRINT LINE                                             
         SPACE 1                                                                
ARPD     DSECT                                                                  
ARP1     DS    CL1                                                              
ARPDTE   DS    CL8                 INVOICE DATE                                 
         DS    CL4                                                              
ARPINV   DS    CL6                 INVOICE NUMBER                               
         DS    CL2                                                              
ARPORG   DS    CL14                ORIGINAL AMOUNT                              
         DS    CL1                                                              
ARPDDAT  DS    0CL8                DUE DATE                                     
         DS    CL1                                                              
ARPJOB   DS    CL6                 JOB NUMBER                                   
         DS    CL2                                                              
ARPDSC   DS    CL22                DESCRIPTION                                  
         SPACE 1                                                                
         ORG   ARPJOB                                                           
ARPDAY   DS    CL15                NUMBER OF DAYS                               
         DS    CL1                                                              
ARPTOT   DS    CL14                AGED AMOUNT                                  
         SPACE 1                                                                
         DS    CL1                                                              
ARPDUE   DS    CL14                AMOUNT DUE                                   
ARPPART  DS    CL1                 FLAG WITH * IF PARTIAL PAYMENT               
ARPLNQ   EQU   *-ARP1              LENGTH OF LINE                               
         EJECT                                                                  
*        DDLOGOD                                                                
*        ACGENBOTH                                                              
*        ACGENPOST                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        DDCNTRL                                                                
*        DDREPXTRAD                                                             
*        DDREPMASTD                                                             
*        DDREMOTED                                                              
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048ACREPAR02 05/01/02'                                      
         END                                                                    
