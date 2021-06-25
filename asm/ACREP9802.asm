*          DATA SET ACREP9802  AT LEVEL 017 AS OF 02/12/20                      
*PHASE AC9802C                                                                  
*INCLUDE ACJOBMNT                                                               
*INCLUDE ACRAPPER                                                               
*INCLUDE BUFFERIN                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE RANDOM                                                                 
         TITLE 'PEEL/UNPEEL PROGRAM'                                            
***********************************************************************         
* INPUTS:                                                             *         
*    QOPT1: D - DRAFT REQUEST    (PEEL/UNPEEL)                        *         
*    QOPT2: C - CLOSE JOB OPTION (PEEL ONLY)                          *         
*    QOPT3: Y - PEEL ALL IF BALANCE=0 (PEEL ONLY)                     *         
*    QOPT4: Y - KEEP COUNTER FOR PEELED/UNPEELED TRANSACTIONS         *         
*    QOPT5: Y - BUILD THE UNIT LEDGER LIST                            *         
*    QOPT6: Y - TAPE OVERRIDE                                         *         
*                                                                     *         
*              PROFILE VALUES  (PROFILES ARE SETUP ONLY FOR PEELING)  *         
*                                                                     *         
* 1,2,3,4      **** NO AUDITING REQUIRED                              *         
*              INNN AUDIT EVERY NNN ITEMS                             *         
*              NNNN AUDIT EVERY $NNN,N00                              *         
* 5            TREAT SR AS A BALANCE FORWARD LEDGER                   *         
* 6            PEEL LOCKED ACCOUNTS ONLY (MAINLY FOR 1R LEDGER)       *         
* 7            PEEL ZERO BALANCE ACCTS (BAL FRWD ONLY)                *         
***********************************************************************         
* ASHA 015 28MAR18  PEELING OF UNIT G IS NOT ALLOWED        SPEC-14038*         
* VGUP 016 07AUG19 RELINK FOR ACJOBMNT                      DSRD-22480          
* JSAY 017 16OCT19  AMENDING PEELED FOR DATE IN ABLELD ELE  SPEC-27345*         
***********************************************************************         
AC9802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC98**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING WORKD,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,PROCRQST                                                    
         BE    RQST                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,UNITFRST                                                    
         BE    UNTF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF                                                             
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                                                             
         CLI   MODE,ACCLAST                                                     
         BE    ACCL                                                             
         CLI   MODE,LEDGLAST                                                    
         BE    LDGL                                                             
         CLI   MODE,UNITLAST                                                    
         BE    UNTL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
XIT      XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
                                                                                
RUNF     ZAP   RUNBF,=P'0'                                                      
         ZAP   RUNDR,=P'0'                                                      
         ZAP   RUNCR,=P'0'                                                      
         ZAP   RUNUP,=P'0'                                                      
         MVI   ACTSW,0                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS REQUEST                                                     *         
***********************************************************************         
                                                                                
RQST     L     RE,ADQSTACK                                                      
         USING ACQD,RE                                                          
         L     R5,AMONACC                                                       
         USING ACMD,R5                                                          
         CLI   ACQUNT,C' '        SPECIFIC REQUEST                              
         BH    XIT                                                              
         CLI   ACQOPT5,C'Y'                                                     
         BNE   XIT                                                              
         TM    ACMINDS5,ACMIAPPL                                                
         BO    XIT                                                              
         BAS   RE,SETUL            BUILD THE U/L LIST                           
         OI    ACMINDS5,ACMIAPPL                                                
         OI    RUNOPT,RUNDDS                                                    
         B     XIT                                                              
         DROP  R5,RE                                                            
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
                                                                                
REQF     L     R5,AMONACC                                                       
         USING ACMD,R5                                                          
         MVI   RCSUBPRG,0                                                       
         MVI   REQOPT,0                                                         
         NI    ACTSW,X'FF'-ACTREQ                                               
         MVC   SVUNTLDG,QUNIT       SAVE REQUEST UNIT/LEDGER                    
         CLC   QUNIT(2),SPACES                                                  
         BNE   *+8                                                              
         OI    REQOPT,REQALL        SET ALL UNITS                               
         MVC   PAGE,=H'1'                                                       
         ZAP   REQBF,=P'0'                                                      
         ZAP   REQDR,=P'0'                                                      
         ZAP   REQCR,=P'0'                                                      
         ZAP   REQUP,=P'0'                                                      
         ZAP   PKCOUNT,=P'0'       TOTAL # OF TRANSACTIONS                      
         CLI   QUNIT,C'G'                PEELING OF UNIT G NOT ALLOWED          
         BE    UNTERR                                                           
         MVI   FCSEQ,FCSEQNEW                                                   
         CLI   QOPT1,C'D'                DRAFT REQUEST ?                        
         BE    SETDRFT                                                          
         CLI   RCPOSTNG,C'N'                                                    
         BE    SETDRFT                                                          
         CLI   RCWRITE,C'N'                                                     
         BNE   *+8                                                              
SETDRFT  OI    REQOPT,DRAFT                                                     
         CLI   RCRERUN,C'Y'              RERUN=YES?                             
         BNE   *+8                                                              
         OI    REQOPT,RERUN+DRAFT        RERUN - DRAFT                          
         CLC   QPROG,=C'UP'              ARE WE UNPEELING                       
         BNE   *+12                                                             
         OI    REQOPT,REQUNP             TURN UNPEELING ON                      
         MVI   RCSUBPRG,3                                                       
*                                                                               
         L     R5,ADCMPEL                                                       
         USING CPYELD,R5                                                        
         MVC   CASHLDG,CPYBANK           CASH LEDGER                            
         MVC   RECVLDG,CPYRECV           RECEIVABLE LEDGER                      
         MVC   PRODLDG,CPYPROD           JOB LEDGER                             
         TM    CPYSTAT6,CPYSRAPP         ACTIVITY POINTERS?                     
         BNO   *+8                                                              
         OI    REQOPT,RAPP                                                      
         TM    CPYSTAT4,CPYSOFF2         TWO CHARACTER OFFICE                   
         BNO   *+8                                                              
         OI    REQOPT,OFF2                                                      
         DROP  R5                                                               
*                                                                               
         L     R5,AMONACC                                                       
         USING ACMD,R5                                                          
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAY2)                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAY3)                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(X'20',TODAY0)                            
         XC    ACMFDTE,ACMFDTE           FOR P&L ITEMS                          
         TM    REQOPT,REQUNP             ARE WE UNPEELING                       
         BO    REQF05                    YES WE DON'T NEED MOS                  
*                                                                               
         CLC   QMOSEND,SPACES            USE MOS END                            
         BNE   *+6                                                              
         DC    H'0'                      MUST HAVE MOSEND                       
*                                                                               
         MVC   WORK(4),QMOSEND           GET PEELED FOR DATE IN YYMM            
         MVC   WORK+4(2),=C'01'          SET IT IN YYMMDD FORMAT                
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6) CONVERT IT IN YMD                
         MVC   PLFORDT,WORK+6            COMPRESSED PEELED FOR DATE             
*                                                                               
         MVC   RCMOS,ACMMEND             SET END MONTH                          
         MVC   DATE3(2),RCMOS            FIRST DAY OF LAST PEEL MONTH           
         MVI   DATE3+2,1                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(1,DATE3),(X'20',DUB)                                
         MVC   YYMM,DUB                    YY/MM                                
*                                                                               
         GOTO1 ADDAY,DMCB,(C'M',DUB),WORK,1                                     
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,NXTMON) FIRST DAY OF NEXT MONTH          
*                                                                               
         TM    REQOPT,REQALL              TEST ALL UNITS                        
         BO    REQF05                     YES, SKIP AUDIT                       
         TM    RUNOPT,RUNDDS                                                    
         BO    REQF05                     YES, SKIP AUDIT                       
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
*                                                                               
REQF05   NI    TOUTSW,X'FF'-(TOUTPUT)                                           
         TM    REQOPT,DRAFT+REQUNP TEST DRAFT  OR UNPEEL                        
         BNZ   REQFX                                                            
         CLI   QOPT6,C'Y'          TEST OVERRIDE                                
         BE    *+12                YES,                                         
         CLI   RCWRITE,C'N'                                                     
         BE    REQFX                                                            
         OI    TOUTSW,TOUTPUT                                                   
         TM    TOUTSW,TOUTOPN      TEST TAPE OPENED                             
         BO    REQFX                                                            
*                                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         L     RF,MCAEXTRA                                                      
         USING MCEXTRA,RF                                                       
         MVC   DSPAGY,MCAGYCOD                                                  
         CLI   DSPAGY,C' '                                                      
         BNH   REQERR                                                           
         MVC   DSPYR(4),YYMM             YY/MM                                  
         MVC   DSPRUN,TODAY0                                                    
         DROP  RF                                                               
*                                                                               
         GOTO1 DYNALLOC,DMCB,(X'02',DDPARM),(X'FE',DSPARM),            X        
               (X'20',0),0                                                      
         ICM   RF,15,DMCB+12                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,ATOUT                                                         
         OPEN  ((R4),(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    TOUTSW,TOUTOPN                                                   
*                                                                               
REQFX    B     XIT                                                              
         DROP  R5                                                               
*                                                                               
REQERR   MVC   P+1(L'ERRMSG),ERRMSG                                             
         GOTO1 ACREPORT                                                         
         MVI   FCRDACC,C'N'                                                     
         MVI   FCRDTRNS,C'N'                                                    
         B     XIT                                                              
*                                                                               
UNTERR   MVC   P+1(L'UNERMS),UNERMS                                             
         GOTO1 ACREPORT                                                         
         MVI   FCRDACC,C'N'                                                     
         MVI   FCRDTRNS,C'N'                                                    
         B     XIT                                                              
*                                                                               
ERRMSG   DC    C'MISSING "AGENCY LABEL" ON GEN ACCESS RECORD'                   
UNERMS   DC    C'PEELING FOR UNIT G IS NOT ALLOWED'                             
         EJECT                                                                  
***********************************************************************         
* UNIT FIRST                                                          *         
***********************************************************************         
                                                                                
UNTF     ZAP   UNTBF,=P'0'                                                      
         ZAP   UNTDR,=P'0'                                                      
         ZAP   UNTCR,=P'0'                                                      
         ZAP   UNTUP,=P'0'                                                      
         NI    ACTSW,X'FF'-ACTUNT                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LEDGER FIRST                                                        *         
***********************************************************************         
                                                                                
LDGF     MVI   LDGFLG,0                                                         
         ZAP   LEDBF,=P'0'                                                      
         ZAP   LEDDR,=P'0'                                                      
         ZAP   LEDCR,=P'0'                                                      
         ZAP   LEDUP,=P'0'                                                      
         NI    ACTSW,X'FF'-ACTLDG                                               
         TM    REQOPT,REQALL             TEST ALL UNITS                         
         BO    LDGF10                    YES, SKIP AUDIT                        
         TM    RUNOPT,RUNDDS                                                    
         BO    LDGF10                                                           
*                                                                               
         XC    RANDINP,RANDINP           CLEAR 'RANDOM' INPUT NUMBER            
         OC    RCFFPARM,RCFFPARM                                                
         BZ    *+18                                                             
         PACK  DUB,RCFFPARM                                                     
         CVB   R1,DUB                                                           
         STCM  R1,15,RANDINP                                                    
*                                                                               
         MVI   AUDIT,0                                                          
         MVC   AUDLIN,SPACES                                                    
         MVC   AUDLNLG,=C'NEW LEDGER'                                           
         L     R2,ADLEDGER                                                      
         MVC   AUDLLGC,2(R2)             LEDGER CODE                            
         L     R2,ADLDGNAM                                                      
         USING NAMELD,R2                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   AUDLLGN(0),NAMEREC        LEDGER NAME                            
         DROP  R2                                                               
*                                                                               
         LA    R6,AUDBUKS                                                       
         USING AUDD,R6                                                          
         LA    RF,AUDBK                                                         
         LA    R0,AUDBKN                 CLEAR AUDIT BUCKETS                    
         ZAP   0(8,RF),=P'0'                                                    
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         CLI   PROGPROF,C'*'                                                    
         BE    LDGF10                    NO AUDITING REQUIRED                   
         CLI   PROGPROF,C'I'                                                    
         BNE   LDGF03                                                           
*                                                                               
         OI    AUDIT,AUDIT#              I FOR ITEM COUNT                       
         PACK  AUDTRG,PROGPROF+1(3)                                             
         B     LDGF05                                                           
*                                                                               
LDGF03   TM    PROGPROF,X'F0'                                                   
         BNO   LDGF10                                                           
         OI    AUDIT,AUDIT$              FINANCIAL                              
         PACK  AUDTRG,PROGPROF(4)                                               
         MP    AUDTRG,=P'10000'          TRIGGER IN 100S OF $S                  
*                                                                               
LDGF05   ZAP   DUB,AUDTRG                                                       
         CVB   R2,DUB                                                           
         GOTO1 RANDOM,DMCB,(R2),0                                               
         L     R1,DMCB+4                                                        
         ICM   RF,15,RANDINP             USE 'RANDOM' INPUT NUMBER              
         BZ    *+6                                                              
         LR    R1,RF                                                            
*                                                                               
         CVD   R1,DUB                                                           
         TM    AUDIT,AUDIT$                                                     
         BNO   LDGF07                                                           
         NI    DUB+7,X'0F'               CONVERT TO WHOLE $S                    
         NI    DUB+6,X'F0'                                                      
         ZAP   AUDTRGD,DUB                                                      
         ZAP   AUDTRGC,DUB                                                      
*                                                                               
LDGF07   ZAP   AUDTRG#,DUB                                                      
         OI    AUDTRG#+L'AUDTRG#-1,X'0F'                                        
         LA    R3,AUDLITMS                                                      
         MVC   0(8,R3),=C'TRIGGER='                                             
         LA    R3,8(R3)                                                         
         TM    AUDIT,AUDIT$              AUDIT DOLLARS                          
         BO    LDGF08                                                           
         UNPK  0(8,R3),AUDTRG                                                   
         B     LDGF09                                                           
*                                                                               
LDGF08   MVI   0(R3),C'$'                                                       
         CURED AUDTRG,(12,1(R3)),2,ALIGN=LEFT                                   
*                                                                               
LDGF09   CLI   0(R3),C' '                                                       
         BNH   *+12                                                             
         LA    R3,1(R3)                                                         
         B     LDGF09                                                           
         MVC   1(7,R3),=C'RANDOM='                                              
         UNPK  8(8,R3),AUDTRG#                                                  
         OI    15(R3),X'F0'                                                     
*                                                                               
         LA    R3,1                                                             
         STCM  R3,15,AUDSEQ                                                     
         GOTO1 ADSORTER,DMCB,=C'PUT',AUDREC    PUT TO SORTER                    
         DROP  R6                                                               
*                                                                               
LDGF10   L     R2,ADLEDGER               GET SPECIAL LEDGER STATUS              
         USING LDGRECD,R2                                                       
         SR    R1,R1                                                            
         LA    R3,LDGTAB                                                        
LDGF11   IC    R1,2(R3)                  LENGTH OF COMPARE                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LDGKUNT(0),0(R3)                                                 
         BE    LDGF13                                                           
         LA    R3,L'LDGTAB(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BNE   LDGF11                                                           
         B     LDGF15                    NOT IN TABLE                           
*                                                                               
LDGF13   IC    R1,3(R3)                  STATUS BYTE TO R1                      
         EX    R1,*+8                                                           
         B     *+8                                                              
         OI    LDGFLG,0                  SET STATUS FLAG(S)                     
*                                                                               
LDGF15   CLC   CASHLDG,LDGKUNT           CASH LEDGER ?                          
         BNE   *+8                                                              
         OI    LDGFLG,LDGCASH            SET CASH FLAG                          
*                                                                               
         CLI   QOPT3,C'Y'                PEEL ALL IF TOTAL=0                    
         BE    XIT                                                              
*                                                                               
         CLC   RECVLDG,LDGKUNT           RECEIVABLE LEDGER ?                    
         BNE   *+20                                                             
         CLI   PROGPROF+4,C'Y'           TREAT AS BALANCE FWRD ?                
         BE    XIT                       YES, NOT RECEIVABLE                    
         OI    LDGFLG,LDGRECV            SET RECEIVABLE FLAG                    
         B     XIT                                                              
*                                                                               
         L     R4,ADLDGEL                                                       
         USING LDGELD,R4                                                        
         CLI   LDGLIKE,LDGLRECV          LIKE RECEIVABLE ?                      
         BNE   *+8                                                              
         OI    LDGFLG,LDGLREC            TREAT 'LIKE RECEIVABLE'                
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT (IF PEELING)                                        *         
***********************************************************************         
                                                                                
PACC     DS    0H                                                               
         TM    REQOPT,REQUNP             ARE WE UNPEELING                       
         BO    PACCUN                    GO UNPEEL                              
*                                                                               
         MVI   FCRDTRNS,C'N'                                                    
         ZAP   ACCBF,=P'0'                                                      
         ZAP   ACCDR,=P'0'                                                      
         ZAP   ACCCR,=P'0'                                                      
         ZAP   ACCUP,=P'0'                                                      
*                                                                               
         L     R1,ADUNIT                                                        
         CLI   1(R1),C'G'                                                       
         BE    XIT                                                              
*                                                                               
         TM    LDGFLG,LDGPROD            TEST PRODUCTION                        
         BZ    PACC01                                                           
         CLC   SVUNTLDG,=C'SJ'           TESTED REQUESTED SJ                    
         BNE   XIT                       NO, SKIP IT                            
*                                                                               
PACC01   L     RF,AOFFTAB                MARK END OF OFFICE TAB                 
         MVI   0(RF),X'FF'                                                      
         CLI   PROGPROF+5,C'Y'           LOCKED ACCOUNTS ONLY                   
         BNE   PACC03                                                           
         ICM   R4,15,ADACCSTA                                                   
         BZ    PACC03                                                           
         USING RSTELD,R4                                                        
         TM    RSTSTAT1,RSTSACIL         TEST ACCOUNT LOCKED                    
         BNO   XIT                       NO, SKIP IT                            
*                                                                               
PACC03   L     R4,ADACC                                                         
         SR    R3,R3                                                            
         ZAP   DUB,=P'0'                                                        
         MVI   ELCODE,APOELQ             GET PEEL ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   PACC05                                                           
         LR    R3,R4                                                            
         USING APOELD,R3                                                        
         CLC   APOPLDT,TODAY3                                                   
         BNE   *+16                                                             
         ZAP   DUB,APODR                 GET NET PEELED AMOUNT                  
         SP    DUB,APOCR                                                        
         TM    REQOPT,RERUN              TEST RERUN                             
         BO    PACC05                    YES,                                   
         CLC   APOPLDT,TODAY3            TEST ALREADY PEELED TODAY              
         BE    XIT                       YES, SKIP IT                           
         DROP  R3                                                               
*                                                                               
PACC05   MVI   ACCFLG,0                  INITIALIZE ACCOUNT FLAG                
         ICM   R4,15,ADACCBAL                                                   
         BZ    XIT                                                              
         USING ABLELD,R4                                                        
         ZAP   ACCBF,ABLFRWD                                                    
         SP    ACCBF,DUB                 ADJUST BBF (IF RERUN)                  
*                                                                               
         MVC   ACCODE,SPACES                                                    
         LA    RE,ACCODE                 PARSE ACCOUNT CODE                     
         L     RF,ADACC                                                         
         LA    RF,ACTKACT-ACTKEY(RF)                                            
         L     R5,ADLDGHIR                                                      
         USING ACLELD,R5                                                        
         LA    R2,ACLVLEN                                                       
         SR    R3,R3                                                            
         SR    R1,R1                                                            
PACC07   IC    R1,0(R2)                                                         
         SR    R1,R3                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         CLI   0(R2),12                  TEST ALL 12 MOVED ?                    
         BE    PACC09                                                           
         AHI   R1,1                                                             
         LA    RF,0(R1,RF)                                                      
         LA    RE,1(R1,RE)                                                      
         AR    R3,R1                     R3=LENGTH USED SO FAR                  
         LA    R2,L'ACLVALS(R2)                                                 
         B     PACC07                                                           
*                                                                               
PACC09   MVC   ACNAME,SPACES             GET ACCOUNT NAME                       
         L     R5,ADACCNAM                                                      
         USING NAMELD,R5                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACNAME(0),NAMEREC                                                
*                                                                               
         TM    LDGFLG,LDGPROD+LDGTALN PRODUCTION OR TALENT ?                    
         BZ    PACC13                                                           
         L     R4,ADACCSTA                                                      
         USING RSTELD,R4                                                        
         CLC   RSTTDATE(2),DATE3         ACTIVITY LATER THAN END MONTH?         
         BH    XIT                       YES, SKIP IT                           
         CLI   QOPT2,C'C'                CLOSE JOB OPTION ?                     
         BE    JOBCLSE                                                          
         TM    RSTSTAT1,RSTSACIC         JOB CLOSED ?                           
         BNO   XIT                       NO, CAN'T PEEL                         
         TM    LDGFLG,LDGTALN            TALENT ?                               
         BO    PACC17                    YES, SKIP BALANCE CHECK                
         L     R4,ADACCBAL                                                      
         USING ABLELD,R4                                                        
         CP    ABLDR,ABLCR               BALANCE = 0                            
         BNE   XIT                       NO, SKIP IT                            
         OI    ACCFLG,ACCZRO             SET - BALANCE MUST BE ZERO             
         B     PACC15                    AND LOOK AHEAD                         
*                                                                               
PACC13   CLI   QOPT3,C'Y'                PEEL IF BALANCE=0                      
         BNE   *+12                                                             
         OI    ACCFLG,ACCZRO             SET - BALANCE MUST BE ZERO             
         B     PACC15                                                           
*                                                                               
         TM    LDGFLG,LDGRECV+LDGLREC RECEIVABLE OR THE LIKE ?                  
         BNZ   PACC15                                                           
         TM    LDGFLG,LDGPAYL            PAYABLE ?                              
         BNZ   PACC15                                                           
         CLI   PROGPROF+6,C'Y'           PEEL ONLY ZERO BALANCE ACCTS?          
         BNE   PACC17                                                           
         OI    ACCFLG,ACCZRO             SET - BALANCE MUST BE ZERO             
*                                                                               
PACC15   BAS   RE,LOOK                   LOOK FOR TRANSACTIONS                  
         TM    ACCFLG,ACCTRN             TEST ANY TRANSACTIONS ?                
         BNO   XIT                       NO, SKIP PROCTRNS                      
         TM    ACCFLG,ACCZRO             TEST BALANCE MUST BE ZERO              
         BNO   PACC17                                                           
         CP    BALANCE,=P'0'             IS IT ?                                
         BNE   XIT                                                              
*                                                                               
PACC17   MVI   FCRDTRNS,C'Y'             SET TO PROCESS TRANSACTIONS            
         MVI   FCRNTIME,C'Y'        PASS R AND N TIME FOR PRODUCTION            
         B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CLOSE A JOB                                              *         
***********************************************************************         
                                                                                
JOBCLSE  MVI   RCSUBPRG,2                                                       
         L     R4,ADACCSTA                                                      
         USING RSTELD,R4                                                        
         TM    RSTSTAT1,RSTSACIC         JOB ALREADY CLOSED ?                   
         BO    JOBCLSE3                                                         
*                                                                               
         LA    R3,WORK                                                          
         USING MPARMD,R3                                                        
         L     RF,ADACC                                                         
         ST    RF,MJOB                                                          
         MVI   MACTION,C'C'              ACTION IS CLOSE                        
         L     RF,ADCOMFAC                                                      
         ST    RF,MCOMFACS                                                      
*                                                                               
         L     RE,AMONACC                                                       
         USING ACMD,RE                                                          
         L     RE,ACMAPRAT                                                      
         ST    RE,MPRORATA                                                      
         DROP  RE                                                               
*                                                                               
         MVI   MDRAFT,C'Y'                                                      
         TM    REQOPT,DRAFT                                                     
         BO    *+8                       DRAFT PEEL                             
         MVI   MDRAFT,C'N'                                                      
*                                                                               
         XC    MPERSON,MPERSON                                                  
         MVC   MGETOPT,GETOPT                                                   
         MVC   MRAPPER,ACRAPPER                                                 
         GOTO1 ACJOBMNT,WORK                                                    
         LA    R1,WORK                                                          
         CLI   4(R1),0                   ANY ERRORS                             
         BNE   XIT                       YES,                                   
*                                                                               
JOBCLSE3 L     R4,ADACCBAL                                                      
         USING ABLELD,R4                                                        
         AP    ACCDR,ABLDR                                                      
         AP    ACCCR,ABLCR                                                      
*                                                                               
         LA    R5,P                                                             
         USING PLD,R5                                                           
         L     R4,ADACCSTA                                                      
         USING RSTELD,R4                                                        
         GOTO1 DATCON,DMCB,(1,RSTTDATE),(8,PLJCLAD)                             
         TM    RSTSTAT1,RSTSACIC                                                
         BO    JOBCLSE5                  PREVIOUSLY CLOSED                      
         GOTO1 DATCON,DMCB,(1,TODAY3),(8,PLJCDAT)                               
         MVI   PLJC2DAY,C'*'                                                    
         B     JOBCLSE7                                                         
*                                                                               
JOBCLSE5 L     R4,ADACC                                                         
         MVI   ELCODE,JOBELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   JOBCLSE7                                                         
         USING JOBELD,R4                                                        
         GOTO1 DATCON,DMCB,(1,JOBCDATE),(8,PLJCDAT)                             
*                                                                               
JOBCLSE7 LA    RE,ACCBF                  ADD ACCOUNT TOTALS                     
         LA    RF,UNTBF                  TO HIGHER LEVELS                       
         LA    R0,4                      UNIT,LEDGER,REQ,RUN                    
JOBCLSE8 AP    0(8,RF),0(8,RE)                                                  
         AP    8(8,RF),8(8,RE)                                                  
         AP    16(8,RF),16(8,RE)                                                
         AP    24(8,RF),24(8,RE)                                                
         LA    RF,32(RF)                                                        
         BCT   R0,JOBCLSE8                                                      
*                                                                               
         GOTO1 FRMT,DMCB,ACCBF           FORMAT ACCUMS                          
         MVC   PLCODE,ACCODE             ACCOUNT CODE                           
         MVC   PLNAME,ACNAME                "    NAME                           
         BAS   RE,PRNT                                                          
         B     XIT                                                              
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* LOOK AHEAD TO FIND TRANSACTIONS                                     *         
***********************************************************************         
                                                                                
LOOK     NTR1  ,                                                                
         ZAP   BALANCE,=P'0'                                                    
         TM    LDGFLG,LDGPAYL+LDGRECV+LDGLREC                                   
         BZ    LOOK01                                                           
         OI    ACCFLG,ACCBUF             SET BUFFER IN USE                      
         GOTO1 BUFFERIN,DMCB,('BUFFAINI',BUFF),(0),ADCOMFAC                     
*                                                                               
LOOK01   L     RF,ADACC                                                         
         LA    R2,DKEY                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,0(RF)              READ ACCOUNT                           
         BAS   RE,DMHGH                                                         
         MVC   AIO,AIO1                                                         
*                                                                               
LOOK03   CLC   DIR(L'ACTKCULA),DKEY TEST SAME ACCOUNT                           
         BNE   XIT                                                              
         LA    R2,DIR                                                           
         USING TRNRECD,R2                                                       
         BAS   RE,DMGETR                 GET THE RECORD                         
         L     R2,AIO                                                           
         CLI   TRNRFST,TRNELQ            MUST BE TRANSACTION                    
         BNE   LOOK17                                                           
         TM    TRNRSTAT,TRNSDRFT         IGNORE DRAFT TRANSACTIONS              
         BNZ   LOOK17                                                           
         CLC   TRNRSMOS,RCMOS            TEST MOS                               
         BH    LOOK17                                                           
         LA    R4,TRNRFST                                                       
         MVI   ELCODE,TRSELQ             GET STATUS ELEMENT                     
         BAS   RE,NEXTEL                                                        
         BNE   LOOK17                                                           
         USING TRSELD,R4                                                        
         CLC   TRSRMOS,RCMOS             TEST REVERSE MONTH                     
         BH    LOOK17                                                           
         TM    TRSSTAT,TRSSOFFS          OFFSET ITEM ?                          
         BNO   *+8                                                              
         BO    LOOK17                    AS PER GBOS & ACCPAK DATABASE          
*                                        ITEM #45407                            
         TM    REQOPT,RERUN              RERUN ?                                
         BNO   *+14                                                             
         CLC   TRSPDAT,TODAY2            PEELED TODAY ?                         
         BE    *+14                                                             
         OC    TRSPDAT,TRSPDAT           ALREADY PEELED ?                       
         BNZ   LOOK17                    YES, SKIP IT                           
*                                                                               
LOOK07   TM    LDGFLG,LDGPROD            PRODUCTION ?                           
         BNO   *+14                                                             
         CLC   TRNKWORK,=C'**'           SKIP ORDERS                            
         BE    LOOK17                                                           
*                                                                               
         TM    LDGFLG,LDGPAYL            PAYABLES ?                             
         BNO   LOOK11                                                           
         OC    TRSUDAT,TRSUDAT                                                  
         BZ    LOOK17                    ONLY USED                              
*                                                                               
LOOK11   LA    R6,ITMWRK                 GET ITEM DETAILS                       
         USING ITMD,R6                                                          
         XC    ITMKEY,ITMKEY                                                    
         ZAP   ITMDR,=P'0'                                                      
         ZAP   ITMCR,=P'0'                                                      
         MVC   ITMREF,TRNKREF            REFERENCE                              
         CLC   TRNKUNT(2),=C'SA'                                                
         BE    *+16                                                             
         MVC   ITMCACT,TRNKCACT          CONTRA                                 
         MVC   ITMDATE,TRNKDATE          DATE                                   
*                                                                               
         LA    R4,TRNRFST                                                       
         USING TRNELD,R4                                                        
         LA    RF,ITMDR                                                         
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    RF,ITMCR                                                         
         ZAP   0(L'ITMDR,RF),TRNAMNT     AMOUNT                                 
*                                                                               
         TM    ACCFLG,ACCBUF             ADD TO BUFFERIN ?                      
         BNO   LOOK13                                                           
         GOTO1 BUFFERIN,DMCB,('BUFFAPUT',BUFF),(0,ITMD),ADCOMFAC                
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LOOK13   OI    ACCFLG,ACCTRN             SET TRANSACTIONS FOUND                 
         AP    BALANCE,ITMDR                                                    
         SP    BALANCE,ITMCR                                                    
*                                                                               
LOOK17   BAS   RE,DMSEQ                 GET THE NEXT FOR THIS ACCOUNT           
         B     LOOK03                                                           
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT (IF UNPEELING)                                      *         
***********************************************************************         
                                                                                
PACCUN   DS    0H                                                               
         MVI   FCRDTRNS,C'N'                                                    
         ZAP   ACCBF,=P'0'                                                      
         ZAP   ACCDR,=P'0'                                                      
         ZAP   ACCCR,=P'0'                                                      
         ZAP   ACCUP,=P'0'                                                      
*                                                                               
         L     RF,AOFFTAB                MARK END OF OFFICE TAB                 
         MVI   0(RF),X'FF'                                                      
         L     R4,ADACC                                                         
         SR    R3,R3                                                            
         ZAP   DUB,=P'0'                                                        
         MVI   ELCODE,APOELQ             GET PEEL ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   PACCUN05                                                         
         LR    R3,R4                                                            
         USING APOELD,R3                                                        
         CLC   APOPLDT,TODAY3                                                   
         BNE   XIT                                                              
         DROP  R3                                                               
*                                                                               
PACCUN05 MVI   ACCFLG,0                  INITIALIZE ACCOUNT FLAG                
         ICM   R4,15,ADACCBAL                                                   
         LTR   R4,R4                                                            
         BZ    XIT                                                              
*                                                                               
         MVC   ACCODE,SPACES                                                    
         LA    RE,ACCODE                 PARSE ACCOUNT CODE                     
         L     RF,ADACC                                                         
         LA    RF,ACTKACT-ACTKEY(RF)                                            
         L     R5,ADLDGHIR                                                      
         USING ACLELD,R5                                                        
         LA    R2,ACLVLEN                                                       
         SR    R3,R3                                                            
         SR    R1,R1                                                            
PACCUN07 IC    R1,0(R2)                                                         
         SR    R1,R3                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         CLI   0(R2),12                  TEST ALL 12 MOVED ?                    
         BE    PACCUN09                                                         
         AHI   R1,1                                                             
         LA    RF,0(R1,RF)                                                      
         LA    RE,1(R1,RE)                                                      
         AR    R3,R1                     R3=LENGTH USED SO FAR                  
         LA    R2,L'ACLVALS(R2)                                                 
         B     PACCUN07                                                         
*                                                                               
PACCUN09 MVC   ACNAME,SPACES             GET ACCOUNT NAME                       
         L     R5,ADACCNAM                                                      
         USING NAMELD,R5                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACNAME(0),NAMEREC                                                
*                                                                               
PACCUN17 MVI   FCRDTRNS,C'Y'             SET TO PROCESS TRANSACTIONS            
         MVI   FCRNTIME,C'Y'        PASS R AND N TIME FOR PRODUCTION            
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTION                                                 *         
***********************************************************************         
                                                                                
PTRN     DS    0H                                                               
         TM    REQOPT,REQUNP             ARE WE UNPEELING                       
         BO    PTRNU                     GO UNPEEL                              
         L     R3,ADTRANS                R3=A(TRANSACTION ELEMENT)              
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ              MUST BE TRANSACTION                    
         BNE   XIT                                                              
         LR    R2,R3                                                            
         SH    R2,DATADISP               R2=A(TRANSACTION RECORD)               
         USING TRNRECD,R2                                                       
         TM    REQOPT,OFF2               TEST TWO CHARACTER OFFICE              
         BNO   PTRN02                    NO,                                    
         CLI   TRNKOFF,C' '              TEST VALID OFFICE                      
         BNH   XIT                       NO, SKIP IT                            
*                                                                               
PTRN02   LR    R4,R3                                                            
         MVI   ELCODE,TRSELQ             GET MOS                                
         BAS   RE,NEXTEL                 R4=A(TRANSACTION STATUS)               
         BNE   XIT                                                              
         USING TRSELD,R4                                                        
         TM    TRSSTAT,TRSSOFFS          OFFSET ITEM ?                          
         BNO   *+8                                                              
         BO    XIT                       AS PER GBOS & ACCPAK DATABASE          
*                                        ITEM #45407                            
         TM    REQOPT,RERUN              RERUN ?                                
         BNO   *+14                                                             
         CLC   TRSPDAT,TODAY2            PEELED TODAY ?                         
         BE    *+14                                                             
         OC    TRSPDAT,TRSPDAT           ALREADY PEELED ?                       
         BNZ   XIT                       YES, SKIP IT                           
         CLC   TRSPMOS,RCMOS             TEST MOS                               
         BH    XIT                                                              
         CLC   TRSRMOS,RCMOS             TEST REVERSE MONTH                     
         BNH   PTRN02A                                                          
         ZAP   DUB,TRNAMNT                                                      
         TM    TRNSTAT,TRNSDR            TEST DEBIT                             
         BO    *+10                                                             
         MP    DUB,=P'-1'                                                       
         AP    ACCUP,DUB                                                        
         B     XIT                                                              
*                                                                               
PTRN02A  MVC   MOS,TRSPMOS               SAVE MOS                               
         TM    ACCFLG,ACCBUF             BUFFER IN USE ?                        
         BNO   PTRN05                    NO,                                    
         LA    R6,ITMWRK                 R6=A(ITEM RECORD)                      
         USING ITMD,R6                                                          
         XC    ITMKEY,ITMKEY                                                    
         MVC   ITMREF,TRNKREF            REFERENCE                              
         CLC   TRNKUNT(2),=C'SA'                                                
         BE    *+16                                                             
         MVC   ITMCACT,TRNKCACT          CONTRA                                 
         MVC   ITMDATE,TRNKDATE          DATE                                   
         MVC   WORK(ITMKLNQ),ITMKEY                                             
         GOTO1 BUFFERIN,DMCB,('BUFFARDH',BUFF),(0,ITMD),ADCOMFAC                
         CLI   4(R1),0                                                          
         BNE   XIT                                                              
         CLC   WORK(ITMKLNQ),ITMKEY ITEM IN TABLE ?                             
         BNE   XIT                       NOT IN TABLE, SKIP IT                  
*                                                                               
         TM    LDGFLG,LDGPAYL            PAYABLE LEDGER ?                       
         BNO   PTRN03                                                           
         OC    TRSUDAT,TRSUDAT           USED ?                                 
         BZ    XIT                       NO, CAN'T PEEL                         
         CP    ITMDR,ITMCR               DEBITS = CREDITS ?                     
         BNE   XIT                       NO, KEEP IT                            
         CP    ITMCR,=P'0'               CREDITS = ZERO                         
         BE    XIT                       YES, BETTER KEEP IT                    
         B     PTRN09                                                           
*                                                                               
PTRN03   TM    LDGFLG,LDGRECV+LDGLREC    RECEIVABLES ?                          
         BZ    PTRN05                                                           
         CP    ITMDR,ITMCR               DEBITS = CREDITS ?                     
         BNE   XIT                       NO, KEEP IT                            
         B     PTRN09                    YES, OK TO PEEL                        
*                                                                               
PTRN05   TM    LDGFLG,LDGPROD            PRODUCTION ?                           
         BNO   PTRN07                                                           
         CLC   TRNKWORK,=C'**'           SKIP ORDERS                            
         BE    XIT                                                              
         B     PTRN09                                                           
*                                                                               
PTRN07   CLI   TRNTYPE,TRNTORD           DON'T PEEL EXPENSE ORDERS              
         BE    XIT                                                              
         TM    LDGFLG,LDGCASH            CASH LEDGER ?                          
         BNO   PTRN09                                                           
         CP    TRNAMNT,=P'0'             ALWAYS PEEL ZERO AMOUNTS               
         BE    PTRN09                                                           
         L     R5,ADACCSTA                                                      
         USING RSTELD,R5                                                        
         LA    R1,RSTSPRDR               RECONCILED DEBITS                      
         TM    TRNSTAT,TRNSDR            DEBIT TRANSACTION ?                    
         BO    *+8                                                              
         LA    R1,RSTSPRCR               RECONCILED CREDITS                     
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    RSTSTAT3,0                TEST RECONCILED ONLY ?                 
         BZ    *+12                                                             
         TM    TRNSTAT,TRNSBREC          ITEM RECONCILED ?                      
         BZ    XIT                       NO, KEEP IT                            
*                                                                               
PTRN09   MVC   TRNRECD+ACCOPEEL(ACCOPLEN),TODAY2                                
*                                                                               
         AP    PKCOUNT,=P'1'       UPDATE # OF TRANSACTIONS COUNTER             
*                                                                               
         LA    R1,ACCDR                                                         
         TM    TRNSTAT,TRNSDR            DEBIT TRANSACTION ?                    
         BO    *+8                                                              
         LA    R1,ACCCR                                                         
         AP    0(L'ACCDR,R1),TRNAMNT                                            
*                                                                               
         CLI   AUDIT,0                   AUDIT REQUIRED ?                       
         BE    *+8                                                              
         BAS   RE,AUDITOR                                                       
*                                                                               
         TM    REQOPT,OFF2               TEST TWO CHARACTER OFFICE              
         BNO   PTRN11                    NO,                                    
         TM    LDGFLG,LDGPROD            TEST PRODUCTION                        
         BO    PTRN11                    YES, SKIP OFFICE ACCOUNT               
         CLI   TRNKUNT,C'G'              FOR UNIT G                             
         BE    PTRN10                                                           
         CLI   TRNKUNT,C'S'              AND S                                  
         BE    PTRN10                                                           
         CLI   TRNKUNT,C'2'              AND 2                                  
         BE    PTRN10                                                           
         CLC   TRNKUNT(2),=C'11'         AND SOME IN UNIT 1                     
         BE    PTRN10                                                           
         CLC   TRNKUNT(2),=C'12'                                                
         BE    PTRN10                                                           
         CLC   TRNKUNT(2),=C'1C'                                                
         BE    PTRN10                                                           
         B     PTRN11                                                           
*                                                                               
PTRN10   BAS   RE,ADDOFF                 ADD TO OFFICE TABLE                    
*                                                                               
PTRN11   TM    REQOPT,DRAFT                                                     
         BO    *+8                                                              
         MVI   MODE,WRITRANS                                                    
         OI    ACCFLG,ACCUPD             SET TO UPDATE ACCOUNT                  
         OI    ACTSW,ACTRUN+ACTREQ+ACTUNT+ACTLDG                                
*                                                                               
         TM    TOUTSW,TOUTPUT      TEST NEED TAPE                               
         BNO   XIT                                                              
         XR    R1,R1                                                            
         ICM   R1,3,ACCORLEN(R2)   SET RECORD LENGTH FOR MOVE                   
         LR    RF,R1                                                            
         LA    R5,28(R1)           ADD LENGTH FOR RCV HEADER                    
         L     RF,AIO2                                                          
         STCM  R5,3,0(RF)          SET TOTAL LENGTH IN IO                       
         L     R0,AIOREC                                                        
         LR    RE,R2                                                            
         MVCL  R0,RE               MOVE THE RECORD                              
*                                                                               
         L     R4,ATOUT                                                         
         L     R3,AIO2                                                          
         PUT   (R4),(R3)                                                        
         B     XIT                                                              
         DROP  R2,R3,R4,R5,R6                                                   
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTION  UNPEEL                                         *         
***********************************************************************         
                                                                                
PTRNU    DS    0H                                                               
         L     R3,ADTRANS                R3=A(TRANSACTION ELEMENT)              
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ              MUST BE TRANSACTION                    
         BNE   XIT                                                              
         LR    R2,R3                                                            
         SH    R2,DATADISP               R2=A(TRANSACTION RECORD)               
         USING TRNRECD,R2                                                       
*                                                                               
PTRNU02  LR    R4,R3                                                            
         MVI   ELCODE,TRSELQ             GET MOS                                
         BAS   RE,NEXTEL                 R4=A(TRANSACTION STATUS)               
         BNE   XIT                                                              
         USING TRSELD,R4                                                        
         CLC   TRSPDAT,TODAY2            PEELED TODAY ?                         
         BNE   XIT                                                              
*                                                                               
         XC    TRSPDAT,TRSPDAT           CLEAR PEELED DATE                      
         NI    TRNSTAT,(X'FF'-X'10')     TURN OFF PEEL BIT                      
         XC    TRNRECD+ACCOPEEL(ACCOPLEN),TRNRECD+ACCOPEEL                      
         AP    PKCOUNT,=P'1'       UPDATE # OF TRANSACTIONS COUNTER             
*                                                                               
         LA    R1,ACCDR                                                         
         TM    TRNSTAT,TRNSDR            DEBIT TRANSACTION ?                    
         BO    *+8                                                              
         LA    R1,ACCCR                                                         
         SP    0(L'ACCDR,R1),TRNAMNT                                            
*                                                                               
         TM    REQOPT,OFF2               TWO CHARACTER ?                        
         BNO   *+8                                                              
         BAS   RE,ADDOFF                 ADD TO OFFICE TABLE                    
*                                                                               
         TM    REQOPT,DRAFT                                                     
         BO    *+8                                                              
         MVI   MODE,WRITRANS                                                    
         OI    ACCFLG,ACCUPD             SET TO UPDATE ACCOUNT                  
         OI    ACTSW,ACTRUN+ACTREQ+ACTUNT+ACTLDG                                
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* ACCOUNT LAST                                                        *         
***********************************************************************         
                                                                                
ACCL     TM    ACCFLG,ACCUPD             NEED TO UPDATE ACCOUNT ?               
         BNO   XIT                                                              
         TM    LDGFLG,LDGHEAD            TEST FORCEHEAD SET                     
         BO    *+12                      YES,                                   
         MVI   FORCEHED,C'Y'             SET IT                                 
         OI    LDGFLG,LDGHEAD                                                   
*                                                                               
         LA    RE,ACCBF                  ADD ACCOUNT TOTALS                     
         LA    RF,UNTBF                  TO HIGHER LEVELS                       
         LA    R0,4                      UNIT,LEDGER,REQ,RUN                    
ACCL01   AP    0(8,RF),0(8,RE)                                                  
         AP    8(8,RF),8(8,RE)                                                  
         AP    16(8,RF),16(8,RE)                                                
         AP    24(8,RF),24(8,RE)                                                
         LA    RF,32(RF)                                                        
         BCT   R0,ACCL01                                                        
*                                                                               
         LA    R3,P                                                             
         USING PLD,R3                                                           
         MVC   PLCODE,ACCODE                                                    
         MVC   PLNAME,ACNAME                                                    
         GOTO1 FRMT,DMCB,ACCBF                                                  
         BAS   RE,PRNT                                                          
         DROP  R3                                                               
*                                                                               
         TM    REQOPT,OFF2               TWO CHARACTER OFFICE ?                 
         BNO   ACCL05                                                           
         TM    LDGFLG,LDGPROD+LDGTALN                                           
         BNZ   ACCL05                                                           
         L     R4,AOFFTAB                                                       
         USING OFFD,R4                                                          
*                                                                               
ACCL03   CLI   0(R4),X'FF'               TEST END OF OFFICE REORDS              
         BE    ACCL05                                                           
         LA    R2,DKEY                                                          
         USING OFARECD,R2                                                       
         L     RF,ADACC                                                         
         MVC   OFAKEY,0(RF)                                                     
         MVC   OFAKOFF,OFFICE                                                   
         BAS   RE,DMRD                   GET THE OFFICE/ACCOUNT RECORD          
         CLC   OFAKEY,DIR                                                       
         BE    ACCL04                    NO OFFICE/ACCOUNT RECORD               
         CP    OFFDR,=P'0'                                                      
         BNE   *+14                                                             
         CP    OFFCR,=P'0'                                                      
         BE    ACCL04A                                                          
         DC    H'0'                                                             
ACCL04   MVC   AIO,AIO1                                                         
         BAS   RE,DMGETR                 GET THE RECORD                         
         GOTO1 UPDACC,DMCB,AIO,OFFDR     UPDATE THE RECORD                      
         BAS   RE,DMPUTR                 WRITE THE RECORD                       
ACCL04A  LA    R4,OFFLNQ(R4)             GET NEXT OFFICE ENTRY                  
         B     ACCL03                                                           
*                                                                               
ACCL05   LA    R2,DKEY                   UPDATE ACCOUNT RECORD                  
         USING ACTRECD,R2                                                       
         L     RF,ADACC                                                         
         MVC   ACTKEY,0(RF)                                                     
         BAS   RE,DMRD                   GET THE ACCOUNT RECORD                 
         CLC   ACTKEY,DIR                                                       
         BE    *+6                                                              
         DC    H'0'                      NO ACCOUNT RECORD                      
         MVC   AIO,AIO1                                                         
         BAS   RE,DMGETR                 GET THE RECORD                         
         GOTO1 UPDACC,DMCB,AIO,ACCDR     UPDATE THE RECORD                      
         BAS   RE,DMPUTR                 WRITE THE RECORD                       
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE ACCOUNT  & STATUS ELEMENTS                        *         
*  PARM 1 = A(RECORD)                                                           
*  PARM 2 = A(PEELED DR/CR)                                                     
***********************************************************************         
                                                                                
UPDACC   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         LR    R4,R2                                                            
         MVI   ELCODE,ABLELQ             GET BALANCE ELEMENT                    
         BAS   RE,GETELN                                                        
         BE    *+6                                                              
         DC    H'0'                      NO BALANCE ELEMENT                     
         USING ABLELD,R4                 GET ABLEDL ELEMENT                     
*                                                                               
         XC    ELEM,ELEM                 INIT ELEM                              
         LLC   RF,ABLLN                  GET CURRENT LENGTH OF ELEMENT          
         BCTR  RF,0                                                             
         MVC   ELEM(0),0(R4)             COPY ABLELD TO ELEM                    
         EX    RF,*-6                                                           
         MVI   ABLEL,DELETEQ             MARK ABLELD FOR DELETION               
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',(R2)),0,0 DELETE IT              
         CLI   12(R1),0                  DELETE ELEMENTS OK?                    
         BE    *+6                       YES, CONTINUE                          
         DC    H'0'                      NO, DIE HERE                           
*                                                                               
         LA    R4,ELEM                   ADD A PEEL ELEMENT                     
         MVI   ABLLN,ABLLN3Q             EXTEND ELEMENT LENGTH TO 40            
         SP    ABLDR,0(8,R3)             REDUCE DEBITS                          
         SP    ABLCR,8(8,R3)             AND CREDITS                            
         AP    ABLFRWD,0(8,R3)           ADD DEBITS TO BBF                      
         SP    ABLFRWD,8(8,R3)           SUBTRACT CREDITS FROM BBF              
         OC    ABLURG,ABLURG             IF ABLURG IS HAVING ANY DATA           
         BNZ   *+10                      YES, SKIP IT                           
         ZAP   ABLURG,=P'0'              ELSE, ZAP IT                           
         OC    ABLTXS,ABLTXS             IF ABLTXS IS HAVING ANY DATA           
         BNZ   *+10                      YES, SKIP IT                           
         XC    ABLTXS,ABLTXS             ELSE, INTITIALIZE IT                   
         MVC   ABLPLDT,PLFORDT           SET COMPRESSED PEELED FOR DATE         
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),ABLEL,0 ADD EXTENDED ELEM          
         CLI   DMCB+12,0                 ADDED SUCCESSFULLY,                    
         BE    *+6                       YES, CONTINUE                          
         DC    H'0'                      NO, DIE HERE                           
*                                                                               
         TM    REQOPT,REQUNP              ARE WE UNPEELING                      
         BO    UPDACC5                                                          
*                                                                               
         LR    R4,R2                                                            
         MVI   ELCODE,RSTELQ             GET STATUS ELEMENT                     
         BAS   RE,GETELN                                                        
         BNE   UPDACC3                                                          
         USING RSTELD,R4                                                        
         MVC   RSTBDATE,NXTMON           BBF DATE(1ST OF NEXT MONTH)            
         B     UPDACC5                                                          
*                                                                               
UPDACC3  LA    R4,ELEM                   ADD STATUS ELEMENT                     
         XC    ELEM,ELEM                                                        
         MVI   RSTEL,RSTELQ                                                     
         MVI   RSTLN,RSTLN2Q                                                    
         MVI   RSTFILT1,C' '                                                    
         MVI   RSTFILT2,C' '                                                    
         MVI   RSTFILT3,C' '                                                    
         MVI   RSTFILT4,C' '                                                    
         MVI   RSTFILT5,C' '                                                    
         MVC   RSTBDATE,NXTMON                                                  
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),RSTEL,0                            
         CLI   DMCB+12,0                                                        
         BE    UPDACC5                                                          
         DC    H'0'                                                             
*                                                                               
UPDACC5  LR    R4,R2                                                            
         MVI   ELCODE,APOELQ             GET PEEL ELEMENT                       
         BAS   RE,GETELN                                                        
         BNE   UPDACC10                                                         
         USING APOELD,R4                                                        
*                                                                               
         TM    REQOPT,REQUNP              ARE WE UNPEELING                      
         BNO   UPDACC7                                                          
         MVC   APOPLDT,APOLBDT           MAKE LAST PEELOFF CURRENT              
         XC    APOLBDT,APOLBDT           CLEAR OUT PREVIOUS PEEL DATE           
         ZAP   APODR,=P'0'               CLEAR DR                               
         ZAP   APOCR,=P'0'               CLEAR CR                               
         B     XIT                                                              
*                                                                               
UPDACC7  MVC   APOLBDT,APOPLDT           SAVE LAST PEEL DATE                    
         MVC   APOPLDT,TODAY3                                                   
         ZAP   APODR,0(8,R3)                                                    
         ZAP   APOCR,8(8,R3)                                                    
         B     XIT                                                              
*                                                                               
UPDACC10 DS    0H                                                               
         TM    REQOPT,REQUNP              ARE WE UNPEELING                      
         BO    XIT                                                              
         LA    R4,ELEM                   ADD A PEEL ELEMENT                     
         XC    ELEM,ELEM                                                        
         MVI   APOEL,APOELQ                                                     
         MVI   APOLN,APOLN2Q                                                    
         MVC   APOPLDT,TODAY3                                                   
         ZAP   APODR,0(8,R3)                                                    
         ZAP   APOCR,8(8,R3)                                                    
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),APOEL,0                            
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LEDGER LAST                                                         *         
***********************************************************************         
                                                                                
LDGL     TM    ACTSW,ACTLDG              TEST ANY ACTIVITY                      
         BZ    XIT                                                              
         CLI   AUDIT,0                                                          
         BE    LDGL03                    NO AUDIT FUNCTION REQUIRED             
         TM    REQOPT,REQALL             TEST ALL UNITS                         
         BO    LDGL03                    YES, SKIP AUDIT                        
         MVC   AUDLIN,SPACES                                                    
         MVC   AUDLLGT(13),=C'LEDGER TOTALS'                                    
         MVC   AUDLBUK,AUDBUKS           AUDIT BUCKETS                          
         LA    R6,AUDLBUK                                                       
         USING AUDD,R6                                                          
         ZAP   AUDDD$,LEDDR              DELETED DEBIT  $                       
         ZAP   AUDDC$,LEDCR                      CREDIT $                       
         ZAP   AUDBBF,LEDBF              BBF                                    
         ZAP   AUDBCF,LEDBF              BCF                                    
         AP    AUDBCF,AUDDD$                                                    
         SP    AUDBCF,AUDDC$                                                    
         ICM   R3,15,AUDSEQ                                                     
         AHI   R3,1                                                             
         STCM  R3,15,AUDSEQ                                                     
         GOTO1 ADSORTER,DMCB,=C'PUT',AUDREC                                     
         DROP  R6                                                               
*                                                                               
LDGL03   BAS   RE,PRNT                                                          
         MVC   P+19(20),=CL20'TOTAL FOR LEDGER'                                 
         GOTO1 FRMT,DMCB,LEDBF                                                  
         BAS   RE,PRNT                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* UNIT LAST                                                           *         
***********************************************************************         
                                                                                
UNTL     TM    ACTSW,ACTUNT              TEST ANY ACTIVITY                      
         BZ    XIT                                                              
         BAS   RE,PRNT                                                          
         MVC   P+19(20),=CL20'TOTAL FOR UNIT'                                   
         GOTO1 FRMT,DMCB,UNTBF                                                  
         BAS   RE,PRNT                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* REQUEST LAST                                                        *         
***********************************************************************         
                                                                                
REQL     CLI   QOPT5,C'Y'                                                       
         BNE   REQL05                                                           
         L     R5,AMONACC                                                       
         USING ACMD,R5                                                          
         CLI   ACMANXL,1                 TEST END OF LAST U/L                   
         BNE   *+10                                                             
         XC    ACMANXL,ACMANXL           SET END OF LIST                        
*                                                                               
REQL05   TM    ACTSW,ACTREQ              TEST ANY ACTIVITY                      
         BZ    REQL10                                                           
         BAS   RE,PRNT                                                          
         MVC   P+19(20),=CL20'TOTAL FOR REQUEST'                                
         GOTO1 FRMT,DMCB,REQBF                                                  
         BAS   RE,PRNT                                                          
*                                                                               
         CLI   QOPT4,C'Y'          DO WE WANT NUMBER OF TRANSACTIONS            
         BNE   REQL10                                                           
         MVC   P+19(31),=CL31'TOTAL NUMBER OF TRANSACTIONS = '                  
         EDIT  PKCOUNT,(10,P+50)                                                
         ZAP   PKCOUNT,=P'0'                                                    
         BAS   RE,PRNT                                                          
*                                                                               
REQL10   TM    REQOPT,REQALL             TEST ALL UNITS                         
         BO    XIT                       YES, SKIP AUDIT                        
         TM    RUNOPT,RUNDDS                                                    
         BO    XIT                                                              
*                                                                               
REQL12   CLI   AUDIT,0                                                          
         BE    REQLX                                                            
         TM    ACTSW,ACTREQ              TEST ANY ACTIVITY                      
         BZ    REQLX                                                            
         BAS   RE,WAR                    WRITE AUDITORS REPORT                  
*                                                                               
REQLX    GOTO1 ADSORTER,DMCB,=C'END'                                            
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
                                                                                
RUNL     DS    0H                                                               
         TM    TOUTSW,TOUTOPN                                                   
         BNO   RUNL01                                                           
         L     R4,ATOUT                                                         
         CLOSE ((R4))                                                           
*                                                                               
RUNL01   TM    ACTSW,ACTRUN              TEST ANY ACTIVITY                      
         BZ    XIT                                                              
         CP    RUNDR,=P'0'               ANY PEELED DATA ?                      
         BNE   *+14                                                             
         CP    RUNCR,=P'0'                                                      
         BE    XIT                       IF ALL DRAFT NO POSTING FILE           
*                                                                               
         TM    REQOPT,REQUNP             ARE WE UNPEELING                       
         BO    XIT                       YES EXIT                               
*                                                                               
         TM    REQOPT,DRAFT              DO NOT CREATE WORKER FILE              
         BO    XIT                       FOR DRAFT RUN                          
*                                                                               
         XC    ID,ID                     BUILD WORKER KEY                       
         MVC   ID(2),ORIGINUM                                                   
         MVC   ID+2(3),=C'A98'                                                  
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK)                                  
         MVC   WORK(2),WORK+4            DAY                                    
         PACK  DUB(2),WORK(3)                                                   
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
         MVI   ID+13,X'01'                                                      
*                                                                               
         GOTO1 WORKER,DMCB,WKOPEN,APOSTB,ID,0                                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R3,WORK+4                                                        
         USING PSSUBFD,R3                                                       
         MVI   PSSBEL,PSSBELQ                                                   
         MVI   PSSBLEN,PSSUBFL                                                  
         MVC   PSSBDESC,=CL15'TRANS. PEEL'                                      
         ZAP   PSSBRECS,RUNDR                                                   
         ZAP   PSSBCASH,RUNCR                                                   
         MVI   WORK+1,PSSUBFL+5                                                 
         GOTO1 WORKER,DMCB,WKADD,APOSTB,ID,WORK                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 WORKER,DMCB,WKCLOSE,APOSTB,ID,0                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ADD TO OFFICE TABLE                                                 *         
***********************************************************************         
                                                                                
ADDOFF   NTR1  ,                                                                
         L     R3,ADTRANS                                                       
         USING TRNELD,R3                                                        
         LR    R2,R3                                                            
         SH    R2,DATADISP                                                      
         USING TRNRECD,R2                                                       
         CLI   TRNKOFF,C' '                                                     
         BH    *+6                                                              
         DC    H'0'                                                             
         LHI   R0,MXOFF                                                         
         L     R4,AOFFTAB                                                       
         USING OFFD,R4                                                          
*                                                                               
ADDOF10  CLI   0(R4),X'FF'               EOT ?                                  
         BE    ADDOF20                   YES, ADD NEW ENTRY                     
         CLC   TRNKOFF,OFFICE            MATCH OFFICE                           
         BE    ADDOF30                                                          
         LA    R4,OFFLNQ(R4)             BUMP TO NEXT                           
         B     ADDOF10                                                          
*                                                                               
ADDOF20  MVC   OFFICE,TRNKOFF            ADD NEW OFFICE                         
         ZAP   OFFDR,=P'0'                                                      
         ZAP   OFFCR,=P'0'                                                      
         MVI   OFFICE+OFFLNQ,X'FF'       NEW EOT                                
         SHI   R0,1                                                             
         BP    *+6                                                              
         DC    H'0'                      OFFICE TABLE IS FULL                   
*                                                                               
ADDOF30  LA    R1,OFFDR                                                         
         TM    TRNSTAT,TRNSDR            DEBIT TRANSACTION ?                    
         BO    *+8                                                              
         LA    R1,OFFCR                                                         
*                                                                               
         TM    REQOPT,REQUNP              ARE WE UNPEELING                      
         BNO   ADDOF40                                                          
         SP    0(L'OFFDR,R1),TRNAMNT                                            
         B     XIT                                                              
ADDOF40  AP    0(L'OFFDR,R1),TRNAMNT      PEELING                               
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* FORMAT BBF/DR/CR/BALANCE                                            *         
***********************************************************************         
                                                                                
FRMT     NTR1  ,                                                                
         L     R2,0(R1)                  R2=A(ACCUMS)                           
         LR    R5,R2                     SAVE IN R5                             
         ZAP   BALANCE,0(8,R2)           BBF                                    
         AP    BALANCE,8(8,R2)           PLUS DEBITS                            
         SP    BALANCE,16(8,R2)          LESS CREDITS                           
*                                                                               
         ZAP   WORK(10),16(8,R2)                                                
         MP    WORK(10),=P'-1'           REVERSE SIGN FOR CREDITS               
         ZAP   16(8,R2),WORK(10)                                                
*                                                                               
         LA    R3,P                                                             
         USING PLD,R3                                                           
         LA    R6,PLBBF                                                         
         LA    R4,3                                                             
         CLI   QOPT2,C'C'                JOB CLOSE ?                            
         BNE   FRMT3                                                            
         LA    R6,PLJCDR                 JUST DEBITS AND CREDITS                
         LA    R4,2                                                             
         LA    R2,8(R2)                                                         
*                                                                               
FRMT3    CURED (P8,0(R2)),(L'PLBBF,0(R6)),2,MINUS=YES,ZERO=BLANK                
         ORG   *-2                                                              
         CLI   QOPT2,C'C'                JOB CLOSE ?                            
         BNE   *+8                                                              
         NI    CURPEDT1-CURPARMD(R1),X'FF'-CURPZERB   ZERO=NOBLANK              
         BASR  RE,RF                                                            
*                                                                               
         LA    R2,8(R2)                                                         
         LA    R6,L'PLBBF(R6)                                                   
         BCT   R4,FRMT3                                                         
         CLI   QOPT2,C'C'                JOB CLOSE ?                            
         BE    XIT                                                              
*                                                                               
         LR    R2,R5                                                            
*                                                                               
         ZAP   WORK(10),16(8,R2)                                                
         MP    WORK(10),=P'-1'           REVERSE SIGN FOR CREDITS               
         ZAP   16(8,R2),WORK(10)                                                
*                                                                               
         CURED BALANCE,(L'PLBCF,PLBCF),2,MINUS=YES,ZERO=BLANK                   
         CURED (P8,24(R2)),(L'PLUP,PLUP),2,MINUS=YES,ZERO=BLANK                 
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*  BUILD LIST OF UNIT/LEDGERS TO PEEL                                 *         
***********************************************************************         
                                                                                
SETUL    NTR1  ,                                                                
         XR    R0,R0                                                            
         LA    R5,ULIST                                                         
         LA    R2,UNITS                                                         
*                                                                               
SETUL01  XC    DKEY,DKEY                                                        
         LA    R3,DKEY                                                          
         USING LDGRECD,R3                                                       
         MVC   LDGKCPY,RCCOMPFL                                                 
         MVC   LDGKUNT,0(R2)                                                    
         MVI   LDGKLDG,C'A'        READ FOR FIRST LEDGER                        
*                                                                               
SETUL02  BAS   RE,DMHGH                                                         
         CLC   DIR(2),DKEY         TEST SAME COMPANY/UNIT                       
         BE    SETUL03             YES,                                         
         LA    R2,1(R2)            NEXT UNIT                                    
         CLI   0(R2),X'FF'         TEST EOL                                     
         BE    SETUL09             YES, ALL DONE                                
         B     SETUL01             NO, PROCESS NEXT UNIT                        
*                                                                               
SETUL03  LA    R3,DIR                                                           
         CLC   LDGKLDG+1(LDGKSTA-(LDGKLDG+1)),SPACES                            
         BE    *+6                                                              
         DC    H'0'                BAD RECORD                                   
*                                                                               
         LA    RF,XUL              CHECK EXCLUDE LIST                           
SETUL04  CLI   0(RF),X'FF'         TEST EOL                                     
         BE    SETUL06             YES,                                         
         CLC   LDGKUNT(2),0(RF)    TEST MATCHES EXCLUDE LIST                    
         BE    SETUL07             YES,                                         
         LA    RF,2(RF)                                                         
         B     SETUL04                                                          
*                                                                               
SETUL06  MVC   0(2,R5),LDGKUNT     SAVE UL                                      
         LA    R5,2(R5)                                                         
         AHI   R0,1                                                             
*                                                                               
SETUL07  LLC   RF,LDGKLDG          BUMP LEDGER FOR READ HIGH                    
         AHI   RF,1                                                             
         STC   RF,LDGKLDG                                                       
         MVC   DKEY,LDGKEY                                                      
         B     SETUL02                                                          
*                                                                               
SETUL09  LA    R5,ULIST                                                         
         L     RE,AMONACC                                                       
         USING ACMD,RE                                                          
         ST    R5,ACMANXL                                                       
         STC   R0,ACMANXL                                                       
         CHI   R0,MXUL                                                          
         BNH   *+6                                                              
         DC    H'0'                ULIST IS TOO SMALL                           
         B     XIT                                                              
*                                                                               
         DROP  R3,RE                                                            
         EJECT                                                                  
***********************************************************************         
*  MISCELLANEOUS                                                      *         
***********************************************************************         
                                                                                
GETELN   AH    R4,NEWDISP                                                       
         B     FIRSTEL                                                          
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
PRNT     NTR1  ,                                                                
         TM    REQOPT,DRAFT                                                     
         BNO   *+10                                                             
         MVC   HEAD2+85(5),=C'DRAFT'                                            
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* AUDIT ROUTINE                                                       *         
***********************************************************************         
                                                                                
AUDITOR  NTR1  ,                                                                
         L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         CLI   0(R4),TRNELQ                                                     
         BNE   XIT                                                              
         LR    R2,R4                                                            
         SH    R2,DATADISP                                                      
         USING TRNRECD,R2                                                       
         LA    R6,AUDBUKS                                                       
         USING AUDD,R6                                                          
         TM    TRNSTAT,TRNSDR            DEBIT ?                                
         BNO   AUD03                                                            
         AP    AUDDD#,=P'1'              DELETED DEBIT                          
         LA    R5,AUDTRGD                                                       
         B     AUD05                                                            
*                                                                               
AUD03    AP    AUDDC#,=P'1'              DELETED CREDIT                         
         LA    R5,AUDTRGC                                                       
*                                                                               
AUD05    TM    AUDIT,AUDIT#              ITEMS ?                                
         BO    AUD07                                                            
         AP    0(8,R5),TRNAMNT           ACCUMULATE TRANSACTION AMOUNTS         
         CP    0(8,R5),AUDTRG            ACCUM VS. TRIGGER                      
         BL    XIT                       TRIGGER HASN'T GONE OFF                
         SP    0(8,R5),AUDTRG            TRIGGER HAS GONE OFF                   
         CP    0(8,R5),AUDTRG            RESET ACCUMULATOR                      
         BNL   *-12                                                             
         B     AUD09                                                            
*                                                                               
AUD07    AP    AUDTRG#,=P'1'             COUNT # OF ITEMS                       
         CP    AUDTRG#,AUDTRG            COUNT VS TRIGGER                       
         BL    XIT                       TRIGGER HASN'T GONE OFF                
         ZAP   AUDTRG#,=P'0'                                                    
*                                                                               
AUD09    TM    TRNSTAT,TRNSDR                                                   
         BNO   AUD11                                                            
         AP    AUDSD#,=P'1'              ADD TO SELECTED DEBITS                 
         AP    AUDSD$,TRNAMNT                                                   
         B     AUD13                                                            
*                                                                               
AUD11    AP    AUDSC#,=P'1'              ADD TO SELECTED CREDITS                
         AP    AUDSC$,TRNAMNT                                                   
         DROP  R6                                                               
*                                                                               
AUD13    MVI   AUDLIN,C' '                                                      
         MVC   AUDLIN+1(L'AUDLIN-1),AUDLIN                                      
         LA    R3,AUDLIN                                                        
         USING APLD,R3                                                          
         MVC   APLREF,TRNREF                                                    
         MVC   APLBTCH,TRNBTCH                                                  
         MVC   APLOFF,TRNANAL                                                   
         GOTO1 DATCON,DMCB,(1,TRNDATE),(8,APLDATE)                              
         TM    TRNSTAT,TRNSDR                                                   
         BNO   *+12                                                             
         LA    R6,APLDR                                                         
         B     *+8                                                              
         LA    R6,APLCR                                                         
         CURED TRNAMNT,(15,0(R6)),2,MINUS=YES                                   
         SR    RF,RF                                                            
         IC    RF,TRNLN                  MOVE IN SOME NARRATIVE                 
         SHI   RF,TRNLN1Q+1                                                     
         CLC   TRNANAL,=C'99'                                                   
         BNE   *+8                                                              
         LA    RF,14                     FOR BILLING ONLY SHOW TYPE             
         CHI   RF,1                                                             
         BNH   AUD15                                                            
         CHI   RF,70                     MAX OF 70 BYTES                        
         BNH   *+8                                                              
         LA    RF,69                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APLNARS(0),TRNNARR                                               
*                                                                               
AUD15    MVC   APLACC,TRNKACT                                                   
         MVC   APLCON,TRNKULC                                                   
*                                                                               
         ICM   R3,15,AUDSEQ                                                     
         AHI   R3,1                                                             
         STCM  R3,15,AUDSEQ                                                     
         GOTO1 ADSORTER,DMCB,=C'PUT',AUDREC                                     
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* WRITE AUDITOR'S REPORT                                              *         
***********************************************************************         
                                                                                
WAR      NTR1  ,                                                                
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
*                                                                               
         USING APLD,R3                                                          
WAR03    GOTO1 ADSORTER,DMCB,=C'GET'     GET TRANS FROM SORTER                  
         L     R3,DMCB+4                                                        
         LA    R3,4(R3)                  BUMP PAST SEQ NUMBER                   
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   WAR05                                                            
         MVI   RCSUBPRG,0                                                       
         B     XIT                                                              
*                                                                               
WAR05    CLI   0(R3),C'N'                TEST NEW LEDGER                        
         BE    WAR09                                                            
         CLI   0(R3),C'L'                TEST LEDGER TOTALS                     
         BE    WAR15                                                            
         MVC   P(APLLNQ),APL                                                    
         CLC   APLNARS,SPACES                                                   
         BE    WAR07                                                            
         LA    RF,P+(APLNARP-APL)                                               
         LA    R0,L'APLNARP                                                     
         GOTO1 CHOPPER,DMCB,(L'APLNARS,APLNARS),((R0),(RF)),(C'P',3)            
WAR07    BAS   RE,PRNT                                                          
         B     WAR03                                                            
*                                                                               
WAR09    MVC   AUDLIN,0(R3)              NEW LEDGER                             
         OC    RANDINP,RANDINP           ONLY PRINT IF 'RANDOM' INPUT           
         BZ    WAR03                                                            
         MVC   P+1(AUDLNLNQ),AUDLIN                                             
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PRNT                                                          
         B     WAR03                                                            
*                                                                               
WAR15    MVC   PSECOND+33(14),=C'LEDGER SUMMARY'                                
         MVC   PTHIRD+33(14),=18C'-'                                            
         MVC   PSECOND+59(14),=C'ITEMS SELECTED'                                
         MVC   PTHIRD+62(11),=C'ITEM VALUES'                                    
*                                                                               
         LA    R4,AUDLBUK-AUDLIN(R3)                                            
         USING AUDD,R4                                                          
         LA    R5,AUDSD#                 SELECTED DEBIT #                       
         LA    R6,PSECOND+(APLDR-APL)                                           
         BAS   RE,WARCU0                                                        
         LA    R5,AUDSC#                 SELECTED CREDIT #                      
         LA    R6,PSECOND+(APLCR-APL)                                           
         BAS   RE,WARCU0                                                        
*                                                                               
         LA    R5,AUDSD$                 SELECTED DEBIT $                       
         LA    R6,PTHIRD+(APLDR-APL)                                            
         BAS   RE,WARCU2                                                        
         LA    R5,AUDSC$                 SELECTED CREDIT $                      
         LA    R6,PTHIRD+(APLCR-APL)                                            
         BAS   RE,WARCU2                                                        
*                                                                               
         MVI   SPACING,2                                                        
         BAS   RE,PRNT                                                          
*                                                                               
         MVC   P+60(13),=C'DELETED ITEMS'                                       
         MVC   PSECOND+59(14),=C'DELETED VALUES'                                
*                                                                               
         LA    R5,AUDDD#                 DELETED DEBIT #                        
         LA    R6,P+(APLDR-APL)                                                 
         BAS   RE,WARCU0                                                        
         LA    R5,AUDDC#                 DELETED CREDIT #                       
         LA    R6,P+(APLCR-APL)                                                 
         BAS   RE,WARCU0                                                        
*                                                                               
         LA    R5,AUDDD$                 DELETED DEBIT $                        
         LA    R6,PSECOND+(APLDR-APL)                                           
         BAS   RE,WARCU2                                                        
         LA    R5,AUDDC$                 DELETED CREDIT $                       
         LA    R6,PSECOND+(APLCR-APL)                                           
         BAS   RE,WARCU2                                                        
*                                                                               
         MVI   SPACING,2                                                        
         BAS   RE,PRNT                                                          
*                                                                               
         MVC   P+62(11),=C'BALANCE B/F'                                         
         LA    R5,AUDBBF                                                        
         LA    R6,P+(APLDR-APL)                                                 
         BAS   RE,WARCU2                                                        
         MVI   SPACING,2                                                        
         BAS   RE,PRNT                                                          
*                                                                               
         MVC   P+62(11),=C'BALANCE C/F'                                         
         LA    R5,AUDBCF                                                        
         LA    R6,P+(APLDR-APL)                                                 
         BAS   RE,WARCU2                                                        
*                                                                               
         BAS   RE,PRNT                                                          
         MVI   FORCEHED,C'Y'                                                    
         B     WAR03                                                            
*                                                                               
WARCU0   ST    RE,SAVRE                                                         
         CURED (P8,0(R5)),(15,0(R6)),0                                          
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
WARCU2   ST    RE,SAVRE                                                         
         CURED (P8,0(R5)),(15,0(R6)),2                                          
         L     RE,SAVRE                                                         
         BR    RE                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
                                                                                
DMRD     ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMHGH    ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMSEQ    ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGETR   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,AIO,DMWORK                         
         B     DMERR                                                            
*                                                                               
DMWRTR   TM    REQOPT,DRAFT                                                     
         BOR   RE                                                               
         ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         B     DMERR                                                            
*                                                                               
DMADDR   TM    REQOPT,DRAFT                                                     
         BOR   RE                                                               
         ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,AIO,DMWORK                         
         B     DMERR                                                            
*                                                                               
DMPUTR   TM    REQOPT,DRAFT                                                     
         BOR   RE                                                               
         ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,AIO,DMWORK                         
*                                                                               
DMERR    MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERAL POOL                                          *         
***********************************************************************         
                                                                                
ACJOBMNT DC    V(ACJOBMNT)                                                      
ACRAPPER DC    V(ACRAPPER)                                                      
BUFFERIN DC    V(BUFFERIN)                                                      
HELLO    DC    V(HELLO)                                                         
RANDOM   DC    V(RANDOM)                                                        
*                                                                               
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
AIOREC   DC    A(IOREC)                                                         
AOFFTAB  DC    A(OFFTAB)                                                        
APOSTB   DC    A(POSTBUF)                                                       
*                                                                               
LDGTAB   DS    0XL4                      UNIT/LEDGER,LENGTH,STATUS              
         DC    C'SJ',AL1(1,LDGPROD)                                             
         DC    C'SP',AL1(1,LDGPAYL)                                             
         DC    C'SQ',AL1(1,LDGPAYL)                                             
         DC    C'SS',AL1(1,LDGPAYL)                                             
         DC    C'ST',AL1(1,LDGPAYL)                                             
         DC    C'SU',AL1(1,LDGPAYL)                                             
         DC    C'SV',AL1(1,LDGPAYL)                                             
         DC    C'SW',AL1(1,LDGPAYL)                                             
         DC    C'SX',AL1(1,LDGPAYL)                                             
         DC    C'SY',AL1(1,LDGPAYL)                                             
         DC    C'T ',AL1(0,LDGTALN)                                             
         DC    C'SM',AL1(1,LDGTALN)                                             
         DC    X'FF'                                                            
*                                                                               
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
*                                                                               
WKOPEN   DC    CL8'OPEN    '                                                    
WKADD    DC    CL8'ADD     '                                                    
WKCLOSE  DC    CL8'CLOSE   '                                                    
*                                                                               
NEWDISP  DC    Y(ACCRFST-ACCKEY)                                                
*                                                                               
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(259,,,)'                              
SORTCARD DC    CL80'SORT FIELDS=(1,004,A),FORMAT=BI,WORK=1'                     
*                                                                               
ATOUT    DC    A(ACCPEEL)                                                       
*                                                                               
DDPARM   DC    CL8'ACCPEEL'                                                     
DSPARM   DC    CL35' '                                                          
         ORG   DSPARM                                                           
DSPACT   DC    CL8'ACCTAPE.'                                                    
DSPA98   DC    CL5'AC98.'                                                       
DSPAGY   DC    CL4'    '           AGENCY                                       
         DC    C'.'                                                             
         DC    C'P'                PEEL                                         
DSPYR    DC    CL2'  '             YEAR                                         
DSPMO    DC    CL2'  '             MONTH                                        
         DC    C'.'                                                             
         DC    C'D'                RUN DATE                                     
DSPRUN   DC    CL6'      '         YYMMDD                                       
         ORG   DSPARM+L'DSPARM                                                  
*                                                                               
UNITS    DC    C'S12',X'FF'                UNITS TO PEEL                        
XUL      DC    C'SJ13141516',X'FF'         UL'S TO SKIP                         
*                                                                               
MXUL     EQU   50                                                               
ULIST    DS    (MXUL)CL2                                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
                                                                                
BUFF     BUFFD TYPE=P,                                                 X        
               KEYLEN=ITMKLNQ,                                         X        
               COLUMNS=2,                                              X        
               FILE=BUFFWK                                                      
*                                                                               
MXOFF    EQU   300                       MAX NUMBER OF OFFICES                  
*                                                                               
         DS    0D                                                               
         DC    CL8'*OFFTAB*'                                                    
OFFTAB   DS    (MXOFF)XL(OFFLNQ)                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*POSTB**'                                                    
POSTBUF  DS    4500C                                                            
                                                                                
         DS    0D                                                               
         DC    CL8'**IO1***'                                                    
IO1      DS    2000C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO2***'                                                    
IO2      DC    F'0'                                                             
IOREC    DS    2000C                                                            
*                                                                               
ACCPEEL  DCB   DDNAME=ACCPEEL,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,LRECL=8200,BLKSIZE=8204                                 
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
                                                                                
WORKD    DSECT                                                                  
*                                                                               
RUNOPT   DS    XL1                 RUN OPTIONS                                  
RUNDDS   EQU   X'80'                DDS OPTION                                  
*                                                                               
REQOPT   DS    XL1                 OPTIONS                                      
DRAFT    EQU   X'80'                DRAFT                                       
RERUN    EQU   X'40'                RERUN                                       
RAPP     EQU   X'20'                ACTIVITY POINTERS                           
OFF2     EQU   X'10'                TWO CHARACTER OFFICE                        
REQUNP   EQU   X'08'                UNPEEL REQUESTED                            
REQALL   EQU   X'04'                REQUESTING ALL UNITS                        
*                                                                               
LDGFLG   DS    XL1                 LEDGER FLAG                                  
LDGPROD  EQU   X'80'                PRODUCTION LEDGER                           
LDGTALN  EQU   X'40'                TALENT LEDGER                               
LDGPAYL  EQU   X'20'                PAYABLE LEDGER                              
LDGCASH  EQU   X'10'                CASH LEDGER                                 
LDGRECV  EQU   X'08'                RECEIVABLE                                  
LDGLREC  EQU   X'04'                (LIKE) RECEIVABLE                           
LDGHEAD  EQU   X'02'                FORCEHED HAS BEEN SET                       
*                                                                               
CASHLDG  DS    CL2                 CASH LEDGER                                  
PRODLDG  DS    CL2                 PRODUCTION LEDGER                            
RECVLDG  DS    CL2                 RECEIVABLE LEDGER                            
*                                                                               
ACCFLG   DS    XL1                 ACCOUNT FLAG                                 
ACCTRN   EQU   X'80'                READ TRANSACTIONS                           
ACCBUF   EQU   X'40'                TRANSACTIONS IN BUFFER                      
ACCUPD   EQU   X'20'                UPDATE ACCOUNT RECORD                       
ACCZRO   EQU   X'10'                BALANCE MUST BE ZERO                        
*                                                                               
ACCBF    DS    PL8                 ACCOUNT BBF                                  
ACCDR    DS    PL8                    "    DEBITS PEELED                        
ACCCR    DS    PL8                    "    CREDITS PEELED                       
ACCUP    DS    PL8                         UNPEELED                             
UNTBF    DS    PL8                 UNIT                                         
UNTDR    DS    PL8                                                              
UNTCR    DS    PL8                                                              
UNTUP    DS    PL8                                                              
LEDBF    DS    PL8                 LEDGER                                       
LEDDR    DS    PL8                                                              
LEDCR    DS    PL8                                                              
LEDUP    DS    PL8                                                              
REQBF    DS    PL8                 REQUEST                                      
REQDR    DS    PL8                                                              
REQCR    DS    PL8                                                              
REQUP    DS    PL8                                                              
RUNBF    DS    PL8                 RUN                                          
RUNDR    DS    PL8                                                              
RUNCR    DS    PL8                                                              
RUNUP    DS    PL8                                                              
*                                                                               
ACCODE   DS    CL15                ACCOUNT CODE                                 
ACNAME   DS    CL36                NAME                                         
PKCOUNT  DS    PL8                 PEELED TRANSCATIONS COUNTER                  
*                                                                               
ACTSW    DS    XL1                 ACTIVITY SWITCH                              
ACTRUN   EQU   X'80'               RUN ACTIVITY                                 
ACTREQ   EQU   X'40'               REQUEST ACTIVITY                             
ACTUNT   EQU   X'20'               UNIT ACTIVITY                                
ACTLDG   EQU   X'10'               LEDGER ACTIVITY                              
*                                                                               
TOUTSW   DS    XL1                 TAPE OUTPUT CONTROL                          
TOUTPUT  EQU   X'80'               WRITING OUTPUT TAPE                          
TOUTOPN  EQU   X'40'               OUTPUT TAPE IS OPENED                        
*                                                                               
AIO      DS    A                                                                
SAVRE    DS    F                                                                
*                                                                               
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    XL4                                                              
*                                                                               
ITMWRK   DS    XL(ITMLNQ)                                                       
BALANCE  DS    PL8                                                              
*                                                                               
RCMOS    DS    XL2                 PACKED YY/MM END DATE                        
MOS      DS    XL2                 TRANSACTION MOS                              
DATE3    DS    CL3                 FIRST DAY OF LAST PEEL MONTH                 
NXTMON   DS    CL3                 FIRST DAY OF NEXT MONTH                      
TODAY3   DS    CL3                 TODAY'S DATE                                 
TODAY2   DS    CL2                                                              
TODAY0   DS    CL6                                                              
YYMM     DS    CL4                                                              
*                                                                               
ID       DS    CL16                                                             
*                                                                               
AUDIT    DS    XL1                 AUDIT OPTIONS                                
AUDIT#   EQU   X'80'                AUDIT #ITEMS                                
AUDIT$   EQU   X'40'                AUDIT $DOLLARS                              
*                                                                               
AUDBUKS  DS    XL(AUDLNQ)          AUDIT ACCUMS                                 
*                                                                               
AUDREC   DS    0C                  AUDIT RECORD                                 
AUDSEQ   DS    XL4                 SEQ NUMBER                                   
AUDLIN   DS    CL255                                                            
         ORG   AUDLIN                                                           
AUDLNLG  DS    CL10                C'NEW LEDGER'                                
         DS    CL1                                                              
AUDLLGC  DS    CL1                 LEDGER CODE                                  
         DS    CL1                                                              
AUDLLGN  DS    CL36                LEDGER NAME                                  
         DS    CL1                                                              
AUDLITMS DS    CL40                TRIGGER=999 RANDOM=00000                     
AUDLNLNQ EQU   *-AUDLNLG           LENGTH OF NEW LEDGER LINE                    
*                                                                               
         ORG   AUDLIN                                                           
AUDLLGT  DS    CL20                C'LEDGER TOTAL'                              
AUDLBUK  DS    XL(AUDLNQ)          ACCUMS                                       
         ORG   AUDLIN+L'AUDLIN                                                  
*                                                                               
RANDINP  DS    XL4                                                              
ELCODE   DS    CL1                                                              
ELEM     DS    CL255                                                            
*                                                                               
SVUNTLDG DS    CL2                                                              
PLFORDT  DS    XL2                 COMPRESSED PEELED FOR DATE                   
DELETEQ  EQU   X'FF'               DELETE MARK                                  
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER OFFICE TOTAL ENTRY                                   *         
***********************************************************************         
                                                                                
OFFD     DSECT                                                                  
OFFICE   DS    CL2                 OFFICE                                       
OFFDR    DS    PL8                 DEBITS                                       
OFFCR    DS    PL8                 DEBITS                                       
OFFLNQ   EQU   *-OFFD                                                           
                                                                                
                                                                                
***********************************************************************         
* DSECT TO COVER AUDIT ACCUMULATORS                                   *         
***********************************************************************         
                                                                                
AUDD     DSECT                                                                  
AUDBK    DS    0X                                                               
AUDSD#   DS    PL8                 SELECTED DEBIT  NUMBER                       
AUDSC#   DS    PL8                          CREDIT                              
AUDSD$   DS    PL8                 SELECTED DEBIT  DOLLARS                      
AUDSC$   DS    PL8                          CREDIT                              
AUDDD#   DS    PL8                 DELETED  DEBIT  NUMBER                       
AUDDC#   DS    PL8                          CREDIT                              
AUDDD$   DS    PL8                 DELETED  DEBIT  DOLLARS                      
AUDDC$   DS    PL8                          CREDIT                              
AUDBBF   DS    PL8                 BBF                                          
AUDBCF   DS    PL8                 BCF                                          
AUDTRGD  DS    PL8                 TRIGGER $ DEBITS                             
AUDTRGC  DS    PL8                    "      CREDITS                            
AUDTRG#  DS    PL8                           NUMBER                             
AUDTRG   DS    PL8                 TRIGGER                                      
AUDLNQ   EQU   *-AUDD                                                           
AUDBKN   EQU   (*-AUDBK)/8                                                      
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER A PRINT LINE                                         *         
***********************************************************************         
                                                                                
PLD      DSECT                                                                  
PL       DS    0C                                                               
         DS    CL1                                                              
PLCODE   DS    CL(L'ACCODE)        ACCOUNT CODE                                 
         DS    CL3                                                              
PLNAME   DS    CL26                NAME                                         
         DS    CL2                                                              
PLBBF    DS    CL17                BBF                                          
PLDR     DS    CL17                DEBITS                                       
PLCR     DS    CL17                CREDITS                                      
PLBCF    DS    CL17                BCF                                          
PLUP     DS    CL14                UNPEELED                                     
*                                                                               
         ORG   PLBBF               FOR JOB CLOSE                                
PLJCDR   DS    CL17                DEBITS                                       
PLJCCR   DS    CL17                CREDITS                                      
         DS    CL2                                                              
PLJCLAD  DS    CL8                 LAST ACTIVITY DATE                           
         DS    CL1                                                              
PLJCDAT  DS    CL8                 CLOSE DATE                                   
         DS    CL1                                                              
PLJC2DAY DS    CL1                 *=CLOSED TODAY                               
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER AN AUDIT PRINT LINE                                  *         
***********************************************************************         
                                                                                
APLD     DSECT                                                                  
APL      DS    0C                                                               
         DS    CL1                                                              
APLACC   DS    CL12                ACCOUNT                                      
         DS    CL1                                                              
APLCON   DS    CL14                CONTRA                                       
         DS    CL1                                                              
APLREF   DS    CL6                 REFERENCE                                    
         DS    CL1                                                              
APLDATE  DS    CL8                 DATE                                         
         DS    CL1                                                              
APLBTCH  DS    CL6                 BATCH                                        
         DS    CL3                                                              
APLOFF   DS    CL2                 OFFICE                                       
         DS    CL1                                                              
APLNARP  DS    CL24                NARRATIVE PRINT AREA                         
         DS    CL1                                                              
APLDR    DS    CL15                                                             
         DS    CL1                                                              
APLCR    DS    CL15                                                             
         DS    CL24                                                             
APLNARS  DS    CL70                NARRATIVE SORT AREA                          
APLLNQ   EQU   *-APL                                                            
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVET ITEM IN BUFFERIN TABLE                               *         
***********************************************************************         
                                                                                
ITMD     DSECT                                                                  
ITMKEY   DS    0XL(ITMKLNQ)                                                     
ITMCACT  DS    XL(L'TRNKCACT)      CONTRA                                       
ITMDATE  DS    XL3                 DATE                                         
ITMREF   DS    CL6                 REFERENCE                                    
ITMKLNQ  EQU   *-ITMCACT                                                        
ITMDR    DS    PL8                 DEBITS                                       
ITMCR    DS    PL8                 CREDITS                                      
ITMLNQ   EQU   *-ITMD                                                           
         EJECT                                                                  
MPARMD   DSECT                                                                  
       ++INCLUDE ACJOBMNTD                                                      
         EJECT                                                                  
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACQD                                                                          
         PRINT OFF                                                              
       ++INCLUDE ACQD                                                           
         PRINT ON                                                               
* ACRAPPERD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRAPPERD                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* DDBUFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT ON                                                               
       ++INCLUDE DDMASTD                                                        
         PRINT OFF                                                              
* DDCOMFACSD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACSD                                                     
         PRINT ON                                                               
* DDCUREDITD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCUREDITD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACREP9802 02/12/20'                                      
         END                                                                    
