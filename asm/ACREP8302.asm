*          DATA SET ACREP8302  AT LEVEL 017 AS OF 05/01/02                      
*PHASE AC8302A,+0                                                               
*INCLUDE ACLIST                                                                 
*INCLUDE SORTER                                                                 
         SPACE 2                                                                
*              PROFILES                                                         
         SPACE 1                                                                
* 1      SHOW COMPANY NAME                                                      
* 2      SHOW COMPANY ADDRESS                                                   
* 3      SUMMARY AT REQLAST                                                     
* 4      SUMMARY AT ACCLAST                                                     
* 5      SHOW REFERENCE DETAIL                                                  
* 6      AGE BY BILL DATE,MOS,DUE DATE,NO AGEING                                
* 7      DON'T MERGE BILLS OF DIFFERENT DATES                                   
* 8      PRINT ACCOUNT LEVELS IN HEADS                                          
* 9      SUPPRESS SOURCE TOTALS                                                 
* 10     SORT FIELD 1                                                           
* 11     SORT FIELD 2                                                           
* 12     SORT FIELD 3                                                           
* 13     SUPPRESS AS AT DATE                                                    
* 14     PRINT '* = PARTIALLLY PAID'                                            
* 15     PRINT DUE DATE / ADVERTISING YEAR                                      
         TITLE 'AC8302 - RECEIVABLE STATEMENTS'                                 
AC8302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC83**,R9,R8,RR=R5                                           
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(WORK CSECT)                             
         LA    RC,SPACEND                                                       
         USING AC83D,RC            RC=A(SPACEND)                                
*                                                                               
*              HANDLE MODE SETTINGS                                             
*                                                                               
         MVC   MYMODE,MODE                                                      
         CLI   MYMODE,LEDGFRST     *                                            
         BE    LDG                                                              
         CLI   MYMODE,LEVAFRST     *                                            
         BE    LEVAF                                                            
         CLI   MYMODE,LEVBFRST     *                                            
         BE    LEVBF                                                            
         CLI   MYMODE,LEVCFRST     *                                            
         BE    LEVCF                                                            
         CLI   MYMODE,PROCACC      *                                            
         BE    LEVDF                                                            
         CLI   MYMODE,PROCTRNS     *                                            
         BE    TRNS2                                                            
         CLI   MYMODE,SBACFRST     *                                            
         BE    SUBAF                                                            
         CLI   MYMODE,SBACLAST     *                                            
         BE    SUBAL                                                            
         CLI   MYMODE,ACCLAST      *                                            
         BE    LEVDL                                                            
         CLI   MYMODE,LEVCLAST     *                                            
         BE    LEVCL                                                            
         CLI   MYMODE,LEVBLAST     *                                            
         BE    LEVBL                                                            
         CLI   MYMODE,LEVALAST     *                                            
         BE    LEVAL                                                            
         CLI   MYMODE,REQLAST      *                                            
         BE    REQL2                                                            
         CLI   MYMODE,RUNFRST                                                   
         BE    RUN1                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*              FIRST FOR RUN                                                    
*                                                                               
RUN1     ST    R5,RELO                                                          
         LA    RE,RELOTAB                                                       
         LA    R1,VTYPES                                                        
RELOOP   L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
         SPACE 1                                                                
         B     EXIT                                                             
         EJECT                                                                  
*              FIRST FOR LEDGER                                                 
*                                                                               
LDG      LA    R7,TOTALS           CLEAR TOTAL LINES                            
         LA    R0,6                                                             
*                                                                               
         USING TOTD,R7                                                          
LDG1     MVI   TOTLEN,0            LEVEL LENGTH                                 
         NI    TOTSTAT,LVNME       STATUS SWITCHES                              
         XC    TOTACC,TOTACC       CLEAR ACCOUNT CODES                          
         MVC   TOTNAME,SPACES      NAMES                                        
         MVC   TOTAMTS(32),=4PL8'0'    AND TOTALS                               
         LA    R7,TOTLNQ(R7)                                                    
         BCT   R0,LDG1                                                          
*                                                                               
         LA    R7,LVATOT                                                        
         L     R2,ADLDGHIR                                                      
*                                                                               
         USING ACHEIRD,R2                                                       
         LA    R1,ACHRLEVA         SAVE LEVEL LENGTHS IN TOTAL TABLE            
         LA    R0,1                COUNT NUMBER OF LEVELS                       
LDG1A    CLI   0(R1),12            IF IT'S THE LAST LEVEL                       
         BNE   *+8                                                              
         LA    R7,LVDTOT           USE LEVEL D                                  
         MVC   TOTLEN,0(R1)                                                     
         OI    TOTSTAT,LVACT       LEVEL IS ACTIVE                              
         STC   R0,TOTLEV           LEVEL NUMBER                                 
         CLI   0(R1),12                                                         
         BE    LDG1D                                                            
         AH    R0,=H'1'                                                         
         LA    R1,16(R1)                                                        
         LA    R7,TOTLNQ(R7)                                                    
         B     LDG1A                                                            
*                                                                               
LDG1D    STC   R0,LEVELS                                                        
         OI    SRCTOT+(TOTSTAT-TOTD),LVACT  SOURCE AND                          
         OI    REQTOT+(TOTSTAT-TOTD),LVACT  REQUEST ARE ALWAYS ACTIVE           
         MVI   TOTSW,C'N'                                                       
         MVC   AGEBY,PROGPROF+5    PROFILE OPTION FOR AGEING                    
*                                                                               
         CLI   QOPT3,C' '          REQUEST OPTION FOR AGEING                    
         BE    *+10                AGE BY, B, M OR D                            
         MVC   AGEBY,QOPT3         OVER RIDE PROFILE                            
*                                                                               
         CLI   AGEBY,0                                                          
         BNE   *+8                                                              
         MVI   AGEBY,C'B'          DEFAULT IS BILL DATE                         
         CLI   AGEBY,C'Y'                                                       
         BNE   *+8                 FOR OLD PROFILES A 'Y'                       
         MVI   AGEBY,C'B'          MEANS AGE ON BILL DATE                       
         CLI   QOPT2,C'M'                                                       
         BE    LDG2A                                                            
         CLI   AGEBY,C'M'          IF HEADING IS FOR INCREMENT                  
         BNE   LDG2A               AND AGE BY MOS - THEY'RE CRAZY               
         MVI   AGEBY,C'B'          AGE IT BY BILL DATE                          
LDG2A    CLI   QOPT2,C'C'          OPTION TO SHOW INCOME                        
         BNE   *+8                 MEANS NO AGEING                              
         MVI   AGEBY,C'N'                                                       
*                                                                               
         MVI   SUMOPT,0            SET SUMMARY OPTIONS                          
         MVI   SUMLEV,4            ASSUME 4 LEVELS                              
         CLI   QOPT1,C' '                                                       
         BE    LDG3                                                             
         OI    SUMOPT,SUMONLY      SUMMARY ONLY                                 
         TM    QOPT1,X'F0'         LEVEL SUMMARY                                
         BNO   LDG2D                                                            
         OI    SUMOPT,RQAC+RQSRC   ACCOUNT AND/ SOURCE SUMMARY                  
         MVC   SUMLEV,QOPT1        SUMMARY LEVEL NUMBER                         
         NI    SUMLEV,X'0F'                                                     
         B     LDG3                                                             
*                                                                               
LDG2D    CLI   QOPT1,C'S'                                                       
         BNE   *+8                                                              
         OI    SUMOPT,RQAC+RQSRC   ACCOUNT AND/ SOURCE SUMMARY                  
         CLI   QOPT1,C'D'                                                       
         BNE   *+8                                                              
         OI    SUMOPT,RQAC+RQACSRC+RQSRC  ACCOUNT SUMMARY BY SOURCE             
         B     LDG4                                                             
*                                                                               
LDG3     CLI   PROGPROF+2,C'Y'     SUMMARY FOR EACH RECEIVABLE ACCT             
         BNE   *+8                                                              
         OI    SUMOPT,RQAC+RQSRC   ACC. SUMMARY AT REQLAST                      
         CLI   PROGPROF+2,C'D'                                                  
         BNE   *+8                                                              
         OI    SUMOPT,RQAC+RQACSRC+RQSRC  ACCOUNT SUMMARY BY SOURCE             
*                                                                               
LDG4     CLI   PROGPROF+3,C'Y'     SUMMARY AT END OF EACH ACCOUNT               
         BNE   LDG4A                                                            
         CLI   QOPT1,C' '          IGNORE IF IT'S A SUMMARY REPORT              
         BNE   *+8                                                              
         OI    SUMOPT,ACSRC                                                     
LDG4A    CLI   SUMOPT,0            ANY SUMMARIES                                
         BE    LDG5                                                             
         GOTO1 BUFFALO,DMCB,=C'SET',VBUFF                                       
*                                                                               
LDG5     MVC   REQNAME,SPACES           SET-UP HEDLINE DATA                     
         CLI   QOPT5,C'N'               SUPPRESS REQUESTORS NAME                
         BE    LDG6                                                             
         MVC   REQNAME(9),=C'REQUESTOR'                                         
         MVC   REQNAME+10(12),QUESTOR                                           
LDG6     MVC   COMPNAME,SPACES                                                  
         CLI   PROGPROF,C'O'            PRINT ORIGIN NAME                       
         BNE   LDG7                                                             
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         CLC   ORIGINAM,SPACES                                                  
         BE    LDG7                                                             
         MVC   COMPNAME(33),ORIGINAM                                            
         B     LDG8                                                             
LDG7     DS    0H                                                               
         CLI   PROGPROF,C'Y'            PRINT MAIN AGENCY NAME                  
         BNE   LDG8                                                             
         L     RF,ADCMPNAM                                                      
         ZIC   R1,1(RF)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   COMPNAME(0),2(RF)                                                
LDG8     MVC   COMPADDR(104),SPACES                                             
         CLI   PROGPROF+1,C'O'          TO PRINT USER ID ADDRESS                
         BNE   LDG8A                                                            
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         GOTO1 CHOPPER,DMCB,(33,ORIGINAD),(26,COMPADDR),2                       
         B     LDG10                                                            
LDG8A    CLI   PROGPROF+1,C'Y'          PRINT MAIN AGENCY'S ADDR.               
         BNE   LDG10                                                            
         L     RF,ADCMPADD                                                      
         ZIC   R1,1(RF)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   COMPADDR(0),3(RF)                                                
LDG10    MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,0               SET SUB PROGRAM NUMBER                  
         MVC   SAVESPRG,RCSUBPRG                                                
         BAS   RE,BUILDATE              BUILD LIST OF AGEING DATES              
         MVC   ASATDATE,SPACES                                                  
         GOTO1 DATCON,DMCB,(1,DATAB),(8,ASATDATE)                               
         BAS   RE,FORMDATE              BUILD HEADINGS FOR DATES                
         MVI   SORTSW,C'N'              INITIALIZE                              
         MVI   TALSW,C'N'                                                       
         MVI   BILLSW,C'N'                                                      
         TM    SUMOPT,SUMONLY           DONT SORT IF SUMMARIZING                
         BO    LDG19                                                            
         MVC   SORTOPTS,PROGPROF+9      SAVE SORT OPTINS                        
*                                                                               
         CLI   QOPT7,C'Y'          SORT OPTION                                  
         BE    LDG11                                                            
         CLI   QOPT7,C'C'          BY COMMERCIAL FOR US TALENT                  
         BNE   LDG19                                                            
         MVI   TALSW,C'Y'   TREAT CLIENT AS CONTRA/ACCOUNT(TALENT)              
*                                                                               
LDG11    OC    SORTOPTS,SORTOPTS                                                
         BZ    LDG19                                                            
         CLC   SORTOPTS,=C'***'                                                 
         BE    LDG19                                                            
*                                  BUILD SORT CARD                              
         MVC   SORTCARD,SPACES                                                  
         MVC   SORTCARD(13),=C'SORT FIELDS=('                                   
         LA    R2,SORTCARD+12                                                   
         LA    R3,SORTOPTS                                                      
         LA    R4,L'SORTOPTS                                                    
LDG13    CLI   0(R3),C'*'                                                       
         BE    LDG18                                                            
         CLI   0(R3),0                                                          
         BE    LDG18                                                            
         LA    RE,SORTKEYS         LOOK UP SORT PARMS IN TABLE                  
LDG15    CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,RE),0(R3)                                                    
         BE    *+12                                                             
         LA    RE,L'SORTKEYS(RE)                                                
         B     LDG15                                                            
         ZIC   R1,1(RE)            DISPLACEMENT                                 
         CVD   R1,DUB                                                           
         UNPK  1(2,R2),DUB                                                      
         OI    2(R2),X'F0'                                                      
         MVI   3(R2),C','                                                       
         ZIC   R1,2(RE)            LENGTH                                       
         CVD   R1,DUB                                                           
         UNPK  4(2,R2),DUB                                                      
         OI    5(R2),X'F0'                                                      
         MVC   6(3,R2),=C',A,'                                                  
         LA    R2,8(R2)                                                         
         MVI   SORTSW,C'Y'         SET INITIALIZE PENDING                       
LDG18    LA    R3,1(R3)                                                         
         BCT   R4,LDG13                                                         
         MVC   0(18,R2),=C'),FORMAT=BI,WORK=1'                                  
         SPACE 1                                                                
         LA    R1,SORTLEN                                                       
         CVD   R1,DUB              CONVERT RECORD LENGTH TO CHARACTER           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         SPACE 1                                                                
*                                                                               
LDG19    MVC   DETSW,PROGPROF+4    PROFILE OPTION TO SHOW REF DETAIL            
*                                                                               
         CLI   QOPT4,C' '          REQ OPTION TO SHOW REFERENCE DETAIL          
         BE    *+10                                                             
         MVC   DETSW,QOPT4                                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*              FIRST FOR ACCOUNTS                                               
*                                                                               
LEVAF    L     R1,ADHEIRA                                                       
         L     R2,ADLVANAM                                                      
         LA    R7,LVA                                                           
         B     LEVALL                                                           
*                                                                               
LEVBF    L     R1,ADHEIRB                                                       
         L     R2,ADLVBNAM                                                      
         LA    R7,LVB                                                           
         B     LEVALL                                                           
*                                                                               
LEVCF    L     R1,ADHEIRC                                                       
         L     R2,ADLVCNAM                                                      
         LA    R7,LVC                                                           
         B     LEVALL                                                           
*                                                                               
LEVDF    L     R1,ADACC                                                         
         L     R2,ADACCNAM                                                      
         LA    R7,LVD                                                           
*                                                                               
*                                                                               
LEVALL   MVI   THSACC,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   RCSUBPRG,SAVESPRG                                                
         MH    R7,=Y(TOTLNQ)                                                    
         LA    R7,TOTALS(R7)                                                    
*                                                                               
         MVC   TOTACC,3(R1)                                                     
         MVC   TOTNAME,SPACES                                                   
         MVC   TOTAMTS(32),=4PL8'0' FIRST TIME CLEAR ACCUMS                     
         NI    TOTSTAT,X'FF'-ACACT TURN-OFF ACTIVITY SWITCH                     
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TOTNAME(0),2(R2)                                                 
         B     EXIT                                                             
         EJECT                                                                  
*              FIRST FOR SUB-ACCOUNT                                            
*                                                                               
SUBAF    L     R2,ADSUBAC                                                       
         LA    R7,SRCTOT                                                        
         NI    TOTSTAT,X'FF'-ACACT   TURN-OFF ACTIVITY SWITCH                   
*                                                                               
         USING TRSUBHD,R2                                                       
         MVC   SAVESBAC,TRSBACNT                                                
         XC    SORTREF,SORTREF                                                  
         XC    SORTDUE,SORTDUE                                                  
         MVI   THISBAC,C'N'                                                     
         MVI   INCLUDE,C'I'                                                     
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         OC    VLISTREC,VLISTREC                                                
         BZ    EXIT                NO LIST RECORD                               
         GOTO1 VACLIST,DMCB,VLISTREC,TRSBACNT+3                                 
         CLI   DMCB,0                                                           
         BNE   *+6                                                              
         DC    H'0'                BAD LIST RECORD                              
         MVC   INCLUDE,DMCB        I=INCLUDE, E=EXCLUDE                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*              DEAL WITH A TRANSACTION                                          
*                                                                               
*                                  R3=A(TRNSTAB)                                
TRNS2    L     R2,ADTRANS                                                       
         USING TRANSD,R2                                                        
         CLI   TRNSEL,X'44'                                                     
         BNE   EXIT                                                             
         MVI   THISLEV,LVD         SET LEVEL FOR PRINT                          
         LR    R3,R2                                                            
         SH    R3,DATADISP                                                      
         USING ACKEYD,R3                                                        
         OC    ACDTPEEL,ACDTPEEL                                                
         BNZ   EXIT                IGNORE PEELED                                
         CLI   INCLUDE,C'E'                                                     
         BE    EXIT                SKIP THIS MEDIA                              
         TM    TRNSSTAT,X'20'                                                   
         BO    EXIT                SKIP REVERSALS                               
         OC    SORTREF,SORTREF     FIRST BILL                                   
         BZ    TRNS3D                                                           
         CLC   SORTREF,TRNSREF     SAME AS LAST                                 
         BNE   TRNS3                                                            
         CLC   TRNSDATE,SORTDATE                                                
         BE    TRNS3B                                                           
         CLI   PROGPROF+6,C'Y'     OPTION TO SPLIT BILLS                        
         BNE   TRNS3B              FOR DIFFERENT DATES                          
TRNS3    BAS   RE,BILLEND          NEW BILL                                     
         B     TRNS3D                                                           
TRNS3B   CLI   DETSW,C'A'          SAME BILL - IF SHOW ALL DETAIL               
         BNE   TRNS6                                                            
         B     TRNS4               THEN SET UP AGAIN                            
TRNS3D   CLI   DETSW,C'A'          NEW BILL - IF SHOWING ALL DETAIL             
         BNE   *+16                                                             
         L     R3,ADRTRNS          THEN RESET TRANSACTION TABLE                 
         MVI   0(R3),X'FF'         MARK END-OF-TABLE                            
         ST    R3,ATRNSTAB                                                      
         SPACE 1                                                                
         ZAP   BILLTOT,=P'0'       CLEAR TOTAL ACCUM                            
TRNS4    MVC   SORTSBAC,SAVESBAC                                                
         MVC   SORTREF,TRNSREF     SET UP FOR THIS BILL                         
*                                                                               
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         MVC   SORTMOS,ACMMDTE                                                  
         DROP  RF                                                               
*                                                                               
         MVC   SORTDATE,TRNSDATE                                                
         ZAP   SORTINCM,=P'0'                                                   
         ZAP   SORTCASH,=P'0'                                                   
         ZAP   THISAMNT,=P'0'                                                   
         LA    R1,SORTAMTS        CLEAR PACKED SORT FIELDS                      
         LA    R0,SORTNUMB                                                      
         ZAP   0(L'SORTAMTS,R1),=P'0'                                           
         LA    R1,L'SORTAMTS(R1)                                                
         BCT   R0,*-10                                                          
         SPACE 1                                                                
         MVI   SORTPART,0                                                       
         XC    SORTNUM,SORTNUM                                                  
         XC    SORTNET,SORTNET                                                  
         XC    SORTCLI,SORTCLI                                                  
         XC    SORTPROD,SORTPROD                                                
         XC    SORTCOMN,SORTCOMN                                                
         MVC   SORTDTE2,SPACES                                                  
         MVI   SORTPROF,0                                                       
TRNS6    ZAP   DUB,TRNSAMNT                                                     
         TM    TRNSSTAT,X'80'                                                   
         BZ    TRNS21                                                           
*                                                                               
         USING ACMD,RF         GET MOS FROM MONACC                              
         L     RF,AMONACC                                                       
         MVC   SORTMOS,ACMMDTE                                                  
         DROP  RF                                                               
*                                                                               
         AP    THISAMNT,DUB        DEBIT TRANSACTION HANDLING                   
         AP    SORTAMTS(8),TRNSAMNT                                             
         ZIC   R1,1(R2)                                                         
         AR    R1,R2                                                            
         SR    RE,RE                                                            
TRNS8    CLI   0(R1),0                                                          
         BE    TRNS50                                                           
         CLI   0(R1),X'50'         CASH ELEMENT                                 
         BE    TRNS13                                                           
         CLI   0(R1),X'23'         NUMBER ELEMENT                               
         BE    TRNS11                                                           
         CLI   0(R1),X'61'         DUE DATE ELEMENT                             
         BE    TRNS19                                                           
         CLI   0(R1),X'4F'         CLIENT/PROD/JOB ELEMENT                      
         BE    TRNS17                                                           
TRNS9    IC    RE,1(R1)            GET ELEMENT LENGTH                           
         AR    R1,RE               BUMP TO NEXT ELEMENT                         
         B     TRNS8               SEE WHICH ELEMNET IT IS                      
         USING ACOTHERD,R1                                                      
TRNS11   MVC   SORTNUM,ACOTNUM                                                  
         MVC   SORTPROF,ACOTPROF                                                
         MVC   SORTDTE2,ACOTDATE                                                
         CLI   ACOTLEN,ACOTLNQ2                                                 
         BL    TRNS9                                                            
         MVC   SORTNET,ACOTNET     NETWORK NAME                                 
         B     TRNS9                                                            
         USING TRCASHD,R1                                                       
TRNS13   CLI   TRCSTYPE,C'D'                                                    
         BNE   TRNS15                                                           
         AP    SORTCASH,TRCSAMNT                                                
         B     TRNS9                                                            
TRNS15   CLI   TRCSTYPE,C'I'                                                    
         BNE   TRNS9                                                            
         AP    SORTINCM,TRCSAMNT                                                
         B     TRNS9                                                            
         USING TRCPJD,R1                                                        
TRNS17   CLI   PROGPROF+4,C'C'                                                  
         BNE   TRNS9                MUST ASK FOR COMMERCIAL DETAIL              
         CLI   TRCPTYPE,C'C'            IS IT A COMMERCIAL                      
         BE    TRNS17A                                                          
         CLI   TRCPTYPE,C'T'            IS IT A T&R SPONSOR/EST                 
         BNE   TRNS9                                                            
         MVC   SORTSBAC,SPACES          CLEAR OUT SUB ACC                       
         MVC   SORTSBAC+3(5),TRCPSPON   MOVE IN SPONSOR                         
         MVC   SORTEST,TRCPEST          MOVE IN ESTIMATE                        
         MVI   TNRSW,C'Y'               TURN ON T&R SWITCH                      
         XC    SORTNUM,SORTNUM                                                  
         XC    SORTNET,SORTNET                                                  
         XC    SORTPROF,SORTPROF                                                
         XC    SORTDTE2,SORTDTE2                                                
         B     TRNS9                                                            
TRNS17A  EQU   *                                                                
         MVC   SORTCLI,TRCPCLI                                                  
         MVC   SORTPROD,TRCPPROD                                                
         MVC   SORTCOMN,TRCPJOB                                                 
         MVI   TNRSW,C'N'                                                       
         XC    SORTNUM,SORTNUM                                                  
         XC    SORTNET,SORTNET                                                  
         XC    SORTPROF,SORTPROF                                                
         XC    SORTDTE2,SORTDTE2                                                
TRNS18   CLI   TALSW,C'Y'                                                       
         BNE   TRNS9                                                            
         MVC   SORTSBAC,SPACES                                                  
         MVC   SORTSBAC+3(3),TRCPCLI    USE CLIENT AS CONTRA/ACCOUNT            
         XC    SORTCLI,SORTCLI           DON'T NEED MEMO CLIENT                 
         B     TRNS9                                                            
         USING TRDUED,R1                                                        
TRNS19   CLI   AGEBY,C'D'                                                       
         BE    *+12                                                             
         CLI   PROGPROF+14,C'D'                                                 
         BNE   TRNS9                                                            
         MVC   SORTDUE,TRDUDATE    DUE DATE OVERRIDE                            
         B     TRNS9                                                            
TRNS21   MP    DUB,=P'-1'          CREDIT TRANSACTION HANDLING                  
         AP    THISAMNT,DUB                                                     
         MVI   SORTPART,X'FF'                                                   
TRNS50   DS    0H                                                               
         AP    BILLTOT,DUB                                                      
         CLI   DETSW,C'A'          IF SHOWING ALL DETAILS                       
         BNE   EXIT                                                             
         CP    BILLTOT,=P'0'       AND IF SUM IS NOT ZERO                       
         BE    TRNS52                                                           
         L     R3,ATRNSTAB                                                      
         MVC   0(SORTLEN,R3),SORTREC             SAVE TRANSACTION               
         ZAP   SORTAMTS-SORTREC(8,R3),THISAMNT                                  
         LA    R3,SORTLEN(R3)                    AND BUMP TABLE                 
         B     *+8                                                              
TRNS52   L     R3,ADRTRNS          RESET TRANSACTION TABLE                      
         MVI   0(R3),X'FF'         MARK END                                     
         ST    R3,ATRNSTAB         AND SAVE ADDRESS                             
         B     EXIT                                                             
         EJECT                                                                  
*              LAST FOR SUB-ACCOUNT                                             
*                                                                               
SUBAL    BAS   RE,BILLEND          DEAL WITH LAST BILL                          
         CLI   TALSW,C'Y'          IF USE CLIENT AS CONTRA                      
         BE    EXIT                IGNORE THIS CONTRA BREAK                     
         BAS   RE,SUBACC                                                        
         B     EXIT                                                             
         SPACE 1                                                                
SUBACC   NTR1  ,                                                                
         CLI   THISBAC,C'Y'                                                     
         BNE   EXIT                                                             
         CLI   SUMOPT,0            ANY SUMMARIES                                
         BE    SUBA6                                                            
         BAS   RE,SUMPOST                                                       
SUBA6    LA    R2,SRC              PRINT SOURCE TOTALS                          
         CLI   PROGPROF+8,C'Y'     OPTION TO SUPPRESS SOURCE TOTALS             
         BNE   *+12                                                             
         MVI   TOTSW,C'Y'                                                       
         B     SUBA8                                                            
         TM    SUMOPT,SUMONLY      NO TOTALS IF SUMMARY                         
         BO    *+8                                                              
         BAS   RE,PRTTOTS                                                       
SUBA8    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*              LAST FOR ACCOUNTS                                                
         SPACE 1                                                                
LEVAL    LA    R2,LVA                                                           
         B     LEVALL4                                                          
         SPACE 1                                                                
LEVBL    LA    R2,LVB                                                           
         B     LEVALL4                                                          
         SPACE 1                                                                
LEVCL    LA    R2,LVC                                                           
         B     LEVALL4                                                          
         SPACE 1                                                                
LEVDL    LA    R2,LVD                                                           
         CLI   SORTSW,C'N'                                                      
         BE    LEVALL2                                                          
         BAS   RE,SORTPRT                                                       
         SPACE 1                                                                
LEVALL2  CLI   THSACC,C'Y'                                                      
         BNE   EXIT                                                             
         TM    SUMOPT,SUMONLY      NO TOTALS IF SUMMARY                         
         BO    *+8                                                              
         BAS   RE,PRTTOTS          PRINT ACCOUNT TOTALS                         
         MVI   BILLSW,C'N'                                                      
*                                                                               
LEVALL4  STC   R2,THISLEV                                                       
         TM    SUMOPT,ACSRC        MEDIA SUMMARY AT END OF ACCOUNT              
         BZ    EXIT                                                             
*                                                                               
         LR    R7,R2               LEVEL NUMBER                                 
         MH    R7,=Y(TOTLNQ)                                                    
         LA    R7,TOTALS(R7)                                                    
         CLI   TOTLEN,12           IS THIS THE LOW LEVEL                        
         BE    LEVALL6             IF IT IS, IT MUST BE PRINTED                 
         CLC   TOTNUM,SUMLEV       OR THE LOWEST SUMMARY LEVEL                  
         BE    LEVALL6                                                          
         LA    RE,TOTLNQ(R7)       RE=TO LOWER LEVEL OF TOTALS ENTRY            
         TM    TOTSTAT-TOTD(RE),LVACT       IS IT AN ACTIVE LEVEL               
         BO    *+8                                                              
         LA    RE,LVDTOT           RE=LOWEST(POSTING ACCOUNT)                   
         LA    R0,4                                                             
*                                                                               
         LA    R1,TOTAMTS          R1=TO THIS LEVEL ACCUMS                      
         LA    RF,TOTAMTS-TOTD(RE) RF=TO LOWER LEVEL ACCUMS                     
         CP    0(8,R1),0(8,RF)                                                  
         BNE   LEVALL6             NOT EQUAL, MUST PRINT THIS LEVEL             
         LA    R1,8(R1)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-18                                                          
         B     EXIT                                                             
*                                                                               
LEVALL6  BAS   RE,PRTBSUM          PRINT BILLING SOURCE SUMMARY                 
         B     EXIT                                                             
         EJECT                                                                  
*              LAST FOR REQUEST                                                 
*                                                                               
REQL2    MVI   FRCETOT,C'N'                                                     
         TM    SUMOPT,RQAC         PRINTING A RECEIVALBE SUMMARY                
         BO    REQL3                                                            
         MVI   FORCEHED,C'Y'       IF NOT JUST DO REQUEST TOTALS                
         MVI   RCSUBPRG,2                                                       
         LA    R2,REQ                                                           
         BAS   RE,PRTTOTS                                                       
         B     EXIT                                                             
         SPACE 1                                                                
REQL3    MVI   RCSUBPRG,2          SET SUB-PRG FOR ACCOUNT/CODE                 
         TM    SUMOPT,RQACSRC      REQUESTING DETAIL                            
         BZ    *+8                                                              
         MVI   RCSUBPRG,3          SET FOR ACCOUNT/CODE/MEDIA(DETAIL)           
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   SORTPART,0                                                       
         XC    BUFREC,BUFREC                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',VBUFF,BUFREC,0                             
         TM    8(R1),X'80'         BUFF E-O-F                                   
         BO    REQL30                                                           
         CLI   BUFACC,X'FF'                                                     
         BE    REQL30              SOURCE SUMMARY RECORDS                       
*                                                                               
REQL4    CLI   BUFSRC,0            MUST BE A HEADER RECORD                      
         BNE   REQL6                                                            
         SR    R7,R7                                                            
         IC    R7,BUFNUM           RELATIVE NUMBER IN TOTALS                    
         MH    R7,=Y(TOTLNQ)                                                    
         LA    R7,TOTALS(R7)                                                    
         MVC   TOTNAME,BUFNAME     SAVE THE NAME                                
         MVC   TOTACC,BUFACC       ACCOUNT CODE                                 
         MVC   TOTAMTS(32),BUFBUCK     AND AMOUNTS                              
*                                                                               
         CLI   TOTLEN,12           IS THIS THE LOW LEVEL                        
         BE    REQL10              IF IT IS, IT MUST BE PRINTED                 
         CLC   TOTNUM,SUMLEV       OR THE SUMMARY LEVEL                         
         BE    REQL10                                                           
         SR    R2,R2                                                            
         IC    R2,TOTLEN                                                        
         LA    R2,TOTACC(R2)                                                    
         MVI   0(R2),C' '          REMOVE THE FF FROM HIGH LEVELS               
         LA    RE,TOTLNQ(R7)       RE=TO LOWER LEVEL OF TOTALS ENTRY            
         TM    TOTSTAT-TOTD(RE),LVACT       IS IT AN ACTIVE LEVEL               
         BO    *+8                                                              
         LA    RE,LVDTOT           RE=LOWEST(POSTING ACCOUNT)                   
         LA    R0,4                                                             
*                                                                               
         LA    R1,TOTAMTS          R1=TO THIS LEVEL ACCUMS                      
         LA    RF,TOTAMTS-TOTD(RE) RF=TO LOWER LEVEL ACCUMS                     
         CP    0(8,R1),0(8,RF)                                                  
         BNE   REQL10              NOT EQUAL, MUST PRINT THIS LEVEL             
         LA    R1,8(R1)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-18                                                          
*                                                                               
*              SKIP PRINTING OF THIS LEVEL                                      
*                                                                               
REQL6    BAS   R3,REQL25           GET NEXT BUFFALO REOCRD                      
         CLI   BUFSRC,0            IS IT AN ACCOUNT RECORD                      
         BE    REQL4               IF IT IS, PROCESS IT                         
         B     REQL6               SKIP MEDIA DETAIL FOR THIS ACCOUNT           
*                                                                               
*              PRINT SOURCE SUMMARY                                             
*                                                                               
REQL10   CLI   BUFREC,X'FF'                                                     
         BE    REQL14                                                           
         TM    SUMOPT,RQACSRC      IF PRINT SOURCE SUMMARY                      
         BZ    REQL14                                                           
         BAS   R3,REQL26           PRINT THE ACCOUNT CODE                       
         BAS   RE,PRNT             AND NAME NOW                                 
         ZAP   SRCCNT,=P'0'        COUNT NUMBER OF SOURCE RECORDS               
*                                                                               
REQL12   BAS   R3,REQL25           GET NEXT BUFFALO RECORD                      
         CLI   BUFSRC,X'FF'        IS IT END OF  ACCOUNT                        
         BNE   REQL13              IF NOT PROCESS SOURCE RECORD                 
         CP    SRCCNT,=P'1'        IF MORE THAN 1 SOURCE RECORD                 
         BH    REQL15              MUST PRINT TOTAL                             
         B     REQL18              ELSE, SKIP THE TOTAL                         
*                                                                               
REQL13   MVC   P+3(12),BUFSRC      SOURCE                                       
         MVC   FORMACCS(32),BUFBUCK TOTALS                                      
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNT                                                          
         AP    SRCCNT,=P'1'                                                     
         B     REQL12                                                           
*                                                                               
*              ACCOUNT TOTALS                                                   
*                                                                               
REQL14   CLI   TOTLEN,12           DON'T PRINT 'TOTAL FOR'                      
         BE    REQL16              IF IT'S LOW LEVEL                            
         CLC   TOTNUM,SUMLEV                                                    
         BE    REQL16                                                           
REQL15   MVI   FRCETOT,C'Y'        PRINT TOTALS FOR                             
REQL16   BAS   R3,REQL26           WITH ACCOUNT CODE/NAME                       
         MVC   FORMACCS(32),TOTAMTS LEVEL TOTALS                                
         BAS   RE,FORMAT                                                        
REQL18   MVI   SPACING,2                                                        
         BAS   RE,PRNT                                                          
         BAS   R3,REQL25           GET NEXT BUFFALO RECORD                      
         B     REQL4                                                            
*                                                                               
*              GET THE NEXT BUFFALO RECORD                                      
*                                                                               
REQL25   GOTO1 BUFFALO,DMCB,=C'SEQ',VBUFF,BUFREC,0                              
         TM    8(R1),X'80'         BUFF E-O-F                                   
         BO    REQL30                                                           
         CLI   BUFACC,X'FF'        END OF ACCOUNT RECORDS                       
         BE    REQL30                                                           
         BR    R3                                                               
*                                                                               
*              FORMAT THE ACCOUNT CODE AND NAME                                 
*                                                                               
REQL26   MVC   P,SPACES                                                         
         LA    R5,P+1              START OF ACC NAME                            
         LA    R0,27               MAX LENGTH OF NAME                           
         CLI   FRCETOT,C'Y'        LOW LEVEL ACCOUNT                            
         BNE   REQL27                                                           
         MVC   P+2(10),=C'TOTALS FOR'                                           
         LA    R5,P+13                                                          
         LA    R0,15                                                            
REQL27   MVC   0(12,R5),TOTACC      ACCOUNT CODE                                
         LA    R5,11(R5)            R5 TO LAST BYTE OF CODE                     
         CLI   0(R5),C' '           AND FIND END OF ACCOUNT                     
         BNE   *+12                                                             
         AH    R0,=H'1'            MORE ROOM FOR THE NAME                       
         BCT   R5,*-12                                                          
*                                                                               
         GOTO1 CHOPPER,DMCB,(36,TOTNAME),((R0),2(R5)),(C'P',2)                  
         MVI   FRCETOT,C'N'                                                     
         BR    R3                                                               
*                                                                               
REQL30   LA    R2,REQ                                                           
         BAS   RE,PRTTOTS                                                       
         BAS   RE,PRTBSUM           PRINT THE REQUEST BILLNG SUMMARY            
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*              POST TO SUMMARY BUFFALO RECORDS                                  
SUMPOST  NTR1  ,                                                                
         LA    R7,SRCTOT                                                        
         LA    R0,4                                                             
         LA    R1,TOTAMTS                                                       
         CP    0(8,R1),=P'0'       DON'T POST IF ALL ZERO                       
         BNE   *+16                                                             
         LA    1,8(R1)                                                          
         BCT   R0,*-14                                                          
         B     EXIT                                                             
*                                                                               
         XC    BUFREC,BUFREC                                                    
         MVI   BUFACC,X'FF'        SOURCE SUMMARY RECORDS                       
         MVC   BUFSRC,SORTSBAC+3                                                
         MVC   BUFBUCK(BUFBLNQ),TOTAMTS SOURCE TOTALS                           
         GOTO1 ,DMCB,=C'PUT',VBUFF,BUFREC                                       
         L     RF,BUFFALO                                                       
         TM    SUMOPT,RQSRC        MEDIA SUMMARY AT REQLAST                     
         BNO   *+6                                                              
         BASR  RE,RF                                                            
*                                                                               
         TM    SUMOPT,RQAC+ACSRC   SUMMARIES BY ACCOUNT                         
         BZ    SUM12                                                            
         LA    R7,LVATOT           POST BY ACOUUNT                              
         LA    R0,4                                                             
*                                                                               
SUM05    TM    TOTSTAT,LVACT       NO ACCOUNTS AT THIS LEVEL                    
         BZ    SUM09                                                            
         CLC   TOTNUM,SUMLEV                                                    
         BH    SUM09               SKIP THE MORE DETAIL LEVELS                  
         XC    BUFSRC,BUFSRC                                                    
         MVC   BUFACC,TOTACC       ACCOUNT CODE                                 
         MVC   BUFNAME,TOTNAME     ACCOUNT NAME                                 
         MVC   BUFNUM,TOTNUM       RELATIVE ACCUM NUMBER IN TOTALS              
         SR    R2,R2                                                            
         IC    R2,TOTLEN                                                        
         LA    R2,BUFACC(R2)       R1 TO END OF LEVEL+1                         
         CLI   TOTLEN,12                                                        
         BE    *+8                                                              
         MVI   0(R2),X'FF'         FORCE HIGH LEVELS TO SORT LAST               
         BASR  RE,RF               ACCOUNT TOTAL TO BUFFALO(HEADER)             
*                                                                               
         TM    SUMOPT,RQACSRC+ACSRC DETAIL(SOURCE SUMMARIES)                    
         BZ    SUM09                                                            
         MVI   BUFSRC,X'FF'        ACCOUNT TOTAL(TRAILER)                       
         BASR  RE,RF                                                            
*                                                                               
         MVC   BUFSRC,SORTSBAC+3   SOURCE TOTAL                                 
         BASR  RE,RF               AGAIN TO BUFFALO                             
*                                                                               
SUM09    LA    R7,TOTLNQ(R7)       NEXT LEVEL                                   
         BCT   R0,SUM05                                                         
SUM12    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*              PRINT BILLING SOURCE SUMMARY                                     
*              R2 CONTAINS LEVEL NUMBER                                         
PRTBSUM  NTR1  ,                                                                
         LR    R7,R2               LEVEL NUMBER                                 
         MH    R7,=Y(TOTLNQ)       X LENGTH OF LEVEL                            
         LA    R7,TOTALS(R7)                                                    
         XC    SAVBACC,SAVBACC     SET KEY FOR REQUEST TOTALS                   
         MVI   SAVBACC,X'FF'                                                    
         CLI   TOTNUM,REQ                                                       
         BE    PRTBSUM1                                                         
*                                                                               
         MVC   SAVBACC,TOTACC      SAVE THE ACCOUNT KEY                         
         SR    R1,R1                                                            
         IC    R1,TOTLEN           LENGTH OF THIS LEVEL                         
         LA    R1,SAVBACC(R1)      R1 TO END OF ACCOUNT                         
         CLI   TOTLEN,12                                                        
         BE    *+8                                                              
         MVI   0(R1),X'FF'         MUST MATCH BUFFALO RECORD                    
*                                                                               
PRTBSUM1 MVI   RCSUBPRG,1          SET SUB-PROGRAM                              
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   SORTPART,0                                                       
*                                                                               
         XC    BUFREC,BUFREC       ADD ENTRY TO BUFF FOR THIS ACCOUNT           
         MVC   BUFACC,SAVBACC                                                   
         GOTO1 BUFFALO,DMCB,=C'HIGH',VBUFF,BUFREC,0                             
*                                                                               
PRTBSUM2 CLI   DMCB+8,0            EOF                                          
         BNE   PRTBSUM6                                                         
         CLC   SAVBACC,BUFACC      OR RECORD NOT FOUND                          
         BNE   PRTBSUM6                                                         
         CLI   BUFSRC,0                                                         
         BE    PRTBSUM4            SKIP ACCOUNT HEADERS                         
         CLI   BUFSRC,X'FF'                                                     
         BE    PRTBSUM4            AND TRAILERS                                 
         MVC   FORMACCS(32),BUFBUCK       FORMAT A LINE                         
         BAS   RE,FORMAT                                                        
         MVC   P+1(12),BUFSRC      SOURCE                                       
         BAS   RE,PRNT                                                          
*                                                                               
PRTBSUM4 GOTO1 BUFFALO,DMCB,=C'SEQ',VBUFF,BUFREC,0                              
         B     PRTBSUM2                                                         
*                                                                               
PRTBSUM6 BAS   RE,PRTTOTS                                                       
         B     EXIT                                                             
         EJECT                                                                  
*              PRINT TOTAL LINE FOR ANY LEVEL                                   
*                                                                               
PRTTOTS  NTR1  ,                                                                
         LR    R7,R2               R2 HAS LEVEL NUMBER                          
         MH    R7,=Y(TOTLNQ)                                                    
         LA    R7,TOTALS(R7)       R7 HAS TOTAL ENTRY FOR THIS LEVEL            
         CLI   TOTNUM,SRC                                                       
         BNE   PRTTOT2                                                          
         CLI   BILLSW,C'Y'                                                      
         BE    PRTTOT2             SPECIAL SORT IS OK                           
         CLI   SORTSW,C'N'         DON'T DO SOURCE TOTALS IF SORTING            
         BNE   EXIT                                                             
PRTTOT2  CLI   MYMODE,ACCLAST                                                   
         BNE   PRTTOT4                                                          
         CLI   BILLSW,C'Y'                                                      
         BE    PRTTOT4                                                          
         CLI   SORTSW,C'N'                                                      
         BE    PRTTOT4                                                          
         BAS   RE,SORTPRT                                                       
PRTTOT4  DS    0H                                                               
         MVC   FORMACCS(32),TOTAMTS                                             
         ZAP   FORMACCS+32(8),=P'0'                                             
         MVI   SORTPART,0                                                       
         BAS   RE,FORMAT                                                        
         CLI   TOTNUM,LVD          ALWAYS PRINT ACCOUNT TOTALS                  
         BE    *+12                                                             
         CLI   FORMSW,C'Y'                                                      
         BNE   PRTTOTX                                                          
         CLI   RCSUBPRG,0                                                       
         BE    *+12                                                             
         TM    TOTSTAT,LVNME       PRINT THE LEVEL NAME                         
         BO    PRTTOT6                                                          
         MVC   P+16(15),TOTCLASS                                                
         B     PRTTOT9                                                          
*                                                                               
PRTTOT6  MVC   P+2(10),=C'TOTALS FOR'                                           
         LA    R5,P+13                                                          
         LA    R0,15                                                            
         MVC   0(12,R5),TOTACC      ACCOUNT CODE                                
         LA    R5,11(R5)            R5 TO LAST BYTE OF CODE                     
         CLI   0(R5),C' '           AND FIND END OF ACCOUNT                     
         BNE   *+12                                                             
         AH    R0,=H'1'            MORE ROOM FOR THE NAME                       
         BCT   R5,*-12                                                          
         GOTO1 CHOPPER,DMCB,(36,TOTNAME),((R0),2(R5)),(C'P',2)                  
*                                                                               
PRTTOT9  LA    R2,P+96                                                          
         CLI   AGEBY,C'N'                                                       
         BNE   *+8                                                              
         LA    R2,P+41                                                          
         CLC   0(14,R2),SPACES                                                  
         BNE   *+10                                                             
         MVC   8(3,R2),=C'NIL'                                                  
         MVC   PTHIRD,PSECOND                                                   
         MVC   PSECOND,P                                                        
         MVC   P,SPACES                                                         
         MVI   P,0                 FORCE BLANK LINE BEFORE TOTALS               
         BAS   RE,PRNT                                                          
PRTTOTX  MVI   TOTSW,C'Y'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*              BUILD A LIST OF DATES                                            
*                                                                               
BUILDATE NTR1  ,                                                                
         CLC   QEND,SPACES                                                      
         BE    BUILD1                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,WORK)                                    
         SPACE 1                                                                
BUILD1   MVC   WORK(6),QEND                                                     
         CLC   WORK(6),SPACES      END DATE SPECIFIED                           
         BNE   BUILD4                                                           
*                                  NO - TODAY IS END DATE                       
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK)                                  
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVC   MOAEND,ACMMEND                                                   
         CLI   ACMMEND,X'FF'               NO MOA END  DATE                     
         BE    BUILD4                                                           
         MVC   WORK+6(2),ACMMEND           GET LAST DAY OF THE MONTH            
         MVI   WORK+8,X'28'                                                     
         GOTO1 DATCON,DMCB,(1,WORK+6),(0,WORK)                                  
BUILD2   GOTO1 ADDAY,DMCB,WORK,WORK+6,F'1'                                      
         CLC   WORK+8(2),WORK+2         IS IT SAME MONTH                        
         BNE   BUILD4                IF NOT WORK HAS LAST DAY OF MONTH          
         MVC   WORK(6),WORK+6                                                   
         B     BUILD2                                                           
BUILD4   LA    R2,DATAB            SET INCREMENTAL VALUE                        
         LA    R3,4                                                             
         LA    R4,30               ASSUME 30 IF NOT SPECIFIED                   
*                                                                               
         CLC   QSRTAREA(2),SPACES                                               
         BE    *+14                                                             
         PACK  DUB,QSRTAREA(2)                                                  
*                                                                               
         CVB   R4,DUB                                                           
         LNR   R4,R4                                                            
         AH    R4,=H'1'                                                         
         SR    R5,R5                                                            
BUILD6   GOTO1 DATCON,DMCB,(0,WORK),(1,0(R2))                                   
         CLI   QOPT2,C'M'          IF DOING MONTHS SET START DATE TO            
         BNE   BUILD8              FIRST-OF-MONTH                               
         MVC   WORK+6(6),WORK                                                   
         MVC   WORK+10(2),=C'01'                                                
         B     BUILD10                                                          
BUILD8   GOTO1 ADDAY,DMCB,WORK,WORK+6,(R4)                                      
BUILD10  GOTO1 DATCON,DMCB,(0,WORK+6),(1,3(R2))                                 
         STH   R5,8(R2)            SET INCREMENT VALUE                          
         STC   R3,7(R2)            AND ELEMENT NUMBER                           
         MVC   DMCB+8(4),=F'-1'                                                 
         GOTO1 ADDAY,DMCB,WORK+6,WORK                                           
         SR    R5,R4               BUMP INCREMENT                               
         AH    R5,=H'1'                                                         
         LA    R2,10(R2)                                                        
         BCT   R3,BUILD6                                                        
         B     EXIT                                                             
         EJECT                                                                  
*              FORMAT A LINE OF ACCUMULATORS                                    
*                                                                               
FORMAT   NTR1  ,                                                                
         ZAP   FORMACCS+32(8),=P'0'     CLEAR OUT TOTAL                         
         LA    R2,FORMACCS                                                      
         LA    R3,4                     LOOP 4 TIMES                            
         AP    FORMACCS+32(8),0(8,R2)   TOTAL UP ACCUMALATORS                   
         LA    R2,8(R2)                 BUMP TO NEXT ACCUM                      
         BCT   R3,*-10                                                          
         SPACE 1                                                                
         LA    R2,FORMACCS                                                      
         LA    R3,P+41                                                          
         LA    R4,5                     PRINT FOUR AMOUNT PLUS TOTAL            
         CLI   AGEBY,C'N'               NO AGEING ON REPORT                     
         BNE   FORMAT1                                                          
         LA    R3,P+41                                                          
         LA    R2,FORMACCS              IF NO AGEING ONLY TOTAL COLUMN          
         LA    R4,1                     ONLY ONE AMOUNT OT PRINT                
         CLI   QOPT2,C'C'               NO AGEING JUST INCOME                   
         BNE   FORMAT1                                                          
         LA    R4,2                     SHOW INCOME ONLY, TWO AMOUNTS           
FORMAT1  MVI   FORMSW,C'N'                                                      
FORMAT2  CP    0(8,R2),=P'0'            IS ACCUM ZERO                           
         BE    FORMAT4                  YES                                     
         MVI   FORMSW,C'Y'              NO SO TURN ON FORM SWITCH               
         EDIT  (P8,0(R2)),(12,0(R3)),2,MINUS=YES      PRINT ACCUM               
         CLI   SORTPART,0               ?                                       
         BE    FORMAT4                                                          
         MVI   12(R3),C'*'              PUT STAR NEXT TO AMOUNT                 
FORMAT4  LA    R2,8(R2)                 BUMP TO NEXT ACCUM                      
         LA    R3,14(R3)                BUMP UP IN PRINT LINE                   
         BCT   R4,FORMAT2                                                       
         B     EXIT                                                             
         EJECT                                                                  
*              FORMAT DATE HEADLINES                                            
*                                                                               
FORMDATE NTR1  ,                                                                
         MVC   DATLIN1,SPACES                                                   
         MVC   DATLIN2,SPACES                                                   
         LA    R2,DATAB                                                         
         LA    R3,DATLIN1+1                                                     
         LA    R4,4                                                             
         MVC   WORK(1),QOPT2                                                    
         CLI   WORK,C' '                                                        
         BNE   *+8                                                              
         MVI   WORK,C'2'                                                        
         CLI   WORK,C'M'           MONTH HEADINGS                               
         BNE   FORMD4                                                           
FORMD2   GOTO1 ,DMCB,(1,WORK),(6,WORK+3)                                        
         L     RF,DATCON                                                        
FORMD2A  MVC   WORK(3),0(R2)                                                    
         BASR  RE,RF                                                            
         MVC   5(6,R3),WORK+3                                                   
         MVI   8(R3),C'/'                                                       
         MVC   L'DATLIN1+5(6,R3),DASHES                                         
         CLI   7(R2),1                                                          
         BNE   *+10                                                             
         MVC   L'DATLIN1+3(9,R3),=C'AND PRIOR'                                  
         LA    R2,10(R2)                                                        
         LA    R3,14(R3)                                                        
         BCT   R4,FORMD2A                                                       
         XC    DATAB+33(3),DATAB+33                                             
         B     FORMD9                                                           
*                                  INCREMENT HEADINGS                           
FORMD4   CLI   WORK,C'I'                                                        
         BNE   FORMD6                                                           
FORMD4A  EDIT  (B2,8(R2)),(3,3(R3))                                             
         MVC   6(4,R3),=C'DAYS'                                                 
         MVC   L'DATLIN1+3(7,R3),DASHES                                         
         CLI   7(R2),4                                                          
         BNE   *+10                                                             
         MVC   3(7,R3),=C'CURRENT'                                              
         CLI   3(R3),C' '                                                       
         BNE   *+8                                                              
         MVI   L'DATLIN1+3(R3),C' '                                             
         CLI   7(R2),1                                                          
         BNE   *+10                                                             
         MVC   L'DATLIN1+3(8,R3),=C'AND OVER'                                   
         LA    R2,10(R2)                                                        
         LA    R3,14(R3)                                                        
         BCT   R4,FORMD4A                                                       
         XC    DATAB+33(3),DATAB+33                                             
         B     FORMD9                                                           
*                                  UK DATE HEADINGS                             
FORMD6   CLI   WORK,C'1'                                                        
         BNE   FORMD8                                                           
         GOTO1 ,DMCB,(1,WORK),(8,WORK+3)                                        
         L     RF,DATCON                                                        
FORMD6A  MVC   WORK(3),3(R2)                                                    
         BASR  RE,RF                                                            
         MVC   2(7,R3),WORK+3                                                   
         MVI   10(R3),C'-'                                                      
         MVC   WORK(3),0(R2)                                                    
         BASR  RE,RF                                                            
         MVC   L'DATLIN1+2(7,R3),WORK+3                                         
         CLI   7(R2),1                                                          
         BNE   *+16                                                             
         MVC   2(9,R3),L'DATLIN1+2(R3)                                          
         MVC   L'DATLIN1+1(9,R3),=C'AND PRIOR'                                  
         LA    R2,10(R2)                                                        
         LA    R3,14(R3)                                                        
         BCT   R4,FORMD6A                                                       
         XC    DATAB+33(3),DATAB+33                                             
         B     FORMD9                                                           
*                                  US DATE HEADINGS                             
FORMD8   CLI   WORK,C'2'                                                        
         BNE   FORMD9                                                           
         GOTO1 ,DMCB,(1,WORK),(8,WORK+3)                                        
         L     RF,DATCON                                                        
FORMD8A  MVC   WORK(3),0(R2)                                                    
         BASR  RE,RF                                                            
         MVC   2(8,R3),WORK+3                                                   
         MVC   WORK(3),3(R2)                                                    
         BASR  RE,RF                                                            
         MVC   L'DATLIN1+2(8,R3),WORK+3                                         
         CLI   7(R2),4                                                          
         BNE   FORMD8B                                                          
         CLC   QEND,SPACES                                                      
         BNE   FORMD8B                                                          
         CLI   MOAEND,X'FF'                                                     
         BNE   FORMD8B                                                          
         MVC   2(8,R3),L'DATLIN1+2(R3)                                          
         MVC   L'DATLIN1+1(9,R3),=C'AND AFTER'                                  
         B     FORMD8C                                                          
FORMD8B  CLI   7(R2),1                                                          
         BE    *+14                                                             
         MVC   7(3,R3),=C' TO'                                                  
         B     FORMD8C                                                          
         MVC   L'DATLIN1+1(9,R3),=C'AND PRIOR'                                  
FORMD8C  LA    R2,10(R2)                                                        
         LA    R3,14(R3)                                                        
         BCT   R4,FORMD8A                                                       
         XC    DATAB+33(3),DATAB+33                                             
         CLC   QEND,SPACES                                                      
         BNE   *+10                                                             
         MVC   DATAB(3),=3X'FF'                                                 
         SPACE 1                                                                
FORMD9   CLI   AGEBY,C'N'                                                       
         BNE   FORMDX                                                           
         MVC   DATLIN1,SPACES      NO AGEING FIX HEADLINES                      
         MVC   DATLIN2,SPACES                                                   
         MVC   DATLIN1+5(6),=C'AMOUNT'                                          
         MVC   DATLIN2+5(6),=C'------'                                          
         CLI   QOPT2,C'C'                                                       
         BNE   FORMDX                                                           
         MVC   DATLIN1+19(6),=C'INCOME'                                         
         MVC   DATLIN2+19(6),=C'------'                                         
FORMDX   B     EXIT                                                             
         EJECT                                                                  
*              PRINT LINE FOR A BILL                                            
*                                                                               
BILLEND  NTR1  ,                                                                
         OC    SORTREF,SORTREF                                                  
         BZ    EXIT                                                             
         CLI   MODE,ACCLAST                                                     
         BE    *+14           IGNORE BILL TOTAL IF COMING FROM SORT             
         CP    BILLTOT,=P'0'                                                    
         BE    BILLNDX                                                          
         CLI   DETSW,C'A'          IF SHOWING ALL DETAILS                       
         BNE   BILLND6                                                          
         L     R6,ADRTRNS         R6=BEGINNING OF TRANSACTION TABLE             
         SPACE 1                                                                
BILLND3  CLI   0(R6),X'FF'                                                      
         BE    BILLNDX            END OF TABLE                                  
         MVC   SORTREC(SORTLEN),0(R6)                                           
         ZAP   THISAMNT,SORTAMTS(8)                                             
         ZAP   SORTAMTS(8),=P'0'                                                
BILLND6  CLI   BILLSW,C'Y'                                                      
         BE    BILLND21           BILL IS FROM SORT/ALREADY AGED                
         ZAP   FORMACCS(8),=P'0'                                                
         MVC   FORMACCS+8(32),FORMACCS                                          
         ST    R4,SAVER4                                                        
         LA    R4,FORMACCS         POST AMOUNT INTO BUCKET                      
         CLI   AGEBY,C'N'                                                       
         BNE   BILLND9                                                          
         AP    0(8,R4),THISAMNT                                                 
         CLI   QOPT2,C'C'                                                       
         BNE   BILLND21                                                         
         AP    8(8,R4),SORTINCM                                                 
         B     BILLND21                                                         
BILLND9  LA    R2,DATAB                                                         
         LA    R3,4                                                             
         MVC   WORK(3),SORTDATE                                                 
         CLI   AGEBY,C'D'                                                       
         BNE   BILLND12                                                         
         OC    SORTDUE,SORTDUE                                                  
         BZ    BILLND12                                                         
         GOTO1 DATCON,DMCB,(2,SORTDUE),(1,WORK)                                 
BILLND12 DS    0H                                                               
         CLI   AGEBY,C'M'                                                       
         BNE   BILLND15                                                         
         MVC   WORK+2(2),SORTMOS                                                
         MVI   WORK+2,X'01'                                                     
BILLND15 CLC   WORK(3),0(R2)                                                    
         BH    BILLNDX                                                          
         CLC   WORK(3),3(R2)                                                    
         BNL   BILLND18                                                         
         LA    R4,8(R4)                                                         
         LA    R2,10(R2)                                                        
         BCT   R3,BILLND15                                                      
         B     BILLNDX                                                          
BILLND18 AP    0(8,R4),THISAMNT                                                 
         MVC   SORTAMTS(32),FORMACCS   ACCUMS TO SORT RECORD                    
         CLI   SORTPART,0          IGNORE PARTIAL PAYMENTS                      
         BNE   BILLND21                                                         
*                                                                               
         CLI   QOPT6,C' '          IGNORE C/D                                   
         BE    BILLND21                                                         
         CLI   QOPT6,C'Y'          ADD C/D TO ALL COLUMNS                       
         BE    *+12                                                             
         CLI   7(R2),4             ADD C/D TO ALL BUT CURRENT COLUMN            
         BE    BILLND21                                                         
         AP    0(8,R4),SORTCASH                                                 
*                                                                               
BILLND21 BAS   RE,FORMAT                                                        
         MVI   THSACC,C'Y'                                                      
         MVI   THISBAC,C'Y'                                                     
         MVC   P+1(12),SORTSBAC+3                                               
         MVC   P+16(6),SORTREF                                                  
         GOTO1 DATCON,DMCB,(1,SORTDATE),(8,P+23)                                
*                                                                               
         CLI   PROGPROF+14,C'A'    ADVERTISING YEAR                             
         BNE   BILLND24                                                         
         CLC   SORTDTE2,SPACES                                                  
         BE    BILLND24                                                         
         MVI   DUB,X'19'                                                        
         MVC   DUB+1(1),SORTDTE2                                                
         MVI   DUB+2,X'0F'                                                      
         UNPK  WORK(5),DUB(3)                                                   
         MVC   P+34(4),WORK                                                     
BILLND24 CLI   PROGPROF+14,C'D'    ALWAYS PRINT DUE DATE                        
         BE    *+12                                                             
         CLI   AGEBY,C'D'          AGE BY DUE DATE                              
         BNE   BILLND27                                                         
         LA    R3,P+32                                                          
         CLI   PROGPROF+14,C'A'    IF ADV YEAR AND DUE DATE                     
         BNE   *+8                                                              
         LA    R3,P+23             CAN'T PRINT BILL DATE                        
         MVC   0(8,R3),SPACES                                                   
         OC    SORTDUE,SORTDUE     PRINT THE DUE DATE                           
         BZ    BILLND27                                                         
         GOTO1 DATCON,DMCB,(2,SORTDUE),(8,0(R3))                                
*                                                                               
BILLND27 DS    0H                                                               
         LA    R1,PSECOND                                                       
         CLI   DETSW,C'Y'               DO WE WANT DETAIL                       
         BE    *+12                                                             
         CLI   DETSW,C'A'               SHOW ALL DETAIL (1 LINE/TRANS.)         
         BNE   BILLND45                                                         
         CLI   TNRSW,C'Y'               SEE IF T&R JOB                          
         BNE   BILLND28                 NO                                      
         OC    SORTEST,SORTEST          YES,CHECK TO SEE IF THERE IS ON         
         BZ    BILLND39                                                         
         MVC   1(4,R1),=C'EST='                                                 
         MVC   5(15,R1),SORTEST                                                 
         B     BILLND45                                                         
BILLND28 EQU   *                                                                
         OC    SORTCLI(12),SORTCLI                                              
         BZ    BILLND39            NO CLI/PRD/COMM                              
         OC    SORTCLI(3),SORTCLI                                               
         BZ    BILLND30             NO CLIENT                                   
         MVC   1(4,R1),=C'CLI='                                                 
         MVC   5(3,R1),SORTCLI                                                  
         LA    R1,7(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         B     *-10                                                             
         LA    R1,1(R1)                                                         
         MVI   0(R1),C','                                                       
BILLND30 OC    SORTPROD,SORTPROD                                                
         BZ    BILLND33                                                         
         MVC   1(5,R1),=C'PROD='                                                
         MVC   6(3,R1),SORTPROD                                                 
         LA    R1,8(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         B     *-10                                                             
         LA    R1,1(R1)                                                         
         MVI   0(R1),C','                                                       
BILLND33 OC    SORTCOMN,SORTCOMN                                                
         BZ    BILLND36                                                         
         MVC   1(5,R1),=C'COMN='                                                
         MVC   6(6,R1),SORTCOMN                                                 
         B     BILLND45                                                         
BILLND36 CLI   0(R1),C','          REMOVE TRAILING COMMAS                       
         BNE   BILLND45                                                         
         MVI   0(R1),C' '                                                       
         B     BILLND45                                                         
         SPACE 1                                                                
BILLND39 OC    SORTNUM,SORTNUM                                                  
         BZ    BILLND45                                                         
         LA    R1,PROFNAME                                                      
         SR    RE,RE                                                            
BILLND42 CLI   0(R1),0             FORMAT SOURCE DETAIL LINE                    
         BE    BILLND45                                                         
         CLC   SORTPROF,1(R1)                                                   
         BE    *+14                                                             
         IC    RE,0(R1)                                                         
         AR    R1,RE                                                            
         B     BILLND42                                                         
         IC    RE,0(R1)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PSECOND+1(0),2(R1)                                               
         LA    R1,PSECOND+2(RE)                                                 
         MVC   0(9,R1),SORTNUM                                                  
         CLC   SORTDTE2,SPACES     MEDIA MONTH OF SERVICE                       
         BE    BILLND45                                                         
         LA    R1,8(R1)                                                         
         CLI   0(R1),C' '          SHUFFLE BACK TO FIRST BLANK                  
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         B     *-10                                                             
         MVI   1(R1),C','                                                       
         LA    R3,2(R1)                                                         
         MVC   DUB(2),SORTDTE2     MOVE IN DATE                                 
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(1,DUB),(6,0(R3))                                    
         OC    SORTNET,SORTNET     TAG ON NETWORK NAME IF WE HAVE               
         BZ    BILLND45                                                         
         LA    R3,6(R3)                                                         
         MVI   0(R3),C','                                                       
         MVC   1(4,R3),SORTNET                                                  
         SPACE 1                                                                
BILLND45 CLI   TOTSW,C'Y'          JUST PRINTED TOTALS                          
         BNE   BILLND48                                                         
         CLI   FORCEHED,C'Y'                                                    
         BE    BILLND48                                                         
         MVC   PTHIRD,PSECOND      SHUFFLE DOWN PRINT LINES                     
         MVC   PSECOND,P                                                        
         MVC   P,SPACES                                                         
         MVI   P,0                                                              
BILLND48 MVI   TOTSW,C'N'          RESET TOAL INDICATOR                         
         TM    SUMOPT,SUMONLY      SUMMARY MODE                                 
         BO    BILLND54                                                         
         CLI   BILLSW,C'Y'                                                      
         BE    BILLND51                                                         
         CLI   SORTSW,C'N'                                                      
         BE    BILLND51                                                         
         L     R4,SAVER4                                                        
         BAS   RE,SORTPUT                                                       
         B     BILLND54                                                         
BILLND51 BAS   RE,PRNT                                                          
BILLND54 MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         BAS   RE,POSTBILL         POST BILL TO ACCUMS                          
         CLI   DETSW,C'A'                                                       
         BNE   BILLNDX                                                          
         LA    R6,SORTLEN(R6)      BUMP TO NEXT IN TABLE                        
         B     BILLND3             AND RETURN                                   
BILLNDX  CLI   BILLSW,C'Y'         IF INPUT FROM SORT                           
         BE    EXIT                DON'T CLEAR NEXT NUMBER                      
         XC    SORTREF,SORTREF                                                  
         XC    SORTDUE,SORTDUE                                                  
         B     EXIT                                                             
         EJECT                                                                  
*              POST BILL VALUES TO ACCUMULATORS                                 
*                                                                               
POSTBILL NTR1  ,                   POST INTO TOTAL LINES                        
         LA    R7,TOTALS           POINT TO TOTAL ACCUMS                        
         LA    R3,6                NUMBER OF LOOPS                              
         CLI   BILLSW,C'Y'         IF BILL IS FROM SORT                         
         BNE   *+8                                                              
         LA    R3,1                ONLY ADD TO SOURCE TOTALS                    
*                                                                               
POSTB8   TM    TOTSTAT,LVACT       IS THIS LEVEL IN USE                         
         BNO   POSTB11             IF NOT DON'T ADD IN ACCUMS                   
         TM    TOTSTAT,ACACT       IS IT ALREADY ACTIVE                         
         BO    *+10                IF IT IS, OK TO ADD TO IT                    
         MVC   TOTAMTS(32),=4PL8'0' FIRST TIME CLEAR ACCUMS                     
         OI    TOTSTAT,ACACT       TURN ON ACCOUNT ACTIVITY                     
         LA    R1,FORMACCS                                                      
         LA    R2,TOTAMTS                                                       
         LA    R4,4                LOOP FOUR TIMES                              
*                                                                               
POSTB10  AP    0(8,R2),0(8,R1)     ADD FORMACCS TO TOTALS                       
         LA    R2,8(R2)            BUMP TO NEXT ACCUM IN TOTALS                 
         LA    R1,8(R1)            BUMP TO NEXT ACCUM IN FORACCS                
         BCT   R4,POSTB10                                                       
*                                                                               
POSTB11  LA    R7,TOTLNQ(R7)                                                    
         BCT   R3,POSTB8                5 OR 1 SET OF TOTAL ACCUMS              
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              PRINT A LINE                                                     
*                                                                               
PRNT     NTR1  ,                                                                
         MVC   HEAD4+42(36),COMPNAME                                            
         MVC   HEAD5+42(26),COMPADDR                                            
         MVC   HEAD6+42(26),COMPADDR+26                                         
         MVC   HEAD7+42(26),COMPADDR+52                                         
         MVC   HEAD8+42(26),COMPADDR+78                                         
         MVC   HEAD7+89(22),REQNAME                                             
         MVC   HEAD10+40(L'DATLIN1),DATLIN1                                     
         MVC   HEAD11+40(L'DATLIN2),DATLIN2                                     
         CLI   AGEBY,C'N'          AGEING ?                                     
         BE    PRNT10                                                           
         MVC   HEAD10+40(L'DATLIN1),DATLIN1                                     
         MVC   HEAD11+40(L'DATLIN2),DATLIN2                                     
         MVC   HEAD10+102(5),=C'TOTAL'                                          
         MVC   HEAD11+102(5),=C'-----'                                          
         MVC   HEAD6+89(20),=CL20'AGED BY DUE DATE'                             
         CLI   AGEBY,C'D'                                                       
         BE    PRNT10                                                           
         MVC   HEAD6+89(20),=CL20'AGED BY BILL DATE'                            
         CLI   AGEBY,C'B'                                                       
         BE    PRNT10                                                           
         MVC   HEAD6+89(21),=CL21'AGED BY MO OF SERVICE'                        
*                                                                               
PRNT10   CLI   RCSUBPRG,0          RECEIVABLE STATEMENT                         
         BNE   PRNT20                                                           
         MVC   HEAD10+25(4),=C'BILL' DEFAULT IS BILL DATE                       
         MVC   HEAD11+25(4),=C'DATE'                                            
         LA    R1,HEAD10+34        WHERE DO WE PUT THE DUE DATE                 
         CLI   PROGPROF+14,C'A'    IF USING ADV YEAR                            
         BNE   *+20                                                             
         MVC   HEAD10+34(3),=C'ADV'                                             
         MVC   HEAD11+34(4),=C'YEAR'                                            
         LA    R1,HEAD10+25        THEN DATE OVERRIDES BILL NUMBER              
         CLI   AGEBY,C'D'          USING DUE DATE NOT BILL DATE                 
         BE    *+12                                                             
         CLI   PROGPROF+14,C'D'                                                 
         BNE   *+16                                                             
         MVC   0(3,R1),=C'DUE '                                                 
         MVC   132(4,R1),=C'DATE'  CAN'T SHOW BILL DATE                         
         CLI   PROGPROF+12,C'Y'    SKIP AS AT DATE                              
         BE    PRNT20                                                           
         MVC   HEAD2+63(5),=C'AS AT'                                            
         MVC   HEAD2+69(8),ASATDATE                                             
         MVC   HEAD3+62(15),=15C'-'                                             
*                                                                               
PRNT20   CLI   RCSUBPRG,1                                                       
         BH    PRNT40                                                           
         CLI   MYMODE,REQLAST                                                   
         BE    PRNT40                                                           
         SR    R7,R7                                                            
         IC    R7,THISLEV                                                       
         MH    R7,=Y(TOTLNQ)                                                    
         LA    R7,TOTALS(R7)                                                    
         MVC   HEAD5+99(12),TOTACC                                              
         MVC   HEAD5+89(7),=C'ACCOUNT'                                          
         CLI   PROGPROF+7,C'Y'     OPTION TO SHOW OTHER NAMES IN HEADS          
         BE    PRNT22                                                           
         MVC   HEAD4+1(36),TOTNAME ONLY PRINT THIS LEVEL NAME                   
         B     PRNT40                                                           
*                                                                               
PRNT22   SR    R0,R0                                                            
         IC    R0,THISLEV                                                       
         LA    R7,LVATOT                                                        
         LA    R2,HEAD2+1                                                       
PRNT23   TM    TOTSTAT,LVACT       IS THIS LEVEL ACTIVE                         
         BNO   PRNT25                                                           
         MVC   0(36,R2),TOTNAME    NAME TO HEADS                                
         LA    R2,132(R2)          R2 TO NEXT HEADS                             
PRNT25   AH    R7,=Y(TOTLNQ)       R7 TO NEXT ACCOUNT LEVEL                     
         BCT   R0,PRNT23                                                        
*                                                                               
PRNT40   GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
*              PUT TO SORTER                                                    
*                                                                               
SORTPUT  NTR1  ,                                                                
         CLI   SORTSW,C'Y'         DO WE NEED TO INITIALIZE                     
         BNE   SORTPUT2                                                         
         MVI   SORTSW,C'I'         YES - SET INITIALIZED STATUS                 
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD                                    
*                                                                               
SORTPUT2 DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
         B     EXIT                                                             
         EJECT                                                                  
*              READ AND PRINT SORTED RECORDS                                    
*                                                                               
SORTPRT  NTR1  ,                                                                
         CLI   SORTSW,C'Y'         IS SORTER ACTIVE                             
         BE    EXIT                NO                                           
         LA    R7,SRCTOT                                                        
         MVC   TOTAMTS(32),=4PL8'0'    CLEAR OUT BILLING SOURCE                 
         XC    SAVESBAC,SAVESBAC                                                
SORTPR2  DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     R5,4(R1)                                                         
         LTR   R5,R5                                                            
         BNZ   SORTPR3                                                          
         CLI   TALSW,C'Y'                                                       
         BE    SORTPR4             FOR TALENT PRINT LAST BILL                   
         B     SORTPRX                                                          
         SPACE 1                                                                
SORTPR3  MVC   SORTREC(SORTLEN),0(R5)                                           
         ZAP   FORMACCS(8),=P'0'                                                
         MVC   FORMACCS+8(32),FORMACCS                                          
         MVC   FORMACCS(32),SORTAMTS                                            
         CLI   TALSW,C'Y'                                                       
         BNE   SORTPR8                                                          
         SPACE 1                                                                
*              TREAT CHANGE OF CONTRA AS IF IT CAME FROM MONACC                 
         MVI   BILLSW,C'Y'         MAKE BELIEVE NO SORT(FOR PRINTING)           
         OC    SAVESBAC,SAVESBAC                                                
         BZ    SORTPR5             FIRST TIME                                   
         MVI   MYMODE,PROCTRNS                                                  
         CLC   SAVESBAC,SORTSBAC   IF SAME SUB/ACC TREAT AS TRANSACTION         
         BE    SORTPR6                                                          
SORTPR4  MVI   MYMODE,SBACLAST      WHEN CONTRA(CLIENT) CHANGES                 
         BAS   RE,SUBACC            GET CLIENT TOTAL                            
SORTPR5  MVC   SAVESBAC,SORTSBAC    SAVE SUB/ACCOUNT                            
         CLI   TNRSW,C'Y'           DO NOT WANT TO PAGE FOR TNR                 
         BE    *+8                  IF TNRSW=Y THEN DON'T PAGE BREAK            
         MVI   FORCEHED,C'Y'        NEW PAGE PER CLIENT                         
         ZAP   FORMACCS(8),=P'0'    RESET BILL TOTALS                           
         MVC   FORMACCS+8(32),FORMACCS                                          
         MVC   FORMACCS(32),SORTAMTS                                            
         LTR   R5,R5                                                            
         BZ    SORTPRX             END OF SORT                                  
SORTPR6  BAS   RE,BILLEND                                                       
         MVC   MYMODE,MODE                                                      
         B     SORTPR2                                                          
         SPACE 1                                                                
SORTPR8  BAS   RE,FORMAT                                                        
         MVC   P+1(12),SORTSBAC+3                                               
         MVC   P+16(6),SORTREF                                                  
         GOTO1 DATCON,DMCB,(1,SORTDATE),(8,P+23)                                
*                                                                               
         CLI   PROGPROF+14,C'A'    ADVERTISING YEAR                             
         BNE   SORTPR10                                                         
         CLC   SORTDTE2,SPACES                                                  
         BE    SORTPR10                                                         
         MVI   DUB,X'19'                                                        
         MVC   DUB+1(1),SORTDTE2                                                
         MVI   DUB+2,X'0F'                                                      
         UNPK  WORK(5),DUB(3)                                                   
         MVC   P+34(4),WORK                                                     
SORTPR10 CLI   PROGPROF+14,C'D'    ALWAYS PRINT DUE DATE                        
         BE    *+12                                                             
         CLI   AGEBY,C'D'          AGE BY DUE DATE                              
         BNE   SORTPR12                                                         
         LA    R3,P+32                                                          
         CLI   PROGPROF+14,C'A'    IF ADV YEAR AND DUE DATE                     
         BNE   *+8                                                              
         LA    R3,P+23             CAN'T PRINT BILL DATE                        
         MVC   0(8,R3),SPACES                                                   
         OC    SORTDUE,SORTDUE     PRINT THE DUE DATE                           
         BZ    SORTPR12                                                         
         GOTO1 DATCON,DMCB,(2,SORTDUE),(8,0(R3))                                
*                                                                               
SORTPR12 DC    0H'0'                                                            
         CLI   DETSW,C'Y'          SWITCH FOR REFERENCE DETAIL                  
         BE    *+12                                                             
         CLI   DETSW,C'A'          SHOW ALL DETAIL                              
         BNE   SORTPR18                                                         
         OC    SORTNUM,SORTNUM                                                  
         BZ    SORTPR18                                                         
         LA    R1,PROFNAME                                                      
         SR    RE,RE                                                            
SORTPR14 CLI   0(R1),0                                                          
         BE    SORTPR18                                                         
         CLC   SORTPROF,1(R1)                                                   
         BE    SORTPR16                                                         
         IC    RE,0(R1)                                                         
         AR    R1,RE                                                            
         B     SORTPR14                                                         
SORTPR16 IC    RE,0(R1)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PSECOND+1(0),2(R1)                                               
         LA    R1,PSECOND+2(RE)                                                 
         MVC   0(9,R1),SORTNUM                                                  
         CLC   SORTDTE2,SPACES     MEDIA MONTH OF SERVICE                       
         BE    SORTPR18                                                         
         LA    R1,8(R1)                                                         
         CLI   0(R1),C' '          SHUFFLE BACK TO FIRST BLANK                  
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         B     *-10                                                             
         MVI   1(R1),C','                                                       
         LA    R3,2(R1)                                                         
         MVC   DUB(2),SORTDTE2                                                  
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(1,DUB),(6,0(R3))                                    
         OC    SORTNET,SORTNET     TAG ON NETWORK NAME IF WE HAVE               
         BZ    SORTPR18                                                         
         LA    R3,6(R3)                                                         
         MVI   0(R3),C','                                                       
         MVC   1(4,R3),SORTNET                                                  
SORTPR18 BAS   RE,PRNT                                                          
         B     SORTPR2                                                          
         SPACE 1                                                                
SORTPRX  GOTO1 VSORTER,DMCB,=C'END'                                             
         MVI   SORTSW,C'Y'         SET RE-INITIALIZE STATUS                     
         B     EXIT                                                             
         EJECT                                                                  
*              LITERALS ETC.                                                    
SORTOPTS DC    XL3'00'             SORT OPTIONS                                 
         SPACE 1                                                                
*                                  SORT PARAMETERS                              
SORTCARD DC    CL80' '                                                          
RECCARD  DC    C'RECORD TYPE=F,LENGTH=(000,,,,) '                               
         SPACE 1                                                                
SORTKEYS DS    0CL3                                                             
         DC    C'C',AL1(SORTSBAC-SORTREC+1,L'SORTSBAC)                          
         DC    C'D',AL1(SORTDATE-SORTREC+1,L'SORTDATE)                          
         DC    C'R',AL1(SORTREF-SORTREC+1,L'SORTREF)                            
         DC    X'FF'                                                            
*                                                                               
DASHES   DC    10C'-'                                                           
         EJECT                                                                  
*              LEVEL NUMBER EQUATES                                             
*                                                                               
SRC      EQU   0                   SOURCE                                       
LVA      EQU   1                   LEVEL A                                      
LVB      EQU   2                   LEVEL B                                      
LVC      EQU   3                   LEVEL C                                      
LVD      EQU   4                   LEVEL D                                      
REQ      EQU   5                   REQUESTS                                     
*                                                                               
TOTALS   DS    0CL(TOTLNQ)                                                      
SRCTOT   DC    AL1(SRC,0,0,0)                                                   
         DC    CL12' '                                                          
         DC    CL36' '                                                          
         DC    CL15'SOURCE TOTALS '                                             
         DC    4PL8'0'                                                          
*                                                                               
LVATOT   DC    AL1(LVA,0,0,LVNME)                                               
         DC    CL12' '                                                          
         DC    CL36' '                                                          
         DC    CL15'ACCOUNT TOTALS'                                             
         DC    4PL8'0'                                                          
*                                                                               
LVBTOT   DC    AL1(LVB,0,0,LVNME)                                               
         DC    CL12' '                                                          
         DC    CL36' '                                                          
         DC    CL15'ACCOUNT TOTALS'                                             
         DC    4PL8'0'                                                          
*                                                                               
LVCTOT   DC    AL1(LVC,0,0,LVNME)                                               
         DC    CL12' '                                                          
         DC    CL36' '                                                          
         DC    CL15'ACCOUNT TOTALS'                                             
         DC    4PL8'0'                                                          
*                                                                               
LVDTOT   DC    AL1(LVD,0,0,LVNME)                                               
         DC    CL12' '                                                          
         DC    CL36' '                                                          
         DC    CL15'ACCOUNT TOTALS'                                             
         DC    4PL8'0'                                                          
*                                                                               
REQTOT   DC    AL1(REQ,0,0,0)                                                   
         DC    CL12' '                                                          
         DC    CL36' '                                                          
         DC    CL15'REQUEST TOTALS'                                             
         DC    4PL8'0'                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
PROFNAME DS    0C                                                               
         DC    AL1(11),C'SESTIMATE='                                            
         DC    AL1(17),C'PPRINT ESTIMATE='                                      
         DC    AL1(18),C'MCAMPAIGN NUMBER='                                     
         DC    AL1(19),C'NNETWORK ESTIMATE='                                    
         DC    AL1(13),C'JJOB NUMBER='                                          
         DC    AL1(12),C'XREFERENCE='                                           
         DC    AL1(0)                                                           
         SPACE 2                                                                
RELOTAB  DS    0A                                                               
         DC    V(BUFFALOC)                                                      
         DC    V(ACLIST)                                                        
         DC    V(SORTER)                                                        
         DC    A(TRNSTAB)                                                       
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER SPACEND                                           
*                                                                               
AC83D    DSECT                                                                  
RELO     DS    F                                                                
VTYPES   DS    0A                                                               
VBUFF    DS    V                                                                
VACLIST  DS    V                                                                
VSORTER  DS    V                                                                
ADRTRNS  DS    A                                                                
         SPACE 2                                                                
THSACC   DS    C                                                                
THISBAC  DS    C                                                                
THISAMNT DS    PL6                 AMOUNT                                       
SAVESBAC DS    CL15                                                             
*                                                                               
SUMOPT   DS    XL1              SUMMARY OPTIONS                                 
RQSRC    EQU   X'80'            BILLING SOURCE SUMMARY AT REQLAST               
RQAC     EQU   X'40'            ACCOUNT SUMMARY AT REQUEST                      
RQACSRC  EQU   X'20'            SOURCE WITHIN ACCOUNT AT AT REQLAST             
ACSRC    EQU   X'10'            SOURCE SUMMARY AT END OF EACH ACCOUNT           
SUMONLY  EQU   X'01'            SUMMARY ONLY (SUPPRESS DETAIL)                  
*                                                                               
DATLIN1  DS    CL61                                                             
DATLIN2  DS    CL61                                                             
REQNAME  DS    CL22                                                             
ASATDATE DS    CL8                                                              
COMPNAME DS    CL36                                                             
COMPADDR DS    4CL26                                                            
DATAB    DS    CL50                                                             
*                                                                               
SAVBACC  DS    CL12                                                             
FRCETOT  DS    CL1                                                              
SUMLEV   DS    XL1                 LEVEL OF SUMMARY                             
SRCCNT   DS    PL2                                                              
THISLEV  DS    XL1                                                              
*                                                                               
TALSW    DS    CL1              FOR TALENT USE CLIENT AS CONTRA                 
BILLSW   DS    CL1                                                              
TOTSW    DS    C                                                                
FORMSW   DS    C                                                                
SAVESPRG DS    C                                                                
LEVELS   DS    C                                                                
FORMACCS DS    CL40                                                             
SAVEACCS DS    CL40                                                             
*                                                                               
INCLUDE  DS    CL1                                                              
SORTSW   DS    CL1                                                              
TNRSW    DS    CL1                                                              
AGEBY    DS    CL1                                                              
DETSW    DS    CL1                                                              
MYMODE   DS    CL1                                                              
BILLTOT  DS    PL8                                                              
MOAEND   DS    CL2                                                              
SAVER4   DS    F                                                                
ATRNSTAB DS    A                   A(CURRENT POSITION IN TRNSTAB)               
         EJECT                                                                  
SORTREC  DS    0C                                                               
SORTSBAC DS    CL15                CONTRA/ACCOUNT                               
SORTDATE DS    CL3                 DATE                                         
SORTREF  DS    CL6                 REFERENCE                                    
SORTMOS  DS    CL2                 MOS                                          
SORTPART DS    XL1                 PARTIAL PAYMENT INDICATOR                    
SORTPROF DS    C                   X'23' EL - PROFILE                           
SORTDTE2 DS    XL2                            DATE                              
SORTNUM  DS    CL9                            NUMBER                            
SORTNET  DS    CL4                            NETWORK CODE                      
SORTCASH DS    PL6                 X'50' EL - CD                                
SORTINCM DS    PL6                            INCOME                            
SORTDUE  DS    CL2                 X'61' EL - DUE DATE                          
SORTCLI  DS    CL3                 X'4F' EL - CLIENT                            
SORTPROD DS    CL3                          - PRODUCT                           
SORTCOMN DS    CL6                          - COMMERCIAL                        
SORTNIL  DS    CL3                 EXTRA                                        
         ORG   SORTCLI                                                          
SORTEST  DS    CL15                         - ESTIMATE NUMBER                   
SORTAMTS DS    4PL8                                                             
SORTNUMB EQU   (*-SORTAMTS)/(L'SORTAMTS)                                        
SORTLEN  EQU   *-SORTREC                                                        
         EJECT                                                                  
*                                                                               
BUFREC   DS    CL(BUFLNQ)                                                       
         ORG   BUFREC                                                           
BUFKEY   DS    0C                                                               
BUFACC   DS    CL12                ACCOUNT CODE                                 
BUFSRC   DS    CL12                BILLING SOURCE                               
BUFKLNQ  EQU   *-BUFKEY                                                         
BUFNUM   DS    XL1                 LEVEL EQUATE                                 
BUFNAME  DS    CL36                ACCOUNT NAME                                 
BUFBUCK  DS    4PL8                ACCUMULATORS                                 
BUFBLNQ  EQU   *-BUFBUCK                                                        
BUFLNQ   EQU   *-BUFKEY                                                         
         EJECT                                                                  
*              DSECT TO COVER TOTAL LINE ENTRIES                                
*                                                                               
TOTD     DSECT                                                                  
TOTNUM   DS    XL1                 LEVEL EQUATE                                 
TOTLEN   DS    CL1                 LEVEL LENGTH                                 
TOTLEV   DS    XL1                 LEVEL NUMBER (LEVEL A =1, B=2, ETC.)         
TOTSTAT  DS    XL1                 STATUS SWITCHES                              
LVACT    EQU   X'80'               LEVEL IS ACTIVE                              
ACACT    EQU   X'40'               ACCOUNT ACTIVITY                             
LVNME    EQU   X'20'               PRINT THE LEVEL NAME                         
*                                                                               
TOTACC   DS    CL12                ACCOUNT/LEVEL CODE                           
TOTNAME  DS    CL36                ACCOUNT NAME                                 
TOTCLASS DS    CL15                CLASSIFICATION (ACCOUNT/REQUEST)             
TOTAMTS  DS    4PL8                ACCUMULATORS                                 
TOTLNQ   EQU   *-TOTD                                                           
         EJECT                                                                  
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
*                                                                               
         BUFF  LINES=500,                                              X        
               ROWS=1,                                                 X        
               COLUMNS=4,                                              X        
               COMMENT=37,                                             X        
               FLAVOR=PACKED,                                          X        
               KEYLIST=(24,A)                                                   
*                                                                               
*                                                                               
TRNSTAB  DS    0D                                                               
         DS    1500CL(SORTLEN)      TABLE OF TRANSACTIONS                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACREP8302 05/01/02'                                      
         END                                                                    
