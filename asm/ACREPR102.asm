*          DATA SET ACREPR102  AT LEVEL 056 AS OF 03/23/15                      
*PHASE ACR102A,+0                                                               
*INCLUDE BUDACC                                                                 
*INCLUDE CONVMOS                                                                
*INCLUDE SORTER                                                                 
*INCLUDE SQUASHER                                                               
*INCLUDE UNDERLIN                                                               
         TITLE 'REALIZATION - CLIENT REPORT'                                    
ACR102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACR1**,R9,R3                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
         USING ACR1D,RC                                                         
         LA    RC,SPACEND                                                       
********************************************************************            
*                                                                   *           
*        READS 1C HISTORY (MONACC) FOR 12 BUDGETS AND INCOME.       *           
*        READS SJ TRANS FOR BILLING VALUE, AND TIME BILLED.         *           
*                                                                   *           
*        START END MONTHS - MMM/YY  MMM/YY                          *           
*                           READ BY BUDGET, BILLING VALUE AND INCOME*           
*                                                                   *           
*        MOA RANGE          MMM/YY       TIME BILLED FOR MMM/YY ONLY*           
*                           -MMM/YY      TIME BILLED THRU MMM/YY    *           
*                           MMM/YY-MMM/YY BILLED MMM/YY THRU MMM/YY *           
*                           BLANK        TIME BILLED ETERNITY       *           
*                           READ BY TIME BILLED.                    *           
*                                                                   *           
*        OPTION 1         - 1C LEVEL OF REPORT                      *           
*                           BLANK - 4TH LEVEL                       *           
*                           3     - 3RD LEVEL                       *           
*                           2     - 2ND LEVEL                       *           
*                           1     - 1ST LEVEL                       *           
*                           S     - CLIENT SUMMARY (ALL CLT IN PF). *           
*                                   NO 1R INFO IS PRINTED SO IT     *           
*                                   DOESN'T MATTER WHAT'S IN QOPT2. *           
*                                                                   *           
*        OPTION 2         - 1R LEVEL OF REPORT                      *           
*                           BLANK - EMPLOYEE LEVEL                  *           
*                           3     - CATEGORY LEVEL                  *           
*                           2     - DEPARTMENT LEVEL                *           
*                           1     - OFFICE LEVEL                    *           
*                           E     - EMPLOYEE'S WITH DEPT AND CAT    *           
*                                   SUPPRESSED.                     *           
********************************************************************            
         EJECT                                                                  
*-------------------------------------------------------                        
*        RUN FIRST ROUTINE                                                      
*-------------------------------------------------------                        
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
         LA    RE,RELOTAB                RELOCATE MY A TYPES                    
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
         ST    RC,0(R2)                  SAVE RC                                
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                  STORE ADDR OF BOX ROUTINE              
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
*                                                                               
         BAS   RE,GETBUFF                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------                        
*        REQUEST FIRST                                                          
*-------------------------------------------------------                        
*                                                                               
REQF     CLI   MODE,REQFRST                                                     
         BNE   BUDG                                                             
         MVC   OPTION2,QOPT2                                                    
         CLI   OPTION2,C'4'                                                     
         BNE   *+8                                                              
         MVI   OPTION2,C' '                                                     
         BAS   RE,GETLEV1C               GET 1C LEVEL STRUCTURE                 
         MVI   NUMCOLS,0                                                        
         MVI   CLSUM,0                                                          
         BAS   RE,BLDCOLS                BUILD COLUMN HEADINGS                  
         CLI   CLILEV,0                                                         
         BNE   *+6                                                              
         DC    H'0'                      NO "CLI=2" ON LEDGER RECORD            
         CLI   CLILEV,2                                                         
         BNE   REQF1                                                            
         CLI   QOPT1,C'S'                                                       
         BNE   REQF1B                                                           
         MVI   CLSUM,1                   SET CLIENT SUMMARY FLAG                
*                                                                               
REQF1B   CLI   QOPT1,C' '                FORCE REPORT ON 3 LEVELS               
         BNE   REQF1                                                            
         MVI   QOPT1,C'3'                                                       
REQF1    CLI   QOPT1,C'S'                CLIENT SUMMARY REPORT                  
         BNE   *+8                                                              
         MVI   RCSUBPRG,4                                                       
*                                                                               
REQF01   MVI   RETSW,0                   INITIALIZE RETAINER SWITCH             
         CLI   QOPT3,C'1'                                                       
         BNE   REQF01A                                                          
         MVI   RETSW,1                   SET TO NONZERO IF WANT RET             
         B     REQF02                    ADDED IN TO TOT BILLED                 
*                                                                               
REQF01A  CLI   QOPT3,C'3'                                                       
         BNE   REQF02                                                           
         MVI   RETSW,1                                                          
*                                                                               
REQF02   L     R4,ADCMPNAM               COMPANY NAME FOR HEADLINES**           
         LA    R5,COMPNAM                                                       
         USING ACNAMED,R4                                                       
         MVC   0(36,R5),SPACES                                                  
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ACNMNAME                                                 
*                                        INIT FOR SORT **                       
         XC    ALSORT,ALSORT             CLEAR A(LAST SORT)                     
         LA    R1,SRTDLNQ                SORT KEY LENGTH                        
         CVD   R1,DUB                    CONVERT KEY LEN TO CHARS               
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         LA    R1,SRTLNQ                 SORT RECORD LENGTH                     
         CVD   R1,DUB                    CONVERT REC LEN TO CHARS               
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,START)      START=YMD PACKED             
         MVC   WORK(4),QEND                                                     
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,ENDATE)     ENDATE=YMD PACKED            
*                                        CLEAR CODE /NAME TABLES **             
REQF03   L     R5,ACODLST                1C NAMES                               
         USING BIND,R5                                                          
         XC    BININ,BININ               CLEAR TABLE                            
         L     R5,AOFFLST                1R OFFICE NAMES                        
         XC    BININ,BININ                                                      
         L     R5,ADPTLST                1R DEPT NAMES                          
         XC    BININ,BININ                                                      
         L     R5,ACATLST                1R CATEGORY NAMES                      
         XC    BININ,BININ                                                      
*                                                                               
         L     R7,AMONACC                CLEAR MOA RANGE DATES SO               
         USING ACMD,R7               MONACC WILL PRINT DATES IN                 
         MVC   M1START,ACMMSTR            DETAILS OF REQUEST BUT WILL           
         MVC   M1END,ACMMEND                NOT HAVE DATES TO FILTER            
         XC    ACMMSTR,ACMMSTR             OUT INFORMATION FROM 1C              
         MVC   ACMMEND,=2X'FF'                                                  
*                                                                               
         MVC   BILLTHRU,SPACES           HEADLINE WORK AREA                     
         CLI   M1END,X'FF'               USE ETERNITY IF NO MOA RANGE           
         BE    REQF13                    (0000 FFFF SET BY MONACC)              
*                                      **MOA RANGE REPORT HEADLINE **           
REQF05   MVC   BILLTHRU(11),=C'TIME BILLED'                                     
         CLI   QOPT3,C'2'          ADD W/O TO HEADLINE FOR OPTS 2&3             
         BL    *+10                                                             
         MVC   BILLTHRU+11(4),=C'-W/O'                                          
         CLC   ACMCMSTR,SPACES                                                  
         BNE   REQF09                                                           
         MVC   BILLTHRU+16(4),=C'THRU'                                          
         MVC   BILLTHRU+21(6),ACMCMEND                                          
         B     REQF13                                                           
REQF09   MVC   BILLTHRU+16(3),=C'FOR'                                           
         MVC   BILLTHRU+20(6),ACMCMSTR                                          
         CLC   M1START,M1END                                                    
         BNE   REQF11                                                           
         MVC   BILLTHRU+27(4),=C'ONLY'                                          
         B     REQF13                                                           
REQF11   MVC   BILLTHRU+27(4),=C'THRU'                                          
         MVC   BILLTHRU+32(6),ACMCMEND                                          
         DROP  R7                                                               
*                                                                               
REQF13   DS    0H                                                               
         GOTO1 SQUASHER,DMCB,BILLTHRU,L'BILLTHRU                                
*                                                                               
REQF15   L     RE,ABUDWK                 CLEAR BUDACC WORK                      
         LA    RF,L'BUDWK                                                       
         XCEF                                                                   
         L     R7,ABUDWK                                                        
         USING BUDACCD,R7                                                       
         MVC   BUADMGR,DATAMGR                                                  
         MVI   BUTYPE,READING            READ BUDGETS MODALLY                   
         MVI   BUCMND,TYPEGET            READ FOR BUDGET TYPE                   
         MVC   BUBUDNO+1(1),QSRTAREA     BUDGET NUMBER                          
         MVC   BUSERKEY(1),QCOMPANY                                             
         GOTO1 BUDACC,DMCB,ABUDWK                                               
         CLI   BUMODE,PROCTYPE           GOOD READ                              
         BE    XIT                                                              
         DC    H'0'                BAD BUDGET READ                              
         EJECT                                                                  
*-------------------------------------------------------                        
*        LOOKUP INCOME BUDGETS AT CLIENT LEVEL OF 1C                            
*-------------------------------------------------------                        
*                                                                               
BUDG     CLI   MODE,PROCLEVC                                                    
         BNE   PROCAC                                                           
*                                                                               
         LA    R6,SRTWRK                                                        
         USING SRTD,R6                                                          
         BAS   RE,CLERSORT               CLEAR SORT WORK AREA*                  
*                                                                               
         L     R7,ABUDWK                                                        
         USING BUDACCD,R7                                                       
         L     R4,ADACC                  ADDR OF CLIENT LEVEL ACC               
*                                                                               
         CLI   CLILEV,3                  IS CLIENT LEVEL ON 3RD LEV?            
         BNE   BUDG1A                                                           
         ZIC   R1,LEVELC                 GET LENGTH OF 1ST 3 LEVS               
         B     BUDG4A                                                           
*                                                                               
BUDG1A   ZIC   R1,LEVELB                 ASSUME ON 2ND LEVEL                    
*                                                                               
BUDG4A   SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTCODE(0),3(R4)          PROFIT CENTER/DIVIS/CLIENT             
         MVC   TEMPCODE,SRTCODE                                                 
         BAS   RE,BREAK                                                         
         MVC   SRTCODE(60),TEMPCODE                                             
*                                                                               
         MVC   BUSTDATE,START            START YYMM OF BUDS                     
         MVC   BUNDDATE,ENDATE           END DATE YYMM OF BUDS                  
         MVC   BUSERKEY,SPACES                                                  
         MVC   BUSERKEY(15),0(R4)        ACCOUNT INTO KEY                       
         MVC   BUSERKEY+17(1),QCOMPANY   COMPANY FOR CONTRA                     
         MVC   BUSERKEY+18(2),=C'12'     FOR 12 CONTRAS ONLY                    
         MVI   BUCMND,AMNTGET            READ AMT REC FROM BUSERKEY             
*                                                                               
BUDGET04 GOTO1 BUDACC,DMCB,ABUDWK                                               
         CLI   BUMODE,FINISHED           NO MORE RECORDS FOR KEY                
         BE    BDGEND                                                           
         CLI   BUMODE,DMGRERR            DM ERROR ON BUDACC READ                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BUSERKEY(15),BULSTKEY+1   SAME AS LAST AMT KEY READ              
         BNE   BDGEND                                                           
         MVI   BUCMND,AMNTNEXT           COMMAND FOR NEXT AMT READ              
         CLI   BUMODE,PROCAMNT           WAS AN AMT REC PROCESSED               
         BNE   BUDGET04                  NO- READ NEXT                          
         LA    R5,BUAMTREC               BUDGET AMOUNT RECORD                   
         USING ACBTKEY,R5                                                       
         CLC   ACBTKCON+1(2),=C'12'      MAKE SURE ITS AN INCOME BUD            
         BNE   BUDGET04                  IF NOT READ NEXT                       
         CLC   QSRTAREA(1),ACBTKBNO+1    MAKE SURE RIGHT BUD NUMBER             
         BNE   BUDGET04                                                         
         AH    R5,DATADISP                                                      
*                                                                               
BUDGET06 CLI   0(R5),0                                                          
         BE    BUDGET04                                                         
         CLI   0(R5),X'1D'               BUDGET AMT ELEMENT                     
         BE    BUDGET10                  YES                                    
*                                                                               
BUDGET08 ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     BUDGET06                  IN SEACH OF X'1D'                      
*                                                                               
         USING ACBAD,R5                                                         
BUDGET10 CLC   ACBAMNTH,START            LOWER THAN START YYMM                  
         BL    BUDGET08                  YES - GO GET NEXT X'1D'                
         CLC   ACBAMNTH,ENDATE           HIGHER THAN END YYMM                   
         BH    BUDGET08                  YES - GO GET NEXT X'1D'                
         AP    SRTBUD,ACBABUDG           ADD BUDGET AMT TO SORT REC             
         B     BUDGET08                  GO GET NEXT X'1D'                      
*                                                                               
         USING ACKEYD,R7                                                        
BDGEND   CP    SRTBUD,=P'0'              DO WE HAVE A BUDGET AMNT               
         BE    BDGEND2                   NO -                                   
         BAS   RE,PUTSORT                PUT TO SORT                            
BDGEND2  BAS   RE,CLERSORT               CLEAR SORT WORK AREA                   
         L     R7,ACREC                  RESET READ SEQ FOR MON                 
         L     R4,ADACC                  ADDR OF CLIENT LEVEL ACC               
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES                               
         MVC   ACKEYACC(42),0(R4)        LAST RECORD PASSED TO ME               
         BAS   RE,READ                   READ FOR THAT RECORD                   
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------                        
*        BUILD SORT RECORD                                                      
*-------------------------------------------------------                        
*                                                                               
PROCAC   CLI   MODE,PROCACC                                                     
         BNE   ACTUAL                                                           
*                                                                               
         LA    R6,SRTWRK                                                        
         USING SRTD,R6                                                          
         BAS   RE,CLERSORT               CLEAR SORT WORK AREA*                  
*                                        BUILD THE 1C SORT KEY*                 
         L     R4,ADACC                  ADDR OF ACCOUNT                        
         MVC   SRTCODE,3(R4)             PROFIT CENTER/DIV/CLT/PROD             
         MVC   TEMPCODE,SRTCODE                                                 
         BAS   RE,BREAK                  BREAK INTO SUBDIVISONS                 
         MVC   SRTCODE(60),TEMPCODE                                             
*                                                                               
         L     R5,ACODLST                                                       
         USING BIND,R5                                                          
         L     R1,BININ            ADD 1 TO NUMBER IN TABLE                     
         LA    R1,1(R1)                                                         
         ST    R1,BININ                                                         
         CLC   BININ,BINMAX        NUMBER TO MAX                                
         BNH   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         BCTR  R1,0                                                             
         MH    R1,=Y(CLILEN)       R1 TO NEXT ENTRY                             
         LA    R5,BINTABLE(R1)                                                  
         USING CLICDE,R5                                                        
         MVC   CLIKEY,0(R4)                                                     
*                                                                               
         MVI   ELCODE,X'20'              NAME ELEMENT                           
         BAS   RE,GETEL                                                         
         BE    *+6                       NO NAME ELEMENT                        
         DC    H'0'                                                             
*                                                                               
         USING ACNAMED,R4                                                       
         ZIC   R1,ACNMLEN                JOB NAME                               
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CLINAME(0),ACNMNAME                                              
         MVC   SRTCNAME,CLINAME          1C CLI NAME INTO SORTREC               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------                        
*        GET THE INCOME ACTUALS FROM U/L 1C                                     
*-------------------------------------------------------                        
*                                                                               
ACTUAL   CLI   MODE,PROCHIST                                                    
         BNE   ACLAST                                                           
         LA    R6,SRTWRK                 SORT WORK AREA                         
         L     R4,ADSUBAC                ADDR OF SUB ACCT                       
         USING TRSUBHD,R4                                                       
         L     R5,ADTRANS                                                       
         CLI   0(R5),X'45'                                                      
         BNE   XIT                                                              
         CLC   TRSBACNT+1(2),=C'12'      MUST BE CONTRA 12                      
         BNE   XIT                                                              
         CLC   TRSBACNT+1(3),=C'124'     RETAINERS HAVE ANALYSIS=4              
         BE    ACTUAL02                                                         
         CLC   TRSBACNT+1(3),=C'126'     EXCLUDE 126/124 ACCS                   
         BE    XIT                                                              
         USING TRHISTD,R5                                                       
ACTUAL01 CLC   TRHSYEAR(2),START         LOWER THAN START YYMM                  
         BL    XIT                                                              
         CLC   TRHSYEAR(2),ENDATE        HIGHER THAN END YYMM                   
         BH    XIT                                                              
         AP    SRTINCOM,TRHSDR           INCOME AMOUNT INTO SORT REC            
         B     XIT                                                              
*                                                                               
ACTUAL02 CLC   TRHSYEAR(2),START                                                
         BL    XIT                                                              
         CLC   TRHSYEAR(2),ENDATE                                               
         BH    XIT                                                              
         AP    SRTRET,TRHSDR             ADD IN RETAINERS                       
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------                        
*        ADD THE RECORD TO SORTER                                               
*-------------------------------------------------------                        
*                                                                               
ACLAST   CLI   MODE,ACCLAST                                                     
         BNE   SJBILD                                                           
ACLAST01 DS    0H                                                               
         BAS   RE,PUTSORT                PUT TO SORT                            
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------                        
*        LOOKUP SJ ACCOUNTS CLI/PRD/JOB                                         
*-------------------------------------------------------                        
*       COSTING ACCT FROM X'24' EL                                              
*       READ TRANS FOR BILLING VALUE                                            
*       AND TIME BILLED                                                         
*                                                                               
SJBILD   CLI   MODE,REQLAST                                                     
         BNE   RUNL                                                             
         MVC   SAVKEY,SPACES                                                    
         LA    R6,SRTWRK                                                        
         USING SRTD,R6                                                          
         BAS   RE,CLERSORT                                                      
         USING ACKEYD,R7                                                        
         L     R7,ACREC                                                         
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES CLEAR KEY                     
         MVC   ACKEYACC(1),RCCOMPFL      MOVE IN COMPANY                        
         MVC   ACKEYACC+1(2),=C'SJ'      U/L SJ                                 
         MVI   ACKEYACC+3,X'41'                                                 
*                                                                               
SJBILD01 BAS   RE,HIGH                                                          
         B     *+8                                                              
SJBILD03 BAS   RE,SEQ                                                           
         CLC   ACKEYACC(3),SAVEKEY       SAME CO/U/L                            
         BE    SJBILD06                  YES CONTINUE                           
         CLC   SRTWRK(SRTDLNQ),SPACES                                           
         BE    *+8                                                              
         BAS   RE,PUTSORT                PUT LAST ONE TO SORT                   
         B     REPORT                                                           
*                                                                               
SJBILD06 CLI   ACKEYACC+6,C' '           IS THIS A CLIENT                       
         BNE   SJBILD10                  NO - CHECK IF PROD                     
         MVC   COSTING(60),SPACES                                               
         MVC   SAVKEY,SPACES                                                    
         MVC   COMPANY(6),ACKEYACC       SAVE CO/UN/LE/CLIENT                   
         XC    CLICOST,CLICOST                                                  
         XC    PRDCOST,PRDCOST                                                  
         XC    JOBCOST,JOBCOST                                                  
         BAS   RE,GET24EL                GET CLIENT COST ACCOUNT                
         MVC   CLICOST,WORK                                                     
         B     SJBILD03                  READ NEXT ACCT                         
*                                                                               
*                                        **PRODUCT LEVEL**                      
SJBILD10 CLI   ACKEYACC+9,C' '           IS THIS A PRODUCT                      
         BNE   SJBILD12                  NO - CHECK JOB                         
         CLC   CLIENT,ACKEYACC+3         SAME AS SAVED CLIENT                   
         BE    *+6                       YES IF NOT....                         
         DC    H'0'                      OUTTA WACK                             
         MVC   PRODUCT,ACKEYACC+6        SAVE PRODUCT CODE                      
         XC    PRDCOST,PRDCOST                                                  
         XC    JOBCOST,JOBCOST                                                  
         BAS   RE,GET24EL                GET JOB COST ACCOUNT                   
         MVC   PRDCOST,WORK                                                     
         B     SJBILD03                  READ NEXT ACCT                         
         EJECT                                                                  
*                                        **JOB LEVEL**                          
SJBILD12 CLC   CLIENT(6),ACKEYACC+3      SAME AS SAVED CLI/PRD                  
         BE    *+6                       YES IF NOT....                         
         DC    H'0'                      OUTTA WACK                             
*                                                                               
         CLC   ACKEYCON,SPACES           IS IT A JOB RECORD                     
         BNE   SJBILD17                                                         
         CLC   SRTWRK(SRTDLNQ),SPACES    ANY THING SAVED                        
         BE    *+12                      NO                                     
         BAS   RE,PUTSORT                PUT RECORD TO SORT                     
         BAS   RE,CLERSORT               *CLEAR SORT WORK AREA*                 
*                                                                               
         MVC   JOB,ACKEYACC+9                                                   
         XC    JOBCOST,JOBCOST                                                  
         BAS   RE,GET24EL                GET JOB COST ACCOUNT                   
         MVC   JOBCOST,WORK                                                     
         MVC   JOB(23),SPACES            CLEAR SAVED JOB WK EMPL                
         LA    R0,3                                                             
         LA    R1,JOBCOST                GET BEST COST ACCOUNT                  
         CLC   0(L'JOBCOST,R1),SPACES                                           
         BNE   SJBILD14                                                         
         SH    R1,=Y(L'JOBCOST)                                                 
         BCT   R0,*-14                                                          
         B     SJBILD15            NO COST ACCOUNT - SKIP IT                    
*                                                                               
SJBILD14 MVC   COSTING,0(R1)                                                    
         L     R5,ACODLST                                                       
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ     MOVE IN PARMS 3,4,5,6                       
         LA    R4,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,COSTING,(R4)                                        
         CLI   DMCB,0                                                           
         BNE   SJBILD15            NOT FOUND - SKIP JOB                         
         L     R5,DMCB                                                          
         ST    R5,ACURCST                                                       
         MVC   TEMPCODE,COSTING+3                                               
         BAS   RE,BREAK                                                         
         MVC   COSTING+3(60),TEMPCODE                                           
         B     SJBILD03            READ TRANSACTIONS                            
*                                                                               
SJBILD15 SR    R1,R1                                                            
         IC    R1,ACKEYACC+14            BUMP TO NEXT JOB                       
         AH    R1,=H'1'                                                         
         STC   R1,ACKEYACC+14                                                   
         MVC   ACKEYWRK(ACLENGTH-ACKEYWRK),SPACES                               
         B     SJBILD01                  READ HIGH                              
*                                                                               
*                                                                               
SJBILD17 CLI   ACRECORD,X'43'                                                   
         BNE   SJBILD20                  SUB-ACCOUNT X'43' EL                   
         CLC   SRTWRK(SRTDLNQ),SPACES    ANY THING SAVED                        
         BE    *+12                      NO                                     
         BAS   RE,PUTSORT                PUT RECORD TO SORT                     
         BAS   RE,CLERSORT               *CLEAR SORT WORK AREA*                 
*                                                                               
         CLC   ACKEYCON+1(2),=C'1R'      IS CONTRA UL 1R                        
         BE    SJBILD18                  YES - OK TO PROCESS                    
         SR    R1,R1                                                            
         IC    R1,ACKEYWRK+1                                                    
         CLC   ACKEYCON+1(2),=C'1R'      IS CONTRA UL 1R                        
         BL    *+8                       PAST THE 1R  FOR W/C                   
         AH    R1,=H'1'                  BUMP TO NEXT W/C                       
         STC   R1,ACKEYWRK+1                                                    
         MVC   ACKEYCON+1(2),=C'1R'      GO RIGHT TO IT                         
         MVC   ACKEYCON+3(ACLENGTH-ACKEYCON-3),SPACES                           
         B     SJBILD01                                                         
*                                                                               
SJBILD18 MVC   JOB(23),ACKEYACC+9        SAVE JOB WKCD EMPLOYEE                 
         LA    R6,SRTWRK                                                        
         USING SRTD,R6                                                          
         LA    R4,ACRECORD                                                      
         USING TRSUBHD,R4                                                       
         MVC   SRTENAME,SPACES                                                  
         ZIC   R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         BM    SJBILD19                                                         
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTENAME(0),TRSBNAME      1R EMPL NAME INTO SRTREC               
*                                                                               
SJBILD19 L     R5,ACURCST                                                       
         USING CLICDE,R5                                                        
         MVC   SRTCODE(60),COSTCODE                                             
         MVC   SRTEMPL,ACKEYCON+3        1R EMPL INTO SORTREC                   
         MVC   SRTCNAME,CLINAME          1C PROD NAME INTO SRTREC               
         B     SJBILD03                                                         
*                                                                               
SJBILD20 CLI   ACRECORD,X'44'            TRANSACTION ELEMENTS                   
         BNE   SJBILD03                                                         
*                                                                               
         USING TRANSD,R4                                                        
         LA    R4,ACRECORD                                                      
         GOTO1 CONVMOS,DMCB,(X'FE',(R4)),MOS                                    
*                                                                               
         CLI   TRNSTYPE,57               CONSIDER WRITEOFFS LATER               
         BE    SJBILD57                                                         
*                                                                               
         MVI   NOTIMESH,C'Y'             ASSUME TIMESHEET WILL FAIL             
*                                                                               
         LA    R4,ACRECORD                                                      
         CLC   MOS,START                 IS MOS LOWER THAN START                
         BL    SJBILD03                  YES - GET NEXT                         
         CLC   MOS,ENDATE                OR HIGHER THAN END DATE                
         BH    SJBILD03                  YES - GET NEXT                         
*                                                                               
         MVI   NOTIMESH,C'N'             TIMESHEET IS GOOD                      
*                                                                               
         AP    SRTBLVAL,TRNSAMNT         BILLING VALUE IN SRTREC                
*                                                                               
         ZAP   BLDVAL,TRNSAMNT           ASSUME FULLY BILLED                    
*                                                                               
         USING ACMD,R4                                                          
         L     R4,AMONACC                                                       
         GOTO1 ACMAPRAT,DMCB,(X'C0',ACREC),0,ADCOMFAC,0,               X        
               ACMAPROB,ACMAPRO2                                                
*                                                                               
         L     R4,ACMAPRO2                                                      
         USING PTAELD,R4                                                        
         CLI   0(R4),PTAELQ        X'77' - PTA ELEMENT                          
         BNE   SJBILD24            ALL DONE                                     
*                                                                               
         MVI   ELCODE,PTAELQ                                                    
         B     *+12                                                             
*                                                                               
SJBILD22 BAS   RE,NEXTEL                 GET  X'77' EL                          
         BNE   SJBILD24                  NONE                                   
*                                                                               
         CLI   PTATYPE,PTATWOF           GET W/O'S                              
         BE    *+12                                                             
         CLI   PTATYPE,PTATWOFR          AND W/O RECOVERY                       
         BNE   SJBILD22                                                         
*                                                                               
         AP    BLDVAL,PTANET                                                    
         B     SJBILD22                                                         
*                                                                               
SJBILD24 OC    ACDTUSED,ACDTUSED         FULLY BILLED                           
         BZ    SJBILD26                  NO - CHECK FOR X'77'                   
*                                                                               
         MVC   HALF,ACDTUSED            FILTER BY BILL DATE                     
         BAS   RE,FILTBDTE                                                      
         BNE   SJBILD03                 B-DATE NO GOOD, GET NEXT                
*                                                                               
         AP    SRTBILLD,BLDVAL          ADD BILLING TO SORT                     
         B     SJBILD03                                                         
*                                                                               
         USING ACMD,R4                                                          
SJBILD26 L     R4,AMONACC                                                       
         L     R4,ACMAPRO2                                                      
         USING PTAELD,R4                                                        
         CLI   0(R4),PTAELQ        X'77' - PTA ELEMENT                          
         BNE   SJBILD03            ALL DONE                                     
*                                                                               
         MVI   ELCODE,PTAELQ                                                    
         B     *+12                                                             
*                                                                               
SJBILD27 BAS   RE,NEXTEL                                                        
         BNE   SJBILD03            ALL DONE                                     
*                                                                               
         CLI   PTATYPE,PTATWOF     IGNORE W/O'S                                 
         BE    SJBILD27                                                         
         CLI   PTATYPE,PTATWOFR    AND W/O RECOVERY                             
         BE    SJBILD27                                                         
*                                                                               
         MVC   HALF,PTARBLDT       BILL DATE                                    
         BAS   RE,FILTBDTE                                                      
         BNE   SJBILD27            NOT BILLED IN DATE RANGE                     
*                                                                               
         AP    SRTBILLD,PTANET     SAVE BILLED AMOUNT                           
         B     SJBILD27                                                         
*                                                                               
*----------------------------------------------------------------------         
*        FILTER MOA OF WRITEOFF VS M1START, M1END                               
*               MOA IS IN MOS                                                   
*----------------------------------------------------------------------         
SJBILD57 CLI   NOTIMESH,C'Y'             WAS THE TIMESHEET FOR THIS NG          
         BE    SJBILD03                  YES, IGNORE ITS W/OS                   
*                                                                               
         CLC   MOS(2),M1START            LOWER THAN START MOA YM                
         BL    SJBILD03                  NOT WRITTEN OFF HERE                   
         CLC   MOS(2),M1END              HIGHER THAN END MOA YM                 
         BH    SJBILD03                                                         
*                                                                               
         USING TRANSD,R4                                                        
         AP    SRTWO,TRNSAMNT                                                   
         B     SJBILD03                                                         
*                                                                               
RUNL     CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         BAS   RE,RELBUFF                                                       
         B     XIT                                                              
*                                                                               
*        FILTER THE PWO BILLDATE IN HALF AGAINST MSTRT, MEND                    
*                                                                               
FILTBDTE NTR1                                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(2,HALF),(1,WORK) DATE BLD =YMD PKED                 
*                                                                               
         CLC   WORK(2),M1START        LOWER THAN START MOA YM                   
         BL    FBBAD                  BILL IS NO GOOD                           
         CLC   WORK(2),M1END                                                    
         BH    FBBAD                                                            
         CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
FBBAD    CR    RE,R1                                                            
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*-------------------------------------------------------                        
*        BREAK SRTCODE INTO PROPER DIVISIONS                                    
*-------------------------------------------------------                        
*                                                                               
BREAK    NTR1                                                                   
         XC    TEMPPC(48),TEMPPC        CLEAR TO BIN ZEROS                      
         ZIC   R1,LEVELA                CHOP DOWN PROFIT CENTER                 
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMPPC(0),TEMPCODE                                               
*                                                                               
         CLI   BLENGTH,0                                                        
         BE    BREAKX                                                           
         ZIC   R1,LEVELB                 CHOP DOWN DIVISION                     
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMPDIV(0),TEMPCODE                                              
*                                                                               
         CLI   CLENGTH,0                                                        
         BE    BREAKX                                                           
         ZIC   R1,LEVELC                 CHOP DOWN CLIENT                       
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMPCLT(0),TEMPCODE                                              
*                                                                               
         CLI   DLENGTH,0                                                        
         BE    BREAKX                                                           
         ZIC   R1,LEVELD                 CHOP DOWN PRODUCT                      
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMPPROD(0),TEMPCODE                                             
*                                                                               
BREAKX   XC    TEMPPROD(L'TEMPPROD),TEMPCLT                                     
         XC    TEMPCLT(L'TEMPCLT),TEMPDIV                                       
         XC    TEMPDIV(L'TEMPDIV),TEMPPC                                        
         OC    TEMPPC(48),SPACES                                                
*                                                                               
         GOTO1 SQUASHER,DMCB,TEMPPROD,12                                        
         GOTO1 SQUASHER,DMCB,TEMPCLT,12                                         
         GOTO1 SQUASHER,DMCB,TEMPDIV,12                                         
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------                        
*        PRINTED REPORT                                                         
*-------------------------------------------------------                        
*                                                                               
REPORT   DS    0H                                                               
         LA    R1,ACCUMS                 CLEAR ALL REPORT TOT ACCUMS            
         LA    R0,BUKCOUNT                                                      
RPT0     ZAP   0(7,R1),=P'0'                                                    
         LA    R1,7(R1)                                                         
         BCT   R0,RPT0                                                          
*                                                                               
         OC    ALSORT,ALSORT             IS THERE A LAST SORT ADDR              
         BZ    RPT99                     NO DATA                                
         MVC   LSTWRK(SRTLNQ),XSPACES    CLEAR SAVE AREA FOR PREV REC           
         MVC   PAGE,=H'1'                SET PAGE TO ONE                        
         MVI   FORCEHED,C'Y'                                                    
         ZAP   RETAIN,=P'0'                                                     
         ZAP   COMART,=P'0'                                                     
         MVI   TOTSW,C'1'                SET FOR CATEGORY TOTAL                 
         MVI   DETAILSW,C'N'             SET TOTAL AS DETAIL LINE SW            
         MVC   SAVKEY,XSPACES                                                   
*                                                                               
RPT1     MVC   COSTING(1),RCCOMPFL       MOVE IN COMPANY                        
         MVC   COSTING+1(2),=C'1C'       U/L 1C                                 
         MVC   COSTCODE(60),SPACES       1C SAVE                                
         MVC   EMPLOYEE(1),RCCOMPFL      MOVE IN COMPANY                        
         MVC   EMPLOYEE+1(2),=C'1R'      U/L 1R                                 
         MVC   EMPCODE,SPACES            1R SAVE                                
         MVC   CNAMES,XSPACES                                                   
         MVC   RNAMES,SPACES                                                    
         ZAP   BUDGETSV,=P'0'                                                   
         ZAP   INCOME,=P'0'                                                     
*                                                                               
RPT2     GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         ST    R6,ALSORT                 ADDRESS OF LAST SORT                   
         LTR   R6,R6                                                            
         BZ    RPT9                      END OF RECORDS FROM SORT               
         MVC   SRTWRK(SRTLNQ),0(R6)      SAVE CURRENT SORT RECORD               
         CLC   LSTWRK(SRTLNQ),XSPACES    DO I HAVE ONE SAVED                    
         BNE   RPT5                      YES - CONTINUE                         
         MVC   LSTWRK(SRTLNQ),SRTWRK     NO  - SAVE THIS ONE                    
         B     RPT2                      AND GET NEXT                           
*                                                                               
RPT5     CLC   LSTWRK(SRTDLNQ),SRTWRK    SAME KEY                               
         BNE   RPT9                      NO - PROCESS SAVED ONE                 
*                                                                               
         LA    R5,LSTWRK                 YES - ADD'EM UP                        
         AP    SRTBUD-SRTD(L'SRTBUD,R5),SRTBUD                                  
         AP    SRTBLVAL-SRTD(L'SRTBLVAL,R5),SRTBLVAL                            
         AP    SRTBILLD-SRTD(L'SRTBILLD,R5),SRTBILLD                            
         AP    SRTINCOM-SRTD(L'SRTINCOM,R5),SRTINCOM                            
         AP    SRTWO-SRTD(L'SRTWO,R5),SRTWO                                     
         AP    SRTRET-SRTD(L'SRTRET,R5),SRTRET                                  
         B     RPT2                      AND GET NEXT                           
*                                                                               
RPT9     BAS   RE,INITNM                                                        
         BAS   RE,REFRES                 PROCESS SAVED RECORD                   
         OC    ALSORT,ALSORT             IS IT END OF FILE                      
         BZ    RPT90                     YES - PRINT FINAL TOTALS               
         LA    R5,LSTWRK                                                        
*                                        IS THERE AN EMPLOYEE CODE              
RPT11    CLC   SRTEMPL-SRTD(L'SRTEMPL,R5),SPACES                                
         BE    RPT18                     NO - ITS A BUD OR INC REC              
*                                        SO SKIP 1R TOTAL CHECK                 
         CLC   SRTEMPL-SRTD(5,R5),SRTEMPL          SAME CAT                     
         BE    RPT12                     YES - CONTINUE                         
         MVI   TOTSW,C'1'                NO  - SET FOR CAT TOTAL                
         BAS   RE,ACCTOT                                                        
*                                                                               
RPT12    CLC   SRTEMPL-SRTD(3,R5),SRTEMPL           SAME DPT                    
         BE    RPT15                     YES - CONTINUE                         
         MVI   TOTSW,C'2'                NO  - SET FOR DPT TOTAL                
         BAS   RE,ACCTOT                                                        
*                                                                               
RPT15    CLC   SRTEMPL-SRTD(1,R5),SRTEMPL          SAME OFF                     
         BE    RPT18                     YES - CONTINUE                         
         MVI   TOTSW,C'3'                NO  - SET FOR OFF TOTAL                
         BAS   RE,ACCTOT                                                        
*                                                                               
RPT18    CLC   LSTWRK(SRTKLNQ),SRTWRK    SAME 1C ACCOUNT                        
         BE    RPT35                     YES - SAVE THIS ONE                    
*                                                                               
         CLI   CLSUM,1                   NO TOTAL FOR CLIENT SUMM               
         BE    RPT22                                                            
         CLI   QOPT1,C'4'                                                       
         BNE   *+8                                                              
         BAS   RE,ROWLINE                                                       
         CLI   QOPT1,C' '                                                       
         BNE   *+8                                                              
         BAS   RE,ROWLINE                                                       
         CLC   SRTPROD-SRTD(L'SRTPROD,R5),SRTPROD     SAME PRODUCT              
         BE    RPT22                     YES - CONTINUE                         
*                                        NO PROD CODE MEANS                     
         CLC   SRTPROD-SRTD(L'SRTPROD,R5),SPACES                                
         BE    RPT22                     CLIENT LEVEL BUDGET                    
         MVI   TOTSW,C'4'                NO  - SET FOR PRD TOTAL                
         BAS   RE,ACCTOT                                                        
*                                                                               
RPT22    CLC   SRTCLT-SRTD(L'SRTCLT,R5),SRTCLT                                  
         BE    RPT25                     YES - CONTINUE                         
         CLI   QOPT1,C'3'                                                       
         BNE   *+8                                                              
         BAS   RE,ROWLINE                                                       
         MVI   TOTSW,C'5'                NO  - SET FOR CLT TOTAL                
         BAS   RE,ACCTOT                                                        
*                                                                               
RPT25    CLC   SRTDIVIS-SRTD(L'SRTDIVIS,R5),SRTDIVIS                            
         BE    RPT28                     YES - CONTINUE                         
         CLI   CLSUM,1                                                          
         BE    RPT28                                                            
         CLI   QOPT1,C'2'                                                       
         BNE   *+8                                                              
         BAS   RE,ROWLINE                                                       
         MVI   TOTSW,C'6'                NO  - SET FOR OFF TOTAL                
         BAS   RE,ACCTOT                                                        
*                                                                               
RPT28    CLC   SRTPROFT-SRTD(L'SRTPROFT,R5),SRTPROFT                            
         BE    RPT35                     YES - CONTINUE                         
         CLI   QOPT1,C'1'                                                       
         BNE   *+8                                                              
         BAS   RE,ROWLINE                                                       
         MVI   TOTSW,C'7'                NO  - SET FOR PF  TOTAL                
         BAS   RE,ACCTOT                                                        
*                                                                               
RPT35    MVC   LSTWRK(SRTLNQ),SRTWRK     SAVE THIS ONE                          
         B     RPT2                      AND GET NEXT.                          
*                                        FINAL TOTALS*                          
RPT90    CLI   CLSUM,1                                                          
         BE    RPT95                                                            
         BAS   RE,ACCTOT                 CAT TOTAL  (1R)                        
         MVI   TOTSW,C'2'                DPT TOTAL  (1R)                        
         BAS   RE,ACCTOT                                                        
         MVI   TOTSW,C'3'                OFF TOTAL  (1R)                        
         BAS   RE,ACCTOT                                                        
         BAS   RE,ROWLINE                                                       
         MVI   TOTSW,C'4'                PRD TOTAL  (1C)                        
         BAS   RE,ACCTOT                                                        
RPT95    MVI   TOTSW,C'5'                CLT TOTAL  (1C)                        
         BAS   RE,ACCTOT                                                        
         CLI   CLSUM,1                                                          
         BE    RPT97                                                            
         MVI   TOTSW,C'6'                DIV TOTAL  (1C)                        
         BAS   RE,ACCTOT                                                        
RPT97    MVI   TOTSW,C'7'                PF TOTAL   (1C)                        
         BAS   RE,ACCTOT                                                        
*                                                                               
RPT99    MVI   TOTSW,C'8'                SET FOR REPORT TOTAL                   
         BAS   RE,ACCTOT                                                        
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------                        
*        INITNAME                                                               
*-------------------------------------------------------                        
*                                                                               
INITNM   NTR1                                                                   
         USING SRTD,R6                                                          
         LA    R6,LSTWRK                                                        
         USING ACKEYD,R7                                                        
         L     R7,ACREC                                                         
         MVC   COSTPC,SRTPROFT     READ FOR PROFIT CENTER                       
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'1C'                                             
         ZIC   R1,LEVELA                                                        
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACKEYACC+3(0),SRTCODE                                            
         BAS   RE,READ                                                          
         BAS   RE,NAMEOUT                                                       
         MVC   PCNAME,WORK                                                      
*                                                                               
         CLI   LEVELB,0                                                         
         BE    INITX                                                            
         MVC   COSTDIV,SRTDIVIS         READ FOR DIVISION NAME                  
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'1C'                                             
         ZIC   R1,LEVELB                                                        
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACKEYACC+3(0),SRTCODE                                            
         BAS   RE,READ                                                          
         BAS   RE,NAMEOUT                                                       
         MVC   DIVNAME,WORK                                                     
*                                                                               
         CLI   LEVELC,0                                                         
         BE    INITX                                                            
         MVC   COSTCLT,SRTCLT            NEW CLIENT CODE                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'1C'                                             
         ZIC   R1,LEVELC                                                        
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACKEYACC+3(0),SRTCODE                                            
         BAS   RE,READ                                                          
         BAS   RE,NAMEOUT                                                       
         MVC   CLTNAME,WORK                                                     
*                                                                               
         CLI   LEVELD,0            READ FOR PRODUCT LEVEL                       
         BE    INITX                                                            
         MVC   COSTPROD,SRTPROD                                                 
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'1C'                                             
         MVC   ACKEYACC+3(12),SRTCODE                                           
         BAS   RE,READ                                                          
         BAS   RE,NAMEOUT                                                       
         MVC   PRDNAME,WORK                                                     
*                                                                               
INITX    DS    0H                                                               
         BAS   RE,HEADUP                                                        
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------                        
*        REFRESH NAMES WHERE NECESSARY                                          
*-------------------------------------------------------                        
*                                                                               
REFRES   NTR1                                                                   
         LA    R6,LSTWRK                                                        
         MVC   XP,XSPACES                                                       
         MVC   XPSECOND,XSPACES                                                 
         USING ACKEYD,R7                                                        
*                                        1C NAMES **                            
         L     R7,ACREC                                                         
         CLC   SRTCODE,COSTCODE          1C ACCOUNT SAME AS SAVED               
         BE    RFRS14Z                   YES- DONT REFRESH 1C NAMES             
*                                                                               
         ZAP   BUDGETSV,=P'0'                                                   
         ZAP   INCOME,=P'0'                                                     
         CP    SRTBUD,=P'0'              IS THERE A BUDGET AMOUNT               
         BE    *+10                      NO - CONTINUE                          
         ZAP   BUDGETSV,SRTBUD           SAVE BUDGET                            
         CP    SRTINCOM,=P'0'            IS THERE A INCOME AMOUNT               
         BE    *+10                      NO - CONTINUE                          
         ZAP   INCOME,SRTINCOM           SAVE INCOME                            
*                                                                               
         CLC   COSTPC,SRTPROFT           SAME PROFIT CENTER                     
         BE    RFRS10                                                           
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES        CLEAR KEY              
         MVC   COSTPC,SRTPROFT                                                  
         MVC   ACKEYACC(3),COSTING       KEY FOR READ                           
         ZIC   R1,LEVELA                                                        
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACKEYACC+3(0),SRTCODE                                            
         BAS   RE,READ                   READ FOR THAT RECORD                   
         BAS   RE,NAMEOUT                                                       
         MVC   PCNAME,WORK               PROFIT CENTER NAME                     
         CLI   QOPT1,C'S'                CLIENT SUMMARY REPORT                  
         BNE   RFRS10                    NO - CONTINUE                          
         BAS   RE,HEADUP                 NEW PAGE FOR EACH PROFIT CTR           
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
         CLI   CLSUM,1                                                          
         BE    RFRS12                                                           
*                                                                               
RFRS10   CLI   QOPT1,C'1'                                                       
         BE    RFRS14                                                           
         CLC   COSTDIV,SRTDIVIS          SAME DIVISION                          
         BE    RFRS12                                                           
         MVC   COSTDIV,SRTDIVIS          NEW DIVISION CODE                      
         CLC   SRTDIVIS,SPACES                                                  
         BE    RFRS12                                                           
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES                               
         MVC   ACKEYACC(3),COSTING       KEY FOR READ                           
         ZIC   R1,LEVELB                                                        
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACKEYACC+3(0),SRTCODE                                            
         BAS   RE,READ                   READ FOR THAT RECORD                   
         BAS   RE,NAMEOUT                                                       
         MVC   DIVNAME,WORK              DIVISION NAME                          
*                                                                               
         CLI   QOPT1,C'S'                CLIENT SUMMARY REPORT                  
         BNE   RFRS12                    NO - SKIP DIV TITLE                    
*                                                                               
         ZIC   RF,LINE                   PRESENT LINE                           
         AH    RF,=H'6'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   RFRS11                                                           
         BAS   RE,HEADUP                 HEAD UP NEW PAGE                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
RFRS11   MVC   XP+1(15),BNAME                                                   
         MVC   XP+15(L'SRTDIVIS),SRTDIVIS NEW DIVISION TITLE                    
         GOTO1 SQUASHER,DMCB,XP+1,50                                            
         BAS   RE,HEADUP                 NEW PAGE FOR EACH PROFIT CTR           
         GOTO1 ACREPORT                                                         
         MVC   XP+1(31),DIVNAME                                                 
         GOTO1 UNDERLIN,DMCB,(31,XP+1),(X'BF',XPSECOND+1)                       
         GOTO1 ACREPORT                                                         
*        B     RFRS12C                                                          
*                                                                               
RFRS12   CLI   QOPT1,C'2'                                                       
         BE    RFRS14                                                           
         CLC   COSTCLT,SRTCLT            SAME CLIENT                            
         BE    RFRS12C                                                          
         MVC   COSTCLT,SRTCLT            NEW CLIENT CODE                        
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES                               
         MVC   ACKEYACC(3),COSTING       KEY FOR READ                           
         ZIC   R1,LEVELC                                                        
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACKEYACC+3(0),SRTCODE                                            
         BAS   RE,READ                   READ FOR THAT RECORD                   
         BAS   RE,NAMEOUT                                                       
         MVC   CLTNAME,WORK              CLIENT NAME                            
*                                                                               
RFRS12C  CLI   QOPT1,C'3'                                                       
         BE    RFRS14                                                           
*                                                                               
         CLI   QOPT1,C'S'                CLIENT SUMMARY REPORT                  
         BNE   RFRS13                                                           
         MVC   COSTCODE(60),SRTCODE                                             
         B     PRNT28                    YES - NO OTHER NAMES NEEDED            
*                                               GO UPDATE TOTALS                
RFRS13   MVC   COSTPROD,SRTPROD          NEW PRODUCT CODE                       
         MVC   PRDNAME,SRTCNAME                      NAME                       
RFRS13B  CLC   SRTPROD,SPACES            NO PROD ITS  BUD REC OR HIGH           
*                                        LEVEL REPORT                           
         BNE   RFRS13C                                                          
         CLI   QOPT1,C' '                1C PRODUCT LEVEL REPORT                
         BE    RFRS14                    YES - SKIP HEADING                     
         CLI   QOPT1,C'C'                1C PRODUCT LEVEL REPORT                
         BE    RFRS14                    YES - SKIP HEADING                     
RFRS13C  BAS   RE,HEADUP                 HEAD UP NEW PAGE                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
         MVC   EMPCODE,SPACES            CLEAR EMPLOYEE SAVE                    
         MVC   RNAMES,SPACES                                                    
*                                        1R NAMES **                            
RFRS14   MVC   COSTCODE(60),SRTCODE                                             
*                                                                               
RFRS14Z  CLI   QOPT1,C'S'                                                       
         BE    PRNT28                                                           
         CLC   SRTEMPL,EMPCODE           1R ACCOUNT SAME AS SAVED               
         BE    PRNT                      YES- DONT REFRESH 1R NAMES             
         SPACE 1                                                                
         CLC   SRTEMPL,SPACES            IF NO EMPL ITS A BUD/INC REC           
         BE    PRNT                                                             
*                                                                               
RFRS14A  CLC   EMPOFFC,SRTOFFC           SAME 1R OFFICE                         
         BE    RFRS16                                                           
         MVC   EMPCODE,SPACES            CLEAR EMPLOYEE SAVE                    
*                                        LOOK IN TABLE FIRST                    
         L     R5,AOFFLST                ADDR OF OFFLIST                        
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ          MOVE IN PARMS 3,4,5,6                  
         LA    R4,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,SRTOFFC,(R4)                                        
         CLI   DMCB,0                                                           
         BNE   RFRS15                    NOT FOUND IN TABLE                     
         L     R5,DMCB                                                          
         USING OFFCDE,R5                                                        
         MVC   OFNAMESV,OFFNAME          1R OFFICE NAME FROM TAB                
         MVC   EMPOFFC,OFFKY             1R OFFICE CODE FROM TAB                
         B     RFRS15A                                                          
*                                        NOT IN TAB - LOOK IT UP                
RFRS15   MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES   CLEAR KEY                   
         MVC   EMPOFFC,SRTOFFC           MOVE IN OFFICE CODE                    
         MVC   ACKEYACC(4),EMPLOYEE      KEY FOR READ                           
         BAS   RE,READ                   READ FOR THAT RECORD                   
         BAS   RE,NAMEOUT                                                       
         MVC   OFNAMESV,WORK             SAVE 1R OFFICE NAME                    
*                                                                               
         MVC   WORK,SPACES               CLEAR TAB ENTRY WORK AREA              
         USING OFFCDE,R5                                                        
         LA    R5,WORK                                                          
         MVC   OFFNAME,OFNAMESV          UPDATE TABLE                           
         MVC   OFFKY,SRTOFFC                                                    
         GOTO1 BINADD,DMCB,(R5),AOFFLST                                         
*                                                                               
RFRS15A  CLI   OPTION2,C'1'              1R OFFICE LEVEL REPORT                 
         BE    PRNT                      YES - SKIP 1R LEVEL TITLES             
*                                                                               
         ZIC   RF,LINE                   PRESENT LINE                           
         AH    RF,=H'6'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   RFRS15B                                                          
         BAS   RE,HEADUP                 HEAD UP NEW PAGE                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
RFRS15B  MVC   XP+1(6),=C'OFFICE'                                               
         MVC   XP+8(1),SRTOFFC           NEW OFFICE TITLE                       
         BAS   RE,HEADUP                 NEW PAGE FOR EACH PROFIT CTR           
         GOTO1 ACREPORT                                                         
         MVC   XP+1(31),OFNAMESV                                                
         GOTO1 UNDERLIN,DMCB,(31,XP+1),(X'BF',XPSECOND+1)                       
         GOTO1 ACREPORT                                                         
*                                                                               
RFRS16   CLC   EMPCODE(3),SRTOFFC        SAME 1R DEPT                           
         BE    RFRS18                                                           
*                                        LOOK IN TABLE FIRST                    
         L     R5,ADPTLST                ADDR OF DPTLIST                        
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ          MOVE IN PARMS 3,4,5,6                  
         LA    R4,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,SRTOFFC,(R4)                                        
         CLI   DMCB,0                                                           
         BNE   RFRS17                    NOT FOUND IN TABLE                     
         L     R5,DMCB                                                          
         USING DPTCDE,R5                                                        
         MVC   DPNAMESV,DPTNAME          DEPT NAME FROM TAB                     
         MVC   EMPDEPT,DPTKY+1           DEPT CODE FROM TAB                     
         B     RFRS17A                                                          
*                                        NOT IN TAB - LOOK IT UP                
RFRS17   MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES   CLEAR KEY                   
         MVC   EMPCODE(3),SRTOFFC        MOVE IN DEPT CODE                      
         MVC   ACKEYACC(6),EMPLOYEE      KEY FOR 1R DEPT                        
         BAS   RE,READ                   READ FOR THAT RECORD                   
         BAS   RE,NAMEOUT                                                       
         MVC   DPNAMESV,WORK             SAVE 1R DEPT NAME                      
*                                                                               
         MVC   WORK,SPACES               CLEAR TAB ENTRY WORK AREA              
         USING DPTCDE,R5                                                        
         LA    R5,WORK                                                          
         MVC   DPTNAME,DPNAMESV          UPDATE TABLE                           
         MVC   DPTKY,SRTOFFC                                                    
         GOTO1 BINADD,DMCB,(R5),ADPTLST                                         
*                                                                               
RFRS17A  CLI   OPTION2,C'2'              1R DEPT LEVEL REPORT                   
         BE    PRNT                      YES - SKIP REST OF 1R TITLES           
*                                                                               
         ZIC   RF,LINE                   PRESENT LINE                           
         AH    RF,=H'6'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   RFRS17B                                                          
         BAS   RE,HEADUP                 HEAD UP NEW PAGE                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
RFRS17B  MVC   XP+1(4),=C'DEPT'                                                 
         MVC   XP+6(2),SRTDEPT           NEW DEPT TITLE                         
         BAS   RE,HEADUP                 NEW PAGE FOR EACH PROFIT CTR           
         GOTO1 ACREPORT                                                         
         MVC   XP+1(31),DPNAMESV                                                
         GOTO1 UNDERLIN,DMCB,(31,XP+1),(X'BF',XPSECOND+1)                       
         GOTO1 ACREPORT                                                         
*                                        LOOK IN TABLE FIRST                    
RFRS18   CLC   EMPCAT,SRTCAT             SAME 1R SUB-DEPT                       
         BE    PRNT                                                             
         L     R5,ACATLST                ADDR OF CATLIST                        
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ          MOVE IN PARMS 3,4,5,6                  
         LA    R4,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,SRTOFFC,(R4)                                        
         CLI   DMCB,0                                                           
         BNE   RFRS19                    NOT FOUND IN TABLE                     
         L     R5,DMCB                                                          
         USING CATCDE,R5                                                        
         MVC   CATNAMSV,CATNAME          CATEGORY NAME FROM TAB                 
         MVC   EMPCAT,CATKY+3            CATEGORY CODE FROM TAB                 
         B     RFRS19A                                                          
*                                                                               
RFRS19   MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES   CLEAR KEY                   
         MVC   EMPCODE(5),SRTOFFC        MOVE IN NEW CATEGORY CODE              
         MVC   ACKEYACC(8),EMPLOYEE      KEY FOR READ                           
         BAS   RE,READ                   READ FOR THAT RECORD                   
         BAS   RE,NAMEOUT                                                       
*                                                                               
         MVC   CATNAMSV,WORK             SAVE 1R CATEGORY NAME                  
         MVC   WORK,SPACES               CLEAR TAB ENTRY WORK AREA              
         USING CATCDE,R5                                                        
         LA    R5,WORK                                                          
         MVC   CATNAME,CATNAMSV          UPDATE TABLE                           
         MVC   CATKY,SRTOFFC                                                    
         GOTO1 BINADD,DMCB,(R5),ACATLST                                         
*                                                                               
RFRS19A  CLI   OPTION2,C'3'              1R CATEGORY LEVEL REPORT               
         BE    PRNT                      YES - SKIP CATEGORY TITLE              
*                                                                               
         ZIC   RF,LINE                   PRESENT LINE                           
         AH    RF,=H'6'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   RFRS19B                                                          
         BAS   RE,HEADUP                 HEAD UP NEW PAGE                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
RFRS19B  MVC   XP+1(8),=C'CATEGORY'                                             
         MVC   XP+10(2),SRTCAT           NEW CATEGORY TITLE                     
         GOTO1 ACREPORT                                                         
         MVC   XP+1(31),CATNAMSV                                                
         GOTO1 UNDERLIN,DMCB,(31,XP+1),(X'BF',XPSECOND+1)                       
         GOTO1 ACREPORT                                                         
         EJECT                                                                  
*-------------------------------------------------------                        
*        PRINT THE DETAIL LINE / UPDATE TOTALS                                  
*-------------------------------------------------------                        
*                                                                               
PRNT     CLI   OPTION2,C'E'              EMPL RPT  CAT/DPT SUPPRESSED           
         BE    *+12                      YES - PRINT EMPL DETAIL                
         CLI   OPTION2,C' '              ANY OTHER INPUT IN OPTION 2            
         BNE   PRNT28                    YES - SURPRESS EMPL DETAIL             
         CLC   SRTEMPL,SPACES            IF NO EMPL ITS A BUD/INC REC           
         BE    PRNT28                    DONT PRINT IT                          
         CLI   QOPT1,C'S'                                                       
         BE    PRNT28                                                           
         ZIC   RF,LINE                   PRESENT LINE                           
         AH    RF,=H'6'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   PRNT05                                                           
         BAS   RE,HEADUP                 HEAD UP NEW PAGE                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
PRNT05   MVC   XP,XSPACES                CLEAR FIRST PRINT LINE                 
         MVC   XPSECOND,XSPACES          CLEAR SECOND PRINT LINE                
         BAS   RE,CLEARBLK               CLEAR PRINT BLOCK                      
         MVC   AMNTTBLE(AMNTLENQ),XSPACES         CLEAR TABLE                   
*                                                                               
         EDIT  (P7,SRTBLVAL),(12,BVAMNT),2,MINUS=YES                            
         EDIT  (P7,SRTBILLD),(12,TBAMNT),2,MINUS=YES                            
         ZAP   SRTBLTOT,SRTBILLD         CALCULATE TOTAL BILLED                 
         EDIT  (P7,SRTBLTOT),(12,BLAMNT),2,MINUS=YES                            
         ZAP   SRTVLDOL,SRTBLVAL         VALUE VS BILLED  (DOLLARS)             
         SP    SRTVLDOL,SRTBILLD                                                
         EDIT  (P7,SRTVLDOL),(12,VBAMNT),2,MINUS=YES                            
         CP    SRTWO,=P'0'                                                      
         BE    PRNT08                                                           
         EDIT  (P7,SRTWO),(12,WOAMNT),2,MINUS=YES                               
*                                                                               
PRNT08   ZAP   TIMEHELD,=P'0'                                                   
         AP    TIMEHELD,SRTBLVAL         BILLING VALUE                          
         AP    TIMEHELD,SRTWO          + TIME WRITTEN OFF                       
         SP    TIMEHELD,SRTBILLD       - TIME BILLED                            
         CP    TIMEHELD,=P'0'          = TIME HELD                              
         BE    PRNT10                                                           
         EDIT  (P7,TIMEHELD),(12,THAMNT),2,MINUS=YES                            
*                                                                               
PRNT10   CP    SRTBLVAL,=P'0'                                                   
         BZ    PRNT23                                                           
         ZAP   ANSWER,SRTBILLD           VALUE VS BILLED  (PCT)                 
         MP    ANSWER,=P'10000'                                                 
         DP    ANSWER,SRTBLVAL                                                  
         SRP   ANSWER(7),64-1,5                                                 
         EDIT  (P7,ANSWER),(11,VPAMNT),1,MINUS=YES,TRAIL=C'%'                   
*                                                                               
PRNT23   MVC   WORK,SPACES                                                      
         MVC   WORK(7),SRTSTAF           EMPLOYEE NAME                          
         MVC   WORK+10(36),SRTENAME                                             
         BAS   RE,PUTNAME                                                       
         BAS   RE,BLDBLC                                                        
*                                                                               
         OC    PRNTBLOC(165),XSPACES                                            
         OC    PRNTBLOC+165(165),XSPACES                                        
         MVC   XP(165),PRNTBLOC                                                 
         MVC   XPSECOND(165),PRNTBLOC+165                                       
         BAS   RE,HEADUP                                                        
         GOTO1 ACREPORT                                                         
         CLC   PRNTBLOC+165(165),XSPACES                                        
         BNE   PRNT28                                                           
         GOTO1 ACREPORT                                                         
*                                                                               
PRNT28   LA    R0,LEVCOUNT               NUMBER OF REPORT LEVEL TOTALS          
         LA    R1,ACCUMS                                                        
PRNT30   AP    0(7,R1),SRTBUD            ADD BUDGET TO TOTALS                   
         AP    7(7,R1),SRTBLVAL          BILLING VALUE                          
         AP    14(7,R1),SRTBILLD         TIME BILLED                            
         AP    21(7,R1),SRTINCOM         INCOME                                 
         AP    28(7,R1),SRTWO                                                   
         AP    35(7,R1),SRTRET                                                  
         LA    R1,TOTLEN(R1)             NEXT LEVEL OF ACCUMS                   
         BCT   R0,PRNT30                                                        
         AP    RETAIN,SRTRET                                                    
         AP    COMART,SRTINCOM           ADD FOR COMMISION ROW                  
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------                        
*        BUILD PRINT BLOCK COLUMNS                                              
*-------------------------------------------------------                        
*                                                                               
BLDBLC  NTR1                                                                    
        XC     CNTNUM,CNTNUM                                                    
        LA     R2,OPTION3                POINT TO OPTION3 TABLE                 
        LA     R5,PRNTBLOC+33                                                   
*                                                                               
        LA     R1,OPTLNQ                 NUMBER OF FORMATS AVAILABLE            
BLC10   CLC    QOPT3(1),0(R2)            CHECK FOR MATCH IN OPTION              
        BE     BLC20                     FOUND CORRECT MATCH                    
        LA     R2,21(R2)                 BUMP TO NEXT OPTION                    
        BCT    R1,BLC10                                                         
        DC     H'0'                      NO MATCHING FORMAT SO DIE              
*                                                                               
        USING  COLUMND,R6                                                       
BLC20   LA     R6,COLUMN                                                        
        LA     R4,AMNTTBLE                                                      
        LA     R2,1(R2)                  BUMP TO FIRST COLUMN POS.              
*                                                                               
BLC25   CLI    0(R2),X'FF'                                                      
        BE     BLCXIT                                                           
        CLI    0(R2),NOPRINT             IS IT NOT PRINTED                      
        BE     BLC20                                                            
        CLI    0(R2),BLANK               IS IT A BLANK COLUMN?                  
        BNE    BLC26                                                            
        LA     R5,14(R5)                 SKIP THAT COLUMN                       
        B      BLC20                                                            
*                                                                               
BLC26   ZIC    R1,0(R2)                  RETRIEVE NUMBER OF COLUMN              
        LA     R7,COLEND                                                        
BLC27   CLM    R1,1,COLNUM               GET FIRST PARAMETER IN COLTBL          
        BE     BLC30                                                            
        LA     R6,8(R6)                  BUMP TO NEXT ROW IN COLUMN             
        BCT    R7,BLC27                                                         
        DC     H'0'                      INVALID INPUT IN TABLE                 
*                                                                               
BLC30   ZIC    R1,COLOFF                                                        
        MH     R1,=H'12'                 CALCULATE OFFSET                       
        AR     R4,R1                     BUMP TO THAT COLUMN                    
        MVC    0(12,R5),0(R4)                                                   
        LA     R5,14(R5)                                                        
        B      BLC20                                                            
*                                                                               
BLCXIT  ZIC    R1,CNTNUM                                                        
        LA     R1,1(R1)                                                         
        STC    R1,CNTNUM                                                        
        LA     R5,PRNTBLOC+198                                                  
        CH     R1,=H'2'                                                         
        BL     BLC20                                                            
        MVC    AMNTTBLE,XSPACES                                                 
        B      XIT                                                              
        DROP   R6                                                               
        EJECT                                                                   
*-------------------------------------------------------                        
*        REPORT TOTALS                                                          
*-------------------------------------------------------                        
ACCTOT   NTR1                                                                   
         USING TOTALD,R5                                                        
         BAS   RE,CLEARBLK                                                      
         MVC   PRNTBLOC(10),=C'TOTALS FOR'                                      
         CLI   TOTSW,C'5'                CLIENT TOTAL                           
         BE    ACTOT13                                                          
         CLI   TOTSW,C'6'                OFFICE TOTAL                           
         BE    ACTOT15                                                          
         CLI   TOTSW,C'7'                PROF CENTER TOT                        
         BE    ACTOT17                                                          
         CLI   TOTSW,C'8'                REPORT TOTAL                           
         BE    ACTOT19                                                          
         CLI   QOPT1,C'S'                                                       
         BE    ACTOT40                                                          
         CLI   TOTSW,C'1'                IS IT A 1R CATEGORY TOTAL              
         BE    ACTOT02                                                          
         CLI   TOTSW,C'2'                DEPT TOTAL                             
         BE    ACTOT05                                                          
         CLI   TOTSW,C'3'                OFFICE TOTAL                           
         BE    ACTOT08                                                          
*                                                                               
         CLI   TOTSW,C'4'                IS IT A 1C PRODUCT TOTAL               
         BE    ACTOT11                                                          
*                                                                               
*                                        ***CATEGORY TOTAL***                   
ACTOT02  LA    R5,CTTOT                  ADDR OF CATEGORY ACCUMS                
         LA    R6,BUKCONT1               NUMBER ACCUM BUCKETS TO CLEAR          
         CLC   EMPCAT,SPACES             IF BLANK,LEVEL TOT NOT NEEDED          
         BE    ACTOT35                   YES- ITS BLANK, CLEAR ACCUMS           
*                                                                               
         CLI   OPTION2,C'3'              CATEGORY LEVEL REPORT                  
         BE    ACT02A                                                           
*                                                                               
ACT02A   CLI   OPTION2,C'3'              CATEGORY LEVEL REPORT                  
         BNE   ACTOT02A                  NO - CONTINUE                          
*                                                                               
         MVI   DETAILSW,C'Y'             PRINT TOTAL AS DETAIL LINE SW          
         BAS   RE,CLEARBLK               PRINT CATEGORY AS DETAIL LINE          
         MVC   PRNTBLOC+1(2),EMPCAT                                             
         MVC   PRNTBLOC+4(36),CATNAMSV   NAME                                   
         B     *+16                                                             
ACTOT02A MVC   PRNTBLOC+11(2),EMPCAT     CATEGORY CODE                          
         MVC   PRNTBLOC+14(30),CATNAMSV  NAME                                   
*                                                                               
         CP    TOTBLVAL,=P'0'            IF NO BILLING VALUE                    
         BNE   *+14                      OR TIME BILLED                         
         CP    TOTBILLD,=P'0'            SKIP THIS 1R LEVEL TOTAL               
         BE    ACTOT35                   CLEAR ACCUMS                           
         B     ACTOT20                   PUT OUT TOTALS                         
*                                        ***DEPARTMENT TOTAL***                 
ACTOT05  LA    R5,DPTTOT                 ADDR OF DEPT ACCUMS                    
         LA    R6,BUKCONT2               NUMBER ACCUM BUCKETS TO CLEAR          
         CLC   EMPDEPT,SPACES            IF BLANK,LEVEL TOT NOT NEEDED          
         BE    ACTOT35                   YES- ITS BLANK, CLEAR ACCUMS           
         CLI   OPTION2,C'2'              DEPT LEVEL REPORT                      
         BNE   ACTOT05A                  NO - CONTINUE                          
*                                                                               
         MVI   DETAILSW,C'Y'             PRINT TOTAL AS DETAIL LINE SW          
         BAS   RE,CLEARBLK               PRINT DEPT AS DETAIL LINE              
         MVC   PRNTBLOC+1(2),EMPDEPT                                            
         MVC   PRNTBLOC+4(36),DPNAMESV   NAME                                   
         B     *+16                                                             
ACTOT05A MVC   PRNTBLOC+11(2),EMPDEPT    DEPT CODE                              
         MVC   PRNTBLOC+14(30),DPNAMESV  NAME                                   
*                                                                               
         CP    TOTBLVAL,=P'0'            IF NO BILLING VALUE                    
         BNE   *+14                      OR TIME BILLED                         
         CP    TOTBILLD,=P'0'            SKIP THIS 1R LEVEL TOTAL               
         BE    ACTOT35                   CLEAR ACCUMS                           
         B     ACTOT20                   PUT OUT TOTALS                         
*                                        ***OFFICE TOTAL***                     
ACTOT08  LA    R5,OFTOT                  ADDR OF OFFICE ACCUMS                  
         LA    R6,BUKCONT3               NUMBER ACCUM BUCKETS TO CLEAR          
         CLC   EMPOFFC,SPACES            IF BLANK,LEVEL TOT NOT NEEDED          
         BE    ACTOT35                   YES- ITS BLANK, CLEAR ACCUMS           
         CLI   OPTION2,C'1'              1R OFFICE LEVEL REPORT                 
         BNE   ACTOT08A                  NO - CONTINUE                          
*                                                                               
         MVI   DETAILSW,C'Y'             PRINT TOTAL AS DETAIL LINE SW          
         BAS   RE,CLEARBLK               PRINT 1R OFFC AS DETAIL LINE           
         MVC   PRNTBLOC+1(1),EMPOFFC                                            
         MVC   PRNTBLOC+3(36),OFNAMESV   NAME                                   
         B     *+16                                                             
*                                                                               
ACTOT08A MVC   PRNTBLOC+11(1),EMPOFFC    OFFICE CODE                            
         MVC   PRNTBLOC+14(30),OFNAMESV  NAME                                   
         CP    TOTBLVAL,=P'0'            IF NO BILLING VALUE                    
         BNE   *+14                      OR TIME BILLED                         
         CP    TOTBILLD,=P'0'            SKIP THIS 1R LEVEL TOTAL               
         BE    ACTOT35                   CLEAR ACCUMS                           
         B     ACTOT20                   PUT OUT TOTALS                         
*                                        ***PRODUCT TOTAL***                    
ACTOT11  LA    R5,PDTOT                  ADDR OF PRODUCT ACCUMS                 
         LA    R6,BUKCONT4               NUMBER ACCUM BUCKETS TO CLEAR          
         CLC   COSTPROD,SPACES           IF BLANK,LEVEL TOT NOT NEEDED          
         BE    ACTOT35                   YES- ITS BLANK, CLEAR ACCUMS           
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'COSTPROD),COSTPROD                                        
         MVC   WORK+12(L'PRDNAME),PRDNAME                                       
         GOTO1 SQUASHER,DMCB,WORK,50                                            
         MVC   PRNTBLOC+11(32),WORK                                             
         B     ACTOT20                                                          
*                                        ***CLIENT TOTAL***                     
ACTOT13  LA    R5,CLTOT                  ADDR OF CLIENT ACCUMS                  
         LA    R6,BUKCONT5               NUMBER ACCUM BUCKETS TO CLEAR          
         CLC   COSTCLT,SPACES            IF BLANK,LEVEL TOT NOT NEEDED          
         BE    ACTOT35                   YES- ITS BLANK, CLEAR ACCUMS           
         CLI   QOPT1,C'S'                1C CLIENT SUMMARY REPORT               
         BNE   ACTOT13A                  NO - CONTINUE                          
*                                                                               
         MVI   DETAILSW,C'Y'             PRINT TOTAL AS DETAIL LINE SW          
         BAS   RE,CLEARBLK               1C CLIENT  AS DETAIL LINE              
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'COSTCLT),COSTCLT                                          
         MVC   WORK+12(L'CLTNAME),CLTNAME                                       
*                                                                               
ACTOT132 GOTO1 SQUASHER,DMCB,WORK,50                                            
         MVC   PRNTBLOC+1(40),WORK                                              
         B     ACTOT20                                                          
*                                                                               
ACTOT13A MVC   WORK,SPACES                                                      
         MVC   WORK(L'COSTCLT),COSTCLT                                          
         MVC   WORK+12(L'CLTNAME),CLTNAME                                       
         GOTO1 SQUASHER,DMCB,WORK,50                                            
         MVC   PRNTBLOC+11(40),WORK                                             
         B     ACTOT20                                                          
*                                        ***DIVISION TOTAL***                   
ACTOT15  LA    R5,DVTOT                  ADDR OF DIVISION ACCUMS                
         LA    R6,BUKCONT6               NUMBER ACCUM BUCKETS TO CLEAR          
         CLI   QOPT1,C'S'                                                       
         BNE   ACTOT15A                                                         
         CLI   CLILEV,2                                                         
         BNE   ACTOT15A                                                         
         B     ACTOT15B                                                         
ACTOT15A CLC   COSTDIV,SPACES            IF BLANK,LEVEL TOT NOT NEEDED          
         BE    ACTOT35                   YES- ITS BLANK, CLEAR ACCUMS           
ACTOT15B MVC   WORK,SPACES                                                      
         MVC   WORK(L'COSTDIV),COSTDIV                                          
         MVC   WORK+12(L'DIVNAME),DIVNAME                                       
         GOTO1 SQUASHER,DMCB,WORK,50                                            
         MVC   PRNTBLOC+11(40),WORK                                             
         B     ACTOT20                                                          
*                                        ***PROFIT CENTER TOTAL***              
ACTOT17  LA    R5,PFTOT                  ADDR OF PROFIT CENTER ACCUMS           
         LA    R6,BUKCONT7               NUMBER ACCUM BUCKETS TO CLEAR          
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'COSTPC),COSTPC                                            
         MVC   WORK+12(L'PCNAME),PCNAME                                         
         GOTO1 SQUASHER,DMCB,WORK,50                                            
         MVC   PRNTBLOC+11(40),WORK                                             
         B     ACTOT20                                                          
*                                        ***REPORT TOTAL***                     
ACTOT19  LA    R5,RPTOT                  ADDR OF REPORT TOTAL ACCUMS            
         LA    R6,BUKCOUNT               NUMBER ACCUM BUCKETS TO CLEAR          
         MVC   PRNTBLOC+11(6),=C'REPORT' REPORT TOTAL                           
*                                                                               
ACTOT20  DS    0H                                                               
         CLI   QOPT1,C' '                1C PRODUCT LEVEL REPORT                
         BNE   ACTOT21                   NO - THEN LONE CLT LEVEL BUDS          
*                                        ARE NO PROBLEM                         
         CLI   TOTSW,C'5'                TOTAL ON CLIENT LEVEL                  
         BNE   ACTOT21                   NO - CONTINUE                          
         CP    TOTBLVAL,=P'0'            IS THERE A BILLING VALUE TOT           
         BNE   ACTOT21                   YES - NO PROBLEM, CONTINUE             
         CP    TOTBILLD,=P'0'            IS THERE A TIME BILLED TOT             
         BNE   ACTOT21                   YES -STILL NO PROBLEM CONTINUE         
         CP    TOTBUD,=P'0'              IS THERE A BUDGET                      
         BE    ACTOT21                   NO - CONTINUE                          
         BAS   RE,HEADUP                 YES - NEW PAGE FOR CLIENT TOTS         
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
ACTOT21  ZAP   PK7,TOTBILLD              FEE'S BILLED                           
         CLI   TOTSW,C'3'                IS THIS A 1R LEVEL TOTAL               
         BNH   ACTOT23                   YES- SKIP EDIT OF BUD/INCOME           
*                                        AND DON'T ADD INCOME INTO              
*                                        TOTAL BILLED                           
         CLI   RETSW,0                   ADD RETAINERS IN TO TOTBILLED?         
         BE    *+10                                                             
         AP    PK7,TOTRET                ONLY ADD RETAINERS TO 1C TOTAL         
         CP    TOTRET,=P'0'                                                     
         BE    ACTOT21A                  PRINT RETAINERS ONLY ON 1C LEV         
         EDIT  (P7,TOTRET),(12,RTAMNT),2,MINUS=YES                              
*                                                                               
ACTOT21A CLI   TOTSW,C'4'                IS THIS A 1C PRODUCT LEVEL             
         BE    ACTOT22                   YES- SKIP EDIT OF BUDGET               
*                                                                               
         EDIT  (P7,TOTBUD),(12,BDAMNT),2,MINUS=YES                              
ACTOT22  AP    PK7,TOTINCOM              FEE'S BILLD+INCOME=TOT BLD             
         EDIT  (P7,TOTINCOM),(12,CMAMNT),2,MINUS=YES                            
*                                                                               
ACTOT23  EDIT  (P7,TOTBLVAL),(12,BVAMNT),2,MINUS=YES                            
         EDIT  (P7,TOTBILLD),(12,TBAMNT),2,MINUS=YES                            
         EDIT  (P7,PK7),(12,BLAMNT),2,MINUS=YES                                 
*                                                                               
         ZAP   DUB,TOTBLVAL              BILLING VAL-FEE BLD=VAL/BLD            
         SP    DUB,TOTBILLD                                                     
         EDIT  (P8,DUB),(12,VBAMNT),2,MINUS=YES                                 
*                                                                               
         CP    TOTBLVAL,=P'0'                                                   
         BZ    ACTOT25                                                          
         ZAP   ANSWER,TOTBILLD           TOTAL BLDX100/BILLING VAL=PCT          
         MP    ANSWER,=P'10000'                                                 
         DP    ANSWER,TOTBLVAL                                                  
         SRP   ANSWER(7),64-1,5                                                 
         EDIT  (P7,ANSWER),(11,VPAMNT),1,MINUS=YES,TRAIL=C'%'                   
*                                                                               
ACTOT25  CLI   TOTSW,C'4'                LEVEL BELOW 1C CLIENT LEVEL            
         BNH   ACTOT28                   YES- SKIP BUD VS BILLED                
*                                                                               
         ZAP   DUB,TOTBUD                BUDGET - TOT BLD=BUD/BLD               
         SP    DUB,PK7                                                          
         EDIT  (P8,DUB),(12,BBAMNT),2,MINUS=YES                                 
*                                                                               
         CP    TOTBUD,=P'0'              NO BUDGET - SKIP BUDGET PCT            
         BZ    ACTOT28                                                          
         ZAP   ANSWER,PK7                TOTAL BILLED                           
         MP    ANSWER,=P'10000'          X  100                                 
         DP    ANSWER,TOTBUD             / TOTAL BUD = BUDGET VS BILLD          
         SRP   ANSWER(7),64-1,5                                                 
         EDIT  (P7,ANSWER),(11,BPAMNT),1,MINUS=YES,TRAIL=C'%'                   
*                                        * PRINT THE TOTAL LINE *               
ACTOT28  DS    0H                                                               
         CP    TOTWO,=P'0'                                                      
         BE    ACTOT28A                                                         
         EDIT  (P7,TOTWO),(12,WOAMNT),2,MINUS=YES                               
*                                                                               
ACTOT28A DS    0H                                                               
         ZAP   TIMEHELD,=P'0'                                                   
         AP    TIMEHELD,TOTBLVAL         BILLING VALUE                          
         AP    TIMEHELD,TOTWO          + TIME WRITTEN OFF                       
         SP    TIMEHELD,TOTBILLD       - TIME BILLED                            
         CP    TIMEHELD,=P'0'                                                   
         BE    ACTOT28B                                                         
*                                      = TIME HELD                              
         EDIT  (P7,TIMEHELD),(12,THAMNT),2,MINUS=YES                            
ACTOT28B ZIC   RF,LINE                   PRESENT LINE                           
         AH    RF,=H'10'                                                        
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   ACTOT30                                                          
         BAS   RE,HEADUP                 HEAD UP NEW PAGE                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
ACTOT30  MVC   WORK(60),PRNTBLOC         MOVE IN NAME TO CRUNCH                 
         BAS   RE,CLEARBLK                                                      
         BAS   RE,PUTNAME                                                       
         BAS   RE,BLDBLC                                                        
*                                                                               
         OC    PRNTBLOC(165),XSPACES                                            
         OC    PRNTBLOC+165(165),XSPACES                                        
         MVC   XP(165),PRNTBLOC                                                 
         MVC   XPSECOND(165),PRNTBLOC+165                                       
         GOTO1 ACREPORT                                                         
         CLI   DETAILSW,C'Y'             IS THIS 'TOTAL' DETAIL LINE            
         BE    *+8                       YES - SKIP SPACING                     
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                  AND PRINT IT                           
*                                      * CLEAR LEVEL ACCUMS *                   
ACTOT35  ZAP   0(7,R5),=P'0'                                                    
         LA    R5,7(R5)                                                         
         BCT   R6,ACTOT35                                                       
ACTOT40  MVI   TOTSW,C'1'                RESET TO CAT TOTAL                     
         MVI   DETAILSW,C'N'             RESET TOTAL AS DETAIL LINE SW          
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*-------------------------------------------------------                        
*        GET LEVEL STRUCTURE OF 1C / CALCULATE OFFSETS                          
*-------------------------------------------------------                        
*                                                                               
GETLEV1C NTR1                                                                   
         USING ACKEYD,R7             BUILD THE KEY                              
         L     R7,ACREC              COVER AREA TO BUILD KEY                    
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES   CLEAR KEY                   
         MVC   ACKEYACC(1),RCCOMPFL  MOVE IN COMPANY                            
         MVC   ACKEYACC+1(2),=C'1C'  COMPANY/1C U/L                             
         BAS   RE,HIGH                                                          
         CLC   ACKEYACC(3),SAVEKEY   SAME CO/U/L?                               
         BE    *+14                  IF YES THEN LOOK FOR 16 ELEM               
         MVC   LEVEL(4),=X'0C000000' SET THE DEFAULT LEDGER LENGTH              
         B     XIT                   AND LEAVE                                  
*                                                                               
         L     R2,ACREC                                                         
         AH    R2,DATADISP           R2=A(RECORD)                               
GETLEV2  CLI   0(R2),0               DID WE HIT END OF RECORD??                 
         BNE   *+14                                                             
         MVC   LEVEL(4),=X'0C000000' SET DEFAULT HIERCHY ELEMENT                
         B     GETLEV5                                                          
         CLI   0(R2),X'16'           CHECK FOR HIERCHY ELEMENT                  
         BE    GETLEV3               WE FOUND THE RIGHT ELEMENT                 
         ZIC   R1,1(R2)              RETRIEVE LENGTH OF ELEMENT                 
         AR    R2,R1                 BUMP TO NEXT ELEMENT                       
         B     GETLEV2               LOOP BACK UP AGAIN                         
*                                                                               
         USING ACHEIRD,R2                                                       
GETLEV3  MVC   LEVELA,ACHRLEVA       STRUCTURE A/B/C/D                          
         MVC   LEVELB,ACHRLEVB                                                  
         MVC   LEVELC,ACHRLEVC                                                  
         MVC   LEVELD,ACHRLEVD                                                  
*                                                                               
         MVC   ANAME,ACHRDESA                                                   
         MVC   BNAME,ACHRDESB                                                   
         MVC   CNAME,ACHRDESC                                                   
         MVC   DNAME,ACHRDESD                                                   
*                                                                               
         XC    LENGTHS,LENGTHS      CLEAR OUT LEVELS                            
         MVC   ALENGTH(1),LEVELA    STORE ALENGTH OF LEVEL                      
         ZIC   R1,LEVELA                                                        
         ZIC   R2,LEVELB                                                        
         SR    R2,R1                                                            
         CLI   LEVELB,0                                                         
         BE    GETLEV5                                                          
         STC   R2,BLENGTH           STORE OFFSET OF SECOND LEVEL                
*                                                                               
         ZIC   R1,LEVELB                                                        
         ZIC   R2,LEVELC                                                        
         SR    R2,R1                                                            
         CLI   LEVELC,0                                                         
         BE    GETLEV5                                                          
         STC   R2,CLENGTH           STORE OFFSET OF THIRD LEVEL                 
*                                                                               
         ZIC   R1,LEVELC                                                        
         ZIC   R2,LEVELD                                                        
         SR    R2,R1                                                            
         CLI   LEVELD,0                                                         
         BE    GETLEV5                                                          
         STC   R2,DLENGTH           STORE ALENGTH OF THIRD LEVEL                
*                                                                               
GETLEV5  L     R2,ACREC                                                         
         AH    R2,DATADISP           R2=A(RECORD)                               
         MVI   CLILEV,0                                                         
*                                                                               
GETLEV6  CLI   0(R2),0               DID WE HIT END OF RECORD??                 
         BNE   *+6                                                              
         DC    H'0'                  MUST BE A LEDGER ELEMENT                   
         CLI   0(R2),X'14'           FIND LEVEL OF CLIENT LEVEL                 
         BE    GETLEV7               WE FOUND THE RIGHT ELEMENT                 
         ZIC   R1,1(R2)              RETRIEVE LENGTH OF ELEMENT                 
         AR    R2,R1                 BUMP TO NEXT ELEMENT                       
         B     GETLEV6               LOOP BACK UP AGAIN                         
*                                                                               
         USING ACLEDGD,R2                                                       
GETLEV7  MVC   CLILEV,ACLTCLI        STORE CLIENT LEVEL                         
         B     XIT                                                              
         DROP  R7                                                               
         DROP  R2                                                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------                        
*       BUILD COLUMN HEADINGS BY USING TABLES                                   
*-------------------------------------------------------                        
*                                                                               
BLDCOLS NTR1                                                                    
        LA     R2,OPTION3                POINT TO OPTION3 TABLE                 
        LA     R5,HEADBLOC                                                      
        MVC    HEADBLOC(130),SPACES                                             
        MVC    HEADBLOC+130(130),SPACES                                         
        MVC    HEADBLOC+260(130),SPACES                                         
*                                                                               
        LA     R1,OPTLNQ                 NUMBER OF FORMATS AVAILABLE            
BLD10   CLC    QOPT3(1),0(R2)            CHECK FOR MATCH IN OPTION              
        BE     BLD20                     FOUND CORRECT MATCH                    
        LA     R2,21(R2)                 BUMP TO NEXT OPTION                    
        BCT    R1,BLD10                                                         
        DC     H'0'                      NO MATCHING FORMAT SO DIE              
*                                                                               
        USING  COLUMND,R4                                                       
BLD20   LA     R4,COLUMN                 CONTAINS DATA ABOUT COLUMN             
        LA     R2,1(R2)                  BUMP TO FIRST COLUMN POS.              
*                                                                               
BLD25   CLI    0(R2),X'FF'               END OF A LINE?                         
        BE     XIT                                                              
        CLI    0(R2),NOPRINT                                                    
        BE     BLD20                                                            
        CLI    0(R2),BLANK               SKIP THESE COLUMNS                     
        BE     BLD45                                                            
*                                                                               
        ZIC    R1,0(R2)                  RETRIEVE NUMBER OF COLUMN              
BLD27   ZIC    R0,COLNUM                 GET FIRST PARAMETER IN COLTBL          
        CR     R1,R0                                                            
        BE     BLD30                                                            
        LA     R4,8(R4)                  BUMP TO NEXT ROW IN COLUMN             
        B      BLD27                                                            
*                                                                               
BLD30   L      R6,COLNAME                LOAD IN ADDRESS OF HEADING             
        MVC    0(12,R5),0(R6)            LOAD IN LINE 1 OF HEADING              
        MVC    130(12,R5),12(R6)         LINE 2                                 
        MVC    260(12,R5),24(R6)         LINE 3                                 
*                                                                               
BLD40   LA     R5,14(R5)                 BUMP TO NEXT COLUMN                    
BLD45   ZIC    R1,NUMCOLS                KEEP TRACK OF NUMBER COLS              
        LA     R1,1(R1)                                                         
        STC    R1,NUMCOLS                                                       
        CH     R1,=H'9'                                                         
        BE     XIT                       MAX COLS ACROSS IS 9                   
        B      BLD20                                                            
        DROP   R4                                                               
        EJECT                                                                   
*-------------------------------------------------------                        
*       PRINT HEADLINES                                                         
*-------------------------------------------------------                        
*                                                                               
HEADUP   NTR1                                                                   
         MVC   XHEAD3+26(36),COMPNAM          COMPANY NAME                      
*                                                                               
         MVC   XHEAD4+1(15),ANAME                                               
         MVC   XHEAD4+17(9),COSTPC            PROFIT CENTER CODE                
         MVC   XHEAD4+26(36),PCNAME                         NAME                
*                                                                               
         CLI   QOPT1,C'S'                     CLIENT SUMMARY REPORT             
         BE    HEADUP03                       YES - SKIP TO DATE                
         CLI   QOPT1,C'1'                                                       
         BE    HEADUP03                                                         
         MVC   XHEAD5+1(15),BNAME                                               
         MVC   XHEAD5+17(9),COSTDIV           DIVISION CODE                     
         MVC   XHEAD5+26(36),DIVNAME                   NAME                     
*                                                                               
         CLI   QOPT1,C'1'                                                       
         BE    HEADUP03                                                         
         CLI   QOPT1,C'2'                                                       
         BE    HEADUP03                                                         
         MVC   XHEAD6+1(15),CNAME                                               
         MVC   XHEAD6+17(9),COSTCLT           CLIENT CODE                       
         MVC   XHEAD6+26(36),CLTNAME                 NAME                       
*                                                                               
         CLI   QOPT1,C'1'                                                       
         BE    HEADUP03                                                         
         CLI   QOPT1,C'2'                                                       
         BE    HEADUP03                                                         
         CLI   QOPT1,C'3'                                                       
         BE    HEADUP03                                                         
         CLI   LEVELD,0                                                         
         BE    HEADUP03                                                         
         MVC   XHEAD7+1(15),DNAME                                               
         MVC   XHEAD7+17(9),COSTPROD          PROD CODE                         
         MVC   XHEAD7+26(36),PRDNAME               NAME                         
*                                                                               
HEADUP03 MVC   XHEAD7+124(38),BILLTHRU        BILLED THRU HEADLINE              
         MVC   XHEAD10+33(130),HEADBLOC                                         
         MVC   XHEAD11+33(130),HEADBLOC+130                                     
         MVC   XHEAD12+33(130),HEADBLOC+260                                     
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------                        
*        LOOKUP NAME ELEMENT                                                    
*-------------------------------------------------------                        
*                                                                               
NAMEOUT  NTR1                            ** NAME LOOKUP **                      
         L     R4,ACREC                                                         
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                      NO NAME ELEMENT                        
         USING ACNAMED,R4                                                       
         MVC   WORK(36),SPACES                                                  
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   WORK(0),ACNMNAME                                                 
         DROP  R4                                                               
         EJECT                                                                  
*-------------------------------------------------------                        
*        LOOK UP THE COSTING ACCOUNT                                            
*-------------------------------------------------------                        
*                                                                               
GET24EL  NTR1                                                                   
         MVC   WORK(15),SPACES                                                  
         L     R4,ACREC                                                         
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING ACPROFD,R4                                                       
         CLI   ACPRCOST+3,C' '                                                  
         BNH   XIT                                                              
         MVC   WORK(15),ACPRCOST                                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*-------------------------------------------------------                        
*        PUT RECORD TO SORTER                                                   
*-------------------------------------------------------                        
*                                                                               
PUTSORT  NTR1                                                                   
         USING SRTD,R6                                                          
         LA    R6,SRTWRK                                                        
         LR    R1,R6                     ADDR OF SORT REC INTO R1               
         LA    R1,SBUKLOC(R1)            START OF COL BUCKETS                   
         LA    R0,SBUKCONT               NUMBER OF BUCKETS INTO R0              
*                                                                               
PUT00    CP    0(7,R1),=P'0'             CONTAIN PACKED ZEROS                   
         BNZ   PUT01                     NOT ZERO-WE WANT IT                    
         LA    R1,7(R1)                  BUMP TO NEXT BUCKET                    
         BCT   R0,PUT00                                                         
         B     XIT                                                              
*                                                                               
PUT01    CLI   QOPT1,C' '                                                       
         BE    PUT55                                                            
*                                                                               
         CLI   CLSUM,1                   CLIENT SUMMARY                         
         BNE   PUT02                                                            
*                                                                               
         MVC   WORK,SPACES                                                      
         ZIC   R1,LEVELB                                                        
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SRTCODE           STORE FIRST 2 LEVS                     
         MVC   SRTCODE(12),WORK                                                 
         MVC   SRTCLT,SRTDIVIS           MOVE CLIENT LEVEL TO 3RD LEV           
         MVC   SRTDIVIS,SPACES                                                  
         MVC   SRTPROD,SPACES                                                   
         MVC   SRTEMPL,SPACES                                                   
         B     PUT99                                                            
*                                                                               
PUT02    CLI   QOPT1,C'3'                CLIENT LEVEL REPORT                    
         BNE   *+14                                                             
         MVC   SRTPROD,SPACES                                                   
         B     PUT55                                                            
         CLI   QOPT1,C'2'                DIVISION LEVEL REPORT                  
         BNE   PUT03                                                            
         MVC   SRTCLT,SPACES                                                    
         MVC   SRTPROD,SPACES                                                   
         B     PUT55                                                            
PUT03    CLI   QOPT1,C'1'                PROFIT CENTER LEVEL                    
         BNE   PUT55                                                            
         MVC   SRTDIVIS,SPACES                                                  
         MVC   SRTCLT,SPACES                                                    
         MVC   SRTPROD,SPACES                                                   
*                                                                               
PUT55    CLI   OPTION2,C' '              EMPLOYEE LEVEL REPORT                  
         BE    PUT99                     YES - PUT TO SORT                      
*                                                                               
         CLI   OPTION2,C'3'              1R CATEGORY LEVEL REPORT               
         BNE   *+14                      NO - CHECK NEXT LEVEL                  
         MVC   SRTSTAF,SPACES            YES - CLEAR STAFF CODE                 
         B     PUT99                     PUT TO SORT                            
*                                                                               
         CLI   OPTION2,C'2'              1R DEPT LEVEL REPORT                   
         BNE   *+14                      NO - CHECK NEXT LEVEL                  
         MVC   SRTCAT(9),SPACES          YES - CLEAR CAT/STAFF CODE             
         B     PUT99                     PUT TO SORT                            
*                                                                               
         CLI   OPTION2,C'E'              1R EMP,DEPT/CAT SUPPRESSED             
         BNE   *+14                      NO - CHECK NEXT LEVEL                  
         MVC   SRTDEPT(4),SPACES         YES - CLEAR DEPT/CAT CODE              
         B     PUT99                     PUT TO SORT                            
*                                                                               
         CLI   OPTION2,C'1'             11R OFFICE LEVEL REPORT                 
         BNE   *+10                      NO - CHECK NEXT LEVEL                  
         MVC   SRTDEPT(11),SPACES        YES - CLEAR DEPT/CAT/STAFF             
*                                                                               
PUT99    GOTO1 SORTER,DMCB,=C'PUT',(R6)                                         
         MVI   ALSORT,1                  ACTIVITY SWITCH                        
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*------------------------------------------------------                         
*        ADD ITEM TO BINSRCH TABLE                                              
*------------------------------------------------------                         
*                                                                               
*              P1                  A(ITEM TO BE ADDED)                          
*              P2                  A(TABLE)                                     
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ          NUMBER LENGTH,KEY,MAX                  
         LA    R6,BINTABLE               A(TABLE)                               
         L     R4,0(R1)                  A(ITEM)                                
         GOTO1 BINSRCH,DMCB,(X'01',(R4)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                      TABLE IS FULL                          
         MVC   BININ,DMCB+8              UPDATE COUNT                           
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*------------------------------------------------------                         
*        CLEAR PRINTBLOCK                                                       
*------------------------------------------------------                         
*                                                                               
CLEARBLK NTR1                                                                   
         LA    R1,PRNTBLOC                                                      
         LA    RE,2                                                             
         MVC   0(165,R1),XSPACES                                                
         LA    R1,165(R1)                                                       
         BCT   RE,*-10                                                          
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------                         
*        PUT NAME IN LEFT COLUMN                                                
*------------------------------------------------------                         
*                                                                               
PUTNAME  NTR1                                                                   
         MVC   WKAREA(60),WORK                                                  
         GOTO1 SQUASHER,DMCB,WKAREA,50                                          
         MVC   WORK,SPACES                                                      
         GOTO1 CHOPPER,DMCB,(60,WKAREA),(30,WORK),(30,2)                        
         MVC   PRNTBLOC+1(30),WORK                                              
         MVC   PRNTBLOC+173(21),WORK+30                                         
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------                         
*        PRINT OUT RETAINERS / COMMISION INTERNAL ART ROWS                      
*------------------------------------------------------                         
*                                                                               
ROWLINE  NTR1                                                                   
         CLI   QOPT1,C'S'                CLIENT SUMMARY?                        
         BE    XIT                                                              
         CP    RETAIN,=P'0'              DONT PRINT OUT ZERO TOTALS             
         BE    ROWL2                                                            
         CLI   RETSW,0                   NO ROW IF  NO RET COLUMN               
         BE    ROWL2                                                            
         BAS   RE,CLEARBLK               RETAINERS                              
         MVC   PRNTBLOC+1(9),=C'RETAINERS'                                      
         EDIT  (P7,RETAIN),(12,BLAMNT),2,MINUS=YES                              
         EDIT  (P7,RETAIN),(12,RTAMNT),2,MINUS=YES                              
         ZAP   RETAIN,=P'0'                                                     
         BAS   RE,BLDBLC                 FORMAT PRINT LINE                      
         OC    PRNTBLOC(165),XSPACES                                            
         MVC   XP(165),PRNTBLOC                                                 
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
ROWL2    CP    COMART,=P'0'              DONT PRINT OUT ZERO TOTALS             
         BE    ROWLX                                                            
         BAS   RE,CLEARBLK               PRINT OUT EMPLOYEE RETAINERS           
         MVC   PRNTBLOC+1(23),=C'COMMISSION/INTERNAL ART'                       
         EDIT  (P7,COMART),(12,BLAMNT),2,MINUS=YES                              
         EDIT  (P7,COMART),(12,CMAMNT),2,MINUS=YES                              
         ZAP   COMART,=P'0'                                                     
         BAS   RE,BLDBLC                 FORMAT PRINT LINE                      
         OC    PRNTBLOC(165),XSPACES                                            
         MVC   XP(165),PRNTBLOC                                                 
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
ROWLX    BAS   RE,CLEARBLK                                                      
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------                         
*        CLEAR SORT RECORD AREA                                                 
*------------------------------------------------------                         
*                                                                               
CLERSORT NTR1                                                                   
         USING SRTD,R6                                                          
         LA    R6,SRTWRK                                                        
         MVC   SRTWRK(SRTLNQ),XSPACES                                           
         LR    R1,R6                     ADDR OF SORT REC INTO R1               
         LA    R1,SBUKLOC(R1)            START OF COL BUCKETS                   
         LA    R0,SBUKCONT               NUMBER OF BUCKETS INTO R0              
*                                                                               
CLER03   ZAP   0(7,R1),=P'0'             CLEAR TO PACKED ZEROS                  
         LA    R1,7(R1)                  BUMP TO NEXT BUCKET                    
         BCT   R0,CLER03                                                        
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        GET SPACE FOR THE NAME TABLES                                          
*----------------------------------------------------------------------         
GETBUFF  NTR1                                                                   
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         GETMAIN R,LV=(0)                                                       
         LA    R0,MAINNUM                                                       
         LR    R5,R1               R5 IS BUFFER POINTER                         
         ST    R1,ABUFF                                                         
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         STCM  R5,15,MCUSRDMP                                                   
         LR    RF,R5                                                            
         A     RF,=A(BUFSIZE)                                                   
         STCM  RF,15,MCUSRDMP+4                                                 
*                                                                               
         USING BIND,R5                                                          
         L     R2,=A(MAINTAB)                                                   
         USING MAIND,R2                                                         
*                                                                               
GETB10   MVC   *+8(2),MAINAST     SCON OF WHERE TO STORE BUFF LOCATION          
         ST    R5,FULL             FULL IS A DUMMY FOR THE ASSEMBLER            
         XC    BININ,BININ                                                      
         MVC   BINLEN,MAINLEN                                                   
         MVC   BINDISP,=F'0'                                                    
         MVC   BINKEY+2(1),MAINKLEN                                             
         MVC   BINMAX,MAINMAX                                                   
         A     R5,MAINSIZE                                                      
*                                                                               
         LA    R2,MAINLN(R2)                                                    
         BCT   R0,GETB10                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        RELEASE GETMAINED SPACE                                                
*----------------------------------------------------------------------         
RELBUFF  NTR1                                                                   
         L     R1,ABUFF                                                         
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         XC    MCUSRDMP,MCUSRDMP   CLEAR XTRA DUMP ADDRESS                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------                        
*        DATAMGR INTERFACE                                                      
*-------------------------------------------------------                        
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'        READ HIGH                              
         MVC   SAVEKEY,0(R7)                                                    
         B     GTREC                                                            
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'        READ SEQUENTIAL                        
         MVC   SAVEKEY,0(R7)                                                    
         B     GTREC                                                            
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'        A SPECIFIC READ                        
*                                                                               
GTREC    NTR1                                                                   
         L     R7,ACREC                                                         
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R7),(R7)                       
         CLI   DMCB+8,0                  TEST FOR ERRORS                        
         BE    *+6                                                              
         DC    H'0'                      DIE IF ERRORS FOUND                    
         B     XIT                                                              
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*-------------------------------------------------------                        
*        CONSTANTS                                                              
*-------------------------------------------------------                        
*                                                                               
RELOTAB  DS    0A                                                               
         DC    A(RECORD)                                                        
         DC    A(BUDWK)                                                         
         DC    V(SORTER)                                                        
         DC    V(CONVMOS)                                                       
         DC    V(UNDERLIN)                                                      
         DC    V(BUDACC)                                                        
         DC    V(SQUASHER)                                                      
         DC    X'FF'                                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
         EJECT                                                                  
*-------------------------------------------------------                        
*        PRINT COLUMN EQUATES                                                   
*-------------------------------------------------------                        
*                                                                               
*        POSSIBLE COLUMN COMBINATIONS AVAILABLE                                 
*                                                                               
BUD      EQU   1                   BUDGET COLUMN                                
BVAL     EQU   2                   BILLING VALUE COLUMN                         
TBLD     EQU   3                   TIME BILLED COLUMN                           
RET      EQU   4                   RETAINERS COLUMN                             
CMS      EQU   5                   COMMISSION INTERNAL ART COLUMN               
TOTB     EQU   6                   TOTAL BILLED COLUMN                          
WRTO     EQU   7                   TOTAL WRITE OFFS COLUMN                      
VALP     EQU   8                   VALUE VS BILLED PERCENT                      
BUDP     EQU   9                   BUDGET VS BILLED PERCENT                     
VALB     EQU   10                  VALUE VS BILLED COLUMN                       
BUDB     EQU   11                  BUDGET VS BILLED COLUMN                      
TIHD     EQU   12                  TIME HELD                                    
NOPRINT  EQU   88                  DONT PRINT COLUMN + DONT BUMP                
BLANK    EQU   99                  LEAVE COLUMN BLANK                           
         EJECT                                                                  
*-------------------------------------------------------                        
*        PRINT COLUMN TABLE - CONVERTS WHAT OPT3 INPUT MEANS                    
*-------------------------------------------------------                        
*                                                                               
*        THIS TABLE CONTAINS WHAT THE COLUMNS OF THE PRINTED                    
*        REPORT WILL BE FOR EACH OPTION3 INPUT.                                 
*        EACH ENTRY LINE SHOWS WHAT WILL PRINT ON THE 2 AVAIL LINES             
*        1ST BYTE OF EACH LINE IS THE INPUT IN QOPT3.                           
*                                                                               
*        BLANK = NO RETAINTERS / NO WRITEOFFS                                   
*        1     = JUST RETAINERS                                                 
*        2     = JUST WRITEOFFS                                                 
*        3     = BOTH RETAINERS AND WRITEOFFS                                   
*                                                                               
OPTION3  DC    C' '          IF OPTION3 IS LEFT BLANK                           
         DC    AL1(BUD),AL1(BVAL),AL1(TBLD),AL1(CMS),AL1(TOTB)                  
         DC    AL1(VALP),AL1(BUDP),AL1(NOPRINT),AL1(NOPRINT),X'FF'              
         DC    AL1(BLANK),AL1(BLANK),AL1(BLANK),AL1(BLANK),AL1(BLANK)           
         DC    AL1(VALB),AL1(BUDB),AL1(NOPRINT),AL1(NOPRINT),X'FF'              
*                                                                               
         DC    C'1'          INCLUDE RETAINERS                                  
         DC    AL1(BUD),AL1(BVAL),AL1(TBLD),AL1(RET),AL1(CMS)                   
         DC    AL1(TOTB),AL1(VALP),AL1(BUDP),AL1(NOPRINT),X'FF'                 
         DC    AL1(BLANK),AL1(BLANK),AL1(BLANK),AL1(BLANK),AL1(BLANK)           
         DC    AL1(BLANK),AL1(VALB),AL1(BUDB),AL1(NOPRINT),X'FF'                
*                                                                               
         DC    C'2'           INCLUDE WRITEOFFS                                 
         DC    AL1(BUD),AL1(BVAL),AL1(TBLD),AL1(CMS),AL1(TOTB)                  
         DC    AL1(WRTO),AL1(VALP),AL1(BUDP),AL1(NOPRINT),X'FF'                 
         DC    AL1(BLANK),AL1(BLANK),AL1(BLANK),AL1(BLANK),AL1(BLANK)           
         DC    AL1(TIHD),AL1(VALB),AL1(BUDB),AL1(NOPRINT),X'FF'                 
*                                                                               
         DC    C'3'           INCLUDE WRITEOFFS AND RETAINERS                   
         DC    AL1(BUD),AL1(BVAL),AL1(TBLD),AL1(RET),AL1(CMS)                   
         DC    AL1(TOTB),AL1(WRTO),AL1(VALP),AL1(BUDP),X'FF'                    
         DC    AL1(BLANK),AL1(BLANK),AL1(BLANK),AL1(BLANK),AL1(BLANK)           
         DC    AL1(BLANK),AL1(TIHD),AL1(VALB),AL1(BUDB),X'FF'                   
*                                                                               
OPTLNQ   EQU   (*-OPTION3)/21                                                   
         EJECT                                                                  
*-------------------------------------------------------                        
*        THIS TABLE CONTAINS ALL DATA FOR A SPECIFIC COLUMN                     
*-------------------------------------------------------                        
*                                                                               
*        PARAMETER 1 = COLUMN TO PROCESS                                        
*        PARAMETER 2 = OFFSET OF COLUMN AMOUNT IS LOCATED IN AMNTTBLE           
*        PARAMETER 3 = LENGTH OF THE OUTPUT FIELD                               
*        PARAMETER 4 = SPARE                                                    
*        PARAMETER 5 = ADDRESS OF THE COLUMN HEADING NAME                       
*                                                                               
         DS    0F                                                               
COLUMN   DC    AL1(BUD),AL1(0),AL1(12),AL1(0),A(BDNAME)                         
         DC    AL1(BVAL),AL1(1),AL1(12),AL1(0),A(BVNAME)                        
         DC    AL1(TBLD),AL1(2),AL1(12),AL1(0),A(TBNAME)                        
         DC    AL1(RET),AL1(3),AL1(12),AL1(0),A(RTNAME)                         
         DC    AL1(CMS),AL1(4),AL1(12),AL1(0),A(CMNAME)                         
         DC    AL1(TOTB),AL1(5),AL1(12),AL1(0),A(BLNAME)                        
         DC    AL1(WRTO),AL1(6),AL1(12),AL1(0),A(WONAME)                        
         DC    AL1(VALP),AL1(9),AL1(12),AL1(0),A(VBNAME)                        
         DC    AL1(BUDP),AL1(10),AL1(12),AL1(0),A(BBNAME)                       
         DC    AL1(TIHD),AL1(11),AL1(12),AL1(0),A(WONAME)                       
         DC    AL1(VALB),AL1(7),AL1(12),AL1(0),A(VBNAME)                        
         DC    AL1(BUDB),AL1(8),AL1(12),AL1(0),A(BBNAME)                        
COLEND   EQU   (*-COLUMN)/8                                                     
         EJECT                                                                  
*-------------------------------------------------------                        
*        HEADINGS FOR PRINT COLUMNS                                             
*-------------------------------------------------------                        
*                                                                               
BDNAME   DC    CL12'   BUDGET   '  HEADING FOR BUDGET COLUMN                    
         DC    CL12'            '                                               
         DC    CL12'            '                                               
BVNAME   DC    CL12'   BILLING  '  HEADING FOR BILLING VALUE COLUMN             
         DC    CL12'    VALUE   '                                               
         DC    CL12'            '                                               
TBNAME   DC    CL12'    TIME    '  HEADING FOR TIME BILLED COLUMN               
         DC    CL12'   BILLED   '                                               
         DC    CL12'            '                                               
RTNAME   DC    CL12'  RETAINERS '  HEADING FOR RETAINER COLUMN                  
         DC    CL12'            '                                               
         DC    CL12'            '                                               
CMNAME   DC    CL12' COMMISSION '  HEADING FOR COMMISSION COLUMN                
         DC    CL12'INTERNAL ART'                                               
         DC    CL12'   BILLED   '                                               
BLNAME   DC    CL12'   TOTAL    '  HEADING FOR TOTAL BILLED                     
         DC    CL12'   BILLED   '                                               
         DC    CL12'            '                                               
WONAME   DC    CL12' WRITE-OFFS '  HEADING FOR TOTAL WRITEOFFS                  
         DC    CL12'    AND     '                                               
         DC    CL12' TIME HELD  '                                               
VBNAME   DC    CL12'  VALUE VS  '  NOTE: THE PERCENT ENTRY IN THE               
         DC    CL12'   BILLED   '        ABOVE TABLE POINTS TO THE              
         DC    CL12'  % OVER $  '        SAME HEADING SINCE THE                 
BBNAME   DC    CL12'  BUDGET VS '        PERCENT AND THE DOLLARS ARE            
         DC    CL12'   BILLED   '        STACKED.                               
         DC    CL12'  % OVER $  '                                               
         EJECT                                                                  
*-------------------------------------------------------                        
*        LITERAL DECLARATIONS                                                   
*-------------------------------------------------------                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        MAINTAB IS TABLE OF HOW GETMAIN CORE SHOULD BE SPLIT UP                
*--------------------------------------------------------------------*          
MAINTAB  DS    0F                                                               
         DC    S(ACODLST)                                                       
         DC    AL3(0),AL1(CLILEN)                                               
         DC    AL1(L'CLIKEY)                                                    
         DC    A(CODMAX)                                                        
         DC    A(CODSIZE)                                                       
*                                                                               
         DC    S(AOFFLST)                                                       
         DC    AL3(0),AL1(OFFLEN)                                               
         DC    AL1(L'OFFKY)                                                     
         DC    AL1(0)                                                           
         DC    A(OFFMAX)                                                        
         DC    A(OFFSIZE)                                                       
*                                                                               
         DC    S(ADPTLST)                                                       
         DC    AL3(0),AL1(DPTLEN)                                               
         DC    AL1(L'DPTKY)                                                     
         DC    AL1(0)                                                           
         DC    A(DPTMAX)                                                        
         DC    A(DPTSIZE)                                                       
*                                                                               
         DC    S(ACATLST)                                                       
         DC    AL3(0),AL1(CATLEN)                                               
         DC    AL1(L'CATKY)                                                     
         DC    AL1(0)                                                           
         DC    A(CATMAX)                                                        
         DC    A(CATSIZE)                                                       
*                                                                               
*                                                                               
MAINNUM  EQU   (*-MAINTAB)/MAINLN                                               
CODMAX   EQU   20000                                                            
CODSIZE  EQU   BINLENQ+(CODMAX*CLILEN)                                          
*                                                                               
OFFMAX   EQU   100                                                              
OFFSIZE  EQU   BINLENQ+(OFFMAX*OFFLEN)                                          
*                                                                               
DPTMAX   EQU   500                                                              
DPTSIZE  EQU   BINLENQ+(DPTMAX*DPTLEN)                                          
*                                                                               
CATMAX   EQU   1000                                                             
CATSIZE  EQU   BINLENQ+(CATMAX*CATLEN)                                          
*                                                                               
BUFSIZE  EQU   CODSIZE+OFFSIZE+DPTSIZE+CATSIZE                                  
         EJECT                                                                  
*-------------------------------------------------------                        
*        BOX HOOK                                                               
*-------------------------------------------------------                        
*                                                                               
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC                  RESTORE REG C                          
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXCOLS(198),XSPACES                                             
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+8,C'T'            SET ROWS                               
         MVI   BOXROWS+12,C'M'                                                  
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS,C'L'              SET LH MARGIN                          
         ZIC   R2,NUMCOLS                                                       
         LA    R1,BOXCOLS+32                                                    
         LA    R0,10                                                            
         CR    R2,R0                                                            
         BL    BOXIT                                                            
         LA    R2,9                      CANT HAVE MORE THAN 9 COLS             
BOXIT    MVI   0(R1),C'C'                                                       
         LA    R1,14(R1)                                                        
         BCT   R2,BOXIT                                                         
         MVI   0(R1),C'R'                                                       
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------                        
*        STORAGE AREA                                                           
*-------------------------------------------------------                        
*                                                                               
ACR1D    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
ACREC    DS    A                                                                
ABUDWK   DS    A                                                                
SORTER   DS    V                                                                
CONVMOS  DS    V                                                                
UNDERLIN DS    V                                                                
BUDACC   DS    V                                                                
SQUASHER DS    V                                                                
*                                                                               
ACODLST  DS    A                   ADCONS SET BY GETMAIN                        
AOFFLST  DS    A                                                                
ADPTLST  DS    A                                                                
ACATLST  DS    A                                                                
*                                                                               
ABUFF    DS    A                                                                
ADBOX    DS    A                                                                
NUMNON   DS    CL1                                                              
OPTION2  DS    CL1                                                              
COMMAND  DS    CL6                                                              
ELCODE   DS    CL1                                                              
START    DS    CL3                 START DATE PACKED YMD                        
ENDATE   DS    CL3                 END DATE PACKED YMD                          
BILDATE  DS    CL3                 BILLING DATE PACKED YMD                      
MOS      DS    CL2                 MONTH OF SERVICE FROM SJ TRANS               
M1START  DS    PL2                 MOS START DATE PACKED                        
M1END    DS    PL2                 MOS END DATE PACKED                          
COMPNAM  DS    CL36                COMPANY NAME FOR HEADLINES                   
BILLTHRU DS    CL38                HEADLINE TO PRINT MOA RANGE                  
*                                                                               
CLICOST  DS    CL15                CLIENT COSTING ACCOUNT                       
PRDCOST  DS    CL15                PRODUCT COSTING ACCOUNT                      
JOBCOST  DS    CL15                JOB COSTING ACCOUNT                          
ACURCST  DS    A                   A(CURRENT COST ENTRY)                        
*                                                                               
SAVKEY   DS    0CL32               THE SAVED SJ ACCOUNT CODE                    
COMPANY  DS    XL1                 COMPANY                                      
UNIT     DS    CL1                 UNIT S                                       
LEDGER   DS    CL1                 LEDGER J                                     
CLIENT   DS    CL3                 SJ CLIENT                                    
PRODUCT  DS    CL3                 SJ PRODUCT                                   
JOB      DS    CL6                 SJ JOB                                       
WORKCD   DS    CL2                 WORKCODE                                     
EMPLOYEE DS    0CL15               1R EMPLOYEE                                  
         DS    CL3                 CO/U/L                                       
EMPCODE  DS    0CL12                                                            
EMPOFFC  DS    CL1                 -OFFICE                                      
EMPDEPT  DS    CL2                 -DEPARTMENT                                  
EMPCAT   DS    CL2                 -SUB-DEPT (CATGORY)                          
EMPSTAF  DS    CL7                 -STAFF NUMBER                                
COSTING  DS    0CL15               THE SAVED COSTING ACCOUNT                    
         DS    CL3                 CO/U/L                                       
COSTCODE DS    CL12                                                             
COSTPC   DS    CL12                PROFIT CENTER                                
COSTDIV  DS    CL12                DIVISION                                     
COSTCLT  DS    CL12                CLIENT                                       
COSTPROD DS    CL12                PRODUCT                                      
ANSWER   DS    PL14                FOR DIVIDE                                   
BUDGETSV DS    PL7                 SAVED BUDGET AMOUNT                          
INCOME   DS    PL7                 SAVED INCOME AMOUNT                          
NOTIMESH DS    CL1                 Y, TIME SHEET IS REJECTED                    
BLDVAL   DS    CL6                 BILLED VAL OF A TIME SHEET                   
SAVEKEY  DS    CL42                                                             
TEMPKEY  DS    CL42                                                             
READKEY  DS    CL42                                                             
*                                                                               
CNAMES   DS    0CL144                                                           
PCNAME   DS    CL36                1C PROFIT CENTER NAME                        
DIVNAME  DS    CL36                1C DIVISION NAME                             
CLTNAME  DS    CL36                1C CLIENT NAME                               
PRDNAME  DS    CL36                1C PRODUCT NAME                              
*                                                                               
RNAMES   DS    0CL108                                                           
OFNAMESV DS    CL36                1R OFFICE NAME                               
DPNAMESV DS    CL36                1R DEPT NAME                                 
CATNAMSV DS    CL36                1R SUB-DEPT NAME                             
*                                                                               
ALSORT   DS    A                   A(LAST SORT RECORD)                          
SRTWRK   DS    (SRTLNQ)C           WORK AREA FOR SORT RECORD                    
LSTWRK   DS    (SRTLNQ)C           WORK AREA FOR LAST RECORD                    
*                                                                               
LEVIND   DS    XL1                                                              
TOTSW    DS    CL1                 LEVEL OF TOTAL SWITCH                        
DETAILSW DS    CL1                 'TOTAL' WILL PRINT AS DETAIL LINE            
RETSW    DS    CL1                 RETAINER SWITCH TO PRINT RET ROW             
*                                  -AND ADD RETAINERS TO TOT BILLED             
PK7      DS    PL7                                                              
*                                                                               
ACCUMS   DS    0C                                                               
*                                  OVERALL REPORT TOTAL                         
RPTOT    DS    0C                                                               
RPBUD    DS    PL7                 REVENUE BUDGET YTD (1C 12 CONTRA)            
RPBLVAL  DS    PL7                 BILLING VALUE  (SJ CONTRA 1R)                
RPBILLD  DS    PL7                 FEE'S BILLED   (SJ CONTRA 1R)                
RPINCOM  DS    PL7                 COMM+INTERN ART BILLED(1C 125/128)           
RPWO     DS    PL7                                                              
RPRET    DS    PL7                                                              
TOTLEN   EQU   *-ACCUMS                                                         
COUNT    EQU   (*-ACCUMS)/7        NUMBER OF ACCUMS IN LEVEL                    
*                                  (1C) PROFIT CENTER ACCUMS                    
PFTOT    DS    0C                                                               
PFBUD    DS    PL7                 REVENUE BUDGET YTD (1C 12 CONTRA)            
PFBLVAL  DS    PL7                 BILLING VALUE  (SJ CONTRA 1R)                
PFBILLD  DS    PL7                 FEE'S BILLED   (SJ CONTRA 1R)                
PFINCOM  DS    PL7                 COMM+INTERN ART BILLED(1C 125/128)           
PFWO     DS    PL7                                                              
PFRET    DS    PL7                                                              
*                                  (1C) DIVISION ACCUMS                         
DVTOT    DS    0C                                                               
DVBUD    DS    PL7                 REVENUE BUDGET YTD (1C 12 CONTRA)            
DVBLVAL  DS    PL7                 BILLING VALUE  (SJ CONTRA 1R)                
DVBILLD  DS    PL7                 FEE'S BILLED   (SJ CONTRA 1R)                
DVINCOM  DS    PL7                 COMM+INTERN ART BILLED(1C 125/128)           
DVWO     DS    PL7                                                              
DVRET    DS    PL7                                                              
*                                  (1C) CLIENT ACCUMS                           
CLTOT    DS    0C                                                               
CLBUD    DS    PL7                 REVENUE BUDGET YTD (1C 12 CONTRA)            
CLBLVAL  DS    PL7                 BILLING VALUE  (SJ CONTRA 1R)                
CLBILLD  DS    PL7                 FEE'S BILLED   (SJ CONTRA 1R)                
CLINCOM  DS    PL7                 COMM+INTERN ART BILLED(1C 125/128)           
CLWO     DS    PL7                                                              
CLRET    DS    PL7                                                              
*                                  (1C) PRODUCT ACCUMS                          
PDTOT    DS    0C                                                               
PDBUD    DS    PL7                 REVENUE BUDGET YTD (1C 12 CONTRA)            
PDBLVAL  DS    PL7                 BILLING VALUE  (SJ CONTRA 1R)                
PDBILLD  DS    PL7                 FEE'S BILLED   (SJ CONTRA 1R)                
PDINCOM  DS    PL7                 COMM+INTERN ART BILLED(1C 125/128)           
PDWO     DS    PL7                                                              
PDRET    DS    PL7                                                              
*                                  (1R) OFFICE ACCUMS                           
OFTOT    DS    0C                                                               
OFBUD    DS    PL7                 REVENUE BUDGET YTD (1C 12 CONTRA)            
OFBLVAL  DS    PL7                 BILLING VALUE  (SJ CONTRA 1R)                
OFBILLD  DS    PL7                 FEE'S BILLED   (SJ CONTRA 1R)                
OFINCOM  DS    PL7                 COMM+INTERN ART BILLED(1C 125/128)           
OFWO     DS    PL7                                                              
OFRET    DS    PL7                                                              
*                                  (1R) DEPT ACCUMS                             
DPTTOT   DS    0C                                                               
DTBUD    DS    PL7                 REVENUE BUDGET YTD (1C 12 CONTRA)            
DTBLVAL  DS    PL7                 BILLING VALUE  (SJ CONTRA 1R)                
DTBILLD  DS    PL7                 FEE'S BILLED   (SJ CONTRA 1R)                
DTINCOM  DS    PL7                 COMM+INTERN ART BILLED(1C 125/128)           
DTWO     DS    PL7                                                              
DTRET    DS    PL7                                                              
*                                  (1R) CAT ACCUMS                              
CTTOT    DS    0C                                                               
CTBUD    DS    PL7                 REVENUE BUDGET YTD (1C 12 CONTRA)            
CTBLVAL  DS    PL7                 BILLING VALUE  (SJ CONTRA 1R)                
CTBILLD  DS    PL7                 FEE'S BILLED   (SJ CONTRA 1R)                
CTINCOM  DS    PL7                 COMM+INTERN ART BILLED(1C 125/128)           
CTWO     DS    PL7                                                              
CTRET    DS    PL7                                                              
BUKCOUNT EQU   (*-ACCUMS)/7        NUMBER OF ACCUMS                             
BUKCONT1 EQU   (*-CTTOT)/7         CLEAR ACCUMS WHEN CAT CHANGES                
BUKCONT2 EQU   (*-DPTTOT)/7                          DPT                        
BUKCONT3 EQU   (*-OFTOT)/7                           OFF                        
BUKCONT4 EQU   (*-PDTOT)/7                           PRD                        
BUKCONT5 EQU   (*-CLTOT)/7                           CLT                        
BUKCONT6 EQU   (*-DVTOT)/7                           DIV                        
BUKCONT7 EQU   (*-PFTOT)/7                           PF                         
LEVCOUNT EQU   (*-ACCUMS)/42       NUMBER OF LEVEL TOTALS                       
*                                                                               
AMNTTBLE DS    0CL144              TABLE OF AMOUNTS                             
BDAMNT   DS    CL12                BUDGET                                       
BVAMNT   DS    CL12                BILLING VALUE                                
TBAMNT   DS    CL12                TIME BILLED                                  
RTAMNT   DS    CL12                RETAINERS                                    
CMAMNT   DS    CL12                COMMISSION                                   
BLAMNT   DS    CL12                TOTAL BILLED                                 
WOAMNT   DS    CL12                WRITEOFFS                                    
VBAMNT   DS    CL12                VALUE VS BILLED                              
BBAMNT   DS    CL12                BUDGET VS BILLED                             
VPAMNT   DS    CL12                VALUE VS BILLED PERCENT                      
BPAMNT   DS    CL12                BUDGET VS BILLED PERCENT                     
THAMNT   DS    CL12                TIME HELD                                    
AMNTLENQ EQU   *-AMNTTBLE          LENGTH OF AMOUNT TABLE                       
*                                                                               
ANAME    DS    CL15                1C LEVEL NAMES                               
BNAME    DS    CL15                                                             
CNAME    DS    CL15                                                             
DNAME    DS    CL15                                                             
CLILEV   DS    XL1                                                              
CLSUM    DS    XL1                 SET TO 1 IF CLIENT SUMMARY AND               
*                                  CLIENT LEVEL IS NOT ON 2ND LEVEL             
LEVEL    DS    0XL4                                                             
LEVELA   DS    XL1                 LEVEL STRUCTURE OF 1C                        
LEVELB   DS    XL1                 EX/ 1,2,5,12                                 
LEVELC   DS    XL1                                                              
LEVELD   DS    XL1                                                              
*                                                                               
LENGTHS  DS    0XL4                                                             
ALENGTH  DS    XL1                 TRUE NUMBER STRUCTURE                        
BLENGTH  DS    XL1                 EX/ 1,1,3,7                                  
CLENGTH  DS    XL1                                                              
DLENGTH  DS    XL1                                                              
*                                                                               
TEMPCODE DS    CL12                1C ACCOUNT      EX/ 12333777                 
TEMPPC   DS    CL12                PROFIT CENTER       1                        
TEMPDIV  DS    CL12                DIVISION            2                        
TEMPCLT  DS    CL12                CLIENT              333                      
TEMPPROD DS    CL12                PRODUCT             777                      
*                                                                               
CNTNUM   DS    XL1                                                              
NUMCOLS  DS    XL1                 NUMBER OF COLUMNS IN REPORT                  
RETAIN   DS    PL7                 RETAINERS                                    
COMART   DS    PL7                 COMMISSION INTERNAL ART ROW ACCUM            
TIMEHELD DS    PL7                 TIME HELD = BIL VAL+WRITEOFFS-TIME           
WKAREA   DS    CL60                TEMP WORK AREA                               
HEADBLOC DS    3CL130              CONTAINS COLUMN HEADINGS                     
PRNTBLOC DS    2CL165              PRINT AREA                                   
         EJECT                                                                  
*-------------------------------------------------------                        
*        DSECT FOR SORT RECORD                                                  
*-------------------------------------------------------                        
*                                                                               
SRTD     DSECT                                                                  
SRTKEY   DS    0C                                                               
SRTCODE  DS    CL12                1C CLIENT CODE                               
SRTPROFT DS    CL12                PROFIT CENTER  (1C)                          
SRTDIVIS DS    CL12                DIVISION       (1C)                          
SRTCLT   DS    CL12                CLIENT         (1C)                          
SRTPROD  DS    CL12                PRODUCT        (1C)                          
SRTKLNQ  EQU   *-SRTKEY            SORT KEY LENGTH                              
SRTEMPL  DS    0CL12               EMPLOYEE'S (1R CONTRA OF THE SJ JOB)         
SRTOFFC  DS    CL1                          -OFFICE                             
SRTDEPT  DS    CL2                          -DEPARTMENT                         
SRTCAT   DS    CL2                          -CATEGORY                           
SRTSTAF  DS    CL7                          -STAFF NUMBER                       
SRTDLNQ  EQU   *-SRTKEY            RECORD DISCRIPTION LENGTH                    
SRTCNAME DS    CL36                1C PRODUCT LEVEL NAME                        
SRTENAME DS    CL36                1R EMPLOYEE NAME                             
SBUKLOC  EQU   *-SRTD              LOCATION OF BUCKETS                          
SRTBUD   DS    PL7                 REVENUE BUDGET YTD (1C CONTRA 12)            
SRTBLVAL DS    PL7                 BILLING VALUE  (SJ CONTRA 1R)                
SRTBILLD DS    PL7                 FEE'S BILLED   (SJ CONTRA 1R)                
SRTINCOM DS    PL7                 COMM+INTERN ART BILLED(1C 125/128)           
SRTBLTOT DS    PL7                 TOTAL BILLED                                 
SRTVLDOL DS    PL7                 VALUE VS BILLED (DOLLARS)                    
SRTVLPCT DS    PL7                 VALUE VS BILLED (PERCENT)                    
SRTBDDOL DS    PL7                 BUDGET VS BILLED (DOLLARS)                   
SRTBDPCT DS    PL7                 BUDGET VS BILLED (PERCENT)                   
SRTWO    DS    PL7                 WRITEOFFS                                    
SRTRET   DS    PL7                 RETAINERS                                    
SBUKCONT EQU   (*-SRTBUD)/7        NUMBER OF BUCKETS                            
SRTLNQ   EQU   *-SRTKEY            RECORD LENGTH                                
         EJECT                                                                  
*-------------------------------------------------------                        
*        DSECT FOR COLUMN TABLE                                                 
*-------------------------------------------------------                        
*                                                                               
COLUMND  DSECT                                                                  
COLNUM   DS    CL1                                                              
COLOFF   DS    CL1                 OFFSET OF COLUMN                             
COLLEN   DS    CL1                 COL LENGTH                                   
         DS    CL1                 SPARE                                        
COLNAME  DS    F                   A(COLUMN HEADER)                             
*                                                                               
*-------------------------------------------------------                        
*        DSECT FOR BINSRCH PARAMETERS                                           
*-------------------------------------------------------                        
*                                                                               
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINTABLE DS    0CL1                                                             
BINLENQ  EQU   *-BIND              BINSEARCH TABLE HEADER LENGTH                
         EJECT                                                                  
*-------------------------------------------------------                        
*        DSECT FOR CODE LIST TABLE                                              
*-------------------------------------------------------                        
*                                                                               
CLICDE   DSECT                                                                  
CLIKEY   DS    CL15                COSTING CODE                                 
CLINAME  DS    CL36                COST NAME                                    
CLIACTV  EQU   X'80'               ACTIVE FOR THIS REQUEST                      
CLILEN   EQU   *-CLIKEY                                                         
*                                                                               
*-------------------------------------------------------                        
*        DSECT FOR 1R OFFICE LIST TABLE                                         
*-------------------------------------------------------                        
*                                                                               
OFFCDE   DSECT                                                                  
OFFKY    DS    CL1                                                              
OFFNAME  DS    CL36                                                             
OFFLEN   EQU   *-OFFKY                                                          
*                                                                               
*-------------------------------------------------------                        
*        DSECT FOR 1R DEPT LIST TABLE                                           
*-------------------------------------------------------                        
*                                                                               
DPTCDE   DSECT                                                                  
DPTKY    DS    CL3                                                              
DPTNAME  DS    CL36                                                             
DPTLEN   EQU   *-DPTKY                                                          
*                                                                               
*-------------------------------------------------------                        
*        DSECT FOR 1R CATEGORY LIST TABLE                                       
*-------------------------------------------------------                        
*                                                                               
CATCDE   DSECT                                                                  
CATKY    DS    CL5                                                              
CATNAME  DS    CL36                                                             
CATLEN   EQU   *-CATKY                                                          
*                                                                               
*-------------------------------------------------------                        
*        DSECT FOR TOTAL LINE ACCUMS                                            
*-------------------------------------------------------                        
*                                                                               
TOTALD   DSECT                                                                  
TOTBUD   DS    PL7                 TOTAL FOR BUDGET                             
TOTBLVAL DS    PL7                 BILLING VALUE                                
TOTBILLD DS    PL7                 TIME BILLED                                  
TOTINCOM DS    PL7                 INCOME                                       
TOTWO    DS    PL7                 WRITEOFFS                                    
TOTRET   DS    PL7                 RETAINERS                                    
         EJECT                                                                  
*-------------------------------------------------------                        
*        ENTRY RECORDS                                                          
*-------------------------------------------------------                        
*                                                                               
ACR102   CSECT                                                                  
         ENTRY RECORD                                                           
RECORD   DS    0D                                                               
         DS    CL42                                                             
         DS    CL1000                                                           
*                                                                               
         ENTRY BUDWK                                                            
BUDWK    DS    0D                                                               
         DC    1500X'00'                                                        
*                                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
* DSECT FOR MAIN TAB, A TABLE WHICH LOOPS THRU THE STORAGE GETMAIN              
*        GETS                                                                   
*-------------------------------------------------------------------*           
MAIND    DSECT                                                                  
MAINAST  DS    S                   ADDRESS TO STORE A(TABLE)                    
MAINLEN  DS    AL4                 LENGTH OF BINSRCH RECORD                     
MAINKLEN DS    AL1                 LENGTH OF BINSRCH KEY                        
MAINMAX  DS    A                                                                
MAINSIZE DS    A                                                                
MAINLN   EQU   *-MAIND                                                          
         EJECT                                                                  
*-------------------------------------------------------                        
*        INCLUDED DSECTS                                                        
*-------------------------------------------------------                        
*                                                                               
*        DDLOGOD                                                                
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        ACGENPOST                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        ACMASTD                                                                
*        ACBIGPRNTD                                                             
*        DDBIGBOX                                                               
*        DDCNTRL                                                                
*        DDREPXTRAD                                                             
*        DDREPMASTD                                                             
*        DDBOXEQUS                                                              
*        DDREMOTED                                                              
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACBUDACCD                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056ACREPR102 03/23/15'                                      
         END                                                                    
