*          DATA SET ACREPG302  AT LEVEL 016 AS OF 04/10/15                      
*PHASE ACG302A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE UNDERLIN                                                               
*INCLUDE SORTER                                                                 
*INCLUDE CONVMOS                                                                
         SPACE 3                                                                
*              PROFILES                                                         
*                                                                               
* 1 - INCLUDE LEDGER S9 POSTINGS (DEFAULT IS EXLUDE).                           
* 2 - INCLUDE NIL BALANCE ACCOUNTS                                              
*                                                                               
         TITLE 'GENERAL LEDGER STATEMENT'                                       
ACG302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACG3**,RR=R5                                                 
*                                                                               
         LA    R9,2048(,RB)                                                     
         LA    R9,2048(,R9)                                                     
*                                                                               
         USING ACG302+4096,R9      R9=2ND BASE REG.                             
*                                                                               
         L     RA,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RA                                                       
*                                                                               
         LA    RC,SPACEND                                                       
*                                                                               
         USING ACG3D,RC                                                         
*                                                                               
         USING PRINTD,R6                                                        
         USING BUFFD,R7                                                         
         USING SORTD,R8                                                         
*                                                                               
         LA    R8,SORTREC          R8=SORT RECORDS                              
         LA    R7,BUFFREC          R7=BUFFALO RECORDS                           
         LA    R6,P                R6=PRINT LINE                                
*                                                                               
         ST    R5,RELO                                                          
         EJECT ,                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   GLS100                                                           
         LA    RF,RELOTAB          RELOCATE EXTERNS                             
         LA    RE,EXTERNS                                                       
GLS050   CLI   0(RE),X'FF'         END OF TABLE                                 
         BE    GLS060                                                           
         L     R1,0(,RE)                                                        
         A     R1,RELO                                                          
         ST    R1,0(,RF)                                                        
         LA    RE,4(,RE)                                                        
         LA    RF,4(,RF)                                                        
         B     GLS050                                                           
GLS060   DS    0H                                                               
*&&US                                                                           
         L     RF,=A(SAVERC)                                                    
         A     RF,RELO                                                          
         ST    RC,0(,RF)           SAVE REGISTER C                              
         L     RF,VEXTRAS                                                       
*                                                                               
         USING RUNXTRAD,RF                                                      
*                                                                               
         L     RF,ADMASTD                                                       
*                                                                               
         USING MASTD,RF                                                         
*                                                                               
         L     RF,MCBXAREA                                                      
         ST    RF,ADBOX                                                         
*                                                                               
         DROP  RF                  KEEP IT CLEAN                                
*                                                                               
         L     RF,=A(HOOK)                                                      
         A     RF,RELO                                                          
         ST    RF,HEADHOOK                                                      
*&&                                                                             
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         B     XIT                                                              
*                                                                               
XIT      XMOD1 1                                                                
         EJECT ,                                                                
GLS100   CLI   MODE,REQFRST                                                     
         BNE   GLS200                                                           
         MVC   PAGE,=H'1'                                                       
         MVC   SORTCARD+13(5),=C'01,28'                                         
         CLI   QOPT1,C'S'          SUPPRESS CONTRA-ACCOUNTS                     
         BNE   XIT                                                              
         MVC   SORTCARD+13(5),=C'16,12'                                         
         B     XIT                                                              
         EJECT ,                                                                
GLS200   CLI   MODE,LEDGFRST                                                    
         BNE   GLS300                                                           
         MVC   LEVCNTS,=4PL3'0'    CLEAR COUNTERS                               
         MVC   BALFWRD(48),PZEROS   AND BAL. FORWARD ACCUMS.                    
         MVC   LEVNAMES(60),SPACES                                              
         L     RF,ADLDGHIR         GET LEVEL NAMES                              
*                                                                               
         USING ACHEIRD,RF                                                       
*                                                                               
         LA    RF,ACHRLEVD         LOOP BACKWARDS                               
         LA    RE,LEVNAMES                                                      
         LA    R1,4                                                             
*                                                                               
GLS250   CLI   0(RF),0                                                          
         BE    GLS260                                                           
         CLC   ACCTNAME,SPACES     DO I HAVE ACCOUNT NAME YET                   
         BNE   GLS255                                                           
         MVC   ACCTNAME,1(RF)      NO - THIS MUST BE IT                         
         STH   R1,LEVNUM           NUMBER OF LEVELS                             
         B     GLS265              KEEP SAME LEVEL IN REG E                     
*                                                                               
GLS255   MVC   0(15,RE),1(RF)      MOVE IN LEVEL NAME                           
*                                                                               
GLS260   LA    RE,15(,RE)                                                       
*                                                                               
GLS265   SH    RF,=H'16'                                                        
         BCT   R1,GLS250                                                        
         B     XIT                                                              
*                                                                               
         DROP  RF                  KEEP IT CLEAN                                
         EJECT ,                                                                
GLS300   CLI   MODE,LEVAFRST                                                    
         BNE   GLS330                                                           
         MVC   LEVAACCT,SPACES                                                  
         L     R2,ADHEIRA                                                       
         MVC   LEVAACCT(12),3(R2)      SAVE ACCOUNT CODE                        
         L     R2,ADLVANAM                                                      
*                                                                               
         USING ACNAMED,R2                                                       
*                                                                               
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LEVAACCT+13(0),ACNMNAME   AND NAME                               
         GOTO1 VSQUASHR,DMCB,LEVAACCT,49                                        
         B     XIT                                                              
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
*                                                                               
GLS330   CLI   MODE,LEVBFRST                                                    
         BNE   GLS360                                                           
         MVC   LEVBACCT,SPACES                                                  
         L     R2,ADHEIRB                                                       
         MVC   LEVBACCT(12),3(R2)  SAVE ACCOUNT CODE                            
         L     R2,ADLVBNAM                                                      
*                                                                               
         USING ACNAMED,R2                                                       
*                                                                               
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LEVBACCT+13(0),ACNMNAME   AND NAME                               
         GOTO1 VSQUASHR,DMCB,LEVBACCT,49                                        
         B     XIT                                                              
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
*                                                                               
GLS360   CLI   MODE,LEVCFRST                                                    
         BNE   GLS400                                                           
         MVC   LEVCACCT,SPACES                                                  
         L     R2,ADHEIRC                                                       
         MVC   LEVCACCT(12),3(R2)      SAVE ACCOUNT CODE                        
         L     R2,ADLVCNAM                                                      
*                                                                               
         USING ACNAMED,R2                                                       
*                                                                               
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LEVCACCT+13(0),ACNMNAME   AND NAME                               
         GOTO1 VSQUASHR,DMCB,LEVCACCT,49                                        
         B     XIT                                                              
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
         EJECT ,                                                                
GLS400   CLI   MODE,PROCACC                                                     
         BNE   GLS500                                                           
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD,(40,ASORTA)                        
         MVI   FORCEHED,C'Y'                                                    
         MVI   ACCTACTV,C'N'                                                    
         ZAP   BALANCE,=P'0'                                                    
         MVC   HEADACCT,SPACES                                                  
         L     R2,ADACC                                                         
         MVC   HEADACCT(12),3(R2)  SAVE ACCOUNT NUMBER                          
         L     R2,ADACCNAM                                                      
         ZIC   R1,1(,R2)           ELEMENT LENGTH                               
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HEADACCT+13(0),2(R2)     AND NAME                                
         GOTO1 VSQUASHR,DMCB,HEADACCT,49                                        
         L     R2,ADACCBAL         GET BALANCE BROUGHT FORWARD                  
*                                                                               
         USING ACBALD,R2                                                        
*                                                                               
         ZAP   YTD,ACBLFRWD                                                     
         ZAP   BALFWRD,ACBLFRWD                                                 
         MVI   RCSUBPRG,0                                                       
         CLI   QOPT1,C'S'                                                       
         BNE   XIT                                                              
         MVI   RCSUBPRG,1                                                       
         B     XIT                                                              
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
         EJECT ,                                                                
GLS500   CLI   MODE,PROCTRNS                                                    
         BNE   GLS600                                                           
         L     R2,ADTRANS                                                       
*                                                                               
         USING TRANSD,R2                                                        
*                                                                               
         CLI   TRNSEL,X'44'                                                     
         BNE   XIT                                                              
         LR    R3,R2                                                            
         SH    R3,DATADISP                                                      
*                                                                               
         USING ACKEYD,R3                                                        
*                                                                               
         OC    ACDTPEEL,ACDTPEEL                                                
         BNZ   XIT                 IGNORE PEELED TRANSACTIONS                   
         L     R3,ADSUBAC                                                       
*                                                                               
         USING TRSUBHD,R3                                                       
*                                                                               
         CLC   TRSBACNT+1(2),=C'S9' EXCLUDE DUMMY POSTINGS                      
         BNE   GLS510                                                           
         CLI   PROGPROF,C'Y'       INCLUDE S9 POSTINGS                          
         BNE   XIT                                                              
*                                                                               
GLS510   MVC   SORTCTRA,TRSBACNT   CONTRA-ACCOUNT                               
         GOTO1 VCONVMOS,DMCB,(R2),SORTMOS                                       
         MVC   SORTDATE,TRNSDATE                                                
         MVC   SORTREF,TRNSREF                                                  
         MVC   SORTSBRF,TRNSSBRF                                                
         MVC   SORTSTAT,TRNSSTAT                                                
         MVC   SORTNAME,SPACES                                                  
         ZIC   R1,TRSBLEN          CONTRA-ACCOUNT NAME                          
         SH    R1,=H'18'                                                        
         BM    GLS550                                                           
         EXMVC R1,SORTNAME,TRSBNAME                                             
*                                                                               
GLS550   ZAP   SORTAMNT,TRNSAMNT                                                
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
         MVI   ACCTACTV,C'Y'                                                    
         B     XIT                                                              
*                                                                               
         DROP  R2,R3               KEEP IT CLEAN                                
         EJECT ,                                                                
GLS600   CLI   MODE,ACCLAST                                                     
         BNE   GLS700                                                           
         XR    R8,R8                                                            
         XC    LASTKEY,LASTKEY                                                  
         CLI   ACCTACTV,C'Y'       IF ACCOUNT NOT ACTIVE                        
         BE    GLS620                                                           
         CP    BALFWRD,=P'0'                                                    
         BNE   GLS642              JUST PRINT BALANCE FORWARD                   
         CLI   PROGPROF+1,C'Y'     INCLUDE NIL BAL. ACCOUNTS                    
         BNE   GLS690                                                           
         B     GLS642                                                           
*                                                                               
GLS620   GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     R8,DMCB+4           ADDRESS OF SORT RECORD                       
         LTR   R8,R8                                                            
         BZ    GLS645              NO MORE                                      
         LA    R2,SORTKEY1                                                      
         LA    R3,L'SORTKEY1                                                    
         CLI   QOPT1,C'S'                                                       
         BNE   GLS640                                                           
         LA    R2,SORTKEY2                                                      
         LA    R3,L'SORTKEY2                                                    
*                                                                               
GLS640   BCTR  R3,0                                                             
         OC    LASTKEY,LASTKEY                                                  
         BNZ   GLS644                                                           
*                                                                               
GLS642   MVC   P+1(23),=C'BALANCE BROUGHT FORWARD'   FIRST TIME                 
         MVI   SPACING,2                                                        
         B     GLS647                                                           
*                                                                               
GLS644   EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),LASTKEY                                                  
         BE    GLS660                                                           
*                                                                               
GLS645   AP    YTD,BALANCE                                                      
         ZAP   BALANCE,=P'0'                                                    
*                                                                               
GLS647   EDIT  (P8,YTD),(15,PYTD),2,COMMAS=YES,MINUS=YES                        
         BAS   RE,PRNTIT                                                        
         LTR   R8,R8               LAST TIME                                    
         BZ    GLS680                                                           
*                                                                               
GLS650   CLI   QOPT1,C'S'          SUPPRESS CONTRA-ACCOUNTS                     
         BE    GLS660                                                           
         CLC   SORTCTRA,LASTKEY    IF NAMES THE SAME                            
         BE    *+8                 DON'T PRINT NAME AGAIN                       
         BAS   RE,PUTNAME                                                       
*                                                                               
GLS660   EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LASTKEY(0),0(R2)                                                 
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTIT                                                        
         BAS   RE,PUTBUFF                                                       
         B     GLS620                                                           
*                                                                               
GLS680   DS    0H                                                               
         CLI   ACCTACTV,C'Y'                                                    
         BNE   GLS682                                                           
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         LA    R2,1                ACCOUNT LEVEL SUMMARY                        
         LA    R3,ACCTNAME                                                      
         BAS   RE,SUMMARY                                                       
         GOTO1 BUFFALO,DMCB,=C'ADD',ADBUFC,1,2,3,4,5,(X'80',6)                  
         GOTO1 (RF),(R1),=C'CLEAR',ADBUFC,(X'80',1)                             
*                                                                               
GLS682   LA    R2,BALFWRD                                                       
         LA    R3,8(,R2)                                                        
         LA    RF,5                                                             
*                                                                               
GLS685   AP    0(8,R3),0(8,R2)     ADD BAL. FORWARD TO HIGHER LEVELS            
         LA    R3,8(,R3)                                                        
         BCT   RF,GLS685                                                        
         AP    ACCTCNT,=P'1'                                                    
         MVI   RCSUBPRG,2                                                       
         CLI   ACCTACTV,C'Y'                                                    
         BE    XIT                                                              
*                                                                               
GLS690   GOTO1 VSORTER,DMCB,=C'END'  SET TO RE-INITIALIZE                       
         B     XIT                                                              
         EJECT ,                                                                
GLS700   CLI   MODE,LEVCLAST                                                    
         BNE   GLS730                                                           
         CP    ACCTCNT,=P'1'                                                    
         BL    GLS710                                                           
         BE    GLS705                                                           
         MVC   HEADACCT,LEVCACCT                                                
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PRNTIT                                                        
         LA    R2,2                                                             
         LA    R3,LEVCNAME                                                      
         BAS   RE,SUMMARY                                                       
*                                                                               
GLS705   AP    LEVCCNT,=P'1'                                                    
*                                                                               
GLS710   GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,(X'80',2)                          
         MVC   BALFWRD+8(8),PZEROS                                              
         ZAP   ACCTCNT,=P'0'                                                    
         B     XIT                                                              
*                                                                               
GLS730   CLI   MODE,LEVBLAST                                                    
         BNE   GLS760                                                           
         LA    R1,LEVCCNT                                                       
         CLC   LEVNUM,=H'3'        IF ONLY THREE LEVELS                         
         BNE   *+8                                                              
         LA    R1,ACCTCNT          THEN LAST LEVEL MUST BE ACCOUNT              
         CP    0(3,R1),=P'1'                                                    
         BL    GLS740                                                           
         BE    GLS735                                                           
         MVC   HEADACCT,LEVBACCT                                                
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PRNTIT                                                        
         LA    R2,3                                                             
         LA    R3,LEVBNAME                                                      
         BAS   RE,SUMMARY                                                       
*                                                                               
GLS735   AP    LEVBCNT,=P'1'                                                    
*                                                                               
GLS740   GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,2,(X'80',3)                        
         MVC   BALFWRD+8(16),PZEROS                                             
         ZAP   LEVCCNT,=P'0'                                                    
         ZAP   ACCTCNT,=P'0'                                                    
         B     XIT                                                              
*                                                                               
GLS760   CLI   MODE,LEVALAST                                                    
         BNE   GLS800                                                           
         LA    R1,LEVBCNT                                                       
         CLC   LEVNUM,=H'2'        IF ONLY TWO LEVELS                           
         BNE   *+8                                                              
         LA    R1,ACCTCNT          THEN LAST LEVEL MUST BE ACCOUNT              
         CP    0(3,R1),=P'1'                                                    
         BL    GLS770                                                           
         BE    GLS765                                                           
         MVC   HEADACCT,LEVAACCT                                                
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PRNTIT                                                        
         LA    R2,4                                                             
         LA    R3,LEVANAME                                                      
         BAS   RE,SUMMARY                                                       
*                                                                               
GLS765   AP    LEVACNT,=P'1'                                                    
*                                                                               
GLS770   GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,2,3,(X'80',4)                      
         MVC   BALFWRD+8(24),PZEROS                                             
         ZAP   LEVBCNT,=P'0'                                                    
         ZAP   ACCTCNT,=P'0'                                                    
         B     XIT                                                              
         EJECT ,                                                                
GLS800   CLI   MODE,LEDGLAST                                                    
         BNE   GLS900                                                           
         LA    R1,LEVACNT                                                       
         CLC   LEVNUM,=H'1'        IF ONLY ONE LEVEL                            
         BNE   *+8                                                              
         LA    R1,ACCTCNT          THEN LAST LEVEL MUST BE ACCOUNT              
         CP    0(3,R1),=P'1'                                                    
         BNH   GLS810                                                           
         MVI   RCSUBPRG,3                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,5                                                             
         LA    R3,=CL15'LEDGER'                                                 
         BAS   RE,SUMMARY                                                       
*                                                                               
GLS810   GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,2,3,4,(X'80',5)                    
         MVC   BALFWRD+8(32),PZEROS                                             
         B     XIT                                                              
         EJECT ,                                                                
GLS900   CLI   MODE,REQLAST                                                     
         BNE   GLS1000                                                          
         MVI   RCSUBPRG,4                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,6                                                             
         LA    R3,=CL15'REQUEST'                                                
         BAS   RE,SUMMARY                                                       
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,(X'80',6)                          
         ZAP   BALFWRD+40(8),PZEROS                                             
         B     XIT                                                              
         SPACE 3                                                                
GLS1000  CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         CLI   ACCTACTV,C'N'       IF LAST SORT INACTIVE                        
         BE    XIT                 THEN I'VE ALREADY ENDED.                     
         GOTO1 VSORTER,DMCB,=C'END'                                             
         B     XIT                                                              
         EJECT ,                                                                
*              PUT OUT A CONTRA-ACCOUNT HEADING                                 
         SPACE 2                                                                
PUTNAME  NTR1                                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(14),SORTCTRA+1                                              
         MVC   WORK+15(36),SORTNAME                                             
         GOTO1 VSQUASHR,DMCB,WORK,51                                            
         L     R5,DMCB+4                                                        
         GOTO1 CHOPPER,DMCB,((R5),WORK),(44,P+1),(C'P',2)                       
         CLC   PSECOND,SPACES                                                   
         BNE   PUTNAME2                                                         
         GOTO1 VUNDER,DMCB,(44,P+1),PSECOND+1                                   
*                                                                               
PUTNAME2 MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         EJECT ,                                                                
*              FORMAT A LINE TO PRINT                                           
         SPACE 2                                                                
FORMAT   NTR1                                                                   
         MVC   WORK(2),SORTMOS                                                  
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(6,PMOS)                                    
         GOTO1 (RF),(R1),(1,SORTDATE),(5,PDATE)                                 
         MVC   PREF,SORTREF                                                     
         ZAP   DOUBLE,SORTAMNT                                                  
         LA    RF,PDR                                                           
         TM    SORTSTAT,X'80'                                                   
         BO    *+14                                                             
         LA    RF,PCR                                                           
         MP    DOUBLE,=P'-1'                                                    
         EDIT  (P8,SORTAMNT),(15,0(RF)),2,COMMAS=YES,MINUS=YES                  
         AP    BALANCE,DOUBLE                                                   
         EDIT  (P8,BALANCE),(15,PBAL),2,COMMAS=YES,MINUS=YES                    
         B     XIT                                                              
         EJECT ,                                                                
PRNTIT   NTR1                                                                   
         CLI   MODE,LEVALAST       PRINT ACCOUNT CODE AND NAME                  
         BL    *+10                                                             
         MVC   HEAD6+9(L'HEADACCT),HEADACCT                                     
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT ,                                                                
*              PUT A RECORD TO BUFFALO                                          
         SPACE 2                                                                
PUTBUFF  NTR1                                                                   
         ZAP   BUFFDR,=P'0'        INITIALIZE ACCUMS.                           
         ZAP   BUFFCR,=P'0'                                                     
         MVC   BUFFMOS,SORTMOS                                                  
         LA    RF,BUFFDR                                                        
         TM    SORTSTAT,X'80'                                                   
         BO    *+8                                                              
         LA    RF,BUFFCR                                                        
         ZAP   0(8,RF),SORTAMNT                                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFFREC                              
         B     XIT                                                              
         EJECT ,                                                                
*              PRINT A SUMMARY                                                  
         SPACE 1                                                                
*                                  R2=LEVEL NUMBER                              
*                                  R3=A(LEVEL NAME)                             
SUMMARY  NTR1                                                                   
         XC    BUFFREC,BUFFREC                                                  
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFFREC,(R2)                        
         TM    DMCB+8,X'80'                                                     
         BO    XIT                 NOTHING TO PRINT                             
         MVC   WORK,SPACES                                                      
         MVC   WORK(15),0(R3)      LEVEL NAME                                   
         MVC   WORK+16(7),=C'SUMMARY'                                           
         GOTO1 VSQUASHR,DMCB,WORK,23                                            
         MVC   P+1(23),WORK                                                     
         GOTO1 VUNDER,DMCB,(23,P+1),PSECOND+1                                   
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         LA    R3,BALFWRD          BALANCE BROUGHT FORWARD                      
         LR    RF,R2               FOR THIS LEVEL                               
         BCTR  RF,0                                                             
         MH    RF,=H'8'            NEED CORRECT DISPLACEMENT                    
         LA    R3,0(RF,R3)                                                      
         ZAP   YTD,0(8,R3)                                                      
         MVC   PMOS(23),=C'BALANCE BROUGHT FORWARD'                             
         EDIT  (P8,0(R3)),(15,PYTD),2,COMMAS=YES,MINUS=YES                      
         BAS   RE,PRNTIT                                                        
         B     SUMMARY4                                                         
*                                                                               
SUMMARY2 GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFFREC,(R2)                         
         TM    DMCB+8,X'80'                                                     
         BO    XIT                                                              
*                                                                               
SUMMARY4 MVC   WORK(2),BUFFMOS                                                  
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(6,PMOS)                                    
         ZAP   BALANCE,BUFFDR                                                   
         SP    BALANCE,BUFFCR                                                   
         AP    YTD,BALANCE                                                      
         LA    RF,4                                                             
         LA    R3,BUFFDR                                                        
         LA    R5,PDR                                                           
*                                                                               
SUMMARY6 EDIT  (P8,0(R3)),(15,(R5)),2,COMMAS=YES,MINUS=YES                      
         LA    R3,8(,R3)                                                        
         LA    R5,16(,R5)                                                       
         BCT   RF,SUMMARY6                                                      
         BAS   RE,PRNTIT                                                        
         B     SUMMARY2                                                         
         EJECT ,                                                                
*              CONSTANTS, LITERAL POOL                                          
         SPACE 2                                                                
EXTERNS  DS    0F                                                               
         DC    V(SQUASHER)                                                      
         DC    V(UNDERLIN)                                                      
         DC    V(SORTER)                                                        
         DC    A(BUFFALOC)                                                      
         DC    A(SORTA)                                                         
         DC    V(CONVMOS)                                                       
         DC    X'FF'                                                            
         SPACE 1                                                                
PZEROS   DC    6PL8'0'                                                          
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(01,28,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(72)'                                  
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
*              BOX ROUTINES (HOOK)                                              
         SPACE 2                                                                
*&&US                                                                           
         ENTRY HOOK                                                             
         SPACE 1                                                                
         USING BOXD,RF                                                          
         SPACE 1                                                                
HOOK     NMOD1 0,*HOOK*                                                         
         L     RC,SAVERC           RESTORE REG C                                
         L     RF,ADBOX                                                         
         MVC   MYCOL,SPACES                                                     
         MVC   MYROW,SPACES                                                     
         MVI   MYROW+6,C'T'        SET ROWS                                     
         MVI   MYROW+9,C'M'                                                     
         MVI   MYROW+56,C'B'                                                    
         MVI   MYCOL+109,C'R'      SET RH MARGIN                                
*                                                                               
*                                  FIND SPROG                                   
         CLI   RCSUBPRG,4                                                       
         BH    HOOKX                                                            
         SPACE 1                                                                
         MVI   MYCOL,C'L'                                                       
         MVI   MYCOL+45,C'C'                                                    
         MVI   MYCOL+61,C'C'                                                    
         MVI   MYCOL+77,C'C'                                                    
         MVI   MYCOL+93,C'C'                                                    
*                                                                               
HOOKX    MVC   BOXROWS,MYROW                                                    
         MVC   BOXCOLS,MYCOL                                                    
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
*                                                                               
SAVERC   DC    A(0)                                                             
*                                                                               
         DROP  RF                  KEEP IT CLEAN                                
*                                                                               
         LTORG                                                                  
*&&                                                                             
         EJECT ,                                                                
         ENTRY SORTA                                                            
SORTA    DS    0D                                                               
         DS    41000C                                                           
         SPACE 2                                                                
         BUFF  LINES=200,ROWS=6,COLUMNS=2,FLAVOR=PACKED,KEYLIST=(2,A)           
         EJECT ,                                                                
SORTD    DSECT                     DSECT FOR SORTER RECORDS                     
SORTKEY1 DS    0CL17               NORMAL CONTROL BREAKS                        
SORTCTRA DS    CL15                                                             
SORTKEY2 DS    0CL2                SUPPRESS CONTRA-ACCOUNTS BREAKS              
SORTMOS  DS    PL2                 PACKED YYMM                                  
SORTDATE DS    PL3                 PACKED YYMMDD                                
SORTREF  DS    CL6                                                              
SORTSBRF DS    CL1                                                              
SORTSTAT DS    CL1                                                              
SORTNAME DS    CL36                                                             
SORTAMNT DS    PL8                                                              
         SPACE 2                                                                
BUFFD    DSECT                     DSECT FOR BUFFALO RECORDS                    
BUFFKEY  DS    0CL2                                                             
BUFFMOS  DS    PL2                 PACKED YYMM                                  
BUFFDR   DS    PL8                                                              
BUFFCR   DS    PL8                                                              
         SPACE 2                                                                
PRINTD   DSECT                     DSECT FOR PRINT LINE                         
         DS    CL16                                                             
PMOS     DS    CL6                                                              
         DS    CL4                                                              
PDATE    DS    CL8                                                              
         DS    CL4                                                              
PREF     DS    CL6                                                              
         DS    CL2                                                              
PDR      DS    CL15                                                             
         DS    CL1                                                              
PCR      DS    CL15                                                             
         DS    CL1                                                              
PBAL     DS    CL15                                                             
         DS    CL1                                                              
PYTD     DS    CL15                                                             
         DS    CL23                                                             
         EJECT ,                                                                
ACG3D    DSECT                     PROGRAM DSECT                                
RELO     DS    F                                                                
         SPACE 1                                                                
RELOTAB  DS    0F                  RELOCATED EXTERNS                            
VSQUASHR DS    V                                                                
VUNDER   DS    V                                                                
VSORTER  DS    V                                                                
ADBUFC   DS    A                                                                
ASORTA   DS    A                                                                
VCONVMOS DS    V                                                                
         SPACE 1                                                                
SORTREC  DS    CL72                RECORDS TO SORTER                            
BUFFREC  DS    CL18                RECORDS TO BUFFALO                           
BALANCE  DS    PL8                 MUST FOLLOW BUFFALO RECORD - SEE             
YTD      DS    PL8                 SUMMARY ROUTINE                              
         SPACE 1                                                                
ADBOX    DS    F                   STORAGE FOR BOX ROUTINE                      
MYROW    DS    CL100                                                            
MYCOL    DS    CL132                                                            
         SPACE 1                                                                
LEVNAMES DS    0H                  ACCOUNT HIERARCHY NAMES                      
LEVCNAME DS    CL15                                                             
LEVBNAME DS    CL15                                                             
LEVANAME DS    CL15                                                             
ACCTNAME DS    CL15                                                             
         SPACE 1                                                                
LEVCNTS  DS    0CL12                                                            
LEVACNT  DS    PL3                 ACTIVITY COUNTERS                            
LEVBCNT  DS    PL3                                                              
LEVCCNT  DS    PL3                                                              
ACCTCNT  DS    PL3                                                              
         SPACE 1                                                                
LEVCACCT DS    CL(L'HEADACCT)      HIGH LEVEL ACCOUNT CODES AND NAMES           
LEVBACCT DS    CL(L'HEADACCT)                                                   
LEVAACCT DS    CL(L'HEADACCT)                                                   
         SPACE 1                                                                
LASTKEY  DS    CL17                LAST KEY FROM SORTER                         
ACCTACTV DS    CL1                 ACCOUNT ACTIVE SWITCH                        
HEADACCT DS    CL49                ACCOUNT NO./NAME FOR HEADLINE                
LEVNUM   DS    H                   NUMBER OF LEVELS OF LEDGER                   
BALFWRD  DS    6PL8                BALANCE BROUGHT FWD. FOR ALL LEVELS          
         EJECT ,                                                                
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DDREPXTRAD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
*DDREPMASTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACREPG302 04/10/15'                                      
         END                                                                    
