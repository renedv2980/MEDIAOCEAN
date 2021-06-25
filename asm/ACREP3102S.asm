*          DATA SET ACREP3102S AT LEVEL 004 AS OF 05/01/02                      
*PHASE AC3102A,+0                                                               
*INCLUDE ACCEDIT                                                                
*INCLUDE CHOPCON                                                                
*INCLUDE DATVAL                                                                 
*INCLUDE COVAIL                                                                 
*INCLUDE CATKIN                                                                 
*---------------------------------------------------------------------*         
*        PROFILES                                                               
*        --------                                                               
*  1)    SORT FIELD 1                                                           
*  2)    SORT FIELD 2                                                           
*  3)    SORT FIELD 3                                                           
*  4)    SORT FIELD 4                                                           
*  5)    SORT FIELD 5                                                           
*  6)    CONTRA A/C SUMMARY WITHIN ACCOUNT                                      
*  7)    NEW PAGE PER ACCOUNT                                                   
*  8)    DISPLAY BUDGET AMOUNT (OLD STYLE) IN HEADS                             
*  9)    CONTRA A/C BREAKDOWN IN SUMMARIES                                      
* 10)    TOTAL ON SORT FIELD                                                    
* 11)    WORK CODE SUMMARY BY ACCOUNT                                           
* 12)    HEADLINES TO SHOW FILTER VALUES FROM REQUEST OR ACCOUNT                
* 13)    PRINT WORK-CODE BALANCE                                                
* 14)    SUPPRESS LONG NARRATIVE                                                
* 15)    SUPPRESS ANALYSIS POSTINGS                                             
* 16)    MAX LINES FOR NARRATIVE                                                
*---------------------------------------------------------------------*         
         TITLE 'STATEMENT PRINTING'                                             
AC3102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC31**,R9,R8,R7,RR=R5                                        
         L     RA,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
*                                                                               
         L     RC,ADWORKC                                                       
*                                                                               
         USING WORKD,RC            RC=A(SAVE W/S)                               
*                                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        HANDLE CONTROLLER MODE SETTINGS                                        
*----------------------------------------------------------------------         
         CLI   MODE,RUNFRST                                                     
         BNE   *+12                                                             
         BAS   RE,AC2                                                           
         B     EXMOD                                                            
*                                                                               
         CLI   MODE,LEDGFRST                                                    
         BNE   *+12                                                             
         BAS   RE,AC10                                                          
         B     EXMOD                                                            
*                                                                               
         CLI   MODE,LEVAFRST                                                    
         BNE   *+26                                                             
         ZAP   LEVACNT,=P'0'                                                    
         ZAP   LEVBCNT,=P'0'                                                    
         ZAP   LEVCCNT,=P'0'                                                    
         B     EXMOD                                                            
*                                                                               
         CLI   MODE,LEVBFRST                                                    
         BNE   *+20                                                             
         ZAP   LEVBCNT,=P'0'                                                    
         ZAP   LEVCCNT,=P'0'                                                    
         B     EXMOD                                                            
*                                                                               
         CLI   MODE,LEVCFRST                                                    
         BNE   *+14                                                             
         ZAP   LEVCCNT,=P'0'                                                    
         B     EXMOD                                                            
*                                                                               
         CLI   MODE,PROCACC                                                     
         BNE   *+12                                                             
         BAS   RE,AC40                                                          
         B     EXMOD                                                            
*                                                                               
         CLI   MODE,ANALFRST                                                    
         BNE   *+12                                                             
         BAS   RE,AC70                                                          
         B     EXMOD                                                            
*                                                                               
         CLI   MODE,SBACFRST                                                    
         BNE   *+12                                                             
         BAS   RE,AC90                                                          
         B     EXMOD                                                            
*                                                                               
         CLI   MODE,PROCTRNS                                                    
         BNE   *+12                                                             
         BAS   RE,AC100                                                         
         B     EXMOD                                                            
*                                                                               
         CLI   MODE,SBACLAST                                                    
         BNE   *+12                                                             
         BAS   RE,AC130                                                         
         B     EXMOD                                                            
*                                                                               
         CLI   MODE,ANALLAST                                                    
         BNE   *+12                                                             
         BAS   RE,AC140                                                         
         B     EXMOD                                                            
*                                                                               
         CLI   MODE,ACCLAST                                                     
         BNE   *+12                                                             
         BAS   RE,AC150                                                         
         B     EXMOD                                                            
*                                                                               
         CLI   MODE,LEVCLAST                                                    
         BNE   *+18                                                             
         BAS   RE,AC160                                                         
         ZAP   LEVCCNT,=P'0'                                                    
         B     EXMOD                                                            
*                                                                               
         CLI   MODE,LEVBLAST                                                    
         BNE   *+18                                                             
         BAS   RE,AC160                                                         
         ZAP   LEVBCNT,=P'0'                                                    
         B     EXMOD                                                            
*                                                                               
         CLI   MODE,LEVALAST                                                    
         BNE   *+18                                                             
         BAS   RE,AC160                                                         
         ZAP   LEVACNT,=P'0'                                                    
         B     EXMOD                                                            
*                                                                               
         CLI   MODE,LEDGLAST                                                    
         BNE   *+16                                                             
         BAS   RE,AC160                                                         
         BAS   RE,AC200                                                         
         B     EXMOD                                                            
*                                                                               
*                                                                               
EXMOD    XMOD1 1                                                                
*                                                                               
EXIT     XIT1  1                                                                
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        FIRST FOR RUN                                                          
*---------------------------------------------------------------------*         
AC2      NTR1                                                                   
         LA    R1,ADCONS                                                        
*                                                                               
AC4      CLI   0(R1),X'FF'         RELOCATE ADCONS                              
         BE    AC6                                                              
         L     RE,0(,R1)                                                        
         AR    RE,R5                                                            
         ST    RE,0(,R1)                                                        
         LA    R1,4(,R1)                                                        
         B     AC4                                                              
*                                                                               
AC6      L     RF,ADBUFF                                                        
         GOTO1 =V(COVAIL),DMCB,C'SETB',20000,1000000,(RF)                       
         MVC   ADBUFF,12(R1)                                                    
*                                                                               
         L     RC,ADWORKC                                                       
         ST    R5,RELO                                                          
         XC    CHOPSAVE,CHOPSAVE                                                
         L     RE,ADOFFBUF                                                      
         XC    0(4,RE),0(RE)                                                    
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAY3)                                
         GOTO1 (RF),(R1),,(2,TODAY2)                                            
         B     EXIT                                                             
*                                                                               
ADCONS   DS    0F                                                               
ADBUFF   DC    A(BUFFALOC)                                                      
ADOFFBUF DC    A(OFFBUFF)                                                       
ADPRNTBC DC    A(PRNTBLOC)                                                      
ADWORKC  DC    A(WORKC)                                                         
AGETANAL DC    A(GETANAL)                                                       
VACCEDIT DC    V(ACCEDIT)                                                       
VCHOPCON DC    V(CHOPCON)                                                       
VDATVAL  DC    V(DATVAL)                                                        
VCATKIN  DC    V(CATKIN)                                                        
ASVSORT  DC    A(SVSORT)                                                        
ASRTREC  DC    A(SRTREC)                                                        
ASRTRECA DC    A(SRTRECA)                                                       
         DC    X'FF'                                                            
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        FIRST FOR LEDGER/REQUEST                                               
*---------------------------------------------------------------------*         
AC10     NTR1                                                                   
         MVI   SORTMODE,0                                                       
         MVI   SWITCHES,C'N'                                                    
         MVC   SWITCHES+1(L'SWITCHES-1),SWITCHES                                
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFF                                      
         GOTO1 PROLLER,DMCB,0,(8,ACCUMS),9,3                                    
         ZAP   LDGBLFWD,=P'0'                                                   
         ZAP   ACCCNT,=P'0'                                                     
         ZAP   LEVACNT,=P'0'                                                    
         ZAP   LEVBCNT,=P'0'                                                    
         ZAP   LEVCCNT,=P'0'                                                    
*                                                                               
         BAS   RE,CLRSREC                                                       
         L     R6,ASRTRECA                                                      
*                                                                               
         USING SRTRECD,R6          R3=A(LEDGER RECORD)                          
*                                                                               
         MVI   SRTRECT,SLEGFTYP                                                 
*                                                                               
         L     R1,ADCMPEL                                                       
*                                                                               
         USING ACCOMPD,R1                                                       
*                                                                               
         TM    ACMPSTAT,X'20'                                                   
         BZ    *+8                                                              
         MVI   OFFSW,C'Y'                                                       
         TM    ACMPSTA4,X'01'      TEST ON NEW OFFICES                          
         BZ    *+8                                                              
         MVI   NEWOFF,C'Y'         YES                                          
         TM    ACMPSTA5,ACMPNCST   NEW COST                                     
         BZ    *+8                                                              
         MVI   NEWCST,C'Y'                                                      
*                                                                               
         DROP  R1                                                               
*                                  SET FILTER SWITCH                            
         CLC   QSTART(12),SPACES                                                
         BNE   *+14                                                             
         CLC   QTRNSFLT,SPACES                                                  
         BE    AC12                                                             
         MVI   FLTSW,C'Y'                                                       
*                                  BUILD A LIST OF OFFICES                      
AC12     CLI   OFFSW,C'Y'                                                       
         BNE   AC20                                                             
         CLC   QUNIT(2),=C'SJ'                                                  
         BE    AC20                                                             
         L     R4,ADOFFBUF                                                      
         OC    0(4,R4),0(R4)       OFFICE BUFFER ALREADY BUILT                  
         BNZ   AC20                                                             
         MVI   0(R4),X'FF'                                                      
         LA    RF,BLOLD            BUILD OLD  FFICE BUFFER                      
         CLI   NEWOFF,C'N'                                                      
         BE    *+8                                                              
         LA    RF,BLNEW                                                         
         BASR  RE,RF                                                            
*                                                                               
AC18     MVI   READSW,C'Y'         FORCE RE-READ OF LEDGER RECORD               
*                                  BUILD TRANSACTION SORT CARD                  
AC20     MVC   LEDGPROF,PROGPROF                                                
         OC    LEDGPROF,LEDGPROF                                                
         BNZ   *+10                                                             
         MVC   LEDGPROF,DEFAULT                                                 
         OI    SORTMODE,SORTPRNT                                                
*                                                                               
         CLI   QOPT3,C'T'          OPTION TO SORT BY TAX ID                     
         BNE   AC21                                                             
         MVC   LEDGPROF(5),=C'C****'   MAKE TAX-ID THE CONTRA ACCT              
         B     AC21A                                                            
*                                                                               
AC21     DS    0H                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   AC28                                                             
*                                                                               
AC21A    CLI   QOPT2,C'N'          DON'T SORT IF SUMMARIZING                    
         BE    AC28                                                             
         CLC   LEDGPROF(5),=C'*****'                                            
         BE    AC28                                                             
         NI    SORTMODE,X'FF'-SORTPRNT                                          
         OI    SORTMODE,SORTALL                                                 
         MVC   SORTCARD,SPACES                                                  
         MVC   SORTCARD(13),=C'SORT FIELDS=('                                   
         MVC   SORTCARD+13(8),=C'05,16,A,'                                      
         LA    R2,SORTCARD+12+8                                                 
         LA    R3,LEDGPROF                                                      
         LA    R4,5                                                             
*                                                                               
AC22     CLI   0(R3),C'*'                                                       
         BE    AC26                                                             
         LA    RE,SORTKEYS         LOOK-UP SORT PARM IN TABLE                   
*                                                                               
AC24     CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,RE),0(R3)                                                    
         BE    *+12                                                             
         LA    RE,L'SORTKEYS(,RE)                                               
         B     AC24                                                             
         ZIC   R1,1(,RE)           DISPLACEMENT                                 
         CVD   R1,DUB                                                           
         UNPK  1(2,R2),DUB                                                      
         OI    2(R2),X'F0'                                                      
         MVI   3(R2),C','                                                       
         ZIC   R1,2(,RE)           LENGTH                                       
         CVD   R1,DUB                                                           
         UNPK  4(2,R2),DUB                                                      
         OI    5(R2),X'F0'                                                      
         MVC   6(3,R2),=C',A,'                                                  
         LA    R2,8(,R2)                                                        
*                                                                               
AC26     LA    R3,1(,R3)                                                        
         BCT   R4,AC22                                                          
         MVC   0(18,R2),=C'),FORMAT=BI,WORK=1'                                  
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
         MVI   SORTSW,C'Y'                                                      
*                                  SET PRINT SWITCH                             
AC28     L     R2,ADLDGEL                                                       
*                                                                               
         USING ACLEDGD,R2                                                       
*                                                                               
         MVC   PRTSW,ACLTPRNT                                                   
         MVI   SUBPROG,1                                                        
         CLI   PRTSW,C'+'                                                       
         BE    AC30                                                             
         CLI   PRTSW,C'-'                                                       
         BE    AC30                                                             
         MVI   PRTSW,C'N'                                                       
         MVI   SUBPROG,0                                                        
*                                  SET START/END DATE FILTERS                   
AC30     XC    PSTART,PSTART                                                    
         MVC   PEND,=3X'FF'                                                     
         L     R3,AMONACC          GET MONACC ADDRESS                           
*                                                                               
         USING ACMD,R3                                                          
*                                                                               
         CLC   QSTART,SPACES                                                    
         BE    AC32                                                             
         GOTO1 DATCON,DMCB,(0,QSTART),(1,PSTART)                                
         MVC   ACMTSTR,PSTART       GIVE MONACC CONVERTED DATE                  
*                                                                               
AC32     CLC   QEND,SPACES                                                      
         BE    AC34                                                             
         GOTO1 DATCON,DMCB,(0,QEND),(1,PEND)                                    
         MVC   ACMTEND,PEND           GIVE CONVERTED DATE TO MONACC             
*                                                                               
AC34     CLI   LEDGPROF+8,C'Y'     ARE WE CHOPPING C/A KEYS                     
         BNE   AC3E                                                             
         CLI   QOPT2,C' '          ARE WE PRINTING SUMMARIES                    
         BE    AC3E                                                             
         OC    CHOPSAVE,CHOPSAVE   LEDGER LIST ALREADY BUILT                    
         BNZ   AC3E                                                             
         MVC   THSKEY,SPACES       YES - READ ALL LEDGER RECORDS                
         MVC   THSKEY(1),RCCOMPFL                                               
         MVI   THSKEY+1,X'41'                                                   
         MVI   THSKEY+2,X'41'                                                   
         L     R3,ASRTREC                                                       
*                                                                               
         USING ACKEYD,R3           R3=A(LEDGER RECORD)                          
*                                                                               
         LA    R4,CHOPSAVE+1       R4=A(NEXT TABLE ENTRY)                       
         SR    R5,R5               R5=NUMBER OF ENTRIES IN TABLE                
*                                                                               
AC36     GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCOUNT',THSKEY,ASRTREC           
         CLI   DMCB+8,0                                                         
         BNE   AC3C                                                             
         CLC   ACKEYACC(1),THSKEY                                               
         BNE   AC3C                                                             
         CLI   ACKEYACC+1,C' '     UNIT RECORD ?                                
         BE    AC3A                                                             
         CLC   ACKEYACC+3(12),SPACES                                            
         BNE   AC3A                                                             
         MVC   0(2,R4),ACKEYACC+1  UNIT/LEDGER TO TABLE                         
         LA    R1,ACRECORD                                                      
         SR    RE,RE                                                            
*                                                                               
AC38     CLI   0(R1),0                                                          
         BE    AC3A                                                             
         CLI   0(R1),X'16'         HEIRARCHY ELEMENT                            
         BE    *+14                                                             
         IC    RE,1(,R1)                                                        
         AR    R1,RE                                                            
         B     AC38                                                             
*                                  SAVE HEIRARCHY INFO IN TABLE                 
*                                                                               
         USING ACHEIRD,R1          R1=A(ELEMENT)                                
*                                                                               
         MVC   2(1,R4),ACHRLEVA                                                 
         MVC   3(1,R4),ACHRLEVB                                                 
         MVC   4(1,R4),ACHRLEVC                                                 
         MVC   5(1,R4),ACHRLEVD                                                 
*                                                                               
         DROP  R1                                                               
*                                                                               
         LA    R4,6(,R4)           BUMP TO NEXT ENTRY                           
         LA    R5,1(,R5)                                                        
*                                                                               
AC3A     MVC   THSKEY(2),ACKEYACC BUMP TO NEXT LEDGER                           
         ZIC   R1,ACKEYACC+2                                                    
         LA    R1,1(,R1)                                                        
         STC   R1,THSKEY+2                                                      
         B     AC36                                                             
*                                                                               
AC3C     STC   R5,CHOPSAVE         SET NUMBER OF ENTRIES IN TABLE               
         MVI   READSW,C'Y'         FORCE RE-READ OF LEDGER RECORD               
*                                                                               
AC3E     CLI   READSW,C'Y'         DO I HAVE TO RE-READ LEDGER RECORD           
         BNE   AC3G                                                             
         MVC   THSKEY,SPACES                                                    
         MVC   THSKEY(15),KEY                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'ACCOUNT',THSKEY,ASRTREC           
         CLI   DMCB+8,0                                                         
         BE    AC3G                                                             
         DC    H'0'                                                             
*                                                                               
AC3G     CLI   LEDGPROF+11,C'Y'    OPTION TO SHOW REQUESTED FILTERS             
         BNE   AC3J                IN HEADLINES                                 
         MVI   FCGTFILT,C'Y'                                                    
         GOTO1 GETFILT,DMCB,(C'A',ADLEDGER),(4,QFILTER1),LEDGFILT,0,   *        
               DATAMGR                                                          
*                                                                               
AC3J     MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVC   RCSUBPRG,SUBPROG                                                 
*                                                                               
         CLI   SORTSW,C'Y'                                                      
         BNE   EXIT                                                             
         MVC   SRTFLTSW,FLTSW                                                   
         MVC   SRTPRTSW,PRTSW                                                   
         LA    R5,SLEGLEN                                                       
         STH   R5,SORTRLEN                                                      
         LA    R4,SORTLEN                                                       
         CR    R5,R4                                                            
         BH    *+8                                                              
         STH   R4,SORTRLEN                                                      
         CLI   QOPT7,C'P'                                                       
         BNE   AC3P                                                             
         MVC   P(132),0(R6)                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
AC3P     GOTO1 ADSORTER,DMCB,=C'PUT',(R6)                                       
         B     EXIT                                                             
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        FIRST FOR ACCOUNT                                                      
*---------------------------------------------------------------------*         
AC40     NTR1                                                                   
         MVI   ACCSW,C'N'                                                       
         MVI   ANALSW,C'N'                                                      
         MVI   SAVESW,C'N'                                                      
         ZAP   TRNSCNT,=P'0'                                                    
         ZAP   ANALCNT,=P'0'                                                    
         AP    ACCCNT,=P'1'                                                     
         AP    LEVCCNT,=P'1'                                                    
         AP    LEVBCNT,=P'1'                                                    
         AP    LEVACNT,=P'1'                                                    
*                                                                               
         BAS   RE,CLRSREC                                                       
         L     R6,ASRTRECA                                                      
*                                                                               
         USING SRTRECD,R6          R3=A(LEDGER RECORD)                          
*                                                                               
         MVI   SRTRECT,SACCTYP                                                  
*                                                                               
         L     R1,ADACC                                                         
         MVC   THSACC,0(R1)        GET A/C CODE & NAME                          
         MVC   SORTACCT,0(R1)                                                   
*                                                                               
         BAS   RE,GETNAME                                                       
         MVC   THISNAME,WORK                                                    
         MVC   SACCNM,WORK                                                      
         MVC   THISHED1,SPACES                                                  
         MVC   THISHED2,SPACES                                                  
         MVC   THISHED3,SPACES                                                  
         MVC   THISHED4,SPACES                                                  
         MVC   THISHED5,SPACES                                                  
         MVC   THISHED6,SPACES                                                  
         MVC   THISHED7,SPACES                                                  
         CLI   QOPT2,C'N'          DON'T FORMAT HEADLINES IF SUMMARY            
         BE    AC60                                                             
         MVC   THSACCT,SPACES                                                   
         GOTO1 VACCEDIT,DMCB,(0,ADACC),ADLDGHIR,THSACCT                         
         MVC   SACCFORM,THSACCT                                                 
         MVI   ACCSW,C'F'                                                       
         LA    R2,LEDGFILT                                                      
         CLI   LEDGPROF+11,C'Y'    OPTION TO SHOW REQUESTED FILTERS             
         BE    AC41                                                             
         CLI   LEDGPROF+6,C'N'                                                  
         BE    AC60                                                             
         CLI   LEDGPROF+11,C'A'    OPTION TO SHOW ACCOUNT LEVELS                
         BNE   *+12                IN HEADLINES (INSTEAD OF FILTERS)            
         BAS   RE,FORMLEV                                                       
         B     AC43                                                             
         L     R2,FILTBLOC                                                      
*                                                                               
AC41     LA    R3,THISHED1         OUTPUT FILTER NAMES/VALUES                   
         LA    R4,4                                                             
*                                                                               
AC42     OC    0(16,R2),0(R2)                                                   
         BZ    *+14                                                             
         MVC   0(46,R3),0(R2)                                                   
         LA    R3,L'THISHED1(,R3)                                               
         LA    R2,60(,R2)                                                       
         BCT   R4,AC42                                                          
         CLI   LEDGPROF+6,C'N'                                                  
         BE    AC60                                                             
*                                                                               
AC43     LR    R4,R3                                                            
         L     R2,ADACC                                                         
         AH    R2,DATADISP                                                      
         ZAP   DUB,=P'0'                                                        
         ZAP   DOUBLE,=P'0'                                                     
*                                  LOOK FOR OTHER ELEMENTS                      
AC44     CLI   0(R2),0                                                          
         BE    AC52                                                             
         CLI   0(R2),X'23'         NUMBER ELEMENT                               
         BE    AC48                                                             
         CLI   0(R2),X'34'         BUDGET ELEMENT                               
         BE    AC50                                                             
         CLI   0(R2),X'25'         EXTRA NUMBER ELEMENT                         
         BE    AC51                                                             
*                                                                               
AC46     ZIC   R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     AC44                                                             
*                                  FORMAT NUMBER ELEMENT                        
*                                                                               
         USING ACOTHERD,R2                                                      
*                                                                               
AC48     CLC   ACOTNUM,SPACES                                                   
         BE    AC46                                                             
         MVC   0(7,R3),=C'NUMBER='                                              
         MVC   7(L'ACOTNUM,R3),ACOTNUM                                          
         MVC   SACCNUM1,ACOTNUM                                                 
         LA    R3,18(,R3)                                                       
         B     AC46                                                             
*                                  ADD BUDGET AMOUNT TO TOTAL                   
*                                                                               
         USING ACBUDGD,R2                                                       
*                                                                               
AC50     CLI   ACBDLEN,X'1C'                                                    
         BNE   AC46                                                             
         LA    R1,DUB                                                           
         CLI   ACBDTYPE,C'D'                                                    
         BE    *+16                                                             
         LA    R1,8(,R1)                                                        
         CLI   ACBDTYPE,C'C'                                                    
         BNE   *+10                                                             
         AP    0(8,R1),ACBDBUDG                                                 
         B     AC46                                                             
*                                  FORMAT EXTRA NUMBER ELEMENT                  
*                                                                               
         USING ACNOD,R2                                                         
*                                                                               
AC51     MVC   0(5,R3),=C'NUM2='                                                
         MVC   5(L'ACNO,R3),ACNO                                                
         MVC   SACCNUM2,ACNO                                                    
         LA    R3,17(,R3)                                                       
         B     AC46                                                             
*                                  OUTPUT BUDGET DETAILS                        
AC52     CLI   LEDGPROF+7,C'Y'                                                  
         BNE   AC56                                                             
         CLC   DUB(16),=4PL8'0'                                                 
         BE    AC56                                                             
         MVC   0(7,R3),=C'BUDGET='                                              
         LA    R2,7(,R3)                                                        
         CP    DUB,=P'0'                                                        
         BE    AC54                                                             
         CVB   R1,DUB                                                           
         EDIT  (R1),(10,0(R2)),2,ALIGN=LEFT                                     
         AR    R2,R0                                                            
         MVC   0(2,R2),=C'DR'                                                   
         CP    DOUBLE,=P'0'                                                     
         BE    AC56                                                             
         MVI   2(R2),C','                                                       
         LA    R2,3(,R2)                                                        
*                                                                               
AC54     CVB   R1,DOUBLE                                                        
         EDIT  (R1),(10,0(R2)),2,ALIGN=LEFT                                     
         AR    R2,R0                                                            
         MVC   0(2,R2),=C'CR'                                                   
*                                                                               
AC56     GOTO1 ADSQUASH,DMCB,(R4),(C',',64)                                     
*                                                                               
AC60     DS    0H                                                               
         CLI   SORTSW,C'Y'                                                      
         BNE   EXIT                                                             
         MVC   SHEAD1,THISHED1                                                  
         MVC   SHEAD2,THISHED2                                                  
         MVC   SHEAD3,THISHED3                                                  
         MVC   SHEAD4,THISHED4                                                  
         MVC   SHEAD5,THISHED5                                                  
         MVC   SHEAD6,THISHED6                                                  
         MVC   SHEAD7,THISHED7                                                  
         LA    R5,SACCLEN                                                       
         STH   R5,SORTRLEN                                                      
         LA    R4,SORTLEN                                                       
         CR    R5,R4                                                            
         BH    *+8                                                              
         STH   R4,SORTRLEN                                                      
         CLI   QOPT7,C'P'                                                       
         BNE   AC65                                                             
         MVC   P(132),0(R6)                                                     
         GOTO1 ACREPORT                                                         
AC65     GOTO1 ADSORTER,DMCB,=C'PUT',(R6)                                       
         B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        FIRST FOR ANALYSIS CODE                                                
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING TRANSD,R2                                                        
         SPACE 1                                                                
AC70     NTR1                                                                   
         MVI   ANALSW,C'N'                                                      
         L     R2,ADTRANS                                                       
         MVC   THISANAL,TRNSANAL   FORMAT CODE/NAME INTO WORK                   
         OC    THISANAL,SPACES                                                  
         GOTO1 AGETANAL,(RC)                                                    
         MVI   ANALSW,C'F'         SET FT SWITCH                                
         CLI   QOPT2,C'N'                                                       
         BNE   *+8                                                              
         MVI   ANALSW,C'Y'                                                      
         CLI   SORTSW,C'N'                                                      
         BE    *+8                                                              
         MVI   ANALSW,C'Y'                                                      
*                                                                               
         CLI   SORTSW,C'Y'                                                      
         BNE   EXIT                                                             
*                                                                               
         USING ACKEYACC,R2                                                      
*                                                                               
         L     R2,ADACC                                                         
         BAS   RE,CLRSREC                                                       
         L     R6,ASRTRECA                                                      
*                                                                               
         USING SRTRECD,R6                                                       
*                                                                               
         MVC   SORTACCT,ACKEYACC                                                
         MVI   SRTRECT,SANATYP                                                  
         MVC   SRTANAL,THISANAL                                                 
         LA    R5,SANALEN                                                       
         STH   R5,SORTRLEN                                                      
         LA    R4,SORTLEN                                                       
         CR    R5,R4                                                            
         BH    *+8                                                              
         STH   R4,SORTRLEN                                                      
         CLI   QOPT7,C'P'                                                       
         BNE   AC71                                                             
         MVC   P(132),0(R6)                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
AC71     GOTO1 ADSORTER,DMCB,=C'PUT',(R6)                                       
*                                                                               
AC72     B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        FIRST FOR SUB-ACCOUNT                                                  
*---------------------------------------------------------------------*         
AC90     NTR1                                                                   
         MVI   SBACSW,C'N'                                                      
         MVI   TRNSSW,C'N'                                                      
         MVC   SAVEKEY,SPACES                                                   
         L     R2,ASRTREC                                                       
*                                                                               
         USING ACKEYD,R2                                                        
*                                                                               
         MVC   ACKEYACC(ACRECORD-ACKEYD),SPACES                                 
         L     R1,ADACC                                                         
         MVC   ACKEYACC,0(R1)                                                   
         L     R1,ADSUBAC                                                       
*                                                                               
         USING TRSUBHD,R1                                                       
*                                                                               
         MVC   ACKEYCON,TRSBACNT                                                
         MVC   ACRECORD(55),0(R1)                                               
         MVC   CNAME,SPACES                                                     
         ZIC   R4,TRSBLEN          ELEMENT LENGTH                               
         SH    R4,=H'18'           SUBTRACT LENGTH OF ELEMENT KEY + 1           
         BM    AC91                BRANCH IF ANSWER IS MINUS                    
         EXMVC R4,CNAME,TRSBNAME   FOR EXECUTED MOVE                            
*                                                                               
         DROP  R1                                                               
AC91     DS    0H                                                               
         L     RF,ADSUBAC                                                       
         SH    RF,DATADISP                                                      
         GOTO1 VCHOPCON,DMCB,(RF),(C'N',CHOPBLOC),CHOPSAVE,DATAMGR,    *        
               ADLEDGER                                                         
         MVC   CHOPBLOC+15(L'CNAME),CNAME                                       
         MVC   CHOPNUM,4(R1)                                                    
         CLI   QOPT2,C'N'                                                       
         BE    AC92                                                             
         MVC   THISSBAC,CHOPBLOC   SAVE CODE/NAME IN WORK SET FT SWITCH         
         MVC   THISSBNM,CHOPBLOC+15                                             
         MVI   SBACSW,C'F'                                                      
AC92     CLI   QOPT2,C' '                                                       
         BNE   *+12                                                             
         MVI   CHOPNUM,0                                                        
         B     AC94                                                             
         CLI   LEDGPROF+8,C'Y'     OPTION TO CHOP C/A KEYS                      
         BNE   AC94                                                             
         MVI   4(R1),0                                                          
         BASR  RE,RF               RE-CALL CHOPCON                              
         MVC   CHOPNUM,4(R1)                                                    
*                                                                               
*              GET MAXIMUM NUMBERS OF LEVELS                                    
*                                                                               
AC94     MVI   MAXLVL,0            MAX NUMBER OF LEVELS IN CONTRA               
         CLI   ACKEYCON+1,C' '                                                  
         BNH   AC99                                                             
         CLC   ACKEYCON+1(2),=C'SR'                                             
         BE    AC99                                                             
         CLC   ACKEYCON+1(2),=C'2P'                                             
         BE    AC99                                                             
         CLC   ACKEYCON+1(2),=C'29'                                             
         BE    AC99                                                             
         LA    R1,CHOPSAVE+1       R1 = LIST OF U/L - LEVELS                    
         SR    R0,R0                                                            
         IC    R0,CHOPSAVE         R0 = NUMBER OF LEVELS                        
         LTR   R0,R0                                                            
         BZ    AC99                                                             
*                                                                               
AC94A    CLC   ACKEYCON+1(2),0(R1) MATCH CONTRA TO U/L LIST                     
         BE    AC94B                                                            
         LA    R1,6(,R1)                                                        
         BCT   R0,AC94A                                                         
         B     AC99                NOT IN LIST                                  
*                                                                               
AC94B    LA    R1,2(,R1)           R1 = HEIRARCHY FOR LEDGER                    
         LA    R0,4                                                             
         LA    R2,1                COUNT NUMBER OF LEVELS                       
*                                                                               
AC94C    CLI   0(R1),12                                                         
         BE    AC95                                                             
         LA    R1,1(,R1)                                                        
         LA    R2,1(,R2)                                                        
         BCT   R0,AC94C                                                         
         B     AC99                                                             
*                                                                               
AC95     STC   R2,MAXLVL           MAX NUMBER LEVELS FOR THIS LEDGER            
*                                                                               
AC99     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        TRANSACTION HANDLING                                                   
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING TRANSD,R2                                                        
         SPACE 1                                                                
AC100    NTR1                                                                   
         MVI   NARRSW,C'N'                                                      
         L     R2,ADTRANS                                                       
         CLI   TRNSEL,X'44'                                                     
         BNE   EXIT                                                             
*                                                                               
         CLI   QOPT5,C' '          AUTH/UNAUTH OPTION                           
         BE    AC101                                                            
         CLI   QOPT5,C'A'                                                       
         BE    AC100B                                                           
         TM    TRNSSTAT,X'08'      U=UNAUTH ONLY                                
         BO    EXIT                                                             
         B     AC101                                                            
*                                                                               
AC100B   TM    TRNSSTAT,X'08'      A=AUTH ONLY                                  
         BZ    EXIT                                                             
*                                                                               
AC101    CLC   TRNSDATE,PSTART     CHECK IT FALLS WITHIN DATES                  
         BL    EXIT                                                             
         CLC   TRNSDATE,PEND                                                    
         BH    EXIT                                                             
         LR    RF,R2                AND IS NOT PEELED                           
         SH    RF,DATADISP                                                      
*                                                                               
         USING ACKEYD,RF                                                        
*                                                                               
         OC    ACDTPEEL,ACDTPEEL                                                
         BZ    *+14                                                             
         CLC   ACDTPEEL,TODAY2                                                  
         BL    EXIT                                                             
*                                                                               
         USING SRTRECD,R6                                                       
*                                                                               
AC101C   L     R6,ASRTREC                                                       
         XC    SORTKEY,SORTKEY                                                  
         MVC   SORTACCT,0(RF)                                                   
         MVI   SRTRECT,STRNTYP                                                  
*                                  BUILD SORT RECORD FROM TRANSACTION           
         MVC   SORTSBNM,CNAME      SAVE CONTRA ACCOUNT NAME IN RECORD           
         MVC   SORTOFFC,TRNSANAL                                                
         OC    SORTOFFC,SPACES                                                  
         MVC   SORTSBAC,THISSBAC                                                
         MVC   SORTDATE,TRNSDATE                                                
         MVC   SORTREF,TRNSREF                                                  
         MVC   SORTMOS,TRNSBTCH                                                 
         ZIC   R1,SORTMOS+1                                                     
         CLI   SORTMOS+1,C'C'      CONVERT C'A/B/C' TO X'FA/FB/FC'              
         BH    *+8                                                              
         AH    R1,=H'57'                                                        
         STC   R1,SORTMOS+1                                                     
         MVC   SORTTYPE,TRNSTYPE                                                
         MVC   SORTSTAT,TRNSSTAT                                                
         ZAP   SORTAMNT,TRNSAMNT                                                
         ZAP   SORTBASE,=P'0'                                                   
         ZAP   SORTRATE,=P'0'                                                   
         MVC   SORTLOC,SPACES                                                   
         LA    R0,SORTLEN                                                       
         STH   R0,SORTRLEN                                                      
         CLI   QOPT2,C'N'          DON'T FORMAT NARRATIVE IF SUMMARY            
         BE    AC116                                                            
         XCEF  SORTNARR,500                                                     
         LA    R3,SORTNARR                                                      
*                                                                               
         ZIC   R1,TRNSLEN                                                       
         SH    R1,=H'29'                                                        
         BNP   AC102                                                            
         MVI   NARRSW,C'Y'                                                      
         CLC   TRNSANAL,=C'99'                                                  
         BNE   *+8                                                              
         LA    R1,14                                                            
         EXMVC R1,0(R3),TRNSNARR   MOVE TRANSACTION NARRATIVE                   
         LA    R3,2(R1,R3)                                                      
*                                                                               
         DROP  RF                                                               
*                                  LOOP FOR SPECIAL ELEMENTS                    
AC102    ZIC   R1,1(,R2)                                                        
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    AC112                                                            
         CLI   0(R2),X'23'         NUMBER ELEMENT                               
         BE    AC104                                                            
         CLI   0(R2),X'25'         EXTRA NUMBER ELEMENT                         
         BE    AC105                                                            
         CLI   0(R2),X'4C'         SUBSIDIARY POSTINGS ELEMENT                  
         BE    AC106                                                            
         CLI   0(R2),X'C0'         SUBSIDIARY POSTINGS ELEMENT                  
         BE    AC1061                                                           
         CLI   0(R2),X'4E'         POSTING TRANSFER ELEMENT                     
         BE    AC107                                                            
         CLI   0(R2),X'4F'         CLIENT ETC ELEMENT                           
         BE    AC107A                                                           
         CLI   0(R2),X'5F'         TAX ELEMENT                                  
         BE    AC107B                                                           
         CLI   0(R2),X'50'         CASH ELEMENT                                 
         BE    AC108                                                            
         CLI   0(R2),X'60'         ACTIVITY DATE ELEMENT                        
         BE    AC110                                                            
         CLI   0(R2),MPYELQ        MANUAL PAYMENT ELEMENT, X'64'                
         BE    AC111                                                            
         B     AC102                                                            
*                                  HANDLE NUMBER ELEMENT                        
*                                                                               
         USING ACOTHERD,R2                                                      
*                                                                               
AC104    LA    RE,PROFLIST                                                      
         CLC   QUNIT(2),=C'SR'                                                  
         BE    AC104A                                                           
         CLI   SORTTYPE,9          MEDIA TRANSFER                               
         BE    AC104A              SPECIAL OTHERS ELEMENT                       
*&&US                                                                           
         CLI   QOPT3,C'T'               SORTING BY TAX LOCALITY                 
         BNE   AC1041                                                           
         CLC   SORTSBAC+1(2),=C'SJ'     AND CONTRA IS SJ                        
         BNE   AC1041                                                           
         MVC   0(4,R3),=C'CLI='         ALSO DISPLAY THE CLIENT CODE            
         MVC   4(3,R3),SORTSBAC+3                                               
         MVI   7(R3),C','                                                       
         LA    R3,8(,R3)                                                        
*                                                                               
AC1041   MVC   0(4,R3),=C'PRD='    NON-RECEIVABLE LEDGER                        
         MVC   4(6,R3),ACOTNUM                                                  
         MVC   10(5,R3),=C',JOB='                                               
         MVC   15(6,R3),ACOTNUM+6                                               
         LA    R3,22(,R3)                                                       
*&&                                                                             
*&&UK                                                                           
         MVC   0(7,R3),=C'SUBREF='                                              
         MVC   7(6,R3),ACOTNUM                                                  
         LA    R3,14(,R3)                                                       
*&&                                                                             
         B     AC102                                                            
*                                                                               
AC104A   CLI   0(RE),X'FF'         LOOK-UP KEYWORD IN TABLE                     
         BE    AC104B                                                           
         CLC   0(1,RE),ACOTPROF                                                 
         BE    AC104B                                                           
         LA    RE,L'PROFLIST(,RE)                                               
         B     AC104A                                                           
*                                                                               
AC104B   MVC   0(4,R3),1(RE)                                                    
         MVC   4(L'ACOTNUM,R3),ACOTNUM                                          
         LA    R3,14(,R3)                                                       
         B     AC102                                                            
*                                  DEAL WITH EXTRA NUMBER ELEMENT               
*                                                                               
         USING ACNOD,R2                                                         
*                                                                               
AC105    MVC   0(4,R3),=C'ORD='                                                 
         MVC   4(6,R3),ACNO        ORDER NUMBER                                 
         LA    R3,10(,R3)                                                       
         B     AC102                                                            
*                                  HANDLE SUBSIDIARY POSTINGS ELEMENT           
*                                                                               
         USING TRSDESCD,R2                                                      
*                                                                               
AC106    CLI   PROGPROF+14,C'Y'    OPTION TO SUPPRESS ANALYSIS POSTING          
         BE    AC102                                                            
         SR    RE,RE                                                            
         LR    RF,R2                                                            
*                                                                               
AC106A   IC    RE,1(,RF)                                                        
         AR    RF,RE                                                            
         CLI   0(RF),X'C0'         IF TRANSACTION HAS C0 ELEMENT                
         BE    AC102               IGNORE 4C                                    
         CLI   0(RF),0                                                          
         BNE   AC106A                                                           
         MVC   0(25,R3),=C'ANALYSIS POSTINGS MADE TO'                           
         ZIC   R1,TRSDLEN                                                       
         SH    R1,=H'3'                                                         
         EXMVC R1,26(R3),TRSDACCS                                               
         LA    R3,27(R3,R1)                                                     
         B     AC102                                                            
*                                                                               
*                                                                               
         USING ACAPD,R2                                                         
*                                                                               
AC1061   CLI   PROGPROF+14,C'Y'    OPTION TO SUPPRESS POSTINGS                  
         BE    AC102                                                            
         MVI   BYTE,C'F'           FIRST TIME INDICATOR                         
         ZIC   R0,ACAPMIN          NUMBER OF MINI'S                             
         LTR   R0,R0                                                            
         BNP   AC102               IF NUMBER IS NOT POSITIVE SKIP LOGIC         
         LA    R5,ACAPMLEN         FIRST MINI                                   
         SR    R1,R1                                                            
*                                                                               
*                                                                               
         USING ACAPMLEN,R5                                                      
*                                                                               
AC1062   IC    R1,ACAPMLEN         LENGTH OF MINI                               
         SH    R1,=H'3'            ADJUST LENGTH FOR EX INSTRU.                 
         BNP   AC102               NEXT ELEM IF BAD MINI ELEMS                  
*                                                                               
AC1063   CLI   BYTE,C'F'                                                        
         BNE   *+18                                                             
         MVC   0(25,R3),=C'ANALYSIS POSTINGS MADE TO'                           
         LA    R3,26(,R3)                                                       
         MVI   BYTE,0                                                           
         EXMVC R1,0(R3),ACAPACCT   ACCOUNT CODE TO OUTPUT STRING                
         LA    R3,2(R1,R3)         R3 TO 1 PAST END OF ACCOUNT                  
*                                                                               
AC1064   LA    R5,3(R1,R5)         R5 TO NEXT MINI                              
         BCT   R0,AC1062                                                        
         B     AC102                                                            
*                                                                               
*                                                                               
         USING TRTRANSD,R2                                                      
*                                                                               
AC107    MVC   0(14,R3),=C'TRANSFERRED TO'                                      
         CLI   TRTRTYPE,C'T'                                                    
         BE    *+10                                                             
         MVC   12(4,R3),=C'FROM'                                                
         MVC   17(L'TRTRACC-1,R3),TRTRACC+1                                     
         MVC   32(2,R3),=C'ON'                                                  
         GOTO1 DATCON,DMCB,(1,TRTRDATE),(8,35(R3))                              
         LA    R3,44(,R3)                                                       
         B     AC102                                                            
*                                  HANDLE CLI/PROD/JOB ELEMENT                  
*                                                                               
         USING TRCPJD,R2                                                        
*                                                                               
AC107A   MVC   0(4,R3),=C'JOB='                                                 
         CLI   TRCPTYPE,C'J'                                                    
         BE    *+10                                                             
         MVC   0(3,R3),=C'EXP'                                                  
         MVC   WORK(18),TRCPCLI                                                 
         GOTO1 ADSQUASH,DMCB,WORK,18                                            
         MVC   4(18,R3),WORK                                                    
         LA    R3,22(,R3)                                                       
         B     AC102                                                            
*                                                                               
*                                                                               
         USING ACTAXD,R2                                                        
*                                                                               
AC107B   MVC   SORTLOC,ACTAXLOC+2  SAVE TAX INFO - LOCALITY CODE                
         ZAP   SORTBASE,ACTAXBAS                 - BASIS                        
         ZAP   SORTRATE,ACTAXRTE                 - RATE                         
         B     AC102                                                            
*                                  HANDLE CASH ELEMENT                          
*                                                                               
         USING TRCASHD,R2                                                       
*                                                                               
AC108    DS    0H                                                               
*&&UK*&& CLC   QUNIT(2),=C'1P'     HARD FOR TIME SHEET LEDGERS                  
*&&US*&& CLC   QUNIT(2),=C'1R'                                                  
         BE    AC109                                                            
         CLI   QOPT6,C'Y'          SPECIAL REQUEST OPTION                       
         BNE   AC102                                                            
*                                                                               
AC109    MVC   0(7,R3),=C'AMOUNT='                                              
         LA    R3,7(,R3)                                                        
         EDIT  (P6,TRCSAMNT),(10,0(R3)),2,ALIGN=LEFT,MINUS=YES                  
         AR    R3,R0                                                            
         MVI   0(R3),C'/'                                                       
         MVC   1(1,R3),TRCSTYPE                                                 
         LA    R3,3(,R3)                                                        
         B     AC102                                                            
*                                  HANDLE DATE ELEMENT                          
*                                                                               
         USING TRSTATD,R2                                                       
*                                                                               
AC110    CLI   QOPT6,C'Y'          SPECIAL REQUEST OPTION                       
         BNE   AC102                                                            
         MVC   0(5,R3),=C'DATE='                                                
         GOTO1 DATCON,DMCB,(2,TRSTDATE),(8,5(R3))                               
         CLI   5(R3),C' '                                                       
         BNE   *+8                                                              
         OI    5(R3),X'F0'                                                      
         LA    R3,14(,R3)                                                       
         B     AC102                                                            
*                                  HANDLE MANUAL PAYMENT ELEMENT                
*                                                                               
         USING MPYELD,R2                                                        
*                                                                               
AC111    DS    0H                                                               
         CLI   NARRSW,C'Y'                                                      
         BE    AC102                                                            
         CLC   MPYNO,SPACES                                                     
         BNH   AC102                                                            
         MVC   0(L'MPYNO,R3),MPYNO    CHECK NUMBER                              
         LA    R3,L'MPYNO+1(,R3)                                                
         GOTO1 DATCON,DMCB,(2,MPYDTE),(8,0(R3))                                 
         LA    R3,9(,R3)                                                        
         EDIT  MPYAMNT,(10,0(R3)),2,ALIGN=LEFT,MINUS=YES                        
         AR    R3,R0                                                            
         LA    R3,1(,R3)                                                        
         MVC   0(L'MPYBNK,R3),MPYBNK                                            
         LA    R3,L'MPYBNK+1(,R3)                                               
         B     AC102                                                            
*                                  DISPLAY TAX INFO                             
AC112    CLI   QOPT3,C'T'          SORT BY TAX INFO                             
         BNE   AC11201                                                          
         MVC   SORTSBAC,SPACES                                                  
         MVC   SORTSBAC(9),SORTLOC    SORT BY LOCALITY                          
         B     AC11202                                                          
*                                                                               
AC11201  CP    SORTRATE,=P'0'                                                   
         BE    AC11204             NO TAX INFO TO DISPLAY                       
         MVC   0(40,R3),SPACES                                                  
         MVC   0(9,R3),=C'LOCALITY='                                            
         MVC   9(8,R3),SORTLOC+1                                                
*                                                                               
AC11202  MVC   25(5,R3),=C'RATE='                                               
         EDIT  (P4,SORTRATE),(8,30(R3)),4,DROP=3,ALIGN=LEFT                     
         MVC   40(6,R3),=C'BASIS='                                              
         EDIT  (P6,SORTBASE),(10,46(R3)),2,MINUS=YES,ALIGN=LEFT                 
         GOTO1 ADSQUASH,DMCB,(R3),55                                            
         L     R0,4(,R1)                                                        
         AR    R3,R0                                                            
*                                  OTHER USEFULL INFO                           
AC11204  TM    SORTSTAT,X'40'                                                   
         BZ    *+14                                                             
         MVC   0(3,R3),=C'*U*'     URGENT MARKER                                
         LA    R3,4(,R3)                                                        
*                                                                               
         CLI   SYSPROF+1,C'Y'      INVOICE REGISTER PROFILE                     
         BNE   AC113                                                            
         CLI   QUNIT,C'S'          INTERESTED ONLY IN CERTAIN LEDGERS           
         BNE   AC113                                                            
         CLI   QLEDGER,C'J'                                                     
         BE    AC112A                                                           
         CLI   QLEDGER,C'V'                                                     
         BE    AC112A                                                           
         CLI   QLEDGER,C'E'                                                     
         BE    AC112A                                                           
         B     AC113                                                            
*                                                                               
AC112A   MVC   0(4,R3),=C'AUTH'                                                 
         TM    SORTSTAT,X'08'                                                   
         BO    *+10                                                             
         MVC   0(6,R3),=C'UNAUTH'                                               
         LA    R3,6(,R3)                                                        
*                                                                               
AC113    CLI   QOPT6,C'Y'          OTHER SPECIAL REQUEST OPTIONS                
         BNE   AC114                                                            
         MVC   0(5,R3),=C'TYPE='   INPUT TYPE                                   
         ZIC   R1,SORTTYPE                                                      
         EDIT  (R1),(3,5(R3)),ALIGN=LEFT                                        
         OI    5(R3),X'F0'                                                      
         LA    R3,9(,R3)                                                        
         MVC   0(7,R3),=C'STATUS=' TRANSACTION STATUS                           
         GOTO1 HEXOUT,DMCB,SORTSTAT,7(R3),1,=C'TOG'                             
         LA    R3,10(,R3)                                                       
*                                  SQUASH NARRATIVE & SET LENGTH                
AC114    LA    R0,SORTNARR                                                      
         SR    R3,R0                                                            
         LTR   R3,R3                                                            
         GOTO1 ADSQUASH,DMCB,SORTNARR,(R3)                                      
         L     R0,4(,R1)                                                        
         AH    R0,SORTRLEN                                                      
         STH   R0,SORTRLEN                                                      
*                                  ADD AMOUNT TO ACCUMS                         
AC116    LA    R0,1                                                             
         TM    SORTSTAT,X'80'                                                   
         BO    *+8                                                              
         LA    R0,2                                                             
         GOTO1 PROLLER,DMCB,3,ACCUMS,SORTAMNT,1,(R0)                            
         GOTO1 (RF),(R1),6         CROSS CAST & ROLL                            
*                                  HANDLE OFFICE/ANALYSIS POSTING               
         CLI   ANALSW,C'N'                                                      
         BNE   AC119                                                            
         MVC   THISANAL,SORTOFFC                                                
*                                                                               
         CLI   SORTSW,C'Y'                                                      
         BE    *+8                                                              
         BAS   RE,POSTANAL                                                      
*                                  SORT OR PRINT TRANSACTION                    
AC119    CLI   SORTSW,C'Y'                                                      
         BNE   AC120                                                            
         CLI   QOPT7,C'P'                                                       
         BNE   AC119A                                                           
         MVC   P(132),0(R6)                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
AC119A   DS    0H                                                               
         MVC   SCHOPBL,CHOPBLOC                                                 
         MVC   SCHOPNM,CHOPNUM                                                  
         MVC   STRNCNM,THISSBNM                                                 
         MVC   STRNMAX,MAXLVL                                                   
         GOTO1 ADSORTER,DMCB,=C'PUT',(R6)                                       
         B     AC122                                                            
*                                                                               
AC120    CLI   QOPT2,C'N'                                                       
         BE    *+8                                                              
         BAS   RE,PRNTTRNS                                                      
*                                                                               
AC122    MVI   TRNSSW,C'Y'                                                      
         AP    TRNSCNT,=P'1'                                                    
         GOTO1 PROLLER,DMCB,2,ACCUMS,1                                          
         B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        LAST FOR SUB-ACCOUNT                                                   
*---------------------------------------------------------------------*         
AC130    NTR1                                                                   
         GOTO1 PROLLER,DMCB,1,ACCUMS,9                                          
         L     R2,DMCB                                                          
         CLI   TRNSSW,C'Y'         ANY ACTIVITY FOR THIS SUB-ACCOUNT            
         BNE   AC132                                                            
*                                                                               
         CLI   MODE,LEDGLAST                                                    
         BE    AC131                                                            
*                                                                               
         CLI   SORTSW,C'N'                                                      
         BNE   AC131                                                            
         CLI   QOPT2,C'N'          SUMMARY ONLY                                 
         BE    *+8                                                              
         BAS   RE,PRNTTRNS         NO - PRINT LAST TRANSACTION                  
*                                                                               
AC131    DS    0H                                                               
         CLI   CHOPNUM,0                                                        
         BE    AC132                                                            
         CP    0(8,R2),=P'0'                                                    
         BNE   *+14                                                             
         CP    8(8,R2),=P'0'                                                    
         BE    AC132                                                            
         SR    R1,R1                                                            
         IC    R1,CHOPNUM          R1 NUMBER OF LEVELS IN CONTRA                
         BCTR  R1,0                                                             
         MH    R1,=H'51'           LENGTH OF CHOP BLOCK                         
         LA    R3,CHOPBLOC(R1)     R3 TO LAST (LOW) LEVEL                       
         LA    R4,1                R4 TO DETAIL LEVEL                           
*                                                                               
         CLI   SORTSW,C'Y'                                                      
         BE    *+8                                                              
         BAS   RE,POSTSBAC                                                      
         CLI   LEDGPROF+8,C'Y'     OPTION TO CHOP CONTRA                        
         BNE   AC132               NO FURTHER POSTINGS                          
         LA    R3,CHOPBLOC         R3 TO FIRST (HIGH) LEVEL                     
         LA    R4,5                R4 TO HIGH LEVEL TOTAL                       
*                                                                               
         CLI   SORTSW,C'Y'                                                      
         BE    *+8                                                              
         BAS   RE,POSTSBAC         POST THE HIGH LEVEL                          
         CLI   CHOPNUM,1                                                        
         BNH   AC132               ONLY ONE LEVEL TO POST                       
*                                                                               
         LA    R4,4                R4 TO LEVEL CONTROL                          
         SR    R1,R1                                                            
         IC    R1,CHOPNUM          R1 NUMBER OF LEVELS IN CONTRA                
         BCTR  R1,0                LESS ONE                                     
         SR    RF,RF                                                            
         IC    RF,MAXLVL           RF MAXIMUM NUMBER                            
         SH    RF,=H'2'            LESS TWO                                     
         BP    *+8                                                              
         LA    RF,10               FORCE RF HIGH                                
         CR    R1,RF                                                            
         BL    *+6                                                              
         LR    R1,RF                                                            
*                                                                               
AC131A   DS    0H                                                               
         LA    R3,51(,R3)          R3 TO NEXT CHOP LEVEL                        
*                                                                               
         CLI   SORTSW,C'Y'                                                      
         BE    *+8                                                              
         BAS   RE,POSTSBAC         POST EACH LEVEL                              
         BCTR  R4,0                ADJUST LEVEL CONTROL                         
         BCT   R1,AC131A                                                        
         LA    R3,CHOPBLOC         R3 TO FIRST (HIGH) LEVEL                     
         LA    R3,51(,R3)          R3 TO NEXT CHOP LEVEL                        
*                                                                               
AC132    DS    0H                                                               
         MVI   TRNSSW,C'N'                                                      
         CLI   SORTSW,C'Y'                                                      
         BE    EXIT                                                             
         GOTO1 PROLLER,DMCB,2,ACCUMS,9                                          
         CLI   MODE,LEDGLAST                                                    
         BE    EXIT                                                             
         GOTO1 PROLLER,DMCB,2,ACCUMS,2                                          
         B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*              LAST FOR ANALYSIS CODE                                           
*---------------------------------------------------------------------*         
AC140    NTR1                                                                   
         CLI   SORTSW,C'Y'                                                      
         BE    *+8                                                              
         BAS   RE,POSTANAL                                                      
         B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        LAST FOR ACCOUNT                                                       
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING ACMD,R2                                                          
         SPACE 1                                                                
AC150    NTR1                                                                   
         L     R2,AMONACC          GET MONACC ADDRESS                           
         SR    R6,R6                                                            
*                                                                               
         TM    SORTMODE,SORTALL                                                 
         BNO   AC151                                                            
         TM    SORTMODE,SORTPRNT                                                
         BO    AC151E                                                           
*                                                                               
AC151    DS    0H                                                               
         ZAP   MACMABAL,ACMABAL                                                 
         AP    LDGBLFWD,MACMABAL   ADD MONACC BAL TO LEDGER BAL                 
*                                                                               
AC151E   CP    TRNSCNT,=P'0'       ANY TRANSACTIONS ?                           
         BNE   AC154                                                            
         CP    MACMABAL,=P'0'      NO - EXIT IF NO BALANCE FORWARD              
         BE    AC158                                                            
         CLI   QOPT1,C'S'          OPTION TO SUPPRESS INACTIVES                 
         BE    AC152                                                            
         CLI   QOPT2,C'N'          DON'T PRINT IF SUMMARY                       
         BE    AC158                                                            
*                                                                               
         CLI   SORTSW,C'Y'                                                      
         BE    AC158                                                            
         BAS   RE,PRNTTRNS                                                      
         B     AC158                                                            
*                                                                               
AC152    CLI   FLTSW,C'Y'          MAY NOT BE INACTIVE IF FILTERING             
         BE    AC158                                                            
         BAS   RE,POSTACCT         POST TO INACTIVE ACCOUNT BUFFER              
         CLI   QOPT2,C'N'                                                       
         BE    AC158                                                            
         B     AC156                                                            
*                                                                               
AC154    DS    0H                                                               
         CLI   QOPT2,C'N'                                                       
         BE    AC157                                                            
*&&UK                                                                           
         BAS   RE,PRNTTRNS         PRINT BALANCE FORWARD                        
         CLI   LEDGPROF+10,C'N'    OPTION TO SUPPRESS W/C ANALYSIS              
         BE    AC155                                                            
         CP    ANALCNT,=P'0'                                                    
         BE    *+12                                                             
         MVI   SAVELVL,1                                                        
         BAS   RE,PRNTANAL         PRINT WORK-CODE SUMMARY                      
*&&                                                                             
*&&US                                                                           
         CLI   LEDGPROF+10,C'N'    OPTION TO SUPPRESS W/C ANALYSIS              
         BE    AC154A                                                           
         CP    ANALCNT,=P'0'                                                    
         BE    AC154A                                                           
         CLI   SAVESW,C'Y'         HAS LAST RECORD BEEN PRINTED?                
         BNE   AC154AA             NO                                           
*                                                                               
         CLI   SORTSW,C'Y'                                                      
         BE    AC154AA                                                          
*                                                                               
         MVI   MODE,PROCTRNS       TRYING TO DUP PRNTSORT                       
         BAS   RE,PRNTTRNS         PRINT IT                                     
         MVI   MODE,ACCLAST                                                     
         MVI   SAVESW,C'N'                                                      
*                                                                               
AC154AA  MVI   SAVELVL,1                                                        
         CLI   SORTSW,C'Y'                                                      
         BE    AC154A                                                           
         BAS   RE,PRNTANAL         PRINT WORK-CODE SUMMARY                      
         GOTO1 ACREPORT                                                         
*                                                                               
AC154A   DS    0H                                                               
         CLI   SORTSW,C'Y'                                                      
         BE    AC155                                                            
         BAS   RE,PRNTTRNS         PRINT BALANCE FORWARD                        
*&&                                                                             
*                                                                               
AC155    CLI   QOPT2,C' '          ARE WE PRINTING SUMMARIES                    
         BE    AC156                                                            
         CLI   LEDGPROF+5,C'Y'                                                  
         BNE   AC156                                                            
*                                                                               
         CLI   SORTSW,C'Y'                                                      
         BE    AC157                                                            
*                                                                               
         MVI   SAVELVL,1                                                        
         BAS   RE,PRNTSM           PRINT CONTRA-ACCOUNT SUMMARY                 
*                                                                               
AC156    GOTO1 ,DMCB,=C'ADD',ADBUFF,1,(X'80',2)                                 
         L     RF,BUFFALO                                                       
         CLI   QOPT2,C'Y'                                                       
         BE    AC156A                                                           
*&&US                                                                           
         B     AC156B                                                           
*&&                                                                             
*&&UK                                                                           
         CLI   QOPT1,C'S'                                                       
         BNE   AC156B                                                           
*&&                                                                             
*                                                                               
AC156A   BASR  RE,RF                                                            
*                                                                               
         CLI   QOPT7,C'B'                                                       
         BNE   AC156B                                                           
         MVC   P(11),=C'BUFFALO ADD'                                            
         GOTO1 ACREPORT                                                         
*                                                                               
AC156B   DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'CLEAR',,(X'80',1)                                
*                                                                               
         CLI   QOPT7,C'B'                                                       
         BNE   AC157                                                            
         MVC   P(11),=C'BUFFALO CLR'                                            
         GOTO1 ACREPORT                                                         
*                                                                               
AC157    GOTO1 PROLLER,DMCB,2,ACCUMS,4                                          
*                                                                               
AC158    DS    0H                                                               
         CLI   SORTSW,C'Y'                                                      
         BNE   EXIT                                                             
*                                                                               
         USING ACKEYD,R2                                                        
*                                                                               
AC158B   L     R2,ADACC                                                         
         BAS   RE,CLRSREC                                                       
         L     R6,ASRTRECA                                                      
*                                                                               
         USING SRTRECD,R6                                                       
*                                                                               
         MVC   SORTACCT,ACKEYACC                                                
         MVI   SRTRECT,SACCLTYP                                                 
         ZAP   SLDGBALF,MACMABAL                                                
         LA    R5,SACCLLEN                                                      
         STH   R5,SORTRLEN                                                      
         LA    R4,SORTLEN                                                       
         CR    R5,R4                                                            
         BH    *+8                                                              
         STH   R4,SORTRLEN                                                      
         CLI   QOPT7,C'P'                                                       
         BNE   AC159                                                            
         MVC   P(132),0(R6)                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
AC159    GOTO1 ADSORTER,DMCB,=C'PUT',(R6)                                       
         B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        LAST FOR HIGH LEVEL ACCOUNTS/LEDGER                                    
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING SRTRECD,R6                                                       
         SPACE 1                                                                
AC160    NTR1                                                                   
         BAS   RE,CLRSREC                                                       
         L     R6,ASRTRECA                                                      
*                                                                               
         USING ACHEIRD,R2                                                       
*                                                                               
         L     R2,ADLDGHIR         R2=A(HEIRARCHY ELEMENT)                      
         LA    R3,5                R3=ACCUM LINE NUMBER                         
         MVI   SRTRECT,SLVCTYP                                                  
         L     R4,ADLVCNAM         R4=A(NAME ELEMENT)                           
         LA    R5,ACHRDESC         R5=A(LEVEL DESCRIPTION)                      
         ZAP   TEMPCNT,LEVCCNT                                                  
         CLI   MODE,LEVCLAST                                                    
         BE    AC162                                                            
         LA    R3,6                                                             
         MVI   SRTRECT,SLVBTYP                                                  
         L     R4,ADLVBNAM                                                      
         LA    R5,ACHRDESB                                                      
         ZAP   TEMPCNT,LEVBCNT                                                  
         CLI   MODE,LEVBLAST                                                    
         BE    AC162                                                            
         LA    R3,7                                                             
         MVI   SRTRECT,SLVATYP                                                  
         L     R4,ADLVANAM                                                      
         LA    R5,ACHRDESA                                                      
         ZAP   TEMPCNT,LEVACNT                                                  
         CLI   MODE,LEVALAST                                                    
         BE    AC162                                                            
         LA    R3,8                                                             
         L     R4,ADLDGNAM                                                      
         LA    R5,=CL15'LEDGER'                                                 
         ZAP   TEMPCNT,=P'1'                                                    
*                                                                               
AC162    GOTO1 PROLLER,DMCB,1,ACCUMS,(R3)                                       
         L     R2,DMCB                                                          
         ZAP   FORMACCS,0(8,R2)                                                 
         ZAP   FORMACCS+8,8(8,R2)                                               
         MVC   0(24,R2),=3PL8'0'   CLEAR THIS LEVEL ACCUMS                      
         CLI   QOPT2,C'N'                                                       
         BE    AC168                                                            
         CLI   QOPT4,C'S'          OPTION TO SUPPRESS OTHER TOTALS              
         BE    AC168                                                            
*                                                                               
         CLI   MODE,LEDGLAST                                                    
         BNE   AC162F                                                           
         CLC   QACCOUNT,SPACES     NO LEDGER TOTALS IF ACCOUNT REQUEST          
         BE    AC162F                                                           
         CP    ACCCNT,=P'1'                                                     
         BNH   AC168                                                            
         LA    R5,=CL15'REQUEST'                                                
         SR    R4,R4                                                            
*                                                                               
AC162F   MVC   P+24(10),=C'TOTALS FOR'                                          
         MVC   P+35(15),0(R5)                                                   
*                                                                               
         TM    SORTMODE,SORTALL                                                 
         BNO   AC162K                                                           
         TM    SORTMODE,SORTPRNT                                                
         BO    AC162H                                                           
         MVC   SACLDESC,0(R5)                                                   
         XC    SACLNAME,SACLNAME                                                
         LTR   R4,R4                                                            
         BZ    AC162K                                                           
         ZIC   R5,1(,R4)                                                        
         BCTR  R5,0                                                             
         EXMVC R5,SACLNAME,0(R4)                                                
         B     AC162K                                                           
*                                                                               
AC162H   DS    0H                                                               
         CLI   MODE,LEDGLAST                                                    
         BE    AC162K                                                           
         L     R6,BACKSORT                                                      
         MVC   P+35(15),SACLDESC                                                
         SR    R4,R4                                                            
         CLI   SACLNAME+1,0                                                     
         BE    AC162K                                                           
         LA    R4,SACLNAME                                                      
*                                                                               
AC162K   LTR   R4,R4                                                            
         BZ    AC164                                                            
         ZIC   R5,1(,R4)           GET LENGTH OF NAME                           
         SH    R5,=H'3'                                                         
         CP    FORMACCS,=P'0'      ANYTHING LEFT TO PRINT                       
         BNE   AC163                                                            
         CP    FORMACCS+8,=P'0'                                                 
         BNE   AC163                                                            
*&&UK                                                                           
         CLI   MODE,LEDGLAST       FOR A UNIT WIDE REQUEST AT                   
         BNE   AC162P              LEDGLAST - SKIP THE TEST AND PRINT           
         CLI   QLEDGER,C' '        THE LEDGER BAL FRWD TOTALS                   
         BE    AC163                                                            
*                                                                               
AC162P   DS    0H                                                               
*&&                                                                             
         CLI   SAVESW,C'Y'         NO - ANYTHING PRINTED PREVIOUSLY             
         BNE   AC168               NO                                           
*                                                                               
AC163    MVC   WORK,SPACES                                                      
         EXMVC R5,WORK,2(R4)       MOVE NAME TO WORK                            
         LA    R4,P+50                                                          
         CLI   0(R4),C' '          FIND END OF DESCRIPTION                      
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         LA    R5,P+69                                                          
         CLI   PRTSW,C'N'                                                       
         BE    *+8                                                              
         LA    R5,P+81                                                          
         SR    R5,R4                                                            
         GOTO1 CHOPPER,DMCB,(36,WORK),((R5),2(R4)),(C'P',2)                     
*                                                                               
AC164    MVI   FORMSW,C'A'                                                      
         BAS   RE,FORMACC                                                       
         CP    FORMACCS,=P'0'                                                   
         BNE   AC165                                                            
         CP    FORMACCS+8,=P'0'                                                 
         BNE   AC165                                                            
         MVC   P+106(3),=C'NIL'                                                 
*                                                                               
AC165    MVC   PTHIRD,PSECOND                                                   
         MVC   PSECOND,P                                                        
         MVC   P,SPACES                                                         
         CLI   MODE,LEDGLAST                                                    
         BNE   AC167                                                            
         CLI   FLTSW,C'Y'          NO LEDGER BALFWD IF FILTERING                
         BE    AC167                                                            
*                                                                               
         CLI   SORTSW,C'Y'                                                      
         BE    *+8                                                              
         BAS   RE,REPORT                                                        
         CP    LDGBLFWD,=P'0'      NO LEDGER B/F IF ZERO                        
         BE    AC166                                                            
         MVC   P+24(23),=C'BALANCE BROUGHT FORWARD'                             
         CLI   PRTSW,C'N'                                                       
         BE    *+10                                                             
         MVC   P+24(23),=C'TOTAL BROUGHT FORWARD  '                             
         ZAP   FORMACCS+24,LDGBLFWD                                             
         MVI   FORMSW,C'B'                                                      
         BAS   RE,FORMACC                                                       
*                                                                               
         CLI   SORTSW,C'Y'                                                      
         BE    *+8                                                              
         BAS   RE,REPORT                                                        
*                                                                               
AC166    MVC   P+24(23),=C'BALANCE CARRIED FORWARD'                             
         CLI   PRTSW,C'N'                                                       
         BE    *+10                                                             
         MVC   P+24(23),=C'TOTAL CARRIED FORWARD  '                             
         ZAP   FORMACCS+24,LDGBLFWD                                             
         MVI   FORMSW,C'C'                                                      
         BAS   RE,FORMACC                                                       
*                                                                               
AC167    DS    0H                                                               
         CLI   SORTSW,C'Y'                                                      
         BE    *+8                                                              
         BAS   RE,REPORT                                                        
*                                                                               
AC168    DS    0H                                                               
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVC   PFOURTH,SPACES                                                   
         CLI   MODE,LEDGLAST                                                    
         BE    AC170               PRINT SUMMARIES AT LEDGLAST                  
*                                                                               
         CLI   SORTSW,C'Y'                                                      
         BNE   EXIT                                                             
         CP    TEMPCNT,=P'0'       NO - ANYTHING PRINTED PREVIOUSLY             
         BE    EXIT                NO                                           
*                                                                               
         L     R6,ASRTRECA                                                      
         L     R2,ADACC                                                         
*                                                                               
         USING ACKEYD,R2                                                        
*                                                                               
         MVC   SORTACCT,ACKEYACC                                                
         LA    R5,SACCLLEN                                                      
         STH   R5,SORTRLEN                                                      
         LA    R4,SORTLEN                                                       
         CR    R5,R4                                                            
         BH    *+8                                                              
         STH   R4,SORTRLEN                                                      
         CLI   QOPT7,C'P'                                                       
         BNE   AC169                                                            
         MVC   P(132),0(R6)                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
AC169    GOTO1 ADSORTER,DMCB,=C'PUT',(R6)                                       
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        LAST FOR LEDGER                                                        
*---------------------------------------------------------------------*         
AC170    DS    0H                                                               
         MVI   SAVELVL,2                                                        
         MVC   HALF2,PAGE                                                       
         CLI   QOPT2,C'N'          USE ACCOUNT LEVEL IF SUMMARY ONLY            
         BNE   *+8                                                              
         MVI   SAVELVL,1                                                        
         MVC   THISHED1,SPACES                                                  
         MVC   THISHED2,SPACES                                                  
         MVC   THISHED3,SPACES                                                  
         MVC   THISHED4,SPACES                                                  
         MVC   THISHED5,SPACES                                                  
         MVC   THISHED6,SPACES                                                  
         MVC   THISHED7,SPACES                                                  
         CLI   QOPT2,C' '                                                       
         BE    AC172                                                            
         MVC   PAGE,=H'1'                                                       
*                                                                               
         CLI   SORTSW,C'Y'                                                      
         BE    AC172                                                            
         BAS   RE,PRNTANAL                                                      
         MVC   PAGE,=H'1'                                                       
         BAS   RE,PRNTSM                                                        
*                                                                               
AC172    MVC   PAGE,=H'1'                                                       
         CLI   QOPT1,C'S'                                                       
         BNE   *+8                                                              
         BAS   RE,PRNTACCT                                                      
         CLI   QOPT6,C'X'                                                       
         BNE   EXIT                SPECIAL CONSOLE OUTPUT                       
         MVC   P(12),=C'TOTAL DEBITS'                                           
         LA    R2,FORMACCS                                                      
         BAS   RE,LOGED                                                         
         GOTO1 LOGIO,DMCB,1,(30,P)                                              
         MVC   P+12(20),SPACES                                                  
         MVC   P+6(7),=C'CREDITS'                                               
         LA    R2,FORMACCS+8                                                    
         BAS   RE,LOGED                                                         
         GOTO1 LOGIO,DMCB,1,(30,P)                                              
         MVC   P(30),SPACES                                                     
         ZAP   DOUBLE,FORMACCS+24                                               
         AP    DOUBLE,FORMACCS                                                  
         SP    DOUBLE,FORMACCS+8                                                
         LA    R2,DOUBLE                                                        
         BAS   RE,LOGED                                                         
         MVC   P(11),=C'BALANCE C/F'                                            
         GOTO1 LOGIO,DMCB,1,(30,P)                                              
         MVC   P(30),SPACES                                                     
         LH    R2,HALF2                                                         
         BCTR  R2,0                                                             
         EDIT  (R2),(5,P+12)                                                    
         MVC   P(11),=C'TOTAL PAGES'                                            
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
LOGED    DS    0H                                                               
         EDIT  (P8,0(R2)),(12,P+14),2,MINUS=YES                                 
         BR    RE                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        GET RECORDS FROM SORT                                                  
*---------------------------------------------------------------------*         
AC200    NTR1                                                                   
         CLI   SORTSW,C'Y'                                                      
         BNE   EXIT                                                             
         GOTO1 PROLLER,DMCB,0,(8,ACCUMS),9,3                                    
         ZAP   ACCCNT,=P'0'                                                     
         MVC   PAGE,=H'1'                                                       
         MVC   RCSUBPRG,SUBPROG                                                 
*                                                                               
AC205    DS    0H                                                               
         GOTO1 ADSORTER,DMCB,=C'GET'                                            
         L     R6,4(,R1)                                                        
         LTR   R6,R6                                                            
         BZ    AC290                                                            
         ST    R6,BACKSORT                                                      
         CLI   QOPT7,C'G'                                                       
         BNE   AC208                                                            
         MVC   P(132),0(R6)                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
AC208    DS    0H                                                               
*                                                                               
         USING SRTRECD,R6                                                       
*                                                                               
*******                                                                         
         MVC   CHOPBLOC,CHOPSAV2                                                
         MVC   CHOPSAV2,SCHOPBL                                                 
*                                                                               
         MVC   CHOPNUM,CHOPNSV                                                  
         MVC   CHOPNSV,SCHOPNM                                                  
*******                                                                         
         CLI   SRTRECT,SLEGFTYP                                                 
         BNE   AC210                                                            
         MVC   FLTSW,SRTFLTSW                                                   
         MVC   PRTSW,SRTPRTSW                                                   
         B     AC205                                                            
*                                                                               
         USING SRTRECD,R6                                                       
*                                                                               
AC210    CLI   SRTRECT,SACCTYP                                                  
         BNE   AC220                                                            
         MVC   THSACC,SORTACCT                                                  
         MVC   THISNAME,SACCNM                                                  
         MVC   THSACCT,SACCFORM                                                 
         MVC   THISHED1,SHEAD1                                                  
         MVC   THISHED2,SHEAD2                                                  
         MVC   THISHED3,SHEAD3                                                  
         MVC   THISHED4,SHEAD4                                                  
         MVC   THISHED5,SHEAD5                                                  
         MVC   THISHED6,SHEAD6                                                  
         MVC   THISHED7,SHEAD7                                                  
         ZAP   TRNSCNT,=P'0'                                                    
         ZAP   ANALCNT,=P'0'                                                    
         AP    ACCCNT,=P'1'                                                     
         MVI   ACCSW,C'F'                                                       
         MVI   ANALSW,C'N'                                                      
         MVI   FRSTTRN,C'Y'                                                     
         MVI   TRNSSW,C'N'                                                      
         XC    SAVSBAC2,SAVSBAC2                                                
         B     AC205                                                            
*                                                                               
AC220    CLI   SRTRECT,SANATYP                                                  
         BNE   AC240                                                            
         MVI   ANALSW,C'F'                                                      
         CLI   QOPT2,C'N'                                                       
         BNE   *+8                                                              
         MVI   ANALSW,C'Y'                                                      
         CLI   SORTSW,C'N'                                                      
         BE    *+8                                                              
         MVI   ANALSW,C'Y'                                                      
         B     AC205                                                            
*                                                                               
AC240    CLI   SRTRECT,STRNTYP                                                  
         BNE   AC255                                                            
         OI    SORTMODE,SORTPRNT                                                
         OC    SAVSBAC2,SAVSBAC2                                                
         BZ    AC241                                                            
         CLC   SORTSBAC,SAVSBAC2                                                
         BE    AC241                                                            
         MVI   SORTSW,C'N'                                                      
         MVI   SBACSW,C'F'                                                      
         BAS   RE,AC130                                                         
         MVI   SORTSW,C'Y'                                                      
         MVI   TRNSSW,C'N'                                                      
*                                                                               
AC241    DS    0H                                                               
         MVC   CHOPBLOC,CHOPSAV2                                                
         MVC   CHOPSAV2,SCHOPBL                                                 
*                                                                               
         MVC   CHOPNUM,CHOPNSV                                                  
         MVC   CHOPNSV,SCHOPNM                                                  
*                                                                               
         MVC   THISANAL,SORTOFFC                                                
         MVC   THISSBAC,SORTSBAC                                                
         MVC   THISSBNM,STRNCNM                                                 
         MVC   MAXLVL,STRNMAX                                                   
         MVC   SAVSBAC2,SORTSBAC                                                
*                                                                               
         CLI   ANALSW,C'N'                                                      
         BNE   AC242                                                            
         AP    ANALCNT,=P'1'                                                    
         MVC   THISANAL,SORTOFFC                                                
*                                                                               
AC242    DS    0H                                                               
         MVI   MODE,LEDGLAST                                                    
         BAS   RE,PRNTSORT                                                      
         GOTO1 PROLLER,DMCB,4,ACCUMS,1,3                                        
         GOTO1 PROLLER,DMCB,4,ACCUMS,1,4                                        
         GOTO1 PROLLER,DMCB,4,ACCUMS,1,5                                        
         GOTO1 PROLLER,DMCB,4,ACCUMS,1,6                                        
         GOTO1 PROLLER,DMCB,4,ACCUMS,1,7                                        
         GOTO1 PROLLER,DMCB,4,ACCUMS,1,8                                        
         GOTO1 PROLLER,DMCB,4,ACCUMS,1,9      LINE 9 FOR CONTRA SUMMARY         
         MVI   MODE,PROCTRNS                                                    
         MVI   FRSTTRN,C'N'                                                     
         AP    TRNSCNT,=P'1'                                                    
         MVI   TRNSSW,C'Y'                                                      
         BAS   RE,POSTANAL                                                      
         MVI   MODE,LEDGLAST       RESET MODE                                   
         GOTO1 PROLLER,DMCB,2,ACCUMS,1                                          
         B     AC205                                                            
*                                                                               
AC255    DS    0H                                                               
         CLI   SRTRECT,SACCLTYP                                                 
         BNE   AC260                                                            
         MVI   SORTSW,C'N'                                                      
         BAS   RE,AC130                                                         
         MVI   SORTSW,C'Y'                                                      
         MVI   TRNSSW,C'N'                                                      
         MVI   FRSTTRN,C'Y'                                                     
         CP    TRNSCNT,=P'0'                                                    
         BE    AC257                                                            
         CLI   QOPT3,C'T'                                                       
         BE    AC256                                                            
         ZIC   RE,LEDGPROF+9                                                    
         LTR   RE,RE                                                            
         BZ    AC257                                                            
*                                                                               
AC256    MVI   MODE,SBACLAST                                                    
         BAS   RE,PRNTTRNS                                                      
*                                                                               
AC257    ZAP   MACMABAL,SLDGBALF                                                
         MVI   MODE,ACCLAST                                                     
         MVC   THSACC,SORTACCT                                                  
         MVI   SORTSW,C'N'                                                      
         BAS   RE,AC150                                                         
         MVI   MODE,LEDGLAST                                                    
         MVI   SORTSW,C'Y'                                                      
         B     AC205                                                            
*                                                                               
AC260    DS    0H                                                               
         CLI   SRTRECT,SLVCTYP                                                  
         BNE   AC270                                                            
         MVI   MODE,LEVCLAST                                                    
         MVI   SORTSW,C'N'                                                      
         BAS   RE,AC160                                                         
         MVI   MODE,LEDGLAST                                                    
         MVI   SORTSW,C'Y'                                                      
         B     AC205                                                            
*                                                                               
AC270    DS    0H                                                               
         CLI   SRTRECT,SLVBTYP                                                  
         BNE   AC280                                                            
         MVI   MODE,LEVBLAST                                                    
         MVI   SORTSW,C'N'                                                      
         BAS   RE,AC160                                                         
         MVI   MODE,LEDGLAST                                                    
         MVI   SORTSW,C'Y'                                                      
         B     AC205                                                            
*                                                                               
AC280    DS    0H                                                               
         CLI   SRTRECT,SLVATYP                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MODE,LEVALAST                                                    
         MVI   SORTSW,C'N'                                                      
         BAS   RE,AC160                                                         
         MVI   MODE,LEDGLAST                                                    
         MVI   SORTSW,C'Y'                                                      
         B     AC205                                                            
*                                                                               
AC290    DS    0H                                                               
         MVI   MODE,LEDGLAST                                                    
         MVI   SORTSW,C'N'                                                      
         BAS   RE,AC160                                                         
         MVI   MODE,LEDGLAST                                                    
         MVI   SORTSW,C'Y'                                                      
*                                                                               
AC300    DS    0H                                                               
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        BUILD OLD OFFICE BUFFER                                                
*---------------------------------------------------------------------*         
BLOLD    NTR1  ,                                                                
         MVC   THSKEY,SPACES                                                    
         MVC   THSKEY(1),RCCOMPFL SET-UP KEY FOR FIRST OFFICE                   
         MVC   THSKEY+1(2),=C'2D'                                               
         MVI   THSKEY+3,X'41'                                                   
         L     R3,ASRTREC                                                       
*                                                                               
         USING ACKEYD,R3                                                        
*                                                                               
BLOLD2   GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCOUNT',THSKEY,ASRTREC           
         CLI   DMCB+8,0                                                         
         BNE   BLOLDX                                                           
         CLC   ACKEYACC(3),THSKEY                                               
         BNE   BLOLDX                                                           
         CLC   ACKEYACC+4(11),SPACES                                            
         BNE   BLOLDX                                                           
         LR    R1,R3                                                            
         BAS   RE,GETNAME                                                       
         MVC   0(1,R4),ACKEYACC+3                                               
         MVI   1(R4),C' '                                                       
         MVC   2(36,R4),WORK                                                    
         LA    R4,38(,R4)                                                       
         MVI   0(R4),X'FF'                                                      
*                                                                               
BLOLD4   ZIC   R1,ACKEYACC+3       BUMP TO NEXT OFFICE                          
         LA    R1,1(,R1)                                                        
         STC   R1,THSKEY+3                                                      
         B     BLOLD2                                                           
*                                                                               
BLOLDX   B     EXIT                                                             
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        BUILD NEW OFFICE BUFFER                                                
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING OFFRECD,R3                                                       
         SPACE 1                                                                
BLNEW    NTR1  ,                                                                
         LA    R3,THSKEY                                                        
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,RCCOMPFL                                                 
         L     R3,ASRTREC                                                       
*                                                                               
BLNEW2   GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCOUNT',THSKEY,ASRTREC           
         CLI   DMCB+8,0                                                         
         BNE   BLNEWX                                                           
         CLC   OFFKEY(OFFKOFF-OFFKEY),THSKEY                                    
         BNE   BLNEWX                                                           
         TM    OFFRECD+(ACSTATUS-ACKEYD),OFFSLIST                               
         BO    BLNEW4              RECORD IS A LIST-SKIP IT                     
         LR    R1,R3                                                            
         BAS   RE,GETNAME                                                       
         MVC   0(2,R4),OFFKOFF                                                  
         MVC   2(36,R4),WORK                                                    
         LA    R4,38(,R4)                                                       
         MVI   0(R4),X'FF'                                                      
*                                                                               
BLNEW4   SR    R1,R1                                                            
         ICM   R1,3,OFFKOFF                                                     
         LA    R1,1(,R1)                                                        
         STCM  R1,3,THSKEY+(OFFKOFF-OFFRECD)                                    
         B     BLNEW2                                                           
*                                                                               
BLNEWX   B     EXIT                                                             
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        POST INACTIVE ACCOUNT TO BUFFER                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING ACMD,R2                                                          
         SPACE 1                                                                
POSTACCT NTR1                                                                   
         XC    BUFFKEY,BUFFKEY                                                  
         MVC   BUFFCODE,THSACC                                                  
         MVC   BUFFNAME,THISNAME                                                
         ZAP   BUFFDR,MACMABAL                                                  
         ZAP   BUFFCR,=P'0'                                                     
         MVI   BUFFTYPE,4                                                       
         ZAP   BUFFCT,=P'0'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFF,BUFFREC                              
*                                                                               
         CLI   QOPT7,C'B'                                                       
         BNE   EXIT                                                             
         MVC   P(L'BUFFREC),BUFFREC                                             
         EDIT  (P8,BUFFDR),(10,P+90),2,MINUS=YES                                
         EDIT  (P8,BUFFCR),(10,P+102),2,MINUS=YES                               
         GOTO1 ACREPORT                                                         
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        POST ANALYSIS CODE TOTAL TO BUFFER                                     
*---------------------------------------------------------------------*         
POSTANAL NTR1                                                                   
         LA    R2,1                PROCTRNS USES LINE1 ACCUMS                   
         CLI   MODE,ANALLAST                                                    
         BNE   *+8                                                              
         LA    R2,3                ANALLAST USES LINE3 ACCUMS                   
         GOTO1 PROLLER,DMCB,1,ACCUMS,(R2)                                       
         L     R2,DMCB                                                          
         XC    BUFFKEY,BUFFKEY                                                  
         MVC   BUFFNAME,SPACES                                                  
         ZAP   BUFFDR,0(8,R2)                                                   
         ZAP   BUFFCR,8(8,R2)                                                   
         ZAP   BUFFCT,=P'0'                                                     
         MVC   BUFFCODE,SPACES                                                  
         MVC   BUFFCODE(2),THISANAL                                             
         MVC   BUFFNAME(L'THISANL1),THISANL1                                    
         MVI   BUFFTYPE,2                                                       
         CLI   MODE,ANALLAST       ALREADY GOT NAME IF ANALLAST                 
         BE    POSTAN4                                                          
         CLI   OFFSW,C'Y'          OR IF AN OFFICE COMPANY                      
         BNE   POSTAN2                                                          
         CLI   NEWOFF,C'Y'         TEST ON NEW OFFICES                          
         BNE   POSTAN1                                                          
         CLC   =C'SJ',QUNIT        TEST REQUEST IS FOR SJ                       
         BE    POSTAN2             YES-ITS A WORKCODE                           
         B     *+12                                                             
*                                                                               
POSTAN1  CLI   THISANAL+1,C' '     CHECK FOR OFFICE                             
         BNE   POSTAN2                                                          
         BAS   RE,GETOFFC          GET OFFICE NAME                              
         MVC   BUFFNAME,WORK                                                    
         MVI   BUFFTYPE,3                                                       
         B     POSTAN4                                                          
*                                                                               
POSTAN2  GOTO1 AGETANAL,(RC)       GET WORK-CODE NAME                           
         MVC   BUFFNAME(L'THISANL1),THISANL1                                    
*                                                                               
POSTAN4  GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFF,BUFFREC                              
*                                                                               
         CLI   QOPT7,C'B'                                                       
         BNE   POSTAN5                                                          
         MVC   P(L'BUFFREC),BUFFREC                                             
         EDIT  (P8,BUFFDR),(10,P+90),2,MINUS=YES                                
         EDIT  (P8,BUFFCR),(10,P+102),2,MINUS=YES                               
         EDIT  (B1,BUFFTYPE),(1,P+114)                                          
         GOTO1 ACREPORT                                                         
*                                                                               
POSTAN5  AP    ANALCNT,=P'1'                                                    
         CLI   MODE,ANALLAST                                                    
         BNE   *+10                                                             
         MVC   0(24,R2),=3PL8'0'   CLEAR ACCUMS IF ANALLAST                     
         B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        POST CONTRA-ACCOUNT TOTAL TO BUFFER                                    
*---------------------------------------------------------------------*         
POSTSBAC NTR1                                                                   
         XC    BUFFKEY,BUFFKEY                                                  
         MVC   BUFFCODE,0(R3)                                                   
         LA    R1,BUFFCODE+L'BUFFCODE-1                                         
         CLI   0(R1),C' '          REPLACE SPACES WITH X'FF' IN KEY             
         BNE   *+12                                                             
         MVI   0(R1),X'FF'                                                      
         BCT   R1,*-12                                                          
         STC   R4,BUFFLVL          SET C/A LEVEL 1-4 (4=HIGHEST)                
         MVI   BUFFTYPE,1                                                       
         MVC   BUFFNAME,15(R3)     NAME SUPPLIED                                
         OC    BUFFNAME,BUFFNAME                                                
         BNZ   *+10                                                             
         MVC   BUFFNAME,SPACES                                                  
         ZAP   BUFFDR,0(8,R2)                                                   
         ZAP   BUFFCR,8(8,R2)                                                   
         ZAP   BUFFCT,=P'1'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFF,BUFFREC                              
*                                                                               
         CLI   QOPT7,C'B'                                                       
         BNE   EXIT                                                             
         MVC   P(L'BUFFREC),BUFFREC                                             
         EDIT  (P8,BUFFDR),(10,P+90),2,MINUS=YES                                
         EDIT  (P8,BUFFCR),(10,P+102),2,MINUS=YES                               
         GOTO1 ACREPORT                                                         
*                                                                               
         B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        ROUTINE TO PRINT A TRANSACTION                                         
*---------------------------------------------------------------------*         
PRNTTRNS NTR1                                                                   
         ST    R6,SAVEASRT         SAVE A(SORT RECORD)                          
         CLI   SAVESW,C'Y'         DO I HAVE TO PRINT THE PREVIOUS              
         BNE   PRNTTRNC            TRANSACTION                                  
*                                                                               
         L     R6,ASVSORT          A(SAVED TRANSACTION RECORD)                  
         BAS   RE,CLEARBLK                                                      
         LA    RE,SORTLEN                                                       
         LH    R0,SORTRLEN                                                      
         SR    R0,RE               R0=L'NARRATIVE                               
         BZ    PRNTTRN2                                                         
         LA    R2,22               WILL NARRATIVE PRINT ON 1 OR 2               
         CLI   PRTSW,C'N'          SHORT LINES                                  
         BE    *+8                                                              
         LA    R2,34                                                            
         LA    RF,20                                                            
         CLI   PROGPROF+15,0                                                    
         BE    *+8                                                              
         IC    RF,PROGPROF+15                                                   
         L     R3,ADPRNTBC                                                      
         GOTO1 CHOPPER,DMCB,(0,SORTNARR),((R2),50(R3)),                X        
               (C'P',(RF)),C'LEN=',(R0)                                         
         CLC   8(4,R1),=F'3'                                                    
         BL    PRNTTRN2                                                         
         CLI   LEDGPROF+13,C'Y'                                                 
         BE    PRNTTRN2                                                         
         BAS   RE,CLEARBLK         NO - PRINT LONG NARRATIVE UNDERNEATH         
         LA    R2,41               TRANSACTION DETAILS                          
         CLI   PRTSW,C'N'                                                       
         BE    *+8                                                              
         LA    R2,53                                                            
         LA    RF,20                                                            
         CLI   PROGPROF+15,0                                                    
         BE    *+8                                                              
         IC    RF,PROGPROF+15                                                   
         L     R3,ADPRNTBC                                                      
         GOTO1 CHOPPER,(R1),,((R2),163(R3)),(C'P',(RF))                         
*                                  FORMAT TRANSACTION DETAILS                   
PRNTTRN2 L     R3,ADPRNTBC                                                      
         MVC   24(6,R3),SORTMOS                                                 
         ZIC   R1,25(,R3)                                                       
         CLI   25(R3),X'FA'   CONVERT X'FA/FB/FC' TO C'A/B/C'                   
         BL    *+8                                                              
         SH    R1,=H'57'                                                        
         STC   R1,25(,R3)                                                       
         GOTO1 DATCON,DMCB,(1,SORTDATE),(8,31(R3))                              
         MVC   40(6,R3),SORTREF                                                 
         MVC   47(2,R3),SORTOFFC                                                
         MVC   FORMACCS(32),=4PL8'0'                                            
         LA    R1,FORMACCS                                                      
         TM    SORTSTAT,X'80'                                                   
         BO    *+8                                                              
         LA    R1,8(,R1)                                                        
         AP    0(8,R1),SORTAMNT                                                 
         L     R3,ADPRNTBC                                                      
         MVC   P,0(R3)                                                          
         MVC   PSECOND,132(R3)                                                  
         MVI   FORMSW,C'D'         FORMAT AMOUNT                                
         BAS   RE,FORMACC                                                       
         CLI   MODE,SBACLAST       IS THIS THE LAST TRANSACTION FOR             
         BNE   PRNTTRN4            THIS CONTRA-ACCOUNT                          
         GOTO1 PROLLER,DMCB,1,ACCUMS,2                                          
         L     R2,DMCB             YES - PRINT CONTRA-ACCOUNT BALANCE           
         ZAP   FORMACCS+24,0(8,R2)                                              
         SP    FORMACCS+24,8(8,R2)                                              
         MVI   FORMSW,C'B'                                                      
         BAS   RE,FORMACC                                                       
*                                                                               
PRNTTRN4 L     R3,ADPRNTBC                                                      
         MVC   0(132,R3),P                                                      
         MVC   132(132,R3),PSECOND                                              
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         L     R2,ADPRNTBC                                                      
         MVC   0(22,R2),SPACES                                                  
         MVC   132(22,R2),SPACES                                                
*                                                                               
         CLI   QOPT3,C'T'          PRINT OUT NAME FOR TAX OPTION                
         BNE   PRNTTRN5                                                         
         MVC   SAVESBAC(110),SPACES                                             
         MVC   SAVESBAC(L'SORTSBAC-1),SORTSBAC+1                                
         MVC   1(22,R2),SAVESBAC                                                
         B     PRNTTRN6                                                         
*                                                                               
PRNTTRN5 CLI   SORTSW,C'N'                                                      
         BE    PRNTTRN6                                                         
         CLI   QOPT3,C'Y'                                                       
         BNE   PRNTTRN6                                                         
         MVC   NEWKEY(15),SORTSBAC                                              
         CLC   SAVEKEY(15),NEWKEY                                               
         BE    PRNTTRN6            DONT PRINT NAME IF SAME ACCT                 
         MVC   SAVESBAC(110),SPACES                                             
         MVC   SAVESBAC(L'SORTSBAC-1),SORTSBAC+1                                
         MVC   SAVEKEY(15),NEWKEY                                               
         MVC   SAVESBAC+14(36),SORTSBNM                                         
         GOTO1 ADSQUASH,DMCB,SAVESBAC,88                                        
         MVC   WORK,SPACES                                                      
         GOTO1 CHOPPER,DMCB,(88,SAVESBAC),(22,WORK),(0,4)                       
         MVC   SAVESBAC(88),WORK                                                
         MVC   1(22,R2),SAVESBAC                                                
         MVC   133(22,R2),SAVESBAC+22                                           
*                                  PRINT THE TRANSACTION                        
PRNTTRN6 CLC   0(132,R2),SPACES                                                 
         BE    PRNTTRN8                                                         
         MVC   P,0(R2)                                                          
         MVC   P+1(22),SAVESBAC                                                 
         MVC   SAVESBAC(88),SAVESBAC+22                                         
         MVC   RCSUBPRG,SUBPROG                                                 
         BAS   RE,REPORT                                                        
         LA    R2,132(,R2)                                                      
         B     PRNTTRN6                                                         
*                                                                               
PRNTTRN8 MVI   SAVESW,C'N'                                                      
         CLI   QOPT3,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,REPORT                                                        
         CLI   MODE,SBACLAST                                                    
         BNE   PRNTTRNC                                                         
         CLI   QOPT3,C'Y'                                                       
         BE    PRNTTRNA                                                         
         CLC   SAVESBAC,SPACES     ENSURE ALL THE CONTRA-ACCOUNT NAME           
         BE    PRNTTRNA            IS PRINTED                                   
         MVC   P+1(22),SAVESBAC                                                 
         MVC   PSECOND+1(22),SAVESBAC+22                                        
         MVI   SPACING,2                                                        
*                                                                               
PRNTTRNA BAS   RE,REPORT                                                        
         B     PRNTTRNX                                                         
*                                                                               
PRNTTRNC DS    0H                                                               
         CLI   ACCSW,C'F'                                                       
         BNE   PRNTTRNR                                                         
         MVI   ACCSW,C'Y'                                                       
         MVC   THISHED6,SPACES     FORMAT NAME INTO HEADLINES                   
         MVC   THISHED7,SPACES     OR PRINTLINES                                
         CLI   LEDGPROF+6,C'Y'                                                  
         BNE   PRNTTRNE                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   SAVEKEY,SPACES      RESET CONTRA KEY                             
         CLI   LEDGPROF+11,C'A'    OPTION TO SHOW ACCOUNT LEVELS                
         BE    PRNTTRNN                                                         
         MVC   THISHED6(7),=C'ACCOUNT'                                          
         MVC   THISHED6+11(L'THSACCT),THSACCT                                   
         MVC   THISHED7+11(L'THISNAME),THISNAME                                 
         B     PRNTTRNN            GO CHECK SKIPPING OPTION                     
*                                                                               
PRNTTRNE BAS   RE,REPORT           SPACE LINE BEFORE NAME                       
         MVC   P+1(7),=C'ACCOUNT'                                               
         MVI   P+9,C'-'                                                         
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'THSACCT),THSACCT                                          
         MVC   WORK+20(L'THISNAME),THISNAME                                     
         GOTO1 ADSQUASH,DMCB,WORK,64                                            
         L     R0,4(,R1)                                                        
         GOTO1 CHOPPER,DMCB,((R0),WORK),(37,P+11),(C'P',2)                      
         B     PRNTTRNN            GO CHECK SKIPPING OPTION                     
*                                                                               
PRNTTRNM MVI   SPACING,2                                                        
         BAS   RE,REPORT           PRINT BALFWD LINE                            
         B     PRNTTRNR                                                         
*                                                                               
PRNTTRNN CLI   LEDGPROF+6,C'Y'     IF NOT SKIPING PAGES PRINT ACCOUNT           
         BNE   PRNTTRNM            NAME                                         
         B     PRNTTRNR                                                         
*                                                                               
PRNTTRNP GOTO1 PROLLER,DMCB,1,ACCUMS,4                                          
         L     R2,DMCB                                                          
         ZAP   FORMACCS,0(8,R2)                                                 
         ZAP   FORMACCS+8,8(8,R2)                                               
         CP    TRNSCNT,=P'0'                                                    
         BE    PRNTTRNG                                                         
         MVC   P+50(18),=C'TOTALS FOR ACCOUNT'                                  
         MVI   FORMSW,C'A'                                                      
         BAS   RE,FORMACC          FORMAT TOTALS LINE                           
         BAS   RE,REPORT                                                        
*                                                                               
         USING ACMD,R4                                                          
         USING ACSTATD,R2                                                       
*                                                                               
PRNTTRNG L     R4,AMONACC                                                       
         L     R2,ADACCSTA                                                      
         ZAP   FORMACCS+24,MACMABAL                                             
         CP    FORMACCS+24,=P'0'   NO BALFWD IF ZERO                            
         BE    PRNTTRP2                                                         
         CLI   FLTSW,C'Y'          OR FILTERING                                 
         BE    PRNTTRP2                                                         
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
         MVC   P+50(15),=C'OPENING BALANCE'                                     
         LA    R3,P+66                                                          
         OC    ACMMSTR,ACMMSTR                                                  
         BNZ   PRNTTRNH            IF NO START USE BAL FWRD DATE                
         GOTO1 DATCON,DMCB,(1,ACSTBFDT),(8,0(R3))                               
         B     PRNTTRNL                                                         
*                                                                               
PRNTTRNH MVC   WORK(2),ACMMSTR      GET CONVERTED START DATE                    
         MVI   WORK+2,X'01'        ADD A DAY                                    
         GOTO1 DATCON,DMCB,(1,WORK),(8,0(R3))                                   
*                                                                               
PRNTTRNL MVI   FORMSW,C'B'                                                      
         BAS   RE,FORMACC                                                       
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                  FIRST TRANSACTION FOR ACCOUNT                
PRNTTRP2 ZAP   FORMACCS+24,MACMABAL                                             
         CLI   FLTSW,C'Y'          NO BALANCE C/F IF FILTERING                  
         BE    PRNTTRNQ                                                         
*                                                                               
         MVC   P+50(15),=C'PRESENT BALANCE'                                     
         CLI   ACMMEND,X'FF'       ANY  DATE ?                                  
         BE    PRNTTRP5            NO,  JUST PRINT PRESENT BALANCE              
         MVC   DUB(2),ACMMEND      END  DATE                                    
         MVI   DUB+2,X'01'         DAY  01                                      
*                                  GET  LAST DAY  OF   MONTH                    
         GOTO1 DATCON,DMCB,(X'31',DUB),(1,WORK),(1,0)                           
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,WORK+10)                               
         CLC   WORK(2),WORK+10     BALANCE ON DATE VS TODAY ?                   
         BNL   PRNTTRP5            IF   BALANCE ON DATE EQ OR GREATER           
         MVC   P+50(15),=C'CLOSING BALANCE'                                     
         GOTO1 DATCON,DMCB,(1,WORK),(8,P+66)                                    
*                                                                               
PRNTTRP5 MVI   FORMSW,C'C'                                                      
         BAS   RE,FORMACC                                                       
         MVI   SPACING,2                                                        
*                                                                               
PRNTTRNQ DS    0H                                                               
         BAS   RE,REPORT                                                        
         B     PRNTTRNX                                                         
*                                  FIRST TRANSACTION FOR WORK-CODE              
PRNTTRNR CLI   MODE,ACCLAST                                                     
         BE    PRNTTRNP                                                         
         CLI   ANALSW,C'F'                                                      
         BNE   PRNTTRNT                                                         
         MVI   ANALSW,C'Y'                                                      
         MVC   P,SPACES            CLEAR PREVIOUS RUBBISH                       
         MVC   P+1(L'THISANL1),THISANL1                                         
         MVC   PSECOND+1(L'THISANL2),THISANL2                                   
         MVI   SPACING,2                                                        
         BAS   RE,REPORT           PRINT WORK-CODE NAME                         
*                                  FIRST TRANSACTION FOR SUB-ACCOUNT            
PRNTTRNT CLI   SBACSW,C'F'                                                      
         BNE   PRNTTRNV                                                         
         MVI   SBACSW,C'Y'                                                      
         MVC   WORK,SPACES         FORMAT CODE/NAME INTO WORK                   
         MVC   WORK(14),THISSBAC+1                                              
         MVC   WORK+16(L'THISSBNM),THISSBNM                                     
         GOTO1 ADSQUASH,DMCB,WORK,64                                            
         L     R0,4(,R1)                                                        
         MVC   SAVESBAC(110),SPACES                                             
         GOTO1 CHOPPER,DMCB,((R0),WORK),(22,SAVESBAC),4                         
*                                                                               
PRNTTRNV L     R0,ASVSORT                                                       
         ICM   RE,15,SAVEASRT      NO SAVED RECORD                              
         BZ    EXIT                                                             
         SR    RF,RF                                                            
         MVC   HALF,0(RE)                                                       
         LH    RF,HALF                                                          
*                                                                               
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         MVI   SAVESW,C'Y'                                                      
*                                                                               
PRNTTRNX B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        PRINT SORTED TRANSACTIONS                                              
*---------------------------------------------------------------------*         
PRNTSORT NTR1                                                                   
         MVC   SAVEMODE,MODE       SAVE CURRENT MODE SETTING                    
*                                                                               
         CLI   FRSTTRN,C'Y'                                                     
         BNE   PRNTSRT0                                                         
*                                                                               
         XC    SORTBRK,SORTBRK                                                  
         XC    THISSBAC,THISSBAC                                                
         MVI   ANALSW,C'N'                                                      
         MVI   TRNSSW,C'N'                                                      
         GOTO1 PROLLER,DMCB,2,ACCUMS,2                                          
*                                                                               
PRNTSRT0 CLI   QOPT3,C'T'          SORT BY TAX-ID                               
         BNE   *+12                                                             
         LA    RE,=C'C'            ID IS NOW IN CONTRA FIELD                    
         B     PRNTSRT1                                                         
*                                                                               
         ZIC   RE,LEDGPROF+9                                                    
         LTR   RE,RE                                                            
         BZ    PRNTSRT2                                                         
         LA    RE,LEDGPROF-1(RE)                                                
         CLI   0(RE),C'*'          ARE WE PRINTING TOTALS                       
         BE    PRNTSRT2                                                         
*                                                                               
PRNTSRT1 LA    R1,SORTKEYS         YES - SET C/B INFO                           
         CLC   0(1,R1),0(RE)                                                    
         BE    *+12                                                             
         LA    R1,L'SORTKEYS(,R1)                                               
         B     *-14                                                             
         ZIC   RE,1(,R1)           SET KEY DISP-1                               
         BCTR  RE,0                                                             
         STC   RE,SORTBRK                                                       
         IC    RE,2(,R1)           SET KEY LEN-1                                
         BCTR  RE,0                                                             
         STC   RE,SORTBRK+1                                                     
*                                  LOOP TO READ SORTED TRANSACTIONS             
PRNTSRT2 DS    0H                                                               
         MVI   SBACSW,C'Y'                                                      
         OC    SORTBRK(2),SORTBRK                                               
         BZ    PRNTSRT6                                                         
         ZIC   R2,SORTBRK          CHECK FOR KEY C/B                            
         LA    R2,SORTKEY(R2)                                                   
         ZIC   R3,SORTBRK+1                                                     
         OC    THISSBAC,THISSBAC   IF FIRST TIME DON'T PRINT TOTALS             
         BZ    PRNTSRT4                                                         
         EXCLC R3,0(R2),SORTBRK+2                                               
         BE    PRNTSRT6                                                         
         MVI   MODE,SBACLAST                                                    
         BAS   RE,PRNTTRNS         PRINT C/B TOTALS                             
*                                                                               
PRNTSRT4 EXMVC R3,SORTBRK+2,0(R2)  SAVE CURRENT KEY C/B INFO                    
         GOTO1 PROLLER,DMCB,2,ACCUMS,2                                          
*                                                                               
PRNTSRT5 MVI   TRNSSW,C'N'                                                      
*                                  ADD AMOUNT TO ACCUMS                         
PRNTSRT6 GOTO1 PROLLER,DMCB,1,ACCUMS,1                                          
         L     R2,DMCB                                                          
         MVC   0(24,R2),=3PL8'0'                                                
         TM    SORTSTAT,X'80'      DEBIT                                        
         BO    *+8                                                              
         LA    R2,8(,R2)           CREDIT                                       
         AP    0(8,R2),SORTAMNT                                                 
         GOTO1 (RF),(R1),5         CROSS-CAST LINE  - PROLLER                   
         GOTO1 (RF),(R1),4,,,2     AND ADD TO C/B TOTALS - PROLLER              
         MVC   THISSBAC,SORTSBAC                                                
         MVI   MODE,PROCTRNS       PRINT THIS TRANSACTION                       
         BAS   RE,PRNTTRNS                                                      
         MVI   TRNSSW,C'Y'                                                      
         MVC   MODE,SAVEMODE                                                    
         B     EXIT                                                             
*                                                                               
PRNTSRT8 MVI   MODE,SBACLAST                                                    
         OC    SORTBRK(2),SORTBRK                                               
         BZ    *+8                                                              
         BAS   RE,PRNTTRNS         PRINT C/B TOTALS                             
         MVC   MODE,SAVEMODE       RESET MODE                                   
         MVI   SORTSW,C'Y'         AND SET SORT SWITCH                          
         GOTO1 PROLLER,DMCB,2,ACCUMS,1                                          
         GOTO1 (RF),(R1),,,2                                                    
         B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        PRINT CONTRA-ACCOUNT SUMMARY                                           
*---------------------------------------------------------------------*         
PRNTSM   NTR1                                                                   
         MVI   RCSUBPRG,2          SET SUB-PROGRAM                              
         CLI   PRTSW,C'N'                                                       
         BE    *+8                                                              
         MVI   RCSUBPRG,3                                                       
         MVI   READSW,C'N'                                                      
         ZIC   R0,SAVELVL          R0=1 FOR ACCOUNT, 2 FOR LEDGER               
         XC    BUFFKEY,BUFFKEY                                                  
         MVI   BUFFTYPE,1                                                       
         ZAP   TOTDR,=P'0'                                                      
         ZAP   TOTCR,=P'0'                                                      
         CLI   MODE,LEDGLAST                                                    
         BNE   *+12                                                             
         MVI   FORCEHED,C'Y'       NEW PAGE FOR LEDGER SUMMARY                  
         B     PRNTSM1                                                          
         MVC   RCSUBPRG,SUBPROG                                                 
         MVC   P+1(22),=C'CONTRA-ACCOUNT SUMMARY'                               
         MVC   PSECOND+1(22),=22C'-'                                            
         BAS   RE,REPORT                                                        
         MVI   PRNTSW,C'N'         CONTROL PRINTING OF TOTALS                   
*                                                                               
PRNTSM1  GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFF,BUFFREC,(R0)                        
         B     PRNTSM4                                                          
*                                                                               
PRNTSM2  GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFF,BUFFREC,(R0)                         
*                                                                               
PRNTSM4  DS    0H                                                               
         CLI   8(R1),0                                                          
         BNE   PRNTSM16                                                         
         CLI   BUFFTYPE,1                                                       
         BNE   PRNTSM16                                                         
         CLI   BUFFLVL,1           IS IT A DETAIL LEVEL                         
         BNE   PRNTSM10                                                         
         CP    BUFFDR,=P'0'        CHECK FOR ACTIVITY                           
         BNE   *+14                                                             
         CP    BUFFCR,=P'0'                                                     
         BE    PRNTSM2                                                          
         AP    TOTDR,BUFFDR                                                     
         AP    TOTCR,BUFFCR                                                     
         MVI   PRNTSW,C'Y'         MUST PRINT A TOTAL                           
         B     PRNTSM12                                                         
*                                                                               
PRNTSM10 DS    0H                  SUMMARY (TOTAL LINE)                         
         MVC   BYTE,PRNTSW         SAVE CURRENT PRINT SWITCH SETTING            
         CLI   BUFFLVL,5           IS IT THE LAST TOTAL                         
         BNE   *+8                 NO, LEAVE SWITCH ALONE                       
         MVI   PRNTSW,C'N'         RESET THE TOTAL SWITCH                       
         CP    BUFFDR,=P'0'        CHECK FOR ACTIVITY                           
         BNE   PRNTSM12                                                         
         CP    BUFFCR,=P'0'                                                     
         BNE   PRNTSM12                                                         
         CLI   BYTE,C'N'           ANY ACTIVITY                                 
         BE    PRNTSM2             NO, SKIP THE TOTAL                           
*                                                                               
PRNTSM12 MVC   THSKEY,SPACES                                                    
         MVC   THSKEY(L'BUFFCODE),BUFFCODE                                      
         LA    R1,THSKEY+L'BUFFCODE-1                                           
         CLI   0(R1),X'FF'         REPLACE X'FF' WITH SPACES IN KEY             
         BNE   *+12                                                             
         MVI   0(R1),C' '                                                       
         BCT   R1,*-12                                                          
*                                  GET CONTRA-ACCOUNT NAME                      
         CLC   BUFFNAME,SPACES                                                  
         BNE   PRNTSM15                                                         
         CLI   THSKEY,C' '         TEST IF FUNNY CONTRA-ACCOUNT KEY             
         BNH   PRNTSM15                                                         
         CLI   NEWCST,C'Y'         ON NEW COSTING                               
         BNE   PRNTSM13                                                         
         CLC   THSKEY+1(2),=C'13'      IS IT A 13 ACCOUNT                       
         BNE   PRNTSM13                                                         
         LA    R1,THSKEY                                                        
         B     PRNTSM14                                                         
*                                                                               
PRNTSM13 GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'ACCOUNT',THSKEY,ASRTREC           
         MVI   READSW,C'Y'                                                      
         MVC   BUFFNAME(15),=C'**NOT ON FILE**'                                 
         CLI   DMCB+8,0            CHECK RECORD N/F                             
         BNE   PRNTSM15                                                         
         L     R1,ASRTREC                                                       
*                                                                               
PRNTSM14 BAS   RE,GETNAME                                                       
         MVC   BUFFNAME,WORK                                                    
*                                  FORMAT LINE & PRINT                          
PRNTSM15 ZAP   FORMACCS,BUFFDR                                                  
         ZAP   FORMACCS+8,BUFFCR                                                
         MVC   P+1(L'BUFFCODE-1),THSKEY+1                                       
         MVC   P+17(36),BUFFNAME                                                
         CLI   BUFFLVL,1                                                        
         BE    *+8                                                              
         MVI   P+16,C'*'           INDICATE HIGH LEVEL TOTAL                    
         MVI   FORMSW,C'A'                                                      
         BAS   RE,FORMACC                                                       
         BAS   RE,REPORT                                                        
         B     PRNTSM2                                                          
*                                                                               
PRNTSM16 CLI   MODE,LEDGLAST                                                    
         BNE   PRNTSM18                                                         
         BAS   RE,REPORT                                                        
         ZAP   FORMACCS,TOTDR                                                   
         ZAP   FORMACCS+8,TOTCR                                                 
         MVC   P+50(12),=C'** TOTALS **'                                        
         MVI   FORMSW,C'A'                                                      
         BAS   RE,FORMACC                                                       
         BAS   RE,REPORT                                                        
*                                                                               
PRNTSM18 MVC   RCSUBPRG,SUBPROG    RESET SUB PROGRAM                            
         CLI   READSW,C'Y'                                                      
         BNE   PRNTSM30                                                         
         MVC   THSKEY,SPACES       RESTORE READ SEQUENCE                        
         MVC   THSKEY(15),KEY                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'ACCOUNT',THSKEY,ASRTREC           
         CLI   DMCB+8,0                                                         
         BE    PRNTSM30                                                         
         DC    H'0'                                                             
*                                                                               
PRNTSM30 B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        PRINT ANALYSIS CODE SUMMARY                                            
*---------------------------------------------------------------------*         
PRNTANAL NTR1                                                                   
         ZIC   R0,SAVELVL          R0=1 FOR ACCOUNT, 2 FOR LEDGER               
         XC    BUFFKEY,BUFFKEY                                                  
         MVI   BUFFTYPE,2                                                       
         ZAP   ANALCNT,=P'0'                                                    
         ZAP   TOTDR,=P'0'                                                      
         ZAP   TOTCR,=P'0'                                                      
         CLI   MODE,LEDGLAST                                                    
         BNE   PRNTANL2                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,4          SET SUB-PROGRAM FOR LEDGER ANALYSIS          
         CLI   PRTSW,C'N'                                                       
         BE    *+8                                                              
         MVI   RCSUBPRG,5                                                       
*                                                                               
PRNTANL2 GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFF,BUFFREC,(R0)                        
         B     PRNTANL6                                                         
*                                                                               
PRNTANL4 GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFF,BUFFREC,(R0)                         
*                                                                               
PRNTANL6 DS    0H                                                               
         CLI   8(R1),0                                                          
         BNE   PRNTANLE                                                         
         CLI   BUFFTYPE,2                                                       
         BL    PRNTANLE                                                         
         CLI   BUFFTYPE,3                                                       
         BH    PRNTANLE                                                         
         CP    BUFFDR,=P'0'        CHECK FOR ACTIVITY                           
         BNE   *+14                                                             
         CP    BUFFCR,=P'0'                                                     
         BE    PRNTANL4                                                         
         AP    TOTDR,BUFFDR                                                     
         AP    TOTCR,BUFFCR                                                     
         ZAP   FORMACCS,BUFFDR                                                  
         ZAP   FORMACCS+8,BUFFCR                                                
*                                  FORMAT LINE FOR LEDGER ANALYSIS              
         CLI   MODE,LEDGLAST                                                    
         BNE   PRNTANL8                                                         
         MVC   P+1(2),BUFFCODE                                                  
         MVC   P+4(36),BUFFNAME                                                 
         B     PRNTANLC                                                         
*                                  FORMAT LINE FOR ACCOUNT ANALYSIS             
PRNTANL8 CP    ANALCNT,=P'0'                                                    
         BNE   PRNTANLA                                                         
         MVC   P+24(13),=C'CODE ANALYSIS'                                       
         CLI   BUFFTYPE,2                                                       
         BE    *+10                                                             
         MVC   P+24(15),=C'OFFICE ANALYSIS'                                     
*                                                                               
PRNTANLA MVC   P+47(2),BUFFCODE                                                 
         MVC   P+50(15),BUFFNAME                                                
         CLI   BUFFTYPE,2          W/C NAMES ARE 15 BYTES LONG                  
         BE    PRNTANLC                                                         
         CLI   PRTSW,C'N'          OFFICES CAN BE UP-TO 36                      
         BE    *+14                                                             
         MVC   P+50(36),BUFFNAME                                                
         B     PRNTANLC                                                         
         GOTO1 CHOPPER,DMCB,(36,BUFFNAME),(22,P+50),(C'P',2)                    
*                                                                               
PRNTANLC MVI   FORMSW,C'A'                                                      
         CLI   LEDGPROF+12,C'N'    OPTION NOT TO SHOW W/C BALANCE               
         BNE   *+8                                                              
         MVI   FORMSW,C'D'                                                      
         BAS   RE,FORMACC                                                       
         BAS   RE,REPORT                                                        
         AP    ANALCNT,=P'1'                                                    
         B     PRNTANL4                                                         
*                                                                               
PRNTANLE CLI   MODE,LEDGLAST                                                    
         BNE   PRNTANLG                                                         
         BAS   RE,REPORT                                                        
         ZAP   FORMACCS,TOTDR                                                   
         ZAP   FORMACCS+8,TOTCR                                                 
         MVC   P+50(12),=C'** TOTALS **'                                        
         MVI   FORMSW,C'A'                                                      
         BAS   RE,FORMACC                                                       
         BAS   RE,REPORT                                                        
*                                                                               
PRNTANLG B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        ROUTINE TO PRINT A LIST OF INACTIVE ACCOUNTS                           
*---------------------------------------------------------------------*         
PRNTACCT NTR1                                                                   
         ZIC   R0,SAVELVL                                                       
         XC    BUFFKEY,BUFFKEY                                                  
         MVI   BUFFTYPE,4                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,6                                                       
         ZAP   INACTOT,=P'0'                                                    
*                                                                               
PRNTACC2 GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFF,BUFFREC,(R0)                        
         B     PRNTACC6                                                         
*                                                                               
PRNTACC4 GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFF,BUFFREC,(R0)                         
*                                                                               
PRNTACC6 CLI   8(R1),0                                                          
         BNE   PRNTACC8                                                         
         CLI   BUFFTYPE,4                                                       
         BNE   PRNTACC8                                                         
         MVC   P+1(L'BUFFCODE-1),BUFFCODE+1                                     
         MVC   P+17(L'BUFFNAME),BUFFNAME                                        
         ZAP   FORMACCS+24,BUFFDR                                               
         MVI   FORMSW,C'B'                                                      
         BAS   RE,FORMACC                                                       
         BAS   RE,REPORT                                                        
         AP    INACTOT,BUFFDR      ADD TO INACTIVE TOTAL                        
         B     PRNTACC4                                                         
*                                  PRINT TOTAL LINE                             
PRNTACC8 CP    INACTOT,=P'0'                                                    
         BE    PRNTACCA                                                         
         BAS   RE,REPORT                                                        
         MVC   P+50(26),=C'TOTAL OF INACTIVE BALANCES'                          
         ZAP   FORMACCS+24,INACTOT                                              
         MVI   FORMSW,C'B'                                                      
         BAS   RE,FORMACC                                                       
         BAS   RE,REPORT                                                        
*                                                                               
PRNTACCA B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        ROUTINE TO EXTRACT NAME FROM A RECORD                                  
*---------------------------------------------------------------------*         
GETNAME  NTR1                                                                   
         MVC   WORK,SPACES                                                      
         CLI   NEWCST,C'Y'         ON NEW COSTING                               
         BNE   *+14                                                             
         CLC   1(2,R1),=C'13'      IS IT A 13 ACCOUNT                           
         BE    GETNAME5                                                         
         AH    R1,DATADISP         R1=A(RECORD)                                 
         SR    RE,RE                                                            
*                                                                               
GETNAME2 CLI   0(R1),0                                                          
         BE    GETNAME4                                                         
         CLI   0(R1),X'20'                                                      
         BE    *+14                                                             
         IC    RE,1(,R1)                                                        
         AR    R1,RE                                                            
         B     GETNAME2                                                         
         IC    RE,1(,R1)                                                        
         SH    RE,=H'3'                                                         
         EXMVC RE,WORK,2(R1)       WORK NOW CONTAINS NAME                       
*                                                                               
GETNAME4 B     EXIT                                                             
*                                                                               
         USING CADCD,R6                                                         
*                                                                               
GETNAME5 LA    R6,CATBLK           BUILD BLOCK FOR DECODE CALL                  
         MVC   CADCDMGR,DATAMGR                                                 
         MVC   CADCA13,0(R1)      13 ACCOUNT                                    
         GOTO1 VCATKIN,CADCD                                                    
         MVC   WORK(L'CADCLOWN),CADCLOWN                                        
         CLC   WORK(L'CADCLOWN),SPACES                                          
         BNE   EXIT                                                             
         MVC   WORK(15),=C'**NOT ON FILE**'                                     
         B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*              ROUTINE TO LOOK-UP OFFICE NAME IN BUFFER                         
*---------------------------------------------------------------------*         
GETOFFC  NTR1  ,                                                                
         L     R1,ADOFFBUF                                                      
         MVC   WORK,SPACES                                                      
*                                                                               
GETOFFC2 CLI   0(R1),X'FF'                                                      
         BE    GETOFFC4                                                         
         CLC   0(2,R1),THISANAL                                                 
         BE    *+12                                                             
         LA    R1,38(,R1)                                                       
         B     GETOFFC2                                                         
         MVC   WORK(36),2(R1)                                                   
*                                                                               
GETOFFC4 B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        CLEAR DOWN TRANSACTION PRINT BLOCK                                     
*---------------------------------------------------------------------*         
CLEARBLK NTR1                                                                   
         L     R1,ADPRNTBC                                                      
         LA    RE,20                                                            
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(,R1)                                                      
         BCT   RE,*-10                                                          
         B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*              ROUTINE TO PRINT A LINE OF REPORT                                
*---------------------------------------------------------------------*         
REPORT   NTR1                                                                   
         MVC   HEAD4+60(L'THISHED1),THISHED1                                    
         MVC   HEAD5+60(L'THISHED1),THISHED2                                    
         MVC   HEAD6+60(L'THISHED1),THISHED3                                    
         MVC   HEAD7+60(L'THISHED1),THISHED4                                    
         MVC   HEAD8+60(L'THISHED1),THISHED5                                    
         MVC   HEAD7+1(L'THISHED6),THISHED6                                     
         MVC   HEAD8+1(L'THISHED7),THISHED7                                     
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        FORMAT ACCOUNT LEVELS INTO SAVE HEADLINES                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING ACHEIRD,R2                                                       
         SPACE 1                                                                
FORMLEV  NTR1                                                                   
         L     R2,ADLDGHIR                                                      
         LA    R3,THISHED1         R3=A(HEADLINE BLOCK)                         
         LA    R4,ACHRLEVA         R4=A(HEIRARCHY LENGTH/NAME)                  
         LA    R5,DUB              R5=A(EXTRACT INFO)                           
         LA    R6,4                R6=MAX LEVELS                                
         XC    DUB,DUB             DUB=POS/LEN FOR EACH LEVEL                   
         XC    DOUBLE,DOUBLE       DOUBLE=L'LONGEST CODE & LEVEL NAME           
*                                                                               
         L     R1,ADACC                                                         
         LA    R1,14(,R1)                                                       
         LA    RE,12                                                            
         CLI   0(R1),C' '          GET ACTUAL KEY LENGTH                        
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         BCT   RE,*-10                                                          
         STC   RE,DOUBLE+3                                                      
         SR    RE,RE                                                            
*                                                                               
FORMLEV2 CLI   0(R4),0             ANY MORE LEVELS ?                            
         BE    FORMLEV4                                                         
         ZIC   RF,0(,R4)                                                        
         CLI   16(R4),0                                                         
         BNE   *+8                                                              
         IC    RF,DOUBLE+3         USE ACTUAL LENGTH ON LAST CODE               
         STC   RE,0(,R5)           SET KEY POSITION                             
         SR    RF,RE                                                            
         STC   RF,1(,R5)           AND CODE LENGTH                              
         AR    RE,RF                                                            
         CLC   1(1,R5),DOUBLE                                                   
         BNH   *+10                                                             
         MVC   DOUBLE(1),1(R5)     SAVE LENGTH OF LONGEST CODE                  
         MVC   0(L'ACHRDESA,R3),1(R4)                                           
         LA    R1,L'ACHRDESA                                                    
         LA    RF,L'ACHRDESA-1(,R3)                                             
*                                                                               
         CLI   0(RF),C' '          FIND LENGTH OF LEVEL NAME AND SAVE           
         BNE   *+10                IF IT'S THE LONGEST                          
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         STC   R1,DOUBLE+2                                                      
         CLC   DOUBLE+2(1),DOUBLE+1                                             
         BNH   *+10                                                             
         MVC   DOUBLE+1(1),DOUBLE+2                                             
*                                                                               
         LA    R3,L'THISHED1(,R3)  BUMP TO NEXTS                                
         LA    R4,16(,R4)                                                       
         LA    R5,2(,R5)                                                        
         BCT   R6,FORMLEV2                                                      
*                                                                               
FORMLEV4 ST    R3,DOUBLE+4         SAVE A(NEXT AVAILABLE HEADLINE)              
         LA    R3,THISHED1                                                      
         ZIC   RF,DOUBLE+1                                                      
         LA    R3,1(R3,RF)         R3=A(OUTPUT CODE)                            
         ZIC   RE,DOUBLE                                                        
         LA    R4,1(R3,RE)         R4=A(OUTPUT NAME)                            
         LA    RE,2(RE,RF)                                                      
         LA    RF,L'THISHED1                                                    
         SR    RF,RE                                                            
         ST    RF,DOUBLE           DOUBLE(4)=MAX LENGTH OF NAME                 
         LA    R2,ADHEIRA          R2=A(A(RECORD))                              
         LA    R5,DUB              R5=A(EXTRACT INFO)                           
         LA    R6,4                R6=MAX LEVELS                                
*                                                                               
FORMLEV6 OC    0(2,R5),0(R5)                                                    
         BZ    FORMLEVX                                                         
         ZIC   RE,0(,R5)           EXTRACT CODE FROM KEY                        
         L     RF,ADACC                                                         
         LA    RE,3(RE,RF)                                                      
         ZIC   RF,1(,R5)                                                        
         BCTR  RF,0                                                             
         EXMVC RF,0(R3),0(RE)                                                   
*                                                                               
         L     R1,0(,R2)           EXTRACT NAME FROM RECORD                     
         BAS   RE,GETNAME                                                       
         L     R0,DOUBLE                                                        
         GOTO1 CHOPPER,DMCB,(36,WORK),((R0),0(R4)),1                            
*                                                                               
         LA    R3,L'THISHED1(,R3)                                               
         LA    R4,L'THISHED1(,R4)                                               
         LA    R5,2(,R5)                                                        
         LA    R2,4(,R2)                                                        
         BCT   R6,FORMLEV6                                                      
*                                                                               
FORMLEVX L     R3,DOUBLE+4         RETURN A(NEXT AVAILABLE HEADLINE)            
         XIT1  REGS=(R3)                                                        
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        ROUTINE TO FORMAT ACCUMULATORS                                         
*---------------------------------------------------------------------*         
FORMACC  NTR1                                                                   
         CLI   PRTSW,C'N'                                                       
         BNE   FORMACCA                                                         
*                                  NORMAL EDITS                                 
*                                  BALANCE (B/F)                                
         CLI   FORMSW,C'B'                                                      
         BNE   FORMACC2                                                         
         ZAP   DUB,FORMACCS+24                                                  
         LA    R2,P+96                                                          
         BAS   RE,EDIT1                                                         
         B     FORMACCX                                                         
*                                  AMOUNT (DR AND/OR CR)                        
FORMACC2 CLI   FORMSW,C'C'                                                      
         BE    FORMACC8                                                         
         CP    FORMACCS,=P'0'                                                   
         BE    FORMACC4                                                         
         ZAP   DUB,FORMACCS                                                     
         LA    R2,P+72                                                          
         BAS   RE,EDIT2                                                         
*                                                                               
FORMACC4 CP    FORMACCS+8,=P'0'                                                 
         BE    FORMACC6                                                         
         ZAP   DUB,FORMACCS+8                                                   
         LA    R2,P+84                                                          
         BAS   RE,EDIT3                                                         
*                                  DIFFERENCE                                   
FORMACC6 CLI   FORMSW,C'D'                                                      
         BE    FORMACCX                                                         
         ZAP   DUB,FORMACCS                                                     
         SP    DUB,FORMACCS+8                                                   
         CP    DUB,=P'0'                                                        
         BE    FORMACCX                                                         
         LA    R2,P+96                                                          
         BAS   RE,EDIT1                                                         
         B     FORMACCX                                                         
*                                  BALANCE C/F                                  
FORMACC8 ZAP   DUB,FORMACCS+24                                                  
         AP    DUB,FORMACCS                                                     
         SP    DUB,FORMACCS+8                                                   
         LA    R2,P+96                                                          
         CP    DUB,=P'0'                                                        
         BNE   *+14                                                             
         MVC   P+106(3),=C'NIL'                                                 
         B     FORMACCX                                                         
         BAS   RE,EDIT1                                                         
         B     FORMACCX                                                         
*                                  SPECIAL EDITS                                
*                                  BALANCE (B/F)                                
FORMACCA CLI   FORMSW,C'B'                                                      
         BNE   FORMACCD                                                         
         ZAP   DUB,FORMACCS+24                                                  
         CLI   PRTSW,C'+'                                                       
         BE    *+10                                                             
         MP    DUB,=P'-1'                                                       
         LA    R2,P+96                                                          
         BAS   RE,EDIT2                                                         
         B     FORMACCX                                                         
*                                  AMOUNT/DIFFERENCE                            
FORMACCD CLI   FORMSW,C'C'                                                      
         BE    FORMACCF                                                         
         ZAP   DUB,FORMACCS                                                     
         SP    DUB,FORMACCS+8                                                   
         CLI   PRTSW,C'+'                                                       
         BE    *+10                                                             
         MP    DUB,=P'-1'                                                       
         ZAP   DOUBLE,DUB                                                       
         LA    R2,P+84                                                          
         BAS   RE,EDIT2                                                         
         CLI   FORMSW,C'D'                                                      
         BE    FORMACCX                                                         
         ZAP   DOUBLE,DUB                                                       
         LA    R2,P+96                                                          
         BAS   RE,EDIT2                                                         
         B     FORMACCX                                                         
*                                  BALANCE C/F                                  
FORMACCF ZAP   DUB,FORMACCS+24                                                  
         AP    DUB,FORMACCS                                                     
         SP    DUB,FORMACCS+8                                                   
         CLI   PRTSW,C'+'                                                       
         BE    *+10                                                             
         MP    DUB,=P'-1'                                                       
         CP    DUB,=P'0'                                                        
         BNE   *+14                                                             
         MVC   P+106(3),=C'NIL'                                                 
         B     FORMACCX                                                         
         LA    R2,P+96                                                          
         BAS   RE,EDIT2                                                         
*                                                                               
FORMACCX B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        EDITING AIDS                                                           
*---------------------------------------------------------------------*         
EDIT1    NTR1                                                                   
         ZAP   THISNUM,DUB                                                      
         EDIT  (P8,THISNUM),(14,THISWORK),2,CR=YES,ZERO=BLANK                   
         MVI   THISWORK+14,C' '                                                 
         B     EDITOUT                                                          
*                                                                               
EDIT2    NTR1                                                                   
         ZAP   THISNUM,DUB                                                      
         EDIT  (P8,THISNUM),(14,THISWORK),2,MINUS=YES,ZERO=BLANK                
         MVI   THISWORK+14,C' '                                                 
         B     EDITOUT                                                          
*                                                                               
EDIT3    NTR1                                                                   
         ZAP   THISNUM,DUB                                                      
         EDIT  (P8,THISNUM),(14,THISWORK),2,MINUS=YES,ZERO=BLANK                
         MVI   THISWORK+14,C' '                                                 
         CLI   THISWORK+13,C'-'                                                 
         BNE   *+8                                                              
         MVI   THISWORK+13,C'+'                                                 
         B     EDITOUT                                                          
*                                                                               
EDITOUT  CLC   THISWORK,SPACES                                                  
         BE    EDITX                                                            
         BCTR  R2,0                R2=A(OUTPUT-1)                               
         LA    R1,THISWORK         R1=A(NUMBER)                                 
         LA    RE,14               RE=L'NUMBER-1                                
*                                                                               
         CLI   0(R1),C' '          FIND FIRST DIGIT POSITION                    
         BNE   *+16                                                             
         LA    R1,1(,R1)                                                        
         LA    R2,1(,R2)                                                        
         BCT   RE,*-16                                                          
         LA    RE,1(,RE)           LNTH MUST COVER 1 EACH SIDE.                 
         EXCLC RE,0(R2),SPACES     WILL THE NUMBER FIT                          
         BE    *+8                                                              
         LA    R2,132(,R2)         NO - BUMP TO NEXT LINE                       
         BCTR  RE,0                RESET LNTH FOR MVC.                          
         EXMVC RE,1(R2),0(R1)      MOVE NUMBER TO OUTPUT LINE                   
         LA    R3,P+110            MAKE SURE MINUS SIGN                         
         CLI   0(R3),C' '          DOESN'T GO PAST COLUMN 110                   
         BNE   *+16                                                             
         LA    R3,PSECOND+110                                                   
         CLI   0(R3),C' '                                                       
         BE    EDITX                                                            
         LR    RF,R3                                                            
         SH    RF,=H'13'                                                        
         CLI   0(RF),C'A'                                                       
         BNL   *+12                                                             
         LA    RF,1(,RF)                                                        
         B     *-12                                                             
         BCTR  RF,0                                                             
         MVC   0(1,RF),0(R3)                                                    
         MVI   0(R3),C' '                                                       
*                                                                               
EDITX    B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        CLEAR SORTREC                                                          
*---------------------------------------------------------------------*         
CLRSREC  NTR1                                                                   
         L     R6,ASRTRECA                                                      
         LA    R5,4                                                             
*                                                                               
CLRS10   XC    0(250,R6),0(R6)                                                  
         LA    R6,250(,R6)                                                      
         BCT   R5,CLRS10                                                        
         B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        LITERALS ETC.                                                          
*---------------------------------------------------------------------*         
*                                  SORT PARAMETERS                              
SORTCARD DC    CL80' '                                                          
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(909,,,,) '                               
*                                  DEFAULT PROFILE                              
DEFAULT  DC    C'*****NYNN YNY   '                                              
*                                  SORT KEY DEFINITIONS !DISP/LEN)              
SORTKEYS DS    0CL3                                                             
         DC    C'O',AL1(21,02)                                                  
         DC    C'C',AL1(23,15)                                                  
         DC    C'D',AL1(38,03)                                                  
         DC    C'R',AL1(41,06)                                                  
         DC    C'M',AL1(47,02)                                                  
         DC    C'B',AL1(47,06)                                                  
         DC    X'FF'                                                            
*                                  PROFILE VALUE KEYWORDS                       
PROFLIST DS    0CL5                                                             
         DC    C'SEST='                                                         
         DC    C'PEST='                                                         
         DC    C'JJOB='                                                         
         DC    C'MCMP='                                                         
         DC    X'FF',C'NUM='                                                    
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        LITERAL POOL                                                           
*---------------------------------------------------------------------*         
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        ROUTINE TO LOOK-UP ANALYSIS CODE IN LEDGER                             
*---------------------------------------------------------------------*         
GETANAL  NMOD1 0,**GETA                                                         
         LR    RC,R1                                                            
         MVC   THISANL1,SPACES                                                  
         MVC   THISANL2,SPACES                                                  
         CLC   THISANAL,=C'99'                                                  
         BNE   *+14                                                             
         MVC   THISANL1(14),=C'PREVIOUS BILLS'                                  
         B     GETAN8                                                           
         MVC   THISANL1(10),=C'WORK CODE='                                      
         MVC   THISANL1+10(2),THISANAL                                          
         CLC   THISANAL,SPACES                                                  
         BNE   *+10                                                             
         MVC   THISANL1(10),=CL10'OTHERS'                                       
         L     R2,ADLEDGER                                                      
         AH    R2,DATADISP                                                      
         SR    R1,R1                                                            
*                                                                               
GETAN2   CLI   0(R2),0                                                          
         BE    GETAN8                                                           
         CLI   0(R2),X'12'                                                      
         BE    GETAN6                                                           
*                                                                               
GETAN4   IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     GETAN2                                                           
*                                                                               
         USING ACANALD,R2                                                       
*                                                                               
GETAN6   MVC   WORK(2),ACANCODE                                                 
         OC    WORK(2),SPACES                                                   
         CLC   WORK(2),THISANAL                                                 
         BNE   GETAN4                                                           
         MVC   THISANL1(L'ACANDESC),ACANDESC                                    
*                                  UNDERLINE IN THISANL2                        
GETAN8   MVI   THISANL2,C'-'                                                    
         MVC   THISANL2+1(L'THISANL2-1),THISANL2                                
         LA    R1,THISANL2+L'THISANL2-1                                         
         LA    RE,THISANL1+L'THISANL1-1                                         
         CLI   0(RE),C' '                                                       
         BNE   *+14                                                             
         MVI   0(R1),C' '                                                       
         BCTR  RE,0                                                             
         BCT   R1,*-14                                                          
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        WORK CONSTANTS                                                         
*---------------------------------------------------------------------*         
*                                                                               
         ENTRY WORKC                                                            
WORKC    DS    0D                                                               
         DS    10000C                                                           
*                                                                               
         ENTRY OFFBUFF                                                          
OFFBUFF  DS    0D                                                               
         DS    (36*36*38)C                                                      
*                                                                               
         ENTRY PRNTBLOC                                                         
PRNTBLOC DS    0D                                                               
         DS    20CL132                                                          
         DS    CL500               A LITTLE EXTRA FOR CHOPPER                   
*                                                                               
         BUFF  LINES=001,ROWS=2,COLUMNS=3,COMMENT=36,FLAVOR=PACKED,    *        
               KEYLIST=(17,A)                                                   
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        DSECT TO COVER SAVE W/S                                                
*---------------------------------------------------------------------*         
WORKD    DSECT                                                                  
RELO     DS    F                                                                
*                                                                               
SWITCHES DS    0CL16               ACTIVITY SWITCHES                            
READSW   DS    C                                                                
OFFSW    DS    C                                                                
NEWOFF   DS    C                                                                
NEWCST   DS    C                                                                
FLTSW    DS    C                                                                
SORTSW   DS    C                                                                
PRTSW    DS    C                                                                
ACCSW    DS    C                                                                
ANALSW   DS    C                                                                
SBACSW   DS    C                                                                
TRNSSW   DS    C                                                                
FORMSW   DS    C                                                                
SAVESW   DS    C                                                                
         DS    CL4                                                              
FRSTTRN  DS    C                                                                
*                                                                               
ACCCNT   DS    PL4                                                              
TRNSCNT  DS    PL4                                                              
LEVACNT  DS    PL4                                                              
LEVBCNT  DS    PL4                                                              
LEVCCNT  DS    PL4                                                              
TEMPCNT  DS    PL4                                                              
ANALCNT  DS    PL4                                                              
MACMABAL DS    PL8                 BBF FOR ACCOUNT                              
SAVSBAC2 DS    CL15                                                             
*                                                                               
LEDGPROF DS    CL16                                                             
PSTART   DS    PL3                 PACKED START DATE (OR ZEROES)                
PEND     DS    PL3                 PACKED END DATE (OR X'FF')                   
TODAY3   DS    CL3                                                              
TODAY2   DS    CL2                                                              
LDGBLFWD DS    PL8                 MY BBF BY LEDGER                             
INACTOT  DS    PL8                                                              
TOTDR    DS    PL8                                                              
TOTCR    DS    PL8                                                              
SUBPROG  DS    X                                                                
SAVEMODE DS    X                                                                
SORTMODE DS    X                                                                
SORTPRNT EQU   X'80'                                                            
SORTALL  EQU   X'40'                                                            
SAVELVL  DS    X                                                                
MAXLVL   DS    CL1                                                              
PRNTSW   DS    CL1                                                              
FORMACCS DS    4PL8                                                             
*                                  THIS TIME SAVE VALUES                        
THSACC   DS    CL15                                                             
THSACCT  DS    CL20                                                             
THISNAME DS    CL36                                                             
THISANAL DS    CL2                                                              
THISANL1 DS    CL15                                                             
THISANL2 DS    CL15                                                             
THISSBAC DS    CL15                                                             
THISSBNM DS    CL36                                                             
*HISHEDA DS    CL31                                                             
THISHED1 DS    CL50                                                             
THISHED2 DS    CL50                                                             
THISHED3 DS    CL50                                                             
THISHED4 DS    CL50                                                             
THISHED5 DS    CL64                                                             
THISHED6 DS    CL50                                                             
THISHED7 DS    CL50                                                             
THSKEY   DS    CL42                                                             
THISNUM  DS    PL8                                                              
THISWORK DS    CL15                                                             
NEWKEY   DS    CL42                                                             
SAVEKEY  DS    CL42                                                             
CNAME    DS    CL36                                                             
*                                  BUFFER RECORD AREA                           
BUFFREC  DS    0CL77                                                            
BUFFKEY  DS    0CL17                                                            
BUFFTYPE DS    X                                                                
BUFFCODE DS    CL15                CAN BE ACCOUNT OR CONTRA                     
BUFFLVL  DS    X                                                                
BUFFNAME DS    CL36                                                             
BUFFDR   DS    PL8                                                              
BUFFCR   DS    PL8                                                              
BUFFCT   DS    PL8                 COUNTER CONTROL, TO ENSURE SAVE              
*                                  RETURN OF ALL ITEMS SENT TO BUFFALO          
*                                  CHOPCON W/S                                  
CHOPSAVE DS    CL256                                                            
CHOPSAV2 DS    CL256                                                            
CHOPBLOC DS    CL256                                                            
LEDGFILT DS    CL250                                                            
CHOPNUM  DS    X                                                                
CHOPNSV  DS    X                                                                
*                                                                               
SAVESBAC DS    5CL22                                                            
SAVEASRT DS    A                                                                
BACKSORT DS    F                                                                
*                                                                               
ACCUMS   DS    CL224               9X3 8BYTE PACKED ACCUMS  +8                  
*                                                                               
CATBLK   DS    CL(CADCLNQ)         CATEGORY DECODE BLOCK                        
*                                                                               
**TSORTBRK  DS    CL64                SORT C/B INFO                             
SORTBRK  DS    CL66                SORT C/B INFO                                
NARRSW   DS    C                                                                
*                                                                               
         CSECT                                                                  
SRTREC   DS    CL1000                                                           
*                                                                               
**TSVSORT   DS    CL750                                                         
SVSORT   DS    CL1000                                                           
*                                                                               
SRTRECA  DS    CL1000                                                           
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        DSECT TO COVER TRANSACTION SORT RECORD                                 
*---------------------------------------------------------------------*         
SRTRECD DSECT                                                                   
SORTKEY  DS    0CL37                                                            
SORTRLEN DS    H                   VARIABLE SORT RECORD LENGTH                  
         DS    H                                                                
SORTACCT DS    CL15                                                             
SRTRECT  DS    CL1                                                              
SLEGFTYP EQU   C'0'                FIRST FOR LEDGER                             
SACCTYP  EQU   C'1'                FIRST FOR ACCOUNT                            
SANATYP  EQU   C'2'                FIRST FOR ANALYSIS CODE                      
STRNTYP  EQU   C'3'                PROCESS TRANSACTION                          
SACCLTYP EQU   C'4'                LAST FOR ACCOUNT                             
SLVCTYP  EQU   C'5'                LAST FOR LEVEL C                             
SLVBTYP  EQU   C'6'                      "        B                             
SLVATYP  EQU   C'7'                      "        A                             
SLEGLTYP EQU   C'8'                LAST FOR LEDGER                              
*                                                                               
SORTOFFC DS    CL2                                                              
SORTSBAC DS    CL15                                                             
SORTDATE DS    CL3                                                              
SORTREF  DS    CL6                                                              
SORTMOS  DS    CL6                                                              
SORTDATA DS    0C                                                               
SORTTYPE DS    XL1                                                              
SORTSTAT DS    XL1                                                              
SORTAMNT DS    PL8                                                              
SORTBASE DS    PL6                                                              
SORTRATE DS    PL4                                                              
SORTLOC  DS    CL9                                                              
SORTSBNM DS    CL36                                                             
*                                                                               
SCHOPBL  DS    CL256                                                            
SCHOPNM  DS    CL1                                                              
STRNMAX  DS    CL1                                                              
STRNCNM  DS    CL36                                                             
*                                                                               
SORTLEN  EQU   *-SRTRECD                                                        
SORTNARR DS    0C                                                               
*                                                                               
         ORG   SORTDATA                                                         
SACCNM   DS    CL36                                                             
SACCNUM1 DS    CL9                                                              
SACCNUM2 DS    CL10                                                             
SACCFORM DS    CL20                                                             
SHEAD1   DS    CL50                                                             
SHEAD2   DS    CL50                                                             
SHEAD3   DS    CL50                                                             
SHEAD4   DS    CL50                                                             
SHEAD5   DS    CL50                                                             
SHEAD6   DS    CL50                                                             
SHEAD7   DS    CL50                                                             
SACCLEN  EQU   *-SORTRLEN                                                       
*                                                                               
*                                                                               
         ORG   SORTDATA                                                         
SLDGBALF DS    CL(L'LDGBLFWD)                                                   
SACLDESC DS    CL15                                                             
SACLNAME DS    CL36                                                             
SACCLLEN EQU   *-SORTRLEN                                                       
*                                                                               
*                                                                               
         ORG   SORTDATA                                                         
SRTANAL  DS    CL(L'THISANAL)                                                   
SANALEN  EQU   *-SORTRLEN                                                       
*                                                                               
*                                                                               
         ORG   SORTDATA                                                         
SRTFLTSW DS    CL(L'FLTSW)                                                      
SRTPRTSW DS    CL(L'PRTSW)                                                      
SLEGLEN  EQU   *-SORTRLEN                                                       
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        STANDARD DSECTS                                                        
*---------------------------------------------------------------------*         
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACCATKIND                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCATKIND                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREP3102S05/01/02'                                      
         END                                                                    
