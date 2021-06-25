*          DATA SET ACREPB202  AT LEVEL 007 AS OF 05/01/02                      
*PHASE ACB202A,*                                                                
*INCLUDE BUDACC                                                                 
*INCLUDE CENTER                                                                 
         TITLE 'BUDGET COMPARISION'                                             
ACB202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACB2**,R9,R8,RR=R5                                           
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING PROGD,RC                                                         
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
         CLI   MODE,RUNFRST                                                     
         BNE   BC10                                                             
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         B     XIT                                                              
                                                                                
XIT      XMOD1                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
BC10     CLI   MODE,REQFRST                                                     
         BNE   BC100                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVC   LISTRUL,PROGPROF                                                 
         XC    DATES,DATES                                                      
         MVC   QSTART+4(2),=C'15'                                               
         MVC   QEND+4(2),=C'15'                                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(1,WORK)                                  
         CLI   QOPT3,C' '                                                       
         BE    *+10                                                             
         MVC   STYTD,WORK                                                       
         CLI   QOPT4,C' '                                                       
         BE    *+10                                                             
         MVC   STYR,WORK                                                        
         GOTO1 (RF),(R1),(0,QEND)                                               
         CLI   QOPT3,C' '                                                       
         BE    *+10                                                             
         MVC   ENYTD,WORK                                                       
         CLI   QOPT2,C' '                                                       
         BE    BC14                                                             
         MVC   ENCUR,WORK                                                       
                                                                                
         SR    RF,RF                                                            
         CLI   QOPT2,C'Y'                                                       
         BE    BC12                                                             
*MN      L     RF,=F'-29'          NO OF MONTHS BACK FOR CURRENT                
*MN      PACK  DUB,QOPT2                                                        
*MN      CVB   RE,DUB                                                           
*MN      BCTR  RE,0                                                             
*MN      STH   RE,HALF                                                          
*MN      MH    RF,HALF                                                          
         PACK  DUB,QOPT2                                                        
         CVB   RF,DUB                                                           
         BCTR  RF,0                                                             
                                                                                
BC12     DS    0H                                                               
*MN      GOTO1 ADDAY,DMCB,QEND,WORK+6,(RF)                                      
         GOTO1 ADDAY,DMCB,(C'M',QEND),WORK+6,(RF)                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK)                                  
         MVC   STCUR,WORK                                                       
                                                                                
BC14     CLI   QOPT4,C' '                                                       
         BE    BC30                                                             
*MN      LA    RF,335              12 MONTH DATES                               
         LA    RF,11               ONE YEAR                                     
         CLI   QOPT4,C'Y'                                                       
         BE    BC16                                                             
         PACK  DUB,QOPT4(2)                                                     
         CVB   RE,DUB                                                           
         BCTR  RE,0                                                             
*MN      STH   RE,HALF                                                          
*MN      LA    RF,30                                                            
*MN      MH    RF,HALF                                                          
                                                                                
BC16     DS    0H                                                               
*MN      GOTO1 ADDAY,DMCB,QSTART,WORK+6,(RF)                                    
         GOTO1 ADDAY,DMCB,(C'M',QSTART),WORK+6,(RF)                             
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK)                                  
         MVC   ENYR,WORK                                                        
BC30     DS    0H                                                               
         LA    RE,QSRTAREA+6       CALCULATE CHUNK WIDTH                        
         CLI   0(RE),0                                                          
         BNE   *+8                                                              
         BCT   RE,*-8                                                           
         LA    RF,QSRTAREA                                                      
         SR    RE,RF                                                            
         LA    RE,1(RE)                                                         
         STC   RE,CHNKCOLS                                                      
                                                                                
         LA    RF,7                FIND OUT RIGHT-MOST BUDGET                   
         LA    RE,QSRTAREA+6                                                    
         MVI   RIGHTBUD,0                                                       
BC32     CLI   0(RE),0                                                          
         BE    BC34                                                             
         CLI   0(RE),249                                                        
         BH    BC34                                                             
         MVC   RIGHTBUD,0(RE)                                                   
         B     BC36                                                             
BC34     BCTR  RE,0                                                             
         BCT   RF,BC32                                                          
BC36     DS    0H                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        READ TYPE RECORDS                                                      
*-------------------------------------------------------------------*           
         MVC   SVKEY(L'KEY),KEY                                                 
         L     R5,ADBUDC                                                        
         USING BUDACCD,R5                                                       
         MVC   BUADMGR,DATAMGR                                                  
         MVC   BUDTCON,DATCON                                                   
         MVI   BUTYPE,READING                                                   
         MVI   BUCMND,TYPEGET                                                   
         MVC   BUSERKEY(1),QCOMPANY                                             
                                                                                
         MVC   COLHEAD1,SPACES                                                  
         MVC   COLHEAD2,SPACES                                                  
         LA    R2,QSRTAREA                                                      
         LA    R3,COLHEAD1                                                      
         LR    R4,RE               CHUNK WIDTH                                  
BC38     CLI   0(R2),0                                                          
         BE    BC53                                                             
         CLI   0(R2),249           LIMIT FOR FIXED WORDS                        
         BH    BC50                                                             
         L     R5,ADBUDC                                                        
         MVC   BUBUDNO+1(1),0(R2)  BUDGET TYPE CODE FROM REQUEST                
         GOTO1 BUDACC,DMCB,ADBUDC                                               
         CLI   BUMODE,PROCTYPE     GOOD READ                                    
         BNE   BC50                NO - GET NEXT                                
         XC    CBUDLEV,CBUDLEV                                                  
         LA    RF,BUTYPREC                                                      
         DROP  R5                                                               
         AH    RF,DATADISP                                                      
         SR    RE,RE                                                            
BC40     CLI   0(RF),0                                                          
         BE    BC50                                                             
         CLI   0(RF),X'1B'         FIND COL NAMES AND VALID U/LS                
         BE    BC44                                                             
         CLI   0(RF),X'1C'                                                      
         BE    BC46                                                             
BC42     IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     BC40                                                             
                                                                                
         USING ACBCD,RF                                                         
BC44     DS    0H                                                               
         LA    R1,ACBCCOL1+9       RIGHT ALIGN COLUMN HEADINGS                  
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,ACBCCOL1                                                      
         SR    R1,R0                                                            
         LA    R5,10                                                            
         SR    R5,R1                                                            
         BCTR  R5,0                                                             
         LA    R5,0(R5,R3)                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ACBCCOL1                                                 
         CLC   ACBCCOL2,SPACES                                                  
         BE    BC42                                                             
         LA    R1,ACBCCOL2+9                                                    
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,ACBCCOL2                                                      
         SR    R1,R0                                                            
         LA    R5,10                                                            
         SR    R5,R1                                                            
         BCTR  R5,0                                                             
         LA    R5,0(R5,R3)                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   90(0,R5),ACBCCOL2                                                
         B     BC42                                                             
                                                                                
         USING ACBVD,RF                                                         
BC46     CLC   ACBVACUL,QUNIT                                                   
         BNE   BC42                                                             
         MVC   ABUDLEV,ACBVACLV    SAVE ACCOUNT LEVEL INFO                      
         LA    R1,CBUDLEV          BUILD TABLE OF CONTRA ACCT RULES             
         ZIC   R6,ACBVLEN                                                       
         LR    R5,RF                                                            
         AR    R5,R6                                                            
         ST    R5,FULL                                                          
         CH    R6,=H'5'            NO CONTRA RULES AT ALL                       
         BNH   BC42                                                             
         LA    R6,ACBVCAUN                                                      
BC47     DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    BC47A                                                            
         MVC   0(3,R1),0(R6)                                                    
         LA    R1,3(R1)                                                         
BC47A    LA    R6,3(R6)                                                         
         C     R6,FULL             END OF ELEMENT                               
         BL    BC47                                                             
         B     BC42                                                             
                                                                                
BC50     LA    R3,10(R3)                                                        
         LA    R2,1(R2)                                                         
         BCT   R4,BC38                                                          
                                                                                
BC53     CLI   CBUDLEV,0           ANY CONTRA RULES                             
         BE    BC60                                                             
         LA    R3,CBUDLEV          FIND LENGTHS FROM CONTRA-LEDGER              
BC54     DS    0H                  HEIRARCHY ELEMENT                            
         CLI   2(R3),0             LEDGER LEVEL ONLY                            
         BE    BC58                IF SO - GET NEXT                             
         CLI   0(R3),C'+'          FUNNY RULE SO SKIP READ                      
         BE    BC58                                                             
         L     R2,=A(IOA)                                                       
         MVC   0(42,R2),SPACES                                                  
         MVC   0(1,R2),QCOMPANY                                                 
         MVC   1(2,R2),0(R3)       UNIT/LEDGER                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R2),(R2),(0,0)              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         AH    R2,DATADISP                                                      
         SR    R1,R1                                                            
BC56     CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'16'         HEIR EL.                                     
         BE    BC57                                                             
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     BC56                                                             
         USING ACHEIRD,R2                                                       
BC57     LA    RF,ACHRLEVA                                                      
         CLI   2(R3),1                                                          
         BE    BC57A                                                            
         LA    RF,ACHRLEVB                                                      
         CLI   2(R3),2                                                          
         BE    BC57A                                                            
         LA    RF,ACHRLEVC                                                      
         CLI   2(R3),3                                                          
         BE    BC57A                                                            
         LA    RF,ACHRLEVD                                                      
BC57A    MVC   2(1,R3),0(RF)                                                    
                                                                                
BC58     LA    R3,3(R3)            GET NEXT CONTRA ENTRY                        
         CLI   0(R3),0                                                          
         BNE   BC54                                                             
         B     BC60                                                             
         DROP  R2                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
BC60     BAS   RE,SVREAD                                                        
         LA    R1,QOPT4            CALCULATE NUMBER OF CHUNKS                   
         SR    RF,RF                                                            
         LA    RE,3                                                             
BC62     CLI   0(R1),C' '                                                       
         BE    *+8                                                              
         LA    RF,1(RF)                                                         
         BCTR  R1,0                                                             
         BCT   RE,BC62                                                          
         STC   RF,CHNKNO                                                        
         STH   RF,HALF                                                          
                                                                                
         ZIC   RE,PAGEWDTH         CALCULATE HOW TO DIVIDE UP PAGE              
         ZIC   R1,CHNKCOLS                                                      
         MH    R1,HALF                                                          
         MH    R1,=H'11'           WIDTH OF ONE COLUMN                          
         SR    RE,R1                                                            
         BCTR  RE,0                                                             
         STC   RE,ACCWDTH          ACCOUNT WIDTH                                
         LA    RE,1(RE)                                                         
         STC   RE,COLSTRT          START OF DATA                                
         CH    RE,=H'50'           MOVE DATA LEFT IF ONLY A FEW COLUMNS         
         BL    *+12                                                             
         MVI   ACCWDTH,49                                                       
         MVI   COLSTRT,50                                                       
         ZIC   RE,COLSTRT                                                       
         ZIC   R1,PAGEWDTH         CALCULATE WIDTH OF DATA                      
         SR    R1,RE                                                            
         BCTR  R1,0                                                             
         STC   R1,DATAWDTH                                                      
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        BUILD HEADLINES                                                        
*-------------------------------------------------------------------*           
         ZIC   R1,CHNKCOLS                                                      
         MH    R1,=H'11'                                                        
         STC   R1,CHNKWDTH         WIDTH OF ONE CHUNK                           
         MVC   WORK,SPACES                                                      
         LA    R2,DATES            BUILD DATE LINE                              
         LA    R3,3                                                             
         ZIC   RF,COLSTRT                                                       
         LA    R4,SVH1(RF)                                                      
         MVI   SVH1,C' '                                                        
         MVC   SVH1+1(L'SVH1-1),SVH1                                            
         MVC   SVH2,SVH1                                                        
         MVC   SVH3,SVH1                                                        
BC64     OC    0(4,R2),0(R2)                                                    
         BZ    BC66                                                             
         MVC   DUB(2),0(R2)                                                     
         MVI   DUB+2,X'01'                                                      
         GOTO1 DATCON,DMCB,(1,DUB),(9,WORK)                                     
         MVC   DUB(2),2(R2)                                                     
         CLC   0(2,R2),2(R2)       ONE MONTH WIDE                               
         BE    BC64A                                                            
         GOTO1 (RF),(R1),,(9,WORK+7)                                            
BC64A    MVC   0(13,R4),WORK                                                    
         ZIC   RF,CHNKWDTH                                                      
         GOTO1 CENTER,DMCB,(R4),(RF)                                            
         ZIC   RF,CHNKWDTH                                                      
         AR    R4,RF                                                            
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         LR    RE,R4                                                            
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
BC65     CLI   0(RE),C' '                                                       
         BNE   BC65A                                                            
         MVI   0(RE),C'-'                                                       
BC65A    BCTR  RE,0                                                             
         BCT   RF,BC65                                                          
BC66     LA    R2,4(R2)            PUT DASHES ON DATE LINE                      
         BCT   R3,BC64                                                          
                                                                                
         LA    R3,COLHEAD1         PUT FIXED WORDS INTO COL HEADINGS            
         LA    RF,QSRTAREA                                                      
         LA    R1,7                                                             
BC70     CLI   0(RF),250                                                        
         BL    BC76                                                             
         LA    RE,FIXED                                                         
BC72     CLC   0(1,RE),0(RF)                                                    
         BE    BC74                                                             
         LA    RE,11(RE)                                                        
         CLI   0(RE),X'FF'                                                      
         BE    BC76                                                             
         B     BC72                                                             
                                                                                
BC74     MVC   0(10,R3),1(RE)                                                   
BC76     LA    R3,10(R3)                                                        
         LA    RF,1(RF)                                                         
         BCT   R1,BC70                                                          
                                                                                
*        MOVE COL HEADINGS TO PRINT LINES                                       
                                                                                
         LA    R3,COLHEAD1                                                      
         ZIC   RF,CHNKNO                                                        
         ZIC   RE,COLSTRT                                                       
         LA    R4,SVH2(RE)                                                      
BC78     ZIC   RE,CHNKCOLS                                                      
BC80     MVC   0(11,R4),0(R3)                                                   
         MVC   132(11,R4),90(R3)                                                
         LA    R4,11(R4)                                                        
         LA    R3,10(R3)                                                        
         BCT   RE,BC80                                                          
                                                                                
         LA    R3,COLHEAD1                                                      
         BCT   RF,BC78                                                          
                                                                                
         XC    MASK,MASK           BUILD CONTROL MASK FOR TABLE LINE            
         ZIC   RF,CHNKCOLS                                                      
         MH    RF,=H'8'                                                         
         SR    R3,R3                                                            
         LA    R1,QSRTAREA                                                      
         LA    R2,MASK                                                          
BC80A    CLI   0(R1),0                                                          
         BE    BC81                                                             
         ZIC   RE,CHNKNO                                                        
BC80B    MVC   0(1,R2),0(R1)                                                    
         AR    R2,RF                                                            
         BCT   RE,BC80B                                                         
         LA    R1,1(R1)                                                         
         LA    R3,8(R3)                                                         
         LA    R2,MASK(R3)                                                      
         B     BC80A                                                            
BC81     DS    0H                                                               
         LA    R2,MASK                                                          
         IC    RE,CHNKNO                                                        
         LA    R3,DATES                                                         
BC81A    OC    0(2,R3),0(R3)                                                    
         BNZ   BC81B                                                            
         LA    R3,4(R3)                                                         
         B     BC81A                                                            
BC81B    IC    RF,CHNKCOLS                                                      
BC81C    MVC   1(4,R2),0(R3)                                                    
         MVC   5(2,R2),2(R3)       EXTRA END DATE                               
         LA    R2,8(R2)                                                         
         BCT   RF,BC81C                                                         
         LA    R3,4(R3)                                                         
         BCT   RE,BC81A                                                         
                                                                                
         LA    RF,TABLE            SET UP TABLE                                 
         LA    RE,54               9*6                                          
BC84     ZAP   0(6,RF),=P'0'                                                    
         LA    RF,6(RF)                                                         
         BCT   RE,BC84                                                          
         MVI   REQLEV,0                                                         
         LA    RE,REQLVTAB         CONVERT QOPT1 TO A LINE NUMBER               
BC86     CLI   0(RE),X'FF'         FOR TABLE POSITIONING                        
         BE    BC90                                                             
         CLC   QOPT1,0(RE)                                                      
         BE    BC88                                                             
         LA    RE,1(RE)                                                         
         B     BC86                                                             
BC88     MVC   REQLEV,1(RE)                                                     
BC90     DS    0H                                                               
*-----------------------------------------------------------------*             
*        HANDLE YEARLY/ACTUAL OPTION                                            
*-----------------------------------------------------------------*             
         CLI   QOPT4,C' '                                                       
         BE    XIT                 NO YEARLY DATA REQUESTED                     
         ZIC   RF,CHNKNO                                                        
         BCTR  RF,0                                                             
         XC    FULL,FULL                                                        
         MVC   FULL+3(1),CHNKCOLS                                               
         MH    RF,FULL+2                                                        
         MH    RF,=H'8'                                                         
         LA    R2,MASK(RF)         POINT TO START OF YEARLY COLS                
         ZIC   R3,CHNKCOLS                                                      
BC92     CLI   0(R2),250           ACTUAL COLUMN                                
         BNL   BC94                                                             
BC93     LA    R2,8(R2)                                                         
         BCT   R3,BC92                                                          
         B     XIT                                                              
BC94     GOTO1 DATCON,DMCB,(0,QEND),(1,WORK)                                    
         MVC   3(2,R2),WORK        FORCE YEARLY END TO BE YTD END               
         OI    7(R2),X'80'         SET STATUS = ACTUAL                          
         CLI   QOPT6,C'Y'                                                       
         BNE   BC93                                                             
         OI    7(R2),X'40'         SET STATUS = PROJECTION                      
         B     BC93                                                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
BC100    CLI   MODE,LEDGFRST                                                    
         BNE   BC120                                                            
         L     R4,ADLDGHIR                                                      
         USING ACHEIRD,R4                                                       
         MVI   LEDGLEV,1                                                        
         CLI   ACHRLEVB,0                                                       
         BE    XIT                                                              
         MVI   LEDGLEV,2                                                        
         CLI   ACHRLEVC,0                                                       
         BE    XIT                                                              
         MVI   LEDGLEV,3                                                        
         CLI   ACHRLEVD,0                                                       
         BE    XIT                                                              
         MVI   LEDGLEV,4                                                        
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
BC120    CLI   MODE,PROCLEVA                                                    
         BNE   BC130                                                            
         L     R1,ADHEIRA                                                       
         ZIC   RF,LEDGLEV                                                       
         MH    RF,=H'54'           POINT TO LEV A LINE                          
         LA    R3,LINE1(RF)                                                     
         ST    R3,LNADD                                                         
         CLI   LEDGLEV,1                                                        
         BE    *+8                                                              
         BAS   RE,GENBUD                                                        
         CLI   QOPT1,C'C'                                                       
         BNE   XIT                                                              
         CLI   ABUDLEV,1                                                        
         BNE   XIT                                                              
         BAS   RE,GENCBUD                                                       
         B     XIT                                                              
                                                                                
BC130    CLI   MODE,PROCLEVB                                                    
         BNE   BC140                                                            
         L     R1,ADHEIRB                                                       
         LA    R3,LINE3            LEV B TOTAL ON LINE 3 FOR 3-DEEP             
         CLI   LEDGLEV,3           LEDGER OR LINE 4 FOR 4-DEEP LEDGER           
         BE    *+8                                                              
         LA    R3,LINE4                                                         
         ST    R3,LNADD                                                         
         CLI   LEDGLEV,3                                                        
         BL    *+8                                                              
         BAS   RE,GENBUD                                                        
         CLI   QOPT1,C'C'                                                       
         BNE   XIT                                                              
         CLI   ABUDLEV,1                                                        
         BE    XIT                                                              
         BAS   RE,GENCBUD                                                       
         B     XIT                                                              
                                                                                
BC140    CLI   MODE,PROCLEVC                                                    
         BNE   BC200                                                            
         L     R1,ADHEIRC                                                       
         LA    RE,LINE3                                                         
         ST    RE,LNADD                                                         
         CLI   LEDGLEV,4                                                        
         BNE   *+8                                                              
         BAS   RE,GENBUD                                                        
         CLI   QOPT1,C'C'                                                       
         BNE   XIT                                                              
         CLI   ABUDLEV,3                                                        
         BL    XIT                                                              
         BAS   RE,GENCBUD                                                       
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
BC200    CLI   MODE,PROCACC                                                     
         BNE   BC260                                                            
         MVC   SVKEY(L'KEY),KEY                                                 
         L     R5,ADBUDC                                                        
         USING BUDACCD,R5                                                       
         MVI   BUTYPE,READING                                                   
         L     RF,ADACC            READ BUDGET AMOUNTS                          
         MVC   BUSERKEY,SPACES                                                  
         MVC   BUSERKEY(15),0(RF)                                               
         XC    BULSTKEY,BULSTKEY                                                
         CLI   QOPT1,C'C'                                                       
         BNE   BC201                                                            
         CLC   ABUDLEV,LEDGLEV                                                  
         BL    XIT                                                              
         L     R1,ADACC                                                         
         BAS   RE,GENCBUD                                                       
         B     XIT                                                              
                                                                                
BC201    ZIC   R2,CHNKCOLS                                                      
         LA    R3,QSRTAREA                                                      
BC202    CLI   0(R3),0                                                          
         BE    BC230                                                            
         CLI   0(R3),249                                                        
         BH    BC222                                                            
         MVC   BUBUDNO+1(1),0(R3)  TYPE CODE FROM REQUEST                       
         MVI   BUCMND,0                                                         
         XC    BULSTKEY,BULSTKEY                                                
BC204    GOTO1 BUDACC,DMCB,ADBUDC                                               
         CLI   BUMODE,FINISHED                                                  
         BE    BC222                                                            
         CLI   BUMODE,DMGRERR                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BUSERKEY(15),BULSTKEY+1                                          
         BNE   BC222                                                            
         CLI   BUMODE,PROCAMNT                                                  
         BNE   BC204                                                            
         LA    R4,BUAMTREC                                                      
         AH    R4,DATADISP                                                      
BC210    CLI   0(R4),0             SCAN AMOUNT RECORD FOR AMOUNT ELS            
         BE    BC204                                                            
         CLI   0(R4),X'1D'                                                      
         BE    BC214                                                            
BC212    ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     BC210                                                            
                                                                                
         USING ACBAD,R4                                                         
BC214    LA    RE,LINE2                                                         
         LA    R1,MASK                                                          
         LA    RF,9                                                             
BC216    DS    0H                                                               
         CLI   0(R1),250           ACTUAL COLUMN                                
         BL    BC217                                                            
         CLC   RIGHTBUD,0(R3)                                                   
         BNE   BC218                                                            
         TM    7(R1),X'40'         PROJECTION REQUIRED                          
         BZ    BC218                                                            
         CLC   ACBAMNTH,3(R1)      IF SO ADD IN BUDGET FOR REST OF              
         BNH   BC218               YEAR TO ACTUAL FOR FIRST PART OF YR          
         CLC   ACBAMNTH,5(R1)                                                   
         BH    BC218                                                            
         AP    0(6,RE),ACBABUDG                                                 
         B     BC218                                                            
BC217    DS    0H                  ADD TO CORRECT PLACE IN TABLE                
         CLC   0(1,R1),0(R3)                                                    
         BNE   BC218                                                            
         CLC   ACBAMNTH,1(R1)                                                   
         BL    BC218                                                            
         CLC   ACBAMNTH,3(R1)                                                   
         BH    BC218                                                            
         AP    0(6,RE),ACBABUDG                                                 
BC218    LA    R1,8(R1)                                                         
         LA    RE,6(RE)                                                         
         BCT   RF,BC216                                                         
         B     BC212                                                            
                                                                                
BC222    LA    R3,1(R3)                                                         
         BCT   R2,BC202                                                         
                                                                                
BC230    BAS   RE,SVREAD                                                        
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
BC260    CLI   MODE,SBACFRST                                                    
         BNE   BC280                                                            
         L     RF,ADSUBAC                                                       
         USING TRSUBHD,RF                                                       
         MVC   CONTRAC,TRSBACNT                                                 
         MVC   CONTRNM,SPACES                                                   
         ZIC   R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CONTRNM(0),TRSBNAME                                              
*&&US                                                                           
         CLI   PROGPROF+2,C'Y'     SPECIAL TYPE E FOR COKE                      
         BNE   XIT                                                              
         LA    R0,NPRODS           CONVERT PRODUCT CODE                         
         L     RE,APRODTAB                                                      
SBAC042  CLC   CONTRAC+3(2),2(RE) MATCH ON CHARACTER CODE                       
         BE    SBAC043                                                          
         LA    RE,L'PRODTAB(RE)                                                 
         BCT   R0,SBAC042                                                       
         DC    H'0'                PRODUCT NOT FOUND                            
SBAC043  MVC   CONTRAC+3(2),0(RE) NUMERIC PRODUCT CODE                          
         MVC   CONTRNM,4(RE)       NAME FROM TABLE                              
*&&                                                                             
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
BC280    CLI   MODE,PROCHIST                                                    
         BNE   BC320                                                            
         L     RF,ADTRANS                                                       
         CLI   0(RF),X'45'                                                      
         BNE   XIT                                                              
*&&US                                                                           
         CLI   PROGPROF+2,C'Y'                                                  
         BNE   *+12                SPECIAL FOR COKE TYPE 'E'                    
         CLI   BUCKTYPE,C'E'                                                    
         B     *+8                                                              
*&&                                                                             
         CLI   BUCKTYPE,C' '       CASH BUCKETS ONLY                            
         BNE   XIT                                                              
         USING TRHISTD,RF                                                       
         LA    R1,MASK                                                          
         LA    RE,LINE1                                                         
         LA    R2,9                                                             
BC282    CLI   0(R1),250           PUT AMOUNTS INTO ACTUAL ETC COLS             
         BL    BC284                                                            
         CLC   TRHSYEAR(2),1(R1)                                                
         BL    BC284                                                            
         CLC   TRHSYEAR(2),3(R1)                                                
         BH    BC284                                                            
         L     R3,ADACC                                                         
*&&US                                                                           
         CLI   PROGPROF+2,C'Y'                                                  
         BE    BC282E              SPECIAL TYPE E BUCKETS                       
*&&                                                                             
         CLC   1(2,R3),=C'SI'                                                   
         BE    BC283                                                            
         CLC   1(2,R3),=C'28'                                                   
         BE    BC283                                                            
         CLC   1(2,R3),=C'29'                                                   
         BE    BC283                                                            
         CLC   1(2,R3),=C'13'                                                   
         BE    BC283                                                            
         CLC   1(2,R3),=C'1C'                                                   
         BNE   BC282A                                                           
         L     R4,ADSUBAC            SOME FUNNY STUFF FOR 1C                    
         CLC   3(2,R4),=C'11'                                                   
         BE    BC282A                                                           
         CLC   3(2,R4),=C'12'                                                   
         BE    BC282A                                                           
         B     BC283                                                            
BC282A   AP    0(6,RE),TRHSDR      ADD DEBITS                                   
         SP    0(6,RE),TRHSCR      SUBTRACT CREDITS                             
         B     BC284                                                            
BC282E   AP    0(6,RE),TRHSDR      ADD DEBITS ONLY FOR TYPE E                   
         B     BC284                                                            
BC283    DS    0H                                                               
         SP    0(6,RE),TRHSDR      EXCEPT FOR 28 AND 29                         
         AP    0(6,RE),TRHSCR      AND 13                                       
BC284    LA    RE,6(RE)                                                         
         LA    R1,8(R1)                                                         
         BCT   R2,BC282                                                         
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
BC320    CLI   MODE,SBACLAST                                                    
         BNE   BC400                                                            
         CLI   LISTRUL,C'Y'                                                     
         BNE   BC330                                                            
         LA    RE,CBUDLEV                                                       
         CLI   0(RE),0             ANY CONTRA RULES                             
         BE    BC330                                                            
BC322    CLI   0(RE),C'+'          DO DIFFERENT CLEAR IF FUNNY C/A              
         BE    BC326                                                            
         CLC   CONTRAC+1(2),0(RE)  MATCH THIS CONTRA WITH RULES                 
         BE    BC324               MATCH FOUND-CARRY ON                         
         LA    RE,3(RE)                                                         
         CLI   0(RE),0                                                          
         BNE   BC322                                                            
         MVC   LINE1,=9PL6'0'      CLEAR AND EXIT IF NO MATCH                   
         B     XIT                                                              
                                                                                
BC324    CLI   2(RE),12            FULL CONTRA WANTED                           
         BE    BC330               YES - SKIP CLEARING CODE                     
         MVC   CONTRNM,SPACES                                                   
         MVC   WORK(12),CONTRAC+3  CLEAR PART OF CONTRA KEY                     
         MVC   CONTRAC+3(12),SPACES                                             
         ZIC   R1,2(RE)                                                         
         LTR   R1,R1               LEDGER LEVEL ONLY                            
         BZ    BC330                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CONTRAC+3(0),WORK                                                
         B     BC330                                                            
                                                                                
BC326    MVC   FULL,0(RE)                                                       
         MVC   WORK(15),SPACES     CLEAR PART OF CONTRA KEY                     
         MVC   CONTRNM,SPACES      DEPENDING ON FUNNY RULES                     
         ZIC   RF,FULL+1           RULE IS OF FORM '+N(M)'                      
         LA    RF,CONTRAC(RF)                                                   
         ZIC   RE,FULL+1                                                        
         LA    R1,15                                                            
         SR    R1,RE                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),0(RF)     MOVE FROM +N TO WORK                         
         ZIC   RF,FULL+2           NOW CLEAR FROM N+M                           
         LA    RF,WORK+1(RF)                                                    
         MVC   0(15,RF),SPACES                                                  
         MVC   CONTRAC,WORK                                                     
         B     BC330                                                            
                                                                                
BC330    CLI   QOPT1,C'C'                                                       
         BE    BC340                                                            
         LA    RE,LINE1                                                         
         LA    RF,LINE2                                                         
         LA    R1,9                                                             
BC332    AP    0(6,RF),0(6,RE)     ADD LINE 1 TO LINE 2                         
         ZAP   0(6,RE),=P'0'                                                    
         LA    RF,6(RF)                                                         
         LA    RE,6(RE)                                                         
         BCT   R1,BC332                                                         
         B     XIT                                                              
                                                                                
BC340    CLC   LINE1,=9PL6'0'                                                   
         BE    XIT                                                              
         MVC   BUFKEY,CONTRAC      PUT THIS CONTRA INTO BUFFALO                 
         LA    R3,LINE1                                                         
         LA    RE,BUFACCS                                                       
         LA    R1,9                                                             
BC399    ZAP   0(8,RE),0(6,R3)                                                  
         ZAP   0(6,R3),=P'0'                                                    
         LA    R3,6(R3)                                                         
         LA    RE,8(RE)                                                         
         BCT   R1,BC399                                                         
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFKEY                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
BC400    CLI   MODE,ACCLAST                                                     
         BNE   BC500                                                            
         CLI   QOPT1,C'C'                                                       
         BE    BC401                                                            
         CLC   LINE2,=9PL6'0'                                                   
         BE    XIT                 EXIT IF NO ACTIVITY                          
         B     BC450                                                            
BC401    MVC   SVKEY(L'KEY),KEY                                                 
         L     R5,ADBUDC                                                        
         USING BUDACCD,R5                                                       
         CLC   BUAMTTBL,BUAMTEND   ANY ENTRIES                                  
         BE    BC440                                                            
         L     R2,ADCATABL                                                      
         USING BUAMTBLD,R2                                                      
                                                                                
*        HANDLE TABLE FOR CONTRAS WITH BUDGETS                                  
                                                                                
BC402    CLI   BUANUM,X'FF'                                                     
         BE    BC430                                                            
         LA    R4,MASK                                                          
         LA    RE,LINE1                                                         
         LA    R1,9                                                             
BC404    ZIC   RF,BUANOMTH                                                      
         LA    R3,BUAMNTHS                                                      
         USING BUAMNTHD,R3                                                      
BC404A   CLI   0(R4),250           ACTUAL COLUMN                                
         BL    BC405                                                            
         CLC   RIGHTBUD,BUANUM+1                                                
         BNE   BC412                                                            
         TM    7(R4),X'40'         PROJECTION REQUIRED                          
         BZ    BC412                                                            
         CLC   BUASYYMM,3(R4)      IF YES ADD BUDGET FOR                        
         BNH   BC410               REST OF YEAR TO ACTUAL                       
         CLC   BUASYYMM,5(R4)      FOR FIRST PART OF YEAR                       
         BH    BC410                                                            
         OC    BUAMBUD,BUAMBUD                                                  
         BZ    BC410                                                            
         AP    0(6,RE),BUAMBUD                                                  
         B     BC410                                                            
                                                                                
BC405    CLC   BUANUM+1(1),0(R4)                                                
         BNE   BC412                                                            
BC406    CLC   BUASYYMM,1(R4)                                                   
         BL    BC410                                                            
         CLC   BUASYYMM,3(R4)                                                   
         BH    BC410                                                            
         OC    BUAMBUD,BUAMBUD                                                  
         BZ    BC410                                                            
         AP    0(6,RE),BUAMBUD                                                  
BC410    LA    R3,BUAMLN2Q(R3)                                                  
         BCT   RF,BC404A                                                        
                                                                                
BC412    LA    R4,8(R4)                                                         
         LA    RE,6(RE)                                                         
         BCT   R1,BC404                                                         
                                                                                
         MVC   BUFKEY,BUACON       ADD TO BUFFALO                               
         MVI   BUANUM,X'FF'                                                     
         MVC   CONTRAC,BUFKEY                                                   
         CLI   LISTRUL,C'Y'                                                     
         BNE   BC426                                                            
         LA    RE,CBUDLEV                                                       
         CLI   0(RE),0             ANY CONTRA RULES                             
         BE    BC426                                                            
BC414    CLC   CONTRAC+1(2),0(RE)  MATCH THIS CONTRA WITH RULES                 
         BE    BC416               MATCH FOUND-CARRY ON                         
         CLI   0(RE),C'+'          SKIP IF A FUNNY ONE                          
         BE    BC420                                                            
         LA    RE,3(RE)                                                         
         CLI   0(RE),0                                                          
         BNE   BC414                                                            
         MVC   LINE1,=9PL6'0'      CLEAR AND EXIT IF NO MATCH                   
         B     BC430                                                            
                                                                                
BC416    CLI   2(RE),12            FULL CONTRA WANTED                           
         BE    BC426               YES                                          
         MVC   CONTRNM,SPACES                                                   
         MVC   WORK(12),CONTRAC+3  CLEAR PART OF CONTRA KEY                     
         MVC   CONTRAC+3(12),SPACES                                             
         ZIC   R1,2(RE)                                                         
         LTR   R1,R1               LEDGER LEVEL ONLY                            
         BZ    BC426                                                            
BC418    BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CONTRAC+3(0),WORK                                                
         MVC   BUFKEY,CONTRAC                                                   
         B     BC426                                                            
                                                                                
BC420    MVC   FULL,0(RE)                                                       
         MVC   WORK(15),SPACES     CLEAR PART OF CONTRA KEY                     
         MVC   CONTRNM,SPACES      DEPENDING ON FUNNY RULES                     
         ZIC   RF,FULL+1           RULE IS OF FORM '+N(M)'                      
         LA    RF,CONTRAC(RF)                                                   
         ZIC   RE,FULL+1                                                        
         LA    R1,15                                                            
         SR    R1,RE                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),0(RF)     MOVE FROM +N TO WORK                         
         ZIC   RF,FULL+2           NOW CLEAR FROM N+M                           
         LA    RF,WORK+1(RF)                                                    
         MVC   0(15,RF),SPACES                                                  
         MVC   CONTRAC,WORK                                                     
         MVC   BUFKEY,CONTRAC                                                   
         B     BC426                                                            
                                                                                
BC426    LA    R3,LINE1                                                         
         LA    RE,BUFACCS                                                       
         LA    R1,9                                                             
BC428    ZAP   0(8,RE),0(6,R3)                                                  
         ZAP   0(6,R3),=P'0'                                                    
         LA    R3,6(R3)                                                         
         LA    RE,8(RE)                                                         
         BCT   R1,BC428                                                         
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFKEY                               
                                                                                
BC430    ZIC   R1,BUANOMTH                                                      
         SR    R3,R3                                                            
         LA    R3,BUAMLN2Q                                                      
         STH   R3,HALF                                                          
         MH    R1,HALF                                                          
         LA    R1,BUAMLN1Q(R1)                                                  
         AR    R2,R1                                                            
         C     R2,TBND                                                          
         BL    BC402                                                            
                                                                                
BC440    CLI   LISTRUL,C'Y'        OBEYING TYPE RECORD RULES                    
         BNE   BC442                                                            
         CLC   ABUDLEV,LEDGLEV     IF YES - TEST IF WE WANT TO PRINT            
         BL    BC450                                                            
BC442    L     R2,ADACCNAM         PULL OUT RIGHT NAME FOR HEADS                
         BAS   RE,NAMOUT                                                        
         MVC   CWORK,WORK                                                       
         LA    R3,LINE1                                                         
         MVC   SVKEY(L'KEY),KEY                                                 
         BAS   RE,BUFOUT                                                        
         BAS   RE,SVREAD                                                        
                                                                                
BC450    L     R2,ADACCNAM                                                      
         BAS   RE,NAMOUT                                                        
         L     R4,ADACC                                                         
         BAS   RE,LHFORM                                                        
         LA    R3,LINE2                                                         
         BAS   RE,FORMAT                                                        
         MVI   BYTE,C'Y'                                                        
         BAS   RE,ANYPRT                                                        
         CLI   BYTE,C'Y'                                                        
         BE    BC451                                                            
         BAS   RE,CLEARP3                                                       
         B     XIT                                                              
BC451    CLI   REQLEV,0                                                         
         BE    BC452                                                            
         CLI   REQLEV,2            REQUEST OPTION SKIPS LOW LEVEL LINE          
         BNH   BC452                                                            
         BAS   RE,CLEARP3                                                       
         B     XIT                                                              
BC452    DS    0H                                                               
         CLI   LISTRUL,C'Y'        OBEYING TYPE RULES                           
         BNE   BC454                                                            
         CLC   ABUDLEV,LEDGLEV     AT BOTTOM LEVEL  SO TEST THAT                
         BNL   BC454               WE WANT TO PRINT AT THIS LEVEL               
         BAS   RE,CLEARP3                                                       
         B     XIT                                                              
BC454    BAS   RE,MYREPORT                                                      
         BAS   RE,MYREPORT                                                      
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
BC500    CLI   MODE,LEVCLAST                                                    
         BNE   BC600                                                            
         LA    R3,LINE3            LEV C TOTAL IS ON LINE 3                     
         CLI   QOPT1,C'C'                                                       
         BNE   BC504                                                            
         CLI   LISTRUL,C'Y'                                                     
         BNE   BC504                                                            
         CLI   ABUDLEV,3                                                        
         BL    BC504                                                            
         L     R2,ADLVCNAM                                                      
         BAS   RE,NAMOUT                                                        
         MVC   CWORK,WORK                                                       
         MVC   SVKEY(L'KEY),KEY                                                 
         BAS   RE,BUFOUT           PRINT CONTRA LINES IF AT RIGHT LEVEL         
         BAS   RE,SVREAD                                                        
BC504    L     R2,ADLVCNAM                                                      
         BAS   RE,NAMOUT                                                        
         L     R4,ADHEIRC                                                       
         BAS   RE,LHFORM                                                        
         BAS   RE,FORMAT                                                        
         MVI   BYTE,C'Y'                                                        
         BAS   RE,ANYPRT                                                        
         CLI   BYTE,C'Y'                                                        
         BE    BC510                                                            
         BAS   RE,CLEARP3                                                       
         B     XIT                                                              
BC510    CLI   REQLEV,0                                                         
         BE    BC512                                                            
         CLI   REQLEV,3                                                         
         BNH   BC512                                                            
         BAS   RE,CLEARP3                                                       
         B     XIT                                                              
BC512    CLI   LISTRUL,C'Y'                                                     
         BNE   BC514                                                            
         CLI   ABUDLEV,3           BUDGET LEVEL MUST BE 3 OR 4                  
         BNL   BC514               TO PRINT AT LEVEL C                          
         BAS   RE,CLEARP3                                                       
         B     XIT                                                              
BC514    BAS   RE,MYREPORT                                                      
         BAS   RE,MYREPORT                                                      
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
BC600    CLI   MODE,LEVBLAST                                                    
         BNE   BC700                                                            
         LA    R3,LINE3            LEV B TOTAL ON LINE 3 FOR 3-DEEP             
         CLI   LEDGLEV,3           LEDGER OR LINE 4 FOR 4-DEEP LEDGER           
         BE    *+8                                                              
         LA    R3,LINE4                                                         
         CLI   QOPT1,C'C'          HANDLE CONTRA BUDGETS AT HIGH                
         BNE   BC604               LEVEL OF ACCOUNT                             
         CLI   LISTRUL,C'Y'                                                     
         BNE   BC604                                                            
         CLI   ABUDLEV,1                                                        
         BE    BC604                                                            
         L     R2,ADLVBNAM                                                      
         BAS   RE,NAMOUT                                                        
         MVC   CWORK,WORK                                                       
         MVC   SVKEY(L'KEY),KEY                                                 
         BAS   RE,BUFOUT                                                        
         BAS   RE,SVREAD                                                        
BC604    L     R2,ADLVBNAM                                                      
         BAS   RE,NAMOUT                                                        
         L     R4,ADHEIRB                                                       
         BAS   RE,LHFORM                                                        
         BAS   RE,FORMAT                                                        
         MVI   BYTE,C'Y'                                                        
         BAS   RE,ANYPRT                                                        
         CLI   BYTE,C'Y'                                                        
         BE    BC610                                                            
         BAS   RE,CLEARP3                                                       
         B     XIT                                                              
BC610    CLI   REQLEV,0                                                         
         BE    BC612                                                            
         CLI   REQLEV,4                                                         
         BNH   BC612                                                            
         BAS   RE,CLEARP3                                                       
         B     XIT                                                              
BC612    CLI   LISTRUL,C'Y'                                                     
         BNE   BC614                                                            
         CLI   ABUDLEV,1           BUDGET LEVEL CAN BE ANY VALUE                
         BNE   BC614               BUT 1 FOR LEVEL B PRINT                      
         BAS   RE,CLEARP3                                                       
         B     XIT                                                              
BC614    BAS   RE,MYREPORT                                                      
         BAS   RE,MYREPORT                                                      
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
BC700    CLI   MODE,LEVALAST                                                    
         BNE   BC800                                                            
         ZIC   RF,LEDGLEV                                                       
         MH    RF,=H'54'           POINT TO LEV A LINE                          
         LA    R3,LINE1(RF)                                                     
         CLI   QOPT1,C'C'                                                       
         BNE   BC704                                                            
         CLI   LISTRUL,C'Y'        IF WE ARE OBEYING TYPE RULES                 
         BNE   BC704                                                            
         CLI   ABUDLEV,1           AND ACCT RULES ARE AT TOP LEVEL              
         BNE   BC704                                                            
         MVC   SVKEY(L'KEY),KEY                                                 
         L     R2,ADLVANAM                                                      
         BAS   RE,NAMOUT                                                        
         MVC   CWORK,WORK                                                       
         BAS   RE,BUFOUT           THEN PRINT WHAT IS IN BUFFALO                
         BAS   RE,SVREAD                                                        
BC704    L     R2,ADLVANAM                                                      
         BAS   RE,NAMOUT                                                        
         L     R4,ADHEIRA                                                       
         BAS   RE,LHFORM                                                        
         BAS   RE,FORMAT                                                        
         MVI   BYTE,C'Y'                                                        
         BAS   RE,ANYPRT                                                        
         CLI   BYTE,C'Y'                                                        
         BE    BC703                                                            
         BAS   RE,CLEARP3                                                       
         B     XIT                                                              
BC703    CLI   PROGPROF+1,C'Y'     SKIP PAGE AT HIGH LEVEL TOTALS               
         BNE   BC703A                                                           
         MVC   HEAD5+85(47),SPACES                                              
         MVC   HEAD6+85(47),SPACES                                              
         MVI   FORCEHED,C'Y'                                                    
BC703A   BAS   RE,MYREPORT                                                      
         BAS   RE,MYREPORT                                                      
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
BC800    CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         ZIC   RF,LEDGLEV                                                       
         LA    RF,1(RF)                                                         
         MH    RF,=H'54'           POINT TO REQUEST LINE                        
         LA    R3,LINE1(RF)                                                     
         BAS   RE,MYREPORT                                                      
         MVC   P+1(19),=C'*TOTAL FOR REQUEST*'                                  
         BAS   RE,FORMAT                                                        
         BAS   RE,MYREPORT                                                      
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PRINT BUFFALO/CONTRA DATA AT ANY LEVEL                                 
*        LINE ADDRESS AT R3                                                     
*-------------------------------------------------------------------*           
BUFOUT   NTR1                                                                   
         MVC   BUFKEY,SPACES                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFKEY,1                            
BF2      TM    DMCB+8,X'80'                                                     
         BO    BF6                                                              
         MVC   SAVEMODE,MODE                                                    
         LA    R4,BUFKEY-2                                                      
         BAS   RE,NAMEFND                                                       
         MVI   MODE,SBACLAST       FOOL FORMAT ROUTINE                          
         BAS   RE,LHFORM                                                        
         MVC   MODE,SAVEMODE                                                    
         LA    R1,9                                                             
         LA    RF,BUFACCS                                                       
         LR    RE,R3                                                            
BF4      ZAP   0(6,RE),0(8,RF)                                                  
         LA    RE,6(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R1,BF4                                                           
         BAS   RE,FORMAT                                                        
         BAS   RE,MYREPORT                                                      
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFKEY,1                             
         B     BF2                                                              
                                                                                
BF6      DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',ADBUFC                                    
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        READ BUDGETS AT HIGHER LEVELS                                          
*-------------------------------------------------------------------*           
GENBUD   NTR1                                                                   
         MVC   SVKEY(L'KEY),KEY                                                 
         L     R5,ADBUDC                                                        
         USING BUDACCD,R5                                                       
         MVI   BUTYPE,READING                                                   
         MVC   BUSERKEY,SPACES                                                  
         MVC   BUSERKEY(15),0(R1)  KEY OF HIGH LEVEL RECORD                     
         XC    BULSTKEY,BULSTKEY                                                
         ZIC   R2,CHNKCOLS                                                      
         LA    R3,QSRTAREA                                                      
GB02     CLI   0(R3),0                                                          
         BE    GB30                                                             
         CLI   0(R3),249                                                        
         BH    GB22                                                             
         MVC   BUBUDNO+1(1),0(R3)  TYPE CODE FROM REQUEST                       
         MVI   BUCMND,0                                                         
         XC    BULSTKEY,BULSTKEY                                                
GB04     GOTO1 BUDACC,DMCB,ADBUDC                                               
         CLI   BUMODE,FINISHED                                                  
         BE    GB22                                                             
         CLI   BUMODE,DMGRERR                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BUSERKEY(15),BULSTKEY+1                                          
         BNE   GB22                                                             
         CLI   BUMODE,PROCAMNT                                                  
         BNE   GB04                                                             
         LA    R4,BUAMTREC                                                      
         AH    R4,DATADISP                                                      
GB10     CLI   0(R4),0             SCAN FOR AMOUNT ELEMENTS                     
         BE    GB04                                                             
         CLI   0(R4),X'1D'                                                      
         BE    GB14                                                             
GB12     ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     GB10                                                             
                                                                                
         USING ACBAD,R4                                                         
GB14     L     RE,LNADD            LINE ADDRESS                                 
         LA    R1,MASK                                                          
         LA    RF,9                                                             
GB16     DS    0H                                                               
         CLI   0(R1),250           ACTUAL COLUMN                                
         BL    GB17                                                             
         CLC   RIGHTBUD,0(R3)                                                   
         BNE   GB18                                                             
         TM    7(R1),X'40'         PROJECTION OPTION                            
         BZ    GB18                                                             
         CLC   ACBAMNTH,3(R1)                                                   
         BNH   GB18                                                             
         CLC   ACBAMNTH,5(R1)                                                   
         BH    GB18                                                             
         AP    0(6,RE),ACBABUDG                                                 
         B     GB18                                                             
GB17     DS    0H                                                               
         CLC   0(1,R1),0(R3)       ADD TO CORRECT PLACE IN TABLE                
         BNE   GB18                                                             
         CLC   ACBAMNTH,1(R1)                                                   
         BL    GB18                                                             
         CLC   ACBAMNTH,3(R1)                                                   
         BH    GB18                                                             
         AP    0(6,RE),ACBABUDG                                                 
GB18     LA    R1,8(R1)                                                         
         LA    RE,6(RE)                                                         
         BCT   RF,GB16                                                          
         B     GB12                                                             
                                                                                
GB22     LA    R3,1(R3)                                                         
         BCT   R2,GB02                                                          
                                                                                
GB30     BAS   RE,SVREAD                                                        
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        READ CONTRA BUDGETS AT ALL LEVELS                                      
*-------------------------------------------------------------------*           
GENCBUD  NTR1                                                                   
         MVC   CKEY,0(R1)                                                       
         L     R5,ADBUDC                                                        
         USING BUDACCD,R5                                                       
         MVC   BUSERKEY,SPACES                                                  
         MVC   BUSERKEY(15),0(R1)                                               
         XC    BULSTKEY,BULSTKEY                                                
         MVI   BUTYPE,TABLER       FOR CONTRA ACCOUNT OPTION                    
         MVC   BUAMTTBL,ADCATABL   BUILD TABLE OF C/AS AND BUDGETS              
         L     RF,ADTYPTAB                                                      
         ST    RF,BUATYTBL                                                      
         MVI   BUTBLPER,1          EACH ENTRY IS 1 MONTH WIDE                   
         MVI   BUBUDNO+1,0                                                      
         L     RF,=F'15000'                                                     
         ST    RF,BUTYTBLN         LENGTH OF TYPE TABLE                         
         L     RF,=F'150000'                                                    
         ST    RF,BUAMTBLN         LENGTH OF AMOUNT TABLE                       
         MVC   BUSTDATE,STYR       FILTER ON MAX POSSIBLE DATE RANGE            
         MVC   BUNDDATE,ENYR                                                    
         CLC   ENYTD,ENYR                                                       
         BNH   *+10                                                             
         MVC   BUNDDATE,ENYTD      DEAL WITH FUNNY REQUEST DATES                
         GOTO1 BUDACC,DMCB,ADBUDC                                               
         CLI   BUMODE,TABLEFUL                                                  
         BNE   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         L     R1,BUAMTEND                                                      
         BCTR  R1,0                                                             
         ST    R1,TBND             END OF AMOUNT TABLE                          
         BAS   RE,SVREAD                                                        
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
MYREPORT NTR1                                                                   
         L     RF,ADACC                                                         
         MVC   HEAD6+10(1),2(RF)                                                
         L     R2,ADUNTNAM                                                      
         BAS   RE,NAMOUT                                                        
         MVC   HEAD5+12(36),WORK                                                
         L     R2,ADLDGNAM                                                      
         BAS   RE,NAMOUT                                                        
         MVC   HEAD6+12(36),WORK                                                
         LA    RF,HEAD6+85                                                      
         CLI   QOPT1,C'C'                                                       
         BNE   *+8                                                              
         LA    RF,HEAD8+85                                                      
         CLI   QOPT6,C'Y'                                                       
         BNE   *+10                                                             
         MVC   0(27,RF),=C'YEARLY ACTUAL IS PROJECTION'                         
         MVC   HEAD9,SVH1                                                       
         MVC   HEAD10,SVH2                                                      
         MVC   HEAD11,SVH3                                                      
         CLI   QOPT1,C'C'                                                       
         BNE   MYRPT2                                                           
         GOTO1 CHOPPER,DMCB,(36,CWORK),(24,HEAD6+85),(C'P',2)                   
         MVC   HEAD5+85(7),=C'ACCOUNT'                                          
         MVC   HEAD5+93(12),CKEY+3                                              
MYRPT2   GOTO1 ACREPORT                                                         
         B     XIT                                                              
                                                                                
NAMOUT   NTR1                                                                   
         USING ACNAMED,R2                                                       
         MVC   WORK(36),SPACES                                                  
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   WORK(0),ACNMNAME                                                 
                                                                                
LHFORM   NTR1                                                                   
         MVC   P+1(12),3(R4)       ACCOUNT CODE                                 
*&&US                                                                           
         CLI   PROGPROF+2,C'Y'                                                  
         BNE   *+10                                                             
         MVC   P+1(2),SPACES                                                    
*&&                                                                             
         CLI   QOPT1,C'C'                                                       
         BNE   LHF2                                                             
         CLI   MODE,SBACLAST                                                    
         BE    LHF6                                                             
         B     LHF4                                                             
LHF2     CLI   MODE,ACCLAST                                                     
         BE    LHF6                                                             
LHF4     MVI   P+1,C'*'            INDICATE HIGHER LEVEL                        
         MVC   P+2(12),3(R4)                                                    
LHF6     ZIC   RF,ACCWDTH                                                       
         SH    RF,=H'14'                                                        
         GOTO1 CHOPPER,DMCB,(36,WORK),((RF),P+14),(C'P',3)                      
         B     XIT                                                              
                                                                                
ANYPRT   NTR1                      TEST IF ANY DATA TO PRINT                    
         ZIC   RE,ACCWDTH                                                       
         ZIC   R1,DATAWDTH                                                      
         LA    RF,P+1(RE)                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),SPACES                                                   
         BNE   XIT                                                              
         MVI   BYTE,C'N'                                                        
         B     XIT                                                              
                                                                                
CLEARP3  MVC   P,SPACES            CLEAR THE 3 PRINT LINES                      
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         BR    RE                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
NAMEFND  NTR1                                                                   
         MVC   WORK(36),SPACES                                                  
         L     R2,=A(IOA)                                                       
         MVC   0(42,R2),SPACES                                                  
         MVC   0(15,R2),BUFKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R2),(R2),(0,0)              
         TM    DMCB+8,X'10'                                                     
         BO    XIT                                                              
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         AH    R2,DATADISP                                                      
         SR    R1,R1                                                            
NMF2     CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLI   0(R2),X'20'                                                      
         BE    NMF4                                                             
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     NMF2                                                             
NMF4     BAS   RE,NAMOUT                                                        
         B     XIT                                                              
                                                                                
SVREAD   NTR1                      READ FOR REPOSITIONING                       
         L     RF,=A(IOA)                                                       
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',SVKEY,(RF),(0,0)             
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
FORMAT   NTR1                      FORMAT A TABLE LINE                          
         LA    RF,9                                                             
         ZIC   R1,COLSTRT                                                       
         LA    R2,P+1(R1)                                                       
         ZAP   LASTBUDG,=P'0'                                                   
         LA    RE,MASK                                                          
         LA    R5,54(R3)                                                        
FMT2     CLI   0(RE),250                                                        
         BH    FMT4                                                             
         BE    *+10                                                             
         ZAP   LASTBUDG,0(6,R3)                                                 
         CP    0(6,R3),=P'0'                                                    
         BE    FMT10                                                            
         ZAP   DIV,0(6,R3)                                                      
         AP    DIV,=P'50'                                                       
         CP    DIV,=P'0'                                                        
         BH    *+10                                                             
         SP    DIV,=P'100'                                                      
         DP    DIV,=P'100'                                                      
         EDIT  (P12,DIV),(10,0(R2)),MINUS=YES                                   
         B     FMT10                                                            
                                                                                
FMT4     LR    R4,R2                                                            
         BCTR  R4,0                                                             
         CLI   0(RE),251           VARIANCE                                     
         BE    FMT6                                                             
         CLI   0(RE),254           BALANCE                                      
         BE    FMT5                                                             
         CP    LASTBUDG,=P'0'                                                   
         BE    FMT10                                                            
         CLI   0(RE),252           PERCENT                                      
         BE    FMT8                                                             
*                                  MUST BE INDEX                                
         CP    0(6,R3),=P'0'                                                    
         BE    FMT10                                                            
         ZAP   DIV,0(6,R3)         INDEX    = ACT X 100  - 100                  
         MP    DIV,=P'10000'                  ---------                         
         DP    DIV,LASTBUDG                     BUD                             
         SP    DIV(8),=P'10000'                                                 
         EDIT  (P8,DIV),(10,0(R4)),2,FLOAT=-                                    
         B     FMT10                                                            
                                                                                
FMT5     DS    0H                  BALANCE = BUDGET - ACTUAL                    
         ZAP   DIV,LASTBUDG                                                     
         SP    DIV,0(6,R3)                                                      
         B     FMT7                                                             
                                                                                
FMT6     DS    0H                                                               
         ZAP   DIV,0(6,R3)                                                      
         SP    DIV,LASTBUDG        VARIANCE = ACTUAL - BUDGET                   
FMT7     DS    0H                                                               
         AP    DIV,=P'50'                                                       
         CP    DIV,=P'0'                                                        
         BH    *+10                                                             
         SP    DIV,=P'100'                                                      
         DP    DIV,=P'100'                                                      
         EDIT  (P12,DIV),(10,0(R4)),FLOAT=-                                     
         B     FMT10                                                            
                                                                                
FMT8     CP    LASTBUDG,=P'0'                                                   
         BE    FMT10                                                            
         ZAP   DIV,0(6,R3)                                                      
         CP    DIV,=P'0'           PERCENT                                      
         BE    FMT10                                                            
         MP    DIV,=P'10000'                                                    
         DP    DIV,LASTBUDG                                                     
         EDIT  (P8,DIV),(10,0(R4)),2                                            
         B     FMT10                                                            
                                                                                
FMT10    CLI   MODE,REQLAST                                                     
         BE    FMT14                                                            
         AP    0(6,R5),0(6,R3)     ADD TO NEXT LINE                             
FMT14    ZAP   0(6,R3),=P'0'                                                    
         LA    R5,6(R5)                                                         
         LA    R2,11(R2)                                                        
         LA    R3,6(R3)                                                         
         LA    RE,8(RE)                                                         
         BCT   RF,FMT2                                                          
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
LINEADD  NTR1                      ADD 1 LINE TO THE NEXT                       
         LA    RF,9                                                             
LADD2    AP    54(6,R3),0(6,R3)                                                 
         LA    R3,6(R3)                                                         
         BCT   RF,LADD2                                                         
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CONSTANTS , TABLES ETC.                                                
*-------------------------------------------------------------------*           
BUDACC   DC    V(BUDACC)                                                        
CENTER   DC    V(CENTER)                                                        
ADBUDC   DC    A(BUDC)                                                          
ADCATABL DC    A(CATABL)                                                        
ADTYPTAB DC    A(TYPTAB)                                                        
ADBUFC   DC    A(BUFFALOC)                                                      
APRODTAB DC    A(PRODTAB)                                                       
PAGEWDTH DC    AL1(132)                                                         
FIXED    DC    AL1(250),C'    ACTUAL'                                           
         DC    AL1(251),C'  VARIANCE'                                           
         DC    AL1(252),C'   PERCENT'                                           
         DC    AL1(253),C'     INDEX'                                           
         DC    AL1(254),C'   BALANCE'                                           
         DC    X'FF'                                                            
                                                                                
REQLVTAB DC    C'C',X'01'                                                       
         DC    C'A',X'02'                                                       
         DC    C'3',X'03'                                                       
         DC    C'2',X'04'                                                       
         DC    C'1',X'05'                                                       
         DC    X'FF'                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        LITERAL POOL                                                           
*-------------------------------------------------------------------*           
         LTORG                                                                  
         EJECT                                                                  
BUDC     DS    0D                                                               
         DC    1300X'00'                                                        
                                                                                
IOA      DS     D                                                               
         DS    1000C                                                            
                                                                                
CATABL   DS    0D                                                               
         DS    150000C                                                          
                                                                                
TYPTAB   DS    0D                                                               
         DS    15000C                                                           
                                                                                
         BUFF  LINES=100,ROWS=1,COLUMNS=9,FLAVOR=PACKED,KEYLIST=(15,A)          
         EJECT                                                                  
                                                                                
PRODTAB  DS    0CL40                                                            
         DC    C'01CC',CL36'COCA-COLA'                                          
         DC    C'01CF',CL36' '            (CAFFINE FREE COKE)                   
         DC    C'02FR',CL36'FRESCA'                                             
         DC    C'03TB',CL36'TAB'                                                
         DC    C'03CT',CL36' '            (CAFFINE FREE TAB)                    
         DC    C'04SP',CL36'SPRITE'                                             
         DC    C'04DS',CL36' '            (DIET SPRITE)                         
         DC    C'05PB',CL36'MR. PIBB'                                           
         DC    C'06MY',CL36'MELLO YELLO'                                        
         DC    C'07FN',CL36'FANTA'                                              
         DC    C'09RB',CL36'RAMBLIN ROOT BEER'                                  
         DC    C'12DC',CL36'DIET COKE'                                          
         DC    C'12CD',CL36' '            (CAFFINE FREE DIET COKE)              
         DC    C'13CY',CL36'CHERRY COKE'                                        
NPRODS   EQU   (*-PRODTAB)/L'PRODTAB                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        DSECT FOR LOCAL W/S                                                    
*-------------------------------------------------------------------*           
PROGD    DSECT                                                                  
TBND     DS    F                                                                
LNADD    DS    F                                                                
DATES    DS    0CL12                                                            
STCUR    DS    CL2                                                              
ENCUR    DS    CL2                                                              
STYTD    DS    CL2                                                              
ENYTD    DS    CL2                                                              
STYR     DS    CL2                                                              
ENYR     DS    CL2                                                              
CHNKWDTH DS    CL1                                                              
CHNKCOLS DS    CL1                 NO OF COLUMNS PER CHUNK                      
CHNKNO   DS    CL1                                                              
ACCWDTH  DS    CL1                                                              
COLSTRT  DS    CL1                                                              
REQLEV   DS    CL1                                                              
THISLEV  DS    CL1                                                              
LEDGLEV  DS    CL1                                                              
RIGHTBUD DS    CL1                                                              
DATAWDTH DS    CL1                                                              
LISTRUL  DS    CL1                                                              
ABUDLEV  DS    CL1                                                              
SAVEMODE DS    CL1                                                              
MASK     DS    CL72                9 8-BYTE ENTRIES -1 COL TYPE                 
*                                                   -2 START DATE               
*                                                   -2 END DATE                 
*                                                   -2 EXTRA END                
*                                                   -1 STATUS                   
TABLE    DS    0CL324                                                           
LINE1    DS    CL54                                                             
LINE2    DS    CL54                                                             
LINE3    DS    CL54                                                             
LINE4    DS    CL54                                                             
LINE5    DS    CL54                                                             
LINE6    DS    CL54                                                             
SVH1     DS    CL132                                                            
SVH2     DS    CL132                                                            
SVH3     DS    CL132                                                            
COLHEAD1 DS    CL90                                                             
COLHEAD2 DS    CL90                                                             
SVKEY    DS    CL49                                                             
CONTRAC  DS    CL15                                                             
CONTRNM  DS    CL36                                                             
CWORK    DS    CL36                                                             
CKEY     DS    CL15                                                             
DIV      DS    PL14                                                             
LASTBUDG DS    PL6                                                              
BUFKEY   DS    CL15                                                             
BUFACCS  DS    9PL8                                                             
CBUDLEV  DS    CL255                                                            
         EJECT                                                                  
                                                                                
* ACGENBOTH                                                                     
* ACGENMODES                                                                    
* ACREPWORKD                                                                    
* ACBUDACCD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACBUDACCD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPB202 05/01/02'                                      
         END                                                                    
