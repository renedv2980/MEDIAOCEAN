*          DATA SET ACREPB302  AT LEVEL 020 AS OF 05/18/07                      
*PHASE ACB302A                                                                  
*INCLUDE BUDACC                                                                 
         TITLE 'MONTHLY BUDGET ANALYSIS'                                        
ACB302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACB3**,R9,R8,RR=R5                                           
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING PROGD,RC                                                         
         ST    R5,PRELOC                                                        
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   BC10                                                             
         L     RF,=A(TYPTBL)                                                    
         A     RF,PRELOC                                                        
         ST    RF,TYPTAB                                                        
         L     RF,=A(BUDC)                                                      
         A     RF,PRELOC                                                        
         ST    RF,ADBUDC                                                        
         L     RF,=V(BUDACC)                                                    
         A     RF,PRELOC                                                        
         ST    RF,BUDACC                                                        
         L     RF,=A(AMTTAB)                                                    
         A     RF,PRELOC                                                        
         ST    RF,ADCATABL                                                      
         L     RF,=A(PBLOC)                                                     
         A     RF,PRELOC                                                        
         ST    RF,ADPBLOC                                                       
         L     RF,=A(BUFFALOC)                                                  
         A     RF,PRELOC                                                        
         ST    RF,ADBUFC                                                        
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         B     XIT                                                              
         SPACE 2                                                                
XIT      XMOD1                                                                  
         EJECT                                                                  
BC10     CLI   MODE,REQFRST                                                     
         BNE   BC100                                                            
         MVC   LISTRUL,PROGPROF    PROFILE TO OBEY TYPE RECD RULES              
         MVC   SVKEY,SPACES                                                     
         MVC   SVKEY(L'KEY),KEY                                                 
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVC   QSTART+4(2),=C'15'                                               
         MVC   QEND+4(2),=C'15'                                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(1,STYR)                                  
         GOTO1 (RF),(R1),,(6,HFROM)                                             
         GOTO1 (RF),(R1),(0,QEND),(1,ENYR)                                      
         GOTO1 (RF),(R1),,(6,HTO)                                               
         MVC   BUDNUM,QSRTAREA                                                  
         L     R5,ADBUDC                                                        
         USING BUDACCD,R5                                                       
         MVC   BUADMGR,DATAMGR                                                  
         MVC   BUDTCON,DATCON                                                   
         MVI   BUTYPE,READING                                                   
         MVI   BUCMND,TYPEGET                                                   
         MVC   BUSERKEY(1),QCOMPANY                                             
         MVC   BUBUDNO+1(1),BUDNUM                                              
         GOTO1 BUDACC,DMCB,ADBUDC                                               
         CLI   BUMODE,PROCTYPE     GOOD READ                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,BUTYPREC         GET BUDGET SHORT NAME                        
         USING ACBTKEY,RF                                                       
         MVC   BUDNAME,ACBTKCOD                                                 
         BAS   RE,GETRUL           READ BUDGET CONTRA RULES                     
*                                                                               
         CLI   QOPT2,C' '                                                       
         BE    BC12                                                             
         MVC   COMPNAME,=CL10'VARIANCE'                                         
         CLI   QOPT2,C'V'                                                       
         BE    BC12                                                             
         MVC   COMPNAME,=CL10'INDEX'                                            
         CLI   QOPT2,C'I'                                                       
         BE    BC12                                                             
         MVC   COMPNAME,=CL10'PERCENT'                                          
         CLI   QOPT2,C'P'                                                       
         BE    BC12                                                             
         MVC   COMPNAME,=CL10'BALANCE'                                          
*                                                                               
BC12     LA    RF,TAB0             CLEAR TABLES                                 
         LA    RE,39*7             3LINES OF 13 - 7 DEEP                        
BC14     ZAP   0(6,RF),=P'0'                                                    
         LA    RF,6(RF)                                                         
         BCT   RE,BC14                                                          
*                                                                               
         BAS   RE,SVREAD                                                        
         MVC   SVH1,SPACES         BUILD MONTH HEADINGS AND MONTH               
         MVC   SVH2,SPACES         LIST FOR BUCKET CONTROL                      
         XC    MASK,MASK                                                        
         MVC   TEMP,SPACES                                                      
         MVC   WORK(6),QSTART                                                   
         LA    R2,MASK                                                          
         LA    R3,TEMP                                                          
         LA    R4,1                                                             
*                                                                               
BC20     GOTO1 DATCON,DMCB,(0,WORK),(6,0(R3))                                   
         PACK  DUB,WORK(5)                                                      
         MVC   0(2,R2),DUB+5                                                    
*MN      GOTO1 ADDAY,DMCB,WORK,WORK+6,30                                        
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,F'1'                               
         CLC   WORK+6(4),QEND                                                   
         BH    BC22                                                             
         MVC   WORK(6),WORK+6                                                   
         LA    R2,2(R2)                                                         
         LA    R3,6(R3)                                                         
         LA    R4,1(R4)                                                         
         B     BC20                                                             
*                                                                               
BC22     STC   R4,REQWDTH          NUMBER OF MONTHS IN REQUEST                  
         MVI   TWOLINE,C'N'                                                     
         CH    R4,=H'6'                                                         
         BNH   *+8                                                              
         MVI   TWOLINE,C'Y'                                                     
         LA    R2,2                                                             
         LA    RF,SVH1+1                                                        
         LA    R1,TEMP                                                          
BC24     LA    RE,6                                                             
BC26     MVC   0(6,RF),0(R1)                                                    
         LA    RF,11(RF)                                                        
         LA    R1,6(R1)                                                         
         BCT   RE,BC26                                                          
         LA    RF,SVH2+1                                                        
         BCT   R2,BC24                                                          
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
         ZAP   CTACC,=P'0'                                                      
         ZAP   CTLVC,=P'0'                                                      
         ZAP   CTLVB,=P'0'                                                      
         B     XIT                                                              
         EJECT                                                                  
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
         DROP  R4                                                               
         EJECT                                                                  
BC120    CLI   MODE,LEVAFRST                                                    
         BNE   BC130                                                            
         ZIC   RF,LEDGLEV          POINT TO LEV A TABLE                         
         MH    RF,=H'234'          39 X 6                                       
         LA    R3,TAB1(RF)                                                      
         ST    R3,TBAD                                                          
         L     R1,ADHEIRA                                                       
         BAS   RE,GENBUD                                                        
         B     XIT                                                              
         SPACE 2                                                                
BC130    CLI   MODE,LEVBFRST                                                    
         BNE   BC140                                                            
         LA    R3,TAB3                                                          
         CLI   LEDGLEV,3           LEV B TOTAL IN TAB 3 FOR 3-DEEP              
         BE    *+8                 LEDGER OR TAB 4 FOR 4-DEEP LEDGER            
         LA    R3,TAB4                                                          
         ST    R3,TBAD                                                          
         L     R1,ADHEIRB                                                       
         BAS   RE,GENBUD                                                        
         B     XIT                                                              
         SPACE 1                                                                
BC140    CLI   MODE,LEVCFRST                                                    
         BNE   BC200                                                            
         LA    R3,TAB3                                                          
         ST    R3,TBAD                                                          
         L     R1,ADHEIRC                                                       
         BAS   RE,GENBUD                                                        
         B     XIT                                                              
         EJECT                                                                  
BC200    CLI   MODE,PROCACC                                                     
         BNE   BC220                                                            
         MVI   CONTCHOP,C'N'                                                    
         MVC   SVCONTRA,SPACES                                                  
         MVC   SVKEY,SPACES                                                     
         MVC   SVKEY(L'KEY),KEY                                                 
         MVC   UL,KEY+1                                                         
         L     R5,ADBUDC                                                        
         USING BUDACCD,R5                                                       
         MVI   BUTYPE,TABLER       FOR CONTRA ACCOUNT OPTION                    
         MVC   BUAMTTBL,ADCATABL   BUILD TABLE OF C/AS AND BUDGETS              
         L     RF,TYPTAB                                                        
         ST    RF,BUATYTBL                                                      
         MVI   BUTBLPER,1          EACH ENTRY IS 1 MONTH WIDE                   
         MVC   BUBUDNO+1(1),BUDNUM                                              
         L     RF,=A(LTYPTAB)                                                   
         ST    RF,BUTYTBLN         LENGTH OF TYPE TABLE                         
         L     RF,=A(AMTLNQ)                                                    
         ST    RF,BUAMTBLN         LENGTH OF AMOUNT TABLE                       
         MVC   BUSTDATE,STYR       FILTER ON MAX POSSIBLE DATE RANGE            
         MVC   BUNDDATE,ENYR                                                    
         L     RF,ADACC                                                         
         MVC   BUSERKEY,SPACES                                                  
         MVC   BUSERKEY(15),0(RF)                                               
         XC    BULSTKEY,BULSTKEY                                                
         GOTO1 BUDACC,DMCB,ADBUDC                                               
         L     R1,BUAMTEND                                                      
         BCTR  R1,0                                                             
         ST    R1,TBND             END OF AMOUNT TABLE                          
         BAS   RE,SVREAD                                                        
         CLI   QOPT1,C'C'                                                       
         BNE   XIT                                                              
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
BC220    CLI   MODE,SBACFRST                                                    
         BNE   BC340                                                            
         L     RF,ADSUBAC                                                       
         USING TRSUBHD,RF                                                       
         MVI   CACTIV,C'N'                                                      
         MVC   CONTRAC,TRSBACNT                                                 
         MVC   CONTRNM,SPACES                                                   
         ZIC   R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CONTRNM(0),TRSBNAME                                              
         CLI   LISTRUL,C'Y'                                                     
         BNE   BC260                                                            
         LA    RE,CBUDLEV          ANY CONTRA RULES                             
         CLI   0(RE),0                                                          
         BE    BC260                                                            
BC222    CLC   CONTRAC+1(2),0(RE)                                               
         BE    BC224               GET CONTRA MATCH                             
         LA    RE,3(RE)                                                         
         CLI   0(RE),0                                                          
         BNE   BC222                                                            
         B     BC260                                                            
*                                                                               
BC224    CLI   2(RE),12            WANT ALL OF CONTRA                           
         BE    BC260                                                            
         MVI   CONTCHOP,C'Y'                                                    
         MVC   CONTRNM,SPACES      IF NOT CLEAR NAME AND PART OF KEY            
         MVC   WORK(12),CONTRAC+3                                               
         MVC   CONTRAC+3(12),SPACES                                             
         ZIC   R1,2(RE)                                                         
         LTR   R1,R1               LEDGER LEVEL ONLY                            
         BZ    BC260                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CONTRAC+3(0),WORK                                                
         B     BC260                                                            
*                                                                               
BC260    L     R2,ADCATABL         LOOK AT TABLE ENTRIES TO SEE IF              
         USING BUAMTBLD,R2         WE SHOULD PRINT OR IGNORE                    
         L     R5,ADBUDC                                                        
         USING BUDACCD,R5                                                       
*                                                                               
BC270    CLI   BUANUM,X'FF'        USED ALREADY                                 
         BE    BC274                                                            
         CLC   CONTRAC,BUACON                                                   
         BE    BC280               IF EQUAL TO C/A ADD AND EXIT                 
BC274    ZIC   R1,BUANOMTH                                                      
         SR    R3,R3                                                            
         LA    R3,BUAMLN2Q                                                      
         STH   R3,HALF                                                          
         MH    R1,HALF                                                          
         LA    R1,BUAMLN1Q(R1)                                                  
         AR    R2,R1                                                            
         C     R2,TBND             END OF TABLE                                 
         BL    BC270                                                            
         B     XIT                                                              
         SPACE 1                                                                
BC280    MVI   CACTIV,C'Y'                                                      
         BAS   RE,TABADD                                                        
         MVI   BUANUM,X'FF'        MARK AS USED                                 
         B     XIT                                                              
         EJECT                                                                  
BC340    CLI   MODE,PROCHIST       DO WE HAVE A HISTORY RECORD ?                
         BNE   BC360               NO, SEE IF SUB-ACCT LAST THEN                
         L     R3,ADTRANS          YES, GET ADDRESS OF THE RECORD               
         USING TRHISTD,R3                                                       
         CLI   TRHSEL,X'45'                                                     
         BNE   XIT                                                              
         CLI   BUCKTYPE,C' '                                                    
         BNE   XIT                                                              
         LA    R2,MASK                                                          
         LA    RE,TAB1+13*6        POINT TO ACTUAL LINE                         
         LA    RF,12                                                            
BC342    CLC   TRHSYEAR(2),0(R2)   BUCKET MONTH VS COLUMN MONTH                 
         BE    BC344                                                            
         LA    R2,2(R2)                                                         
         LA    RE,6(RE)                                                         
         BCT   RF,BC342                                                         
         B     XIT                                                              
*                                                                               
BC344    DS    0H                                                               
         CLC   UL,=C'28'           FUNNY 28 ETC STUFF                           
         BE    BC350                                                            
         CLC   UL,=C'29'                                                        
         BE    BC350                                                            
         CLC   UL,=C'SI'                                                        
         BE    BC350                                                            
         CLC   UL,=C'1C'                                                        
         BNE   BC346                                                            
         CLC   CONTRAC+1(2),=C'11'                                              
         BE    BC346                                                            
         CLC   CONTRAC+1(2),=C'12'                                              
         BNE   BC350                                                            
BC346    AP    0(6,RE),TRHSDR      ADD DEBITS                                   
         SP    0(6,RE),TRHSCR      SUBTRACT CREDITS                             
         AP    TAB1+25*6(6),TRHSDR TOTAL COUNTER FOR ACTUAL                     
         SP    TAB1+25*6(6),TRHSCR                                              
         MVI   CACTIV,C'Y'                                                      
         CLI   QOPT2,C' '          ANY NEED FOR THE COMPARISON LINE             
         BE    XIT                                                              
         AP    13*6(6,RE),TRHSDR                                                
         SP    13*6(6,RE),TRHSCR                                                
         AP    TAB1+38*6(6),TRHSDR TOTAL COUNTER FOR COMPARISON                 
         SP    TAB1+38*6(6),TRHSCR                                              
         B     XIT                                                              
*                                                                               
BC350    SP    0(6,RE),TRHSDR      SUBTRACT DEBITS                              
         AP    0(6,RE),TRHSCR      ADD CREDITS                                  
         SP    TAB1+25*6(6),TRHSDR                                              
         AP    TAB1+25*6(6),TRHSCR                                              
         MVI   CACTIV,C'Y'                                                      
         CLI   QOPT2,C'Y'                                                       
         BE    XIT                                                              
         SP    13*6(6,RE),TRHSDR                                                
         AP    13*6(6,RE),TRHSCR                                                
         SP    TAB1+38*6(6),TRHSDR                                              
         AP    TAB1+38*6(6),TRHSCR                                              
         B     XIT                                                              
         EJECT                                                                  
BC360    CLI   MODE,SBACLAST                                                    
         BNE   BC400                                                            
         CLI   CACTIV,C'Y'                                                      
         BNE   XIT                                                              
         CLI   LISTRUL,C'Y'                                                     
         BNE   BC370                                                            
         LA    RE,CBUDLEV                                                       
         CLI   0(RE),0             ANY CONTRA RULES                             
         BE    BC370                                                            
BC362    CLC   CONTRAC+1(2),0(RE)  MATCH THIS CONTRA WITH RULES                 
         BE    BC370                                                            
         LA    RE,3(RE)                                                         
         CLI   0(RE),0                                                          
         BNE   BC362                                                            
         MVC   TAB1,TAB0           CLEAR AND EXIT IF NO MATCH                   
         B     XIT                                                              
*                                                                               
BC370    CLI   QOPT1,C'C'                                                       
         BE    BC380                                                            
         LA    R3,TAB1                                                          
         BAS   RE,ADDUP            ADD TO ACCOUNT LEVEL                         
         B     XIT                                                              
BC380    DS    0H                                                               
         LA    R3,TAB1                                                          
*                                                                               
         LA    RE,13*3             ADD TAB1 TO BUFFALO FOR PRINTING             
         LA    RF,BUFACCS          AT ACCLAST                                   
         LA    R1,TAB1                                                          
BC382    ZAP   0(8,RF),0(6,R1)                                                  
         LA    RF,8(RF)                                                         
         LA    R1,6(R1)                                                         
         BCT   RE,BC382                                                         
         MVC   BUFKEY,CONTRAC                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFKEY                               
         BAS   RE,ADDUP            ADD TO ACCOUNT LEVEL                         
         B     XIT                                                              
         EJECT                                                                  
BC400    CLI   MODE,ACCLAST                                                     
         BNE   BC500                                                            
         MVC   SVKEY,SPACES                                                     
         MVC   SVKEY(L'KEY),KEY                                                 
         L     R2,ADCATABL         LOOK FOR ANY UNUSED TABLE ENTRIES            
         USING BUAMTBLD,R2                                                      
         L     R5,ADBUDC                                                        
         USING BUDACCD,R5                                                       
         CLI   QOPT1,C'C'                                                       
         BE    BC420                                                            
         CLC   BUAMTTBL,BUAMTEND   ANY ENTRIES                                  
         BE    BC450                                                            
*                                                                               
BC402    CLI   BUANUM,X'FF'                                                     
         BE    BC404                                                            
         BAS   RE,TABADD                                                        
         LA    R3,TAB1                                                          
         BAS   RE,ADDUP                                                         
BC404    ZIC   R1,BUANOMTH                                                      
         SR    R3,R3                                                            
         LA    R3,BUAMLN2Q                                                      
         STH   R3,HALF                                                          
         MH    R1,HALF                                                          
         LA    R1,BUAMLN1Q(R1)                                                  
         AR    R2,R1                                                            
         C     R2,TBND             END OF TABLE                                 
         BL    BC402                                                            
         B     BC450                                                            
*                                                                               
BC420    CLC   BUAMTTBL,BUAMTEND   ANY ENTRIES                                  
         BE    BC426                                                            
         CLI   BUANUM,X'FF'                                                     
         BE    BC424               FOR CONTRA OPTION ADD ALL UNUSED             
         MVC   CONTRAC,BUACON      CONTRA ENTRIES TO BUFFALO                    
         MVC   LHBLOCK,BUACON      THEN PRINT THE LOT                           
         CLC   BUACON,SPACES                                                    
         BNE   *+10                                                             
         MVC   LHBLOCK+3(3),=C'ALL'                                             
         BAS   RE,TABADD                                                        
         LA    RE,13*3             ADD TAB1 TO BUFFALO FOR PRINTING             
         LA    RF,BUFACCS                                                       
         LA    R1,TAB1                                                          
BC422    ZAP   0(8,RF),0(6,R1)                                                  
         LA    RF,8(RF)                                                         
         LA    R1,6(R1)                                                         
         BCT   RE,BC422                                                         
         MVC   BUFKEY,LHBLOCK                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFKEY                               
         LA    R3,TAB1                                                          
         BAS   RE,ADDUP                                                         
BC424    ZIC   R1,BUANOMTH         GET NEXT TABLE ENTRY                         
         SR    R3,R3                                                            
         LA    R3,BUAMLN2Q                                                      
         STH   R3,HALF                                                          
         MH    R1,HALF                                                          
         LA    R1,BUAMLN1Q(R1)                                                  
         AR    R2,R1                                                            
         C     R2,TBND             END OF TABLE                                 
         BL    BC420                                                            
*                                  NOW PRINT                                    
BC426    MVC   BUFKEY,SPACES                                                    
         MVI   MODE,SBACLAST       FOOL FORMAT ROUTINE                          
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFKEY,1                            
BC430    TM    DMCB+8,X'80'                                                     
         BO    BC440                                                            
         LA    R1,13*3                                                          
         LA    RE,BUFACCS          BUILD TAB1 FROM BUFFALO                      
         LA    RF,TAB1                                                          
BC432    ZAP   0(6,RF),0(8,RE)                                                  
         LA    RE,8(RE)                                                         
         LA    RF,6(RF)                                                         
         BCT   R1,BC432                                                         
         MVC   WORK(15),BUFKEY                                                  
         BAS   RE,NAMEFND                                                       
         BAS   RE,NAMCHOP                                                       
         LA    R3,TAB1                                                          
         MVC   LHBLOCK,BUFKEY                                                   
         BAS   RE,FORMAT                                                        
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFKEY,1                             
         B     BC430                                                            
BC440    MVI   MODE,ACCLAST                                                     
         GOTO1 BUFFALO,DMCB,=C'RESET'                                           
         B     BC450                                                            
BC450    DS    0H                                                               
         BAS   RE,SVREAD                                                        
         L     RF,ADACC                                                         
         CLC   TAB2,TAB0                                                        
         BE    XIT                 SKIP IF NOTHING TO PRINT                     
         MVC   LHBLOCK,0(RF)                                                    
         L     R2,ADACCNAM                                                      
         BAS   RE,NAMOUT                                                        
         BAS   RE,NAMCHOP                                                       
         LA    R3,TAB2                                                          
         CLI   REQLEV,1                                                         
         BE    BC452                                                            
         CLI   REQLEV,2                                                         
         BNE   *+8                                                              
BC452    BAS   RE,FORMAT                                                        
         BAS   RE,ADDUP                                                         
         B     XIT                                                              
         EJECT                                                                  
BC500    CLI   MODE,LEVCLAST                                                    
         BNE   BC600                                                            
         LA    R3,TAB3             LEV C TOTAL IS IN TABLE 3                    
         CLC   TAB3,TAB0                                                        
         BE    XIT                                                              
         L     R2,ADLVCNAM                                                      
         BAS   RE,NAMOUT                                                        
         L     RF,ADHEIRC                                                       
         MVC   LHBLOCK,0(RF)                                                    
         BAS   RE,NAMCHOP                                                       
         CLI   REQLEV,0                                                         
         BE    BC510                                                            
         CLI   REQLEV,3                                                         
         BH    BC510                                                            
         BAS   RE,FORMAT                                                        
*                                                                               
BC510    BAS   RE,ADDUP                                                         
         B     XIT                                                              
         EJECT                                                                  
BC600    CLI   MODE,LEVBLAST                                                    
         BNE   BC700                                                            
         LA    R3,TAB3                                                          
         CLI   LEDGLEV,3           LEV B TOTAL IN TAB 3 FOR 3-DEEP              
         BE    *+8                 LEDGER OR TAB 4 FOR 4-DEEP LEDGER            
         LA    R3,TAB4                                                          
         CLI   LEDGLEV,4                                                        
         BNE   BC602                                                            
BC602    ZAP   CTLVC,=P'0'                                                      
         CLC   0(L'TAB1,R3),TAB0                                                
         BE    XIT                                                              
         L     R2,ADLVBNAM                                                      
         BAS   RE,NAMOUT                                                        
         L     RF,ADHEIRB                                                       
         MVC   LHBLOCK,0(RF)                                                    
         BAS   RE,NAMCHOP                                                       
         CLI   REQLEV,0                                                         
         BE    BC610                                                            
         CLI   REQLEV,5                                                         
         BE    BC610                                                            
         BAS   RE,FORMAT                                                        
*                                                                               
BC610    BAS   RE,ADDUP                                                         
         B     XIT                                                              
         EJECT                                                                  
BC700    CLI   MODE,LEVALAST                                                    
         BNE   BC800                                                            
         ZIC   RF,LEDGLEV          POINT TO LEV A TABLE                         
         MH    RF,=H'234'          39 X 6                                       
         LA    R3,TAB1(RF)                                                      
         CLI   LEDGLEV,2                                                        
         BNE   BC702                                                            
BC702    ZAP   CTLVB,=P'0'                                                      
         CLC   0(L'TAB1,R3),TAB0                                                
         BE    XIT                                                              
         L     R2,ADLVANAM                                                      
         BAS   RE,NAMOUT                                                        
         L     R4,ADHEIRA                                                       
         L     RF,ADHEIRA                                                       
         MVC   LHBLOCK,0(RF)                                                    
         BAS   RE,NAMCHOP                                                       
         BAS   RE,FORMAT                                                        
         BAS   RE,MYREPORT                                                      
BC710    BAS   RE,ADDUP                                                         
         B     XIT                                                              
         EJECT                                                                  
BC800    CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         MVC   LHBLOCK+3(12),=CL12'TOTALS FOR'                                  
         MVC   LHBLOCK2,SPACES                                                  
         MVC   LHBLOCK3,SPACES                                                  
         MVC   LHBLOCK2+3(7),=C'REQUEST'                                        
         ZIC   RF,LEDGLEV                                                       
         LA    RF,1(RF)                                                         
         MH    RF,=H'234'          POINT TO REQUEST TABLE                       
         LA    R3,TAB1(RF)                                                      
         BAS   RE,MYREPORT                                                      
         BAS   RE,FORMAT                                                        
         B     XIT                                                              
         EJECT                                                                  
GETRUL   NTR1                      GET ACCOUNT/CONTRA ACCT RULES                
         XC    CBUDLEV,CBUDLEV                                                  
         AH    RF,DATADISP                                                      
GR02     CLI   0(RF),0                                                          
         BE    GR50                                                             
         CLI   0(RF),X'1C'         VALIDATION ELEMENT                           
         BE    GR10                                                             
GR04     ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     GR02                                                             
*                                                                               
         USING ACBVD,RF                                                         
GR10     CLC   ACBVACUL,QUNIT                                                   
         BNE   GR04                                                             
         MVC   ABUDLEV,ACBVACLV    SAVE ACCOUNT LEVEL INFO                      
         LA    R1,CBUDLEV          BUILD TABLE OF CONTRA ACCT RULES             
         ZIC   R6,ACBVLEN                                                       
         LR    R5,RF                                                            
         AR    R5,R6                                                            
         ST    R5,FULL                                                          
         CH    R6,=H'5'            NO CONTRA RULES AT ALL                       
         BNH   GR04                                                             
         LA    R6,ACBVCAUN                                                      
GR12     CLI   0(R6),C'+'          IGNORE FUNNIES                               
         BE    GR14                                                             
         CLI   0(R6),0                                                          
         BE    GR14                                                             
         MVC   0(3,R1),0(R6)                                                    
         LA    R1,3(R1)                                                         
GR14     LA    R6,3(R6)                                                         
         C     R6,FULL             END OF ELEMENT YET                           
         BL    GR12                                                             
         B     GR04                                                             
GR50     CLI   CBUDLEV,0           ANY CONTRA RULES                             
         BE    XIT                                                              
         LA    R3,CBUDLEV          FIND LENGTHS FROM CONTRA-LEDGER              
GR54     DS    0H                  HEIRARCHY ELEMENT                            
         CLI   2(R3),0             LEDGER LEVEL ONLY                            
         BE    GR60                IF SO - GET NEXT                             
         L     R2,=A(IOA)                                                       
         A     R2,PRELOC                                                        
         MVC   0(42,R2),SPACES                                                  
         MVC   0(1,R2),QCOMPANY                                                 
         MVC   1(2,R2),0(R3)       UNIT/LEDGER                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R2),(R2),(0,0)              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         AH    R2,DATADISP                                                      
         SR    R1,R1                                                            
GR56     CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'16'         HEIR EL.                                     
         BE    GR57                                                             
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GR56                                                             
         USING ACHEIRD,R2                                                       
GR57     LA    RF,ACHRLEVA                                                      
         CLI   2(R3),1                                                          
         BE    GR58                                                             
         LA    RF,ACHRLEVB                                                      
         CLI   2(R3),2                                                          
         BE    GR58                                                             
         LA    RF,ACHRLEVC                                                      
         CLI   2(R3),3                                                          
         BE    GR58                                                             
         LA    RF,ACHRLEVD                                                      
GR58     MVC   2(1,R3),0(RF)                                                    
*                                                                               
GR60     LA    R3,3(R3)            GET NEXT CONTRA ENTRY                        
         CLI   0(R3),0                                                          
         BNE   GR54                                                             
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
MYREPORT NTR1                                                                   
         L     RF,ADACC                                                         
         MVC   HEAD6+10(1),2(RF)                                                
         MVC   HEAD5+10(1),1(RF)   UNIT                                         
         L     R2,ADUNTNAM                                                      
         BAS   RE,NAMOUT                                                        
         MVC   HEAD5+12(36),WORK                                                
         L     R2,ADLDGNAM                                                      
         BAS   RE,NAMOUT                                                        
         MVC   HEAD6+12(36),WORK                                                
         MVC   HEAD5+85(6),HFROM                                                
         MVC   HEAD5+92(2),=C'TO'                                               
         MVC   HEAD5+95(6),HTO                                                  
         MVC   HEAD9+36(L'SVH1),SVH1                                            
         MVC   HEAD10+36(L'SVH2),SVH2                                           
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 2                                                                
NAMOUT   NTR1                                                                   
         USING ACNAMED,R2                                                       
         MVC   WORK(36),SPACES                                                  
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   WORK(0),ACNMNAME                                                 
         SPACE 2                                                                
NAMCHOP  NTR1                                                                   
         GOTO1 CHOPPER,DMCB,(36,WORK),(20,LHBLOCK2),2                           
         B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
NAMEFND  NTR1                                                                   
         L     R2,=A(IOA)                                                       
         A     R2,PRELOC                                                        
         MVC   0(42,R2),SPACES                                                  
         MVC   0(15,R2),WORK                                                    
         MVC   WORK(36),SPACES                                                  
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
         SPACE 2                                                                
SVREAD   NTR1                      READ FOR REPOSITIONING                       
         L     RF,=A(IOA)                                                       
         A     RF,PRELOC                                                        
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',SVKEY,(RF),(0,0)             
         B     XIT                                                              
         EJECT                                                                  
*              READ BUDGETS FOR HIGHER LEVELS AND ADD TO TABLES                 
         SPACE 2                                                                
GENBUD   NTR1                                                                   
         MVC   SVKEY,SPACES                                                     
         MVC   SVKEY(L'KEY),KEY                                                 
         L     R5,ADBUDC                                                        
         USING BUDACCD,R5                                                       
         MVI   BUTYPE,TABLER       FOR CONTRA ACCOUNT OPTION                    
         MVC   BUAMTTBL,ADCATABL   BUILD TABLE OF C/AS AND BUDGETS              
         L     RF,TYPTAB                                                        
         ST    RF,BUATYTBL                                                      
         MVI   BUTBLPER,1          EACH ENTRY IS 1 MONTH WIDE                   
         MVC   BUBUDNO+1(1),BUDNUM                                              
         L     RF,=A(LTYPTAB)                                                   
         ST    RF,BUTYTBLN         LENGTH OF TYPE TABLE                         
         L     RF,=A(AMTLNQ)                                                    
         ST    RF,BUAMTBLN         LENGTH OF AMOUNT TABLE                       
         MVC   BUSTDATE,STYR       FILTER ON MAX POSSIBLE DATE RANGE            
         MVC   BUNDDATE,ENYR                                                    
         MVC   BUSERKEY,SPACES                                                  
         MVC   BUSERKEY(15),0(R1)                                               
         XC    BULSTKEY,BULSTKEY                                                
         GOTO1 BUDACC,DMCB,ADBUDC                                               
         L     R1,BUAMTEND                                                      
         C     R1,ADCATABL                                                      
         BE    XIT                 NO ENTRIES                                   
         BCTR  R1,0                                                             
         ST    R1,TBND             END OF AMOUNT TABLE                          
         BAS   RE,SVREAD                                                        
         SPACE 1                                                                
         L     R2,ADCATABL                                                      
         USING BUAMTBLD,R2                                                      
         LA    RF,BUAMNTHS                                                      
         USING BUAMNTHD,RF                                                      
         ZIC   R1,BUANOMTH                                                      
         L     R3,TBAD                                                          
GB2      LA    RE,12*6(R3)         TOTAL FIELD                                  
         LA    R4,MASK                                                          
         LR    R6,R3                                                            
         LA    R2,12                                                            
GB4      CLC   BUASYYMM,0(R4)                                                   
         BNE   GB6                                                              
         OC    BUAMBUD,BUAMBUD                                                  
         BZ    GB6                                                              
         AP    0(6,R6),BUAMBUD                                                  
         AP    0(6,RE),BUAMBUD                                                  
         USING BUAMNTHD,RF                                                      
GB6      LA    R6,6(R6)                                                         
         LA    R4,2(R4)                                                         
         BCT   R2,GB4                                                           
         LA    RF,BUAMLN2Q(RF)                                                  
         BCT   R1,GB2                                                           
         B     XIT                                                              
         EJECT                                                                  
TABADD   NTR1                      ADD BUDGET STRING TO TABLE 1                 
         USING BUDACCD,R5                                                       
         USING BUAMTBLD,R2                                                      
         LA    RF,BUAMNTHS                                                      
         USING BUAMNTHD,RF                                                      
         ZIC   R1,BUANOMTH                                                      
TB2      LA    RE,TAB1+12*6        POINT TO TOTAL FIELD                         
         LA    R4,MASK                                                          
         LA    R6,TAB1                                                          
         LA    R2,12                                                            
TB4      CLC   BUASYYMM,0(R4)                                                   
         BNE   TB6                                                              
         OC    BUAMBUD,BUAMBUD                                                  
         BZ    TB6                                                              
         AP    0(6,R6),BUAMBUD                                                  
         AP    0(6,RE),BUAMBUD                                                  
         USING BUAMNTHD,RF                                                      
TB6      LA    R6,6(R6)                                                         
         LA    R4,2(R4)                                                         
         BCT   R2,TB4                                                           
         LA    RF,BUAMLN2Q(RF)                                                  
         BCT   R1,TB2                                                           
         B     XIT                                                              
         EJECT                                                                  
ADDUP    NTR1                      ADD ONE TABLE TO ANOTHER                     
         LA    RE,39*6(R3)                                                      
         LA    RF,39                                                            
ADDUP2   AP    0(6,RE),0(6,R3)                                                  
         ZAP   0(6,R3),=P'0'                                                    
         LA    RE,6(RE)                                                         
         LA    R3,6(R3)                                                         
         BCT   RF,ADDUP2                                                        
         B     XIT                                                              
         EJECT                                                                  
FORMAT   NTR1                      PRINT A TABLE POINTED TO BY R3               
         ZIC   RF,LINE             DO WE HAVE ROOM FOR                          
         ZIC   RE,MAXLINES         ANOTHER 3 OR 6 LINES                         
         LA    R1,3                                                             
         CLI   TWOLINE,C'Y'                                                     
         BNE   *+8                                                              
         LA    R1,6                                                             
         AR    RF,R1                                                            
         CR    RF,RE                                                            
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         L     R5,ADPBLOC                                                       
         USING PBLOCD,R5                                                        
         MVC   PEE1+1(12),LHBLOCK+3                                             
         CLI   QOPT1,C'C'                                                       
         BNE   FMT1A                                                            
         CLI   MODE,SBACLAST                                                    
         BNE   FMT1C                                                            
         MVC   PEE1+1(14),LHBLOCK+1                                             
         B     FMT1E                                                            
FMT1A    CLI   MODE,ACCLAST                                                     
         BE    FMT1E                                                            
FMT1C    MVI   PEE1+1,C'*'                                                      
         MVC   PEE1+2(12),LHBLOCK+3                                             
FMT1E    DS    0H                                                               
         MVC   PEE2+2(20),LHBLOCK2                                              
         MVC   PEE3+2(20),LHBLOCK3                                              
         MVC   PEE1+23(10),BUDNAME                                              
         LR    R4,R3                                                            
         LA    R2,PEE1+33                                                       
         BAS   RE,TFORM                                                         
*                                                                               
         LA    R2,PEE2+33                                                       
         LA    R1,PEE2+23                                                       
         CLI   TWOLINE,C'Y'                                                     
         BNE   FMT2                                                             
         LA    R2,PEE3+33                                                       
         LA    R1,PEE3+23                                                       
FMT2     MVC   0(10,R1),=CL10'ACTUAL'                                           
         LA    R4,13*6(R4)                                                      
         BAS   RE,TFORM                                                         
*                                                                               
         LA    R2,2                                                             
         CLI   QOPT2,C' '          THIRD CHUNK TO PRINT                         
         BE    FMT10                                                            
         LA    R2,PEE3+33                                                       
         LA    R1,PEE3+23                                                       
         CLI   TWOLINE,C'Y'                                                     
         BNE   FMT4                                                             
         LA    R2,PEE5+33                                                       
         LA    R1,PEE5+23                                                       
FMT4     MVC   0(10,R1),COMPNAME                                                
         LA    R4,26*6(R3)                                                      
         BAS   RE,CFORM                                                         
         LA    R2,3                                                             
*                                                                               
FMT10    DS    0H                                                               
         CLI   TWOLINE,C'Y'                                                     
         BNE   FMT12                                                            
         SLA   R2,1                                                             
FMT12    MVC   P(110),0(R5)                                                     
         BAS   RE,MYREPORT                                                      
         MVC   0(110,R5),SPACES                                                 
         LA    R5,110(R5)                                                       
         BCT   R2,FMT12                                                         
         BAS   RE,MYREPORT                                                      
         B     XIT                                                              
         EJECT                                                                  
TFORM    NTR1                                                                   
         ST    R4,FULL                                                          
         LR    R3,R2                                                            
         LA    R5,2                                                             
         CLI   TWOLINE,C'Y'                                                     
         BE    *+8                                                              
         LA    R5,1                                                             
TFM2     LA    RF,6                                                             
TFM4     ZAP   DIV,0(6,R4)                                                      
         AP    DIV,=P'50'                                                       
         CP    DIV,=P'0'                                                        
         BH    *+10                                                             
         SP    DIV,=P'100'                                                      
         DP    DIV,=P'100'                                                      
         EDIT  (P12,DIV),(10,0(R3)),FLOAT=-                                     
         LA    R3,11(R3)                                                        
         LA    R4,6(R4)                                                         
         BCT   RF,TFM4                                                          
         LA    R3,110(R2)          POINT TO NEXT LINE                           
         BCT   R5,TFM2                                                          
*                                                                               
         L     R4,FULL             NOW TOTAL FIELD                              
         CLI   TWOLINE,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,110(R2)          FIRST OR SECOND LINE                         
         ZAP   DIV,12*6(6,R4)                                                   
         AP    DIV,=P'50'                                                       
         CP    DIV,=P'0'                                                        
         BH    *+10                                                             
         SP    DIV,=P'100'                                                      
         DP    DIV,=P'100'                                                      
         LA    R2,66(R2)                                                        
         EDIT  (P12,DIV),(10,0(R2)),FLOAT=-                                     
         B     XIT                                                              
         EJECT                                                                  
CFORM    NTR1                      FORMAT COMPARISION LINES                     
         ST    R4,FULL                                                          
         LR    R5,R4                                                            
         SH    R5,=H'156'          GO BACK TO BUDGET LINE                       
         LR    R3,R2                                                            
         LA    RF,13                                                            
         LA    R1,TEMP                                                          
         CLI   QOPT2,C'V'          VARIANCE                                     
         BE    CFORM2                                                           
         CLI   QOPT2,C'I'          INDEX                                        
         BE    CFORM10                                                          
         CLI   QOPT2,C'B'                                                       
         BE    CFORM30                                                          
         B     CFORM20             PERCENT                                      
*                                                                               
CFORM2   ZAP   DIV,0(6,R4)                                                      
         SP    DIV,0(6,R5)         VARIANCE = ACTUAL - BUDGET                   
         AP    DIV,=P'50'                                                       
         CP    DIV,=P'0'                                                        
         BH    *+10                                                             
         SP    DIV,=P'100'                                                      
         DP    DIV,=P'100'                                                      
         ZAP   0(6,R1),DIV(12)                                                  
         LA    R1,6(R1)                                                         
         LA    R4,6(R4)                                                         
         LA    R5,6(R5)                                                         
         BCT   RF,CFORM2                                                        
         B     CFORM70                                                          
         SPACE 2                                                                
CFORM10  ZAP   0(6,R1),=P'0'                                                    
         CP    0(6,R4),=P'0'                                                    
         BE    CFORM12                                                          
         CP    0(6,R5),=P'0'                                                    
         BE    CFORM12                                                          
         ZAP   DIV,0(6,R4)         INDEX    = ACT X 100  -  100                 
         MP    DIV,=P'10000'                  ---------                         
         DP    DIV,0(6,R5)                       BUD                            
         SP    DIV(8),=P'10000'                                                 
         ZAP   0(6,R1),DIV(8)                                                   
CFORM12  DS    0H                                                               
         LA    R1,6(R1)                                                         
         LA    R4,6(R4)                                                         
         LA    R5,6(R5)                                                         
         BCT   RF,CFORM10                                                       
         B     CFORM70                                                          
         SPACE 2                                                                
CFORM20  ZAP   0(6,R1),=P'0'       PERCENT                                      
         CP    0(6,R4),=P'0'                                                    
         BE    CFORM22                                                          
         CP    0(6,R5),=P'0'                                                    
         BE    CFORM22                                                          
         ZAP   DIV,0(6,R4)                                                      
         MP    DIV,=P'10000'                                                    
         DP    DIV,0(6,R5)                                                      
         ZAP   0(6,R1),DIV(8)                                                   
CFORM22  DS    0H                                                               
         LA    R1,6(R1)                                                         
         LA    R4,6(R4)                                                         
         LA    R5,6(R5)                                                         
         BCT   RF,CFORM20                                                       
         B     CFORM70                                                          
         SPACE 1                                                                
CFORM30  ZAP   DIV,0(6,R5)         BALANCE = BUDGET - ACTUAL                    
         SP    DIV,0(6,R4)                                                      
         AP    DIV,=P'50'                                                       
         CP    DIV,=P'0'                                                        
         BH    *+10                                                             
         SP    DIV,=P'100'                                                      
         DP    DIV,=P'100'                                                      
         ZAP   0(6,R1),DIV(12)                                                  
         LA    R1,6(R1)                                                         
         LA    R4,6(R4)                                                         
         LA    R5,6(R5)                                                         
         BCT   RF,CFORM30                                                       
         B     CFORM70                                                          
         SPACE 2                                                                
CFORM70  LA    RE,TEMP                                                          
         LA    R5,2                                                             
         CLI   TWOLINE,C'Y'                                                     
         BE    *+8                                                              
         LA    R5,1                                                             
CFORM72  LA    RF,6                                                             
CFORM74  DS    0H                                                               
         CP    0(6,RE),=P'0'                                                    
         BE    CFORM80                                                          
         CLI   QOPT2,C'V'                                                       
         BE    CFORM76                                                          
         CLI   QOPT2,C'I'                                                       
         BE    CFORM78                                                          
         CLI   QOPT2,C'B'                                                       
         BE    CFORM76                                                          
         EDIT  (P6,0(RE)),(10,0(R3)),2                                          
         B     CFORM80                                                          
*                                                                               
CFORM76  DS    0H                                                               
         EDIT  (P6,0(RE)),(10,0(R3)),FLOAT=-                                    
         B     CFORM80                                                          
*                                                                               
CFORM78  DS    0H                                                               
         EDIT  (P6,0(RE)),(10,0(R3)),2,FLOAT=-                                  
         B     CFORM80                                                          
*                                                                               
CFORM80  LA    R3,11(R3)                                                        
         LA    RE,6(RE)                                                         
         BCT   RF,CFORM74                                                       
         LA    R3,110(R2)                                                       
         BCT   R5,CFORM72                                                       
*                                  TOTALS                                       
         L     R4,FULL                                                          
         LA    RE,TEMP+12*6                                                     
         CLI   TWOLINE,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,110(R2)                                                       
         LA    R2,66(R2)                                                        
         CLI   QOPT2,C'V'                                                       
         BE    CFORM90                                                          
         CLI   QOPT2,C'I'                                                       
         BE    CFORM92                                                          
         CLI   QOPT2,C'B'                                                       
         BE    CFORM90                                                          
         B     CFORM94                                                          
*                                                                               
CFORM90  DS    0H                                                               
         EDIT  (P6,0(RE)),(10,0(R2)),FLOAT=-                                    
         B     XIT                                                              
*                                                                               
CFORM92  DS    0H                                                               
         EDIT  (P6,0(RE)),(10,0(R2)),2,FLOAT=-                                  
         B     XIT                                                              
*                                                                               
CFORM94  DS    0H                                                               
         EDIT  (P6,0(RE)),(10,0(R2)),2                                          
         B     XIT                                                              
         EJECT                                                                  
REQLVTAB DC    C'C',X'01'                                                       
         DC    C'A',X'02'                                                       
         DC    C'3',X'03'                                                       
         DC    C'2',X'04'                                                       
         DC    C'1',X'05'                                                       
         DC    X'FF'                                                            
         EJECT                                                                  
*              DSECT FOR LOCAL W/S                                              
         SPACE 2                                                                
PROGD    DSECT                                                                  
TYPTAB   DS    A                                                                
PRELOC   DS    F                                                                
BUDACC   DS    V                                                                
ADBUDC   DS    A                                                                
ADBUFC   DS    A                                                                
ADCATABL DS    A                                                                
TBND     DS    F                                                                
ADPBLOC  DS    A                                                                
TBAD     DS    F                                                                
STYR     DS    CL2                                                              
         DS    CL1                                                              
ENYR     DS    CL2                                                              
         DS    CL1                                                              
HFROM    DS    CL6                                                              
HTO      DS    CL6                                                              
BUDNUM   DS    CL1                                                              
BUDNAME  DS    CL10                                                             
COMPNAME DS    CL10                                                             
SVH1     DS    CL62                                                             
SVH2     DS    CL62                                                             
TEMP     DS    CL100                                                            
MASK     DS    CL24                                                             
THISLEV  DS    CL1                                                              
LEDGLEV  DS    CL1                                                              
REQLEV   DS    CL1                                                              
TWOLINE  DS    CL1                                                              
CACTIV   DS    CL1                                                              
SVKEY    DS    CL49                                                             
CONTRAC  DS    CL15                                                             
CONTRNM  DS    CL36                                                             
DIV      DS    PL14                                                             
CTACC    DS    PL4                                                              
CTLVC    DS    PL4                                                              
CTLVB    DS    PL4                                                              
LHBLOCK  DS    CL15                                                             
LHBLOCK2 DS    CL20                                                             
LHBLOCK3 DS    CL20                                                             
REQWDTH  DS    CL1                                                              
UL       DS    CL2                                                              
LISTRUL  DS    CL1                                                              
SVCONTRA DS    CL15                                                             
CONTCHOP DS    CL1                                                              
ABUDLEV  DS    CL1                                                              
CBUDLEV  DS    CL255                                                            
BUFREC   DS    0CL327                                                           
BUFKEY   DS    CL15                                                             
BUFACCS  DS    312C                13*3*8                                       
TAB0     DS    CL234                                                            
TAB1     DS    CL234                                                            
TAB2     DS    CL234                                                            
TAB3     DS    CL234                                                            
TAB4     DS    CL234                                                            
TAB5     DS    CL234                                                            
TAB6     DS    CL234                                                            
LTYPTAB  EQU   8000                                                             
         SPACE 2                                                                
PBLOCD   DSECT                     FOR PRINT BLOCK                              
PEE1     DS    CL110                                                            
PEE2     DS    CL110                                                            
PEE3     DS    CL110                                                            
PEE4     DS    CL110                                                            
PEE5     DS    CL110                                                            
PEE6     DS    CL110                                                            
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
         BUFF  LINES=100,ROWS=1,COLUMNS=39,FLAVOR=PACKED,KEYLIST=(15,A)         
BUDC     DS    0H                                                               
         DC    1300X'00'                                                        
IOA      DS    0H                                                               
         DS    1000C                                                            
*                                                                               
AMTTAB   DS    0D                    AMOUNT TABLE                               
         DS    (800*((12*BUAMLN2Q)+BUAMLN1Q))C                                  
         DS    CL1                                                              
AMTLNQ   EQU   *-AMTTAB                                                         
*                                                                               
PBLOC    DS    0H                                                               
         DS    6CL110                                                           
TYPTBL   DS    0H                                                               
         DS    8000C                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020ACREPB302 05/18/07'                                      
         END                                                                    
