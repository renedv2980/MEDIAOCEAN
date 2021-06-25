*          DATA SET PPREPS202  AT LEVEL 189 AS OF 11/20/15                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 046188.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE PPS202A                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE PPRDSHR                                                                
*INCLUDE GETCOST                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE RIGHT                                                                  
         TITLE 'CHANGE LOG'                                                     
*                                                                               
* BPLA 01/05/2012   RELINK WITH NEW GETCOST QST CHANGE                          
*                                                                               
* BPLA 01/11    RELINK WITH NEW GETCOST - CANADIAN QST CHANGE                   
*               + FIX AMBIGUOUS USING ISSUE                                     
*                                                                               
* SMYE 05/00    CHANGES FOR LARGER PPNEWFILE                                    
*                                                                               
* SMYE 12/12/95 CHANGED DTCNV TO DATCON WITH NEW PARAM'S                        
*                                                                               
* BPLA 6/95     FIX YET SOME MORE OF ABBY'S BUGS                                
*                                                                               
* BPLA 6/95     FIX MORE OF ABBY'S BUGS                                         
*                                                                               
* BPLA 5/95     REMOVE EST AND OR PRODUCT FROM HEADLINES ON RECAP               
*               IF PRD/EST IS ALL                                               
*                                                                               
* BPLA 5/95     CHANGES FOR NEW GETCOST - READ AND SAVE B2 PROFILE              
*               IN BLDREQT AND CHECK WHEN GOTO GETCOST                          
*                                                                               
* AROT 1/11/95  ADCODE RECAP (IF QOPT5=Y)                                       
*                                                                               
* BPLA 6/30/93  MOVE SETTING MTHOPT FROM NPROC TO FBCLI (FBUYCLI)               
*                                                                               
* BPLA 5/13/93  PROFILE OPTION TO SUPPRESS MONTHLY TOTALS FOR                   
*               PUB AND VENDOR -PROGPROF+15   N = DON'T SUPPRESS                
*                                       $ = SUPPRESS DOLLARS                    
*                                       I = SUPPRESS INSERTION COUNTS           
*                                       B = SUPPRESS BOTH                       
*                                                                               
* BPLA 6/17/92  FIX BUG IN DATE CHANGE INDICATOR AND FLAG                       
*               (WITH *) CHANGES TO ANY DATE BEING DISPLAYED                    
*                                                                               
* BPLA 4/10/92  IN FBPRO IF PROD IS IN HEADLINES SET FORCEHED TO "Y"            
*               IN FBEST IF EST IS IN HEADLINES SET FORCEHED TO "Y"             
*                                                                               
* BPLA 1/28/92  QOPT7 (INCLUDE TEST BUYS) WAS CHANGING REPORT CODE              
*               TO "ST" INSTEAD OF 'S2T"                                        
* BPLA 7/22/91  ROGER'S BEGUGGING LOGIC NO-OPED (MYREPT)                        
*               ADD *INCLUDES FOR PRINT AND PRNTBL IF YOU WANT TO USE           
*         AND CHANGE THE "GOTO1 REPORT"S WITH THE "GOTO1 =V(MYPEPT)"S           
* BPLA 4/17/91  GLOSSARY FIXES                                                  
*                                                                               
* ROSA 4/1/91   ADD LOGIC FOR PI= FOR JWT ONLY                                  
* ROSA 2/15/91  ADD STANDARD COMMENT TO PRINT AT END OF REP                     
         TITLE 'PPS202 - PRINTPAK  SCHEDULE SPREADSHEET REPORT'                 
*                                                                               
*                                                                               
* QOPT3        WEEKS          W=WEEKS                                           
* QOPT4        DOLLAR TYPE    G=GROSS,N=NET,C=CD,1=G-CD,2=N-CD,T=COST           
* QOPT5        SHOULD AN ADCODE RECAP BE MADE?                                  
* QOPT7        USED IN REQ CARD FOR TEST INSERTIONS                             
*                                                                               
*                                                                               
PPS202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPS202                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'       2ND DSECT                                    
         L     R7,PPWORK2C                                                      
         USING PPWORK2D,R7                                                      
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPS2WRKD,R8                                                      
         MVC   ACONIO1,ACONIO      (A)PCONREC                                   
         SPACE 3                                                                
         CLI   MODE,REQLAST                                                     
         BE    LEVLO9A                                                          
         CLI   MODE,RUNFRST                                                     
         BE    FIRST                                                            
         CLI   MODE,LBUYPUB                                                     
         BNE   PPS2B                                                            
         BAS   RE,LBPUB                                                         
         B     EXIT                                                             
*                                                                               
PPS2B    CLI   MODE,LBUYVEN                                                     
         BNE   PPS2C                                                            
         BAS   RE,LBPUB                                                         
         B     EXIT                                                             
*                                                                               
PPS2C    JIF   MODE,=,PROCBUY,PRBUY,JUMP=N                                      
         JIF   MODE,=,LBUYMKT,LBMKT,JUMP=N                                      
         JIF   MODE,=,LBUYDST,LBDST,JUMP=N                                      
         JIF   MODE,=,LBUYREG,LBREG,JUMP=N                                      
         JIF   MODE,=,LBUYEST,LBEST,JUMP=N                                      
         JIF   MODE,=,LBUYPRO,LBPRD,JUMP=N                                      
         JIF   MODE,=,LBUYDIV,LBDIV,JUMP=N                                      
         JIF   MODE,=,LBUYCLI,LBCLT,JUMP=N                                      
         JIF   MODE,=,LBUYREQ,LBREQ,JUMP=N                                      
         JIF   MODE,=,FBUYPUB,FBPUB,JUMP=N                                      
         JIF   MODE,=,FBUYMKT,FBMKT,JUMP=N                                      
         JIF   MODE,=,FBUYDST,FBDST,JUMP=N                                      
         JIF   MODE,=,FBUYREG,FBREG,JUMP=N                                      
         JIF   MODE,=,FBUYEST,FBEST,JUMP=N                                      
         JIF   MODE,=,FBUYPRO,FBPRO,JUMP=N                                      
         JIF   MODE,=,FBUYDIV,FBDIV,JUMP=N                                      
         JIF   MODE,=,FBUYCLI,FBCLI,JUMP=N                                      
         JIF   MODE,=,FBUYREQ,FBREQ,JUMP=N                                      
         JIF   MODE,=,REQLAST,LEVLO9A,JUMP=N                                    
         MVC   LASTMODE,MODE                                                    
EXIT     DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
FIRST    DS    0H                                                               
         LA    RE,PPS2WRK                                                       
         LA    RF,PPS2WRKL                                                      
         XCEF                                                                   
*                                                                               
         RELOC (R2)                                                             
*                                                                               
         LA    R3,ADDRS                                                         
         LA    R4,ACONS                                                         
         LA    R0,(ACONSX-ACONS)/4                                              
FIRST2   DS    0H                                                               
         L     RF,0(R4)                                                         
         AR    RF,R2                                                            
         ST    RF,0(R3)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,FIRST2                                                        
*                                                                               
         MVI   MYDASH,C'-'                                                      
         MVC   MYDASH+1(L'MYDASH-1),MYDASH                                      
         MVC   EFFS,=13X'FF'                                                    
         B     EXIT                                                             
         SPACE 3                                                                
ACONS    DS    0F                                                               
         DC    A(DTL)                                                           
         DC    A(SETHEAD)                                                       
         DC    A(SETHOOK)                                                       
         DC    A(DATOUT)                                                        
         DC    A(POST)                                                          
         DC    A(BLDBUF)                                                        
         DC    A(RECAP)                                                         
         DC    V(BINSRCH)                                                       
         DC    A(GETSPCE)                                                       
         DC    A(SETPUB)                                                        
         DC    A(NPROC)                                                         
         DC    A(ROLLUP)                                                        
         DC    A(PRTALL)                                                        
         DC    V(PPRDSHR)                                                       
         DC    V(CHOPPER)                                                       
         DC    V(RIGHT)                                                         
         DC    A(FORMAT)                                                        
         DC    A(BLDREQT)                                                       
         DC    V(GETCOST)                                                       
         DC    A(GETBF)                                                         
         DC    A(JOBLST)                                                        
         DC    A(JOBLSTX)                                                       
         DC    A(BUYTAB)                                                        
         DC    A(BUYTABX)                                                       
         DC    A(MYBUY)                                                         
         DC    A(DTLINS)                                                        
         DC    A(COMTAB)                                                        
         DC    A(SPCTAB)                                                        
         DC    A(SPCTOTS)                                                       
         DC    A(SPCTAB2)                                                       
         DC    A(SPCTOTS2)                                                      
         DC    A(PUBTOTS)                                                       
         DC    A(VENTOTS)                                                       
         DC    A(MKTTOTS)                                                       
         DC    A(DSTTOTS)                                                       
         DC    A(REGTOTS)                                                       
         DC    A(ESTTOTS)                                                       
         DC    A(PRDTOTS)                                                       
         DC    A(DIVTOTS)                                                       
         DC    A(CLTTOTS)                                                       
         DC    A(REQTOTS)                                                       
         DC    A(BUFFALOC)                                                      
ACONSX   EQU   *                                                                
         EJECT                                                                  
         SPACE 3                                                                
FBEST    DS    0H                                                               
         MVI   RISW,0              SHOW 'APPARENT' INS COUTNS                   
         TM    ESTSW,X'80'                                                      
         BZ    *+8                                                              
         BAS   RE,SETEST                                                        
         GOTO1 ASETHEAD           SET DATES AND HEADLINES                       
*                                                                               
         TM    ESTSW,X'08'        SEE IF EST IN HEADLINES                       
         BZ    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         OC    PROF,PROF                                                        
         BZ    FBEST2                                                           
         CLC   QEST,=C'ALL'                                                     
         BNE   EXIT                                                             
FBEST2   DS    0H                                                               
         MVI   BFSW,C'Y'                                                        
         B     EXIT                                                             
         SPACE 2                                                                
FBPUB    DS    0H                                                               
         L     R1,ABUYTAB                                                       
         ST    R1,ANXTBUY          RESET ANXTBUY FOR EACH PUB                   
         MVI   0(R1),X'FF'                                                      
         GOTO1 ASETPUB                                                          
         B     EXIT                                                             
         SPACE 2                                                                
FBMKT    DS    0H                                                               
         BAS   RE,SETMKT                                                        
         B     EXIT                                                             
         SPACE 2                                                                
FBDST    DS    0H                                                               
         BAS   RE,SETDST                                                        
         B     EXIT                                                             
         SPACE 2                                                                
FBREG    DS    0H                                                               
         BAS   RE,SETREG                                                        
         B     EXIT                                                             
         SPACE 2                                                                
FBPRO    DS    0H                                                               
         TM    PRDSW,X'80'                                                      
         BZ    *+8                                                              
         BAS   RE,SETPRD                                                        
***PRD                                                                          
         TM    PRDSW,X'08'     SEE IF PRD IN HEADLINES                          
         BNZ   *+14                                                             
         XC    PPRDKEY(4),PPRDKEY                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   QPRODUCT,C' '   NO PRD IN HEADS IF QPRODUCT IS C' '              
         BNE   *+10                                                             
         XC    PPRDKEY(4),PPRDKEY  NO PRD IN HEADLINES                          
***PRD                                                                          
         JIF   LEVSW,=,C'B',OR,C'V',OR,C'C',EXIT,JUMP=N                         
         MVI   BFSW,C'Y'           PRINT COST FORMULA                           
         B     EXIT                                                             
         SPACE 2                                                                
FBDIV    DS    0H                                                               
         TM    DIVSW,X'80'                                                      
         BZ    *+8                                                              
         BAS   RE,SETDIV                                                        
         B     EXIT                                                             
         SPACE 2                                                                
FBCLI    DS    0H                                                               
         TM    CLTSW,X'80'                                                      
         BZ    *+8                                                              
         BAS   RE,SETCLI                                                        
         GOTO1 ABLDREQT                                                         
         TM    CLTSW,X'08'        CLIENT IN HEADS                               
         BZ    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         SPACE 2                                                                
FBREQ    DS    0H                                                               
*****                                                                           
         MVI   FCRDTEST,C'N'                                                    
         CLI   QOPT7,C'Y'                                                       
         BNE   FBREQ10                                                          
         MVI   FCRDTEST,C'Y'                                                    
FBREQ10  GOTO1 ANPROC                                                           
         GOTO1 ASETHOOK    MUST SET HEADHOOK IN SEPERATE CSECT                  
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF      INITIALIZE BUFFALO               
         B     EXIT                                                             
         SPACE 3                                                                
PRBUY    DS    0H                  PROCESS BUY                                  
*                                                                               
         MVC   MYPRD,PPRDKPRD      SAVE PRODUCT I'M PROCESSING                  
         CLI   QPRODUCT,C' '                                                    
         BNE   *+10                                                             
         MVC   MYPRD,PBUYKPRD       NOTE THAT IF QPRODUCT IS SPACES             
*                                   ONLY ZZZ FORMULAS CAN BE READ               
         GOTO1 APOST                                                            
PRBUYX   B     EXIT                                                             
         SPACE 3                                                                
LBPUB    NTR1                      LAST FOR PUB/VENDOR                          
         MVC   SVPPGKEY,KEY                                                     
*                               READ BUYS IN BUYTAB AND DISPLAY THEM            
         MVC   SVGROSS,GROSS       SAVE PROCBUY'S GROSS ,ETC                    
*                                                                               
         CLI   MODE,LBUYVEN        ONLY DO BUYS FOR AT LBUYPUB                  
         BE    PUBOPTV             SKIP TO TOTALS                               
*                                                                               
         L     R5,ABUYTAB                                                       
         CLI   0(R5),X'FF'         NO BUYS                                      
         BE    LBPUBX                                                           
*                                                                               
         CLI   PAGOPT,C'P'                                                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                  PRINT ANY HIGHER BREAK DESCS                 
         XC    ADESC,ADESC                                                      
         LA    R3,DIVN             THIS CODE WAS IN LEVEQ (PP60)                
         LA    R4,NML*4                                                         
         LCR   R4,R4                                                            
         LA    R5,MKTN                                                          
LBPUB1   DS    0H                                                               
         C     R3,ADESC                                                         
         BNH   LBPUB1B                                                          
         CLC   0(NML*4,R3),SPACES                                               
         BE    LBPUB1A             NONE                                         
         GOTO1 APRTALL,DMCB,(R3),SPACES,SPACES                                  
         MVC   0(NML*4,R3),SPACES       CLEAR                                   
LBPUB1A  DS    0H                                                               
         BXH   R3,R4,LBPUB1                                                     
*                                                                               
LBPUB1B  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         LA    R5,PUBN                                                          
         LA    R4,P1                                                            
LBPB0    CLC   0(PNML,R5),SPACES                                                
         BE    LBPB1                                                            
         MVC   0(PNML,R4),0(R5)                                                 
         LA    RF,1(RF)                                                         
         LA    R4,132(R4)                                                       
         LA    R5,PNML(R5)                                                      
         B     LBPB0                                                            
*                                                                               
LBPB1    MVI   SPACING,1                                                        
         LA    RF,2(RF)            WAS 3                                        
         MVI   ALLOWLIN,0          JUST IN CASE                                 
         ZIC   RE,ALLOWLIN                                                      
         AR    RE,RF                                                            
         ZIC   RF,DATANUM                                                       
         AR    RE,RF                                                            
         STC   RE,ALLOWLIN                                                      
         GOTO1 REPORT                                                           
******   GOTO1 =V(MYREPT),DMCB,=C'1'                                            
         CLI   DOBOXES,C'N'                                                     
         BE    GOTORPA                                                          
         L     RF,ABOX                                                          
         USING BOXD,RF                                                          
         MVI   BOXREQ,C'O'                                                      
GOTORPA  DS    0H                                                               
         GOTO1 INITBOX,DMCB,(R7),(RA),LINE                                      
         GOTO1 REPORT                                                           
*******  GOTO1 =V(MYREPT),DMCB,=C'A'                                            
         DROP  RF                                                               
*                                                                               
LBPB2    SR    R2,R2                                                            
         STC   R2,WORK                                                          
         MVI   DATASW,0                                                         
LBPB4    L     R5,ABUYTAB                                                       
LBPB5    CLI   0(R5),X'FF'         END OF TABLE                                 
         BE    LBPB20                                                           
         CLC   WORK(1),0(R5)                                                    
         BE    LBPB10                                                           
         LA    R5,6(R5)                                                         
         B     LBPB5                                                            
*                                                                               
LBPB10   DS    0H                                                               
LBPB10A  MVC   KEY+27(4),2(R5)                                                  
         MVC   AREC,AMYBUY                                                      
         GOTO1 GETPRT                                                           
         L     R4,AMYBUY                                                        
         USING PBUYREC,R4                                                       
         GOTO1 GETINS,DMCB,PBUYKEY,GROSS,(FCOAPRD,PBUYKPRD)                     
         TM    PBUYCNTL,X'80'      SEE IF DELETED                               
         BZ    *+10                                                             
         XC    GROSS(12),GROSS     CLEAR GROSS                                  
         CLI   QOPT4,C'T'          SEE IF DOING COST                            
         BNE   LBPB11                                                           
         MVC   MYPRD,PBUYKPRD      IF ZZZ BUY WILL USE ZZZ FORMULAS             
*                                  FOR REPORTING IN GRID                        
*                                  PROGRAM USES BRAND FOR POSTING               
*                                  TO ACCUMLATORS                               
         GOTO1 AGETBF,DMCB,PBUYREC     BILLING FORMULA                          
LBPB11   DS    0H                                                               
*                                                                               
         DROP  R4                                                               
*                                                                               
LBPB15   LA    R4,P1+22                                                         
         LR    R1,R2                                                            
         MH    R1,=H'8'                                                         
         AR    R4,R1               ADD DISPLACEMENT                             
         ST    R4,FULL                                                          
         GOTO1 AFORMAT                                                          
         MVI   DATASW,1                                                         
         MVI   0(R5),X'99'         SO I WON'T REDO THIS BUY                     
LBPB20   LA    R2,1(R2)                                                         
         STC   R2,WORK                                                          
         C     R2,NMOS                                                          
         BL    LBPB4                                                            
*                                                                               
LBPB50   CLI   DATASW,1            SEE IF I FOUND BUYS                          
         BNE   LBPUB2                                                           
LBPB60   XC    FULL,FULL           TELLS FORMAT TO DO ANNOTATION                
         GOTO1 AFORMAT                                                          
         MVI   JUSTNOW,C'N'                                                     
         MVI   SPACING,1           WAS 2                                        
         SR    RF,RF                                                            
         LA    RF,1(RF) WAS 2                                                   
         MVI   ALLOWLIN,0                                                       
         ZIC   RE,ALLOWLIN                                                      
         AR    RE,RF                                                            
         ZIC   RF,DATANUM          NUMBER OF DATA LINES                         
         AR    RE,RF                                                            
         STC   RE,ALLOWLIN                                                      
         STC   RE,CONTROLS                                                      
NOTJAN9  DS    0H                                                               
         GOTO1 REPORT                                                           
******** GOTO1 =V(MYREPT),DMCB,=C'B'                                            
*                                                                               
         L     RF,ABOX                                                          
         USING BOXD,RF                                                          
         ZIC   RE,CONTROLS                                                      
         ZIC   R1,LINE             WHERE I AM ON PAGE                           
         LA    RE,1(R1,RE)                                                      
         STC   RE,DMCB             LINES                                        
         CLC   MAXLINES(1),DMCB     WILL IT FIT ON PAGE                         
         BH    DOBOXB                                                           
         CLI   DOBOXES,C'N'                                                     
         BE    GOREPTC                                                          
         MVI   BOXREQ,C'C'         CLOSE THE SUCKER                             
GOREPTC  BAS   RE,ENDDATA                                                       
         BE    DOBOXC                                                           
         GOTO1 REPORT                                                           
*******  GOTO1 =V(MYREPT),DMCB,=C'C'                                            
         BAS   R5,CLEARBOX                                                      
         L     RF,ABOX                                                          
         USING BOXD,RF                                                          
         CLI   DOBOXES,C'N'                                                     
         BE    *+8                                                              
         MVI   BOXYORN,C'Y'       NO BOXES IN HEAD                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
******** GOTO1 =V(MYREPT),DMCB,=C'D'                                            
         GOTO1 INITBOX,DMCB,(R7),(RA),LINE                                      
         CLI   DOBOXES,C'N'                                                     
         BE    DOBOXR                                                           
         L     RF,ABOX                                                          
         MVI   BOXREQ,C'O'         OPEN                                         
         B     DOBOXR                                                           
*                                                                               
DOBOXB   DS    0H                                                               
         BAS   RE,ENDDATA                                                       
         BNE   DOBOXBB                                                          
DOBOXC   CLI   DOBOXES,C'N'                                                     
         BE    *+8                                                              
         MVI   BOXREQ,C'C'                                                      
         GOTO1 REPORT                                                           
******** GOTO1 =V(MYREPT),DMCB,=C'E'                                            
         CLI   DOBOXES,C'N'                                                     
         BE    LBPB2                                                            
         BAS   R5,CLEARBOX                                                      
         L     RF,ABOX                                                          
         MVI   BOXYORN,C'Y'                                                     
         B     LBPB2                                                            
*                                                                               
*   CHECK TO SEE IF CLOSING THE BOX AS A RESULT OF NO MORE DATA                 
*    FOR PUB                                                                    
ENDDATA  L     R1,ABUYTAB                                                       
*                                                                               
         CLI   0(R1),X'99'       FINISHED WITH THIS BUY                         
         BNE   *+12                AM I AT END                                  
         LA    R1,6(R1)                                                         
         B     *-12                                                             
         CLI   0(R1),255           END                                          
         BR    RE                                                               
*                                                                               
DOBOXBB  CLI   DOBOXES,C'N'                                                     
         BE    DOBOXR                                                           
         L     RF,ABOX                                                          
         MVI   BOXREQ,C'B'         ASSUME IT WILL FIT                           
*                                                                               
DOBOXR   DS    0H                                                               
         GOTO1 REPORT                                                           
******** GOTO1 =V(MYREPT),DMCB,=C'F'                                            
*                                                                               
         B     LBPB2                                                            
*                                                                               
*                                                                               
LBPUB2   LH    RF,PBRCNT                                                        
         LA    RF,1(RF)                                                         
         STH   RF,PBRCNT                                                        
         LH    RF,EDITCNT                                                       
         LA    RF,1(RF)                                                         
         STH   RF,EDITCNT                                                       
*                                                                               
LBPUB3   DS    0H                                                               
         CLI   DOBOXES,C'N'                                                     
         BE    PUBOPTV                                                          
         L     RF,ABOX                                                          
         USING BOXD,RF                                                          
         MVI   BOXREQ,C'C'                                                      
GOTOREPG DS    0H                                                               
         GOTO1 REPORT                                                           
******** GOTO1 =V(MYREPT),DMCB,=C'G'                                            
         CLI   DOBOXES,C'N'                                                     
         BE    PUBOPTV                                                          
         BAS   R5,CLEARBOX                                                      
         L     RF,ABOX                                                          
         USING BOXD,RF                                                          
         MVI   BOXYORN,C'Y'        STOP  PRINTING BOXES                         
         DROP  RF                                                               
PUBOPTV  CLI   PUBOPT,C'V'                                                      
         BE    LBPUB4                                                           
         CLI   MODE,LBUYVEN                                                     
         BNE   LBPUB6                                                           
         CLI   EDITCNT+1,1                                                      
         BE    LBPUB4                                                           
         MVI   EDVENSW,C'E'                                                     
         BAS   RE,ENDPUB                                                        
LBPUB4   DS    0H                                                               
         MVI   EDVENSW,C'V'                                                     
         BAS   RE,ENDPUB                                                        
         XC    EDITCNT,EDITCNT                                                  
         B     LBPUB8                                                           
LBPUB6   DS    0H                                                               
         MVI   EDVENSW,C'E'                                                     
         BAS   RE,ENDPUB                                                        
LBPUB8   DS    0H                                                               
LBPUBX   MVC   GROSS(68),SVGROSS       RESTORE PROCBUY'S GROSS                  
         MVC   KEY,SVPPGKEY                                                     
         CLC   QSORT,SPACES                                                     
         BNE   LBPUBX5                                                          
         GOTO1 HIGH                  MUST RESTORE PPG SEQ READ                  
*                                                                               
LBPUBX5  L     R1,ABUYTAB                                                       
         ST    R1,ANXTBUY                                                       
         MVI   0(R1),X'FF'                                                      
         B     EXIT                                                             
         SPACE 3                                                                
ENDPUB   NTR1                                                                   
         SPACE 2                                                                
*                                                                               
ENDP1    DS    0H                                                               
         CLI   EDVENSW,C'E'                                                     
         BE    ENDP2                                                            
         CLI   EDITCNT+1,1                                                      
         BNH   ENDP2                                                            
         MVC   ATOTS,AVENTOTS                                                   
         L     RF,AVENTWS                                                       
         ST    RF,ATOTWDS                                                       
         B     LEVLO                                                            
*                                                                               
ENDP2    DS    0H                                                               
         MVC   ATOTS,APUBTOTS                                                   
         L     RF,AEDTTWS                                                       
         CLI   EDVENSW,C'E'                                                     
         BE    *+14                                                             
         MVC   ATOTS,AVENTOTS                                                   
         L     RF,AVENTWS                                                       
         ST    RF,ATOTWDS                                                       
         XC    ADESC,ADESC            DON'T REPRINT PUB NAME                    
         B     LEVEQ                                                            
         SPACE 3                                                                
LBMKT    DS    0H                  LAST FOR MARKET                              
         CLI   PAGOPT,C'M'                                                      
         BNE   *+8                                                              
         MVI   NEWPAGE,C'Y'                                                     
         MVC   ATOTS,AMKTTOTS                                                   
         CLI   LEVSW,C'M'                                                       
         BE    LBMKT2                                                           
         JIF   LEVSW,NE,C'P',EXIT,JUMP=N                                        
         CLI   PBRCNT+1,1                                                       
         BH    LBMKT2                                                           
*                                  IF ONLY ONE VEN FOR MKT                      
*                                  PRINT VEN MONTHLY $ NOW                      
         MVI   ALLOWLIN,4                                                       
         GOTO1 APRTALL,DMCB,0                                                   
         XC    PBRCNT,PBRCNT                                                    
         B     LEVLO4                                                           
LBMKT2   DS    0H                                                               
         L     RF,AMKTTWS                                                       
         ST    RF,ATOTWDS                                                       
         XC    PBRCNT,PBRCNT                                                    
         JIF   LEVSW,=,C'P',LEVLO,JUMP=N                                        
         JIF   LEVSW,NE,C'M',EXIT,JUMP=N                                        
         BAS   RE,SETMKT                                                        
         LA    RF,MKTN                                                          
         ST    RF,ADESC                                                         
         B     LEVEQ                                                            
         SPACE 3                                                                
LBDST    DS    0H                  LAST FOR DISTRICT                            
         CLI   QDIST,C' '                                                       
         BE    EXIT                                                             
         BAS   RE,SETDST                                                        
         MVC   ATOTS,ADSTTOTS                                                   
         MVC   W(NML*3),DSTN       DIST NAME                                    
         L     RF,ADSTTWS                                                       
         MVC   W+NML*3(54),0(RF)      **                                        
         LA    RF,W                                                             
         ST    RF,ATOTWDS                                                       
         MVI   NEWPAGE,C'Y'                                                     
         JIF   LEVSW,=,C'P',OR,C'M',LEVLO,JUMP=N                                
         CLI   PAGOPT,C'D'                                                      
         BE    *+8                                                              
         MVI   NEWPAGE,C'N'                                                     
         JIF   LEVSW,NE,C'D',EXIT,JUMP=N                                        
         LA    RF,DSTN                                                          
         ST    RF,ADESC                                                         
         B     LEVEQ                                                            
         SPACE 3                                                                
LBREG    DS    0H                  LAST FOR REGION                              
         CLI   QREGION,C' '                                                     
         BE    EXIT                                                             
         BAS   RE,SETREG                                                        
         MVC   ATOTS,AREGTOTS                                                   
         MVC   W(NML*3),REGN       REG NAME                                     
         L     RF,AREGTWS                                                       
         MVC   W+NML*3(54),0(RF)        **                                      
         LA    RF,W                                                             
         ST    RF,ATOTWDS                                                       
         MVI   NEWPAGE,C'Y'                                                     
         JIF   LEVSW,=,C'P',OR,C'M',OR,C'D',LEVLO,JUMP=N                        
         CLI   PAGOPT,C'R'                                                      
         BE    *+8                                                              
         MVI   NEWPAGE,C'N'                                                     
         JIF   LEVSW,NE,C'R',EXIT,JUMP=N                                        
         LA    RF,REGN                                                          
         ST    RF,ADESC                                                         
         B     LEVEQ                                                            
         SPACE 3                                                                
LBEST    DS    0H                  LAST FOR ESTIMATE                            
         MVI   RISW,C'R'           SHOW ONLY 'REAL' INS COUNTS                  
         CLI   QEST,C' '                                                        
         BE    EXIT                                                             
         CLI   QESTEND,C' '                                                     
         BNE   EXIT                                                             
*                                                                               
         MVC   ATOTS,AESTTOTS                                                   
         L     RF,AESTTWS                                                       
         ST    RF,ATOTWDS                                                       
         MVI   NEWPAGE,C'Y'                                                     
         JIF   LEVSW,=,C'P',OR,C'M',OR,C'R',OR,C'D',LEVLO,JUMP=N                
         CLI   PAGOPT,C'E'                                                      
         BE    *+8                                                              
         MVI   NEWPAGE,C'N'                                                     
         JIF   LEVSW,NE,C'E',EXIT,JUMP=N                                        
         BAS   RE,SETEST                                                        
         LA    RF,ESTN                                                          
         ST    RF,ADESC                                                         
         B     LEVEQ                                                            
         SPACE 3                                                                
LBPRD    DS    0H                  LAST FOR PRODUCT                             
         CLI   QPRODUCT,C' '                                                    
         BE    EXIT                                                             
         MVC   ATOTS,APRDTOTS                                                   
         L     RF,APRDTWS                                                       
         ST    RF,ATOTWDS                                                       
         MVI   NEWPAGE,C'Y'                                                     
         JIF   LEVSW,=,C'P',OR,C'M',OR,C'D',OR,C'R',OR,C'E',LEVLO,     +        
               JUMP=N                                                           
         CLI   PAGOPT,C'B'                                                      
         BE    *+8                                                              
         MVI   NEWPAGE,C'N'                                                     
         JIF   LEVSW,NE,C'B',EXIT,JUMP=N                                        
         BAS   RE,SETPRD                                                        
         LA    RF,PRDN                                                          
         ST    RF,ADESC                                                         
         B     LEVEQ                                                            
         SPACE 3                                                                
LBDIV    DS    0H                                                               
         CLI   QDIV,C' '                                                        
         BE    EXIT                                                             
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    LBDIV2                                                           
         CLI   QPRODUCT,C' '                                                    
         BNE   EXIT                                                             
LBDIV2   DS    0H                                                               
         MVC   ATOTS,ADIVTOTS                                                   
         L     RF,ADIVTWS                                                       
         ST    RF,ATOTWDS                                                       
         MVI   NEWPAGE,C'Y'                                                     
         JIF   LEVSW,NE,C'V',LEVLO,JUMP=N                                       
         CLI   PAGOPT,C'V'                                                      
         BE    *+8                                                              
         MVI   NEWPAGE,C'N'                                                     
         BAS   RE,SETDIV                                                        
         LA    RF,DIVN                                                          
         ST    RF,ADESC                                                         
         B     LEVEQ                                                            
         SPACE 3                                                                
LBCLT    DS    0H                  LAST FOR CLIENT                              
         CLI   QPRODUCT,C' '                                                    
         BE    LBCLT2                                                           
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   EXIT                                                             
LBCLT2   DS    0H                                                               
         CLC   QDIV,=C'ALL'                                                     
         BE    LBCLT4                                                           
         CLI   QDIV,C' '                                                        
         BNE   EXIT                                                             
LBCLT4   DS    0H                                                               
         XC    PPRDKEY(10),PPRDKEY       CLEAR PRODUCT FROM HEADLINES           
*                                                                               
         MVC   ATOTS,ACLTTOTS                                                   
         L     RF,ACLTTWS                                                       
         ST    RF,ATOTWDS                                                       
*                                                                               
         JIF   LEVSW,NE,C'C',LEVLO,JUMP=N                                       
         BAS   RE,SETCLI                                                        
         LA    RF,CLTN                                                          
         ST    RF,ADESC                                                         
         B     LEVEQ                                                            
         SPACE 3                                                                
LBREQ    DS    0H                  LAST FOR REQ                                 
         CLI   LEVSW,C'C'                                                       
         BNE   EXIT                                                             
         MVC   ATOTS,AREQTOTS                                                   
         L     RF,AREQTWS                                                       
         ST    RF,ATOTWDS                                                       
         BAS   RE,SETRPT                                                        
         LA    RF,RPTN                                                          
         ST    RF,ADESC                                                         
*                                                                               
         MVC   BSPARS(24),BSPARS2                                               
         MVI   TABSW,2                                                          
         B     LEVEQ1                                                           
         SPACE 3                                                                
LEVEQ    DS    0H                  LEVSW EQUAL TO MODE                          
         MVC   BSPARS(24),BSPARS1                                               
         MVI   TABSW,1                                                          
LEVEQ1   DS    0H                                                               
*****    OC    BSPARS+8(4),BSPARS+8     TEST ANY DATA                           
*****    BZ    EXIT                                                             
         CLI   SPCOPT,0                                                         
         BE    LEVEQ4                                                           
         GOTO1 APRTALL,DMCB,ADESC,SPACES,SPACES                                 
         GOTO1 AGETSPC                                                          
         B     LEVLO                                                            
*                                                                               
LEVEQ4   DS    0H                                                               
         GOTO1 AGETSPC                                                          
         MVI   ALLOWLIN,4                                                       
         GOTO1 APRTALL,DMCB,0      PRINT MONTHLY $                              
         B     LEVLO4                                                           
         SPACE 3                                                                
LEVLO    DS    0H                  LEVSW 'LOWER' THAN MODE                      
         CLI   ATOTS,0             NO TOTALS AT THIS LEVEL                      
         BE    LEVLO9                                                           
         L     RF,ATOTS                                                         
         OC    0(20,RF),0(RF)      NO $, NO INSERTIONS                          
         BZ    LEVLO9                                                           
         MVI   TOTSW,1                                                          
         GOTO1 ADATOUT,DMCB,ATOTS                                               
         MVI   TOTSW,1                                                          
         GOTO1 APRTALL,DMCB,ATOTWDS,ADTLINS,DOLLNS                              
LEVLO4   DS    0H                                                               
         CLI   NEWPAGE,C'Y'                                                     
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         LA    R3,APUBTOTS                                                      
         LA    R4,4                                                             
         LA    R5,ADDRSX-1                                                      
         CLC   1(3,R3),ATOTS+1                                                  
         BH    *+12                                                             
         BXLE  R3,R4,*-10                                                       
         B     LEVLO8                                                           
         CLI   0(R3),0                                                          
         BNE   LEVLO6                                                           
         BXLE  R3,R4,*-8                                                        
         B     LEVLO8                                                           
*                                                                               
LEVLO6   DS    0H                                                               
         MVC   DMCB+4(4),0(R3)                                                  
         GOTO1 AROLLUP,DMCB,ATOTS                                               
*                                                                               
LEVLO8   DS    0H                                                               
         L     RE,ATOTS                                                         
         LA    RF,TOTL                                                          
         XCEF                                                                   
*                                                                               
         MVC   ANXTTOT1,ASPCTOT1                                                
         MVC   ANXTCOM,ACOMTAB                                                  
         XC    BSPARS1+8(4),BSPARS1+8     RESET BSPARS                          
*                                                                               
         CLI   TABSW,2                                                          
         BNE   LEVLO9                                                           
         MVC   ANXTTOT2,ASPCTOT2                                                
         XC    BSPARS2+8(4),BSPARS2+8                                           
*                                                                               
*                                                                               
LEVLO9   DS    0H                                                               
         MVI   NEWPAGE,C'N'                                                     
         B     EXIT                                                             
LEVLO9A  GOTO1 STDCOMM                                                          
         MVI   NEWPAGE,C'N'                                                     
         CLI   QOPT5,C'Y'                                                       
         BNE   EXIT                                                             
****     MVI   CONTROLS,C'S'                                                    
****     MVI   RCSUBPRG,1                                                       
         GOTO1 ARECAP                                                           
         B     EXIT                                                             
         SPACE 2                                                                
CLEARBOX L     RE,ABOX                                                          
         USING BOXD,RE                                                          
         L     RF,=A(BOXSCORR+132-BOXYORN)                                      
         DROP  RE                                                               
         XCEF                                                                   
         BR    R5                                                               
         EJECT                                                                  
SETMKT   DS    0H                                                               
         MVC   MKTN,SPACES                                                      
         CLI   QMEDIA,C'O'                                                      
         BNE   SETMKT2                                                          
         MVC   MKTN(2),PUBSTACD    USE STACD                                    
         CLI   PUBSTACD,C' '       IF THERE                                     
         BNH   *+12                                                             
         CLI   PUBSTACD,C'0'       UNLESS NUMERIC                               
         BL    *+10                                                             
         MVC   MKTN(2),PUBSTATE                                                 
         MVI   MKTN+2,C','                                                      
         MVC   MKTN+4(20),PUBZNAME                                              
         BR    RE                                                               
SETMKT2  DS    0H                                                               
         MVC   MKTN(2),PUBSTATE                                                 
         MVI   MKTN+2,C','                                                      
         MVC   MKTN+4(16),PUBCITY                                               
         BR    RE                                                               
         SPACE 3                                                                
SETDST   DS    0H                                                               
         MVC   DSTN,SPACES                                                      
         CLI   QDIST,C' '                                                       
         BER   RE                                                               
         MVC   DSTN1(8),=C'DISTRICT'                                            
         MVC   DSTN1+9(3),PDSTKDST                                              
         MVC   DSTN2(20),PDSTNAME                                               
         MVC   DSTN3,MYDASH                                                     
         LA    RF,DSTN3-1                                                       
SETDST2  DS    0H                                                               
         CLI   0(RF),C' '                                                       
         BHR   RE                                                               
         MVI   NML(RF),C' '                                                     
         BCT   RF,SETDST2                                                       
         SPACE 3                                                                
SETREG   DS    0H                                                               
         MVC   REGN,SPACES                                                      
         CLI   QREGION,C' '                                                     
         BER   RE                                                               
         MVC   REGN1(6),=C'REGION'                                              
         MVC   REGN1+7(3),PREGKREG                                              
         MVC   REGN2(20),PREGNAME                                               
         MVC   REGN3,MYDASH                                                     
         LA    RF,REGN3-1                                                       
SETREG2  DS    0H                                                               
         CLI   0(RF),C' '                                                       
         BHR   RE                                                               
         MVI   NML(RF),C' '                                                     
         BCT   RF,SETREG2                                                       
         SPACE 3                                                                
SETEST   DS    0H                                                               
         MVC   ESTN,SPACES                                                      
         CLC   QEST,=C'ALL'                                                     
         BNER  RE                                                               
         MVC   ESTN1(8),=C'ESTIMATE'                                            
         MVC   HALF,PESTKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTN1+9(3),DUB                                                   
         MVC   ESTN2(20),PESTNAME                                               
         BR    RE                                                               
         SPACE 3                                                                
SETPRD   DS    0H                                                               
         MVC   PRDN,SPACES                                                      
         CLC   QPRODUCT,=C'ALL'                                                 
         BNER  RE                                                               
         MVC   PRDN1(7),=C'PRODUCT'                                             
         MVC   PRDN1+9(3),PPRDKPRD                                              
         MVC   PRDN2(20),PPRDNAME                                               
         BR    RE                                                               
         SPACE 3                                                                
SETDIV   DS    0H                                                               
         MVC   DIVN,SPACES                                                      
         CLC   QDIV,=C'ALL'                                                     
         BNER  RE                                                               
         MVC   DIVN1(8),=C'DIVISION'                                            
         MVC   DIVN1+9(3),PDIVKDIV                                              
         MVC   DIVN2(20),PDIVNAME                                               
         BR    RE                                                               
         SPACE 2                                                                
SETCLI   DS    0H                                                               
         MVC   CLTN,SPACES                                                      
         MVC   CLTN1(6),=C'CLIENT'                                              
         MVC   CLTN1+9(3),PCLTKCLT                                              
         MVC   CLTN2(20),PCLTNAME                                               
         BR    RE                                                               
         SPACE 2                                                                
SETRPT   DS    0H                                                               
         MVC   RPTN,SPACES                                                      
         MVC   RPTN1(06),=C'REPORT'                                             
         MVC   RPTN2(06),MYDASH                                                 
         BR    RE                                                               
         SPACE 3                                                                
         EJECT                                                                  
*                                  FORMAT SPACES,DATE,+DOLLARS                  
         SPACE 2                                                                
GETSPCE  NTR1                                                                   
         SPACE 2                                                                
         USING STSD,R3                                                          
         L     R3,BSPARS+4                                                      
         L     R4,BSPARS+8                                                      
         LTR   R4,R4                                                            
         BZ    GSX                                                              
GS2      DS    0H                                                               
         L     R5,ADESC                                                         
         CLI   SPCOPT,0                                                         
         BE    GS4                                                              
GS4      DS    0H                                                               
         MVI   TOTSW,0                                                          
         MVC   DMCB(4),STADDR                                                   
         MVI   DMCB,C'D'           DATES, NOT COUNTS                            
         GOTO1 ADATOUT,DMCB                                                     
         MVC   DMCB(4),STADDR                                                   
         GOTO1 APRTALL,DMCB,(R5),ADTLINS,DOLLNS                                 
         CLI   TABSW,2             DONT TOTAL HERE IS 2(D TAB                   
         BE    GS5                                                              
         MVC   DMCB(4),STADDR                                                   
         GOTO1 AROLLUP,DMCB,,ATOTS                                              
GS5      DS    0H                                                               
         A     R3,BSPARS+12                                                     
         BCT   R4,GS2                                                           
*                                                                               
GSX      DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
ROLLUP   NTR1                                                                   
         SPACE 2                                                                
         LM    R1,R2,0(R1)                                                      
         LM    R3,R6,0(R1)                                                      
         A     R3,00(R2)                                                        
         A     R4,04(R2)                                                        
         A     R5,08(R2)                                                        
         A     R6,12(R2)                                                        
         STM   R3,R6,0(R2)                                                      
         LH    RF,16(R1)                                                        
         AH    RF,16(R2)                                                        
         STH   RF,16(R2)                                                        
         LH    RF,18(R1)                                                        
         AH    RF,18(R2)                                                        
         STH   RF,18(R2)                                                        
         LA    R0,MAXMOS                                                        
         LA    R1,24(R1)                                                        
         LA    R2,24(R2)                                                        
RU2      DS    0H                                                               
         LH    RF,32(R1)                                                        
         AH    RF,32(R2)                                                        
         STH   RF,32(R2)                                                        
         LH    RF,34(R1)                                                        
         AH    RF,34(R2)                                                        
         STH   RF,34(R2)                                                        
         LA    R1,36(R1)                                                        
         LA    R2,36(R2)                                                        
         BCT   R0,RU2                                                           
RUX      DS    0H                                                               
         XIT                                                                    
*                                                                               
*                                  FORMAT $ AMTS                                
JUSTNOW  DC    X'0'                                                             
SVPPGKEY DS     CL32                                                            
         EJECT                                                                  
AEDTTWS   DC   A(EDTTWS)                                                        
AVENTWS   DC   A(VENTWS)                                                        
AMKTTWS   DC   A(MKTTWS)                                                        
ADSTTWS   DC   A(DSTTWS)                                                        
AREGTWS   DC   A(REGTWS)                                                        
AESTTWS   DC   A(ESTTWS)                                                        
APRDTWS   DC   A(PRDTWS)                                                        
ADIVTWS   DC   A(DIVTWS)                                                        
ACLTTWS   DC   A(CLTTWS)                                                        
AREQTWS   DC   A(REQTWS)                                                        
         LTORG                                                                  
         SPACE 3                                                                
EDTTWS   DC    CL54'**EDITION TOTAL**'                                          
VENTWS   DC    CL54'**VENDOR TOTAL**'                                           
MKTTWS   DC    CL54'**MARKET TOTAL**'                                           
DSTTWS   DC    CL54'**DISTRICT TOTAL**'                                         
REGTWS   DC    CL54'**REGION TOTAL**'                                           
ESTTWS   DC    CL54'**ESTIMATE TOTAL**'                                         
PRDTWS   DC    CL54'**PRODUCT TOTAL**'                                          
DIVTWS   DC    CL54'**DIVISION TOTAL**'                                         
CLTTWS   DC    CL54'**CLIENT TOTAL**'                                           
REQTWS   DC    CL54'**REPORT TOTAL**'                                           
         SPACE 3                                                                
         EJECT                                                                  
*                                  ROUTINE TO SET REQTAB FROM PROFILE           
MYREPT   CSECT                                                                  
         NMOD1 0,MREPT                                                          
*                                                                               
         L     R8,ABOX                                                          
         L     R1,0(R1)                                                         
         MVC   HINT+4(1),0(R1)                                                  
         MVI   HINT+5,C'B'                                                      
         GOTO1 =V(PRNTBL),DMCB,(6,HINT),(R8),C'DUMP',900,=C'1D'                 
         CP    DUMPCNT,=P'18'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CP    DUMPCNT,=P'500'                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
         AP    DUMPCNT,=P'1'                                                    
         GOTO1 =V(PRNTBL),DMCB,(6,PRINT),P1,C'DUMP',396,=C'1D'                  
         GOTO1 REPORT                                                           
         L     R8,ABOX                                                          
         MVI   HINT+5,C'A'                                                      
         GOTO1 =V(PRNTBL),DMCB,(6,HINT),(R8),C'DUMP',984,=C'1D'                 
         XIT                                                                    
DUMPCNT  DC    PL3'1'                                                           
         LTORG                                                                  
HINT     DC    C'DUMP  '                                                        
         DROP  R7                                                               
         EJECT                                                                  
STDCOMM  CSECT                                                                  
         NMOD1 0,STCOMM                                                         
         L     RC,PPFILEC                                                       
         L     R6,=A(STDCOMSV)     BEGINING OF ST COMMENT                       
TLCOMM   DS    0H                PRINT REQUESTED STANDARD COMMENT               
         MVI   FORCEHED,C'N'     MIGHT HAVE BEEN SET TO "Y" IN TLOUT            
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         C     R6,AOFSTCOM         CHECK TO SEE IF USED                         
         BE    NOMULTC             NO MULTIPLE STANDARD COMMENTS                
         MVI   MULTSTD,C'Y'                                                     
         CLC   Q2USER(6),SPACES       WAS STANDARD COMMENT REQSTD               
         BH    NOMULTC                PROCESS STD COMM FIRST                    
NORQCOMM MVI   CONTROLS,C'S'       PROCESSING STANDARD COMMENTS                 
         MVI   FORCEHED,C'Y'                                                    
         L     RF,PPWORK2C                                                      
TLCOMMA  CLI   0(R6),255           END                                          
         BE    TLCOMMX                                                          
         MVC   Q2USER(6),0(R6)                                                  
*                                                                               
NOMULTC  CLC   Q2USER(6),SPACES       WAS STANDARD COMMENT REQSTD               
         BNH   TLCOMMX                                                          
*                                                                               
*                                                                               
         BAS   RE,TLPRT                 SKIP A LINE                             
         L     RF,PPWORK2C                                                      
         LA    R4,P                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY                                                   
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),Q2USER         STD COMMENT RIGHT ALIGNED                
*                                                                               
         DROP  RF                                                               
*                                                                               
         USING PPWORK2D,R7                                                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+8                                                              
         B     TRTC30            COMMENT NOT FOUND                              
*NOP*    LA    R0,PCONREC                                                       
         L     R0,ACONIO1          (A)PCONREC                                   
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         MVI   LINENEED,0                                                       
         LA    R4,0                                                             
         L     R2,ACONIO1          (A)PCONREC                                   
*NOP*    LA    R2,PCONREC+33                                                    
         LA    R2,33(R2)                                                        
         MVI   ELCODE,X'40'                                                     
         CLI   0(R2),X'40'                                                      
         BE    *+8                                                              
TLOOP    BAS   RE,NEXTEL                                                        
         BNE   TRTC13                                                           
         LA    R4,1(R4)                                                         
         B     TLOOP                                                            
TRTC13   STC   R4,LINENEED                                                      
*                                                                               
         LA    R4,P1             FOR PRINT LINE                                 
         CLI   CONTROLS,C'S'                                                    
         BNE   *+10                                                             
         MVC   1(6,R4),Q2USER                                                   
*                                                                               
         L     R2,ACONIO1          (A)PCONREC                                   
*NOP*    LA    R2,PCONREC+33                                                    
         LA    R2,33(R2)                                                        
         MVI   ELCOD,X'40'                                                      
         CLI   0(R2),X'40'                                                      
         BE    TRTC15                                                           
TRTC12   BAS   RE,NEXTEL                                                        
         BNE   TRTC30                                                           
TRTC15   ZIC   R5,1(R2)                                                         
         LA    R3,2(R2)                                                         
         CLI   2(R2),C'+'                                                       
         BNE   TRTC17                                                           
         MVC   SPACING,3(R2)                                                    
         NI    SPACING,X'0F'                                                    
         CLI   SPACING,3                                                        
         BNH   *+8                                                              
         MVI   SPACING,3                                                        
         CLI   SPACING,0                                                        
         BNE   *+8                                                              
         MVI   SPACING,1                                                        
         MVI   0(R4),0                                                          
         BAS   RE,TLPRT                                                         
         SH    R5,=H'2'        FOR '+' AND NUMBER OF LINES                      
         LA    R3,4(R2)                                                         
*                                                                               
TRTC17   SH    R5,=H'3'        FOR ELEM CODE+LENGHT +1 FOR EXECUTE              
         BM    TRTC18          NOTHING TO PRINT SO PRINT BLANK LINE             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   10(0,R4),0(R3)                                                   
*        B     *+8                                                              
TRTC18   MVI   0(R4),0                                                          
         BAS   RE,TLPRT                                                         
         B     TRTC12                                                           
*                                                                               
TRTC30   DS    0H                                                               
         CLI   MULTSTD,C'Y'                                                     
         BNE   TLCOMMX                                                          
         CLI   CONTROLS,C'S'       PROCESSED STANDARD COMMENT FIRST             
         BNE   NORQCOMM                                                         
         LA    R6,6(R6)            BUMP TO NEXT                                 
         L     RF,PPWORK2C                                                      
         B     TLCOMMA                                                          
TLCOMMX  DS    0H                                                               
******************************************                                      
*                                                                               
         MVI   CONTROLS,0          INITIAL                                      
         MVI   MULTSTD,0           INITIAL                                      
XIT      XIT1                                                                   
         SPACE 3                                                                
*                                  PRINTING                                     
TLPRT    NTR1                                                                   
         SPACE 2                                                                
         BAS   RE,TLCKHD                                                        
*                                                                               
TLPRT2   DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVI   LINENEED,0                                                       
         B     XIT                                                              
*                                                                               
         SPACE 3                                                                
TLCKHD   DS    0H                                                               
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         IC    R0,LINE                                                          
         IC    RF,LINENEED                                                      
         AR    R0,RF                                                            
         STC   R0,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BLR   RE                                                               
         MVI   FORCEHED,C'Y'                                                    
         BR    RE                                                               
         SPACE 3                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCOD,0(R2)                                                      
         BER   RE                                                               
         B     NEXTEL+2                                                         
NEXTEL2  LTR   RE,RE                                                            
         BR    RE                                                               
         LTORG                                                                  
         TITLE 'TLHEAD PRINT HEADLINES'                                         
*                                                                               
BLDREQT  CSECT                                                                  
         NMOD1 0,BLDREQT                                                        
         L     RC,PPFILEC                                                       
*                                                                               
*       MUST SET OPTIONS FROM PROFILE AT FBUYCLT                                
*       SINCE PPG ONLY READS IT THEN                                            
*                                                                               
         CLI   QOPT4,C' '               SEE IF COST SPECIFIED                   
         BNE   *+8                                                              
         MVI   QOPT4,C'G'               DEFAULT TO GROSS                        
*                                                                               
         OC    PROGPROF,PROGPROF        TEST PROFILE PRESENT                    
         BNZ   BR5B                                                             
         MVC   PROGPROF(5),=C'NNNNN'   SET DEFAULTS                             
         MVC   PROGPROF+5(9),=C'NNNNNNNNN'    SET DEFAULTS                      
*                                  SET PROF OPTS IN OLD POSITIONS               
BR5B     MVI   PROF+30,C'S'        SUPPRESS                                     
         CLI   PROGPROF+1,C'N'                                                  
         BE    BR5D                                                             
         MVC   PROF+30(1),PROGPROF+1                                            
BR5D     DS    0H                                                               
         MVI   PROF+27,C'0'        VENDOR/EDIT TOTALS                           
         CLI   PROGPROF+0,C'N'                                                  
         BE    *+8                                                              
         MVI   PROF+27,C'V'                                                     
*                                                                               
         CLI   PROGPROF+2,C'Y'     TEST SPECIAL PAGE SKIPPING                   
         BNE   BR5F                                                             
         MVI   PAGOPT,C'P'         PUB                                          
         CLC   QSORT,=C'07'                                                     
         BNE   *+8                                                              
         MVI   PAGOPT,C'M'                                                      
BR5F     DS    0H                                                               
         B     BR6                                                              
*                                                                               
BR6      DS    0H                                                               
         MVC   FREQOPT,PROGPROF+3  N= NO FREQUENCY                              
         CLI   DOLLOPT,C'S'                                                     
         BE    *+10                                                             
         MVC   DOLLOPT,PROF+19          $                                       
         MVI   PUBOPT,C'V'         ONLY VENDOR TOTALS                           
         CLI   PROF+27,C'0'                                                     
         BE    BR8                                                              
         CLI   PROF+27,C'T'                                                     
         BE    BR8                                                              
         MVI   PUBOPT,C'E'         EDIT + VENDOR TOTALS                         
BR8      DS    0H                                                               
         MVI   JOBOPT,0                                                         
         CLI   SPCOPT,0                                                         
         BE    BR10                                                             
BR10     DS    0H                                                               
         MVI   CIRCOPT,0                                                        
         CLI   PROF+30,C'0'                                                     
         BE    *+10                                                             
         MVC   CIRCOPT,PROF+30                                                  
*                                                                               
BR10X    DS    0H                                                               
*                                                                               
         MVI   DOBOXES,C'N'                                                     
         CLI   PROGPROF+4,C'Y'     PRINT BOXES                                  
         BNE   *+8                                                              
         MVI   DOBOXES,C'Y'                                                     
         XC    REQTAB,REQTAB                                                    
         LA    R1,REQTAB                                                        
         MVI   0(R1),X'01'         ALWAYS START WITH INS DATE                   
         LA    R1,1(R1)                                                         
         CLI   PROGPROF+5,C'N'                                                  
         BE    BLDR5                                                            
         MVI   0(R1),X'31'         SPACE                                        
         LA    R1,1(R1)                                                         
*                                                                               
BLDR5    CLI   PROGPROF+6,C'N'                                                  
         BE    BLDR10                                                           
         MVI   0(R1),X'20'         PRODUCT                                      
         CLI   PROGPROF+6,C'P'     PRODUCT ONLY                                 
         BE    BLDR8                                                            
         MVI   0(R1),X'21'         ESTIMATE                                     
         CLI   PROGPROF+6,C'E'     ESTIMATE ONLY                                
         BE    BLDR8                                                            
         MVI   0(R1),X'22'         BOTH PRD + EST                               
         CLI   PROGPROF+6,C'B'                                                  
         BE    BLDR8                                                            
BLDR8    LA    R1,1(R1)                                                         
BLDR10   LA    R4,PROGPROF+7       REST ARE N,Y                                 
         LA    R5,7                FOR BCT WAS 6                                
         LA    R6,RTABLE                                                        
BLDR15   CLI   0(R4),C'N'                                                       
         BE    BLDR20                                                           
         MVC   0(1,R1),0(R6)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
BLDR20   LA    R4,1(R4)                                                         
         LA    R6,1(R6)                                                         
         BCT   R5,BLDR15                                                        
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         CLI   QOPT4,C'G'                                                       
         BE    BLDR40                                                           
         LA    R4,REQTAB           FIND COST BYTE                               
BLDR25   CLI   0(R4),X'10'                                                      
         BE    BLDR30                                                           
         LA    R4,1(R4)                                                         
         CLI   0(R4),0             SEE IF AT END OF TABLE                       
         BNE   BLDR25                                                           
         B     BLDR40                                                           
*                                                                               
BLDR30   MVI   0(R4),X'12'         NET                                          
         CLI   QOPT4,C'N'                                                       
         BE    BLDR40                                                           
         MVI   0(R4),X'14'         CD                                           
         CLI   QOPT4,C'C'                                                       
         BE    BLDR40                                                           
         MVI   0(R4),X'11'         G-CD                                         
         CLI   QOPT4,C'1'                                                       
         BE    BLDR40                                                           
         MVI   0(R4),X'13'         N-CD                                         
         CLI   QOPT4,C'2'                                                       
         BE    BLDR40                                                           
         MVI   0(R4),X'16'         CLIENT COST                                  
         CLI   QOPT4,C'T'                                                       
         BE    BLDR40                                                           
*                                                                               
BLDR40   SR    R1,R1                                                            
         LA    R2,REQTAB           COUNT NUMBER OF DATA LINES                   
BLDR43   CLI   0(R2),0                                                          
         BE    BLDR45                                                           
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         B     BLDR43                                                           
*                                                                               
BLDR45   LA    R1,1(R1)           FOR BOTTOM LINE OF DETAILS                    
         STC   R1,DATANUM                                                       
*                                                                               
         MVI   MTHOPT,C'Y'         SHOW PUB/VEN MONTHLY TOTLAS                  
         CLI   PROGPROF+15,C'N'                                                 
         BE    *+10                                                             
         MVC   MTHOPT,PROGPROF+15  SUPPRESS PUB/VEN MONTHLY TOTLAS              
*                                  $= DOLLARS, I=INS, B= BOTH                   
*                                                                               
*            FIRST TRY TO READ B2 PROFILE (NEEDED FOR GETCOST)                  
         XC    B2PROF,B2PROF                                                    
         XC    WORK,WORK                                                        
         MVC   WORK(12),=CL12'P000'                                             
         MVC   WORK+2(2),=C'B2'                                                 
         MVC   WORK+4(2),QAGENCY                                                
         MVC   WORK+6(1),QMEDIA                                                 
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         GOTO1 GETPROF,DMCB,WORK,B2PROF,DATAMGR                                 
         MVC   TAXOPT,B2PROF+12                                                 
         CLI   TAXOPT,0                                                         
         BNE   *+8                                                              
         MVI   TAXOPT,C'N'                                                      
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
PATCH1   DC    XL20'00'                                                         
*                                                                               
*                                  REQTAB ENTRIES FOR PROGPROF+7(6)             
RTABLE   DC    X'10'               COST                                         
         DC    X'30'               AD CODE                                      
         DC    X'02'               BILLABLE DATE                                
         DC    X'03'               PAYABLE DATE                                 
         DC    X'04'               CLOSING DATE                                 
         DC    X'05'               ON-SALE DATE                                 
         DC    X'32'               PCOM                                         
         DC    X'00'                                                            
         EJECT                                                                  
FORMAT   CSECT                                                                  
         NMOD1 0,FORMAT                                                         
         L     RC,PPFILEC                                                       
         L     R6,AMYBUY                                                        
         USING PBUYREC,R6                                                       
         OC    FULL,FULL                                                        
         BZ    FORM50              GO DO ANNOTATION                             
*                                                                               
FORM20   LA    R5,REQTAB                                                        
         L     R4,FULL             IS DISPLACED INTO PRINT LINE                 
FORM25   LA    R3,DATATAB                                                       
FORM27   CLI   0(R5),0             SEE IF END OF REQTAB                         
         BE    FORMATX             DONE                                         
FORM28   CLC   0(1,R5),0(R3)                                                    
         BE    FORM30                                                           
         LA    R3,28(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   FORM28                                                           
         DC    H'0'                FATAL ERROR                                  
*                                                                               
FORM30   L     RF,4(R3)                                                         
         BASR  RE,RF                                                            
         LA    R4,132(R4)                                                       
         LA    R5,1(R5)                                                         
         B     FORM25                                                           
*                                                                               
FORM50   LA    R5,REQTAB                                                        
         LA    R4,P1                                                            
FORM55   LA    R3,DATATAB                                                       
FORM57   CLI   0(R5),0             SEE IF END OF REQTAB                         
         BE    FORMATX             DONE                                         
FORM58   CLC   0(1,R5),0(R3)                                                    
         BE    FORM60                                                           
         LA    R3,28(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   FORM58                                                           
         DC    H'0'                FATAL ERROR                                  
*                                                                               
FORM60   MVC   0(20,R4),8(R3)                                                   
         CLI   QMEDIA,C'O'         FOR OUTDOOR CHANGE TO POSTING DATE           
         BNE   FORM65                                                           
         CLI   0(R3),X'01'                                                      
         BNE   FORM65                                                           
         MVC   0(20,R4),=CL20'POSTING DATE-'                                    
FORM65   LA    R4,132(R4)                                                       
         LA    R5,1(R5)                                                         
         B     FORM55                                                           
*                                                                               
FORMATX  XIT1                                                                   
*                                                                               
FINSD    ST    RE,SAVERE           INSERTION DATE                               
*****                                                                           
         MVI   TBUYSW,0                                                         
         CLI   PBDBFD,C'T'                                                      
         BNE   FINSD3                                                           
         MVI   TBUYSW,C'Y'                                                      
*****                                                                           
FINSD3   LA    R2,PBUYKDAT                                                      
         MVI   MYCHG1,0                                                         
         MVI   MYCHG2,0                                                         
         MVI   MYCHG3,0                                                         
         MVI   MYCOSTC,0                                                        
*                                                                               
         CLI   CHGCDAT,0           CHK FOR CHG CONTROL DATE                     
         BE    PUTDATE                                                          
*                                                                               
         ST    RE,SAVERE                                                        
         BAS   RE,GETCHGS      ORS INTO MYCHG1,2,3  X'24' ELEM DATA             
         L     RE,SAVERE                                                        
         LA    R2,PBUYKDAT         MUST RESET R2                                
*                                                                               
         TM    PBUYCNTL,X'80'      CHK FOR DELETE                               
         BZ    FINSD5                                                           
         MVI   7(R4),C'D'                                                       
         B     PUTDATE                                                          
*                                                                               
FINSD5   CLC   PBDBUYDT,CHGCDAT     CHK FOR ADD                                 
         BL    FINSD10                                                          
         MVI   7(R4),C'A'                                                       
         B     PUTDATE                                                          
*                                                                               
FINSD10  TM    MYCHG1,X'08'        DATE CHANGE                                  
         BNO   PUTDATE                                                          
         MVI   7(R4),C'C'                                                       
         B     PUTDATE                                                          
*                                                                               
FBILD    ST    RE,SAVERE           BILLABLE DATE                                
*****                                                                           
         MVI   TBUYSW,0                                                         
         TM    MYCHG2,X'20'        CHANGE                                       
         BNO   *+8                                                              
         MVI   7(R4),C'*'                                                       
         LA    R2,PBDBDATE                                                      
         B     PUTDATE                                                          
*                                                                               
FPAYD    ST    RE,SAVERE           PAYABLE DATE                                 
*****                                                                           
         MVI   TBUYSW,0                                                         
         TM    MYCHG2,X'10'        CHANGE                                       
         BNO   *+8                                                              
         MVI   7(R4),C'*'                                                       
         LA    R2,PBDPDATE                                                      
         B     PUTDATE                                                          
*                                                                               
FCLOD    ST    RE,SAVERE           CLOSING DATE                                 
*****                                                                           
         MVI   TBUYSW,0                                                         
         TM    MYCHG2,X'80'        CHANGE                                       
         BNO   *+8                                                              
         MVI   7(R4),C'*'                                                       
         LA    R2,PBDCDATE                                                      
         B     PUTDATE                                                          
*                                                                               
FOSLD    ST    RE,SAVERE           ON-SALE DATE                                 
*****                                                                           
         MVI   TBUYSW,0                                                         
         TM    MYCHG2,X'40'        CHANGE                                       
         BNO   *+8                                                              
         MVI   7(R4),C'*'                                                       
         LA    R2,PBDSDATE                                                      
         B     PUTDATE                                                          
*                                                                               
PUTDATE  OC    0(3,R2),0(R2)                                                    
         BZ    PUTDX                                                            
*****                                                                           
         CLI   TBUYSW,C'Y'       IF IT IS A TEST BUY AND INSERTION DATE         
         BNE   PUTDTES           IS BEING PRINTED TBUYSW=Y                      
         MVI   1(R4),C'T'        SO INDICATE A TEST BUY                         
*****                                                                           
PUTDTES  GOTO1 DATCON,DMCB,(3,0(R2)),(4,2(R4))                                  
PUTDX    L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
FGROSS   L     R2,GROSS                                                         
         B     PUTCOST                                                          
*                                                                               
FGRLCD   L     R2,GROSS                                                         
         S     R2,CSHDSC                                                        
         B     PUTCOST                                                          
*                                                                               
FNET     L     R2,GROSS                                                         
         S     R2,AGYCOM                                                        
         B     PUTCOST                                                          
*                                                                               
FNLCD    L     R2,GROSS                                                         
         S     R2,AGYCOM                                                        
         S     R2,CSHDSC                                                        
         B     PUTCOST                                                          
*                                                                               
FCSHD    L     R2,CSHDSC                                                        
         B     PUTCOST                                                          
*                                                                               
FCOST    DS    0H                                                               
         ST    RE,SAVERE                                                        
         MVC   WORK(4),GROSS                                                    
         MVC   WORK+4(4),CSHDSC                                                 
         MVC   WORK+8(4),AGYCOM                                                 
         CLI   TAXOPT,C'N'                                                      
         BE    FCOST5                                                           
         GOTO1 AGETCOST,DMCB,(C'T',BILBASA),WORK,PBUYREC                        
         B     FCOST5X                                                          
*                                                                               
FCOST5   DS    0H                                                               
         GOTO1 AGETCOST,DMCB,BILBASA,WORK,PBUYREC                               
FCOST5X  MVC   MYCOST,DMCB+4                                                    
         L     RE,SAVERE                                                        
         L     R2,DMCB+4                                                        
         B     PUTCOST                                                          
*                                                                               
PUTCOST  LR    R0,R2                                                            
         MVC   5(2,R4),=C'$0'      FOR ZERO COST BUYS                           
         LTR   R0,R0                                                            
         BZ    PUTCOSTX                                                         
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LR    R9,R1                                                            
         C     R9,=F'99999'                                                     
         BH    PUTC10                                                           
         C     R9,=F'0'        SEE IF MINUS                                     
         BNL   PUTC5                                                            
         C     R9,=F'-99999'                                                    
         BL    PUTC10                                                           
         EDIT  (R9),(7,0(R4)),FLOAT=-,COMMAS=YES                                
         B     PUTCOSTX                                                         
*                                                                               
PUTC5    EDIT  (R9),(7,0(R4)),FLOAT=$,COMMAS=YES                                
         B     PUTCOSTX                                                         
*                                                                               
PUTC10   C     R9,=F'999999'                                                    
         BH    PUTC20                                                           
         C     R9,=F'0'       SEE IF MINUS                                      
         BNL   PUTC15                                                           
         C     R9,=F'-999999'                                                   
         BL    PUTC20                                                           
         EDIT  (R9),(7,0(R4)),FLOAT=-                                           
         B     PUTCOSTX                                                         
*                                                                               
PUTC15   EDIT  (R9),(7,0(R4)),FLOAT=$                                           
         B     PUTCOSTX                                                         
*                                                                               
PUTC20   EDIT  (R9),(7,0(R4)),0                                                 
PUTCOSTX DS    0H                                                               
         TM    MYCOSTC,X'01'                                                    
         BNO   *+8                                                              
         MVI   7(R4),C'*'                                                       
*                                                                               
         BR    RE                  RETURN                                       
*                                                                               
FPRD     MVC   4(3,R4),PBUYKPRD                                                 
         BR    RE                                                               
*                                                                               
FEST     MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R4),DUB                                                      
         BR    RE                                                               
*                                                                               
FPEST    MVC   0(3,R4),PBUYKPRD    PRODUCT/ESTIMATE                             
         CLI   PBUYKPRD+2,C' '     SEE IF 2 CHAR PRD                            
         BNE   FPEST5                                                           
         MVI   0(R4),C' '                                                       
         MVC   1(2,R4),PBUYKPRD                                                 
*                                                                               
FPEST5   DS    0H                                                               
         MVI   3(R4),C'/'                                                       
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R4),DUB                                                      
         BR    RE                                                               
*                                                                               
FJOB     ST    RE,SAVERE                                                        
         MVC   X(6),PBDJOB         RIGHT JUSTIFY JOB                            
         GOTO1 ARIGHT,DMCB,X,6                                                  
         MVC   1(6,R4),X                                                        
         TM    MYCHG2,X'08'        JOB CHANGED                                  
         BNO   *+8                                                              
         MVI   7(R4),C'*'                                                       
         TM    MYCHG3,X'40'        JOB ADDED                                    
         BNO   *+8                                                              
         MVI   7(R4),C'*'                                                       
         L     RE,SAVERE           RESTORE RETURN REGISTER                      
         BR    RE                                                               
*                                                                               
FSPACE   ST    RE,SAVERE                                                        
         XC    X(20),X                                                          
         CLI   PBUYKMED,C'N'                                                    
         BE    FSPC20                                                           
         CLI   PBUYKMED,C'O'                                                    
         BNE   FSPC2                                                            
         CLI   PBDSPACE,X'FF'                                                   
         BNE   FSPC2                                                            
         MVC   X(2),=C'S='         DISPLAY SHOWING                              
         CLC   PBDSPACE+1(3),=C'SPC'                                            
         BNE   FSPC1                                                            
         MVC   X+2(3),PBDSPACE+1                                                
         B     FSPC50                                                           
*                                                                               
FSPC1    OI    PBDSPACE+3,X'0F'                                                 
         UNPK  X+2(3),PBDSPACE+1(3)                                             
         B     FSPC50              GO RIGHT ALIGN                               
*                                                                               
FSPC2    OC    PBDSPACE(17),SPACES                                              
         CLC   PBDSPACE+7(10),SPACES                                            
         BNE   FSPC5                                                            
         MVC   X(7),PBDSPACE                                                    
         B     FSPC50              GO RIGHT ALIGN                               
*                                                                               
FSPC5    LA    R0,17               FOR BCT                                      
         LA    R1,X                                                             
         LA    R2,PBDSPACE                                                      
FSPC10   CLI   0(R2),C' '                                                       
         BE    FSPC15                                                           
         MVC   0(1,R1),0(R2)       MOVE ALL NON-BLANKS                          
         LA    R1,1(R1)                                                         
FSPC15   LA    R2,1(R2)                                                         
         BCT   R0,FSPC10                                                        
*                                                                               
         B     FSPC50              GO RIGHT ALIGN                               
*                                                                               
FSPC20   OC    PBDSPACE,SPACES                                                  
         CLC   PBDSPACE,SPACES                                                  
         BNE   FSPC2               TREAT LIKE MAGS                              
         CLI   PBDUIND,C'L'                                                     
         BNE   FSPC30                                                           
         MVI   X+6,C'L'                                                         
FSPC25   EDIT  (P3,PBDUNITS),(6,X),COMMAS=YES                                   
         B     FSPC50                                                           
*                                                                               
FSPC30   CLI   PBDUIND,C'I'                                                     
         BNE   FSPC40                                                           
         MVI   X+6,C'I'                                                         
         B     FSPC25                                                           
*                                                                               
FSPC40   CLI   PBDUIND,X'89'    LOWER CASE I - INCHES WITH 2 DECIMALS           
         BNE   FSPCX                                                            
         EDIT  (P3,PBDUNITS),(6,X),2                                            
         MVI   X+6,C'I'                                                         
         B     FSPC50                                                           
*                                                                               
FSPC50   GOTO1 ARIGHT,DMCB,X,7                                                  
         MVC   0(7,R4),X                                                        
         TM    MYCHG1,X'30'      UNITS OR SPACE CHANGE                          
         BZ    *+8                                                              
         MVI   7(R4),C'*'                                                       
FSPCX    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
FPCOMM   NTR1                                                                   
         L     R6,AMYBUY                                                        
         LA    R2,33(R6)         BUMP TO FIRST ELEMENT                          
         MVI   ELCOD,X'68'       ELEMENT                                        
FPCOMGET BAS   RE,PPNXTEL                                                       
         BNE   FPXIT                                                            
         CLC   2(4,R2),=C'COM='                                                 
         BNE   FPCOMGET                                                         
         XC    X(10),X                                                          
         ZIC   RF,1(R2)            LENGTH OF ELEMENT                            
         SH    RF,=H'7'            ID,LEN,COM=,-1                               
         BNP   FPCOMGET                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   X(0),6(R2)                                                       
         GOTO1 ARIGHT,DMCB,X,6                                                  
         MVC   1(6,R4),X                                                        
         LA    RF,X                                                             
         ST    RF,BINPARS                                                       
         MVI   BINPARS,1          ADD TO TABLE IF NOT THERE                     
         L     RF,=V(BINSRCH)                                                   
         LA    R1,BINPARS                                                       
         BASR  RE,RF                                                            
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'               TABLE FULL                                    
         L     RF,=A(STDCOMSV)    ACT AS SWITCH                                 
         LA    RF,6(RF)                                                         
         ST    RF,AOFSTCOM                                                      
FPXIT    XIT                                                                    
         DS    0F                  FOR ALIGNMENT                                
DATATAB  DC    X'01',XL3'00',AL4(FINSD),CL20'INSERTION DATE-'                   
         DC    X'02',XL3'00',AL4(FBILD),CL20'BILLABLE DATE-'                    
         DC    X'03',XL3'00',AL4(FPAYD),CL20'PAYABLE DATE-'                     
         DC    X'04',XL3'00',AL4(FCLOD),CL20'CLOSING DATE-'                     
         DC    X'05',XL3'00',AL4(FOSLD),CL20'ON-SALE DATE-'                     
*                                                                               
         DC    X'10',XL3'00',AL4(FGROSS),CL20'GROSS COST-'                      
         DC    X'11',XL3'00',AL4(FGRLCD),CL20'GROSS LESS CD-'                   
         DC    X'12',XL3'00',AL4(FNET),CL20'NET COST-'                          
         DC    X'13',XL3'00',AL4(FNLCD),CL20'NET LESS CD-'                      
         DC    X'14',XL3'00',AL4(FCSHD),CL20'CASH DISC.-'                       
         DC    X'16',XL3'00',AL4(FCOST),CL20'CLIENT COST-'                      
*                                                                               
         DC    X'20',XL3'00',AL4(FPRD),CL20'PRODUCT-'                           
         DC    X'21',XL3'00',AL4(FEST),CL20'ESTIMATE-'                          
         DC    X'22',XL3'00',AL4(FPEST),CL20'PRODUCT/ESTIMATE'                  
*                                                                               
         DC    X'30',XL3'00',AL4(FJOB),CL20'AD CODE-'                           
         DC    X'31',XL3'00',AL4(FSPACE),CL20'SPACE-'                           
         DC    X'32',XL3'00',AL4(FPCOMM),CL20'POSITION CODE-'                   
         DC    X'FFFF'             END OF DATATAB                               
*                                                                               
         SPACE 3                                                                
PPNXTEL  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
PPFSTEL  DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    PPNXTELX                                                         
         CLC   ELCOD,0(R2)                                                      
         BER   RE                                                               
         B     PPNXTEL                                                          
PPNXTELX DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
         DROP  R6                                                               
*                                                                               
*                                                                               
         DS    F                                                                
GETCHGS  DS    0H                                                               
         ST    RE,GETCHGS-4                                                     
         L     R2,AMYBUY                                                        
         LA    R2,33(R2)                                                        
         MVI   ELCOD,X'24'                                                      
         USING PCHGELD,R2                                                       
GETCH5   BAS   RE,PPNXTEL                                                       
         BNE   GETCHX                                                           
         CLC   2(2,R2),CHGCDATC         COMPRESSED DATE                         
         BL    GETCH5                                                           
         OC    MYCHG1,PCHGIND1                                                  
         OC    MYCHG2,PCHGIND2                                                  
         OC    MYCHG3,PCHGIND3                                                  
         CLI   1(R2),PCHGNEWS          CHK FOR LEN >11  COST CHANGE             
         BNH   GETCH5                                                           
         OI    MYCOSTC,X'01'                                                    
         B     GETCH5                                                           
*                                                                               
GETCHX   L     RE,GETCHGS-4                                                     
         BR    RE                                                               
         DROP  R2                                                               
         LTORG                                                                  
*****                                                                           
TBUYSW   DS    CL1                                                              
PATCH2   DC    XL20'00'                                                         
*                                                                               
         EJECT                                                                  
SETPUB   CSECT                                                                  
         NMOD1 0,SETPUB                                                         
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
         MVI   PUBN,C' '                                                        
         MVC   PUBN+1(256),PUBN                                                 
         MVC   PUBN+257(L'PUBN-257),PUBN                                        
         LA    R4,PUBN                                                          
         CLI   QMEDIA,C'N'                                                      
         BNE   SP20                                                             
*                                  NEWSPAPER                                    
         MVC   0(2,R4),PUBSTATE                                                 
         MVI   2(R4),C','                                                       
         MVC   4(16,R4),PUBCITY                                                 
         MVC   PNML(20,R4),PUBNAME                                              
         LA    R4,PNML*2(R4)                                                    
         MVC   0(20,R4),PUBZNAME                                                
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,PNML(R4)                                                      
*                                  UNDERLINE PUB NAME                           
         LR    R5,R4                                                            
         SH    R5,=Y(PNML+1)                                                    
         MVC   0(PNML,R4),MYDASH                                                
SP19     DS    0H                                                               
         CLI   0(R5),C' '                                                       
         BH    SP19B                                                            
         CLI   PNML(R5),C' '                                                    
         BH    SP19B                                                            
         MVI   PNML*2(R5),C' '                                                  
         BCT   R5,SP19                                                          
SP19B    DS    0H                                                               
         LA    R4,PNML(R4)                                                      
         LR    R5,R4                                                            
         CLI   PUBKPUB+5,0                                                      
         BE    SP30                                                             
         GOTO1 PUBEDIT,DMCB,PUBKPUB,(C'E',WORK)                                 
*                                                                               
         MVC   0(11,R4),WORK                                                    
         LA    R5,12(R4)                                                        
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
*                                                                               
         LA    R5,2(R5)                                                         
         B     SP30                                                             
*                                  MAGAZINES                                    
SP20     DS    0H                                                               
         MVC   0(20,R4),PUBNAME                                                 
         LA    R4,PNML(R4)                                                      
         MVC   0(20,R4),PUBZNAME                                                
         CLI   QMEDIA,C'O'         FOR OUTDOOR USE ST, MKT                      
         BNE   SP21                                                             
         MVC   0(2,R4),PUBSTACD    TRY STACD                                    
         CLI   PUBSTACD,C' '                                                    
         BNH   SP20B                                                            
         CLI   PUBSTACD,C'0'       UNLESS NUMERIC                               
         BL    *+10                                                             
SP20B    DS    0H                                                               
         MVC   0(2,R4),PUBSTATE                                                 
         MVI   2(R4),C','                                                       
         MVI   3(R4),C' '                                                       
         MVC   4(20,R4),PUBZNAME                                                
SP21     DS    0H                                                               
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,PNML(R4)                                                      
*                                  UNDERLINE PUB NAME                           
         LR    R5,R4                                                            
         SH    R5,=Y(PNML+1)                                                    
         MVC   0(PNML,R4),MYDASH                                                
SP21A    DS    0H                                                               
         CLI   0(R5),C' '                                                       
         BH    SP21B                                                            
         CLI   PNML(R5),C' '                                                    
         BH    SP21B                                                            
         MVI   PNML*2(R5),C' '                                                  
         BCT   R5,SP21A                                                         
SP21B    DS    0H                                                               
         LA    R4,PNML(R4)                                                      
         LR    R5,R4                                                            
         CLI   QMEDIA,C'O'         NO FREQ FOR OUTDOOR                          
         BE    SP22                                                             
         CLI   FREQOPT,C'N'        SEE IF SHOWING FREQUENCY                     
         BE    SP22                                                             
         MVI   ELCOD,X'20'                                                      
         LA    R2,PUBREC+33                                                     
         BAS   RE,SPNXTEL                                                       
         BNE   SP22                                                             
         USING PUBGENEL,R2                                                      
         CLI   PUBMFREQ,C' '                                                    
         BNH   SP22                                                             
         MVC   0(5,R4),=C'PUBL='                                                
         MVC   5(2,R4),PUBMFREQ                                                 
         LA    R5,8(R4)                                                         
SP22     DS    0H                                                               
         B     SP30                                                             
*                                  TRY FOR CLIENT NUMBER                        
SP30     DS    0H                                                               
         MVI   ELCOD,X'14'                                                      
         LA    R2,PUBREC+33                                                     
         BAS   RE,SPFSTEL                                                       
         B     *+8                                                              
SP31     DS    0H                                                               
         BAS   RE,SPNXTEL                                                       
         BNE   SP33                                                             
         USING PUBREPEL,R2                                                      
         CLC   PUBRPOFF,PCLTKCLT                                                
         BNE   SP31                                                             
         OC    PUBCVEN,PUBCVEN                                                  
         BZ    SP33                                                             
         CLC   PUBCVEN,SPACES                                                   
         BE    SP33                                                             
         CLI   PUBCVEN,X'FF'                                                    
         BE    SP33                                                             
         CLI   PUBCVEN,X'99'                                                    
         BH    SP32                                                             
         IC    R3,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R3),PUBCVEN),1(R5)                                
         B     SP32B                                                            
SP32     DS    0H                                                               
         MVC   1(12,R5),PUBCVEN                                                 
SP32B    DS    0H                                                               
         MVI   0(R5),C'('                                                       
         LA    R5,18(R5)                                                        
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C')'                                                       
*                                  VENDOR NO.                                   
SP33     DS    0H                                                               
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,PNML(R4)                                                      
         IC    R2,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R2),PUBKPUB),1(R4)                                
*                                                                               
         MVI   0(R4),C'('                                                       
         LA    R5,19(R4)                                                        
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C')'                                                       
         LA    R5,3(R5)                                                         
*                                                                               
*                                  CIRC                                         
         CLI   CIRCOPT,C'S'                                                     
         BE    SP36                                                             
         LA    R4,PNML(R4)         PUT CIRC ON NEXT LINE                        
         LR    R5,R4                                                            
         LA    R2,PUBREC+33                                                     
         MVI   ELCOD,X'30'                                                      
         MVC   X,SPACES                                                         
         MVC   X(5),=C'CIRC='                                                   
         XC    FULL,FULL                                                        
SP34     DS    0H                                                               
         BAS   RE,SPNXTEL                                                       
         USING PUBCIREL,R2                                                      
         BNE   SP34F                                                            
         CLC   PUBCDAT,BEND        TEST AFTER END                               
         BH    SP34G                                                            
         CLC   PUBCDAT,BSTART      TEST BEFORE START                            
         BH    SP34D                                                            
         BE    SP34E                                                            
         ST    R2,FULL             SAVE LATEST                                  
         B     SP34                                                             
*                                                                               
SP34D    DS    0H                                                               
         ST    R2,WORD                                                          
         L     R2,FULL             DO LATEST                                    
         LTR   R2,R2                                                            
         BNP   *+8                                                              
         BAS   RE,SETCIRC                                                       
         L     R2,WORD                                                          
*                                                                               
SP34E    DS    0H                                                               
         XC    FULL,FULL           CLEAR LATEST                                 
         BAS   RE,SETCIRC                                                       
         B     SP34                                                             
*                                                                               
SP34F    DS    0H                                                               
         L     R2,FULL                                                          
         LTR   R2,R2                                                            
         BZ    SP34Z                                                            
         BAS   RE,SETCIRC                                                       
         B     SP34Z                                                            
*                                                                               
SP34G    DS    0H                                                               
         CLI   X,C'C'                                                           
         BNE   *+8                                                              
         BAS   RE,SETCIRC                                                       
         B     SP34                                                             
*                                                                               
SP34Z    DS    0H                                                               
*                                                                               
SP36     DS    0H                                                               
         MVI   RDSHRFST,1          PRIMARY REG/DST                              
         CLC   QREGION(6),SPACES                                                
         BE    SP40                                                             
         XC    WORK(12),WORK                                                    
         MVC   WORK(3),PCLTKCLT                                                 
         MVC   WORK+3(3),PDIVKDIV                                               
         CLI   QDIV,C' '                                                        
         BNE   *+10                                                             
         MVC   WORK+3(3),PPRDDIV                                                
         CLC   QPUB+1(3),=C'RD='                                                
         BNE   *+16                                                             
         MVC   WORK(3),QPUB+4                                                   
         MVC   WORK+3(3),=C'000'                                                
         CLI   QREGION,C' '                                                     
         BE    *+10                                                             
         MVC   WORK+6(3),PREGKREG                                               
         CLI   QDIST,C' '                                                       
         BE    *+10                                                             
         MVC   WORK+9(3),PDSTKDST                                               
*                                  NOTE- DTLINS USED HERE AS WORK AREA          
         GOTO1 ARDSHR,DMCB,ALTLREC,(1,WORK),ADTLINS                             
*                                                                               
         L     RF,ADTLINS                                                       
         MVC   RDSHRFST,0(RF)                                                   
SP40     DS    0H                                                               
SPX      DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
SPNXTEL  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
SPFSTEL  DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    SPNXTELX                                                         
         CLC   ELCOD,0(R2)                                                      
         BER   RE                                                               
         B     SPNXTEL                                                          
SPNXTELX DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
         DS    F                                                                
SETCIRC  DS    0H                                                               
         ST    RE,SETCIRC-4                                                     
         USING PUBCIREL,R2                                                      
         LA    R6,X+5                                                           
         CLI   X,C'C'                                                           
         BE    *+8                                                              
         LA    R6,X                                                             
         EDIT  (P5,PUBCIR1),(17,0(R6)),COMMAS=YES,ALIGN=LEFT                    
         AR    R6,R0                                                            
         CLI   CIRCOPT,C'D'                                                     
         BNE   SC10                                                             
*                                                                               
         OC    PUBCDAT,PUBCDAT                                                  
         BNZ   SC4                                                              
         CLI   PUBCSRC,C' '                                                     
         BNH   SC10                                                             
*                                                                               
SC4      DS    0H                                                               
         MVI   1(R6),C'('                                                       
         LA    R3,2(R6)                                                         
         OC    PUBCDAT,PUBCDAT                                                  
         BZ    SC5                                                              
         MVI   BYTE,5              MMMDD/YY                                     
         LA    R3,11(R6)                                                        
         CLI   PUBCDAT+2,0                                                      
         BNE   *+12                                                             
         MVI   BYTE,9              MMM/YY                                       
         LA    R3,9(R6)                                                         
         LR    R0,RE                                                            
         GOTO1 DATCON,DMCB,(3,PUBCDAT),(BYTE,2(R6))                             
         LR    RE,R0                                                            
SC5      DS    0H                                                               
         MVC   0(4,R3),PUBCSRC                                                  
         LA    R6,16(R6)                                                        
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         MVI   1(R6),C')'                                                       
*                                                                               
SC10     DS    0H                                                               
         LA    R0,X                                                             
         SR    R6,R0               R6 HAS LENGTH                                
         AR    R6,R5                                                            
         LA    RF,PNML(R4)                                                      
         CR    RF,R6                                                            
         BNH   SC12                                                             
         MVC   0(30,R5),X                                                       
         B     SCX                                                              
SC12     DS    0H                                                               
         CLI   X,C'C'                                                           
         BNE   SC11                                                             
         MVC   0(5,R5),X                                                        
         MVC   X(30),X+5                                                        
         MVC   X+30(5),SPACES                                                   
         LA    R4,PNML(R4)                                                      
SC11     DS    0H                                                               
         MVC   0(30,R4),X                                                       
SCX      DS    0H                                                               
         LA    R4,PNML(R4)                                                      
         LR    R5,R4                                                            
         MVC   X,SPACES                                                         
         L     RE,SETCIRC-4           RESTORE RETURN REGISTER                   
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                  PROCESS REQUEST PARAMS                       
         SPACE 2                                                                
NPROC    CSECT                                                                  
         NMOD1 0,NPROC                                                          
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
         MVI   TWOTABS,C'N'        NO 2(D SPACE TAB                             
         CLI   LEVSW,C'C'          UNLESS CLIENT RUN                            
         BNE   *+8                                                              
         MVI   TWOTABS,C'Y'                                                     
*                                                                               
         MVI   BFSW,C'N'                                                        
         XC    CHGCDAT,CHGCDAT                                                  
         XC    CHGCDATC,CHGCDATC                                                
         XC    LMCPE,LMCPE         MUST CLEAR LAST MED/CLT/PRD/EST              
         CLI   QPAY,C' '           CHK FOR CHG CONTROL DATE                     
         BE    NPROC7                                                           
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
*                                                                               
*        GOTO1 DTCNV,DMCB,QPAY,(1,CHGCDAT)                                      
         GOTO1 DATCON,DMCB,(0,QPAY),(3,CHGCDAT)                                 
*        GOTO1 DTCNV,DMCB,QPAY,(2,CHGCDATC)                                     
         GOTO1 DATCON,DMCB,(0,QPAY),(2,CHGCDATC)                                
*                                                                               
NPROC7   MVC   PAGE,=H'1'                                                       
         MVC   P1,SPACES                                                        
         MVI   FORCEHED,C'Y'                                                    
         L     R1,ABUYTAB                                                       
         ST    R1,ANXTBUY          RESET ANXTBUY FOR EACH PUB                   
         MVI   0(R1),X'FF'                                                      
*                               DO CONTROLLER'S WORK                            
         LA    RF,PAGYREC                                                       
         ST    RF,ADAGY                                                         
         LA    RF,PCLTREC                                                       
         ST    RF,ADCLT                                                         
         LA    RF,PPRDREC                                                       
         ST    RF,ADPRD                                                         
         LA    RF,PESTREC                                                       
         ST    RF,ADEST                                                         
         LA    RF,PBUYREC                                                       
         ST    RF,ADBUY                                                         
         LA    RF,PBILLREC                                                      
         ST    RF,ADBILL                                                        
         LA    RF,PUBREC                                                        
         ST    RF,ADPUB                                                         
*                                                                               
         L     RE,=A(STDCOMSV)                                                  
         ST    RE,AOFSTCOM                                                      
         XC    EDITCNT,EDITCNT                                                  
         XC    PBRCNT,PBRCNT                                                    
         L     R3,AJOBLST                                                       
         MVI   0(R3),X'FF'         CLEAR JOBLIST                                
         MVI   NEWPAGE,C'N'                                                     
         XC    PROF,PROF                                                        
         MVC   MKTN,SPACES                                                      
         MVC   DSTN,SPACES                                                      
         MVC   REGN,SPACES                                                      
         MVC   ESTN,SPACES                                                      
         MVC   PRDN,SPACES                                                      
         MVC   DIVN,SPACES                                                      
         MVI   MTOTLNS,C' '                                                     
         MVC   MTOTLNS+1(L'MTOTLNS-1),MTOTLNS                                   
         MVI   LEVSW,C'P'          SET DEFAULT                                  
*                                                                               
*                                  SET SWITCHES TO CONTROL                      
*                                  EST, PRD, DIV, CLT IN STUBS AND              
*                                  HEADLINES                                    
*                                                                               
         MVI   ESTSW,0                                                          
         CLI   LEVSW,C'E'                                                       
         BNE   NP2B                                                             
         CLI   QEST,C'0'                                                        
         BL    NP2                                                              
         CLI   QESTEND,C' '                                                     
         BE    NP2B                                                             
NP2      DS    0H                                                               
         OI    ESTSW,X'80'         EST IN STUBS                                 
NP2B     DS    0H                                                               
         CLC   QEST,=C'ALL'                                                     
         BNE   NP2D                                                             
         JIF   LEVSW,=,C'E',OR,C'B',OR,C'V',OR,C'C',NP2F,JUMP=N                 
NP2D     DS    0H                                                               
         OI    ESTSW,X'08'         EST IN HEADLINES                             
NP2F     DS    0H                                                               
         MVI   PRDSW,0                                                          
         CLI   LEVSW,C'B'                                                       
         BNE   NP2J                                                             
         OI    PRDSW,X'80'         PRD IN STUBS                                 
         B     NP2N                                                             
NP2J     DS    0H                                                               
         CLI   QPRODUCT,C' '                                                    
         BE    NP2N                                                             
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   NP2M                                                             
         JIF   LEVSW,=,C'B',OR,C'V',OR,C'C',NP2N,JUMP=N                         
NP2M     DS    0H                                                               
         OI    PRDSW,X'08'         PRD IN HEADLINES                             
NP2N     DS    0H                                                               
         CLI   QPRODUCT,C' '                                                    
         BNE   *+8                                                              
         MVI   FCRDACTV,C'Y'       READ ONLY ACTIVE IF QPROD BLACK              
         MVI   DIVSW,0                                                          
         CLI   LEVSW,C'V'                                                       
         BNE   NP2R                                                             
         OI    DIVSW,X'80'         DIV IN STUBS                                 
         B     NP2U                                                             
NP2R     DS    0H                                                               
         CLI   QDIV,C' '                                                        
         BE    NP2U                                                             
         OI    DIVSW,X'08'         DIV IN HEADLINES                             
NP2U     DS    0H                                                               
         MVI   CLTSW,X'80'         CLT IN STUBS                                 
         JIF   LEVSW,=,C'C',NP2V,JUMP=N                                         
         MVI   CLTSW,X'08'         CLT IN HEADS                                 
NP2V     DS    0H                                                               
         MVI   SPCOPT,C'Y'    SET DEFAULTS                                      
         MVI   COSTOPT,C'Y'                                                     
NP4      DS    0H                                                               
         MVC   WEEKSW,QOPT3                                                     
         CLI   WEEKSW,C' '                                                      
         BH    *+8                                                              
         MVI   WEEKSW,0                                                         
*                                                                               
         MVI   DOLLOPT,0                                                        
*                                                                               
         MVI   DATSW,0                                                          
         MVI   DATSW2,0                                                         
*                                  FIRST FOR ESTIMATE                           
NP5      DS    0H                                                               
         MVI   PAGOPT,0                                                         
*                                  SET TOTAL SWITCHES                           
NP11     DS    0H                                                               
         LA    R3,APUBTOTS                                                      
         LA    R4,4                                                             
         LA    R5,ADDRSX-1                                                      
         MVI   0(R3),0                                                          
         BXLE  R3,R4,*-4                                                        
*                                                                               
         MVI   APUBTOTS,1                                                       
         MVI   AVENTOTS,1                                                       
         CLC   QSORT,=C'07'                                                     
         BNE   *+8                                                              
         MVI   AMKTTOTS,1                                                       
         CLI   QDIST,C' '                                                       
         BE    *+8                                                              
         MVI   ADSTTOTS,1                                                       
         CLI   QREGION,C' '                                                     
         BE    *+8                                                              
         MVI   AREGTOTS,1                                                       
         CLI   QEST,C' '                                                        
         BE    NP13                                                             
         CLI   QESTEND,C' '                                                     
         BNE   NP13                                                             
         MVI   AESTTOTS,1                                                       
         CLC   QEST,=C'ALL'                                                     
         BNE   NP13                                                             
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    NP13                                                             
         CLI   QPRODUCT,C' '                                                    
         BNE   NP14                IF ONE EST - NO HIGHER TOTALS                
NP13     DS    0H                                                               
         CLI   QPRODUCT,C' '                                                    
         BE    NP13B                                                            
         MVI   APRDTOTS,1                                                       
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   NP14                                                             
NP13B    DS    0H                                                               
*                                                                               
         MVI   ACLTTOTS,1                                                       
         CLI   QDIV,C' '                                                        
         BE    *+8                                                              
         MVI   ADIVTOTS,1                                                       
NP14     DS    0H                                                               
         CLI   LEVSW,C'C'                                                       
         BNE   *+8                                                              
         MVI   AREQTOTS,1                                                       
*                                                                               
         L     RE,ASPCTAB1                                                      
         L     RF,=A(ACCSL)                                                     
         XCEF                                                                   
*                                                                               
*                                  SET BSPARS                                   
         SR    R1,R1                                                            
         L     R2,ASPCTAB1                                                      
         SR    R3,R3                                                            
         LA    R4,STL                                                           
         LA    R5,STKL                                                          
         LA    R6,NSPC                                                          
         STM   R1,R6,BSPARS1                                                    
*                                                                               
         L     R2,ASPCTAB2                                                      
         STM   R1,R6,BSPARS2                                                    
*                                                                               
         MVC   ANXTTOT1,ASPCTOT1                                                
         MVC   ANXTCOM,ACOMTAB                                                  
         MVC   ANXTTOT2,ASPCTOT2                                                
*                                                                               
         L     RE,=A(STDCOMSV)    MUST CLEAR FOR EACH REQUEST                   
         L     RF,=F'3000'                                                      
         XCEF                                                                   
*                                                                               
         L     RF,=A(STDCOMSV)     ADDRESS OF TABLE                             
         ST    RF,BINTAB                                                        
         LA    RF,0                                                             
         ST    RF,BINNUMBR                                                      
         LA    RF,500                                                           
         ST    RF,BINMAXN                                                       
         LA    RF,6                                                             
         ST    RF,BINLEN                                                        
         LA    RF,6                                                             
         ST    RF,BINDISP                                                       
**       NOTE DISP OF KEY INTO RECORD IS 0                                      
*****                                                                           
*                                       CIRC                                    
NPX      DS    0H                                                               
         XIT                                                                    
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
INITBOX  CSECT                                                                  
         NMOD1 0,INITBOX                                                        
         L     R7,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R5,8(R1)     ADDRESS OF LINE                                     
*                                                                               
*******  L     R8,PPFILEC                                                       
         SPACE 2                                                                
*                                  CLEAR BOXES                                  
         L     R1,ABOX                                                          
         L     RF,=A(BOXSCORR+132-BOXYORN)                                      
         LR    RE,R1                                                            
         XCEF                                                                   
         CLI   DOBOXES,C'N'                                                     
         BE    CLROUT                                                           
         USING BOXD,R1                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXREQ,C'O'                                                      
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         MVC   BOXCOLS+21(L'MYBOX),MYBOX                                        
CLROUT   XIT                                                                    
         DROP  R1                                                               
         SPACE                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
DATOUT   CSECT                                                                  
         NMOD1 0,DATOUT                                                         
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
         MVI   ALLOWLIN,0                                                       
*                                 CLEAR MTOTLINS                                
         MVI   MTOTLNS,C' '                                                     
         MVC   MTOTLNS+1(L'MTOTLNS-1),MTOTLNS                                   
*                                  CLEAR DTLINS                                 
         L     R4,ADTLINS                                                       
         LA    R0,DTLINN                                                        
         L     R2,0(R1)            A(TOTS)                                      
         MVC   0(DTLINL,R4),SPACES                                              
         LA    R4,DTLINL(R4)                                                    
         BCT   R0,*-10                                                          
         SH    R4,=Y(DTLINL*3)                                                  
         ST    R4,ALASTLIN         A(LAST USABLE LINE)                          
DT20     DS    0H                                                               
*                                  FIND FIRST BLANK DTLIN                       
         L     R4,ADTLINS                                                       
*                                                                               
         CLI   MODE,LBUYPUB        SEE IF LAST BUY FOR PUB OR VENDOR            
         BE    DT21                                                             
         CLI   MODE,LBUYVEN                                                     
         BNE   DT21X                                                            
DT21     CLI   MTHOPT,C'B'         SEE IF SUPPRESSING BOTH                      
         BE    DTX                 $ AND INS COUNTS                             
         CLI   MTHOPT,C'I'         SEE IF SUPPRESSING INS COUNTS                
         BE    DT26                                                             
*                                                                               
DT21X    LA    R5,24(R2)           POINT PAST TOTALS                            
         L     R6,NMOS                                                          
DT22     DS    0H                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),32(R5)    'APPARENT' INSERTIONS                        
         CLI   RISW,C'R'                                                        
         BNE   *+10                                                             
         MVC   FULL+2(2),34(R5)    'REAL' INSERTIONS                            
         OC    FULL,FULL                                                        
         BZ    DT24                                                             
         EDIT  (B4,FULL),(5,WORK)                                               
*                                                                               
         OI    WORK+4,C'0'                                                      
         MVC   1(4,R4),WORK+1                                                   
         CLI   WORK,C' '           DONT CLOBBER * WITH SPACES                   
         BE    *+10                                                             
         MVC   0(1,R4),WORK                                                     
         CLI   TOTSW,1                                                          
         BNE   *+8                                                              
         MVI   5(R4),C'*'                                                       
*                                                                               
DT24     DS    0H                                                               
         LA    R4,8(R4)                                                         
         LA    R5,36(R5)                                                        
         BCT   R6,DT22                                                          
DT25     DS    0H                                                               
*                                  TOTAL INSERTIONS                             
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),16(R2)    'APPARENT' INSERTIONS                        
         CLI   RISW,C'R'                                                        
         BNE   *+10                                                             
         MVC   FULL+2(2),18(R2)    'REAL' INSERTIONS                            
         EDIT  (B4,FULL),(5,WORK)                                               
*                                                                               
         OI    WORK+4,C'0'                                                      
         MVC   3(5,R4),WORK                                                     
         CLI   TOTSW,1                                                          
         BNE   *+8                                                              
         MVI   8(R4),C'*'                                                       
*                                                                               
         ZIC   R1,ALLOWLIN                                                      
         LA    R1,1(R1)                                                         
         STC   R1,ALLOWLIN                                                      
*                                                                               
         CLI   MODE,LBUYPUB   SEE IF LAST BUY FOR VENDOR OR PUB                 
         BE    DT25C                                                            
         CLI   MODE,LBUYVEN                                                     
         BNE   DT25M                                                            
DT25C    CLI   MTHOPT,C'$'         SEE IF SUPPRESSING $                         
         BE    DT25X               YES THEN SKIP TOTAL $                        
*                                                                               
DT25M    LA    R4,DTLINL(R4)       NOW DO TOTAL DOLLARS                         
         LA    R5,0(R2)            GROSS                                        
         BAS   RE,MTEDT                                                         
*                                                                               
         ZIC   R1,ALLOWLIN                                                      
         LA    R1,1(R1)                                                         
         STC   R1,ALLOWLIN                                                      
*                                                                               
*                                  DO MONTH TOTS IF ANY                         
DT25X    DS    0H                                                               
DT26     LA    RF,TOTL-MAXMOS*16(R2)     POINT TO FIRST MONTH                   
         OC    0(MAXMOS*16,RF),0(RF)                                            
         BZ    DTX                 NONE                                         
*                                                                               
         CLI   MODE,LBUYPUB      SEE IF LAST BUY FOR PUB OR VENDOR              
         BE    DT26C                                                            
         CLI   MODE,LBUYVEN                                                     
         BNE   DT26X                                                            
DT26C    CLI   MTHOPT,C'B'       SEE IF SUPPRESSING BOTH                        
         BE    DTX                                                              
         CLI   MTHOPT,C'$'      SEE IF SUPPRESSING $                            
         BE    DTX                                                              
*                                                                               
DT26X    DS    0H                                                               
         ZIC   R0,ALLOWLIN                                                      
         AH    R0,=H'2'                                                         
         STC   R0,ALLOWLIN                                                      
*                                                                               
         LA    R4,MTOTLN1                                                       
         LR    R5,RF                                                            
         LA    R6,(MAXMOS+1)/2                                                  
DT27     DS    0H                                                               
         BAS   RE,MTEDT                                                         
         LA    R4,16(R4)                                                        
         LA    R5,16*2(R5)                                                      
         BCT   R6,DT27                                                          
*                                                                               
         CLI   MODE,LBUYPUB                                                     
         BE    DT27C                                                            
         CLI   MODE,LBUYVEN                                                     
         BNE   DT27X                                                            
DT27C    CLI   MTHOPT,C'I'       SEE IF SUPPRESSING ONLY INS COUNTS             
         BNE   DT27X                                                            
*                                                                               
         L     R6,NMOS          MUST POINT R4 TO LAST MONTH                     
         LA    R4,MTOTLN1                                                       
DT27E    LA    R4,8(R4)                                                         
         BCT   R6,DT27E                                                         
         LA    R5,0(R2)          MUST DO TOTAL DOLLARS                          
         BAS   RE,MTEDT                                                         
*                                                                               
DT27X    LA    R4,MTOTLN2+8                                                     
         LA    R5,16(RF)                                                        
         LA    R6,MAXMOS/2                                                      
DT28     DS    0H                                                               
         BAS   RE,MTEDT                                                         
         LA    R4,16(R4)                                                        
         LA    R5,16*2(R5)                                                      
         BCT   R6,DT28                                                          
*                                                                               
*                                  PUT MONTHLY $ ON 1 LN IF POSS.               
         LA    R4,MTOTLN1+1                                                     
         LA    R0,MTOTLNL-2                                                     
DT29     DS    0H                                                               
         CLI   0(R4),C' '                                                       
         BNH   DT30                                                             
         CLC   MTOTLNL-1(3,R4),SPACES                                           
         BNE   DTX                                                              
DT30     DS    0H                                                               
         LA    R4,1(R4)                                                         
         BCT   R0,DT29                                                          
*                                                                               
         OC    MTOTLN1,MTOTLN2                                                  
         MVC   MTOTLN2,SPACES                                                   
*                                                                               
         ZIC   R0,ALLOWLIN         CAN FIT ON ONE LINE -                        
         SH    R0,=H'1'            ADJUST ALLOWLIN                              
         STC   R0,ALLOWLIN                                                      
*                                                                               
*                                                                               
DTX      DS    0H                                                               
         MVI   TOTSW,0                                                          
         XIT1                                                                   
         SPACE 2                                                                
MTEDT    NTR1                                                                   
         USING DOLLD,R5                                                         
         LA    R1,REQTAB                                                        
MTEDT2   CLI   0(R1),0                                                          
         BE    MTEDTX                                                           
         CLI   0(R1),X'10'         FIND FIRST $ TYPE (NOW ONLY ONE)             
         BL    MTEDT5                                                           
         CLI   0(R1),X'1F'                                                      
         BH    MTEDT5                                                           
         B     MTEDT10                                                          
*                                                                               
MTEDT5   LA    R1,1(R1)                                                         
         B     MTEDT2                                                           
*                                                                               
MTEDT10  CLI   0(R1),X'16'         COST (FROM BILL FORMULA)                     
         BE    MT2                                                              
         L     R0,TGRS                                                          
         CLI   0(R1),X'10'         GROSS                                        
         BE    MT3                                                              
         S     R0,TCD                                                           
         CLI   0(R1),X'11'         GROSS/GLCD                                   
         BE    MT3                                                              
         L     R0,TGRS                                                          
         S     R0,TAC                                                           
         CLI   0(R1),X'12'         NET                                          
         BE    MT3                                                              
         S     R0,TCD                                                           
         CLI   0(R1),X'13'         NET-CD                                       
         BE    MT3                                                              
         L     R0,TCD              CD - X'14'                                   
         B     MT3                                                              
*                                                                               
MT2      DS    0H                                                               
         L     R0,TCOST                                                         
MT3      DS    0H                                                               
         LTR   R0,R0                                                            
         BZ    MTEDTX                                                           
         DROP  R5                                                               
         LR    R9,R0                                                            
         MVC   X(20),SPACES                                                     
         EDIT  (R9),(14,X),2,FLOAT=$,COMMAS=YES                                 
         LA    R3,X+5                                                           
         C     R9,=F'100000'                                                    
         BL    MT4                                                              
         LA    R3,X+4                                                           
         C     R9,=F'1000000'                                                   
         BL    MT4                                                              
         LA    R3,X+3                                                           
         C     R9,=F'10000000'                                                  
         BL    MT4                                                              
         LA    R3,X+2                                                           
         C     R9,=F'100000000'                                                 
         BL    MT4                                                              
         LA    R3,X+1                                                           
*                                                                               
MT4      MVC   0(13,R4),0(R3)                                                   
MTEDTX   DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                  SET DATE LIST                                
DTL      CSECT                                                                  
         NMOD1 0,DTL                                                            
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
         XC    DTLIST,DTLIST                                                    
*        GOTO1 DTCNV,DMCB,QSTART,(1,BSTART)                                     
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
*        GOTO1 (RF),(R1),QEND,(1,BEND)                                          
         GOTO1 DATCON,(R1),(0,QEND),(3,BEND)                                    
         SR    R6,R6                                                            
         LA    R4,DTLIST                                                        
         LA    R5,MAXMOS                                                        
         MVC   0(3,R4),BSTART                                                   
         CLI   WEEKSW,0                                                         
         BNE   DTL10                                                            
         MVI   2(R4),1                                                          
DTL4     DS    0H                                                               
         CLC   0(3,R4),BEND                                                     
         BH    DTL7                                                             
         CLI   1(R4),12                                                         
         BE    DTL5                                                             
         IC    R6,1(R4)                                                         
         LA    R6,1(R6)                                                         
         STC   R6,5(R4)                                                         
         MVC   4(1,R4),0(R4)                                                    
         B     DTL6                                                             
DTL5     DS    0H                                                               
         IC    R6,0(R4)                                                         
         LA    R6,1(R6)                                                         
         STC   R6,4(R4)                                                         
         MVI   5(R4),1                                                          
DTL6     DS    0H                                                               
         MVI   6(R4),1                                                          
         LA    R4,4(R4)                                                         
         BCT   R5,DTL4                                                          
DTL7     DS    0H                                                               
         MVC   0(4,R4),=4X'FF'                                                  
         B     DTL20                                                            
*                                                                               
DTL10    DS    0H                                                               
         MVC   WORK(6),QSTART                                                   
         LA    R3,1                1 DAY IF DAY REQ                             
         CLI   WEEKSW,C'D'                                                      
         BE    *+8                                                              
         LA    R3,7                7 DAYS IF WEEK REQ                           
*                                                                               
DTL11    DS    0H                                                               
         CLC   0(3,R4),BEND                                                     
         BH    DTL12                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R3)                                      
*                                                                               
*        GOTO1 DTCNV,DMCB,WORK+6,(1,4(R4))                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,4(R4))                                 
*                                                                               
         MVC   WORK(6),WORK+6                                                   
         LA    R4,4(R4)                                                         
         BCT   R5,DTL11                                                         
*                                                                               
DTL12    DS    0H                                                               
         MVC   0(4,R4),=4X'FF'                                                  
*                                                                               
DTL20    DS    0H                                                               
         SH    R5,=Y(MAXMOS)                                                    
         LCR   R5,R5                                                            
         ST    R5,NMOS             NO OF 'MONTHS'                               
*                                                                               
DTLX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                  SET SAVED HEADLINES                          
         SPACE 2                                                                
SETHEAD  CSECT                                                                  
         NMOD1 0,SETHEAD                                                        
         L     RC,PPFILEC                                                       
         SPACE                                                                  
*  SET UP BOXES PARAMETERS *                                                    
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'N'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
*                                  SET RPTNAME                                  
         SPACE 3                                                                
SETHDD   LA    R4,MYHA                                                          
         LA    R0,3                3 LINES                                      
         MVC   0(132,R4),SPACES                                                 
         LA    R4,132(R4)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         SR    R4,R4                                                            
         MVC   WORK,SPACES                                                      
         CLI   LEVSW,C'P'                                                       
         BNE   SH4                                                              
         MVC   WORK(6),=C'VENDOR'                                               
         LA    R4,6                                                             
         B     SH15                                                             
*                                                                               
SH4      DS    0H                                                               
         CLI   LEVSW,C'M'                                                       
         BNE   SH5                                                              
         MVC   WORK(6),=C'MARKET'                                               
         LA    R4,6                                                             
         B     SH15                                                             
*                                                                               
SH5      DS    0H                                                               
         CLI   DATSW,0                                                          
         BNE   *+8                                                              
         MVI   DATSW,C'S'          NO DATES                                     
         CLI   LEVSW,C'D'                                                       
         BNE   SH6                                                              
         CLI   QREGION,C' '                                                     
         BE    SH5B                                                             
         MVC   WORK(15),=C'REGION/DISTRICT'                                     
         LA    R4,15                                                            
         B     SH15                                                             
SH5B     DS    0H                                                               
         MVC   WORK(8),=C'DISTRICT'                                             
         LA    R4,8                                                             
         B     SH15                                                             
*                                                                               
SH6      DS    0H                                                               
         CLI   LEVSW,C'R'                                                       
         BNE   SH7                                                              
         MVC   WORK(6),=C'REGION'                                               
         LA    R4,6                                                             
         B     SH15                                                             
*                                                                               
SH7      DS    0H                                                               
         CLI   LEVSW,C'E'                                                       
         BNE   SH8                                                              
         MVC   WORK(8),=C'ESTIMATE'                                             
         LA    R4,8                                                             
         B     SH15                                                             
*                                                                               
SH8      DS    0H                                                               
         CLI   LEVSW,C'B'                                                       
         BNE   SH9                                                              
         MVC   WORK(7),=C'PRODUCT'                                              
         LA    R4,7                                                             
         B     SH15                                                             
*                                                                               
SH9      DS    0H                                                               
         CLI   LEVSW,C'V'                                                       
         BNE   SH10                                                             
         MVC   WORK(8),=C'DIVISION'                                             
         LA    R4,8                                                             
         B     SH15                                                             
*                                                                               
SH10     DS    0H                                                               
*                                                                               
SH15     DS    0H                                                               
         LA    R5,MYHA                                                          
SH16     DS    0H                                                               
         LTR   R4,R4                                                            
         BZ    SH20                                                             
         MVC   132(20,R5),WORK                                                  
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   264(0,R5),MYDASH                                                 
*                                                                               
*                                  DATES                                        
SH20     DS    0H                                                               
         GOTO1 ADTL                SET DATE LIST                                
         L     R1,ABOX                                                          
         LA    R4,DTLIST                                                        
         LA    R5,MYHA+22                                                       
***      MVC   MYBOX,SPACES         INITIALIZE                                  
*        LA    R6,MYBOX          LEFT MARGIN                                    
         LA    R6,BOXCOLS+21     LEFT MARGIN                                    
         MVI   0(R6),C'L'          LEFT ID                                      
SH21     DS    0H                                                               
         CLI   0(R4),X'FF'                                                      
         BNE   SH22                                                             
         MVC   136(5,R5),=C'TOTAL'                                              
         MVC   268(5,R5),MYDASH                                                 
         MVI   0(R6),C'R'          RIGHT MARGIN                                 
         L     R1,ABOX                                                          
         MVC   MYBOX,BOXCOLS+21                                                 
         B     SH40                                                             
         DROP  R1                                                               
*                                                                               
*****    B     SH30                OLD CODE FOR $ FIELDS                        
*                                                                               
SH22     DS    0H                                                               
*        GOTO1 DTCNV,DMCB,(1,0(R4)),(3,WORK)                                    
         GOTO1 DATCON,DMCB,(3,0(R4)),(5,WORK)                                   
*                                                                               
         CLI   WEEKSW,0                                                         
         BNE   SH23                                                             
         MVC   133(3,R5),WORK                                                   
         MVI   136(R5),C'/'                                                     
         MVC   137(2,R5),WORK+6         MTH/YEAR                                
         B     SH24                                                             
SH23     DS    0H                                                               
         MVC   134(5,R5),WORK           MTH DAY                                 
*                                                                               
SH24     DS    0H                                                               
         MVC   266(5,R5),MYDASH                                                 
         CLI   WEEKSW,0                                                         
         BNE   *+8                                                              
         MVI   265(R5),C'-'                                                     
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)            TO NEXT COL                                  
         MVI   0(R6),C'C'                                                       
         B     SH21                                                             
SH40     DS    0H                                                               
SH50     DS    0H                                                               
SHX      DS    0H                                                               
******   MVI   SH21,0                                                           
         XIT1                                                                   
*                                                                               
         SPACE 2                                                                
SHHIGH   LA    R0,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
*                                                                               
SHDIR    DS    0H                                                               
         ST    R0,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,,PRTDIR,KEY,KEY                                     
         B     SHDMCK                                                           
*                                                                               
SHGET    LA    R0,GETREC                                                        
*                                                                               
SHFIL    DS    0H                                                               
         ST    R0,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,,PRTFILE,KEY+27,AREC,DMWORK                         
*                                                                               
SHDMCK   DS    0H                                                               
         LR    RE,R0                                                            
         MVC   BYTE,DMOUTBTS                                                    
         NC    BYTE,DMCB+8                                                      
         BZR   RE                                                               
         DC    H'0'                                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
* THIS WILL PRINT THE ADCODE RECAP REPORT                          *            
********************************************************************            
RECAP    CSECT                                                                  
         NMOD1 0,RECAP                                                          
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
         MVI   CONTROLS,C'S'                                                    
*                                                                               
*        SET MYSPROG - 21 HAS PRD AND EST                                       
*                       11 HAS PRODUCT BUT NOT EST                              
*                       1  HAS NEITHER PRD NOR EST                              
*                                                                               
*                                                                               
         CLC   QEST,=C'ALL'                                                     
         BE    RECAP2                                                           
         CLC   QEST(6),SPACES      QEST AND QESTEND                             
         BE    RECAP2                                                           
         MVI   MYSPROG,21              PRD AND EST IN HEADLINES                 
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    RECAP1                                                           
         CLC   QPRODUCT,SPACES                                                  
         BE    RECAP1                                                           
         B     RECAP2X                                                          
*                                                                               
RECAP1   DS    0H                                                               
         XC    PPRDREC(100),PPRDREC      CLEAR PRODUCT                          
         B     RECAP2X                                                          
*                                                                               
RECAP2   DS    0H                                                               
         MVI   MYSPROG,1                                                        
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    RECAP2X                                                          
         CLC   QPRODUCT,SPACES                                                  
         BE    RECAP2X                                                          
         MVI   MYSPROG,11              PRD IN HEADLINES                         
RECAP2X  DS    0H                                                               
RECAP8   DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         XC    BUFREC,BUFREC                                                    
         ZAP   BUFNINS,=P'0'                                                    
         ZAP   BUFDOLS,=P'0'                                                    
         ZAP   BUFCDSC,=P'0'                                                    
         ZAP   BUFGRS,=P'0'                                                     
         ZAP   BUFAGYCM,=P'0'                                                   
         MVI   BUFTYPE,BUFTYP1                                                  
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,BUFREC,BUFTYP1                       
*                                                                               
         B     RECAP20                                                          
RECAP10  DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,BUFREC,BUFTYP1                        
RECAP20  TM    DMCB+8,X'80'        **END OF FILE**                              
         BO    RECAPX                                                           
         CLI   BUFTYPE,BUFTYP1                                                  
         BNE   RECAP30                                                          
*                                                                               
         LA    R5,P1                                                            
         MVC   6(3,R5),BUFCLI       MOVE RECORD TO PRINTLINE                    
         MVC   15(3,R5),BUFPRD                                                  
         MVC   23(6,R5),BUFADCD                                                 
         CLC   BUFADCD,EFFS                                                     
         BNE   RECAP20C                                                         
         MVC   23(16,R5),=C'*PRODUCT TOTALS*'                                   
         B     RECAP20P                                                         
*                                                                               
RECAP20C DS    0H                                                               
*                                                                               
RECAP20P DS    0H                                                               
         EDIT  (P8,BUFNINS),(6,106(R5)),COMMAS=YES                              
*                                                                               
         CLI   QOPT4,C'T'                                                       
         BNE   *+14                                                             
         ZAP   PFIELD,BUFDOLS      FROM MYCOST                                  
         B     EDIT                                                             
         CLI   QOPT4,C'C'                                                       
         BNE   *+14                                                             
         ZAP   PFIELD,BUFCDSC                                                   
         B     EDIT                                                             
*                                                                               
         ZAP   PFIELD,BUFGRS                                                    
         CLI   QOPT4,C'G'                                                       
         BE    EDIT                                                             
         CLI   QOPT4,C'N'                                                       
         BNE   *+14                                                             
         SP    PFIELD,BUFAGYCM                                                  
         B     EDIT                                                             
         SP    PFIELD,BUFCDSC                                                   
         CLI   QOPT4,C'1'                                                       
         BE    EDIT                                                             
         SP    PFIELD,BUFAGYCM                                                  
*                                                                               
EDIT     EDIT  (P8,PFIELD),(14,116(R5)),2,FLOAT=$,COMMAS=YES                    
*                                                                               
         CLC   BUFADCD,EFFS        SEE IF PRODUCT TOTAL REC                     
         BNE   RECAP20R                                                         
         MVI   112(R5),C'*'                                                     
         MVI   130(R5),C'*'                                                     
         MVI   SPACING,2                                                        
         B     RECAP20X                                                         
*                                                                               
RECAP20R DS    0H                                                               
         OC    BUFADCD,BUFADCD                                                  
         BNZ   RECAP20T                                                         
         MVC   23(6,R5),=C'*NONE*'                                              
         B     RECAP20X                                                         
*                                                                               
RECAP20T DS    0H                                                               
         XC    KEY,KEY             RD JOBREC FOR REST OF DATA                   
         LA    R4,KEY                                                           
         USING PJOBREC,R4                                                       
         MVC   PJOBKAGY,QAGENCY                                                 
         MVC   PJOBKMED,QMEDIA                                                  
         MVI   PJOBKRCD,X'15'                                                   
         MVC   PJOBKCLT,BUFCLI                                                  
         MVC   PJOBKPRD,BUFPRD                                                  
         MVC   PJOBKJOB,BUFADCD                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(16),KEYSAVE                                                  
         BNE   RECAP20X                                                         
         GOTO1 GETPRT                                                           
         L     R4,AREC                                                          
*                                                                               
         MVC   32(L'PJOBCPY,R5),PJOBCPY                                         
*                                                                               
         MVC   52(L'PJOBCAP1,R5),PJOBCAP1                                       
*                                         IF I HAVE PJOBCAP2 DATA               
*                                         PUT IT IN P2                          
         CLC   PJOBCAP2(10),SPACES                                              
         BNH   *+10                                                             
         MVC   132+52(L'PJOBCAP2,R5),PJOBCAP2                                   
         GOTO1 DATCON,DMCB,(3,PJOBSTA),(5,82(R5))                               
         GOTO1 DATCON,DMCB,(3,PJOBEND),(5,95(R5))                               
*                                                                               
         DROP  R4                                                               
*                                                                               
RECAP20X DS    0H                                                               
         MVC   RCSUBPRG,MYSPROG                                                 
         GOTO1 REPORT                                                           
         B     RECAP10                                                          
*                                                                               
RECAP30  DS    0H                                                               
*                                                                               
RECAP40  CLI   BUFTYPE,BUFTYP3     MOVE IN TOTAL                                
         BNE   RECAPX                                                           
         MVI   P1,X'00'                                                         
         LA    R5,P2                                                            
         MVC   98(5,R5),=C'TOTAL'                                               
         EDIT  (P8,BUFNINS),(6,106(R5)),COMMAS=YES                              
         MVC   112(2,R5),=C'**'                                                 
*                                                                               
         ZAP   PFIELD,=P'0'                                                     
         CLI   QOPT4,C'T'                                                       
         BNE   *+14                                                             
         ZAP   PFIELD,BUFDOLS      FROM MYCOST                                  
         B     EDIT2                                                            
         CLI   QOPT4,C'C'                                                       
         BNE   *+14                                                             
         ZAP   PFIELD,BUFCDSC                                                   
         B     EDIT2                                                            
*                                                                               
         ZAP   PFIELD,BUFGRS                                                    
         CLI   QOPT4,C'G'                                                       
         BE    EDIT2                                                            
         CLI   QOPT4,C'N'                                                       
         BNE   *+14                                                             
         SP    PFIELD,BUFAGYCM                                                  
         B     EDIT2                                                            
         SP    PFIELD,BUFCDSC                                                   
         CLI   QOPT4,C'1'                                                       
         BE    EDIT2                                                            
         SP    PFIELD,BUFAGYCM                                                  
*                                                                               
EDIT2    EDIT  (P8,PFIELD),(14,116(R5)),2,FLOAT=$,COMMAS=YES                    
         MVC   130(2,R5),=C'**'                                                 
         MVC   RCSUBPRG,MYSPROG                                                 
         GOTO1 REPORT                                                           
*                                                                               
RECAPX   DS    0H                                                               
         MVI   RCSUBPRG,0                                                       
         MVI   CONTROLS,0                                                       
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
MYSPROG  DS    CL1                                                              
         EJECT                                                                  
         SPACE 2                                                                
*                                  POST TO SPACE TOTALS                         
POST     CSECT                                                                  
         NMOD1 0,POST                                                           
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
         CLI   QOPT4,C'T'          SEE IF DOING COST                            
         BNE   POST0                                                            
         GOTO1 AGETBF,DMCB,PBUYREC     BILLING FORMULA                          
POST0    DS    0H                                                               
*                                                                               
*                                  ADD BUY TO BUYTAB                            
*                                  ENTRIES ARE 6 BYTES                          
*                                  1 BYTE DTLIST CODE 0-11                      
*                                  1 BYTE SPARE                                 
*                                  4 BYTE DISK ADDR                             
*                                                                               
         SR    R3,R3                                                            
         LA    R4,DTLIST                                                        
         LA    R5,PBUYKDAT         USE INS DATE                                 
         CLI   QBPDATE,C' '                                                     
         BE    POST15                                                           
         LA    R5,PBDBDATE         OR BILLABE DATE                              
         CLI   QBPDATE,C'B'                                                     
         BE    POST15                                                           
         LA    R5,PBDPDATE         OR PAYABLE DATE                              
         CLI   QBPDATE,C'P'                                                     
         BE    POST15                                                           
         LA    R5,PBDSDATE         OR SALE DATE                                 
         CLI   QBPDATE,C'S'                                                     
         BE    POST15                                                           
         LA    R5,PBDCDATE         OR CLOSE DATE                                
         CLI   QBPDATE,C'C'                                                     
         BE    POST15                                                           
POST15   DS    0H                                                               
         CLC   0(3,R5),DTLIST                                                   
         BL    POSTX               SKIP IF BEFORE DTLIST START                  
         CLC   0(3,R5),4(R4)                                                    
         BL    POST16                                                           
         LA    R4,4(R4)                                                         
         LA    R3,1(R3)                                                         
         B     POST15                                                           
*                                                                               
POST16   DS    0H                                                               
         L     R1,ANXTBUY                                                       
         STC   R3,0(R1)                                                         
         MVC   2(4,R1),KEY+27      SAVE DISK ADDR                               
         LA    R1,6(R1)                                                         
         ST    R1,ANXTBUY                                                       
         C     R1,ABUYTABX                                                      
         BL    *+6                                                              
         DC    H'0'                BUYTAB FULL                                  
*                                                                               
         MVI   0(R1),X'FF'         SET END OF TABLE                             
*                                                                               
*                                  POST TO MONTH TOTALS                         
         TM    PBUYCNTL,X'80'      SEE IF DELETED                               
         BNZ   POSTX               YES - DON'T POST TO TOTLALS                  
         XC    MYCOST,MYCOST                                                    
         LA    R1,REQTAB          SEE IF CLIENT COST SPECIFIED                  
POST17   CLI   0(R1),0            END OF TABLE                                  
         BE    POST19X                                                          
         CLI   0(R1),X'16'                                                      
         BE    POST19                                                           
         LA    R1,1(R1)                                                         
         B     POST17                                                           
*                                                                               
*                                                                               
POST19   DS    0H                                                               
         MVC   MYSVPRD,PBUYKPRD                                                 
*                                                                               
         CLI   QPRODUCT,C' '                                                    
         BE    *+10                                                             
         MVC   PBUYKPRD,PPRDKPRD   SO GETCOST WILL USE PRODUCT                  
*                                  BEING PROCESSED FOR ZZZ BUYS                 
*                                                                               
         MVC   WORK(4),GROSS                                                    
         MVC   WORK+4(4),CSHDSC                                                 
         MVC   WORK+8(4),AGYCOM                                                 
         ST    RF,SAVERF                                                        
         CLI   TAXOPT,C'N'                                                      
         BE    POST19C                                                          
         GOTO1 AGETCOST,DMCB,(C'T',BILBASA),WORK,PBUYREC                        
         B     POST19CX                                                         
*                                                                               
POST19C  DS    0H                                                               
         GOTO1 AGETCOST,DMCB,BILBASA,WORK,PBUYREC                               
POST19CX MVC   MYCOST,DMCB+4                                                    
         MVC   PBUYKPRD,MYSVPRD     RESTORE REAL PRODUCT TO PBUYREC             
         L     RF,SAVERF                                                        
*                                                                               
POST19X  LA    R2,DTLIST                                                        
         SR    R4,R2                                                            
         ST    R4,FULL             SAVE MONTH DISP                              
         LA    R3,APUBTOTS                                                      
         CLI   PUBOPT,C'V'                                                      
         BNE   *+8                                                              
         LA    R3,AVENTOTS                                                      
         LA    R0,9                                                             
         L     RE,FULL             MONTH DIST X 3                               
         SLL   RE,2                X 4 = DISP                                   
POST21   DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    POST22                                                           
         L     RF,0(R3)                                                         
         LA    RF,TOTL-MAXMOS*16(RF,RE)    THIS MONTH                           
         LM    R4,R6,0(RF)                                                      
         A     R4,GROSS                                                         
         A     R5,CSHDSC                                                        
         A     R6,AGYCOM                                                        
         STM   R4,R6,0(RF)                                                      
         L     R4,12(RF)                                                        
         A     R4,MYCOST                                                        
         ST    R4,12(RF)                                                        
*                                                                               
POST22   DS    0H                                                               
         LA    R3,4(R3)                                                         
         BCT   R0,POST21                                                        
*                                                                               
POST23   DS    0H                                                               
         L     R2,APUBTOTS                                                      
         CLI   PUBOPT,C'V'         SEE IF NOT DOING EDTION TOTALS               
         BNE   POST23C                                                          
         L     R2,AVENTOTS         YES THEN POST TO VENDOR TOTLALS              
POST23C  LA    R4,DTLIST                                                        
         LA    R5,PBUYKDAT         USE INS DATE                                 
         CLI   QBPDATE,C' '                                                     
         BE    POST35                                                           
         LA    R5,PBDBDATE         OR BILLABE DATE                              
         CLI   QBPDATE,C'B'                                                     
         BE    POST35                                                           
         LA    R5,PBDPDATE         OR PAYABLE DATE                              
         CLI   QBPDATE,C'P'                                                     
         BE    POST35                                                           
         LA    R5,PBDSDATE         OR SALE DATE                                 
         CLI   QBPDATE,C'S'                                                     
         BE    POST35                                                           
         LA    R5,PBDCDATE         OR CLOSE DATE                                
         CLI   QBPDATE,C'C'                                                     
         BE    POST35                                                           
POST35   DS    0H                                                               
         CLC   0(3,R5),4(R4)                                                    
         BL    POST36                                                           
         LA    R4,4(R4)                                                         
         B     POST35                                                           
*                                                                               
POST36   DS    0H                                                               
         MVI   BYTE,0                                                           
         CLI   PBDBFD,C'P'                                                      
         BNE   POST37                                                           
         CLI   QBPDATE,C' '                                                     
         BNE   POST37                                                           
         CLI   4(R4),X'FF'                                                      
         BE    POST37                                                           
         MVI   BYTE,X'80'          PRIOR MONTH                                  
         LA    R4,4(R4)                                                         
POST37   DS    0H                                                               
         SR    RF,RF               SET INS COUNTER                              
         IC    RF,2(R5)                                                         
         LA    R0,DTLIST                                                        
         SR    R4,R0                                                            
         ST    R4,FULL             SAVE MONTH DISP                              
         MH    R4,=H'9'           4X9=36                                        
         LA    R4,24(R2,R4)                                                     
         SR    RF,RF                                                            
         CLI   PBDSPACE,C'*'       NO INS                                       
         BE    POST37D                                                          
*                                                                               
         LA    RF,1                SET 1 INSERTION                              
         CLI   QMEDIA,C'O'                                                      
         BNE   POST37D                                                          
*                                  OUTDOOR - COUNT NO OF DISPLAYS               
         SR    RF,RF                                                            
         CLI   HOLDSPC,C'*'                                                     
         BE    POST37D                                                          
         CLI   HOLDSPC+17,C'*'                                                  
         BE    POST37D                                                          
         ZAP   DUB,PBDREG                                                       
         AP    DUB,PBDILLUM                                                     
         CVB   RF,DUB                                                           
POST37D  DS    0H                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   POST37E                                                          
         ST    RF,DUB              SAVE RF                                      
         GOTO1 ABLDBUF                                                          
         L     RF,DUB              RESTORE RF                                   
POST37E  LH    R1,32(R4)           'APPARENT' INSERTIONS                        
         AR    R1,RF                                                            
         STH   R1,32(R4)                                                        
         CLI   RDSHRFST,1          TEST PRIMARY R/D                             
         BNE   POST38              NO                                           
         LH    R1,34(R4)           'REAL' INSERTIONS                            
         AR    R1,RF                                                            
         STH   R1,34(R4)                                                        
POST38   DS    0H                                                               
         L     R2,APUBTOTS                                                      
         CLI   PUBOPT,C'V'         SEE IF NOT DOING EDTION TOTALS               
         BNE   POST38C                                                          
         L     R2,AVENTOTS         YES THEN POST TO VENDOR TOTLALS              
POST38C  LM    R4,R6,0(R2)                                                      
         A     R4,GROSS                                                         
         A     R5,CSHDSC                                                        
         A     R6,AGYCOM                                                        
         STM   R4,R6,0(R2)                                                      
         L     R4,12(R2)                                                        
         A     R4,MYCOST                                                        
         ST    R4,12(R2)                                                        
         LH    RE,16(R2)           'APPARENT' INSERTIONS                        
         AR    RE,RF                                                            
         STH   RE,16(R2)                                                        
         CLI   RDSHRFST,1                                                       
         BNE   POSTX                                                            
         LH    RE,18(R2)           'REAL' INSERTIONS                            
         AR    RE,RF                                                            
         STH   RE,18(R2)                                                        
POSTX    DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
MYSVPRD  DS    CL3                                                              
*                                                                               
***********************************************************************         
* THIS SUBROUTINE WILL BUILD THE BUFFALO RECORD AND PUT IT TO BUFFALO *         
* AS A REGULAR RECORD,A PRODUCT TOTAL RECORD, AND A TOTAL RECORD      *         
***********************************************************************         
         SPACE 1                                                                
BLDBUF   CSECT                                                                  
         NMOD1 0,BLDB                                                           
         L     RC,PPFILEC                                                       
         XC    BUFREC,BUFREC                                                    
         ZAP   BUFGRS,=P'0'                                                     
         ZAP   BUFAGYCM,=P'0'                                                   
         ZAP   BUFCDSC,=P'0'                                                    
         ZAP   BUFDOLS,=P'0'                                                    
         MVI   BUFTYPE,BUFTYP1              INSERTION RECORDS                   
         MVC   BUFCLI,PBUYKCLT                                                  
         MVC   BUFPRD,PBUYKPRD                                                  
         MVC   BUFADCD,PBDJOB                                                   
         L     R1,DUB                                                           
         CVD   R1,BUFNINS                   # INSERTS (0 IF APPRNT)             
*                                                                               
         SR    R1,R1                                                            
         L     R1,GROSS                                                         
         CVD   R1,BUFGRS                                                        
         SR    R1,R1                                                            
         L     R1,AGYCOM                                                        
         CVD   R1,BUFAGYCM                                                      
         SR    R1,R1                                                            
         L     R1,CSHDSC                                                        
         CVD   R1,BUFCDSC                                                       
         SR    R1,R1                                                            
         L     R1,MYCOST                                                        
         CVD   R1,BUFDOLS                                                       
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC     REGULAR                    
*                                                                               
         MVC   BUFADCD,EFFS                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC    PRD TOTAL                   
*                                                                               
         MVI   BUFTYPE,BUFTYP3                                                  
         MVI   BUFSTAT,BUFTOT                                                   
         MVC   BUFADCD,EFFS                                                     
         MVC   BUFCLI,EFFS                                                      
         MVC   BUFPRD,EFFS                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC     TOTAL                      
BLDBUFX  XIT1                                                                   
         SPACE 3                                                                
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                  PRINTING INTERFACE                           
PRTALL   CSECT                                                                  
         NMOD1 0,PRTALL                                                         
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
         MVC   HOLDPARS,0(R1)                                                   
*                                                                               
         MVC   SVP,SPACES                                                       
*                                                                               
         SPACE 3                                                                
         LM    R2,R4,HOLDPARS                                                   
         LTR   R2,R2                                                            
         BZ    *+10                                                             
         MVC   P1,SPACES                                                        
*                                                                               
PRT11    DS    0H                                                               
PRT11B   DS    0H                                                               
         LM    R2,R4,HOLDPARS                                                   
         LTR   R2,R2                                                            
         BZ    PRT14                                                            
         CLI   MODE,LBUYVEN    FOR VENDOR TOTALS                                
         BE    PRT11D                                                           
         CLI   MODE,LBUYPUB    OR PUB TOTALS                                    
         BNE   PRT12                                                            
PRT11D   CLI   MTHOPT,C'B'    SEE IF I'M TO PRINT MONTHLY TOTALS                
         BE    PRTX           PROGPROF+15 = SETS MTHOPT TO N,$,I,B              
*                             B= SUPPRESS BOTH INS COUNT AND $'S                
         CLI   MTHOPT,C'I'    I= SUPPRESS ONLY INSERTION COUNTS                 
         BE    PRT14                                                            
         B     PRT12A         BYPASS SKIP BEFORE                                
*                             TO SUPPRESS THEM                                  
PRT12    DS    0H                                                               
         GOTO1 REPORT         SKIP BEFORE                                       
******** GOTO1 =V(MYREPT),DMCB,=C'H'                                            
PRT12A   MVC   P1(NML),0(R2)                                                    
         MVC   P1+22(DTLINL),0(R3)                                              
*                                                                               
*                                                                               
PRT12B   DS    0H                                                               
         CLC   P1,SPACES                                                        
         BE    PRT14                                                            
         MVC   SVP,P1                                                           
         GOTO1 REPORT                                                           
******** GOTO1 =V(MYREPT),DMCB,=C'I'                                            
*                                                                               
         CLC   0(NML,R2),SPACES                                                 
         BE    *+8                                                              
         LA    R2,NML(R2)                                                       
         CLC   0(DTLINL,R3),SPACES                                              
         BE    *+8                                                              
         LA    R3,DTLINL(R3)                                                    
         CLC   0(DOLLNL,R4),SPACES                                              
         BE    *+8                                                              
         LA    R4,DOLLNL(R4)                                                    
         B     PRT12A                                                           
*                                                                               
PRT14    DS    0H                                                               
         CLI   MODE,LBUYVEN    FOR VENDOR TOTALS                                
         BE    PRT14A                                                           
         CLI   MODE,LBUYPUB    OR PUB TOTALS                                    
         BNE   PRT14B                                                           
PRT14A   CLI   MTHOPT,C'$'   SEE IF I'M SUPPRESSING $'S                         
         BE    PRTX                                                             
         CLI   MTHOPT,C'B'   SEE IF I'M SUPPRESSING BOTH INS AND $'S            
         BE    PRTX                                                             
         CLI   MTHOPT,C'Y'   SEE IF I'M DOING BOTH INS AND $'S                  
         BE    PRT14B                                                           
         CLI   MTHOPT,C'I'   SEE IF I SUPPRESSED INSERTION COUNTS               
         BNE   PRT14B                                                           
*                                                                               
         LTR   R2,R2                                                            
         BZ    PRT14B                                                           
*                                                                               
         MVC   P1(NML),0(R2)     MUST SHOW TOTAL LINE NAME                      
*                                                                               
PRT14B   DS    0H                                                               
*                                                                               
*****    L     RF,NMOS                                                          
*****    MH    RF,=H'5'                                                         
*****    BCTR  RF,R0                                                            
*****    EX    RF,*+8                                                           
*****    B     *+10                                                             
*****    CLC   SVP+28(0),SPACES                                                 
*                                                                               
*****    BE    PRT14C              IF LAST LINE HAD DATA                        
*****    GOTO1 REPORT              IN DATE AREA - SKIP A LINE                   
*******  GOTO1 =V(MYREPT),DMCB,=C'J'                                            
PRT14C   DS    0H                                                               
         CLI   MTOTLNS,C' '                                                     
         BH    PRT15                                                            
         CLC   MTOTLNS+1(L'MTOTLNS-1),MTOTLNS                                   
         BE    PRTX                                                             
PRT15    DS    0H                                                               
         LA    RF,P1+NML-6                                                      
         MVC   000(MTOTLNL,RF),MTOTLN1                                          
         MVC   132(MTOTLNL,RF),MTOTLN2                                          
         MVC   SVP,P1     SO I CAN TELL IF I PRINTED ANYTHING                   
         GOTO1 REPORT                                                           
******** GOTO1 =V(MYREPT),DMCB,=C'K'                                            
         MVI   MTOTLNS,C' '                                                     
         MVC   MTOTLNS+1(L'MTOTLNS-1),MTOTLNS                                   
         B     PRTX                                                             
*                                                                               
PRTX     DS    0H                                                               
         CLC   SVP,SPACES       SEE IF I PRINTED ANYTHING                       
         BNE   PRTX5            YES THEN SKIP AFTER                             
         B     PRTXX                                                            
*                                                                               
PRTX5    MVI   ALLOWLIN,0                                                       
         MVC   P1,SPACES      JUST IN CASE                                      
         CLC   LINE,MAXLINES   SEE IF AT END OF PAGE                            
         BE    PRTXX          DON'T SKIP AFTER                                  
         GOTO1 REPORT         SKIP AFTER                                        
PRTXX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                              HEADHOOK                                         
SETHOOK  CSECT                                                                  
         NMOD1 0,SETHOOK                                                        
         L     RC,PPFILEC                                                       
         STM   R7,RC,HDHKR7C                                                    
         LA    R0,HDHOOK                                                        
         ST    R0,HEADHOOK                                                      
         XIT1                                                                   
*                                                                               
         CNOP  0,4                                                              
         USING *,RF                                                             
HDHOOK   NTR1                                                                   
         LM    R7,RC,HDHKR7C       LOAD SAVED REGISTERS                         
         B     HDHK1                                                            
*                                                                               
HDHKR7C  DS    6F'0'                                                            
         DROP  RF                                                               
*                                                                               
HDHK1    CLI   QOPT7,C'Y'                                                       
         BNE   *+8                                                              
         MVI   HEAD4+116,C'T'                                                   
*                                                                               
         LA    R2,HEAD9                                                         
         CLI   CONTROLS,C'S'       STANDARD COMMENT CALL                        
         BNE   NOSTDCO                                                          
         CLI   RCSUBPRG,1       AD CODE RECAP                                   
         BE    YESTDCO                                                          
         CLI   RCSUBPRG,11      AD CODE RECAP                                   
         BE    YESTDCO                                                          
         CLI   RCSUBPRG,21      AD CODE RECAP                                   
         BE    YESTDCO                                                          
*                                                                               
         MVC   3(4,R2),=C'CODE'                                                 
         MVC   135(4,R2),MYDASH                                                 
         MVC   10(7,R2),=C'COMMENT'                                             
         MVC   HEAD8+55(22),=C'POSITION CODE GLOSSARY'                          
         MVC   HEAD9+55(22),MYDASH                                              
         MVI   HEAD10,X'0'                                                      
         MVC   142(7,R2),MYDASH                                                 
         B     YESTDCO                                                          
*                                                                               
NOSTDCO  MVC   000(132,R2),MYHA                                                 
         MVC   132(132,R2),MYHB                                                 
         MVC   264(132,R2),MYHC                                                 
*                                                                               
YESTDCO  CLI   FCRDTEST,C'Y'                                                    
         BNE   HDHK2                                                            
         MVC   HEAD7+97(34),=C'*INCLUDES ANY PROPOSED INSERTIONS*'              
         CLI   QMEDIA,C'O'                                                      
         BNE   HDHK2                                                            
         MVC   HEAD7+120(11),=C'POSTINGS*  '                                    
HDHK2    CLI   QBPDATE,C' '                                                     
         BE    HDHK6                                                            
         LA    RF,=CL20'** BILLING PERIOD **'                                   
         CLI   QBPDATE,C'B'                                                     
         BE    HDHK4                                                            
         LA    RF,=CL20'** PAYABLE PERIOD **'                                   
         CLI   QBPDATE,C'P'                                                     
         BE    HDHK4                                                            
         LA    RF,=CL20'** ON-SALE DATES **'                                    
         CLI   QBPDATE,C'S'                                                     
         BE    HDHK4                                                            
         LA    RF,=CL20'** CLOSING DATES **'                                    
         CLI   QBPDATE,C'C'                                                     
         BE    HDHK4                                                            
HDHK4    DS    0H                                                               
         MVC   HEAD4+55(20),0(RF)                                               
HDHK6    CLI   QPAY,C' '           CHK FOR CHANGE CONTROL DATE                  
         BE    HDHK6C                                                           
         MVC   HEAD6+54(13),=C'CONTROL DATE='                                   
*        GOTO1 DTCNV,DMCB,(0,QPAY),(3,HEAD6+68)                                 
         GOTO1 DATCON,DMCB,(0,QPAY),(5,HEAD6+68)                                
HDHK6C   DS    0H                                                               
         CLI   CONTROLS,C'S'        SEE IF PRINTING GLOSSARY                    
         BE    HDHK10                                                           
*                                                                               
         TM    ESTSW,X'08'                                                      
         BNZ   *+10                                                             
         XC    PESTKEY,PESTKEY     NO EST IN HEADLINES                          
         TM    PRDSW,X'08'                                                      
         BNZ   *+10                                                             
         XC    PPRDKEY(4),PPRDKEY  NO PRD IN HEADLINES                          
***PRD                                                                          
         CLI   QPRODUCT,C' '   NO PRD IN HEADS IF QPRODUCT IS C' '              
         BNE   *+10                                                             
         XC    PPRDKEY(4),PPRDKEY  NO PRD IN HEADLINES                          
***PRD                                                                          
         TM    DIVSW,X'08'                                                      
         BNZ   *+10                                                             
         XC    PDIVKEY,PDIVKEY     NO DIV IN HEADLINES                          
         TM    CLTSW,X'08'                                                      
         BNZ   *+10                                                             
         XC    PCLTKEY,PCLTKEY                                                  
*                                                                               
         CLI   MODE,LBUYEST                                                     
         BNL   HDHK20                                                           
         CLI   MODE,LBUYREQ                                                     
         BL    HDHK20                                                           
         XC    PESTKEY,PESTKEY     NO EST IN HEADLINE                           
         CLI   MODE,LBUYPRO                                                     
         BNL   *+10                                                             
         XC    PPRDKEY(4),PPRDKEY  NO PRD IN HEADLINES                          
         CLI   MODE,LBUYDIV                                                     
         BNL   *+10                                                             
         XC    PDIVKEY,PDIVKEY     NO DIV IN HEADLINES                          
         B     HDHK20                                                           
*                                                                               
*                               HERE IF DOING AD CODE RECAP                     
HDHK10   DS    0H                                                               
         CLI   RCSUBPRG,1       AD CODE RECAP                                   
         BE    HDHK11                                                           
         CLI   RCSUBPRG,11      AD CODE RECAP                                   
         BE    HDHK11                                                           
         CLI   RCSUBPRG,21      AD CODE RECAP                                   
         BE    HDHK11                                                           
         B     HDHK20            MUST BE GLOSSARY                               
*                                                                               
HDHK11   DS    0H                                                               
         CLI   QOPT4,C'G'                                                       
         BNE   HDHK12                                                           
         MVC   H8+125(5),=C'GROSS'                                              
         MVC   H9+125(5),=C' COST'                                              
         MVC   H10+125(5),=C'-----'    UNDERLINING                              
         B     HDHK20                                                           
*                                                                               
HDHK12   DS    0H                                                               
         CLI   QOPT4,C'2'                                                       
         BNE   HDHK12B                                                          
         MVC   H8+125(3),=C'NET'                                                
         MVC   H9+123(7),=C'LESS CD'                                            
         MVC   H10+123(7),=C'-------'    UNDERLINING                            
         B     HDHK20                                                           
*                                                                               
HDHK12B  CLI   QOPT4,C'1'                                                       
         BNE   HDHK12C                                                          
         MVC   H8+124(5),=C'GROSS'                                              
         MVC   H9+123(7),=C'LESS CD'                                            
         MVC   H10+123(7),=C'-------'    UNDERLINING                            
         B     HDHK20                                                           
*                                                                               
HDHK12C  CLI   QOPT4,C'T'                                                       
         BNE   HDHK12E                                                          
         MVC   H8+124(6),=C'CLIENT'                                             
         MVC   H9+125(4),=C'COST'                                               
         MVC   H10+124(6),=C'------'    UNDERLINING                             
         B     HDHK20                                                           
*                                                                               
HDHK12E  DS    0H                                                               
         CLI   QOPT4,C'N'                                                       
         BNE   HDHK12G                                                          
         MVC   H8+127(3),=C'NET'                                                
         MVC   H9+126(4),=C'COST'                                               
         MVC   H10+126(4),=C'----'    UNDERLINING                               
         B     HDHK20                                                           
*                                                                               
HDHK12G  CLI   QOPT4,C'C'                                                       
         BNE   HDHK20                                                           
         MVC   H8+125(4),=C'CASH'                                               
         MVC   H9+125(5),=C'DISC.'                                              
         MVC   H10+125(5),=C'-----'                                             
*                                                                               
HDHK20   DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
GETBF    CSECT                                                                  
         NMOD1 0,GETBF                                                          
         L     RC,PPFILEC                                                       
         L     R4,DMCB                                                          
         USING PBUYREC,R4                                                       
         CLI   QOPT4,C'T'          SEE IF DOING COST                            
         BNE   GBFX                                                             
*                                                                               
         MVC   WORK(1),PBUYKMED    FIRST SEE IF SAME MED/CLT/PRD/EST            
         MVC   WORK+1(3),PBUYKCLT                                               
         MVC   WORK+4(3),MYPRD                                                  
         MVC   WORK+7(2),PBUYKEST                                               
         CLC   LMCPE,WORK                                                       
         BNE   GBF40                                                            
         OC    BILPROF,BILPROF                                                  
         BZ    GBF40                                                            
         B     GBFX                ALREADY HAVE FORMULA                         
GBF40    DS    0H                                                               
         MVC   SVAREC,AREC         SAVE PPG'S AREC                              
         MVC   PPGKEY,KEY          SAVE PPG KEY AND KEYSAVE                     
*                                                                               
         MVC   LMCPE,WORK          SAVE MED/CLT/PRD/EST                         
         XC    BILPROF,BILPROF                                                  
         MVC   BFORMD,SPACES                                                    
*                                                                               
GBF41    DS    0H                                                               
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(7),PBUYREC     AGY/MED/CLT                                   
         MVC   KEY+7(3),MYPRD                                                   
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+10(2),PBUYKEST                                               
         CLC   PESTKEY(12),KEY                                                  
         BE    GBF41X                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                 EST NOT ON FILE                             
         L     R0,ADEST                                                         
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
GBF41X   DS    0H                                                               
         OC    BILPROF,PESTBILP                                                 
         BNZ   GBF46                                                            
GBF42    DS    0H                    THEN TRY FOR AAA EST                       
         XC    KEY,KEY                                                          
         MVC   KEY(7),PESTREC                                                   
         MVC   KEY+7(3),=C'AAA'                                                 
         MVC   KEY+10(2),PESTKEST                                               
         MVI   KEY+3,7                                                          
         L     RF,ALISREC                                                       
         CLC   0(12,RF),KEY                                                     
         BE    GBF42C                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   GBF43                                                            
         L     R0,ALISREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         L     RF,AREC                                                          
*                                                                               
GBF42C   OC    BILPROF,PESTBILP-PESTREC(RF)                                     
         BNZ   GBF46                                                            
*                                                                               
GBF43    DS    0H                  NEXT TRY FOR PRODUCT                         
         XC    KEY,KEY                                                          
         MVC   KEY(7),PBUYREC                                                   
         MVC   KEY+7(3),MYPRD                                                   
         MVI   KEY+3,X'06'                                                      
GBF43B   L     RF,ALISREC                                                       
         CLC   0(10,RF),KEY                                                     
         BE    GBF43C                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   GBF44                                                            
         L     R0,ALISREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         L     RF,AREC                                                          
*                                                                               
GBF43C   OC    BILPROF,PPRDBILP-PPRDREC(RF)                                     
         BNZ   GBF46                                                            
GBF44    DS    0H                  NEXT TRY FOR PRODUCT AAA                     
         CLC   KEYSAVE+7(3),=C'AAA'    SEE IF I JUST DID AAA                    
         BE    GBF46                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYREC                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+7(3),=C'AAA'                                                 
         B     GBF43B                                                           
*                                                                               
GBF46    DS    0H                                                               
         CLI   BILBASA,0                                                        
         BNE   *+10                                                             
         MVC   BILBASA(5),DEFFORM                                               
         CLI   TAXOPT,C'N'                                                      
         BE    GBF48                                                            
         GOTO1 AGETCOST,DMCB,(C'T',BILBASA),(1,BFORMD)                          
         B     GBF50                                                            
*                                                                               
GBF48    DS    0H                                                               
         GOTO1 AGETCOST,DMCB,BILBASA,(1,BFORMD)                                 
GBF50    DS    0H                                                               
         MVC   KEY(32),PPGKEY                                                   
         GOTO1 HIGH                                                             
         MVC   KEYSAVE,PPGKEY+32   RESTORE PPG'S KEYSAVE                        
         MVC   AREC,SVAREC         AND AREC                                     
GBFX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
DEFFORM  DC    X'0505000000'                                                    
         SPACE 3                                                                
*                                                                               
         DROP  R4                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         BUFF  FLAVOR=PACKED,                                          X        
               KEYLIST=(13,A),                                         X        
               COMMENT=3,                                              X        
               LINES=100,                                              X        
               COLUMNS=5,                                              X        
               ROWS=1                                                           
*                                                                               
PPS2WRKD DSECT                                                                  
PPS2WRK  DS    0C                                                               
*                                                                               
ADDRS    DS    0F                                                               
ADTL     DS    A                                                                
ASETHEAD DS    A                                                                
ASETHOOK DS    A                                                                
ADATOUT  DS    A                                                                
APOST    DS    A                                                                
ABLDBUF  DS    A                                                                
ARECAP   DS    A                                                                
ABINSRCH DS    A                                                                
AGETSPC  DS    A                                                                
ASETPUB  DS    A                                                                
ANPROC   DS    A                                                                
AROLLUP  DS    A                                                                
APRTALL  DS    A                                                                
ARDSHR   DS    A                                                                
ACHOPPER DS    A                                                                
ARIGHT   DS    A                                                                
AFORMAT  DS    A                                                                
ABLDREQT DS    A                                                                
AGETCOST DS    A                                                                
AGETBF   DS    A                                                                
AJOBLST  DS    A                                                                
AJOBLSTX DS    A                                                                
*                                                                               
ABUYTAB  DS    A                                                                
ABUYTABX DS    A                                                                
AMYBUY   DS    A                                                                
ADTLINS  DS    A                                                                
ACOMTAB  DS    A                                                                
ASPCTAB1 DS    A                                                                
ASPCTOT1 DS    A                                                                
ASPCTAB2 DS    A                                                                
ASPCTOT2 DS    A                                                                
*                                                                               
APUBTOTS DS    A                                                                
AVENTOTS DS    A                                                                
AMKTTOTS DS    A                                                                
ADSTTOTS DS    A                                                                
AREGTOTS DS    A                                                                
AESTTOTS DS    A                                                                
APRDTOTS DS    A                                                                
ADIVTOTS DS    A                                                                
ACLTTOTS DS    A                                                                
AREQTOTS DS    A                                                                
ABUFF    DS    A                                                                
ADDRSX   EQU   *                                                                
*                                                                               
ACONIO1  DS    A                   PCONREC ADDRESS FROM PPG                     
*                                                                               
B2PROF   DS    XL16        B2 BILLING PROFILE - NEEDED FOR GETCOST              
*                                                                               
PROF     DS    XL32                                                             
PFIELD   DS    PL8                 FOR ADCODE REPORT COST FIELD                 
         DS    0F                                                               
HOLDPARS DS    CL24                                                             
ANXTBUY  DS    A                 ADDR OF NEXT AVAIL ENTRY IN BUYTAB             
LASTMODE DS    X                                                                
LEVSW    DS    X                                                                
BOXSET   DS    C                   HAVE BOXES BEEN TURNED ON                    
TABSW    DS    X                                                                
DATASW   DS    X                                                                
TAXOPT   DS    CL1               SET FROM B2PROF+12                             
SPCOPT   DS    X                                                                
COSTOPT  DS    X                                                                
JOBOPT   DS    X                                                                
PAGOPT   DS    X                                                                
FREQOPT  DS    X                                                                
WEEKSW   DS    X                                                                
DATSW    DS    X                                                                
DATSW2   DS    X                                                                
ESTSW    DS    X                                                                
PRDSW    DS    X                                                                
DIVSW    DS    X                                                                
CLTSW    DS    X                                                                
DOLLOPT  DS    X                                                                
PUBOPT   DS    X                                                                
CIRCOPT  DS    X                                                                
MTHOPT   DS    X                                                                
DOLLS    DS    XL3                                                              
COMFILT  DS    CL2                                                              
TWOTABS  DS    CL1                                                              
DATANUM  DS    CL1                                                              
*                                                                               
MYCHG1   DS    XL1                                                              
MYCHG2   DS    XL1                                                              
MYCHG3   DS    XL1                                                              
MYCOSTC  DS    XL1                                                              
*                                                                               
BSTART   DS    XL3                                                              
BEND     DS    XL3                                                              
SAVDAT   DS    XL3                                                              
CHGCDAT  DS    XL3                 CHANGE CONTROL DATE                          
CHGCDATC DS    XL2                 CHANGE CONTROL DATE COMPRESSED               
LMCPE    DS    CL9                 LAST MED/CLT/PRD/EST                         
PPGKEY   DS    CL64                                                             
SVAREC   DS    F                                                                
MYPRD    DS    CL3                 PROCESSING PRODUCT                           
DTLIST   DS    CL68                MAXMOS X 4                                   
MAXMOS   EQU   12                                                               
NMOS     DS    F                                                                
SAVERE   DS    F                                                                
SAVERF   DS    F                                                                
MYCOST   DS    F                                                                
AOFSTCOM DS    F                                                                
*                                                                               
ATOTS    DS    A                                                                
ADOLL    DS    A                                                                
ANXTTOT1 DS    A                                                                
ANXTTOT2 DS    F                                                                
ANXTCOM  DS    A                                                                
COMINDX  DS    X                                                                
MULTSTD  DS    C                                                                
ATOTWDS  DS    A                                                                
ADESC    DS    A                                                                
ALASTLIN DS    A                                                                
BSPARS   DS    6F                                                               
BSPARS1  DS    6F                                                               
BSPARS2  DS    6F                                                               
EDITCNT  DS    H                                                                
PBRCNT   DS    H                                                                
EDVENSW  DS    X                                                                
RISW     DS    X                                                                
LINENEED DS    X                                                                
ELCOD    DS    X                                                                
RDSHRFST DS    X                                                                
TOTSW    DS    X                                                                
BFSW     DS    C                                                                
NEWPAGE  DS    X                                                                
SVP      DS    CL132                                                            
HOLDSPC  DS    CL34                                                             
MYDASH   DS    CL40'-'                                                          
X        DS    CL132                                                            
W        DS    CL200                                                            
RPTNAME  DS    CL19                                                             
RPTUND   DS    CL19                                                             
EFFS     DS    13X'FF'                                                          
*                                                                               
REQTAB   DS    CL14                UP TO 14 FIELDS                              
*                                                                               
*                                                                               
*                                                                               
SPCN     DS    0CL405                                                           
         DS    15CL27                                                           
*                                                                               
PNML     EQU   30                                                               
*                                                                               
PUBN     DS    0CL300                                                           
         DS    10CL30                                                           
*                                                                               
MKTN     DS    0CL108                                                           
MKTN1    DS    CL27                                                             
MKTN2    DS    CL27                                                             
         DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
DSTN     DS    0CL108                                                           
DSTN1    DS    CL27                                                             
DSTN2    DS    CL27                                                             
DSTN3    DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
REGN     DS    0CL108                                                           
REGN1    DS    CL27                                                             
REGN2    DS    CL27                                                             
REGN3    DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
ESTN     DS    0CL108                                                           
ESTN1    DS    CL27                                                             
ESTN2    DS    CL27                                                             
         DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
PRDN     DS    0CL108                                                           
PRDN1    DS    CL27                                                             
PRDN2    DS    CL27                                                             
         DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
DIVN     DS    0CL108                                                           
DIVN1    DS    CL27                                                             
DIVN2    DS    CL27                                                             
         DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
CLTN     DS    0CL108                                                           
CLTN1    DS    CL27                                                             
CLTN2    DS    CL27                                                             
         DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
RPTN     DS    0CL108                                                           
RPTN1    DS    CL27                                                             
RPTN2    DS    CL27                                                             
         DS    CL27                                                             
         DS    CL27                                                             
*                                                                               
*                                                                               
*                                                                               
NML      EQU   27                                                               
*                                                                               
*                                                                               
MYHA     DS    CL132                                                            
MYHB     DS    CL132                                                            
MYHC     DS    CL132                                                            
*                                                                               
*                                                                               
DOLLNS   DS    0CL117                                                           
DOLLN1   DS    CL39                                                             
DOLLN2   DS    CL39                                                             
         DS    CL39                                                             
DOLLNL   EQU   39                                                               
*                                                                               
DTNEED   DS    F                                                                
DTLINL   EQU   110                                                              
DTLINN   EQU   75                                                               
*                                                                               
MTOTLNS  DS    0CL220                                                           
MTOTLN1  DS    CL110                                                            
MTOTLN2  DS    CL110                                                            
MTOTLNL  EQU   110                                                              
*                                                                               
       ++INCLUDE PBILPROF                                                       
*                                                                               
BFORMD   DS    CL53                                                             
SVGROSS  DS    CL68                                                             
*                                                                               
BINPARS  DS    F                                                                
BINTAB   DS    F                                                                
BINNUMBR DS    F                                                                
BINLEN   DS    F                                                                
BINDISP  DS    F                                                                
BINMAXN  DS    F                                                                
CONTROLS DS    X                                                                
DOBOXES  DS    C                                                                
MYBOX    DS    CL120                                                            
PPS2WRKL EQU   *-PPS2WRK                                                        
         SPACE 2                                                                
*                                                                               
         DS    0D                                                               
BUFREC   DS    0CL56                                                            
BUFKEY   DS    0CL13               *** BUFFALO KEY ***                          
BUFTYPE  DS    CL1                                                              
BUFTYP1  EQU   X'01'               SHOW ON REGULAR REPORT                       
BUFTYP3  EQU   X'10'               GRAND TOTAL                                  
BUFCLI   DS    CL3                                                              
BUFPRD   DS    CL3                                                              
BUFADCD  DS    CL6                                                              
*                                                                               
BUFSTAT  DS    CL1                 *** BUFFALO COMMENT DATA ***                 
BUFPRDT  EQU   X'10'               PRD TOT                                      
BUFTOT   EQU   X'02'               TOTAL                                        
         DS    XL2                                                              
*                                                                               
BUFNINS  DS    PL8                 *** BUFFALO ACCUMULATORS ***                 
BUFDOLS  DS    PL8                 COST                                         
BUFCDSC  DS    PL8                 CASH DISCOUNT                                
BUFGRS   DS    PL8                 GROSS                                        
BUFAGYCM DS    PL8                 AGENCY COMMISSION                            
BUFLNQ   EQU   *-BUFREC                                                         
*                                                                               
*                                                                               
       ++INCLUDE DDBUFFALOD                                                     
*                                                                               
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         SPACE 2                                                                
DOLLD    DSECT                                                                  
*                                                                               
TGRS     DS    F                                                                
TCD      DS    F                                                                
TAC      DS    F                                                                
TCOST    DS    F                                                                
         DS    F                                                                
         DS    F                                                                
         SPACE 2                                                                
STSD     DSECT                     DSECT TO COVER SPACE TAB                     
STDSPACE DS    0CL34                                                            
STUNITS  DS    XL4                                                              
STUNIND  DS    XL1                                                              
STCIND   DS    XL1                                                              
STUCOST  DS    XL4                                                              
STPREM   DS    XL1                                                              
STPRCOS  DS    XL4                                                              
         DS    CL2                                                              
STDSPC2  DS    CL17                EXTRA SPACE FOR OUTDOOR                      
STCOST   DS    XL4                                                              
STPRDJOB DS    CL9                 PRD/JOB NO.                                  
STKL     EQU   *-STSD                                                           
*                                                                               
STCOMX   DS    X                   COMMENT INDEX                                
STADDR   DS    AL4                 A(TOTS)                                      
*                                                                               
STL      EQU   *-STSD                                                           
TOTL     EQU   24+(MAXMOS*((32+4)+16))                                          
*                                  24 ACCUMALTOR TOTALS PLUS                    
*                                  FOR EACH MONTH-                              
*                                  32 FOR DATE OR PRD LIST                      
*                                  4 FOR INS TOTAL                              
*                                  16 FOR $ TOTALS                              
*                                                                               
PCHGELD  DSECT                                                                  
       ++INCLUDE PCHGELEM                                                       
*                                                                               
         PRINT OFF    WAS OFF                                                   
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
*                                                                               
         PRINT ON                                                               
*                                                                               
*                                                                               
PPS2ACCS CSECT                                                                  
NSPC     EQU   100                                                              
SPCTAB   DS    0F                                                               
         ORG   *+STL*NSPC                                                       
SPCTABL  EQU   *-SPCTAB                                                         
SPCTAB2  DS    0F                                                               
         ORG   *+STL*NSPC                                                       
COMTAB   DS    0F                                                               
         ORG   *+5000                                                           
COMTABL  EQU   *-COMTAB                                                         
SPCTOTS  DS    0F                                                               
         ORG   *+TOTL*NSPC                                                      
SPCTOTL  EQU   *-SPCTOTS                                                        
SPCTOTS2 DS    0F                                                               
         ORG   *+TOTL*NSPC                                                      
PUBTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
VENTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
MKTTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
DSTTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
REGTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
ESTTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
PRDTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
DIVTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
CLTTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
REQTOTS  DS    0F                                                               
         ORG   *+TOTL                                                           
ACCSL    EQU   *-PPS2ACCS                                                       
*                                                                               
DTLINS   DS    75CL110                                                          
*                                                                               
STDCOMSV CSECT                                                                  
         DS   CL3000                                                            
STCOMSVX DS   X                                                                 
*                                                                               
MYBUY    CSECT                                                                  
*NOP*    DS    CL3000                                                           
         DS    CL4000                                                           
         DS    0H                                                               
*                                                                               
JOBLST   CSECT                                                                  
         DS    1020X               10 X 102 BYTES OF JOB RECORD                 
JOBLSTX  DS    X                                                                
*                                                                               
BUYTAB   CSECT                                                                  
         DS    3000X               500 X 6 BYTES PER BUY                        
BUYTABX  DS    X                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'189PPREPS202 11/20/15'                                      
         END                                                                    
