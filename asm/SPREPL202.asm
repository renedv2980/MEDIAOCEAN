*          DATA SET SPREPL202  AT LEVEL 029 AS OF 02/23/15                      
*PHASE SPL202A                                                                  
         SPACE 1                                                                
*===============================================================*               
* 08/15/02  DEIS  USE FIELD BGRSP (PL6) FROM BILLHDR, NOT BAMT  *               
* 03/26/02  MHER  SINCE NETWORK DOESN'T USE BUCKETS, SKIP THEM  *               
* 01/22/02  MHER  PACKED BUCKETS IN ESTHDR                                      
* 01/03/01  EJOR  MAKE TOTAL COLUMN 8 CHARS WIDE                *               
* 11/12/97  MHER  SUPPORT FILTER/HEADLINES FOR OOWR ESTS ONLY   *               
*===============================================================*               
         TITLE 'SPREPL202 - NEW SPOTPAK ESTIMATE HEADER REPORT'                 
*===============================================================*               
*        QOPT1 N=ONE PRD PER PAGE                                               
*        QOPT2  =PRINT ALL ESTIMATES                                            
*              A=ACTIVE ONLY                                                    
*              I=INACTIVE ONLY                                                  
*        QOPT3 D=ONLY DEMOS ,ETC                                                
*              $=ONLY DOLLAR COLUMNS                                            
*        QOPT4 Y=SHOW ONLY REQ PERIOD $                                         
*        QOPT5 A=ADD -  FOR TURNAROUNDS                                         
*              C=CHANGE                                                         
*                                                                               
*         FOLLOWING OPT NOT ON REQ SCREEN                                       
*        QOPT7 O=SHOW ONLY 'OLD' NETPAK ESTS                                    
*              N=SHOW ONLY 'NEW' NETPAK ESTS                                    
*===============================================================*               
         SPACE 1                                                                
SPL202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPL202,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    R7,4095(RB)                                                      
         LA    R7,1(R7)                                                         
         USING SPL202+4096,R7      ** NOTE USE OF SECOND BASE REG **            
         LA    RC,SPACEND                                                       
         USING ESTWORKD,RC                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
         CLI   MODE,PRDFRST                                                     
         BE    PRDF                                                             
         CLI   MODE,ESTFRST                                                     
         BE    ESTF                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
INITIAL  DS    0H                                                               
         STM   R7,RC,HDHKR7                                                     
         LA    R0,ESTHDHK                                                       
         ST    R0,HEADHOOK                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   RQALLPOL,C'Y'                                                    
         MVI   RQSTDTLN,4          TO ALLOW YYMM-YYMM                           
         L     R8,ADAGY                                                         
         USING AGYHDR,R8                                                        
         MVC   SVAPROF,AGYPROF     SAVE AGYPROF                                 
         DROP  R8                                                               
         CLC   QSTART(12),SPACES                                                
         BNE   INIT2                                                            
         MVC   QSTAUTO(2),=C'ES'                                                
         MVC   BQSTART(6),=X'000000FFFFFF'                                      
         MVC   BQSTARTP(4),=X'0000FFFF'                                         
*                                                                               
INIT2    DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
         CLC   QEST(2),=C'NO'                                                   
         BNE   INIT5                                                            
         MVI   RCSUBPRG,4                                                       
         CLI   QOPT1,C'N'          SEE IF DOING ONE PRD PER PAGE                
         BNE   INITX                                                            
         MVI   RCSUBPRG,3                                                       
         B     INITX                                                            
*                                                                               
INIT5    DS    0H                                                               
         CLI   QOPT1,C'N'                                                       
         BNE   INITX                                                            
         MVI   RCSUBPRG,2                                                       
*                                                                               
INITX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
CLTF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         XC    SAVEPRD,SAVEPRD                                                  
         L     R8,ADCLT                                                         
         USING CLTHDR,R8                                                        
         MVC   SVCEXTRA,CEXTRA                                                  
         MVC   CUSER1,CEU1                                                      
         MVC   CUSER2,CEU2                                                      
         DROP  R8                                                               
         B     EXIT                                                             
         SPACE 2                                                                
PRDF     DS    0H                                                               
         L     R8,ADPRD                                                         
         USING PRDHDR,R8                                                        
         CLC   SAVEPRD,PKEYPRD                                                  
         BE    EXIT                                                             
         CLI   QOPT1,C'N'          SEE IF DOING ONE PRD PER PAGE                
         BNE   PRDF5                                                            
         MVI   FORCEHED,C'Y'       YES                                          
         MVC   SAVEPRD,PKEYPRD                                                  
         MVC   SAVEPACC,PACCT                                                   
         MVI   PPNTSW,1                                                         
         B     EXIT                                                             
*                                                                               
PRDF5    DS    0H                                                               
         MVC   SAVEPRD,PKEYPRD                                                  
         MVC   SAVEPNAM,PNAME                                                   
         MVC   SAVEPACC,PACCT                                                   
         MVI   PPNTSW,0                                                         
         B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
ESTF     DS    0H                                                               
         LA    R5,BTITLES                                                       
         LA    R8,9         FOR BCT                                             
ESTF0B   MVI   14(R5),X'00'        INITIALIZE BILLING ACTIVITY BYTE             
         LA    R5,15(R5)                                                        
         BCT   R8,ESTF0B                                                        
*                                                                               
*                                                                               
ESTF0D   CLC   QEST(2),=C'NO'      IF USING FILTERS MUST READ                   
         BNE   ESTF4               ESTS MYSELF - FROM ESTLST                    
         MVC   SAVESKEY,KEY         SAVE SPONSOR'S KEY                          
         MVI   DISP,0                                                           
*                                                                               
ESTF1    DS    0H                                                               
         LA    R8,ESTLST                                                        
         ZIC   R0,DISP                                                          
         AR    R8,R0                                                            
         LA    R8,1(R8)            BUMP PAST EST JUST DONE                      
ESTF2    CLI   0(R8),0                                                          
         BNE   ESTF3                                                            
         LA    R8,1(R8)                                                         
         LA    R5,ESTLST+255                                                    
         CR    R8,R5               SEE IF I'M AT END OF LIST                    
         BNH   ESTF2                                                            
         MVC   KEY,SAVESKEY        RESTORE SPONSOR'S KEY                        
         GOTO1 HIGH                RESTORE SEQ READ                             
         B     EXIT                                                             
*                                                                               
ESTF3    DS    0H                                                               
         MVC   DISP,0(R8)          SAVE THIS DISPLACEMENT INTO ESTLST           
*                                  AS STARTING POINT TO CONTINUE SEARCH         
*                                  FOR ESTS TO REPORT                           
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),PRD                                                     
         MVC   KEY+7(1),0(R8)                                                   
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY                                                   
         BNE   ESTF1               NOT FOUND - BYPASS                           
         CLI   KEY+8,0             MUST BE AN EST                               
         BNE   ESTF1                                                            
         GOTO1 GETEST                                                           
ESTF4    DS    0H                                                               
         L     R8,ADEST                                                         
         USING ESTHDR,R8                                                        
*                                                                               
         MVC   USER1,EUSER1        SAVE USER DEFINITION FIELDS                  
         MVC   USER2,EUSER2                                                     
*                                                                               
         CLI   Q2OPT1,C'Y'         TEST OOWR ESTIMATES ONLY                     
         BNE   *+12                                                             
         CLI   EOWSDAY,0           TEST OOWR ESTIMATE                           
         BE    ESTDX               NO - SKIP                                    
*                                                                               
         CLI   QSTART,C'0'         SEE IF I HAVE START AND END                  
         BL    ESTF4A                                                           
         CLC   EEND(4),QSTART      SEE IF EST ENDS BEFORE QSTART                
         BL    ESTDX               YES THEN SKIP THIS EST                       
         CLI   QSTART+4,C'0'       SEE IF DAY SPECIFIED                         
         BL    ESTF4T                                                           
         CLC   EEND,QSTART      SEE IF EST ENDS BEFORE QSTART                   
         BL    ESTDX               YES THEN SKIP THIS EST                       
ESTF4T   CLC   ESTART(4),QEND      SEE IF STARTS AFTER QEND                     
         BH    ESTDX               YES THEN SKIP THIS EST                       
         CLI   QEND+4,C'0'       SEE IF DAY SPECIFIED                           
         BL    ESTF4A                                                           
         CLC   ESTART,QEND      SEE IF STARTS AFTER QEND                        
         BH    ESTDX               YES THEN SKIP THIS EST                       
*                                                                               
ESTF4A   DS    0H                                                               
         CLI   QOPT7,C'O'          SEE IF SHOWING ONLY OLD                      
         BNE   ESTF4B                                                           
         TM    EPRDCD,X'80'                                                     
         BO    ESTDX                                                            
         B     ESTF4D                                                           
*                                                                               
ESTF4B   CLI   QOPT7,C'N'          SEE IF SHOWING ONLY NEW                      
         BNE   ESTF4D                                                           
         TM    EPRDCD,X'80'                                                     
         BNO   ESTDX                                                            
*                                                                               
ESTF4D   MVI   DOLSW,0                                                          
         MVI   LINEED,0                                                         
         XC    SVNETYM,SVNETYM                                                  
         ZAP   TOTBILL,=P'0'                                                    
*                                                                               
         LA    R6,ATOTS                                                         
         LHI   R5,NUMROWS*NUMCOLS                                               
*                                                                               
ESTF5    ZAP   0(8,R6),=P'0'                                                    
         LA    R6,8(R6)                                                         
         BCT   R5,ESTF5                                                         
*                                                                               
         LHI   R5,NUMROWS                                                       
         LA    R6,ESTTOTS                                                       
*                                                                               
ESTF6    ZAP   0(8,R6),=P'0'                                                    
         LA    R6,8(R6)                                                         
         BCT   R5,ESTF6                                                         
         SPACE 1                                                                
*============================================================                   
*  IF DOING ALL ESTS AND NOT DISPLAYING                                         
*  DOLLARS - SKIP READ OF BILLS                                                 
*============================================================                   
         SPACE 1                                                                
         JIF   QOPT2,EQ,C' ',AND,QOPT3,EQ,C'D',ESTF68,JUMP=N                    
*                                                                               
         BAS   RE,BLDMLST          BUILDS EST MTH LIST                          
*                                  AND DELETES EST $ OUT OF REQ PERIOD          
         CLI   QMED,C'N'           TEMP ! TEMP ! TEMP ! TEMP !                  
         BE    ESTF30                                                           
*                                  AND DELETES EST $ OUT OF REQ PERIOD          
         LA    R1,ATOTS                                                         
         LA    R2,EAUTH                                                         
         LHI   R0,NUMCOLS                                                       
*                                                                               
ESTF7    ZAP   0(8,R1),0(6,R2)                                                  
         LA    R1,8(R1)                                                         
         LA    R2,6(R2)                                                         
         BCT   R0,ESTF7                                                         
*                                                                               
         LA    R6,EAUTH                                                         
         LHI   R5,NUMCOLS                                                       
*                                                                               
ESTF8    AP    EATOTAL,0(6,R6)                                                  
         LA    R6,6(R6)                                                         
         BCT   R5,ESTF8                                                         
*                                                                               
         CLI   QMED,C'N'           NO BUCKETS FOR NETWORK                       
         BE    ESTF30                                                           
         LA    R6,EORD                                                          
         LA    R2,EOTOTAL                                                       
         LHI   R5,NUMCOLS                                                       
         LA    R4,OTOTS                                                         
         BAS   RE,ESTROLL                                                       
*                                                                               
ESTF20   LA    R6,EPAID                                                         
         LA    R2,ECTOTAL                                                       
         LHI   R5,NUMCOLS                                                       
         LA    R4,CTOTS                                                         
         BAS   RE,ESTROLL                                                       
         B     ESTF30                                                           
*                                                                               
ESTROLL  AP    0(8,R2),0(6,R6)     ADD TO EST TOTAL                             
**NOP**  AP    0(8,R2),78(6,R6)                                                 
*                                                                               
         AP    0(8,R4),0(6,R6)                                                  
**NOP**  AP    0(8,R4),78(6,R6)                                                 
*                                                                               
         LA    R6,6(R6)            NEXT MONTH IN ESTHDR                         
         LA    R4,8(R4)            NEXT MONTH IN ACCUMS                         
         BCT   R5,ESTROLL                                                       
         BR    RE                                                               
*                                                                               
ESTF30   DS    0H                                                               
         MVC   SAVEKEY,KEY         READ BILLING RECORDS                         
         XC    KEY,KEY                                                          
         MVC   KEY(13),EKEY                                                     
         GOTO1 HIGH                                                             
*                                                                               
ESTF35   GOTO1 SEQ                                                              
         CLC   KEYSAVE(8),KEY      CHK SAME A/M CLT PRD EST                     
         BNE   ESTF60                                                           
         GOTO1 GETBILL                                                          
*                                                                               
         DROP  R8                                                               
*                                                                               
         L     R8,ADBILL                                                        
         USING BILLREC,R8                                                       
*                                                                               
         CLI   BRETAIL,X'41'       IGNORE CORP RETAIL BILLS                     
         BE    ESTF35                                                           
*                                                                               
         CLI   BKEY+9,12                                                        
         BNH   *+8                                                              
         MVI   BKEY+9,12           FUNNY BILLING PERIOD - MAKE DEC              
         ZIC   R6,BKEY+9                                                        
         BCTR  R6,0                                                             
         LA    R1,DELMTHS                                                       
         AR    R1,R6                                                            
         CLI   0(R1),1                                                          
         BE    ESTF35              SKIP BILLS OUT OF REQ PERIOD                 
*                                                                               
         CLI   BTYPE,C'B'          SEE IF NEW BILLING REC                       
         BNE   ESTF39              NO                                           
*                                                                               
         LA    R2,9               SET TO UNKNOWN BILLING TYPE                   
         CLI   BTYPE+1,C'4'                                                     
         BL    ESTF36                                                           
         CLI   BTYPE+1,C'9'                                                     
         BH    ESTF36                                                           
*                                                                               
         NI    BTYPE+1,X'0F'                                                    
         ZIC   R2,BTYPE+1                                                       
         BCTR  R2,0                                                             
         OI    BTYPE+1,X'F0'                                                    
*                                                                               
ESTF36   LA    R4,TITLES                                                        
         MHI   R2,15                                                            
         AR    R4,R2                                                            
         MVI   14(R4),C'Y'        SET ON ACTIVITY SWITCH                        
*                                                                               
         LA    R2,9               SET TO UNKNOWN BILLING TYPE                   
         CLI   BTYPE+1,C'4'                                                     
         BL    ESTF37                                                           
         CLI   BTYPE+1,C'9'                                                     
         BH    ESTF37                                                           
*                                                                               
         NI    BTYPE+1,X'0F'                                                    
         ZIC   R2,BTYPE+1                                                       
         BCTR  R2,0                                                             
*                                                                               
ESTF37   LA    R4,ESTTOTS                                                       
         MHI   R2,8                                                             
         AR    R4,R2                                                            
         AP    0(8,R4),BGRSP       ADD TO BILLING TYPE ACCUM                    
*                                                                               
         LA    R4,ATOTS                                                         
         MHI   R2,NUMCOLS          GIVES DSPL TO BILLING ACCUM                  
         AR    R4,R2               BUMP TO BILLING ACCUM                        
         B     ESTF40                                                           
*                                                                               
ESTF39   DS    0H                                                               
         LA    R5,DTITLE                                                        
         LA    R4,EDETTOT                                                       
         CLI   BTYPE+1,4           OLD STYLE DETAIL BILLING                     
         BE    ESTF39C                                                          
*                                                                               
         LA    R4,ESUMTOT                                                       
         LA    R5,STITLE                                                        
*                                                                               
ESTF39C  AP    0(8,R4),BGRSP                                                    
         MVI   14(R5),C'Y'         SET ON ACTIVITY SWITCH                       
*                                                                               
         LA    R4,DETTOTS                                                       
         CLI   BTYPE+1,4           OLD STYLE DETAIL BILLING                     
         BE    ESTF40                                                           
*                                                                               
         LA    R4,SUMTOTS                                                       
*                                                                               
ESTF40   DS    0H                                                               
         SR    R6,R6                                                            
         IC    R6,BKEY+9           GET BILLING MONTH                            
         BCTR  R6,0                                                             
         SLL   R6,3                X 8                                          
*                                                                               
*                                  BILLING BY MONTH BY TYPE                     
*                                                                               
         AR    R4,R6               R4 POINTS TO MONTHLY ACCUM                   
         AP    0(8,R4),BGRSP                                                    
*                                                                               
         LA    R2,TOTBILL                                                       
         AR    R2,R6               GET RIGHT MTH                                
         AP    0(8,R2),BGRSP       BILLING BY MONTH ACCUM                       
*                                                                               
         AP    EBILTOT,BGRSP       TOTAL BILLING ACCUM                          
         B     ESTF35                                                           
*                                                                               
         DROP  R8                                                               
*                                                                               
ESTF60   DS    0H                                                               
         L     R8,ADEST                                                         
         USING ESTHDR,R8                                                        
         MVC   KEY,SAVEKEY         RESTORE SEQ READ                             
         GOTO1 HIGH                                                             
*                                                                               
         SR    R6,R6                                                            
         LA    R4,ATOTS                                                         
         LA    R2,TITLES                                                        
         LA    R5,NUMROWS                                                       
*                                                                               
ESTF62   LR    R1,R4                                                            
         BAS   RE,TSTZERO                                                       
         BNE   ESTF62B                                                          
*                                                                               
         CLI   14(R2),C'Y'                                                      
         BE    ESTF62B                                                          
         B     ESTF63                                                           
*                                                                               
ESTF62B  LA    R6,1(R6)                                                         
         MVI   DOLSW,1             SET FOR ACTIVITY                             
*                                                                               
ESTF63   AHI   R4,NUMCOLS*8                                                     
         LA    R2,15(R2)                                                        
         BCT   R5,ESTF62                                                        
*                                                                               
         LA    R6,3(R6)            BUMP FOR MTH HEADS                           
         CLI   QOPT2,C'A'          SEE IF DOING ACTIVE ONLY                     
         BNE   ESTF64              NO                                           
         CLI   DOLSW,1             YES - CHK FOR ACTIVITY                       
         BNE   ESTDX                                                            
         B     ESTF67                                                           
*                                                                               
ESTF64   CLI   QOPT2,C'I'          SEE IF DOING INACTIVE ONLY                   
         BNE   ESTF65              NO                                           
         CLI   DOLSW,1                                                          
         BE    ESTDX                                                            
         B     ESTF67                                                           
*                                                                               
ESTF65   DS    0H                                                               
         CLI   QOPT2,C'B'          BILLABLE EST ONLY                            
         BNE   ESTF67                                                           
         CLC   TOTBILL(96),OTOTS                                                
         BE    ESTDX               TOTALLY BILLED - BYPASS                      
*                                                                               
ESTF67   DS    0H                                                               
         CLI   QOPT3,C'D'          SEE IF DISPLAYING DOLLARS                    
         BE    *+8                 NO - LEAVE LINEED 0                          
         STC   R6,LINEED                                                        
*                                                                               
ESTF68   CLI   PPNTSW,1            SEE IF I NEED TO PRINT PRD                   
         BE    ESTF70              NO                                           
*                                                                               
         MVC   MID1(7),=C'PRODUCT'                                              
         MVC   MID1+8(3),SAVEPRD                                                
         MVC   MID1+12(20),SAVEPNAM                                             
         LA    RF,MID1+32                                                       
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   2(RF),C'('                                                       
         MVC   3(4,RF),SAVEPACC                                                 
         MVI   7(RF),C')'                                                       
         CLI   SAVEPACC,X'FF'                                                   
         BNE   ESTF69                                                           
         OI    SAVEPACC+3,X'0F'                                                 
         UNPK  3(5,RF),SAVEPACC+1(3)                                            
         MVI   8(RF),C')'                                                       
*                                                                               
ESTF69   DS    0H                                                               
         MVC   MID2(11),=11C'-'                                                 
         CLI   MID1+10,C' '                                                     
         BNE   *+8                                                              
         MVI   MID2+10,C' '                                                     
         MVI   FORCEMID,C'Y'                                                    
         AI    LINEED,3                                                         
*                                                                               
ESTF70   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1(3),DUB                                                        
         TM    ECNTRL,X'04'        CHK HELD                                     
         BZ    ESTF71                                                           
         MVI   P1+3,C'H'                                                        
         B     ESTF72                                                           
*                                                                               
ESTF71   TM    ECNTRL,X'08'        CHK LOCKED                                   
         BZ    ESTF72                                                           
         MVI   P1+3,C'L'                                                        
*                                                                               
ESTF72   DS    0H                                                               
         MVC   P1+5(20),EDESC                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(0,ESTART),(5,P1+26)                                 
         MVI   P1+34,C'-'                                                       
         GOTO1 DATCON,DMCB,(0,EEND),(5,P1+35)                                   
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,EOWSDAY                                                     
         BZ    ESTF72A                                                          
         BCTR  RE,0                                                             
         MHI   RE,3                                                             
         LA    RE,DAYTABL(RE)                                                   
         MVC   P1+22(3),0(RE)                                                   
*                                                                               
ESTF72A  DS    0H                                                               
         XC    SVLOCKYM,SVLOCKYM                                                
         OC    ELOCKYM,ELOCKYM         PRINT LOCK DATES                         
         BZ    ESTF72C                                                          
         MVC   WORK(L'ELOCKYM),ELOCKYM     MOVE YM TO WORK                      
         NI    WORK+1,X'FF'-X'80'-X'40'    TURN OFF FLAG BITS                   
         GOTO1 DATCON,DMCB,(3,WORK),(6,SVLOCKYM)                                
         TM    ELOCKMON,X'80'                                                   
         BNO   *+8                                                              
         MVI   SVLOCKYM+6,C'-'             TEST IF PRIOR                        
         TM    ELOCKMON,X'40'                                                   
         BNO   *+8                                                              
         MVI   SVLOCKYM+6,C'+'             TEST IF SUBSEQUENT                   
         MVC   P+45(5),=C'LOCK='                                                
         MVC   P+50(L'SVLOCKYM),SVLOCKYM                                        
*                                                                               
ESTF72C  AI    LINEED,2                                                         
         MVI   SPACING,2                                                        
         CLI   QOPT3,C'$'          SEE IF OMITTING DEMOS                        
         BE    ESTDOLS             YES SKIP TO DOLLARS                          
         MVI   SPACING,1                                                        
         MVI   P2,0                SO P3 WILL PRINT WHEN P2 NOT USED            
         CLI   QOPT5,C' '          SEE IF TURNAROUND                            
         BE    ESTF72D                                                          
         MVC   P2(5),=C'*ADD*'                                                  
         CLI   QOPT5,C'A'                                                       
         BE    ESTF72D                                                          
         MVC   P2+1(3),=C'CHA'                                                  
ESTF72D  DS    0H                                                               
*                                                                               
         MVC   P3+1(21),P1+22      MOVE DATES TO P3                             
         MVC   P1+22(37),SPACES    CLEAR ESTIMATE DATES & LOCK DATES            
         XC    DEMTBL,DEMTBL                                                    
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'                                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'      CANADIAN                                     
         BNE   ESTF72F                                                          
         CLI   SVCEXTRA,C'U'       USING US DEMOS                               
         BE    ESTF72F                                                          
         MVI   DBSELMED,C'C'                                                    
ESTF72F  DS    0H                                                               
         GOTO1 DEMOCON,DMCB,(14,EDEMLST),(13,DEMTBL),(C'S',DBLOCK),    X        
               (SPOTPROF+9,EUSRNMS)                                             
         LA    R5,DEMTBL                                                        
         LA    RF,EDEMLST                                                       
         LA    R6,P1+26        DISPLAY DEMOS IN P1 + P2 (IF NEEDED)             
ESTF73   LA    R4,8               10 DEMOS PER LINE                             
****73   LA    R4,10              10 DEMOS PER LINE                             
ESTF75   CLI   0(R5),C' '                                                       
         BNH   ESTF78              LAST DEMO                                    
         CLI   1(RF),X'21'         SEE IF DOING USER DEMO                       
         BNE   ESTF76                                                           
         MVC   0(3,R6),=C'U /'                                                  
         MVC   3(11,R6),0(R5)                                                   
         ZIC   R0,2(RF)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(1,R6),DUB+7(1)                                                 
         LA    RE,10(R6)                                                        
         LA    R6,12(R6)           USER DEMOS MAY NEED 11                       
         LA    R1,5                TRY TO SHORTEN IT TO 7                       
*                                                                               
ESTF75C  CLI   0(RE),C' '                                                       
         BH    ESTF77              NON-SPACE ENCOUNTERED                        
         BCTR  R6,0                DECREMENT R6                                 
         BCTR  RE,0                                                             
         BCT   R1,ESTF75C                                                       
         B     ESTF77                                                           
*                                                                               
ESTF76   MVC   0(11,R6),0(R5)                                                   
         LA    R6,13(R6)                                                        
*                                                                               
ESTF77   LA    R5,11(R5)                                                        
         LA    RF,3(RF)            NEXT DEMO                                    
         BCT   R4,ESTF75                                                        
*                                                                               
         CLI   0(R5),C' '          SEE IF DONE                                  
         BNH   ESTF78                                                           
         LA    R6,P2+26                                                         
         B     ESTF73                                                           
*                                                                               
ESTF78   LA    R5,P1+26                                                         
         CLI   0(R5),C' '                                                       
         BE    ESTF79                                                           
         LA    R5,P2+26                                                         
         CLI   0(R5),C' '                                                       
         BE    ESTF79                                                           
         LA    R5,P3+26                                                         
         AI    LINEED,1                                                         
*                                                                               
ESTF79   DS    0H                                                               
         OC    SVLOCKYM,SVLOCKYM                                                
         BZ    ESTF79C                                                          
         MVC   0(5,R5),=C'LOCK='                                                
         MVC   5(7,R5),SVLOCKYM                                                 
         LA    R5,11(R5)                                                        
         CLI   0(R5),C' '          CHECK IF LAST BYTE BEING USED                
         BNH   *+8                                                              
         LA    R5,1(R5)                                                         
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
*                                                                               
ESTF79C  MVC   0(5,R5),=C'BOOK='                                                
         CLI   QMED,C'N'           SEE IF NETWORK                               
         BNE   *+10                                                             
         MVC   0(5,R5),=C'HUTF='                                                
         MVC   5(6,R5),=C'LATEST'                                               
         OC    EBOOK,EBOOK                                                      
         BZ    ESTF79X                                                          
         GOTO1 DATCON,DMCB,(3,EBOOK),(6,5(R5))                                  
ESTF79X  DS    0H                                                               
         MVI   11(R5),C','                                                      
         LA    R5,12(R5)                                                        
*                                                                               
ESTF80   DS    0H                                                               
         MVC   0(4,R5),=C'HUT='                                                 
         CLI   EHUTADJ,0                                                        
         BNE   ESTF81                                                           
         MVC   4(4,R5),=C'AUTO'                                                 
         MVI   8(R5),C','                                                       
         LA    R5,9(R5)                                                         
         B     ESTF82                                                           
*                                                                               
ESTF81   DS    0H                                                               
         ZIC   R4,EHUTADJ                                                       
         SRL   R4,4                                                             
         MH    R4,=H'3'                                                         
         LA    R4,MONTHS-3(R4)                                                  
         MVC   4(3,R5),0(R4)                                                    
         LA    R5,7(R5)                                                         
         ZIC   R4,EHUTADJ                                                       
         SLL   R4,28                                                            
         SRL   R4,28                                                            
         LTR   R4,R4                                                            
         BZ    ESTF81X                                                          
         MH    R4,=H'3'                                                         
         LA    R4,MONTHS-3(R4)                                                  
         MVI   0(R5),C'-'                                                       
         MVC   1(3,R5),0(R4)                                                    
         LA    R5,4(R5)                                                         
ESTF81X  MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
ESTF82   MVC   0(9,R5),=C'DPT MENU='                                            
         MVC   9(1,R5),EDAYMENU                                                 
         MVI   10(R5),C','                                                      
         LA    R5,11(R5)                                                        
*                                                                               
         CLC   ERTLSCHM,SPACES                                                  
         BNH   ESTF83                                                           
         MVC   0(14,R5),=C'RETAIL SCHEME='                                      
         MVC   14(2,R5),ERTLSCHM                                                
         MVI   16(R5),C','                                                      
         LA    R5,17(R5)                                                        
ESTF83   OC    EBILLBAS(5),EBILLBAS                                             
         BZ    ESTF88                                                           
         MVC   0(13,R5),=C'BILL FORMULA='                                       
         MVI   13(R5),C'G'                                                      
         TM    EBILLBAS,X'10'                                                   
         BZ    *+8                                                              
         MVI   13(R5),C'N'                                                      
         MVI   14(R5),C'+'                                                      
         L     R2,EBILLCOM                                                      
         LTR   R2,R2                                                            
         BNM   *+8                                                              
         MVI   14(R5),C'-'                                                      
         EDIT  (R2),(7,15(R5)),4,ALIGN=LEFT,DROP=3                              
         LA    R4,21(R5)                                                        
ESTF84   CLI   0(R4),C' '                                                       
         BNE   ESTF85                                                           
         BCTR  R4,0                                                             
         B     ESTF84                                                           
*                                                                               
ESTF85   MVI   1(R4),C'G'                                                       
         TM    EBILLBAS,X'01'                                                   
         BZ    *+8                                                              
         MVI   1(R4),C'N'                                                       
         LR    R5,R4                                                            
         MVI   2(R5),C','                                                       
         LA    R5,3(R5)                                                         
*                                                                               
ESTF88   CLC   EREP(2),=2X'00'                                                  
         BE    ESTF90                                                           
         CLC   EREP(2),=C'  '                                                   
         BE    ESTF90                                                           
         MVC   0(4,R5),=C'REP='                                                 
         MVC   HALF,EREP                                                        
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R5),DUB                                                      
         MVI   7(R5),C','                                                       
         LA    R5,8(R5)                                                         
*                                                                               
ESTF90   CLI   ECPPEST,0                                                        
         BE    ESTF95                                                           
         MVC   0(8,R5),=C'CPP EST='                                             
         LA    R5,8(R5)                                                         
         OC    ECPPCLT,ECPPCLT                                                  
         BZ    ESTF92                                                           
         GOTO1 CLUNPK,DMCB,ECPPCLT,0(R5)                                        
         LA    R5,2(R5)                                                         
         CLI   0(R5),C' '                                                       
         BE    *+8                                                              
         LA    R5,1(R5)                                                         
         MVI   0(R5),C'/'                                                       
         LA    R5,1(R5)                                                         
ESTF92   ZIC   R0,ECPPEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R5),DUB                                                      
         MVI   3(R5),C','                                                       
         LA    R5,4(R5)                                                         
ESTF95   CLI   ECPPTYPE,0                                                       
         BE    ESTF99                                                           
         MVC   0(5,R5),=C'TYPE='                                                
         LA    R5,5(R5)                                                         
         CLI   ECPPTYPE,C'C'                                                    
         BNE   ESTF96                                                           
         MVC   0(3,R5),=C'CUT'                                                  
         MVI   3(R5),C','                                                       
         LA    R5,4(R5)                                                         
         B     ESTF99                                                           
*                                                                               
ESTF96   LA    R4,TYPTAB                                                        
ESTF97   CLC   ECPPTYPE,2(R4)                                                   
         BE    ESTF98                                                           
         LA    R4,3(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   ESTF97                                                           
         MVC   0(3,R5),=C'UNK'                                                  
         MVI   3(R5),C','                                                       
         LA    R5,4(R5)                                                         
         B     ESTF99                                                           
*                                                                               
ESTF98   MVC   0(2,R5),0(R4)                                                    
         MVI   2(R5),C','                                                       
         LA    R5,3(R5)                                                         
         B     ESTF99                                                           
ESTF99   DS    0H                                                               
         CLC   EPROF(3),=3C' '                                                  
         BNH   ESTF100                                                          
         MVC   0(8,R5),=C'FILTERS='                                             
         MVC   8(3,R5),EPROF                                                    
         LA    R5,11(R5)                                                        
*                                                                               
ESTF100  DS    0H                                                               
         BCTR  R5,0                                                             
         CLI   0(R5),C','               BLANK LAST COMMA                        
         BNE   *+8                                                              
         MVI   0(R5),C' '                                                       
         EJECT                                                                  
ESTDOLS  DS    0H             PRINT DOLLARS                                     
*                             SEE IF EST WILL FIT ON THIS PAGE                  
         ZIC   R0,LINE                                                          
         ZIC   R1,LINEED                                                        
         AR    R0,R1                                                            
         STC   R0,WORK                                                          
         CLC   WORK(1),MAXLINES                                                 
         BNH   ESTD3                                                            
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT1,C'N'         ONE PRD PER PAGE                              
         BE    ESTD3               YES                                          
         MVC   MID1(7),=C'PRODUCT'                                              
         MVC   MID1+8(3),SAVEPRD                                                
         MVC   MID1+12(20),SAVEPNAM                                             
         LA    RF,MID1+32                                                       
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   2(RF),C'('                                                       
         MVC   3(4,RF),SAVEPACC                                                 
         MVI   7(RF),C')'                                                       
         CLI   SAVEPACC,X'FF'                                                   
         BNE   ESTD                                                             
         OI    SAVEPACC+3,X'0F'                                                 
         UNPK  3(5,RF),SAVEPACC+1(3)                                            
         MVI   8(RF),C')'                                                       
ESTD     DS    0H                                                               
         MVC   MID2(11),=11C'-'                                                 
         CLI   MID1+10,C' '                                                     
         BNE   *+8                                                              
         MVI   MID2+10,C' '                                                     
         CLI   PPNTSW,1                                                         
         BNE   ESTD3                                                            
         LA    R1,MID1+39                                                       
ESTD1    CLI   0(R1),C' '                                                       
         BH    ESTD2                                                            
         BCTR  R1,0                                                             
         B     ESTD1                                                            
*                                                                               
ESTD2    MVC   2(11,R1),=C'(CONTINUED)'                                         
*                                                                               
         MVI   FORCEMID,C'Y'                                                    
*                                                                               
ESTD3    DS    0H                                                               
         MVI   PPNTSW,1            SET PRD PRINTED                              
         CLI   QOPT3,C'D'          SEE IF OMITTING $                            
         BNE   ESTD5                                                            
         MVC   P2+5(17),P3+5       MOVE DATES TO LINE 2                         
         MVC   P3+5(17),SPACES                                                  
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         LA    R5,P+26             POINTER TO P LINE                            
         BAS   RE,PG00             PRINT PGEST LINE                             
         B     ESTXX                                                            
*                                                                               
ESTD5    DS    0H                                                               
         CLI   DOLSW,1             CHK FOR $S                                   
         BE    ESTD10              YES                                          
*                                                                               
         LA    R5,P2+26                                                         
         CLI   QOPT3,C'$'          IF SUPPRESSING DEMOS,ETC                     
         BE    ESTD7               PUT MSG IN P2                                
         LA    R5,P3+26                                                         
         CLI   0(R6),C' '                                                       
         BE    *+8                                                              
         LA    R5,P4+26                                                         
*                                                                               
ESTD7    DS    0H                                                               
         BAS   RE,PG00                                                          
         CLC   P,SPACES                                                         
         BNE   *+8                                                              
         LA    R5,P+26                                                          
         MVC   0(19,R5),=C'*** NO ACTIVITY ***'                                 
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     ESTXX                                                            
*                                                                               
ESTD10   DS    0H                                                               
         LA    R5,P3+26                                                         
         SR    RE,RE                                                            
         LA    RE,103(RE)                                                       
ESTD11   CLI   0(R5),C' '                                                       
         BH    ESTD110                                                          
         LA    R5,1(R5)                                                         
         BCTR  RE,0                                                             
         OR    RE,RE                                                            
         BNZ   ESTD11                                                           
         LA    R5,P3+26                                                         
         B     ESTD12                                                           
*                                                                               
ESTD110  GOTO1 REPORT                                                           
         LA    R5,P+26                                                          
*                                                                               
ESTD12   BAS   RE,PG00                                                          
         MVC   P+26(103),MTHLN1                                                 
         MVC   P2+26(103),MTHLN2                                                
         GOTO1 REPORT                                                           
*                                                                               
         LHI   R3,NUMROWS                                                       
         SR    R5,R5               USED TO COUNT BILLING ROWS                   
         LA    R4,B4TOTS                                                        
         LA    R2,BTITLES                                                       
         LA    R6,9                FOR BCT                                      
*                                                                               
ESTD15   LR    R1,R4                                                            
         BAS   RE,TSTZERO                                                       
         BZ    ESTD18                                                           
         LA    R5,1(R5)                                                         
         B     ESTD19                                                           
*                                                                               
ESTD18   CLI   14(R2),C'Y'         CHK FOR ACTIVITY                             
         BNE   *+8                                                              
         LA    R5,1(R5)                                                         
*                                                                               
ESTD19   AHI   R4,NUMCOLS*8                                                     
         LA    R2,15(R2)           NEXT TITLE                                   
         BCT   R6,ESTD15                                                        
*                                                                               
         CHI   R5,1                SEE IF MORE THAN ONE ROW                     
         BH    *+6                                                              
         BCTR  R3,0                SO I WON'T GIVE TOTAL LINE                   
*                                                                               
         LA    R2,TITLES                                                        
         LA    R4,ATOTS                                                         
         LA    R8,ESTTOTS                                                       
*                                                                               
ESTD40   LR    R1,R4                                                            
         BAS   RE,TSTZERO                                                       
         BNZ   ESTD43                                                           
         CLI   14(R2),C'Y'         SEE IF ACTIVITY                              
         BNE   ESTD50              NO SKIP                                      
*                                                                               
ESTD43   MVC   P1+9(14),0(R2)                                                   
*                                                                               
         LA    R5,12                                                            
         LR    RE,R4                                                            
         LA    R6,P1+25                                                         
*                                                                               
ESTD45   ZAP   DUB,0(8,RE)                                                      
         SRP   DUB,64-2,5                                                       
         EDIT  (P8,DUB),(7,0(R6)),0,FLOAT=-                                     
         LA    RE,8(RE)                                                         
         LA    R6,8(R6)                                                         
         BCT   R5,ESTD45                                                        
*                                                                               
         ZAP   DUB,0(8,R8)                                                      
         SRP   DUB,64-2,5                                                       
         EDIT  (P8,DUB),(8,P+122),0,FLOAT=-                                     
         GOTO1 REPORT                                                           
*                                                                               
ESTD50   LA    R2,15(R2)                                                        
         AHI   R4,NUMCOLS*8                                                     
         LA    R8,8(R8)                                                         
         BCT   R3,ESTD40                                                        
*                                                                               
         ZIC   R3,LINE                                                          
         LA    R3,1(R3)                                                         
         STC   R3,X                                                             
         CLC   X(1),MAXLINES                                                    
         BH    ESTXX                                                            
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
ESTXX    BAS   RE,PRTUSER          PRINT USER DEFINITION FIELDS                 
*                                                                               
ESTDX    CLC   QEST(2),=C'NO'      SEE IF DOING FILTERED ESTS                   
         BNE   EXIT                NO DONE                                      
         B     ESTF1               YES GO FIND NEXT EST IN ESTLST               
         EJECT                                                                  
TSTZERO  LHI   R0,NUMCOLS                                                       
         CP    0(8,R1),=P'0'                                                    
         BNER  RE                                                               
         LA    R1,8(R1)                                                         
         BCT   R0,*-12                                                          
         BR    RE                                                               
         EJECT                                                                  
*==========================================================                     
* READ PGEST RECORD                                                             
*  INPUT : R5 - ADDRESS OF P1, P2, P3, OR P4                                    
*==========================================================                     
         SPACE 1                                                                
PG00     NTR1                                                                   
         ST    R5,APRTBUF          SAVE A(PRINT BUFFER)                         
         MVC   PGSAVE,KEY          SAVE LAST KEY READ                           
         LA    R4,KEY                                                           
         USING PGESTD,R4                                                        
         XC    0(PGKLENQ,R4),0(R4)                                              
         MVI   PGKRID,PGKNDIRQ                                                  
         MVI   PGKSID,PGKNDISQ                                                  
         L     R8,ADEST                                                         
         USING ESTHDR,R8                                                        
         MVC   PGKAM(7),EKEYAM                                                  
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PGEXIT                                                           
         MVC   AREC,ADBUY                                                       
         GOTO1 GET                                                              
*                                                                               
         L     R6,ADBUY                                                         
         USING PGSTELMD,R6                                                      
         MVI   ELCODE,PGSTEIDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PGEXIT                                                           
*                                                                               
PGLP     LA    R3,PGSTNAME                                                      
         LA    R2,L'PGSTNAME                                                    
         BAS   RE,STRLEN                                                        
         OR    R2,R2                                                            
         BZ    PG10                                                             
*                                                                               
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),PGSTNAME                                                 
         AR    R5,R2                                                            
         LA    R5,1(R5)                                                         
*                                                                               
PG10     MVI   0(R5),C'='                                                       
         LA    R5,1(R5)                                                         
         LA    R3,PGSTDATA                                                      
         LA    R2,L'PGSTDATA                                                    
         BAS   RE,STRLEN                                                        
         OR    R2,R2                                                            
         BZ    PG20                                                             
*                                                                               
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),PGSTDATA                                                 
         AR    R5,R2                                                            
         LA    R5,1(R5)                                                         
*                                                                               
PG20     MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         BAS   RE,NEXTEL                                                        
         BNE   PGXIT                                                            
         B     PGLP                                                             
*                                                                               
PGXIT    BCTR  R5,0                                                             
         CLI   0(R5),C','                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '          BLANK LAST COMMA                             
*                                                                               
         L     RE,APRTBUF                                                       
         SR    R5,RE                                                            
         CHI   R5,0                                                             
         BNH   PGEXIT                                                           
         GOTO1 REPORT                                                           
         MVC   KEY,PGSAVE                                                       
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
PGEXIT   DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   KEY,PGSAVE                                                       
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
*/                                                                              
*/       STRLEN - FIND LENGTH OF STRING                                         
*/        INPUT : R3 --> A(STRING),  R2 --> MAX LENGTH                          
*/       OUTPUT : R2 <-- L'STRING                                               
*/                                                                              
STRLEN   DS    0H                                                               
         AR    R3,R2               PAST END OF STRING                           
STRLP    BCTR  R3,0                BACKUP POINTER                               
         BCTR  R2,0                                                             
         OR    R2,R2                                                            
         BZ    STRXIT                                                           
         CLI   0(R3),C' '          NON SPACE CHAR?                              
         BNH   STRLP                                                            
STRXIT   LA    R2,1(R2)            RETURN ACTUAL LENGTH                         
         BR    RE                                                               
         EJECT                                                                  
*                                  BUILD ESTMLST                                
*                                  AND DELETE $ OUT OF REQ PERIOD               
*                                  AND BUILD TABLE OF MTHS TO IGNORE            
BLDMLST  NTR1                                                                   
         XC    ESTMLST,ESTMLST                                                  
         XC    DELMTHS,DELMTHS                                                  
         CLI   QMED,C'N'                                                        
         BNE   ESTD20                                                           
         XC    WORK,WORK           SEE IF NETPAK ESTIMATE                       
         MVC   WORK(4),=C'S000'                                                 
         MVC   WORK+4(2),QAGY                                                   
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),CLIENT                                                 
         GOTO1 GETPROF,DMCB,WORK,WORK+20,DATAMGR                                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVNETYM,WORK+29                                                  
         GOTO1 GETBROAD,DMCB,(1,ESTART),WORK,GETDAY,ADDAY                       
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,WORK+6,(3,WORK+12)                                   
         CLC   SVNETYM,WORK+12                                                  
         BNH   *+10                                                             
         XC    SVNETYM,SVNETYM                                                  
         CLI   SVNETYM,0                                                        
         BE    ESTD20                                                           
         MVC   WORK(6),ESTART      NETPAK EST                                   
         B     ESTD25                                                           
*                                                                               
ESTD20   DS    0H                                                               
         L     RF,ADCONLST                                                      
         L     RF,VBRDMON-SPADCONS(RF)                                          
         GOTO1 (RF),DMCB,ESTART,WORK                                            
*                                                                               
ESTD25   GOTO1 DATCON,DMCB,WORK,(3,SVSTARTB)                                    
         IC    RE,SVSTARTB                                                      
         LA    RE,1(RE)                                                         
         STC   RE,BYTE             SAVE START YEAR+1                            
*                                                                               
         LA    R0,12                                                            
         LA    R3,ESTMLST                                                       
         LA    R4,1                                                             
*                                                                               
ESTD27   MVC   FULL(1),SVSTARTB      ASSUME EST START YEAR                      
         STC   R4,FULL+1                                                        
         CLC   FULL+1(1),SVSTARTB+1  CURRENT MON TO EST START MON               
         BNL   *+10                  IF MONTH >= ESTART, USE SYR                
         MVC   FULL(1),BYTE          IF MONTH <  ESTART, USE SYR+1              
*                                                                               
         GOTO1 DATCON,DMCB,(3,FULL),WORK                                        
         MVC   0(4,R3),WORK                                                     
*                                                                               
         LA    R3,4(R3)                                                         
         LA    R4,1(R4)            NEXT MONTH                                   
         BCT   R0,ESTD27                                                        
*                                                                               
         LA    R0,12                                                            
         LA    R3,ESTMLST                                                       
         LA    R4,MTHLN1                                                        
*                                                                               
ESTD29   MVC   WORK(4),0(R3)                                                    
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,WORK,(6,(R4))                                        
         MVC   L'MTHLN1(6,R4),=C'------'                                        
*                                                                               
         LA    R3,4(R3)            NEXT YYMM                                    
         LA    R4,8(R4)            NEST PRINT POSITION                          
         BCT   R0,ESTD29                                                        
*                                                                               
         MVC   MTHLN1+98(5),=C'TOTAL'                                           
         MVC   MTHLN2+98(5),=5C'-'                                              
         CLI   QOPT4,C'Y'          SEE IF SHOWING REQ MTHS ONLY                 
         BNE   BLDMX                                                            
*                                                                               
         CLC   QSTAUTO(2),=C'ES'                                                
         BE    BLDMX                                                            
         MVC   WORK(12),QSTART                                                  
*                                                                               
         LA    R1,DELMTHS                                                       
         LA    R2,ESTMLST                                                       
         LA    R5,12                                                            
*                                                                               
BLDM8    CLC   0(4,R2),WORK                                                     
         BL    BLDM10                                                           
         CLC   0(4,R2),WORK+6                                                   
         BH    BLDM10                                                           
         B     BLDM15                                                           
*                                                                               
BLDM10   MVI   0(R1),1                                                          
*                                                                               
BLDM15   LA    R1,1(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R5,BLDM8                                                         
*                                                                               
BLDM20   DS    0H                  DELETE EST $ OUT OF REQ PERIOD               
         LA    R1,EAUTH                                                         
         LA    R2,DELMTHS                                                       
         LHI   R5,NUMCOLS                                                       
*                                                                               
BLDM25   CLI   0(R2),1                                                          
         BNE   *+10                                                             
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R5,BLDM25                                                        
*                                                                               
         LA    R1,EORD                                                          
         LA    R2,DELMTHS                                                       
         LHI   R5,NUMCOLS                                                       
*                                                                               
BLDM30   CLI   0(R2),1                                                          
         BNE   BLDM35                                                           
         ZAP   0(6,R1),=P'0'                                                    
**NOP**  ZAP   78(6,R1),=P'0'                                                   
*                                                                               
BLDM35   LA    R1,6(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R5,BLDM30                                                        
*                                                                               
         LA    R1,EPAID                                                         
         LA    R2,DELMTHS                                                       
         LA    R5,NUMCOLS                                                       
*                                                                               
BLDM40   CLI   0(R2),1                                                          
         BNE   BLDM45                                                           
         ZAP   0(6,R1),=P'0'                                                    
**NOP**  ZAP   78(6,R1),=P'0'                                                   
*                                                                               
BLDM45   LA    R1,6(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R5,BLDM40                                                        
*                                                                               
BLDMX    B     EXIT                                                             
         EJECT                                                                  
*============================================================                   
*        PRINT OUT USER DEFINITION FIELDS                                       
*============================================================                   
         SPACE 1                                                                
PRTUSER  NTR1                                                                   
         CLC   CUSER1,SPACES                                                    
         BNH   PU10                                                             
         MVC   P+5(20),CUSER1                                                   
         MVC   P+26(32),USER1                                                   
         GOTO1 REPORT                                                           
*                                                                               
PU10     CLC   CUSER2,SPACES                                                    
         BNH   PU20                                                             
         MVC   P+5(20),CUSER2                                                   
         MVC   P+26(16),USER2                                                   
         GOTO1 REPORT                                                           
*                                                                               
PU20     CLC   CUSER1,SPACES                                                    
         BH    PU30                                                             
         CLC   CUSER2,SPACES                                                    
         BNH   PUX                                                              
*                                                                               
PU30     GOTO1 REPORT                                                           
*                                                                               
PUX      B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
*        HEADHOOK                                                               
         CNOP  0,4                                                              
         USING *,RF                                                             
ESTHDHK  NTR1                                                                   
         LM    R7,RC,HDHKR7                                                     
         B     HDHK2                                                            
*                                                                               
HDHKR7   DC    6F'0'                                                            
         DROP  RF                                                               
*                                                                               
HDHK2    DS    0H                                                               
         CLI   QOPT1,C'N'          CHK ONE PRD PER PAGE                         
         BNE   HDHK5                                                            
         MVI   H5+34,C'('                                                       
         MVC   H5+35(4),SAVEPACC                                                
         MVI   H5+39,C')'                                                       
         CLI   SAVEPACC,X'FF'                                                   
         BNE   HDHK5                                                            
         OI    SAVEPACC+3,X'0F'                                                 
         UNPK  H5+35(5),SAVEPACC+1(3)                                           
         MVI   H5+40,C')'                                                       
*                                                                               
HDHK5    DS    0H                                                               
*                                                                               
         CLI   QOPT7,C'O'          SEE IF FILTERING OLD OR NEW                  
         BNE   HDHK6                                                            
         MVC   H5+59(5),=C'*OLD*'                                               
         B     HDHK8                                                            
*                                                                               
HDHK6    CLI   QOPT7,C'N'                                                       
         BNE   HDHK8                                                            
         MVC   H5+59(5),=C'*NEW*'                                               
*                                                                               
HDHK8    CLC   QSTAUTO(2),=C'ES'                                                
         BE    HDHK12                                                           
         CLC   QSTART+4(2),SPACES                                               
         BNE   HDHK10                                                           
         MVC   H4+49(28),=C'PERIOD FROM MMM/YY TO MMM/YY'                       
         MVC   DUB(6),QSTART                                                    
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,(0,DUB),(6,H4+61)                                    
         MVC   DUB(6),QEND                                                      
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 (RF),(R1),(0,DUB),(6,H4+71)                                      
         B     HDHK12                                                           
*                                                                               
HDHK10   EQU   *                                                                
         MVC   H4+47(32),=C'PERIOD FROM MMMDD/YY TO MMMDD/YY'                   
         GOTO1 DATCON,DMCB,(0,QSTART),(5,H4+59)                                 
         GOTO1 (RF),(R1),(0,QEND),(5,H4+71)                                     
*                                                                               
HDHK12   CLI   Q2OPT1,C'Y'         TEST OOWR ESTIMATES ONLY                     
         BNE   HDHKX                                                            
         MVC   H5+47(32),=C'** OUT OF WEEK ESTIMATES ONLY **'                   
*                                                                               
HDHKX    B     EXIT                                                             
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
APRTBUF  DS    A                                                                
ELCODE   DS    XL1                                                              
PGSAVE   DS    CL20                                                             
CUSER1   DS    CL20                                                             
CUSER2   DS    CL20                                                             
USER1    DS    CL32                                                             
USER2    DS    CL16                                                             
*                                                                               
TYPTAB   DS    0C                                                               
         DC    C'M$',X'03'                                                      
         DC    C'M%',X'04'                                                      
         DC    C'Q$',X'05'                                                      
         DC    X'FF'                                                            
*                                                                               
*             BTITLE+14 HAS BILLING ACTIVITY SWITCH - 'Y' =YES                  
*                                                                               
*                                                                               
TITLES   DS    0C                                                               
         DC    CL14'    AUTHORIZED',X'00'                                       
         DC    CL14'       ORDERED',X'00'                                       
         DC    CL14'       CLEARED',X'00'                                       
BTITLES  DC    CL14'     B4 BILLED',X'00'                                       
         DC    CL14'     B5 BILLED',X'00'                                       
         DC    CL14'     B6 BILLED',X'00'                                       
         DC    CL14'     B7 BILLED',X'00'                                       
         DC    CL14'     B8 BILLED',X'00'                                       
         DC    CL14'     B9 BILLED',X'00'                                       
OTITLE   DC    CL14'  OTHER BILLED',X'00'                                       
STITLE   DC    CL14'SUMMARY BILLED',X'00'                                       
DTITLE   DC    CL14' DETAIL BILLED',X'00'                                       
         DC    CL14'  TOTAL BILLED',X'00'                                       
         EJECT                                                                  
ESTWORKD DSECT                                                                  
*                                                                               
NUMROWS  EQU   13                  NUMBER OF ACCUMULATOR ROWS                   
NUMCOLS  EQU   12                  NUMBER OF ACCUMULATOR COLS                   
         DS    0D                                                               
ATOTS    DS    12PL8               AUTH                                         
OTOTS    DS    12PL8               ORDERED TOTALS                               
CTOTS    DS    12PL8               CLEARED                                      
B4TOTS   DS    12PL8               B4 BILLING                                   
B5TOTS   DS    12PL8               B5 BILLING                                   
B6TOTS   DS    12PL8               B6 BILLING                                   
B7TOTS   DS    12PL8               B7 BILLING                                   
B8TOTS   DS    12PL8               B8 BILLING                                   
B9TOTS   DS    12PL8               B9 BILLING                                   
BATOTS   DS    12PL8               OTHER NEW BILLING                            
SUMTOTS  DS    12PL8               OLD STYPE SUMMARY                            
DETTOTS  DS    12PL8               OLD STYLE DETAIL BILLING                     
*                                                                               
*                                                                               
TOTBILL  DS    12PL8               TOTAL BILLING ACCUM                          
*                                                                               
*                                                                               
TOTNUM   EQU   13                  NUMBER OF TOTAL ROWS BELOW                   
ESTTOTS  DS    0D                                                               
EATOTAL  DS    PL8                 AUTH TOTAL                                   
EOTOTAL  DS    PL8                 ORDERED TOTAL                                
ECTOTAL  DS    PL8                 CLEARED TOTAL                                
EB4TOTAL DS    PL8                 BILLED TOTAL                                 
EB5TOTAL DS    PL8                 BILLED TOTAL                                 
EB6TOTAL DS    PL8                 BILLED TOTAL                                 
EB7TOTAL DS    PL8                 BILLED TOTAL                                 
EB8TOTAL DS    PL8                 BILLED TOTAL                                 
EB9TOTAL DS    PL8                 BILLED TOTAL                                 
EBATOTAL DS    PL8                 BILLED TOTAL                                 
ESUMTOT  DS    PL8                 SUMMARY BILLED TOTAL                         
EDETTOT  DS    PL8                 DETAIL BILLED TOTAL                          
*                                                                               
EBILTOT  DS    PL8                 TOTAL BILLED                                 
*                                                                               
SAVESKEY DS    CL13                                                             
SAVEKEY  DS    CL13                                                             
SAVEPRD  DS    CL3                                                              
SAVEPNAM DS    CL20                                                             
SAVEPACC DS    CL4                 SAVED CLT NUMBER                             
SVLOCKYM DS    CL7                 SAVE LOCK YEAR/MONTH                         
SVAPROF  DS    CL20                SAVED AGY PROFILE                            
SVCEXTRA DS    CL15                SAVED CEXTRA                                 
SVSTARTB DS    XL3                                                              
*                                                                               
DISP     DS    CL1                 DISP INTO ESTLST FOR FILTERED ESTS           
DOLSW    DS    CL1                                                              
PPNTSW   DS    CL1                                                              
SVNETYM  DS    CL2                 NONE ZERO IF NETPAK EST                      
LINEED   DS    CL1                                                              
X        DS    CL30                                                             
*                                                                               
ESTMLST  DS    CL48                EST MTH LIST IN BUCKET ORDER                 
*                                                                               
DELMTHS  DS    CL12                12 MTHS X'01' MEANS BYPASS THIS MTH          
*                                                                               
MTHLN1   DS    CL106                                                            
MTHLN2   DS    CL106                                                            
*                                                                               
DEMTBL   DS    CL220               USED BY DEMOCON - 20 DEMO DESCS              
       ++INCLUDE DEDBLOCK                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
*                                                                               
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BILLHDRD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
       ++INCLUDE SPGENPGEST                                                     
*                                                                               
QOPT7    EQU   QOPT5+2                                                          
Q2OPT1   EQU   Q2USER              Q2/COL21 = OOWR ESTIMATES ONLY               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SPREPL202 02/23/15'                                      
         END                                                                    
