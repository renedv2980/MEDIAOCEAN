*          DATA SET SPREPKA02  AT LEVEL 174 AS OF 03/05/18                      
*PHASE SPKA02A                                                                  
*INCLUDE SORTER                                                                 
*================================================================*              
*                                                                *              
* THIS PROGRAM READS GOAL RECORDS AND DELETES THE X'21' ELEMENTS *              
*    WITHIN THE REQUESTED DATES                                  *              
*                                                                *              
*      QOPT1   = Y TEST RUN - DO NOT MARK FILE                   *              
*      QOPT2   = Y PRINT SORT TRACE                              *              
*                                                                *              
* IF Q2PRD IS > C'   ' THERE IS A LIST OF PRODUCTS THAT THE      *              
*    DELETED GOALS WILL BE REALLOCATED TO                        *              
*================================================================*              
         EJECT                                                                  
         TITLE 'SPKA02 - UNGOAL REPORT'                                         
SPKA02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPKA02,R8                                                      
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         STM   R9,RB,REGSTR                                                     
         LA    R1,HEADHK                                                        
         ST    R1,HEADHOOK                                                      
         SPACE 2                                                                
*                                                                               
MODE00   DS    0H                                                               
         CLI   MODE,PROCGOAL                                                    
         BE    PROCGL                                                           
         CLI   MODE,REQFRST                                                     
         BE    M00                                                              
         CLI   MODE,ESTFRST                                                     
         BE    M10                                                              
         CLI   MODE,MGR1FRST                                                    
         BE    M20                                                              
         CLI   MODE,PRDLAST                                                     
         BE    M30                                                              
         CLI   MODE,MKTFRST                                                     
         BE    M40                                                              
         CLI   MODE,MKTLAST                                                     
         BE    M50                                                              
         CLI   MODE,REQLAST                                                     
         BE    M100                                                             
         SPACE                                                                  
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
M00      DS    0H                  REQFRST                                      
         BRAS  RE,GETG0PRF         READ G0 PROF AND SET 2-DEC FLAG              
*                                                                               
M01      MVI   FORCEHED,C'Y'                                                    
         XC    RECTOTS,RECTOTS                                                  
         XC    MKTTOTS,MKTTOTS                                                  
         XC    PRDTOTS,PRDTOTS                                                  
         MVI   AGERR,0                                                          
         MVI   RCSUBPRG,0                                                       
         XC    BRDLIST,BRDLIST                                                  
         XC    BRDTOTSH,BRDTOTSH                                                
         CLI   Q2PRD1,C' '         TEST ALLOCATE TO NEW PRD                     
         BNH   EXIT                NO - DONE                                    
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
* EDIT LIST OF PRDS TO BE ALLOCATED                                             
         LA    R4,BRD1                                                          
         LA    R5,Q2PRD1                                                        
         LA    R3,4                                                             
*                                                                               
M02      DS    0H                                                               
         IC    RE,RCSUBPRG                                                      
         LA    RE,1(RE)            INCREMENT PRD COUNT                          
         STC   RE,RCSUBPRG                                                      
*                                                                               
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         LA    R6,CLIST                                                         
         DROP  R6                                                               
*                                                                               
         CLC   =C'***',0(R5)       DELETE THIS PORTION?                         
         BNE   M02A                                                             
         MVC   0(3,R4),0(R5)                                                    
         B     M02B1                                                            
M02A     CLC   0(3,R5),0(R6)                                                    
         BE    M02B                                                             
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BH    M02A                                                             
         DC    H'0'                                                             
*                                                                               
M02B     MVC   0(4,R4),0(R6)       MOVE EBCDIC AND BINARY CODES                 
*                                                                               
M02B1    LA    R1,3(R5)                                                         
         LA    R0,3                                                             
*                                                                               
M02C     CLI   0(R1),C'0'                                                       
         BL    BADWGT                                                           
         CLI   0(R1),C'9'                                                       
         BH    BADWGT                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,M02C                                                          
*                                                                               
         PACK  DUB,3(3,R5)         PACK WEIGHT                                  
         CVB   R0,DUB                                                           
         ST    R0,4(R4)            SET WEIGHT                                   
*                                                                               
* MAKE SURE EST EXISTS FOR NEW PRD'S                                            
         CLC   =C'***',0(R5)       SKIP TEST FOR DELETE                         
         BE    M02C1                                                            
         L     R1,ADCLT                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),1(R1)      AGYMD/CLT                                    
         MVC   KEY+4(3),0(R5)      PRD                                          
         PACK  DUB,QEST                                                         
         CVB   R1,DUB                                                           
         STC   R1,KEY+7            EST                                          
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BE    M02C1                                                            
         MVC   P(80),QAREA                                                      
         MVC   P2(80),QAREA2                                                    
         MVC   P3(35),=C'** EST DOESN''T EXIST FOR PRD XXX **'                  
         MVC   P3+29(3),0(R6)                                                   
         GOTO1 REPORT                                                           
         GOTO1 =V(SORTER),DMCB,=C'END',(R5)                                     
         GOTO1 AENDREQ                                                          
*                                                                               
M02C1    LA    R4,8(R4)                                                         
         LA    R5,6(R5)                                                         
         CLC   0(3,R5),SPACES                                                   
         BE    *+8                                                              
         BCT   R3,M02                                                           
*                                                                               
* GET SUM OF WEIGHTS & HIGHEST WEIGHT                                           
         LA    R1,BRDLIST                                                       
         LLC   R0,RCSUBPRG                                                      
         SR    RE,RE                                                            
         XC    FULL,FULL                                                        
*                                                                               
M02D     L     RF,4(R1)                                                         
         AR    RE,RF                                                            
         C     RF,FULL             THIS WEIGHT > PREV HIGH?                     
         BNH   M02E                 NO                                          
         ST    RF,FULL             MAKE IT THE NEW HIGH & SAVE IT'S #           
         LA    R3,4                                                             
         SR    R3,R0                                                            
         STC   R3,BRDHIGH                                                       
M02E     LA    R1,BRDLEN(R1)                                                    
         BCT   R0,M02D                                                          
         ST    RE,BRDTOTSH                                                      
*                                                                               
         B     EXIT                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,12,A),FORMAT=BI'                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=194'                                   
*                                                                               
BADWGT   MVC   P(23),=C'ALL WEIGHTS NOT NUMERIC'                                
         MVC   P+25(80),QAREA                                                   
         MVC   P2+25(80),QAREA2                                                 
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
M10      DS    0H                 *ESFRST - SET QSTART TO PREV MON              
         SPACE                                                                  
         GOTO1 GETDAY,DMCB,QSTART,WORK                                          
         LLC   R0,0(R1)                     NUM OF DAY OF WEEK TO R0            
         LA    R3,1                                                             
         SR    R3,R0                    NEG NUM TO ADD TO QSTART IN R3          
         GOTO1 ADDAY,DMCB,QSTART,WORK,(R3)             WORK = PREV MON          
         SPACE                                                                  
M11      GOTO1 DATCON,DMCB,(0,WORK),(2,BQSTARTP)                                
         GOTO1 DATCON,DMCB,(0,QEND),(2,BQENDP)                                  
         SPACE                                                                  
         CLC   =C'POL',QPRD                       SET PRODUCT                   
         BNE   M12                                                              
         MVC   P(80),QAREA                                                      
         MVC   P+82(32),=C'** PRD=POL IS INVALID REQUEST **'                    
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         SPACE                                                                  
M12      CLC   =CL3' ',Q2SLN       SPOTLEN                                      
         BE    M16                                                              
         LA    R5,3                                                             
         LA    R6,Q2SLN                                                         
M14      CLI   0(R6),C'0'                                                       
         BL    SPERR                                                            
         CLI   0(R6),C'9'                                                       
         BH    SPERR                                                            
         LA    R6,1(R6)                                                         
         BCT   R5,M14                                                           
         PACK  DUB,Q2SLN(3)                                                     
         CVB   R0,DUB                                                           
         STC   R0,BSLN                                                          
         SPACE                                                                  
M16      CLC   =C'ALL',QMKT        MKT=ALL                                      
         BNE   M18                                                              
         MVI   MKTOTSW,C'Y'                                                     
M18      MVI   FORCEHED,C'Y'                                                    
         CLC   =CL3'NO',QEST       DOES EST=NO                                  
         BE    M19                                                              
         CLI   QESTEND,C' '        DOES EST=A RANGE                             
         BE    M19A                NO.                                          
M19      MVI   ESTOTSW,C'Y'                                                     
         SPACE                                                                  
M19A     XC    TOTALS,TOTALS       CLEAR TOTALS                                 
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
M20      DS    0H                 *MGR1FRST                                     
         CLI   QMGR,C' '                                                        
         BE    EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVI   MKTOTSW,C'Y'                                                     
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
M30      DS    0H                 *PRDLAST                                      
         CP    ESTCNT,=P'0'                                                     
         BE    EXIT                                                             
         BAS   RE,FNLTOTS                                                       
         ZAP   ESTCNT,=P'0'                                                     
* IF REALLOCATING DELETED GOALS, PROCESS OTHER PRODUCTS NOW                     
         CLI   BRD1,C' '                                                        
         BNH   EXIT                                                             
         BAS   RE,ADDGLS                                                        
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
M40      DS    0H                 *MKTFRST                                      
         MVI   FRSTSW,C'Y'         SET FIRST TIME SWITCH                        
         ZAP   RECCNT,=P'0'                                                     
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
M50      DS    0H                 *MKTLAST                                      
         CLI   ESTOTSW,C'Y'        ARE THERE ESTOTS                             
         BNE   M55                                                              
         MVI   BESTSV,0            YES. FORCE EST UNEQUAL                       
         BAS   RE,ESTOTS                                                        
M55      BAS   RE,MKTOTS                                                        
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
M100     DS    0H                  REQLAST                                      
         CLI   Q2PRD1,C' '         TEST ANY RE-ALLOC                            
         BNH   EXIT                NO - DONE                                    
         GOTO1 =V(SORTER),DMCB,=C'END',(R5)                                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
PROCGL   DS    0H                  *PROCGL                                      
         L     R7,ADGOAL                                                        
         USING GOALRECD,R7                                                      
         SPACE                                                                  
         CLI   Q2DPT,C' '          DAYPART                                      
         BE    PROC10                                                           
         CLC   Q2DPT,GKEYDPT                                                    
         BNE   EXIT                                                             
         SPACE                                                                  
PROC10   CLC   =CL3' ',Q2SLN       SPOTLEN                                      
         BE    PROC20                                                           
         CLC   BSLN,GKEYSLN                                                     
         BNE   EXIT                                                             
         SPACE 2                                                                
PROC20   LA    R6,24(R7)           GET '21' ELEMENT                             
         MVI   ELCODE,X'21'                                                     
         USING GLEMENT,R6                                                       
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         SPACE                                                                  
PROC25   MVI   DELSW,C'N'          SET TO PROCESS ELEMS                         
         CLI   FRSTSW,C'Y'         FIRST TIME                                   
         BNE   *+10                                                             
         MVC   BESTSV,GKEYEST                                                   
         MVI   FRSTSW,C'N'                                                      
         BAS   RE,ESTOTS           EST TOT BREAK                                
         SPACE                                                                  
PROC30   CLC   GLWEEK,BQSTARTP     CHECK DATE SPREAD                            
         BL    PROC50                                                           
         CLC   GLWEEK,BQENDP                                                    
         BH    PROC50                                                           
         CLI   DELSW,C'Y'                                                       
         BE    PROC35                                                           
         MVC   STDTSV,GLWEEK       SAVE BEG/END DATES                           
*                                                                               
PROC35   MVC   ENDTSV,GLWEEK                                                    
         SPACE                                                                  
PROC40   CLI   Q2PRD1,C' '         TEST REALLOCATION                            
         BNH   PROC45               NO                                          
         CLC   GKEYSLN,GKEYSEC     MAKE SURE NOT A P/B                          
         BE    PROC45                                                           
         SR    R0,R0                                                            
         ICM   R0,3,GKEYMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PMKT,DUB                                                         
         MVC   PMKNM,MKTNM                                                      
         EDIT  (1,GKEYEST),(3,PEST)                                             
         MVC   PDPT,GKEYDPT                                                     
         SR    R0,R0                                                            
         ICM   R0,1,GKEYDPT+1                                                   
         EDIT  (R0),(3,PSLN),ALIGN=LEFT                                         
         MVC   PSDTE(21),=C'** P/B NOT ALLOWED **'                              
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE                                                                  
PROC45   L     R0,DOLTOT           ADD UP DOLLAR/POINT TOTS                     
         ICM   R1,15,GLBUDGET                                                   
         AR    R0,R1                                                            
         ST    R0,DOLTOT                                                        
*                                                                               
         L     R0,PTOT                                                          
         MVC   FULL,GLGRP                                                       
         BRAS  RE,ROUND                                                         
         A     R0,FULL                                                          
         ST    R0,PTOT                                                          
*                                                                               
         LLC   R1,1(R6)            SAVE ELEM FOR SRGEN                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
*                                                                               
         CLI   QOPT2,C'Y'                                                       
         BNE   PROC47                                                           
         MVC   P(11),=C'DELETE WEEK'                                            
         GOTO1 DATCON,DMCB,(2,2(R6)),P+12                                       
         GOTO1 REPORT                                                           
*                                                                               
PROC47   GOTO1 RECUP,DMCB,(R7),(R6),0         DELETE ELEMENT                    
         MVI   DELSW,C'Y'                                                       
*                                                                               
         CLI   Q2PRD1,C' '         TEST REALLOCATION                            
         BNH   *+8                                                              
         BAS   RE,SRGEN            GENERATE SORT RECORDS                        
*                                                                               
         BAS   RE,NXT10                                                         
         B     *+8                                                              
         SPACE                                                                  
PROC50   BAS   RE,NEXTEL                                                        
         BE    PROC30                                                           
         CLI   DELSW,C'Y'          END OF ELEMS, ANY DELETED                    
         BNE   EXIT                NO. EXIT                                     
         SPACE                                                                  
         MVC   AREC,ADGOAL                                                      
         CLI   RCWRITE,C'N'        IF WRITE=NO                                  
         BNE   *+12                                                             
         BAS   RE,PRTPUT           THEN TEST TO PRINT 'PUTREC'                  
         B     PROC54                                                           
*                                                                               
         CLI   QOPT1,C'Y'          TEST USER OPTION TO NOT MARK FILE            
         BE    PROC54                                                           
         GOTO1 PUT                 NO.WRITE RECORD                              
*                                                                               
PROC54   DS    0H                                                               
         DROP  R6                                                               
         SPACE 2                                                                
         MVC   PMKT,MKT                                                         
         MVC   PMKNM,MKTNM                                                      
         EDIT  (1,GKEYEST),(3,PEST)                                             
         MVC   PDPT,GKEYDPT                                                     
         EDIT  (1,GKEYSLN),(3,PSLN),ZERO=BLANK,ALIGN=LEFT                       
         GOTO1 DATCON,DMCB,(2,STDTSV),(8,PSDTE)                                 
         GOTO1 DATCON,DMCB,(2,ENDTSV),(8,PEDTE)                                 
         EDIT  (4,DOLTOT),(10,PDOLS),2,ZERO=BLANK                               
         L     R0,PTOT                                                          
         BRAS  RE,DOPTS                                                         
         MVC   PPTS,PAPTS          MOVE TO CORRECT PRINT PSON                   
         MVC   PAPTS,SPACES                                                     
*                                                                               
         GOTO1 REPORT                                                           
         AP    RECCNT,=P'1'                                                     
         AP    ESTCNT,=P'1'                                                     
         SPACE                                                                  
         CLI   ESTOTSW,C'Y'                                                     
         BNE   PROC60                                                           
         L     R0,ESTDOLS          ADD UP EST TOTALS                            
         A     R0,DOLTOT                                                        
         ST    R0,ESTDOLS                                                       
         L     R0,ESTPTS                                                        
         A     R0,PTOT                                                          
         ST    R0,ESTPTS                                                        
         SPACE                                                                  
PROC60   DS    0H                                                               
         L     R0,MKTDOLS          ADD UP MKT TOTALS                            
         A     R0,DOLTOT                                                        
         ST    R0,MKTDOLS                                                       
         SPACE                                                                  
PROC70   XC    DOLTOT,DOLTOT                                                    
         XC    PTOT,PTOT                                                        
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
*=============================================================*                 
* IF GOALS ARE BEING REALLOCATED, GENERATE SORT DATA          *                 
*=============================================================*                 
         SPACE 1                                                                
SRGEN    NTR1                                                                   
         XC    TOTBDGT,TOTBDGT                                                  
         XC    TOTGRP,TOTGRP                                                    
*                                                                               
SRGEN10  XC    MYREC,MYREC                                                      
         LA    R5,MYREC                                                         
         USING SRRECD,R5                                                        
*                                                                               
         LA    R6,ELEM             COPY OF DELETED ELEMENT                      
         USING GLEMENT,R6                                                       
*                                                                               
         LA    R1,BRDLIST                                                       
         LLC   R0,RCSUBPRG         GET NUMBER OF REALLOCATION PRDS              
*                                                                               
SRGEN20  MVC   SRMKT,GKEYMKT                                                    
         MVC   SREST,GKEYEST                                                    
         MVC   SRMKNM,MKTNM                                                     
         MVC   SRDPTSLN(2),GKEYDPT                                              
         MVC   SRSTDAY,GDSTDAY                                                  
         MVC   SRQPRD,0(R1)                                                     
         MVC   SRPRD,3(R1)                                                      
         MVC   SRWEEK,GLWEEK                                                    
         MVC   SRCPPCL,GDCPPCL     SAVE CPP CLIENT                              
         MVC   SRCPPES,GDCPPES      AND ESTIMATES                               
*                                                                               
         SR    RE,RE               COMPUTE THE NEW ALLOCATION OF POINTS         
         MVC   FULL,GLGRP                                                       
         BRAS  RE,ROUND                                                         
         L     RF,FULL                                                          
*                                                                               
         M     RE,4(R1)            POINTS * SHARE                               
         SLDA  RE,1                                                             
         D     RE,BRDTOTSH         / TOTAL SHARES                               
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,SRPTS            = NEW POINTS                                 
         A     RF,TOTGRP                                                        
         ST    RF,TOTGRP                                                        
*                                                                               
         SR    RE,RE               COMPUTE THE NEW ALLOCATION OF $$$            
         ICM   RF,15,GLBUDGET                                                   
         M     RE,4(R1)            $$$ * SHARE                                  
         SLDA  RE,1                                                             
         D     RE,BRDTOTSH         / TOTAL SHARES                               
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,SRDOLS           = NEW $$$                                    
         A     RF,TOTBDGT                                                       
         ST    RF,TOTBDGT                                                       
*                                                                               
         LA    R1,8(R1)                                                         
         LA    R5,SRRECLEN(R5)                                                  
         BCT   R0,SRGEN20                                                       
*                                                                               
* IF TOTALS DON'T ADD UP TO ORIGINAL, ADD REMAINDER TO LARGEST SHARE            
* OR FIRST IF ALL SAME                                                          
         LLC   R1,BRDHIGH                                                       
         MH    R1,=AL2(SRRECLEN)                                                
         LA    R5,MYREC(R1)                                                     
*                                                                               
         MVC   FULL,GLGRP                                                       
         BRAS  RE,ROUND                                                         
         CLC   FULL,TOTGRP                                                      
         BE    SRGEN30                                                          
         L     RF,FULL             GET OLD POINTS                               
         S     RF,TOTGRP            MINUS NEW POINTS                            
         A     RF,SRPTS            ADD TO CURRENT SHARE                         
         ST    RF,SRPTS                                                         
*                                                                               
SRGEN30  CLC   GLBUDGET,TOTBDGT                                                 
         BE    SRGEN40                                                          
         ICM   RF,15,GLBUDGET       GET OLD $$$                                 
         S     RF,TOTBDGT           MINUS NEW $$$                               
         A     RF,SRDOLS           ADD TO CURRENT $$$                           
         ST    RF,SRDOLS                                                        
*                                                                               
SRGEN40  LA    R5,MYREC                                                         
         LLC   R0,RCSUBPRG                                                      
*                                                                               
SRGEN50  GOTO1 =V(SORTER),DMCB,=C'PUT',(R5)                                     
         CLI   QOPT2,C'Y'                                                       
         BNE   SRGEN52                                                          
         MVC   P(7),=C'SORTPUT'                                                 
         MVC   P+8(SRRECLEN),0(R5)                                              
         GOTO1 REPORT                                                           
*                                                                               
SRGEN52  LA    R5,SRRECLEN(R5)                                                  
         BCT   R0,SRGEN50                                                       
         B     EXIT                                                             
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
*==============================================================*                
* READ SORT RECORDS AND ADD/CHANGE GOAL RECORDS AS NEEDED      *                
*==============================================================*                
         SPACE 1                                                                
ADDGLS   NTR1                                                                   
         L     R7,ADGOAL                                                        
         ST    R7,AREC                                                          
         USING GOALRECD,R7                                                      
*                                                                               
         XC    THISREC,THISREC                                                  
*                                                                               
AG2      MVC   LASTREC,THISREC     SAVE PREVIOUS RECORD                         
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R5,15,4(R1)                                                      
         BZ    AG4                                                              
         CLI   QOPT2,C'Y'          TEST TO TRACE SORT                           
         BNE   AG6                                                              
         MVC   P(L'THISREC),0(R5)                                               
         GOTO1 REPORT                                                           
         B     AG6                                                              
*                                                                               
         USING SRRECD,R5                                                        
AG4      MVC   THISREC(4),=F'-1'    SET E-O-F FLAG                              
         B     *+10                                                             
*                                                                               
AG6      MVC   THISREC,0(R5)       MOVE SORTREC TO VISIBLE STORAGE              
         LA    R5,THISREC                                                       
         CLC   THISREC+3(6),LASTREC+3  BPRD/MKT/EST/DPT/SLN                     
         BE    AG30                WE GOT THE RECORD PREVIOUSLY                 
*                                                                               
                                                                                
         OC    LASTREC,LASTREC     TEST FOR PREVIOUS RECORD                     
         BZ    AG20                NO                                           
         CLC   LASTREC(3),=C'***'  TEST DELETING GOALS                          
         BE    AG12                                                             
*                                                                               
         OC    DMWORK,DMWORK       TEST PREVIOUS RECORD ON FILE                 
         BZ    AG10                NO                                           
         CLI   RCWRITE,C'N'                                                     
         BNE   *+12                                                             
         BAS   RE,PRTPUT                                                        
         B     AG12                                                             
*                                                                               
         CLI   QOPT1,C'Y'          USER OPTION TO NOT MARK FILE                 
         BE    AG12                                                             
         NI    GCNTRLS,X'7F'       TURN OFF DELETE BIT (MIGHT BE ON)            
         GOTO1 PUT                 WRITE PREVIOUS RECORD                        
         TM    KEY+13,X'80'        IS KEY DELETED?                              
         BZ    AG12                                                             
         NI    KEY+13,X'7F'         YES - UNDELETE IT                           
         GOTO1 WRITE                                                            
         B     AG12                                                             
*                                                                               
AG10     CLI   QOPT1,C'Y'                                                       
         BE    AG12                                                             
         GOTO1 ADD                                                              
*                                                                               
AG12     BAS   RE,AGENDGL           PRINT RECORD DETAILS                        
*                                                                               
* IF PRD OR MKT HAS CHANGED, PRINT TOTALS                                       
         CLI   THISREC,X'FF'       MKT BREAK ON EOF                             
         BNE   AG14           IF NOT EOF, COMPARE MARKETS                       
         BAS   RE,PMKTTOT                                                       
         BAS   RE,PPRDTOT                                                       
         B     AG18           EOF, SO SKIP OTHER COMPARISONS                    
*                                                                               
AG14     CLC   THISREC+4(2),LASTREC+4   SAME MKT?                               
         BNE   *+12           DIFF MKTS, CHECK IF END OF CONSEC MKTS            
         MVI   CMKFLG,1       TURN CONSEC MKT FLAG ON                           
         B     AG16           BRANCH TO COMPARE PRODUCTS                        
*                                                                               
         CLI   CMKFLG,0                                                         
         BE    *+8                                                              
         BAS   RE,PMKTTOT     IF # OF CONSEC MKTS NOT 0, PRINT MKTTOT           
         XC    MKTTOTS,MKTTOTS                                                  
*                                                                               
AG16     DS    0H                                                               
         CLC   THISREC+3(1),LASTREC+3   SAME PRD?                               
         BE    AG18                                                             
         BAS   RE,PPRDTOT                                                       
*                                                                               
AG18     LA    R5,THISREC                                                       
*                                                                               
AG20     DS    0H                                                               
         CLC   SRQPRD,=C'***'      DELETE PORTION?                              
         BE    AG50                 YES - ADD TO TOTS, NOT TO ELS               
         CLI   SRQPRD,X'FF'        TEST E-0-F                                   
         BE    AG100                                                            
*                                                                               
         XC    KEY,KEY             GET GOAL RECORD                              
         MVI   KEY,2                                                            
         L     R6,ADCLT                                                         
         MVC   KEY+1(3),1(R6)      A-M/CLT                                      
         MVC   KEY+4(4),SRPRD      PRD/MKT/EST                                  
         MVC   KEY+8(2),SRDPTSLN   DPT/SLN                                      
         MVC   KEY+10(1),KEY+9     SLN TO TLN                                   
         MVI   DMINBTS,X'08'       SET TO PASS DELETED RECORDS                  
         MVI   DMOUTBTS,X'FD'      DO NOT TEST X'02'                            
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   AG22                                                             
         GOTO1 GET                                                              
*        BAS   RE,PRTGL            PRINT ORIGINAL GOALS                         
         B     AG30                                                             
*                                                                               
* CREATE NEW GOAL RECORD                                                        
*                                                                               
AG22     XC    DMWORK,DMWORK       REMEMBER RECORD NOT ON FILE                  
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         L     R7,ADGOAL                                                        
         XC    0(256,R7),0(R7)                                                  
         MVC   0(13,R7),KEY                                                     
         MVC   GLENGTH,=AL2(GDLENQ+24)                                          
         MVC   GAGYALPH,AGY                                                     
         MVI   GDELEM,X'20'                                                     
         MVI   GOLEN,GDLENQ                                                     
         MVC   GBUYNAME,=CL12'SKA TRANSFER'                                     
         GOTO1 DATCON,DMCB,(5,0),(2,GREDATE)  SET CREATE DATE                   
         MVC   GDSTDAY,SRSTDAY     SET GOAL WEEK START DAY                      
         MVC   GDCPPCL,SRCPPCL     CPP CLIENT                                   
         MVC   GDCPPES(2),SRCPPES  AND ESTIMATES                                
         EJECT                                                                  
*=================================================================*             
* ADD OR CHANGE EXISTING WEEKLY DATA                              *             
*=================================================================*             
         SPACE 1                                                                
AG30     DS    0H                                                               
         CLC   SRQPRD,=C'***'      DELETE PORTION?                              
         BE    AG50                 YES - ADD TO TOTS, NOT TO ELS               
         GOTO1 DATCON,DMCB,(5,0),(2,GACTDATE)  SET ACTIVITY DATE                
         MVI   ELCODE,X'21'                                                     
         LA    R6,GDELEM                                                        
         USING GLEMENT,R6                                                       
*                                                                               
AG32     BAS   RE,NEXTEL                                                        
         BNE   AG40                                                             
         CLC   SRWEEK,2(R6)        MATCH WEEK DATE                              
         BNE   AG32                                                             
* CHANGE EXISTING ELEMENT                                                       
         OC    SRPTS,SRPTS         TEST POINTS IN SORT DATA                     
         BNZ   AG33                 YES                                         
         MVC   FULL,GLGRP                                                       
         BRAS  RE,ROUND                                                         
         OC    FULL,FULL            NO - TEST POINTS IN EXISTING RECORD         
         BZ    AG34                  NO - OK                                    
         B     AG35                                                             
*                                                                               
* PTS IN SRT REC - MUST BE IN GOAL                                              
AG33     OC    FULL,FULL                                                        
         BZ    AG35                                                             
*                                                                               
AG34     OC    SRDOLS,SRDOLS       TEST DOLLARS IN SORT DATA                    
         BNZ   AG34A                YES                                         
         OC    GLBUDGET,GLBUDGET    NO - TEST DOLLARS IN EXISTING DATA          
         BZ    AG36                  NO - OK                                    
         B     AG35                                                             
*                                                                               
* PTS IN SRT REC - MUST BE IN GOAL                                              
AG34A    OC    GLBUDGET,GLBUDGET                                                
         BNZ   AG36                                                             
AG35     OI    AGERR,AGINCON                                                    
         B     AG2                                                              
*                                                                               
AG36     MVC   FULL,GLGRP                                                       
         BRAS  RE,ROUND                                                         
         L     R0,FULL                                                          
         A     R0,SRPTS                                                         
         STCM  R0,15,GLGRP                                                      
         CLI   TWODEC,C'Y'                                                      
         JNE   *+8                                                              
         OI    GLGRP,GLGRP2DEC    SET 2-DEC FLAG IN PTS                         
         ICM   R0,15,GLBUDGET                                                   
         A     R0,SRDOLS                                                        
         STCM  R0,15,GLBUDGET                                                   
         B     AG50                                                             
*                                                                               
AG40     XC    ELEM,ELEM                                                        
         MVI   ELEM,X'21'                                                       
         MVI   ELEM+1,12                                                        
         MVC   ELEM+2(2),SRWEEK                                                 
         MVC   ELEM+4(4),SRPTS                                                  
         CLI   TWODEC,C'Y'                                                      
         JNE   *+8                                                              
         OI    ELEM+4,GLGRP2DEC    SET 2-DEC FLAG IN PTS                        
         MVC   ELEM+8(4),SRDOLS                                                 
*                                                                               
         LA    R6,GDELEM                                                        
         SR    R0,R0                                                            
AG42     IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    AG44                                                             
         CLI   0(R6),X'21'                                                      
         BL    AG42                                                             
         BH    AG44                                                             
         CLC   ELEM+2(2),2(R6)     NEW ELEM DATE TO EXIST ELEM DATE             
         BH    AG42                IF HIGH, KEEP GOING                          
AG44     GOTO1 RECUP,DMCB,(R7),ELEM,(C'R',(R6))                                 
         CLI   8(R1),C'R'                                                       
         BE    *+12                                                             
         OI    AGERR,AGRECFUL                                                   
         B     AG2                                                              
*                                                                               
AG50     LM    R0,R1,RECTOTS       UPDATE RECORD TOTALS                         
         A     R0,SRDOLS                                                        
         A     R1,SRPTS                                                         
         STM   R0,R1,RECTOTS                                                    
         B     AG2                                                              
         DROP  R5                                                               
*                                                                               
AG100    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
AGENDGL  NTR1                                                                   
         OC    RECTOTS,RECTOTS                                                  
         BZ    AGENDGLX                                                         
         LA    R5,LASTREC                                                       
         USING SRRECD,R5                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,SRMKT                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PMKT,DUB                                                         
*                                                                               
         MVC   PMKNM,SRMKNM                                                     
*                                                                               
         EDIT  (1,SREST),(3,PEST)                                               
*                                                                               
         MVC   PPROD,SRQPRD                                                     
*                                                                               
         MVC   PADPT,SRDPTSLN                                                   
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,SRDPTSLN+1                                                  
         EDIT  (R0),(3,PASLN),ALIGN=LEFT                                        
*                                                                               
         CLI   AGERR,0                                                          
         BE    AGEND20                                                          
         TM    AGERR,AGINCON                                                    
         BZ    AGEND10                                                          
         MVC   PSDTE(25),=C'** PTS/$$$ INCONSISTENT **'                         
         GOTO1 REPORT                                                           
*                                                                               
AGEND10  TM    AGERR,AGRECFUL                                                   
         BZ    AGEND50                                                          
         MVC   PSDTE(17),=C'** RECORD FULL **'                                  
         GOTO1 REPORT                                                           
         B     AGEND50                                                          
*                                                                               
AGEND20  EDIT  (4,RECDOLS),(10,PADOLS),2,ZERO=BLANK                             
         L     R0,RECPTS                                                        
         BRAS  RE,DOPTS                                                         
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
AGEND50  DS    0H                                                               
         LM    R0,R1,MKTTOTS                                                    
         A     R0,RECDOLS                                                       
         A     R1,RECPTS                                                        
         STM   R0,R1,MKTTOTS                                                    
*                                                                               
         LM    R0,R1,PRDTOTS                                                    
         A     R0,RECDOLS                                                       
         A     R1,RECPTS                                                        
         STM   R0,R1,PRDTOTS                                                    
*                                                                               
         XC    RECTOTS,RECTOTS                                                  
         MVI   AGERR,0                                                          
*                                                                               
AGENDGLX J     EXIT                                                             
         EJECT                                                                  
* MARKET TOTALS FOR RE-ALLOC *                                                  
*                                                                               
PMKTTOT  NTR1                                                                   
         EDIT  (4,MKTTDOL),(10,PADOLS),2,ZERO=BLANK                             
         L     R0,MKTTPTS                                                       
         BRAS  RE,DOPTS                                                         
*                                                                               
         MVC   P+58(10),=C'** MARKET '                                          
*                                                                               
         SR    R0,R0          **MAYBE JUST USE 'DUB'                            
         ICM   R0,3,SRMKT                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+69(4),DUB                                                      
*                                                                               
         MVC   P+74(8),=C'TOTAL **'                                             
         MVI   SPACING,X'03'                                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         XC    CMKFLG,CMKFLG                                                    
*                                                                               
PMKX     J     EXIT                                                             
         SPACE 2                                                                
* PRODUCT TOTALS FOR RE-ALLOC *                                                 
*                                                                               
PPRDTOT  NTR1                                                                   
         EDIT  (4,PRDDOLS),(10,PADOLS),2,ZERO=BLANK                             
         L     R0,PRDPTS                                                        
         BRAS  RE,DOPTS                                                         
*                                                                               
         MVC   P+58(12),=C'** PRODUCT  '                                        
         MVC   P+70(3),LASTREC                                                  
         MVC   P+74(9),=C' TOTAL **'                                            
         CLI   LASTREC,C'*'                                                     
         BNE   NOTDEL1                                                          
         MVI   P+69,C'"'                                                        
         MVI   P+73,C'"'                                                        
*                                                                               
NOTDEL1  MVI   SPACING,X'03'                                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         XC    PRDTOTS,PRDTOTS                                                  
*                                                                               
PPRDX    J     EXIT                                                             
         EJECT                                                                  
* MARKET TOTALS *                                                               
*                                                                               
MKTOTS   NTR1                                                                   
         CP    RECCNT,=P'1'                                                     
         BNH   MKT10                                                            
         EDIT  (4,MKTDOLS),(10,PDOLS),2,ZERO=BLANK                              
         MVC   P+2(10),=C'** MARKET '                                           
         MVC   P+13(4),MKT                                                      
         MVC   P+18(8),=C'TOTAL **'                                             
         MVI   SPACING,X'03'                                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         SPACE                                                                  
MKT10    L     R0,MKTDOLF                                                       
         A     R0,MKTDOLS                                                       
         ST    R0,MKTDOLF                                                       
         XC    MKTDOLS,MKTDOLS                                                  
*                                                                               
MKTX     J     EXIT                                                             
         SPACE 2                                                                
* EST TOTALS *                                                                  
*                                                                               
ESTOTS   NTR1                                                                   
         CLC   BESTSV,GKEYEST      EST=EST PREV                                 
         BE    ESTX                                                             
         MVC   BESTSV,GKEYEST      NO.SET NEW EST NUM                           
         CP    ESTCNT,=P'0'                                                     
         BE    ESTX                                                             
         CP    ESTCNT,=P'1'                                                     
         BNH   ESTXA                                                            
         SPACE                                                                  
         MVI   SPACING,2                                                        
         EDIT  (4,ESTDOLS),(10,PDOLS),2,ZERO=BLANK                              
         L     R0,ESTPTS                                                        
         BRAS  RE,DOPTS                                                         
         MVC   P+25(20),=C'** ESTIMATE TOTAL **'                                
         GOTO1 REPORT                                                           
ESTXA    XC    ESTDOLS,ESTDOLS                                                  
         XC    ESTPTS,ESTPTS                                                    
         ZAP   ESTCNT,=P'0'                                                     
         XC    P,P                                                              
         GOTO1 REPORT             TO GET BLNK LINE BETWEEN DIFF ESTS            
*                                                                               
ESTX     J     EXIT                                                             
         DROP  R7                                                               
         SPACE                                                                  
* PRODUCT TOTALS *                                                              
*                                                                               
FNLTOTS  NTR1                                                                   
         CLI   MKTOTSW,C'Y'                                                     
         BNE   FNLX                                                             
         EDIT  (4,MKTDOLF),(10,PDOLS),2,ZERO=BLANK                              
         MVC   P+2(12),=C'** PRODUCT  '                                         
         MVC   P+14(3),QPRD                                                     
         MVC   P+18(9),=C' TOTAL **'                                            
         CLI   QPRD,C'*'                                                        
         BNE   NOTDEL2                                                          
         MVI   P+13,C'"'                                                        
         MVI   P+17,C'"'                                                        
*                                                                               
NOTDEL2  GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         ZAP   RECCNT,=P'0'                                                     
*                                                                               
FNLX     J     EXIT                                                             
         EJECT                                                                  
*                                                                               
NEXTEL   DS    0H                                                               
         CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         LLC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DS    H'0'                                                             
         AR    R6,R0                                                            
NXT10    CLC   ELCODE,0(R6)                                                     
         JNE   NEXTEL                                                           
         BR    RE                  EXIT WITH CC =                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  EXIT WITH CC NOT =                           
         SPACE                                                                  
*                                                                               
SPERR    DS    0H                                                               
         MVC   P(80),QAREA                                                      
         MVC   P+82(23),=C'** SPTLEN NOT NUMERIC **'                            
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
* PRINT OUT ORIGINAL GOALS *                                                    
         SPACE 1                                                                
PRTGL    NTR1                                                                   
         L     R6,ADGOAL                                                        
         LA    R6,24(R6)                                                        
         MVI   ELCODE,X'21'                                                     
         SR    R5,R5               ELEM COUNTER                                 
PRTGL10  LA    R7,P                                                             
PRTGL12  BAS   RE,NEXTEL                                                        
         BNE   PRTGL14                                                          
         CLC   GLWEEK,BQSTARTP     CHECK DATE SPREAD                            
         BL    PRTGL12                                                          
         CLC   GLWEEK,BQENDP                                                    
         BH    PRTGL12                                                          
         GOTO1 DATCON,DMCB,(2,GLWEEK),(4,(R7))                                  
         EDIT  (4,GLBUDGET),(6,6(R7)),2,ZERO=BLANK                              
         EDIT  (4,GLGRP),(6,13(R7)),1,ZERO=BLANK                                
         LA    R7,21(R7)                                                        
         LA    R5,1(R5)                                                         
         CH    R5,=H'5'            PUT 5 ELEMS ON A PRINT LINE                  
         BL    PRTGL12                                                          
         GOTO1 REPORT                                                           
         SR    R5,R5                                                            
         B     PRTGL10                                                          
*                                                                               
PRTGL14  LTR   R5,R5               ANY LEFT TO PRINT                            
         BZ    EXIT                                                             
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* PRINT A TRACE OF PUTRECS IF WE GOT THE OPTIONS RIGHT *                        
         SPACE 1                                                                
PRTPUT   NTR1                                                                   
         CLI   QOPT2,C'Y'          SORT TRACE                                   
         BNE   EXIT                                                             
         L     R6,ADGOAL                                                        
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)         GET RECORD LENGTH                            
         GOTO1 PRNTBL,DMCB,=C'PUTREC',(R6),C'DUMP',(R0),=C'1D'                  
         B     EXIT                                                             
*                                                                               
ROUND    NTR1                                                                   
         L     R1,FULL                                                          
         N     R1,=X'3FFFFFFF'     DROP FLAGS                                   
*                                                                               
         CLI   TWODEC,C'Y'         TEST REPORTING 2-DEC                         
         JNE   ROUND10                                                          
         TM    FULL,GLGRP2DEC      TEST GRP TO 2-DEC                            
         JO    ROUNDX                                                           
* GRP IS 1-DEC - SCALE UP                                                       
         MHI   R1,10                                                            
         J     ROUNDX                                                           
* DOING 1-DEC                                                                   
ROUND10  TM    FULL,GLGRP2DEC      TEST GRP TO 2-DEC                            
         JZ    ROUNDX              NO - DONE                                    
         M     R0,=F'2'            ELSE ROUND TO 1-DEC                          
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
ROUNDX   ST    R1,FULL             SET RETURN VALUE                             
         XIT1                                                                   
         DROP  R6                                                               
*                                                                               
DOPTS    NTR1                                                                   
         CLI   TWODEC,C'Y'                                                      
         JE    DOPTS2                                                           
         EDIT  ((R0)),(8,PAPTS),1,ZERO=BLANK                                    
         J     DOPTSX                                                           
*                                                                               
DOPTS2   EDIT  ((R0)),(8,PAPTS),2,ZERO=BLANK                                    
*                                                                               
DOPTSX   XIT1                                                                   
         LTORG                                                                  
                                                                                
*===========================================================                    
* READ G0 AT AGENCY LEVEL FOR 2-DEC OPTION                                      
* MEDIA CODES R AND X ARE ALWAYS 1 DECIMAL                                      
*===========================================================                    
                                                                                
GETG0PRF NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         XC    SVG0PROF,SVG0PROF   CLEAR IN CASE NOT FOUND                      
         MVC   WORK(4),=C'S0G0'                                                 
         MVC   WORK+4(2),QAGY                                                   
         GOTO1 GETPROF,DMCB,WORK,SVG0PROF,DATAMGR                               
*                                                                               
         MVI   TWODEC,C'N'                                                      
         CLI   QMED,C'R'                                                        
         JE    EXIT                                                             
         CLI   QMED,C'X'                                                        
         JE    EXIT                                                             
         MVC   TWODEC,SVG0PROF+7   FOR MEDIA T, USE AGENCY OPTION               
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
HEADHK   NTR1  BASE=*,LABEL=*                                                   
         LM    R9,RA,REGSTR                                                     
 *                                                                              
         CLI   QOPT1,C'Y'                                                       
         BNE   *+10                                                             
         MVC   H3+28(34),=C'*** TEST RUN - FILE NOT MARKED ***'                 
*                                                                               
         XC    WORK,WORK                                                        
         CLC   Q2DPT(4),SPACES     TEST FILTERS                                 
         BNH   *+10                                                             
         MVC   WORK(4),Q2DPT                                                    
         MVC   WORK+5(3),QPRD                                                   
         MVC   WORK+9(14),=C'GOALS MOVED TO'                                    
         MVC   WORK+24(3),Q2PRD1                                                
         MVI   WORK+27,C'('                                                     
         MVC   WORK+28(3),Q2WGT1                                                
         MVI   WORK+31,C')'                                                     
*                                                                               
         CLC   Q2PRD2,SPACES                                                    
         BNH   SQUAPRD                                                          
         MVI   WORK+32,C','                                                     
         MVC   WORK+34(3),Q2PRD2                                                
         MVI   WORK+37,C'('                                                     
         MVC   WORK+38(3),Q2WGT2                                                
         MVI   WORK+41,C')'                                                     
*                                                                               
         CLC   Q2PRD3,SPACES                                                    
         BNH   SQUAPRD                                                          
         MVI   WORK+42,C','                                                     
         MVC   WORK+44(3),Q2PRD3                                                
         MVI   WORK+47,C'('                                                     
         MVC   WORK+48(3),Q2WGT3                                                
         MVI   WORK+51,C')'                                                     
*                                                                               
         CLC   Q2PRD4,SPACES                                                    
         BNH   SQUAPRD                                                          
         MVI   WORK+52,C','                                                     
         MVC   WORK+54(3),Q2PRD4                                                
         MVI   WORK+57,C'('                                                     
         MVC   WORK+58(3),Q2WGT4                                                
         MVI   WORK+61,C')'                                                     
*                                                                               
SQUAPRD  LA    RF,64          SQUASH WORK FIELD FOR PRINTING                    
         ST    RF,DMCB+4                                                        
         GOTO1 SQUASHER,DMCB,WORK                                               
         L     RF,DMCB+4                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   H10(0),WORK                                                      
*                                                                               
HD02     LA    R1,H8+30                                                         
         CLI   QMGR,C' '                                                        
         BE    HD07                                                             
         LA    R0,4                                                             
         LA    R1,H9+30                                                         
HD05     CLC   =C'MARKET',0(R1)                                                 
         BE    HD07                                                             
         SH    R1,=H'132'                                                       
         BCT   R0,HD05                                                          
         J     EXIT                                                             
HD07     MVC   0(37,R1),SPACES                                                  
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
         DS    0F                                                               
CMKFLG   DC    X'0'           FLAG FOR CONSECUTIVE # OF SAME MARKETS            
TOTALS   DS    0XL24                                                            
DOLTOT   DS    F                                                                
ESTDOLS  DS    F                                                                
PTOT     DS    F                                                                
ESTPTS   DS    F                                                                
MKTDOLS  DS    F                                                                
MKTDOLF  DS    F                                                                
*                                                                               
TOTBDGT  DS    F                                                                
TOTGRP   DS    F                                                                
*                                                                               
RECTOTS  DS    0XL8                                                             
RECDOLS  DS    F                                                                
RECPTS   DS    F                                                                
*                                                                               
PRDTOTS  DS    0XL8                                                             
PRDDOLS  DS    F                                                                
PRDPTS   DS    F                                                                
*                                                                               
MKTTOTS DS    0XL8                                                              
MKTTDOL DS    F                                                                 
MKTTPTS DS    F                                                                 
*                                                                               
REGSTR   DS    4F                                                               
SVG0PROF DS    CL16                                                             
*                                                                               
DELSW    DS    CL1                                                              
FRSTSW   DS    CL1                                                              
ELCODE   DS    CL1                                                              
BESTSV   DS    CL1                                                              
ESTOTSW  DS    CL1                                                              
MKTOTSW  DS    CL1                                                              
STDTSV   DS    CL2                                                              
ENDTSV   DS    CL2                                                              
BMKTSV   DS    CL2                                                              
BSLN     DS    CL1                                                              
*                                                                               
AGERR    DS    X                   ERROR CODES                                  
AGRECFUL EQU   X'80'               RECORD FULL                                  
AGINCON  EQU   X'40'               INCONSISTANT PTS/$$$                         
*        EQU   X'20'               **UNUSED                                     
*        EQU   X'10'               **UNUSED                                     
*        EQU   X'08'               **UNUSED                                     
*        EQU   X'04'               **UNUSED                                     
*        EQU   X'02'               **UNUSED                                     
*        EQU   X'01'               **UNUSED                                     
*                                                                               
         DS    0F                                                               
ELEM     DS    CL256                                                            
         DS    0D                                                               
         DC    CL8'THISREC'                                                     
THISREC  DS    XL48                                                             
         DC    CL8'LASTREC'                                                     
LASTREC  DS    XL48                                                             
         DC    CL8'**MYREC'                                                     
MYREC    DS    XL200                                                            
*                                                                               
TWODEC   DS    C                                                                
BRDHIGH  DS    X                   BRAND WITH HIGHEST WEIGHT (0-3)              
BRDTOTSH DS    F                   SUM OF BRAND WEIGHTS                         
*                                                                               
         DS    0D                                                               
         DC    CL8'*BRDLIST'                                                    
BRDLIST  DS    0XL32                                                            
*                                                                               
BRD1     DS    CL3                 EBCDIC PRD                                   
BRDCD1   DS    XL1                 PRD CODE                                     
BRDSH1   DS    F                   PRD SHARE                                    
BRDLEN   EQU   *-BRD1                                                           
*                                                                               
BRD2     DS    CL3                                                              
BRDCD2   DS    XL1                                                              
BRDSH2   DS    F                                                                
*                                                                               
BRD3     DS    CL3                                                              
BRDCD3   DS    XL1                                                              
BRDSH3   DS    F                                                                
*                                                                               
BRD4     DS    CL3                                                              
BRDCD4   DS    XL1                                                              
BRDSH4   DS    F                                                                
*                                                                               
RECCNT   DC    PL4'0'                                                           
ESTCNT   DC    PL4'0'                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
SRRECD   DSECT                                                                  
SRQPRD   DS    CL3                 EBCDIC PRODUCT                               
SRPRD    DS    XL1                 BINARY PRODUCT                               
SRMKT    DS    XL2                 MARKET                                       
SREST    DS    XL1                 ESTIMATE                                     
SRDPTSLN DS    XL2                 DPT/SLN                                      
SRSTDAY  DS    XL1                 GOAL WEEK START DAY                          
SRWEEK   DS    XL2                 WEEK START DATE                              
SRPTS    DS    XL4                 POINTS                                       
SRDOLS   DS    XL4                 DOLLARS                                      
SRMKNM   DS    XL24                MARKET NAME                                  
SRCPPCL  DS    XL2                 CPP CLIENT CODE                              
SRCPPES  DS    XL2                 CPP START/END ESTIMATES                      
SRRECLEN EQU   *-SRRECD            KEEP LEN IN SYNC WITH THIS/LASTREC           
         EJECT                                                                  
*                                                                               
PLINED   DSECT                                                                  
*                                                                               
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL3                                                              
PMKNM    DS    CL16                                                             
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL2                                                              
PDOLS    DS    CL8                                                              
         DS    CL3                                                              
PPTS     DS    CL8                                                              
         DS    CL3                                                              
PSDTE    DS    CL8                                                              
         DS    CL2                                                              
PEDTE    DS    CL8                                                              
         DS    CL2                                                              
PDPT     DS    CL1                                                              
PSLN     DS    CL3                                                              
         DS    CL2                                                              
PPROD    DS    CL3                                                              
         DS    CL2                                                              
PADOLS   DS    CL8                                                              
         DS    CL3                                                              
PAPTS    DS    CL8                                                              
         DS    CL3                                                              
PADPT    DS    CL1                                                              
PASLN    DS    CL3                                                              
         EJECT                                                                  
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
         ORG   Q2USER                                                           
Q2PRD1   DS    CL3    COL 21                                                    
Q2WGT1   DS    CL3                                                              
Q2PRD2   DS    CL3    COL 27                                                    
Q2WGT2   DS    CL3                                                              
Q2PRD3   DS    CL3    COL 33                                                    
Q2WGT3   DS    CL3                                                              
Q2PRD4   DS    CL3    COL 39                                                    
Q2WGT4   DS    CL3                                                              
Q2DPT    DS    CL1    COL 45                                                    
Q2SLN    DS    CL3    COL 46                                                    
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'174SPREPKA02 03/05/18'                                      
         END                                                                    
