*          DATA SET SPBUY38    AT LEVEL 022 AS OF 03/20/15                      
*PHASE T21138B                                                                  
                                                                                
*================================================================               
* THIS PROGRAM IS CALLED FROM SPBUY31 TO DO SCHEDULE EVALUATION                 
* ON ENTRY R1 POINTS TO A(SPBUYWK), A(SPBUY31WK)                                
*                                                                               
* NOTE THAT WHILE MGTSRNUM LOOKS LIKE THE FIRST TSAR RECORD NUMBER              
* TO BE DISPLAYED, THE TSAR RECORDS ARE SORTED IN STORAGE, AND                  
* MGTSRNUM IS THE FIRST RECORD IN THE SORT BUFFER TO BE DISPLAYED               
*================================================================               
T21138   TITLE 'SPBUY38 - SPOTPAK BUY - MAKEGOOD SCHEDULE ANALYSIS'             
T21138   CSECT                                                                  
*                                                                               
PFKTOP   EQU   1                   GO TO TOP                                    
PFKBACK  EQU   2                   GO BACK                                      
PFKNEXT  EQU   3                   NEXT                                         
PFKDIFF  EQU   4                   DIFFERENTIAL DISPLAY                         
PFKDEM   EQU   7                   6 DEMOS                                      
PFKCPP   EQU   8                   6 DEMOS/CPP                                  
PFKM7WK  EQU   10                  -7 WEEKS                                     
PFKP7WK  EQU   11                  +7 WEEKS                                     
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,T21138,R9,RR=R7                                                
*                                                                               
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
*                                                                               
         L     R8,4(R1)                                                         
         USING BSPOOLD,R8                                                       
*                                                                               
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
*                                                                               
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
T        USING TSARD,TSARBLK                                                    
*                                                                               
         ST    R7,RELO                                                          
*                                                                               
         CLI   SVSCR,X'F8'         TEST SKEVAL SCREEN LOADED                    
         BE    SE00                                                             
*                                                                               
         MVC   ELEM(1),MGEINP1H+5  SAVE INPUT LINE LENGTH                       
         MVC   ELEM+1(78),MGEINP1  SAVE DATA ON INPUT LINE                      
*                                                                               
         XC    DMCB,DMCB           LOAD SKEVAL SCREEN                           
         MVC   DMCB+4(4),=X'D90211F8'                                           
         GOTO1 VCALLOV,DMCB,BUYHL1H                                             
         MVI   SVSCR,X'F8'                                                      
*                                                                               
         LA    R2,MGEINP1H                                                      
         OI    4(R2),X'20'         SET PREV VALIDATED                           
         MVC   5(1,R2),ELEM        RESTORE INPUT LENGTH                         
         MVC   8(78,R2),ELEM+1     RESTORE DATA                                 
                                                                                
*======================================================                         
* NOW REMOVE ANY LEFT-OVER BUY RECORDS FROM TSAR                                
*======================================================                         
                                                                                
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
*                                                                               
DELB2    BRAS  RE,CALLTSAR                                                      
         BNE   DELBX                                                            
         CLI   MSTYPE,C'B'         TEST BUY                                     
         BNE   DELB4                                                            
         MVI   T.TSACTN,TSADEL                                                  
         BRAS  RE,CALLTSAR                                                      
         MVI   T.TSACTN,TSAGET                                                  
         B     DELB2                                                            
*                                                                               
DELB4    MVI   T.TSACTN,TSANXT                                                  
         B     DELB2                                                            
*                                                                               
DELBX    MVC   MGTSRNUM,=Y(1)                                                   
         MVI   DSPFMT,0            START IN SKED DISPLAY                        
         B     SE10                                                             
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*================================================================               
* PREVIOUSLY IN THIS MODULE - DO PFKEY ACTION IF REQUIRED                       
* BUT FIRST, CLEAR ALL DISPLAY LINES AND RESET TO NORMAL INTENSITY              
*================================================================               
                                                                                
SE00     LA    R2,SKELIN1H                                                      
         USING LINED,R2                                                         
*                                                                               
SE02     XC    8(LLEN-8,R2),8(R2)                                               
         NI    6(R2),X'FF'-X'08'   UNSET  HIGH INT                              
         OI    6(R2),X'80'         SET XMT                                      
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),LLEN                                                       
         BE    SE02                                                             
                                                                                
*===========================================================                    
* NOW REBUILD SORT BUFFER FROM TSAR RECORDS                                     
*===========================================================                    
                                                                                
         BRAS  RE,RBTSAR                                                        
         L     R5,FULL             AT SE60, R5 HAS END BUFFER ADDR              
*                                                                               
SE04     ICM   R0,3,MGTSRNUM       GET STARTING RECNUM                          
         BNZ   *+8                                                              
         LHI   R0,1                                                             
*                                                                               
         CLI   PFKEY,PFKTOP        TEST TOP                                     
         BNE   *+8                                                              
         LHI   R0,1                                                             
*                                                                               
         CLI   PFKEY,PFKBACK       TEST BACK                                    
         BNE   SE04A                                                            
         AHI   R0,-(NUMLINES-1)                                                 
         BP    SE04A                                                            
         LHI   R0,1                                                             
*                                                                               
SE04A    CLI   PFKEY,PFKNEXT       TEST NEXT                                    
         BNE   SE04E                                                            
         AHI   R0,NUMLINES-1                                                    
         CH    R0,T.TSPRECN        COMPARE TO RECORD COUNT                      
         BNH   *+8                                                              
         AHI   R0,-(NUMLINES-1)    BACK UP IF PAST END                          
*                                                                               
SE04E    STH   R0,MGTSRNUM         SET STARTING LINE NUMBER                     
*                                                                               
         LHI   R0,7                SET TO ADVANCE 7 WEEKS                       
         CLI   PFKEY,PFKP7WK       TEST SCROLL RIGHT                            
         BE    SE04F                                                            
         LHI   R0,-7                                                            
         CLI   PFKEY,PFKM7WK       TEST SCROLL LEFT                             
         BNE   SE04J                                                            
SE04F    BRAS  RE,ADJPER                                                        
         BRAS  RE,SETSKED                                                       
*                                                                               
SE04J    CLI   PFKEY,PFKDIFF       TEST DIFFERENCE DISPLAY                      
         BNE   SE04M                                                            
         CLI   DSPFMT,C'-'         IF THAT IS CURRENT OPTION                    
         BE    SEDEMOFF            TURN IT OFF NOW                              
         MVI   DSPFMT,C'-'                                                      
         B     SEDEMON                                                          
*                                                                               
SE04M    CLI   PFKEY,PFKCPP                                                     
         BE    SECPP                                                            
         CLI   PFKEY,PFKDEM                                                     
         BE    SEDEM                                                            
         B     SEDEMON             RETAIN CURRENT DISPLAY FORMAT                
*                                                                               
SECPP    CLI   DSPFMT,C'C'         TEST CPP FMT NOW                             
         BE    SEDEMOFF            YES - BACK TO STD                            
         MVI   DSPFMT,C'C'         ELSE SET FOR CPP                             
         B     SEDEMON                                                          
*                                                                               
SEDEM    CLI   DSPFMT,C'D'         TEST DEMO FMT NOW                            
         BE    SEDEMOFF                                                         
         MVI   DSPFMT,C'D'                                                      
*                                                                               
SEDEMON  BRAS  RE,SEHL             BUILD DEMO HEADLINES                         
         B     SE60                AND GO DISPLAY                               
*                                                                               
SEDEMOFF MVI   DSPFMT,0            CLEAR DEMO FORMAT                            
         BRAS  RE,SEHL             REBUILD HEADLINES                            
         B     SE60                                                             
         EJECT                                                                  
*============================================================                   
* READ MISSED TSAR RECORDS AND EXTRACT DAYS/TIMES                               
*============================================================                   
                                                                                
SE10     BRAS  RE,SETSKED                                                       
         BRAS  RE,SEHL                                                          
         XC    MISSLIST,MISSLIST   CLEAR LIST OF MISSED DAYS/TIMES              
*                                                                               
         LA    R4,MISSLIST                                                      
         XC    MSREC,MSREC                                                      
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
*                                                                               
SE16     BRAS  RE,CALLTSAR                                                      
         BNE   SE20                                                             
         MVI   T.TSACTN,TSANXT                                                  
         CLI   MSTYPE,MSTYPEQ                                                   
         BNE   SE16                                                             
*                                                                               
         MVC   0(1,R4),MSDAYS                                                   
         SR    R1,R1                                                            
         ICM   R1,3,MSSTIM                                                      
         LHI   R0,-15                                                           
         BAS   RE,ADJTIM                                                        
         MVC   1(2,R4),DUB                                                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,MSETIM                                                      
         LHI   R0,15                                                            
         BAS   RE,ADJTIM                                                        
         MVC   3(2,R4),DUB                                                      
*                                                                               
         LA    R4,5(R4)                                                         
         LA    R0,MISSLSTX                                                      
         CR    R4,R0                                                            
         BNH   SE16                                                             
         EJECT                                                                  
SE20     LA    R4,OFFRLIST                                                      
         XC    OFFRLIST,OFFRLIST                                                
         XC    MOREC,MOREC                                                      
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
*                                                                               
SE22     BRAS  RE,CALLTSAR                                                      
         BNE   SE30                                                             
         MVI   T.TSACTN,TSANXT                                                  
         CLI   MOTYPE,MOTYPEQ                                                   
         BNE   SE22                                                             
         CLI   MOCOMNUM,0          TEST COMMENT                                 
         BNE   SE22                                                             
*                                                                               
         MVC   0(1,R4),MODAYS                                                   
         SR    R1,R1                                                            
         ICM   R1,3,MOSTIM                                                      
         LHI   R0,-15                                                           
         BAS   RE,ADJTIM                                                        
         MVC   1(2,R4),DUB                                                      
         OC    MOETIM,MOETIM                                                    
         BZ    SE24                                                             
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,MOETIM                                                      
         LHI   R0,15                                                            
         BAS   RE,ADJTIM                                                        
SE24     MVC   3(2,R4),DUB                                                      
*                                                                               
         LA    R4,5(R4)                                                         
         LA    R0,OFFRLSTX                                                      
         CR    R4,R0                                                            
         BNH   SE22                                                             
         B     SE30                                                             
                                                                                
*===============================================================                
* ADD R0 MINUTES TO TIME IN R1 AND RETURN IN DUB(2)                             
*===============================================================                
                                                                                
ADJTIM   NTR1                                                                   
         SR    RE,RE                                                            
         LR    RF,R1               GET MILITARY TIME                            
         CHI   RF,600                                                           
         BNL   *+8                                                              
         AHI   RF,2400                                                          
         D     RE,=F'100'          GIVE MIN IN RE, HRS IN RF                    
         MHI   RF,60               HRS TIMES 60 GIVES MINUTES                   
         AR    RF,RE               ADD TO MINUTES                               
         AR    RF,R0               AMOUNT OF ADJUSTMENT                         
         SR    RE,RE                                                            
         D     RE,=F'60'           GIVES MIN IN RE, HRS IN RF                   
         MHI   RF,100                                                           
         AR    RF,RE                                                            
         STH   RF,DUB                                                           
         B     EXIT                                                             
         EJECT                                                                  
*============================================================                   
* READ THROUGH THE BUYS AND BUILD A SORT TABLE IN AREC3(12000)                  
* AND ADD TSAR RECORDS FOR EACH BUY                                             
*============================================================                   
                                                                                
SE30     L     R0,AREC3            POINT TO START OF BUFFER                     
         L     R1,=F'12000'                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R5,AREC3                                                         
         USING SRTRECD,R5                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY       A-M/CLT/PRD/MKT/STA/EST                      
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    SE34                                                             
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOBUYS)                                                
         GOTO1 ERROR                                                            
*                                                                               
SE32     GOTO1 SEQ                                                              
         CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST                 
         BNE   SE50                                                             
         CLI   KEY+10,0            TEST PASSIVE POINTER                         
         BNE   SE32                YES - SKIP                                   
*                                                                               
SE34     MVC   AREC,AREC1                                                       
         GOTO1 GETREC                                                           
         CLI   SVPOLPRD,0          TEST BRAND POL BY BRAND                      
         BE    SE35                NO                                           
         CLC   BDMASPRD(1),SVPOLPRD                                             
         BE    SE35                                                             
         CLC   BDMASPRD+1(1),SVPOLPRD                                           
         BNE   SE32                                                             
* TEST FOR OVERLAP WITH MISSED LIST OR MAKEGOOD LIST                            
SE35     LA    R4,MISSLIST                                                      
         BAS   RE,CHKOVLAP                                                      
         BE    SE36                USE THIS BUY                                 
         LA    R4,OFFRLIST                                                      
         BAS   RE,CHKOVLAP                                                      
         BNE   SE32                                                             
*                                                                               
SE36     BRAS  RE,GETSKED          BUILD SCHEDULE (READS TSAR)                  
         OC    SKED,SKED           IF NO SPOTS                                  
         BZ    SE32                NO TSAR RECORD                               
*                                                                               
         XC    MBREC,MBREC                                                      
         MVI   MBTYPE,MBTYPEQ                                                   
         MVC   MBRECID,BUYKEY+10   SET BUYLINE NUMBER IN KEY                    
         MVC   MBDAYS,BDDAY                                                     
         SR    R0,R0                                                            
         IC    R0,BDSEDAY                                                       
         SRL   R0,4                                                             
         STC   R0,MBOOW            SET OOW START DAY                            
         MVC   MBSTIM(4),BDTIMST                                                
         MVC   MBSLN,BDSEC                                                      
         MVI   MBLNUNIT,C'S'                                                    
         MVC   MBDPT,BDDAYPT                                                    
         MVC   MBADJ,BDPROGT                                                    
         MVC   MBPROG,BDPROGRM                                                  
         MVC   MBCOST,BDCOST                                                    
         GOTO1 VDATCON,DMCB,(3,BDSTART),(19,MBSTRT) PWOS JULIAN                 
         MVC   MBWKS,BDWKS                                                      
         MVC   MBNPW,BDNOWK                                                     
         MVC   MBMGDATE,BDMGDATE                                                
*                                                                               
         LA    R4,SVDEMOS                                                       
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R4,SVBRDEMS                                                      
         LA    R1,MBDEM1                                                        
*                                                                               
SE38     BAS   RE,PULLDEM          EXTRACT DEMO FROM DEMEL                      
         LA    R4,3(R4)                                                         
         OC    0(3,R4),0(R4)                                                    
         BZ    SE40                                                             
         LA    R1,4(R1)                                                         
         LA    R0,MBDEM6                                                        
         CR    R1,R0                                                            
         BNH   SE38                                                             
*                                                                               
SE40     MVC   MBSKED(28),SKED                                                  
*                                                                               
         MVI   T.TSACTN,TSAADD                                                  
         BRAS  RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SE42     MVC   SRTSEDAY,BDSEDAY                                                 
         MVC   SRTTIME,BDTIMST                                                  
         MVC   SRTPROG,BDPROGRM                                                 
         MVI   SRTTYPE,C'B'                                                     
         MVC   SRTTSAR,T.TSRNUM     SET TSAR RECORD NUMBER                      
         LA    R5,SRTNEXT                                                       
         B     SE32                                                             
*                                                                               
PULLDEM  LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         IC    R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BNPR  RE                                                               
         SRL   R0,3                LOOP ON NUMBER OF DEMOS                      
         LA    R6,24(R6)           POINT TO FIRST DEMO                          
*                                                                               
PULLDEM2 CLC   0(3,R4),0(R6)                                                    
         BE    PULLDEM4                                                         
         LA    R6,8(R6)                                                         
         BCT   R0,PULLDEM2                                                      
         BR    RE                                                               
*                                                                               
PULLDEM4 MVC   0(4,R1),4(R6)                                                    
         NI    0(R1),X'7F'       DROP OVRD FLAG                                 
         BR    RE                                                               
*============================================================                   
* ADD AN ENTRY TO SORT BUFFER FOR EACH MAKEGOOD LINE                            
*============================================================                   
                                                                                
SE50     CLI   SVMNSTAT,C'O'       TEST OFFER 'OKAYED'                          
         BE    SE60                YES - OFFERS ARE NOW BUYLINES                
*                                                                               
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
*                                                                               
SE52     BRAS  RE,CALLTSAR                                                      
         BNE   SE60                                                             
         MVI   T.TSACTN,TSANXT                                                  
         CLI   MSTYPE,MOTYPEQ                                                   
         BNE   SE52                                                             
         CLI   MOCOMNUM,0          TEST COMMENT                                 
         BNE   SE52                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,MODAYS                                                        
         BAS   RE,SETSEDAY                                                      
         STC   R1,SRTSEDAY                                                      
*                                                                               
         MVC   SRTTIME,MOSTIM                                                   
         MVC   SRTPROG,MOPROG                                                   
         MVI   SRTTYPE,C'O'                                                     
         MVC   SRTTSAR,T.TSRNUM    SAVE TSAR RECORD NUMBER                      
         LA    R5,SRTNEXT                                                       
         B     SE52                                                             
*                                                                               
CHKOVLAP NTR1                                                                   
*                                                                               
CHKOV2   SR    R0,R0                                                            
         IC    R0,0(R4)            GET MISSED DAY FROM LIST                     
         SR    R1,R1                                                            
         IC    R1,BDDAY            DAYS FOR BUY                                 
         NR    R0,R1                                                            
         BZ    CHKOV6              NO OVERLAP                                   
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,BDTIMST        GET START TIME                               
         CHI   R0,600                                                           
         BNL   *+8                                                              
         AHI   R0,2400                                                          
         CLM   R0,3,3(R4)          BUY START AFTER MISSED ENDS                  
         BH    CHKOV6                                                           
*                                                                               
         OC    BDTIMEND,BDTIMEND   IF NO END TIME, USE START AGAIN              
         BZ    CHKOV4                                                           
         ICM   R0,3,BDTIMEND                                                    
         CHI   R0,600                                                           
         BNL   *+8                                                              
         AHI   R0,2400                                                          
*                                                                               
CHKOV4   CLM   R0,3,1(R4)          BUY END BEFORE MISSED STARTS                 
         BNL   EQXIT                                                            
*                                                                               
CHKOV6   LA    R4,5(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   CHKOV2                                                           
         B     NEQXIT                                                           
                                                                                
*=============================================================                  
* SET START END DAYS FOR BIT STRING IN R0. RESULT IN R1                         
* NOTE THERE IS NO NTR AND USES RELATIVE ADDRESSING                             
*=============================================================                  
                                                                                
SETSEDAY LHI   R1,1                SET VALUE FOR MONDAY                         
         SLL   R0,25               MOVE MONDAY BIT TO BIT0                      
*                                                                               
SETSE2   LTR   R0,R0                                                            
         JM    SETSE4                                                           
         AHI   R1,1                                                             
         SLL   R0,1                                                             
         J     SETSE2                                                           
*                                                                               
SETSE4   MHI   R1,17               COPY VALUE TO LHS OF REG                     
*                                                                               
SETSE6   SLL   R0,1                                                             
         LTR   R0,R0                                                            
         BZR   RE                                                               
         AHI   R1,1                                                             
         J     SETSE6                                                           
         EJECT                                                                  
*=========================================================                      
* DISPLAY THE SORTED BUYS                                                       
*=========================================================                      
         SPACE 1                                                                
*                                                                               
SE60     MVC   DMCB+4(4),=X'D9000A'                                             
         MVI   DMCB+7,QQSORT                                                    
         GOTO1 VCALLOV,DMCB,0                                                   
         ICM   RF,15,0(R1)                                                      
         BP    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,AREC3            START OF SORT RECORDS                        
         SR    R5,RE               GIVES LENGTH OF SORT DATA                    
         SRL   R5,5                GIVES NUMBER OF RECORDS IN R5                
         ST    R5,DMCB+4                                                        
*                                                                               
         LHI   R0,L'SRTREC                                                      
         ST    R0,DMCB+8           SET RECORD LENGTH                            
         BCTR  R0,0                                                             
         ST    R0,DMCB+12          SET KEYLEN (=RECLEN-1)                       
         SR    R0,R0                                                            
         ST    R0,DMCB+16          SET DSPL OF KEY IN REC                       
*                                                                               
         GOTO1 (RF),DMCB,AREC3     SORT THE RECORDS                             
                                                                                
*=========================================================                      
* EXTRACT SKED DATA, ADJUST FOR MISSED SPOTS, AND DISPLAY                       
*=========================================================                      
                                                                                
         L     R5,AREC3                                                         
         USING SRTRECD,R5                                                       
*                                                                               
         LH    R0,MGTSRNUM         GET STARTING RECORD NUMBER                   
         B     *+8                                                              
         LA    R5,SRTNEXT          POSITION TO STARTING RECORD                  
         BCT   R0,*-4                                                           
         CLI   0(R5),0             MAKE SURE SOMETHING THERE                    
         BNE   SE62                SET 'NO MORE TO DISPLAY'                     
         AHI   R0,-(NUMLINES-1)    BACK UP TO PREVIOUS DISPLAY                  
         B     SE82                AND SAY 'NO MAS'                             
*                                                                               
SE62     LA    R2,SKELIN1H                                                      
         USING LINED,R2                                                         
*                                                                               
         XC    8(LLEN-8,R2),8(R2)    CLEAR THE SCREEN                           
         AHI   R2,LLEN                                                          
         CLI   0(R2),LLEN                                                       
         BE    *-14                                                             
         LA    R2,SKELIN1H                                                      
*                                                                               
SE70     CLI   SRTTYPE,C'B'        TEST BUY                                     
         BNE   SE74                                                             
*                                                                               
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,SRTTSAR                                                 
         BRAS  RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,DSPBUY           DISPLAY BUY DETAILS                          
*                                                                               
         CLI   DSPFMT,0            TEST SKED DISPLAY                            
         BE    SE72                                                             
         CLI   DSPFMT,C'-'         TEST DIFF DISPLAY                            
         BNE   SE78                NO                                           
*                                                                               
SE72     XC    SKED,SKED                                                        
         MVC   SKED(28),MBSKED                                                  
         BRAS  RE,DSPSKED                                                       
         B     SE80                                                             
*                                                                               
SE74     DS    0H                                                               
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,SRTTSAR    SET TSAR RECORD NUMBER                       
         BRAS  RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,DSPOFF           DISPLAY OFFER DETAILS                        
         CLI   DSPFMT,0            TEST SKED DISPLAY                            
         BE    SE76                                                             
         CLI   DSPFMT,C'-'         TEST DIFF DISPLAY                            
         BNE   SE78                                                             
*                                                                               
SE76     BRAS  RE,OFFSKED          BUILD SKED DATA IN SKED                      
         BRAS  RE,DSPSKED          AND DISPLAY IT                               
         B     SE80                                                             
*                                                                               
SE78     BRAS  RE,DSPDEM                                                        
*                                                                               
SE80     OI    LHDRH+6,X'80'       SET TRANSMIT                                 
         AHI   R5,L'SRTREC                                                      
         OC    0(2,R5),0(R5)       TEST ANY MORE DATA                           
         BZ    SE82                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO NEXT LINE                           
         CLI   0(R2),LLEN          TEST ANOTHER DISPLAY LINE                    
         BE    SE70                                                             
         BRAS  RE,MOREMSG                                                       
         MVC   BUYMSG(40),0(R1)                                                 
         J     EXIT                                                             
*                                                                               
SE82     BRAS  RE,NOMORMSG                                                      
         MVC   BUYMSG(40),0(R1)                                                 
         J     EXIT                                                             
         EJECT                                                                  
*============================================================                   
* SET SKED PERIOD START/END DATES (MAX 14 WEEKS)                                
*============================================================                   
                                                                                
SETSKED  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    SVSKPER(2),SVSKPER  FIRST TIME THROUGH?                          
         BNZ   STSK10              NO                                           
         MVC   WORK(6),SVSTART     YES, USE ESTIMATE START DATE                 
         GOTO1 VDATCON,DMCB,WORK,(2,SVSKPER)                                    
         B     STSK20                                                           
STSK10   GOTO1 VDATCON,DMCB,(2,SVSKPER),WORK                                    
*                                                                               
STSK20   LA    R5,14                                                            
*                                                                               
STSK30   GOTO1 VADDAY,DMCB,WORK,WORK+6,F'7'                                     
         CLC   WORK+6(6),SVEND     COMPARE TO EST END                           
         BH    STSK40                                                           
         MVC   WORK(6),WORK+6                                                   
         BCT   R5,STSK30                                                        
         B     STSK50                                                           
*                                                                               
STSK40   MVC   WORK(6),WORK+6                                                   
*                                                                               
STSK50   GOTO1 (RF),(R1),WORK,WORK+6,F'-1' BACK UP TO GET LAST DAY              
         GOTO1 VDATCON,DMCB,WORK+6,(2,SVSKPER+2)                                
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* EXTRACT SCHEDULE DATA FOR A BUY                                               
*=========================================================                      
                                                                                
GETSKED  NTR1  BASE=*,LABEL=*                                                   
         XC    SKED,SKED                                                        
*                                                                               
         LA    R1,SVMVDTS                                                       
         LA    R4,SKED                                                          
         LA    R6,BDELEM                                                        
         SR    R7,R7                                                            
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
GETSK2   BRAS  RE,NEXTEL                                                        
         BNE   GETSK6                                                           
         CLC   2(2,R6),0(R1)       BEFORE WEEK START DATE                       
         BL    GETSK2                                                           
         CLC   2(2,R6),SVSKPER+2   AFTER PERIOD END DATE                        
         BH    GETSK6              YES - DONE                                   
         OC    2(2,R1),2(R1)       TEST LAST WEEK                               
         BZ    *+14                                                             
         CLC   2(2,R6),2(R1)       AFTER WEEK END                               
         BNL   GETSK6                                                           
         TM    6(R6),X'C0'         TEST MINUS OR MINUSSED                       
         BNZ   GETSK2                                                           
         BCT   R7,GETSK2           BUMP COUNTER AND CONTINUE                    
*                                                                               
GETSK6   LPR   R7,R7                                                            
         STC   R7,0(R4)                                                         
*                                                                               
GETSK8   LA    R4,2(R4)            NEXT COUNTER POSN                            
         LA    R1,2(R1)            NEXT WEEK IN LIST                            
         OC    0(2,R1),0(R1)                                                    
         JZ    GETSK10                                                          
         LA    R6,BDELEM                                                        
         SR    R7,R7                                                            
         B     GETSK2                                                           
                                                                                
*==============================================================                 
* SEE IF ANY MISSED SPOTS ON THIS BUYLINE AND APPLY THEM                        
*==============================================================                 
                                                                                
GETSK10  CLI   SVMNSTAT,C'O'       TEST 'OKAYED'                                
         BE    GETSK20             YES- GET MINUS OTOS FROM BUYS                
*                                                                               
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
*                                                                               
GETSK12  BRAS  RE,CALLTSAR                                                      
         BNE   GETSK30                                                          
         MVI   T.TSACTN,TSANXT                                                  
         CLI   MSTYPE,MSTYPEQ                                                   
         BNE   GETSK12                                                          
*                                                                               
         CLC   MSACTBUY,BUYKEY+10  MATCH LINE NUMBER                            
         BNE   GETSK12             NO                                           
*                                                                               
         LA    R1,SVMVDTS                                                       
         LA    R4,SKED                                                          
*                                                                               
GETSK14  CLC   MSACTDAT,0(R1)      BEFORE WEEK START DATE                       
         BL    GETSK16             YES                                          
         CLC   MSACTDAT,SVSKPER+2  AFTER PERIOD END DATE                        
         BH    GETSK12             YES - TRY NEXT MISSED                        
         OC    2(2,R1),2(R1)       TEST LAST WEEK                               
         BZ    GETSK18                                                          
         CLC   MSACTDAT,2(R1)      AFTER WEEK END                               
         BL    GETSK18             NO - MISS THIS WEEK                          
*                                                                               
GETSK16  LA    R4,2(R4)            NEXT SKED ENTRY                              
         LA    R1,2(R1)            NEXT WEEK ENTRY                              
         B     GETSK14                                                          
*                                                                               
GETSK18  SR    R0,R0                                                            
         IC    R0,1(R4)            GET CURRENT MISSED COUNT                     
         AHI   R0,1                                                             
         STC   R0,1(R4)                                                         
         B     GETSK12             AND LOOK FOR MORE                            
                                                                                
* TRY TO EXTRACT -OTO'S FROM BUYS                                               
* BUT FOR NOW THE -SPOTS DIDN'T COUNT, SO IGNORE                                
                                                                                
GETSK20  B     GETSK30                                                          
*                                                                               
GETSK30  J     EXIT                                                             
         LTORG                                                                  
*=========================================================                      
* EXTRACT SCHEDULE DATA FOR A MAKEGOOD OFFER                                    
*=========================================================                      
                                                                                
OFFSKED  NTR1  BASE=*,LABEL=*                                                   
         XC    SKED,SKED                                                        
* GET START DATE IN 2-BYTE FORMAT                                               
         GOTO1 VDATCON,DMCB,(8,MOSTRT),(2,HALF)                                 
*                                                                               
         LA    R1,SVMVDTS                                                       
         LA    R4,SKED                                                          
*                                                                               
OFFSK2   CLC   HALF,0(R1)          BEFORE WEEK START DATE                       
         BL    OFFSK4                                                           
         CLC   HALF,SVSKPER+2      AFTER PERIOD END DATE                        
         JH    EXIT                YES - DONE                                   
         OC    2(2,R1),2(R1)       TEST LAST WEEK                               
         BZ    OFFSK10                                                          
         CLC   HALF,2(R1)          AFTER WEEK END                               
         BL    OFFSK10             NO - IT'S IN THIS WEEK                       
*                                                                               
OFFSK4   LA    R1,2(R1)            NEXT WEEK                                    
         LA    R4,2(R4)            NEXT SKED ENTRY                              
         B     OFFSK2                                                           
*                                                                               
OFFSK10  SR    R0,R0                                                            
         IC    R0,MOWKS            GET NUMBER OF WEEKS                          
*                                                                               
OFFSK12  MVC   0(1,R4),MONPW       SET SPOTS/WEEK                               
         LA    R1,2(R1)            NEXT WEEK                                    
         OC    0(2,R1),0(R1)       TEST REACHED EOL                             
         JZ    EXIT                                                             
         LA    R4,2(R4)            NEXT SKED TABLE ENTRY                        
         BCT   R0,OFFSK12                                                       
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
         USING LINED,R2                                                         
*                                                                               
DSPBUY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,MBRECID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LLIN(3),DUB                                                      
*                                                                               
         MVI   LLIN+3,C'+'         INDICATOR  FOR MAKEGOOD                      
         OC    MBMGDATE,MBMGDATE   TEST LINE IS A MAKEGOOD                      
         BNZ   DSB2                                                             
         MVI   LLIN+3,C' '                                                      
*                                                                               
DSB2     LA    R0,7                                                             
         LA    R1,LDAYS                                                         
         MVC   0(7,R1),=C'MTWTFSS'                                              
*                                                                               
         IC    RE,MBDAYS                                                        
         SLL   RE,25               GET MONDAY BIT LEFT ALIGNED                  
*                                                                               
DSB4     LTR   RE,RE               REG IS NEG IF DAY BIT ON                     
         BM    *+8                                                              
         MVI   0(R1),C'.'                                                       
*                                                                               
         LA    R1,1(R1)                                                         
         SLL   RE,1                                                             
         BCT   R0,DSB4                                                          
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A11'  GET UNTIME ADDRESS                   
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),MBSTIM,LTIME                                           
*                                                                               
         MVC   LDPTLEN(1),MBDPT                                                 
*                                                                               
         SR    R0,R0                                                            
         IC    R0,MBSLN                                                         
         EDIT  (R0),(3,LDPTLEN+1),ALIGN=LEFT                                    
*                                                                               
         MVC   LPROG,MBPROG                                                     
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* DISPLAY AN OFFER                                                              
*================================================================               
         SPACE 1                                                                
DSPOFF   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   LLIN,=C'*MG'                                                     
*                                                                               
         LA    R0,7                                                             
         LA    R1,LDAYS                                                         
         MVC   0(7,R1),=C'MTWTFSS'                                              
*                                                                               
         IC    RE,MODAYS                                                        
         SLL   RE,25               GET MONDAY BIT LEFT ALIGNED                  
*                                                                               
DSOF4    LTR   RE,RE               REG IS NEG IF DAY BIT ON                     
         BM    *+8                                                              
         MVI   0(R1),C'.'                                                       
*                                                                               
         LA    R1,1(R1)                                                         
         SLL   RE,1                                                             
         BCT   R0,DSOF4                                                         
*                                                                               
         L     RF,VUNTIME                                                       
         GOTO1 (RF),DMCB,MOSTIM,LTIME                                           
*                                                                               
         MVC   LDPTLEN(1),MODPT                                                 
*                                                                               
         SR    R0,R0                                                            
         IC    R0,MOSLN                                                         
         EDIT  (R0),(3,LDPTLEN+1),ALIGN=LEFT                                    
*                                                                               
         MVC   LPROG,MOPROG                                                     
         J     EXIT                                                             
         EJECT                                                                  
*========================================================                       
* DISPLAY SPOTS IN SKED                                                         
*========================================================                       
                                                                                
DSPSKED  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,LSKED                                                         
         LA    R5,SKED                                                          
         LHI   R6,14                                                            
*                                                                               
DSK2     CLI   DSPFMT,C'-'         TEST DIFF DISPLAY                            
         BNE   DSK10                                                            
         SR    R0,R0                                                            
         IC    R0,1(R5)            GET MISSED COUNT                             
         CLI   MSTYPE,C'B'         TEST BUY                                     
         BNE   DSK4                                                             
         LTR   R0,R0                                                            
         BZ    DSK30                                                            
         EDIT  (R0),(2,1(R4))                                                   
         LA    R1,C'-'             PUT A - SIGN IN R1                           
         B     DSK20                                                            
*                                                                               
DSK4     ICM   R0,1,0(R5)          OFFER SHOWS SPOT COUNT                       
         BZ    DSK30                                                            
         EDIT  (R0),(3,0(R4))                                                   
         LA    R1,C'+'                                                          
         B     DSK20                                                            
*                                                                               
DSK10    SR    R0,R0                                                            
         IC    R0,0(R5)            GET SPOT COUNT                               
         SR    RE,RE                                                            
         ICM   RE,1,1(R5)          GET MISSED SPOT COUNT                        
         SR    R0,RE                                                            
         CLI   1(R5),0             TEST ANY MISSED SPOTS                        
         BNZ   DSK16                                                            
         EDIT  (R0),(2,1(R4))                                                   
         B     DSK30                                                            
*                                                                               
DSK16    EDIT  (R0),(2,1(R4)),ZERO=NOBLANK                                      
         LA    R1,C'!'                                                          
         OI    LHDRH+6,X'88'       SET XMT/HIGH INT                             
*                                                                               
DSK20    LR    RE,R4                                                            
         CLI   1(R4),C' '          TEST USED TWO DISP CHARS                     
         BH    *+8                 YES                                          
         LA    RE,1(RE)                                                         
         STC   R1,0(RE)            SET APPROPRIATE CHAR                         
*                                                                               
DSK30    LA    R4,3(R4)                                                         
         LA    R5,2(R5)                                                         
         BCT   R6,DSK2                                                          
         J     EXIT                                                             
         EJECT                                                                  
*=========================================================                      
* DISPLAY DEMOS OR CPPS                                                         
*=========================================================                      
                                                                                
DSPDEM   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,LDEMOS                                                        
         LA    R5,MBDEM1                                                        
         CLI   MSTYPE,C'B'         TEST BUY                                     
         BE    *+8                                                              
         LA    R5,MODEM1                                                        
         LHI   RF,6                                                             
*                                                                               
DDM20    CLI   DSPFMT,C'C'         TEST CPP                                     
         BE    DDM22               YES                                          
         SR    R0,R0                                                            
         ICM   R0,15,0(R5)                                                      
         BZ    DDM30                                                            
         EDIT  (R0),(5,(R4)),1                                                  
         B     DDM30                                                            
*                                                                               
DDM22    SR    R1,R1               DISPLAY CPP                                  
         ICM   R1,7,MBCOST         GET DOLLARS (IN PENNIES)                     
         CLI   MSTYPE,C'B'         TEST BUY OR OFFER                            
         BE    *+8                                                              
         ICM   R1,7,MOCOST                                                      
         M     R0,=F'20'           X 2 X10                                      
         ICM   RE,15,0(R5)         POINTS TO 1 DECIMAL                          
         BZ    DDM30                                                            
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LR    R0,R1                                                            
         CHI   R0,9999             NEED FOUR DIGITS ?                           
         BH    DDM24                                                            
         EDIT  (R0),(5,(R4)),2                                                  
         B     DDM30                                                            
*                                                                               
DDM24    AR    R0,R0               R0 X 2                                       
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LR    R0,R1                                                            
         EDIT  (R0),(5,(R4)),0,FLOAT=$                                          
*                                                                               
DDM30    LA    R4,7(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   RF,DDM20                                                         
         J     EXIT                                                             
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*=========================================================                      
* REBUILD THE SORT BUFFER FROM THE TSAR RECORDS AND                             
* LOCATE THE FIRST ENTRY TO BE DISPLAYED                                        
*=========================================================                      
                                                                                
RBTSAR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,AREC3                                                         
         USING SRTRECD,R5                                                       
*                                                                               
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
*                                                                               
RBTS2    BRAS  RE,CALLTSAR                                                      
         BNE   RBTSX                                                            
         MVI   T.TSACTN,TSANXT                                                  
*                                                                               
         CLI   MSTYPE,MOTYPEQ      TEST OFFER                                   
         BNE   RBTS10              NO                                           
         CLI   SVMNSTAT,C'O'       TEST OFFER OKAYED                            
         BE    RBTS2               YES - IGNORE OFFERS                          
         CLI   MOCOMNUM,0          TEST COMMENT                                 
         BNE   RBTS2                                                            
* MAKEGOOD OFFER                                                                
         SR    R0,R0                                                            
         IC    R0,MODAYS                                                        
         BRAS  RE,SETSEDAY                                                      
         STC   R1,SRTSEDAY                                                      
*                                                                               
         MVC   SRTTIME,MOSTIM                                                   
         MVC   SRTPROG,MOPROG                                                   
         MVI   SRTTYPE,C'O'                                                     
         MVC   SRTTSAR,T.TSRNUM    SAVE TSAR RECORD NUMBER                      
         LA    R5,SRTNEXT                                                       
         B     RBTS2                                                            
*                                                                               
RBTS10   CLI   MSTYPE,C'B'         TEST BUY                                     
         BNE   RBTS2               NO - IGNORE                                  
*                                                                               
         SR    R0,R0                                                            
         IC    R0,MBDAYS                                                        
         BRAS  RE,SETSEDAY                                                      
         STC   R1,SRTSEDAY                                                      
*                                                                               
         MVC   SRTTIME,MBSTIM                                                   
         MVC   SRTPROG,MBPROG                                                   
         MVI   SRTTYPE,C'B'                                                     
         MVC   SRTTSAR,T.TSRNUM    SAVE TSAR RECORD NUMBER                      
         LA    R5,SRTNEXT                                                       
         B     RBTS2                                                            
*                                                                               
RBTSX    ST    R5,FULL                                                          
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* BUILD HEADLINES FOR SCHEDULE DISPLAY                                          
*=========================================================                      
                                                                                
SEHL     NTR1  BASE=*,LABEL=*                                                   
         XC    BUDATA,BUDATA                                                    
         XC    SVMVDTS(30),SVMVDTS                                              
         XC    SKEHL2B,SKEHL2B                                                  
         XC    SKEHL3B,SKEHL3B                                                  
         OI    SKEHL2BH+6,X'80'                                                 
         OI    SKEHL3BH+6,X'80'                                                 
*                                                                               
         CLI   DSPFMT,C'D'         TEST DEMO DISPLAY                            
         BE    SEHL2                                                            
         CLI   DSPFMT,C'C'         TEST CPP DISPLAY                             
         BNE   SEHL10                                                           
*                                                                               
SEHL2    LA    R1,SKEHL2B                                                       
         LA    R0,6                                                             
         LA    RE,MGDEMNM1                                                      
*                                                                               
SEHL4    MVC   0(6,R1),0(RE)                                                    
         LA    R1,7(R1)                                                         
         LA    RE,6(RE)                                                         
         BCT   R0,SEHL4                                                         
         J     EXIT                                                             
                                                                                
*============================================================                   
* BUILD MONTH TITLES                                                            
*============================================================                   
                                                                                
SEHL10   GOTO1 VDATCON,DMCB,(2,SVSKPER),WORK                                    
         LA    R4,BUDATA+6                                                      
         LA    R5,14               MAX WEEKS                                    
         LA    R6,SVMVDTS                                                       
*                                                                               
SEHL12   GOTO1 VDATCON,DMCB,WORK,(2,(R6))  GET 2BYTE DATES                      
         GOTO1 (RF),(R1),,(4,(R4))         GET MMMDD DATES                      
         BAS   RE,TESTHIAT                 TEST HIATUS WEEK                     
         BNE   *+10                                                             
         MVC   3(2,R4),=C'*H'                                                   
*                                                                               
         LA    R4,5(R4)                                                         
         LA    R6,2(R6)                                                         
         GOTO1 VADDAY,DMCB,WORK,WORK+6,F'7'                                     
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK(6),SVEND                                                    
         BH    SEHL20                                                           
         BCT   R5,SEHL12                                                        
*                                                                               
* COUNT NUMBER OF WEEKS IN EACH MONTH                                           
*                                                                               
SEHL20   LA    R4,BUDATA           WEEK COUNTERS                                
*                                                                               
         LHI   R0,1                                                             
         LA    R1,BUDATA+6         FIRST MMMDD DATE                             
*                                                                               
SEHL22   CLC   0(3,R1),5(R1)       NEXT WEEK IN SAME MONTH                      
         BE    SEHL26                                                           
*                                                                               
SEHL24   STC   R0,0(R4)            SET NUM WEEKS THIS MONTH                     
         SR    R0,R0               RESET COUNTER                                
         CLI   5(R1),0             ANY MORE WEEKS                               
         BE    SEHL30              NO                                           
         LA    R4,1(R4)            NEXT COUNTER                                 
*                                                                               
SEHL26   LA    R1,5(R1)                                                         
         AHI   R0,1                                                             
         B     SEHL22                                                           
*                                                                               
SEHL30   LA    R1,SKEHL2B          MONTH LINE                                   
*                                                                               
         LA    R4,BUDATA           POINT TO WEEKS/MONTH LIST                    
         LA    R5,BUDATA+6         POINT TO FIRST WEEK                          
*                                                                               
SEHL32   SR    RE,RE                                                            
         ICM   RE,1,0(R4)          TEST ANY WEEKS THIS MONTH                    
         BZ    SEHL34              NO - DONE                                    
         BCTR  RE,0                                                             
         MHI   RE,L'SEHLTAB                                                     
         LA    RE,SEHLTAB(RE)      POINT TO TABLE ENTRY                         
         SR    RF,RF                                                            
         IC    RF,0(RE)            DATA LEN                                     
         BCTR  RF,0                                                             
         MVC   0(0,R1),2(RE)       MOVE IN TITLE                                
         EX    RF,*-6                                                           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,1(RE)            GET DSPL TO XXX                              
         AR    RF,R1                                                            
         MVC   0(3,RF),0(R5)       MOVE MONTH OVER XXX                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,0(RE)            GET ENTRY LENGTH FROM TABLE                  
         AR    R1,RF               NEXT POSN IN MONTH LINE                      
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R4)            NUMBER OF WEEKS THIS MONTH                   
         MHI   R0,5                                                             
         AR    R5,R0               FIRST MMMDD OF NEXT MONTH                    
         LA    R4,1(R4)            NEXT MONTH WEEK COUNT                        
         B     SEHL32                                                           
*                                                                               
SEHL34   LA    R5,BUDATA+6         POINT TO FIRST WEEK                          
         LA    R1,SKEHL3B          POINT TO WEEK LINE                           
*                                                                               
SEHL36   MVC   1(2,R1),3(R5)                                                    
         LA    R1,3(R1)                                                         
         LA    R5,5(R5)            NEXT WEEK                                    
         CLI   0(R5),0                                                          
         BNE   SEHL36                                                           
         XC    BUDATA,BUDATA                                                    
         J     EXIT                                                             
*                                                                               
SEHLTAB  DS    0CL17               LEN TO MOVE, DSPL TO XXX                     
         DC    AL1(3,0),CL15'XXX'                                               
         DC    AL1(6,2),CL15' -XXX-'                                            
         DC    AL1(9,3),CL15' --XXX---'                                         
         DC    AL1(12,5),CL15' ----XXX----'                                     
         DC    AL1(15,6),CL15' -----XXX------'                                  
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* SEARCH GOALS TABLE TO SEE IF THIS IS A HIATUS WEEK                            
* 2 BYTE DATE IS AT 0(R6)                                                       
*===============================================================                
                                                                                
TESTHIAT NTR1  BASE=*,LABEL=*                                                   
         TM    SVOPT2,X'20'        TEST OPTION TO IGNORE GOALS                  
         BO    TESTHX                                                           
         CLI   SVCXTRA+8,C'Y'      TEST OPTION ON AT ALL                        
         BNE   TESTHX                                                           
*                                                                               
         LHI   RF,WSGLTAB-SPBUYWKD GET DSPL TO GOAL TABLE                       
         AR    RF,RC               POINT TO TABLE OF SUNDAY DATES               
         SR    RE,RE               CLEAR COUNTER                                
         LR    R4,RF                                                            
         AHI   R4,WSGLDPTS-WSGLTABD POINT TO TABLE OF DPT/WEEKS                 
*                                                                               
TESTH2   CLC   0(2,R6),0(RF)       TEST SPOT IN WEEK                            
         BNH   TESTH4                                                           
         LA    RF,2(RF)            TABLE ENDS WITH X'FFFF'                      
         BCT   RE,TESTH2           SO NO TEST FOR END OF TABLE NEEDED           
*                                                                               
TESTH4   LPR   RE,RE                                                            
         SRDL  RE,3                DIVIDE BY 8                                  
*                                                                               
         SRL   RF,29               SHIFT REMAINDER                              
         IC    R0,GLBITTAB(RF)     GET BIT VALUE                                
*                                                                               
TESTH6   STC   R0,BYTE                                                          
         LA    R1,2(R4,RE)         POINT TO BYTE IN TABLE ENTRY                 
         NC    BYTE,0(R1)          TEST BIT ON (W/O CHANGING TABLE)             
         BNZ   TESTHX                                                           
*                                                                               
         LA    R4,10(R4)           NEXT DAYPART ENTRY                           
         CLI   0(R4),0                                                          
         BNE   TESTH6                                                           
*                                                                               
TESTHX   XIT1                      EXIT NEQ IF GOALS THIS WEEK                  
*                                                                               
GLBITTAB DC    X'8040201008040201'                                              
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* ADJUST PERIOD START DATE BY NUMBER OF WEEKS IN R1              *              
*================================================================*              
                                                                                
ADJPER   NTR1  BASE=*,LABEL=*                                                   
         XC    BUDATA,BUDATA                                                    
         GOTO1 VDATCON,DMCB,SVSTART,(2,BUDATA)                                  
*                                                                               
         LA    R4,BUDATA+2                                                      
         MVC   WORK(6),SVSTART                                                  
         GOTO1 VGETDAY,DMCB,SVSTART,WORK+6                                      
         CLI   0(R1),1             TEST MONDAY                                  
         BE    ADJPER4                                                          
         SR    R5,R5                                                            
         IC    R5,0(R1)                                                         
         BCTR  R5,0                NUMBER OF DAYS TO BACK UP                    
         LCR   R5,R5                                                            
         GOTO1 VADDAY,DMCB,SVSTART,WORK,(R5)  BACK UP TO MONDAY                 
*                                                                               
ADJPER4  GOTO1 VADDAY,DMCB,WORK,WORK+6,F'7'   AND GET NEXT MONDAY               
         MVC   WORK(6),WORK+6                                                   
         GOTO1 VDATCON,DMCB,WORK,(2,(R4))                                       
         CLC   WORK(6),SVEND                  TEST PAST EST END                 
         BNL   ADJPER6                                                          
         LA    R4,2(R4)                                                         
         B     ADJPER4                                                          
*                                                                               
ADJPER6  XC    0(2,R4),0(R4)       PREVIOUS WEEK IS LAST                        
*                                                                               
         LR    R5,R4               SAVE ADDR OF LAST TABLE ENTRY                
         AHI   R5,-2                                                            
         LA    R4,BUDATA                                                        
*                                                                               
ADJPER8  CLC   0(2,R4),SVSKPER     FIND CURRENT WEEK IN TABLE                   
         BE    ADJPER10                                                         
         LA    R4,2(R4)                                                         
         OC    0(2,R4),0(R4)                                                    
         BNZ   ADJPER8                                                          
         DC    H'0'                                                             
*                                                                               
ADJPER10 LTR   R0,R0                                                            
         BM    ADJPER20                                                         
         LR    RE,R0               GET NUMBER OF WEEKS TO ADVANCE               
         AR    RE,RE               X 2                                          
         AR    RE,R4               POINT TO WEEK                                
         MVC   SVSKPER(2),0(RE)    AND SET AS DISPLAY START                     
         LA    RF,28(RE)           POINT 14 WEEKS LATER (MAX DISPLAY)           
         CR    RF,R5               TEST PAST END OF TABLE                       
         BNH   ADJPERX                                                          
         LR    RF,R5                                                            
         AHI   RF,-26              BACK UP 13 MORE WEEKS FROM END               
         MVC   SVSKPER(2),0(RF)    AND USE AS START DATE                        
*                                                                               
         LA    RE,BUDATA           MAKE SURE NOT BEFORE TABLE START             
         CR    RF,RE                                                            
         BNL   *+10                                                             
         MVC   SVSKPER(2),BUDATA   ELSE USE EST START DATE                      
         B     ADJPERX                                                          
*                                                                               
ADJPER20 LR    RE,R0               GET NUMBER OF WEEKS TO BACK UP               
         AR    RE,RE               X 2                                          
         AR    RE,R4               POINT TO WEEK                                
         LA    RF,BUDATA                                                        
         CR    RE,RF               TEST PRIOR TO EST START                      
         BH    *+8                 NO                                           
         LA    RE,BUDATA           USE EST START DATE                           
         MVC   SVSKPER(2),0(RE)                                                 
*                                                                               
ADJPERX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR                                             *             
*=================================================================*             
         SPACE 1                                                                
CALLTSAR LR    R0,RE                                                            
         LA    RE,64                   TSAR BLOCK + TSARKEY                     
         STC   RE,TSARTRC              SET DATA LENGTH IN BYTE 0                
         GOTO1 VDATAMGR,DMCB,QDMTRACE,QDMDATA,TSARTRC                           
         ORG   *-2                                                              
         DC    X'0700'             NOP TRACE                                    
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   T.TSERRS,0              SET CC ON EXIT                           
         LR    RE,R0                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
       ++INCLUDE SPBUY38MSG                                                     
         PRINT OFF                                                              
       ++INCLUDE SPBUYWORK                                                      
SPBUYWKD DSECT                                                                  
         ORG   ELEM                                                             
SKED     DS    XL32                COUNT/FLAGS X'80'=MSSD IN WEEK               
MISSLIST DS    CL80                                                             
MISSLSTX EQU   *                                                                
         DS    CL5                                                              
OFFRLIST DS    CL80                                                             
OFFRLSTX EQU   *                                                                
         DS    CL5                                                              
*                                                                               
SRTRECD  DSECT                                                                  
SRTREC   DS    0XL32                                                            
SRTSEDAY DS    XL1                                                              
SRTTIME  DS    XL4                                                              
SRTPROG  DS    CL17                                                             
SRTTYPE  DS    CL1                 C'B'=BUY,C'O'=OFFER                          
SRTTSAR  DS    XL2                 TSAR RECORD NUMBER FOR OFFER                 
         DS    XL7                                                              
SRTNEXT  EQU   *                                                                
*                                                                               
LINED    DSECT                                                                  
*                                                                               
LHDRH    DS    XL8                                                              
LLIN     DS    CL3                                                              
         DS    CL1                                                              
LDAYS    DS    CL7                                                              
         DS    CL1                                                              
LTIME    DS    CL11                                                             
         DS    CL1                                                              
LDPTLEN  DS    CL3                                                              
         DS    CL1                                                              
LPROG    DS    CL9                                                              
*                                                                               
LSKED    DS    CL42                                                             
LDEMOS   EQU   LSKED                                                            
LLEN     EQU   *-LHDRH                                                          
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE SPBUY31WRK                                                     
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDCOREQUS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022SPBUY38   03/20/15'                                      
         END                                                                    
