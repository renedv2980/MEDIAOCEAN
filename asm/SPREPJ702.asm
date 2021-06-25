*          DATA SET SPREPJ702  AT LEVEL 008 AS OF 02/13/03                      
*PHASE SPJ702A                                                                  
*                                                                               
*==================================================================             
* ADD CODE TO SPOT/BUY TO MAKE SURE SPODS ALL PRESENT                           
* DO NOT ALLOW CHANGE THAT MAKES CONSECUTIVE SPOTS NOT SUM TO BDSEC             
*==================================================================             
*                                                                               
SPJ702   TITLE 'P&&G CANADA CAMPAIGN SCHEDULE'                                  
SPJ702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPJ702                                                       
*                                                                               
         L     RC,=A(SPJ7WORK)                                                  
         USING SPJ7WORK,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
                                                                                
         CLI   MODE,PROCBUY                                                     
         BE    PROCB                                                            
         CLI   MODE,STAFRST                                                     
         BE    STAF                                                             
         CLI   MODE,STALAST                                                     
         BE    STAL                                                             
         CLI   MODE,MKTFRST                                                     
         BE    MKTF                                                             
         CLI   MODE,MKTLAST                                                     
         BE    MKTL                                                             
         CLI   MODE,ESTFRST                                                     
         BE    ESTF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
*=====================================================================*         
* RUNFRST FIRST PROCESSING                                            *         
*=====================================================================*         
RUNF     DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* REQUEST FIRST PROCESSING                                            *         
*=====================================================================*         
                                                                                
REQF     L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         LHI   R0,1                                                             
         STCM  R0,15,MEDNUMPE                                                   
         LHI   R0,256                                                           
         STCM  R0,15,MEDLCHNK                                                   
         XC    MEDNUMMO,MEDNUMMO                                                
         XC    MEDNUMQT,MEDNUMQT                                                
         LHI   R0,14                                                            
         ST    R0,MEDNUMWK                                                      
         MVI   MEDEXTDM,0          NO DEMOS                                     
         MVI   RQDAYPT,C'Y'                                                     
         LHI   R0,1                                                             
         STH   R0,PAGE                                                          
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         LA    RE,MYHDHK                                                        
         ST    RE,HEADHOOK                                                      
         STM   R9,RC,HDHKR9                                                     
*                                                                               
         LA    RE,MYMIDHK                                                       
         ST    RE,MIDHOOK                                                       
         STM   R9,RC,MIDHKR9                                                    
*                                                                               
         L     R4,AMYPRT1                                                       
REQF10   MVC   0(132,R4),SPACES                                                 
         LA    R4,132(R4)                                                       
         C     R4,AMYPRTX                                                       
         BL    REQF10                                                           
*                                                                               
         J     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
*=====================================================================*         
* ESTIMATE FIRST PROCESSING                                           *         
*=====================================================================*         
                                                                                
ESTF     DS    0H                                                               
         GOTO1 MEDDATE,DMCB,SPWORKD                                             
                                                                                
*====================================================================           
* FORMAT DATES FOR HEADLINES                                                    
*====================================================================           
                                                                                
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
*                                                                               
         L     R5,MEDAFRST         GET A(WEEK 1)                                
         L     R4,=A(MYDATES)      2 SETS OF CL90 DATA                          
         MVC   MYDATES,SPACES                                                   
*                                                                               
ESTF30   GOTO1 DATCON,DMCB,(2,(R5)),(4,(R4))   GET MMMDD                        
         MVC   90(5,R4),=C'====='                                               
         LA    R4,6(R4)                                                         
         LA    R5,12(R5)                                                        
         C     R5,MEDALAST                                                      
         BNH   ESTF30                                                           
         MVC   0(5,R4),=C'TOTAL'                                                
         MVC   90(5,R4),=C'====='                                               
* CALCULATE AND SAVE PRINT LINE DSPL FOR TOTALS                                 
         L     R0,=A(MYDATES)                                                   
         SR    R4,R0               GIVES DISPLACEMENT INTO MYDATES              
         STH   R4,TOTDISP                                                       
         J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*=====================================================================*         
* MARKET FIRST                                                        *         
*=====================================================================*         
                                                                                
MKTF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         J     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* STAFRST                                                             *         
*=====================================================================*         
                                                                                
STAF     MVI   STASW,C'N'          SET NO ACTIVITY FLAG                         
         MVI   FORCEMID,C'Y'                                                    
         XC    TOTSPOTS,TOTSPOTS   CLEAR STATION TOTALS                         
*                                                                               
         CLI   LINE,48             AT LEAST 12 LINES FOR A STATION              
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* STALAST                                                             *         
*=====================================================================*         
                                                                                
STAL     MVI   RCSUBPRG,1                                                       
         CLI   STASW,C'Y'          TEST ANY ACTIVITY                            
         JNE   EXIT                                                             
*                                                                               
         MVI   P1,C'='                                                          
         MVC   P1+1(131),P1                                                     
*                                                                               
         MVC   P2,SPACES                                                        
         MVC   P2(19),=C'STATION WABC TOTALS'                                   
         MVC   P2+8(4),BIGSTA                                                   
*                                                                               
         LA    R4,PDATES-P+P2                                                   
         LA    R5,TOTSPOTS                                                      
         LHI   R6,14                                                            
*                                                                               
STAL10   LH    R0,0(R5)                                                         
         EDIT  (R0),(5,(R4))                                                    
         LTR   R0,R0                                                            
         BNP   *+8                                                              
         MVI   5(R4),C'*'                                                       
         LA    R4,6(R4)                                                         
         LA    R5,2(R5)                                                         
         BCT   R6,STAL10                                                        
         MVI   P3,0                FORCE A BLANK LINE AFTER                     
*                                                                               
STALX    GOTO1 REPORT                                                           
         XC    TOTSPOTS,TOTSPOTS                                                
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* BUY RECORD PROCESSING                                               *         
*=====================================================================*         
                                                                                
PROCB    DS    0H                                                               
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
*                                                                               
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
*                                                                               
         CLC   KEY+4(2),BUYKMKTN   TEST MARKETS MATCH                           
         JNE   EXIT                IGNORE SPILL                                 
*                                                                               
         XC    ELEMDATE,ELEMDATE                                                
         XC    BUYSPOTS,BUYSPOTS   CLEAR BUYLINE COUNTER                        
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
*                                                                               
PB2      BRAS  RE,NEXTEL                                                        
         BNE   PB30                                                             
*                                                                               
         CLC   2(2,R6),MEDPERD     TEST PRIOR TO PERIOD START                   
         BL    PB2                                                              
         CLC   2(2,R6),MEDPERD+2   TEST AFTER PERIOD END                        
         BH    PB2                                                              
*                                                                               
         CLI   1(R6),10            TEST ALLOCATED                               
         BNH   PB2                 NO - SKIP FOR NOW                            
         TM    6(R6),X'80'         TEST MINUS                                   
         BO    PB2                                                              
*                                                                               
         MVI   STASW,C'Y'          SET STATION ACTIVE                           
         CLC   ELEMDATE,2(R6)      SAME DATE                                    
         BE    PB10                                                             
*                                                                               
PB4      MVC   ELEMDATE,2(R6)                                                   
         MVI   ELEMNUM,0           CLEAR SPOT PRINTED FLAG                      
         XC    SPODSEC,SPODSEC                                                  
         TM    6(R6),X'40'         TEST MISSED                                  
         BO    PB20                YES - ADD TO SPOD TOTAL                      
*                                                                               
         L     R4,AMYPRT1                                                       
         ST    R4,MYPRTADR                                                      
         CLC   0(132,R4),SPACES    TEST BUY ALREADY FORMATTED                   
         BH    *+8                                                              
         BAS   RE,FMTBUY           FORMAT BUY DESC DATA                         
*                                                                               
         BAS   RE,GETCOL                                                        
         AH    R4,COLDISP          POINT TO PRINT POSITION                      
*                                                                               
PB10     TM    6(R6),X'40'         TEST MISSED                                  
         BO    PB20                YES - ADD TO SPOD TOTAL                      
         C     R4,AMYPRTX          TEST BEYOND LAST PRINT SET                   
         BNH   *+6                 NO                                           
         DC    H'0'                TOO MANY SPODS ON ONE DAY                    
*                                                                               
         L     RF,ADCLT                                                         
         AHI   RF,CLIST-CLTHDRD                                                 
*                                                                               
PB12     CLC   10(1,R6),3(RF)                                                   
         BE    PB14                                                             
         LA    RF,4(RF)                                                         
         CLI   0(RF),C'A'                                                       
         BNL   PB12                                                             
         LA    RF,=C'???'                                                       
*                                                                               
PB14     MVC   0(3,R4),0(RF)                                                    
*                                                                               
         IC    R0,ELEMNUM          ADD TO SPOTS PRINTED COUNTER                 
         AHI   R0,1                                                             
         STC   R0,ELEMNUM                                                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,11(R6)                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(2,R4),DUB                                                      
*                                                                               
         LA    R4,132(R4)          NEXT PRINT LINE                              
*                                                                               
PB20     SR    R0,R0                                                            
         IC    R0,11(R6)                                                        
         AH    R0,SPODSEC          ADD SECONDS TO SPOD TOTAL                    
         STH   R0,SPODSEC                                                       
         CLC   SPODSEC+1(1),BDSEC  COMPARE TO BUYSLN                            
         BL    PB2                 LOW - GET NEXT SPOT IN SPOD                  
         BE    *+6                                                              
         DC    H'0'                SPOD SHOULD ALWAYS ADD UP !                  
* ADD 1 TO SPOT TOTALS FOR EACH COMPLETED SPOD                                  
         CLI   ELEMNUM,0           TEST ANY SPOT PRINTED THIS SPOD              
         BE    PB22                NO                                           
         LH    R0,BUYSPOTS                                                      
         AHI   R0,1                                                             
         STH   R0,BUYSPOTS                                                      
*                                                                               
         LH    RE,WEEKNUM          GET WEEK NUMBER                              
         AR    RE,RE               X 2                                          
         LH    R0,TOTSPOTS(RE)                                                  
         AHI   R0,1                BUMP WEEK COUNTER                            
         STH   R0,TOTSPOTS(RE)                                                  
* ADVANCE TO NEXT SET OF PRINT LINES                                            
PB22     XC    SPODSEC,SPODSEC                                                  
         L     R4,MYPRTADR                                                      
         AHI   R4,MYPRTL                                                        
         AH    R4,COLDISP          SET PRINT POSITIONI                          
         ST    R4,MYPRTADR                                                      
         B     PB2                                                              
         EJECT                                                                  
*===============================================================                
* NOW PRINT THE FORMATTED PRINT LINES                                           
*===============================================================                
                                                                                
PB30     LH    R0,BUYSPOTS                                                      
         L     R4,AMYPRT1                                                       
         AHI   R4,PDATES-P         GET DSPL OF DATES INTO PRT LINE              
         AH    R4,TOTDISP          ADD DSPL OF TOTALS                           
         EDIT  (R0),(5,(R4))                                                    
         LTR   R0,R0                                                            
         BNP   *+8                                                              
         MVI   5(R4),C'*'                                                       
*                                                                               
         XC    MYCOUNTS,MYCOUNTS   PRINT LINES PER SET COUNTERS                 
         LHI   R0,8                NUMBER OF PRINT LINE SETS                    
         LA    R1,MYCOUNTS                                                      
         L     R4,AMYPRT1                                                       
*                                                                               
PB32     BAS   RE,PRCOUNT                                                       
         LA    R1,2(R1)            NEXT COUNTER                                 
         AHI   R4,MYPRTL           NEXT PRINT LINE SET                          
         BCT   R0,PB32                                                          
*                                                                               
         OC    MYCOUNTS,MYCOUNTS   TEST ANYTHING TO PRINT                       
         JZ    EXIT                                                             
*                                                                               
         MVI   NEEDCONT,C'N'       SUPPRESS 'CONTINUED'                         
*                                                                               
         L     R4,AMYPRT1          FIRST SET OF PRINT LINES                     
         LA    R5,MYCOUNTS                                                      
*                                                                               
PR40     LA    R1,P                                                             
         LR    RE,R4                                                            
         LH    RF,0(R5)                                                         
*                                                                               
PR42     MVC   0(132,R1),0(RE)                                                  
         LA    R1,132(R1)                                                       
         LA    RE,132(RE)                                                       
         BCT   RF,PR42                                                          
         MVI   0(R1),0             FORCE A SPACE AFTER LAST                     
         GOTO1 REPORT                                                           
*                                                                               
         MVI   NEEDCONT,C'Y'       NOW NEED 'CONTINUED'                         
         AHI   R4,MYPRTL           NEXT SET OF PRINT LINES                      
         LA    R5,2(R5)            NEXT COUNTER                                 
         OC    0(2,R5),0(R5)                                                    
         BNZ   PR40                                                             
*                                                                               
         L     R4,AMYPRT1          CLEAR ALL PRINT LINES                        
PR44     MVC   0(132,R4),SPACES                                                 
         LA    R4,132(R4)                                                       
         C     R4,AMYPRTX                                                       
         BNH   PR44                                                             
         J     EXIT                                                             
*                                                                               
PRCOUNT  NTR1                                                                   
         LHI   R5,4                                                             
*                                                                               
PRCOUNT2 CLC   0(132,R4),SPACES                                                 
         BNH   PRCOUNTX                                                         
         LA    R4,132(R4)                                                       
         BCT   R5,PRCOUNT2                                                      
*                                                                               
PRCOUNTX LHI   RE,4                                                             
         SR    RE,R5               GIVES NUMBER OF PRINT LINES                  
         STH   RE,0(R1)                                                         
         J     EXIT                                                             
         EJECT                                                                  
*==============================================================                 
* FORMAT BUY DESCRIPTION DATA                                                   
*==============================================================                 
                                                                                
FMTBUY   NTR1                                                                   
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PESTLIN(3),DUB                                                   
         MVI   PESTLIN+3,C'-'                                                   
         SR    R0,R0                                                            
         IC    R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PESTLIN+4(3),DUB                                                 
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BDSEC                                                         
         EDIT  (R0),(3,PSLN)                                                    
*                                                                               
         GOTO1 CODAY,DMCB,(BDSEDAY,BDDAY),WORK  (REALLY DAYUNPK ??)             
         MVC   PDAYS,WORK                                                       
*&&DO                                                                           
         LA    R0,7                                                             
         LA    R1,PDAYS                                                         
         MVC   0(7,R1),=C'MTWTFSS'                                              
*                                                                               
         IC    RE,BDDAY                                                         
         SLL   RE,25               GET MONDAY BIT LEFT ALIGNED                  
*                                                                               
FMTB10   LTR   RE,RE               REG IS NEG IF DAY BIT ON                     
         BM    *+8                                                              
         MVI   0(R1),C'.'                                                       
*                                                                               
         LA    R1,1(R1)                                                         
         SLL   RE,1                                                             
         BCT   R0,FMTB10                                                        
*&&                                                                             
         GOTO1 UNTIME,DMCB,BDTIMST,PTIME                                        
*                                                                               
         MVC   HALF,ELCDLO         SAVE ELCODES                                 
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   PID,3(R6)                                                        
         MVC   ELCDLO(2),HALF      RESTORE ELCODES                              
*                                                                               
         MVC   PPROGRAM,BDPROGRM                                                
* TRANSLATE DAYPART CODE                                                        
         L     R1,ADDPTTAB                                                      
         SR    R0,R0                                                            
*                                                                               
FMTB20   CLC   0(1,R1),BDDAYPT     LOOK UP DAY-PART TA                          
         BE    FMTB22                                                           
         LA    R1,5(R1)                                                         
         AHI   R0,1                                                             
         CLI   0(R1),C'Z'          TEST EOL                                     
         BE    FMTB22                                                           
         CHI   R0,36                                                            
         BL    FMTB20                                                           
         LA    R1,=5C'Z'           DEFAULT                                      
                                                                                
FMTB22   MVC   PDPT(3),2(R1)       3 CHAR DAYPART                               
*                                                                               
         L     R4,AMYPRT1          POINT TO SAVED PRINT LINE AREA               
         MVC   0(132,R4),P                                                      
         MVC   132(132,R4),P2      AND SAVE THE PRINT LINES                     
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         J     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
GETCOL   NTR1                                                                   
         SR    R0,R0               WEEK NUMBER COUNTER                          
         L     RE,MEDAFRST         GET A(WEEK 1)                                
         LA    RF,PDATES-P         SET DSPL FOR FIRST WEEK                      
*                                                                               
GETCOL2  CLC   2(2,R6),0(RE)                                                    
         BL    GETCOL4                                                          
         CLC   2(2,R6),2(RE)                                                    
         BNH   GETCOLX                                                          
GETCOL4  LA    RE,12(RE)                                                        
         AHI   RF,6                NEXT COL DSPL                                
         BCTR  R0,0                                                             
         C     RE,MEDALAST                                                      
         BNH   GETCOL2                                                          
         DC    H'0'                                                             
*                                                                               
GETCOLX  LPR   R0,R0                                                            
         STH   R0,WEEKNUM                                                       
         STH   RF,COLDISP                                                       
         J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*=====================================================================*         
* MARKET LAST                                                                   
*=====================================================================*         
                                                                                
MKTL     MVI   FORCEHED,C'Y'                                                    
         J     EXIT                                                             
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
         DROP  RB,RC                                                            
         DS    0D                                                               
         USING *,RF                                                             
MYHDHK   NTR1                                                                   
         LM    R9,RC,HDHKR9                                                     
         DROP  RF                                                               
         USING SPJ702,RB                                                        
         USING SPJ7WORK,RC                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         MVC   HDATES,MYDATES                                                   
         MVC   HDATES+132(90),MYDATES+90                                        
* CENTER MARKET NAME                                                            
         OC    H6+43(44),SPACES    FILL MARKET WITH SPACES                      
         GOTO1 CENTER,DMCB,H6+43,44                                             
*                                                                               
         XIT1                                                                   
*                                                                               
HDHKR9   DS    A                                                                
HDHKRA   DS    A                                                                
HDHKRB   DS    A                                                                
HDHKRC   DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* MIDHOOK ROUTINES                                                              
*==============================================================                 
         SPACE 1                                                                
         DROP  RB,RC                                                            
         DS    0D                                                               
         USING *,RF                                                             
MYMIDHK  NTR1                                                                   
         LM    R9,RC,MIDHKR9                                                    
         DROP  RF                                                               
         USING SPJ702,RB                                                        
         USING SPJ7WORK,RC                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         MVC   MID1(7),=C'STATION'                                              
         MVC   MID1+8(8),BIGSTA                                                 
*                                                                               
         CLI   NEEDCONT,C'Y'       TEST NEED TO PRINT CONTINUED                 
         BNE   MYMIDHK2                                                         
*                                                                               
         L     RE,AMYPRT1                                                       
         MVC   MID1+17(7),0(RE)                                                 
         MVC   MID1+25(11),=C'(CONTINUED)'                                      
*                                                                               
MYMIDHK2 GOTO1 SQUASHER,DMCB,MID1,36                                            
*                                                                               
MYMIDHKX XIT1                                                                   
*                                                                               
MIDHKR9  DS    A                                                                
MIDHKRA  DS    A                                                                
MIDHKRB  DS    A                                                                
MIDHKRC  DS    A                                                                
MIDNEXT  DS    X                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'SPJ7WORK'                                                    
SPJ7WORK DS    0D                                                               
ELCDLO   DC    X'00'                                                            
ELCDHI   DC    X'00'                                                            
NEEDCONT DS    C                                                                
ELEMNUM  DS    C                                                                
ELEMDATE DS    H                                                                
SPODSEC  DS    H                                                                
TOTDISP  DS    H                   DSPL OF TOTALS IN DATE FIELD                 
COLDISP  DS    H                                                                
WEEKNUM  DS    H                                                                
MYPRTADR DS    A                   CURRENT PRINT LINE SET                       
MYDATES  DS    2CL90                                                            
         DS    0D                                                               
MYCOUNTS DS    XL16                8 2-BYTE COUNTERS                            
         DC    2X'00'              EOL TERMINATOR                               
*                                                                               
         DS    0D                                                               
TOTSPOTS DS    XL32                14 2-BYTE ACCUMS                             
BUYSPOTS DS    XL2                                                              
*                                                                               
AMYPRT1  DC    A(MYPRT1)                                                        
AMYPRT2  DC    A(MYPRT2)                                                        
AMYPRT3  DC    A(MYPRT3)                                                        
AMYPRT4  DC    A(MYPRT4)                                                        
AMYPRT5  DC    A(MYPRT5)                                                        
AMYPRT6  DC    A(MYPRT6)                                                        
AMYPRT7  DC    A(MYPRT7)                                                        
AMYPRT8  DC    A(MYPRT8)                                                        
AMYPRTX  DC    A(MYPRTX)                                                        
*                                                                               
MYPRT1   DS    4XL132                                                           
MYPRT2   DS    4XL132                                                           
MYPRT3   DS    4XL132                                                           
MYPRT4   DS    4XL132                                                           
MYPRT5   DS    4XL132                                                           
MYPRT6   DS    4XL132                                                           
MYPRT7   DS    4XL132                                                           
MYPRT8   DS    4XL132                                                           
MYPRTX   EQU   *-1                                                              
*                                                                               
MYPRTL   EQU   MYPRT2-MYPRT1                                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPREPPTBUF                                                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
SPWORKD  DSECT                                                                  
         ORG   P                                                                
PESTLIN  DS    CL7                                                              
         DS    CL2                                                              
PSLN     DS    CL3                                                              
         DS    CL2                                                              
PDAYS    DS    CL7                                                              
         DS    CL1                                                              
PTIME    DS    CL11                                                             
         DS    CL8                                                              
PDATES   DS    CL90                                                             
         DS    CL1                                                              
         ORG   P2                                                               
PID      DS    CL12                CONTRACT                                     
         DS    CL1                                                              
PDPT     DS    CL3                                                              
         DS    CL1                                                              
PPROGRAM DS    CL17                                                             
         DS    CL1                                                              
*                                                                               
         ORG   H9+41                                                            
HDATES   DS    CL90                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPREPJ702 02/13/03'                                      
         END                                                                    
