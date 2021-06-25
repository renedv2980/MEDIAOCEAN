*          DATA SET SPBUY23    AT LEVEL 033 AS OF 11/02/16                      
*PHASE T21123B                                                                  
                                                                                
*================================================================               
* THIS PROGRAM IS CALLED FROM SPBUY00 WHEN SVDSPMOD NEQ 0                       
* IT DISPLAYS A MULTILINE SCHEDULE WITH NPW, DEMOS, OR CPPS                     
*================================================================               
T21123   TITLE 'SPBUY23 - SPOTPAK BUY - MAKEGOOD SCHEDULE ANALYSIS'             
T21123   CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,T21123,R9,RR=R7                                                
*                                                                               
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
*                                                                               
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
*                                                                               
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         CLI   SVSCR,X'F8'         TEST SKEVAL SCREEN LOADED                    
         BE    SE00                                                             
*                                                                               
         MVC   ELEM(1),SKEINP1H+5  SAVE INPUT LINE LENGTH                       
         MVC   ELEM+1(78),SKEINP1  SAVE DATA ON INPUT LINE                      
*                                                                               
         XC    DMCB,DMCB           LOAD SKEVAL SCREEN                           
         MVC   DMCB+4(4),=X'D90211F8'                                           
         GOTO1 VCALLOV,DMCB,BUYHL1H                                             
         MVI   SVSCR,X'F8'                                                      
*                                                                               
         BRAS  RE,GETHL1A                                                       
         MVC   SKEHL1A(40),0(R1)                                                
*                                                                               
         BRAS  RE,GETPFK1                                                       
         MVC   SKEPFK1(34),0(R1)                                                
*                                                                               
         BRAS  RE,GETPFK2                                                       
         MVC   SKEPFK2(30),0(R1)                                                
*                                                                               
         LA    R2,SKEINP1H                                                      
         OI    4(R2),X'20'         SET PREV VALIDATED                           
         MVC   5(1,R2),ELEM        RESTORE INPUT LENGTH                         
         MVC   8(78,R2),ELEM+1     RESTORE DATA                                 
         B     SE00                                                             
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
* CLEAR ALL DISPLAY LINES AND RESET TO NORMAL INTENSITY                         
*================================================================               
                                                                                
SE00     OC    BLDROW,BLDROW       TEST FIRST TIME                              
         BNZ   SE20                                                             
         LA    R2,SKELIN1H                                                      
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
         EJECT                                                                  
*============================================================                   
* SET SKED PERIOD START/END DATES (MAX 14 WEEKS)                                
*============================================================                   
                                                                                
SE10     GOTO1 VDATCON,DMCB,(2,SVSKPER),WORK                                    
         LA    R5,14                                                            
*                                                                               
SE12     GOTO1 VADDAY,DMCB,WORK,WORK+6,F'7'                                     
         CLC   WORK+6(6),SVEND     COMPARE TO EST END                           
         BH    SE14                                                             
         MVC   WORK(6),WORK+6                                                   
         BCT   R5,SE12                                                          
         B     SE16                                                             
*                                                                               
SE14     MVC   WORK(6),WORK+6                                                   
*                                                                               
SE16     GOTO1 (RF),(R1),WORK,WORK+6,F'-1' BACK UP TO GET LAST DAY              
         GOTO1 VDATCON,DMCB,WORK+6,(2,SVSKPER+2)                                
*                                                                               
         BRAS  RE,SEHL             BUILD HEADLINES (AND SVMVDTS)                
         EJECT                                                                  
*=========================================================                      
* DISPLAY THE CURRENT BUY RECORD                                                
*=========================================================                      
         SPACE 1                                                                
SE20     LH    RE,BLDROW                                                        
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         LHI   RE,12               SET ROW = 13-1                               
         AHI   RE,1                                                             
         STH   RE,BLDROW                                                        
*                                                                               
         AHI   RE,-13              ROW 13 IS FIRST                              
         MHI   RE,LLEN             LENGTH OF ROW + HEADER                       
         LA    R2,SKELIN1H(RE)     POINT TO DISPLAY ROW                         
         LA    R0,SKEPFK1H                                                      
         CR    R2,R0                                                            
         BL    SE22                                                             
         MVI   ERRCD,X'FE'         TELL CALLER NO MORE ROOM                     
         BRAS  RE,MOREMSG                                                       
         MVC   BUYMSG(40),0(R1)                                                 
         J     EXIT                                                             
*                                                                               
         USING LINED,R2                                                         
*                                                                               
SE22     BRAS  RE,DSPBUY           DISPLAY BUY DETAILS                          
*                                                                               
         CLI   SVDSPMOD,C'C'       TEST CPP  DISPLAY                            
         BE    SE30                                                             
         CLI   SVDSPMOD,C'D'       OR DEMO DISPLAY                              
         BE    SE30                                                             
*                                                                               
         BRAS  RE,GETSKED                                                       
         BRAS  RE,DSPSKED                                                       
         B     SE40                                                             
*                                                                               
SE30     BRAS  RE,DSPDEM                                                        
*                                                                               
SE40     OI    LHDRH+6,X'80'       SET TRANSMIT                                 
         J     EXIT                                                             
*                                                                               
SE42     BRAS  RE,NOMORMSG                                                      
         MVC   BUYMSG(40),0(R1)                                                 
         J     EXIT                                                             
         EJECT                                                                  
*=========================================================                      
* EXTRACT SCHEDULE DATA FOR A BUY                                               
*=========================================================                      
                                                                                
GETSKED  NTR1  BASE=*,LABEL=*                                                   
         XC    WORK2,WORK2                                                      
*                                                                               
         LA    R1,SVMVDTS                                                       
         LA    R4,WORK2                                                         
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
         JZ    EXIT                                                             
         LA    R6,BDELEM                                                        
         SR    R7,R7                                                            
         B     GETSK2                                                           
         EJECT                                                                  
         USING LINED,R2                                                         
*                                                                               
DSPBUY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,BUYKEY+10                                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LLIN(3),DUB                                                      
*                                                                               
         MVI   LLIN+3,C'+'         INDICATOR  FOR MAKEGOOD                      
         OC    BDMGDATE,BDMGDATE   TEST LINE IS A MAKEGOOD                      
         BNZ   DSB2                                                             
         MVI   LLIN+3,C' '                                                      
*                                                                               
DSB2     LA    R0,7                                                             
         LA    R1,LDAYS                                                         
         MVC   0(7,R1),=C'MTWTFSS'                                              
*                                                                               
         IC    RE,BDDAY                                                         
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
         GOTO1 (RF),(R1),BDTIMST,LTIME                                          
*                                                                               
         MVC   LDPTLEN(1),BDDAYPT                                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BDSEC                                                         
         EDIT  (R0),(3,LDPTLEN+1),ALIGN=LEFT                                    
*                                                                               
         MVC   LPROG,BDPROGRM                                                   
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*========================================================                       
* DISPLAY SPOTS IN WORK2                                                        
*========================================================                       
                                                                                
DSPSKED  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,LSKED                                                         
         LA    R5,WORK2                                                         
         LHI   R6,14                                                            
*                                                                               
DSK2     SR    R0,R0                                                            
         ICM   R0,1,0(R5)          GET SPOT COUNT                               
         BZ    DSK30                                                            
         EDIT  (R0),(2,1(R4))                                                   
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
         LA    R5,SVDEMLST                                                      
         CLI   SVPOLPRD,0                                                       
         BE    *+8                                                              
         LA    R5,SVBRDEMS                                                      
         LHI   RF,6                                                             
*                                                                               
DDM10    BAS   RE,PULLDEM          GET DEMO VALUE INTO FULL(4)                  
         BNE   DDM30                                                            
*                                                                               
DDM20    CLI   SVDSPMOD,C'C'       TEST CPP                                     
         BE    DDM22               YES                                          
         SR    R0,R0                                                            
         ICM   R0,15,FULL                                                       
         BZ    DDM30                                                            
         EDIT  (R0),(5,(R4)),1                                                  
         B     DDM30                                                            
*                                                                               
DDM22    SR    R1,R1               DISPLAY CPP                                  
         ICM   R1,7,BDCOST         GET DOLLARS (IN PENNIES)                     
         M     R0,=F'20'           X 2 X10                                      
         ICM   RE,15,FULL          POINTS TO 1 DECIMAL                          
         BZ    DDM30                                                            
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LR    R0,R1                                                            
         CHI   R0,9999             NEED FOUR DIGITS ?                           
         BH    DDM24                                                            
         EDIT  (R0),(5,1(R4)),2                                                 
         B     DDM30                                                            
*                                                                               
DDM24    AR    R0,R0               R0 X 2                                       
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LR    R0,R1                                                            
         EDIT  (R0),(5,1(R4)),0,FLOAT=$                                         
*                                                                               
DDM30    LA    R4,7(R4)                                                         
         LA    R5,3(R5)                                                         
         BCT   RF,DDM10                                                         
         J     EXIT                                                             
         LTORG                                                                  
         DROP  R2                                                               
                                                                                
*===========================================================                    
* ON ENTRY R5 POINTS TO DEMO CODE TO BE EXTRACTED                               
*===========================================================                    
                                                                                
PULLDEM  NTR1                                                                   
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         IC    R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BNP   PULLDEMX                                                         
         SRL   R0,3                LOOP ON NUMBER OF DEMOS                      
         LA    R6,24(R6)           POINT TO FIRST DEMO                          
*                                                                               
PULLDEM2 CLC   0(3,R5),0(R6)                                                    
         BE    PULLDEM4                                                         
         LA    R6,8(R6)                                                         
         BCT   R0,PULLDEM2                                                      
         B     PULLDEMX                                                         
*                                                                               
PULLDEM4 MVC   FULL,4(R6)                                                       
         NI    FULL,X'7F'          DROP OVRD FLAG                               
*                                                                               
PULLDEMX XIT1                                                                   
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
         CLI   SVDSPMOD,C'D'       TEST DEMO DISPLAY                            
         BE    SEHL50                                                           
         CLI   SVDSPMOD,C'C'       TEST CPP DISPLAY                             
         BE    SEHL50                                                           
                                                                                
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
*                                                                               
SEHL50   BAS   RE,GETDEMNS                                                      
         LA    R1,SKEHL2B                                                       
         LA    R0,6                                                             
         LA    RE,ELEM                                                          
*                                                                               
SEHL52   MVC   0(6,R1),0(RE)                                                    
         LA    R1,7(R1)                                                         
         LA    RE,6(RE)                                                         
         BCT   R0,SEHL52                                                        
         J     EXIT                                                             
         EJECT                                                                  
*================================================================               
* GET DEMO NAMES INTO ELEM                                                      
*================================================================               
         SPACE 1                                                                
GETDEMNS NTR1                                                                   
*                                                                               
         XC    ELEM,ELEM           CLEAR OUTPUT AREA                            
         LA    R4,SVDEMOS                                                       
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R4,SVBRDEMS                                                      
*                                                                               
         L     RE,ADBLOCK                                                       
         USING DBLOCKD,RE                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE(3),=C'TP '                                                
*                                                                               
         MVI   DBSELMED,C'T'                                                    
         CLI   BUYMD,C'R'                                                       
         BNE   *+12                                                             
         MVI   DBSELMED,C'R'                                                    
         B     GDMN2                                                            
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   GDMN2                                                            
         CLI   SVCXTRA,C'U'        TEST US DEMOS                                
         BE    GDMN2                                                            
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
*                                                                               
GDMN2    XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000AE0'  DEMOCON                                  
         GOTO1 VCALLOV,DMCB                                                     
*                                                                               
         L     RF,DMCB                                                          
         LHI   RE,SVNTDMS-BUYSAVE  NON-TRAD INDEX DEMO LIST                     
         AR    RE,RA                                                            
         STCM  RE,15,DMCB+16                                                    
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(14,(R4)),(6,ELEM),(C'S',ADBLOCK),SVUSRNMS             
         J     EXIT                                                             
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
         LHI   RF,WSGLTAB-SPBUYWKD POINT TO GOAL DATE TABLE                     
         AR    RF,RC                                                            
         SR    RE,RE               CLEAR COUNTER                                
         LR    R4,RF                                                            
         AHI   R4,WSGLDPTS-WSGLTABD  POINT TO TABLE OF WEEKLY DATA              
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
       ++INCLUDE SPBUY23MSG                                                     
         PRINT OFF                                                              
       ++INCLUDE SPBUYWORK                                                      
SPBUYWKD DSECT                                                                  
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
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDCOREQUS                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033SPBUY23   11/02/16'                                      
         END                                                                    
