*          DATA SET SPCSO0C    AT LEVEL 093 AS OF 05/01/02                      
*PHASE T2180CA                                                                  
         TITLE 'T2180C - CHILD SPOT 6030 TRANSFER'                              
T2180C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 53*TRDTABL+1,T2180C                                              
         LR    R6,RC                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R6,ATRDTAB                                                       
*                                                                               
         LA    R7,TRDTABL          CLEAR TRADE SPOTS TABLE                      
         MH    R7,=H'53'                                                        
         SR    R1,R1                                                            
         MVCL  R6,R0                                                            
*                                                                               
         CLI   MYOVNUM,X'0C'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    REQMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'0C'       STORE OVERLAY NUMBER                         
*                                                                               
         CLI   MODE,VALKEY         DIE IF NOT VALKEY                            
         BE    MAIN                                                             
         DC    H'0'                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* MAIN ROUTINE                                                                  
*                                                                               
MAIN     BAS   RE,VALIKEY          VALIDATE KEY                                 
*                                                                               
         BAS   RE,BLDTAB60         BUILD 60 SECOND PROGRAMS TABLE               
*                                                                               
         BAS   RE,BLDTAB30         BUILD 30 SECOND PROGRAMS TABLE               
*                                                                               
         BAS   RE,TRANSFER         TRANSFER 30S TO 60S                          
*                                                                               
         BAS   RE,SAVETRD          SAVE TRADE SPOTS BACK INTO RECORDS           
*                                                                               
         BAS   RE,BLDACCS          TOTAL TRADE SPOTS INTO ACCTAB                
*                                                                               
         GOTO1 CLEARF,DMCB,(1,REQL1H),REQLAST                                   
*                                  DISPLAY 30 AND 60 SPOT TOTALS                
         MVC   CALHEADS,=C':30S:60S'                                            
         CLI   CONREC,C'1'                                                      
         BNE   *+10                                                             
         MVC   CALHEADS,=C':30S:15S'                                            
         LA    R2,REQL1H                                                        
         ST    R2,CALSPTR                                                       
         GOTO1 BLDCAL                                                           
*                                                                               
         LA    R5,ACCTAB           COMPUTE YEARLY TOTALS                        
         USING ACCTABD,R5                                                       
         GOTO1 TOTFULL,DMCB,ACCONE,ACCTABL                                      
         MVC   TOTAL30,0(R1)                                                    
         GOTO1 TOTFULL,DMCB,ACCTWO,ACCTABL                                      
         MVC   TOTAL60,0(R1)                                                    
         DROP  R5                                                               
*                                                                               
         LA    R2,REQTOTS+15       POINT R2 TO TOTALS FIELD                     
         MVC   0(12,R2),=C'TOTAL SPOTS '                                        
         LA    R2,12(R2)                                                        
*                                  DISPLAY TOTALS                               
         EDIT  (4,TOTAL30),(5,0(R2)),ZERO=NOBLANK                               
         LA    R2,7(R2)                                                         
         EDIT  (4,TOTAL60),(5,0(R2)),ZERO=NOBLANK                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VALIKEY  NTR1                                                                   
         LA    R2,REQMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    REQCLTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,REQCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKSTA                                                            
         NI    REQSTAH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKSTA    LA    R2,REQSTAH          VALIDATE STATION FIELD                       
         TM    4(R2),X'20'                                                      
         BO    VKCSH3                                                           
         GOTO1 ANY                                                              
         GOTO1 VALISTA                                                          
         MVC   MYSTA,BSTA          SAVE STATION CODE AND CLEAR TO AVOID         
         XC    BSTA,BSTA               APPLYING STATION PERCENTAGES             
         OI    4(R2),X'20'                                                      
*                                                                               
VKCSH3   LA    R2,REQCSH3H         VALIDATE 30 SEC CASH EST FIELD               
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         MVC   START30,QSTART                                                   
         GOTO1 CLRACC              BUILD 30 SEC GOALS IN ACCONE                 
         MVC   ACCNUM,=F'0'                                                     
         XC    BSTA,BSTA           GET GOALS FOR WHOLE MARKET                   
         GOTO1 CMPGOL                                                           
         OI    4(R2),X'20'                                                      
*                                                                               
VKCSH6   LA    R2,REQCSH6H         VALIDATE 60 SEC CASH EST FIELD               
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         CLC   QSTART,START30      START DATE MUST BE SAME AS FIRST EST         
         BNE   ERRMAT                                                           
         XC    BSTA,BSTA                                                        
         MVC   ACCNUM,=F'1'        BUILD 60 SEC GOALS IN ACCTWO                 
         XC    BSTA,BSTA           GET GOALS FOR WHOLE MARKET                   
         GOTO1 CMPGOL                                                           
         OI    4(R2),X'20'                                                      
*                                                                               
VKEST3   LA    R2,REQEST3H         VALIDATE 30 SEC ESTIMATE FIELD               
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         CLC   QSTART,START30      START DATE MUST BE SAME FIRST EST            
         BNE   ERRMAT                                                           
         MVC   BMEST30,BMEST                                                    
         OI    4(R2),X'20'                                                      
*                                                                               
VKEST6   LA    R2,REQEST6H         VALIDATE 60 SEC ESTIMATE FIELD               
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         CLC   QSTART,START30      START DATE MUST BE SAME AS FIRST EST         
         BNE   ERRMAT                                                           
         MVC   BMEST60,BMEST                                                    
         OI    4(R2),X'20'                                                      
*                                                                               
VKPER    LA    R5,ACCTAB           COMPUTE PERCENTAGE OF GOAL FOR 60S           
         USING ACCTABD,R5                                                       
         GOTO1 TOTFULL,DMCB,ACCONE,ACCTABL     TOTAL 30S                        
         MVC   TOTAL30,0(R1)                                                    
         GOTO1 TOTFULL,DMCB,ACCTWO,ACCTABL     TOTAL 60S                        
         MVC   TOTAL60,0(R1)                                                    
*                                                                               
         L     RF,TOTAL60          GROSS % = (10000 * TOTAL 60S) /              
         M     RE,=F'10000'                  (TOTAL 30S + TOTAL 60S)            
         L     R0,TOTAL30                                                       
         A     R0,TOTAL60                                                       
         LTR   R0,R0                                                            
         BZ    ERRZERO                                                          
         DR    RE,R0                                                            
         ST    RF,GROSSPER                                                      
*                                                                               
         XC    GOALFLGS,GOALFLGS   SET FLAGS FOR WEEKS THAT HAVE NON-           
         LA    R3,GOALFLGS             ZERO 60 SECOND GOALS                     
         LA    R5,ACCTAB           POINT TO GOAL ACCUMULATORS                   
         L     R0,YRWEEKS                                                       
*                                                                               
VKGOAL   OC    ACCTWO,ACCTWO       IF 60 SECOND GOAL IS NON-ZERO                
         BZ    *+8                                                              
         MVI   0(R3),1             THEN SET FLAG                                
*                                                                               
         LA    R3,1(R3)            BUMP TO NEXT FLAG                            
*                                                                               
         LA    R5,ACCTABL(R5)      BUMP TO NEXT ACCUMULATOR                     
         BCT   R0,VKGOAL           REPEAT FOR EACH WEEK IN THE ESTIMATE         
         DROP  R5                                                               
*                                                                               
         LA    R6,KEY              BUILD KEY                                    
         USING CSOKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   CSOKTYPE,CSOKTYPQ   CSO RECORD TYPE                              
         MVI   CSOKSTYP,CSOKSTPQ   CSO RECORD SUB-TYPE                          
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
         MVC   CSOKMKT,BMKT                                                     
         MVC   CSOKSTA,MYSTA                                                    
         DROP  R6                                                               
*                                                                               
         MVC   MYKEY,KEY           SAVE IN MYKEY                                
         B     XIT                                                              
         EJECT                                                                  
* BUILD TABLE OF 60 SECOND ESTIMATE PROGRAMS.                                   
*                                                                               
BLDTAB60 NTR1                                                                   
         LA    R4,KEY              POINT R4 TO KEY                              
         USING CSOKEY,R4                                                        
         MVC   KEY,MYKEY           BUILD KEY                                    
         MVC   CSOKEST,BMEST60                                                  
         DROP  R4                                                               
*                                                                               
         LA    R5,TAB60            POINT R5 TO 60 SECOND TABLE                  
         USING TAB60D,R5                                                        
*                                                                               
         MVI   RDUPDATE,C'N'       READ FOR FIRST ONE                           
         GOTO1 HIGH                                                             
*                                                                               
BS10     CLC   KEY(11),KEYSAVE     WHILE NOT END OF PROGRAMS                    
         BNE   BS50                                                             
*                                                                               
         MVC   T60DISKA,KEY+14     SAVE DISK ADDRESS                            
*                                                                               
         MVI   RDUPDATE,C'N'       READ RECORD                                  
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              POINT TO DESCRIPTION ELEMENT                 
         MVI   ELCODE,DSCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSODSCEL,R6                                                      
*                                                                               
         MVC   T60DAY,DSCDAY       SAVE DAYS AND TIMES                          
         MVC   T60TIME,DSCTIME                                                  
*                                                                               
         LA    R5,TAB60L(R5)       BUMP TO NEXT 60 TABLE ENTRY                  
*                                                                               
         MVI   RDUPDATE,C'N'       READ FOR NEXT KEY                            
         GOTO1 SEQ                                                              
         B     BS10                                                             
*                                                                               
BS50     MVI   0(R5),X'FF'         MARK END OF TABLE                            
         B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
* BUILD TABLE OF 30 SECOND ESTIMATE PROGRAMS AND THEIR MATCHING 60              
* SECOND ESTIMATE PROGRAMS.                                                     
* SAVE 30 SECOND TRADE SPOTS AND COSTS FOR 30 SEC AND 60 SEC                    
* ROUTINE WILL ALSO CALCULATE THE PERCENTAGE TO TRANSFER FROM 30 SECOND         
* PROGRAMS TO 60 SECOND PROGRAMS.                                               
*                                                                               
BLDTAB30 NTR1                                                                   
         XC    GROSSDOL,GROSSDOL   CLEAR TOTAL NTP$ FOR ALL WEEKS               
         XC    SUBDOL,SUBDOL       CLEAR TOTAL NTP$ FOR WEEKS WITH NON-         
*                                      ZERO 60 SECOND GOALS                     
*                                                                               
         LA    R6,KEY              POINT R4 TO KEY                              
         USING CSOKEY,R6                                                        
         MVC   KEY,MYKEY           BUILD KEY                                    
         MVC   CSOKEST,BMEST30                                                  
         DROP  R6                                                               
*                                                                               
         SR    R2,R2               CLEAR PROGRAM NUMBER                         
*                                                                               
         LA    R3,TAB30            POINT R3 TO 30 SECOND TABLE                  
         USING TAB30D,R3                                                        
*                                                                               
         MVI   RDUPDATE,C'N'       READ FOR FIRST ONE                           
         GOTO1 HIGH                                                             
*                                                                               
BT10     CLC   KEY(11),KEYSAVE     WHILE NOT END OF PROGRAMS                    
         BNE   BT100                                                            
*                                                                               
         MVC   T30DISKA,KEY+14     SAVE DISK ADDRESS                            
*                                                                               
         MVI   RDUPDATE,C'N'       READ RECORD                                  
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              POINT TO DESCRIPTION ELEMENT                 
         MVI   ELCODE,DSCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSODSCEL,R6                                                      
*                                                                               
         LA    R4,TAB60            FIND 60 SECOND PROGRAM THAT MATCHES          
         USING TAB60D,R4                                                        
*                                                                               
BT20     CLI   0(R4),X'FF'         IF END OF TABLE THEN ERROR                   
         BE    ERRMAT                                                           
*                                                                               
         CLC   T60DAY,DSCDAY       TEST MATCH ON DAYS AND TIMES                 
         BNE   BT30                                                             
         CLC   T60TIME,DSCTIME                                                  
         BE    BT40                                                             
*                                                                               
BT30     LA    R4,TAB60L(R4)       BUMP TO NEXT 60 TABLE ENTRY                  
         B     BT20                                                             
*                                                                               
BT40     MVC   T30DISK6,T60DISKA   SAVE DISK ADDRESS OF MATCHING 60 REC         
*                                                                               
         L     R6,AIO              READ WEEKLY ELEMENTS AND ACCUMULATE          
         MVI   ELCODE,WKCODEQ        1) TOTAL NTP$ FOR ALL WEEKS                
         BAS   RE,GETEL              2) TOTAL NTP$ FOR WEEKS WITH NON-          
         BE    *+6                          ZERO 60 SEC GOALS                   
         DC    H'0'                  3) SAVE TRADE SPOTS/COSTS                  
         USING CSOWKEL,R6                                                       
*                                                                               
         LA    R4,GOALFLGS         POINT R4 TO GOAL FLAGS                       
         L     R5,ATRDTAB          POINT R5 TO TRADE SPOTS TABLE                
         USING TRDTABD,R5                                                       
*                                                                               
BT50     SR    RF,RF               CALULATE NTP$ FOR THIS WEEK                  
         ICM   RF,3,WKTSPOTS                                                    
         ICM   R0,15,WKCOST                                                     
         MR    RE,R0                                                            
*                                                                               
         L     R1,GROSSDOL         ADD NTP$ TO GROSS$                           
         AR    R1,RF                                                            
         ST    R1,GROSSDOL                                                      
*                                                                               
         CLI   0(R4),1             IF THERE ARE 60 SECOND GOAL$                 
         BNE   BT60                                                             
         L     R1,SUBDOL           THEN ADD NTP$ TO SUB$                        
         AR    R1,RF                                                            
         ST    R1,SUBDOL                                                        
*                                                                               
BT60     LA    RF,TRDSPOT3(R2)     MOVE IN TRADE SPOTS FOR THIS WEEK            
         MVC   0(1,RF),WKTSPOTS+1                                               
*                                                                               
         LR    RF,R2               MOVE IN COST FOR THIS WEEK                   
         SLL   RF,1                                                             
         LA    RF,TRDCOST(RF)                                                   
         MVC   0(2,RF),WKCOST+2                                                 
*                                                                               
         LA    R4,1(R4)            BUMP R4 TO NEXT GOAL FLAG                    
         LA    R5,TRDTABL(R5)      BUMP R5 TO NEXT WEEK                         
         DROP  R5                                                               
*                                                                               
         BAS   RE,NEXTEL           BUMP R6 TO NEXT WEEKLY ELEMENT               
         BE    BT50                IF MORE LEFT THEN BRANCH BACK                
         DROP  R6                                                               
*                                                                               
         MVI   0(R5),X'FF'         MARK END OF TRADE SPOTS TABLE                
*                                                                               
         LA    R3,TAB30L(R3)       BUMP TO NEXT 30 TABLE ENTRY                  
*                                                                               
         LA    R2,1(R2)            INCREMENT PROGRAM COUNTER                    
*                                                                               
         MVI   RDUPDATE,C'N'       READ FOR NEXT KEY                            
         GOTO1 SEQ                                                              
         B     BT10                                                             
*                                                                               
BT100    XC    0(4,R3),0(R3)       MARK END OF TABLE                            
         ST    R2,NUMPROGS         SAVE NUMBER OF PROGRAMS                      
         DROP  R3                                                               
*                                                                               
         OC    SUBDOL,SUBDOL       IF NO DOLLARS FROM SUBSET WEEKS              
         BZ    ERRSUB                  THEN ERROR                               
*                                                                               
         L     RF,GROSSPER         CALCULATE FINAL PERCENTAGE =                 
         M     RE,GROSSDOL             GROSS % * GROSS$ / SUB$                  
         D     RE,SUBDOL                                                        
         ST    RF,PERCENT                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* TRANSFER SPOTS FOR ALL PROGRAMS IN THE 30 SECOND ESTIMATE TO THE              
* MATCHING PROGRAMS FOUND IN THE 60 SECOND ESTIMATE.                            
*                                                                               
TRANSFER NTR1                                                                   
         XC    LEFTOVER,LEFTOVER   CLEAR LEFTOVER DOLLARS                       
         XC    SUMTAB,SUMTAB                                                    
*                                                                               
         L     R3,ATRDTAB          POINT R3 TO TRADE SPOTS TABLE                
         LA    R4,GOALFLGS         POINT R4 TO GOAL FLAGS TABLE                 
         USING TRDTABD,R3                                                       
*                                                                               
TF10     CLI   0(R3),X'FF'         WHILE NOT END OF TRADE SPOTS TABLE           
         BE    TF100                                                            
*                                                                               
         CLI   0(R4),0             IF NO GOALS THIS WEEK THEN SKIP              
         BE    TF90                                                             
*                                                                               
         SR    R5,R5               CALC DOLLARS TO BE TRANSFERED IN R5          
         SR    R2,R2               CLEAR R2 THE PROGRAM NUMBER                  
*                                                                               
TF20     C     R2,NUMPROGS         WHILE R2 < NUMBER OF PROGRAMS                
         BNL   TF30                                                             
*                                                                               
         LA    RF,TRDSPOT3(R2)     CALCULATE COST OF 30 SEC TRADE SPOTS         
         ZIC   RE,0(RF)                FOR THIS PROGRAM THIS WEEK               
         LR    RF,R2                                                            
         SLL   RF,1                                                             
         LA    RF,TRDCOST(RF)                                                   
         MH    RE,0(RF)                                                         
*                                                                               
         AR    R5,RE               ACCUMULATE IN R5                             
         LA    R2,1(R2)                                                         
         B     TF20                END OF WHILE LOOP                            
*                                                                               
TF30     LR    RF,R5               APPLY PERCENTAGE TO R5 TO GET TOTAL          
         M     RE,PERCENT              DOLLARS TO TRANSFER FOR THIS             
         D     RE,=F'10000'            WEEK                                     
         LR    R5,RF                                                            
*                                                                               
         A     R5,LEFTOVER         ADD IN DOLLARS LEFT FROM LAST WEEK           
*                                                                               
TF40     MVI   CHOICE,X'FF'        INITIALIZE CHOICE TO NO CHOICE               
         MVC   LOWSPOTS,=F'99999'  LOWEST NUMBER OF SPOTS = INFINITY            
*                                                                               
         SR    R2,R2               CLEAR R2 THE PROGRAM NUMBER                  
*                                                                               
TF50     C     R2,NUMPROGS         WHILE R2 < NUMBER OF PROGRAMS                
         BNL   TF70                                                             
*                                                                               
         LA    RF,TRDSPOT3(R2)     IF THERE IS AT LEAST ONE 30 SEC              
         CLI   0(RF),0                 SPOT LEFT (TWO FOR 6030)                 
         BE    TF60                                                             
         CLI   CONREC,C'1'                                                      
         BE    *+12                                                             
         CLI   0(RF),1                                                          
         BE    TF60                                                             
*                                                                               
         LR    RF,R2               AND THE COST OF THE SPOT(S) < R5             
         SLL   RF,1                                                             
         LA    RF,TRDCOST(RF)                                                   
         SR    RE,RE                                                            
         ICM   RE,3,0(RF)                                                       
         CLI   CONREC,C'1'                                                      
         BE    *+8                                                              
         SLL   RE,1                                                             
         CR    RE,R5                                                            
         BH    TF60                                                             
*                                                                               
         LR    RF,R2               AND THE TOTAL NUMBER OF 60 SEC SPOTS         
         SLL   RF,2                    FOR THIS PROGRAM < LOWSPOTS              
         LA    RF,SUMTAB(RF)                                                    
         ICM   RE,15,0(RF)                                                      
         C     RE,LOWSPOTS                                                      
         BNL   TF60                                                             
*                                                                               
         ST    RE,LOWSPOTS         THEN LOWSPOTS = TOTAL FOR THIS PROG          
         STC   R2,CHOICE           SAVE THIS PROGRAM AS CHOSEN ONE              
*                                                                               
TF60     LA    R2,1(R2)            BUMP R2                                      
         B     TF50                END OF WHILE LOOP                            
*                                                                               
TF70     CLI   CHOICE,X'FF'        IF NO CHOICE MADE THEN DONE FOR              
         BE    TF80                    THIS WEEK                                
*                                                                               
         ZIC   R2,CHOICE           OTHERWISE SET R2 TO CHOSEN PROGRAM           
*                                                                               
         LA    RF,TRDSPOT3(R2)     SUBTRACT 30 SEC TRADE SPOT(S) FROM           
         ZIC   RE,0(RF)                THIS PROGRAM THIS WEEK                   
         SH    RE,=H'2'                                                         
         CLI   CONREC,C'1'                                                      
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,0(RF)                                                         
*                                                                               
         LA    RF,TRDSPOT6(R2)     ADD 60 SEC TRADE SPOT(S) FOR                 
         ZIC   RE,0(RF)                THIS PROGRAM THIS WEEK                   
         LA    RE,1(RE)                                                         
         CLI   CONREC,C'1'                                                      
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,0(RF)                                                         
*                                                                               
         LR    RF,R2               INCREMENT TOTAL 60 SEC TRADE SPOTS           
         SLL   RF,2                    FOR THIS PROGRAM FOR ALL WEEKS           
         LA    RF,SUMTAB(RF)                                                    
         ICM   RE,15,0(RF)                                                      
         LA    RE,1(RE)                                                         
         CLI   CONREC,C'1'                                                      
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         STCM  RE,15,0(RF)                                                      
*                                                                               
         LR    RF,R2               SUBTRACT THE COST OF THIS(THESE)             
         SLL   RF,1                    SPOT(S) FROM R5                          
         LA    RF,TRDCOST(RF)                                                   
         SH    R5,0(RF)                                                         
         CLI   CONREC,C'1'                                                      
         BE    *+8                                                              
         SH    R5,0(RF)                                                         
         B     TF40                END OF WHLE LOOP                             
*                                                                               
TF80     ST    R5,LEFTOVER         SAVE LEFTOVER DOLLARS                        
*                                                                               
TF90     LA    R3,TRDTABL(R3)      BUMP R3 TO NEXT WEEK                         
         LA    R4,1(R4)            BUMP R4 TO NEXT WEEK                         
         B     TF10                END OF WHILE LOOP                            
         DROP  R3                                                               
*                                                                               
TF100    DS    0H                                                               
*                                                                               
TFX      B     XIT                                                              
         EJECT                                                                  
* SAVE SPOTS FROM TRADE SPOTS TABLE INTO 30 AND 60 SEC PROGRAM RECORDS.         
*                                                                               
SAVETRD  NTR1                                                                   
         SR    R2,R2               CLEAR R2 THE PROGRAM NUMBER                  
         LA    R4,TAB30            POINT R4 TO 30 SECOND EST TABLE              
         USING TAB30D,R4                                                        
*                                                                               
ST10     C     R2,NUMPROGS         WHILE R2 < NUMBER OF PROGRAMS                
         BNL   ST100                                                            
*                                                                               
         MVC   KEY+14(4),T30DISKA  SAVE SPOTS IN 30 SECOND RECORD               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              POINT R6 TO FIRST WEEKLY ELEMENT             
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
*                                                                               
         L     R3,ATRDTAB          POINT R3 TO TRADE SPOTS TABLE                
         USING TRDTABD,R3                                                       
*                                                                               
ST20     CLI   0(R3),X'FF'         WHILE NOT END OF TRADE SPOTS TABLE           
         BE    ST30                                                             
*                                                                               
         LA    RF,TRDSPOT3(R2)     SAVE THIS WEEK'S SPOTS IN RECORD             
         MVC   WKTSPOTS+1(1),0(RF)                                              
*                                                                               
         LA    R3,TRDTABL(R3)      BUMP R3 TO NEXT WEEK                         
*                                                                               
         BAS   RE,NEXTEL           BUMP R6 TO NEXT WEEK                         
         B     ST20                END OF WHILE LOOP                            
*                                                                               
ST30     GOTO1 PUTREC              WRITE RECORD BACK                            
*                                                                               
         MVC   KEY+14(4),T30DISK6  SAVE SPOTS IN 60 SEC RECORD                  
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              POINT R6 TO FIRST WEEKLY ELEMENT             
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
*                                                                               
         L     R3,ATRDTAB          POINT R3 TO TRADE SPOTS TABLE                
         USING TRDTABD,R3                                                       
*                                                                               
ST40     CLI   0(R3),X'FF'         WHILE NOT END OF TRADE SPOTS TABLE           
         BE    ST50                                                             
*                                                                               
         LA    RF,TRDSPOT6(R2)     SAVE THIS WEEK'S SPOTS IN RECORD             
         MVC   WKTSPOTS+1(1),0(RF)                                              
*                                                                               
         LA    R3,TRDTABL(R3)      BUMP R3 TO NEXT WEEK                         
*                                                                               
         BAS   RE,NEXTEL           BUMP R6 TO NEXT WEEK                         
         B     ST40                END OF WHILE LOOP                            
         DROP  R3                                                               
*                                                                               
ST50     GOTO1 PUTREC              WRITE RECORD BACK                            
*                                                                               
         LA    R2,1(R2)            BUMP R2 TO NEXT PROGRAM                      
         LA    R4,TAB30L(R4)       BUMP R4 TO PAIR OF PROGRAMS                  
         B     ST10                                                             
*                                                                               
ST100    DS    0H                                                               
*                                                                               
STX      B     XIT                                                              
         EJECT                                                                  
* BUILD ACCUMULATORS WITH TOTAL SPOTS FOR EACH WEEK (30S AND 60S).              
*                                                                               
BLDACCS  NTR1                                                                   
         GOTO1 CLRACC              INITIALIZE ACCUMULATOR TABLE                 
*                                                                               
         LA    R5,ACCTAB           POINT R5 TO ACCUMULATOR TABLE                
         USING ACCTABD,R5                                                       
*                                                                               
         L     R4,ATRDTAB          POINT R4 TO TRADE SPOTS TABLE                
         USING TRDTABD,R4                                                       
*                                                                               
BA10     CLI   0(R4),X'FF'         WHILE NOT END OF TRADE SPOTS TABLE           
         BNL   BA100                                                            
*                                                                               
         SR    R3,R3               ACCUMULATE NUMBER OF 30 SPOTS IN R3          
         SR    R6,R6               ACCUMULATE NUMBER OF 60 SPOTS IN R6          
*                                                                               
         SR    R2,R2               CLEAR R2 THE PROGRAM NUMBER                  
*                                                                               
BA20     C     R2,NUMPROGS         WHILE R2 < NUMBER OF PROGRAMS                
         BNL   BA30                                                             
*                                                                               
         LA    RF,TRDSPOT3(R2)     ADD 30 SEC SPOTS TO R3                       
         ZIC   RE,0(RF)                                                         
         AR    R3,RE                                                            
*                                                                               
         LA    RF,TRDSPOT6(R2)     ADD 60 SEC SPOTS TO R6                       
         ZIC   RE,0(RF)                                                         
         AR    R6,RE                                                            
*                                                                               
         LA    R2,1(R2)            BUMP R2 TO NEXT PROGRAM                      
         B     BA20                                                             
*                                                                               
BA30     STCM  R3,15,ACCONE        SAVE NUMBER OF 30 SPOTS IN ACCONE            
         STCM  R6,15,ACCTWO        SAVE NUMBER OF 60 SPOTS IN ACCONE            
*                                                                               
         LA    R4,TRDTABL(R4)      BUMP R4 TO NEXT WEEK                         
         LA    R5,ACCTABL(R5)      BUMP R5 TO NEXT WEEK                         
         B     BA10                END OF WHILE LOOP                            
*                                                                               
BA100    DS    0H                                                               
*                                                                               
BAX      B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
ERRMAT   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'ERROR - ESTIMATES DO NOT MATCH'                   
         OI    CONSERVH+6,X'81'    SET MODIFIED AND TRANSMIT                    
         LA    R2,REQMEDH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
ERRSUB   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'ERROR - NO DOLLARS TO TRANSFER'                   
         OI    CONSERVH+6,X'81'    SET MODIFIED AND TRANSMIT                    
         LA    R2,REQMEDH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
ERRZERO  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(23),=C'ERROR - NO GOAL DOLLARS'                          
         OI    CONSERVH+6,X'81'    SET MODIFIED AND TRANSMIT                    
         LA    R2,REQMEDH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
EXIT     XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(18),=C'TRANSFER COMPLETED'                               
         OI    CONSERVH+6,X'81'    SET MODIFIED AND TRANSMIT                    
         LA    R2,REQMEDH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPCSOFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPCSOFCD                                                       
         EJECT                                                                  
GOALFLGS DS    XL53                FLAGS WEEKS WITH NON-ZERO 60SEC GOLS         
TAB30    DS    XL(32*TAB30L)       TABLE OF 30 SECOND ESTIMATES                 
TAB60    DS    XL(32*TAB60L)       TABLE OF 60 SECOND ESTIMATES                 
*                                                                               
ATRDTAB  DS    A                   A(TRADE SPOTS TABLE)                         
*                                                                               
TAB30D   DSECT                     30 SECOND ESTIMATE TABLE                     
T30DISKA DS    XL4                 DISK ADDRESS                                 
T30DISK6 DS    XL4                 DISK ADDRESS OF MATCHING 60 REC              
TAB30L   EQU   *-TAB30D                                                         
*                                                                               
TAB60D   DSECT                     60 SECOND ESTIMATE TABLE                     
T60DAY   DS    X                   DAYS                                         
T60TIME  DS    XL4                 TIMES                                        
T60DISKA DS    XL4                 DISK ADDRESS                                 
TAB60L   EQU   *-TAB60D                                                         
*                                                                               
TRDTABD  DSECT                     TRADE SPOTS TABLE (FOR 30 PROGS)             
TRDSPOT3 DS    XL30                NUMBER OF 30 SEC TRADE SPOTS                 
TRDSPOT6 DS    XL30                NUMBER OF 60 SEC TRADE SPOTS                 
TRDCOST  DS    XL(30*2)            COST PER SPOT (30S COST)                     
TRDTABL  EQU   *-TRDTABD                                                        
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
MYSTA    DS    XL3                 BINARY STATION CODE                          
MYKEY    DS    XL48                SAVED KEY FIELDS                             
*                                                                               
BMEST30  DS    X                   30 SECOND ESTIMATE NUMBER                    
START30  DS    CL6                 30 SECOND ESTIMATE START TIME                
BMEST60  DS    X                   60 SECOND ESTIMATE NUMBER                    
*                                                                               
TOTAL30  DS    F                   TOTAL 30 SECOND DOLLARS FOR THE YEAR         
TOTAL60  DS    F                   TOTAL 60 SECOND DOLLARS FOR THE YEAR         
GROSSPER DS    F                   PERCENTAGE OF 60 SEC GOAL/TOTAL GOAL         
*                                                                               
NUMPROGS DS    X                   NUMBER OF PROGRAMS                           
*                                                                               
GROSSDOL DS    F                   TOTAL NTP$ FROM 30 SEC PROGS                 
SUBDOL   DS    F                   NTP$ FOR WEEKS WITH NON-ZERO 60 GOAL         
PERCENT  DS    F                   PERCENTAGE OF 60 SEC GOAL/TOTAL GOAL         
*                                                                               
LEFTOVER DS    F                   DOLLARS LEFTOVER FROM LAST WEEK              
*                                                                               
LOWSPOTS DS    F                   LEAST NUMBER OF SPOTS FOR ANY PROG           
SUMTAB   DS    XL(30*4)            TOTAL 60 SEC SPOTS FOR EACH PROG             
CHOICE   DS    X                   PROGRAM NUMBER CHOSEN TO TRANSER             
         PRINT OFF                                                              
       ++INCLUDE SPGENDMN                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    C                                                                
LSTREF   DS    CL2                                                              
         DS    CL4                                                              
LSTPROG  DS    CL19                                                             
         DS    CL4                                                              
LSTDAY   DS    CL6                                                              
         DS    CL4                                                              
LSTTIME  DS    CL6                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'093SPCSO0C   05/01/02'                                      
         END                                                                    
