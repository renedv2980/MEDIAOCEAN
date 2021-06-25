*          DATA SET SPRES04    AT LEVEL 006 AS OF 11/12/03                      
*PHASE T20F04A                                                                  
         TITLE 'T20F04 - COMBINED LAYOUT/TREND REPORT'                          
T20F04   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1904**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*        VALIDATE KEY                                                           
*                                                                               
VKEY     MVI   PRINTOPT,0          CLEAR OUT PRINTOPT                           
         LA    R2,LAYSRCEH         VALIDATE SOURCE                              
         GOTO1 VVALSRC                                                          
         SPACE 1                                                                
         LA    R2,LAYBOOKH         VALIDATE BOOK                                
         MVI   MAX,10              DUMMY SO I CAN DO ERROR MSG                  
         GOTO1 VVALBOOK                                                         
         CLI   NBOOKS,1                                                         
         BNE   *+8                                                              
         OI    PRINTOPT,X'80'      1 BOOK                                       
         CLI   NBOOKS,8            REALLY 8 BOOKS ALLOWED                       
         BNH   VKEY5                                                            
VKEYTM   MVI   ERROR,MANYBKS       TOO MANY BOOKS                               
         B     ERREND                                                           
         SPACE 1                                                                
VKEY5    LA    R2,LAYDEMOH         VALIDATE DEMOS                               
         MVI   MAX,20              DUMMY SO I CAN DO ERROR MSG                  
         MVI   NFLDS,1                                                          
         GOTO1 VVALDEM                                                          
         MVI   ERROR,MISSING                                                    
         CLI   NDEMOS,0                                                         
         BE    ERREND                                                           
         CLI   NDEMOS,8            REALLY ALLOW 8 DEMOS                         
         BNH   VKEY10                                                           
         MVI   ERROR,MANYDEM       TOO MANY DEMOS                               
         B     ERREND                                                           
         SPACE 1                                                                
VKEY10   LA    R2,LAYSTATH         VALIDATE STATION                             
         LA    R3,6                                                             
         LA    R4,STATS                                                         
         XC    STATS,STATS                                                      
         SR    R1,R1               COUNT NUMBER OF STATIONS                     
         LA    R5,MKTSV                                                         
         XC    MKTSV(144),MKTSV                                                 
         XC    MKTSV+144(144),MKTSV+144                                         
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
VKEY20   GOTO1 VVALSTA                                                          
         LA    R1,1(R1)                                                         
         SPACE 1                                                                
         MVC   0(5,R4),ACTSTAT                                                  
* GET MARKET VALUE                                                              
         L     R6,AIO1                                                          
         USING STAHDRD,R6                                                       
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,ACTMKT                                                        
         GOTO1 VGETMKT                                                          
*                                                                               
         L     R6,AIO1                                                          
         USING MKTHDRD,R6                                                       
         MVC   0(29,R5),MKTNAME                                                 
*                                                                               
         LA    R4,5(R4)                                                         
         LA    R5,29(R5)                                                        
         BAS   RE,BUMP                                                          
         BCT   R3,*+8                                                           
         B     VKEY30                                                           
         CLI   5(R2),0             ANOTHER STATION?                             
         BNE   VKEY20                                                           
         SPACE 1                                                                
VKEY30   STC   R1,NUMSTAT                                                       
*                                                                               
*                                                                               
         LA    R2,LAYDAYH          VALIDATE DAY/DETAIL AND TIME FIELDS          
         GOTO1 VVALDYTM,PARAS,8    8 DAY/DETAIL FIELDS                          
*                                                                               
         LA    R1,8                                                             
         LA    R2,LAYDAYH                                                       
         LA    R3,DAYTMLST                                                      
* ALL DAYS AND OR TIMES NOT ALLOWED                                             
VKEY50   MVI   ERROR,INVDAYTM                                                   
         CLI   0(R3),X'FF'                                                      
         BE    ERREND                                                           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVI   ERROR,INVTIME                                                    
         CLI   1(R3),X'FF'                                                      
         BE    ERREND                                                           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R3,5(R3)                                                         
         BCT   R1,VKEY50                                                        
         SPACE 1                                                                
         TM    WHEN,OV             OVERNITES ALWAYS OK                          
         BO    VKEY60                                                           
         LA    R3,DAYTMLST         CHECK SIZE OF REQUEST                        
         LA    R2,8                                                             
         SR    R1,R1                                                            
VKEY52   OC    0(5,R3),0(R3)                                                    
         BZ    *+16                                                             
         LA    R1,1(R1)                                                         
         LA    R3,5(R3)                                                         
         BCT   R2,VKEY52                                                        
         ZIC   R0,NUMSTAT          NUMBER OF STATIONS                           
         MR    R0,R0               X NUMBER OF DAYS AND TIMES                   
         ZIC   R0,NBOOKS           X NUMBER OF BOOKS                            
         MR    R0,R0                                                            
         LA    R2,50               LIMIT FOR NOW REPORTS                        
         TM    WHEN,NOW                                                         
         BO    *+8                                                              
         LA    R2,120              LIMIT FOR SOON REPORTS                       
         CR    R1,R2                                                            
         BNH   VKEY60                                                           
         LA    R2,LAYSRCEH                                                      
         MVI   ERROR,TOOBIG                                                     
         B     ERREND                                                           
         SPACE 1                                                                
VKEY60   LA    R2,LAYNDXH          INDEX BOOK                                   
         CLI   5(R2),0             OPTIONAL INPUT FIELD                         
         BE    VKEYX                                                            
         MVI   MAX,20              DUMMY SO I CAN DO ERROR MSG                  
         MVC   BOOKS+76(4),BOOKS    SAVE 1ST BOOK VALUE                         
         GOTO1 VVALBOOK                                                         
         MVC   BOOKS+72(4),BOOKS    SAVE INDEX BOOK HERE                        
         MVC   BOOKS(4),BOOKS+76     RESTORE 1ST BOOK                           
         OI    PRINTOPT,X'40'      INDICATE INDEX BOOK                          
         CLI   NBOOKS,1            REALLY ONLY 1 BOOK ALLOWED                   
         BE    VKEYX                                                            
         MVI   ERROR,MANYBKS       TOO MANY BOOKS                               
         B     ERREND                                                           
         SPACE 1                                                                
VKEYX    B     PREP                DON'T PRINT,JUST CONT VALIDTING              
         EJECT                                                                  
* *********************************************************************         
* PRINT DATA TYPE REPORT                                                        
*        VALKEY WILL GO THRU THIS CODE BUT WILL NOT PRINT OUT ANYTHING          
*        IF THERE DOES NOT EXIST DATA FOR THE SPECIFIED BOOK/STATION            
*        AND REPORT IS REQUESTED SOON, WE MUST INFORM REQUESTED OF HIS/         
*        HER INVALID SELECTION BEFORE IT JUST GOES AND PRODUCES AN              
*        EMPTY REPORT.                                                          
* *********************************************************************         
*                                                                               
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
PREP5    MVI   RCSUBPRG,0                                                       
         MVI   FIRST,C'Y'          INDICATES TOP OF PAGE                        
         BAS   RE,CLEARBUF                                                      
         EJECT                                                                  
*   CONTROL I/O                                                                 
         SPACE 2                                                                
         LA    R2,DAYTMLST         LIST OF DAY/TIMES                            
         ST    R2,SVDAYTM                                                       
         SPACE 1                                                                
LAY20    MVC   SVDAY,0(R2)         SAVE DAY FOR PRINTING                        
         XC    PROGTMQ,PROGTMQ                                                  
         MVC   STIM,1(R2)          START TIME                                   
         MVC   ETIM,3(R2)          END TIME                                     
         BAS   RE,RNDTIME          ROUND TIMES TO NEAREST 1/2 HOUR              
         MVC   HALF,ETIM                                                        
         BAS   RE,TOQTRHR          CONVERT END TIME TO QTR HR                   
         MVC   ETIMQ,BYTE          AND SAVE                                     
         SPACE 1                                                                
LAY30    BAS   RE,ADDHLF           GO TO DEMAND WITH 1/2 HR CHUNKS              
         MVI   COUNT,0             COUNT STATIONS THROUGH LOOP                  
         LA    R3,STATS            LIST OF STATIONS IN DBSELSTA FMT             
         LA    R2,SVACTRMK         RATING SERVICE MARKET                        
         SPACE 1                                                                
LAY40    LA    R5,BOOKS            LIST OF BOOKS                                
         LA    R6,BUFF                                                          
         SPACE 1                                                                
         ZIC   R1,COUNT            DO WE WANT THIS 1/2 HR                       
         SLL   R1,3                                                             
         LA    R1,PROGTMQ(R1)                                                   
         ST    R1,SVPRGTMQ                                                      
         CLC   0(1,R1),STIMQ       FOR THIS BOOK                                
         BH    LAY70                                                            
         SPACE 1                                                                
*  BUILD REST OF DBLOCK                                                         
         SPACE 1                                                                
LAY50    MVI   GOTPROG,0           CLEAR GOT PROGRAM INDICATOR                  
         MVI   SVWKS,0             AND SAVE WEEKS INDICATOR                     
         LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBSELRMK,0(R2)      RATING SERVICE MARKET                        
         MVC   DBSELSTA,0(R3)                                                   
         MVC   DBSELBK,1(R5)                                                    
         MVC   DBBTYPE,3(R5)       BOOK TYPE                                    
         MVI   DBBEST,C'A'                                                      
         ZIC   R1,SVDAY                                                         
         LA    R1,DAYBITS(R1)                                                   
         MVC   DBSELDAY,0(R1)                                                   
         CLI   DBSELDAY,3          IS IS SA-SU                                  
         BNE   *+8                                                              
         MVI   DBDAYOPT,C'Y'       INDICATES SA-SU AVERAGE LINE                 
         MVC   DBSELTIM(2),STIM                                                 
         MVC   DBSELTIM+2(2),HALFHR                                             
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     R1,0(R1)                                                         
         SPACE 1                                                                
         TM    FATFLAG-FACTSD(R1),X'01'       ON MEANS OFFLINE                  
         BO    LAY60                                                            
         SPACE 1                                                                
         LH    RE,FATIOCNT-FACTSD(R1)   IF ONLINE, AND IO COUNT IS              
         LA    RE,50(RE)                WITHIN 50 OF LIMIT, THEN                
         CH    RE,FATMAXIO-FACTSD(R1)                                           
         BNH   LAY60                                                            
         LA    R2,LAYSRCEH                                                      
         MVI   ERROR,TOOBIG        REQUEST TOO BIG                              
         B     ERREND                                                           
*                                                                               
LAY60    MVC   MYDBLOCK(50),DBLOCK                                              
         L     RF,AIO2                                                          
         USING SPDEMLKD,RF                                                      
         XC    SPDEMLK,SPDEMLK                                                  
         MVC   SPLKAFAC,ACOMFACS                                                
         MVC   SPLKAREC,DBAREC     IO AREA                                      
         ZIC   R1,NDEMOS                                                        
         MH    R1,=H'3'                                                         
         LA    R1,DEMOS(R1)                                                     
         MVI   0(R1),X'FF'                                                      
         LA    RE,DEMOS            LIST OF DEMOS                                
         ST    RE,SPLKALST                                                      
         LA    RE,ELEM             LIST OF OUTPUT DEMO VALUES                   
         ST    RE,SPLKAVAL                                                      
         LA    RE,FILL             SET THE HOOK                                 
         ST    RE,SPLKHOOK                                                      
         MVC   SPLKFIL,DBFILE      CONVERT TO SPLK BLOCK                        
         MVC   SPLKMED,DBSELMED                                                 
         MVC   SPLKSRC,DBSELSRC                                                 
         MVC   SPLKAGY,DBSELAGY                                                 
         MVC   SPLKCLI,=C'=L'      INHIBIT XTRA QH OPTION                       
         MVC   SPLKDBK,DBSELBK                                                  
         MVC   SPLKRMK,DBSELRMK                                                 
         MVC   SPLKSTA,DBSELSTA                                                 
         MVC   SPLKDOPT,DBDAYOPT                                                
         MVC   SPLKBEST,DBBEST                                                  
         MVC   SPLKBTYP,DBBTYPE                                                 
         MVC   SPLKDAY,DBSELDAY                                                 
         MVC   SPLKTIM,DBSELTIM                                                 
         MVI   SPLKSVI,X'FF'       SUPPRESS THE SVI'S                           
         DROP  RF                                                               
         GOTO1 GETDEMO,DMCB,(X'FF',AIO2)                                        
*                                                                               
         CLI   0(R1),0             ERROR                                        
         BE    LAY65                                                            
         MVC   CONHEAD(L'NOFOUND),NOFOUND                                       
         MVC   CONHEAD+L'NOFOUND(5),0(R3) SHOW STATION                          
         LA    R2,CONHEAD+L'NOFOUND+6                                           
         MVI   0(R2),C'/'                                                       
         LA    R2,1(R2)                                                         
         ZIC   R1,2(R5)            AND BOOK                                     
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   0(3,R2),0(R1)                                                    
         EDIT  (1,1(R5)),(2,3(R2))                                              
         CLI   3(R5),0             SPECIAL BOOK TYPE                            
         BE    LAY63                                                            
         MVC   5(3,R2),=C'( )'                                                  
         MVC   6(1,R2),3(R5)                                                    
         SPACE 1                                                                
LAY63    LA    R2,LAYSTATH                                                      
         MVC   WORK,CONHEAD        BECAUSE GETERR WANTS IT THERE                
         B     MYEND                                                            
         SPACE 1                                                                
LAY65    MVI   DBDAYOPT,0          RESET FOR NEXT TIME                          
         CLI   MODE,VALKEY         ARE WE STILL VALIDATING?                     
         BE    *+8                                                              
         BAS   RE,FILLDEM                                                       
         MVC   DBLOCK(50),MYDBLOCK                                              
         SPACE 2                                                                
LAY70    LA    R5,4(R5)            NEXT BOOK                                    
         LA    R6,220(R6)          NEXT BUFFER AREA                             
         OC    0(4,R5),0(R5)                                                    
         BZ    LAY75                                                            
         SPACE 1                                                                
         L     R1,SVPRGTMQ                                                      
         LA    R1,1(R1)            TIME FOR NEXT BOOK                           
         ST    R1,SVPRGTMQ                                                      
         CLC   0(1,R1),STIMQ       DO WE WANT THIS 1/2 HR                       
         BH    LAY70               FOR THIS BOOK                                
         B     LAY50                                                            
         SPACE 1                                                                
LAY75    TM    PRINTOPT,X'04'      HAVE WE DONE INDEX BOOK YET                  
         BO    LAY80                                                            
         OI    PRINTOPT,X'04'      NOW WE HAVE                                  
         TM    PRINTOPT,X'40'      IS THERE AN INDEX BOOK                       
         BZ    LAY80                                                            
         TM    PRINTOPT,X'10'      ONLY DO INDEX IF                             
         BZ    LAY80               RECORDS FOR THIS 1/2 HR.                     
*                                                                               
         LA    R5,BOOKS+72          POINT TO INDEX BOOK                         
         LA    R6,BUFF                                                          
         LA    R6,2200(R6)         AND TO INDEX BUFF DEMOS AREA                 
         SPACE 1                                                                
         B     LAY50               AND GO BUILD DBLOCK                          
         SPACE 1                                                                
LAY80    NI    PRINTOPT,X'FB'      RESET DONE INDEX BOOK INDICATOR              
         LA    R3,5(R3)            NEXT STATION                                 
         LA    R2,2(R2)            NEXT RATING SERVICE MARKET                   
         ZIC   R1,COUNT            INCREMENT STATION NUMBER                     
         LA    R1,1(R1)                                                         
         STC   R1,COUNT                                                         
         OC    0(5,R3),0(R3)                                                    
         BNZ   LAY40                                                            
         SPACE 1                                                                
         CLI   MODE,VALKEY         ARE WE STILL VALIDATING?                     
         BE    *+8                                                              
         BAS   RE,SPLAT            PRINT WHAT WE'VE GOT SO FAR                  
         NI    PRINTOPT,X'EF'      RESET PRINT INDICATOR                        
         SPACE 1                                                                
         CLC   HALFHRQ,ETIMQ       COMPARE QUARTER HOURS                        
         BNL   LAY90                                                            
         MVC   STIM,HALFHR                                                      
         B     LAY30                                                            
         SPACE 1                                                                
LAY90    L     R2,SVDAYTM                                                       
         LA    R2,5(R2)            NEXT DAY/TIME                                
         ST    R2,SVDAYTM                                                       
         CLI   MODE,VALKEY         ARE WE STILL VALIDATING?                     
         BE    LAY93                                                            
         BAS   RE,ALLSTARS         PRINT CLOSING STARS                          
         GOTO1 SPOOL,PARAS,(R8)                                                 
LAY93    MVI   FORCEHED,C'Y'       STARTS NEW PAGE                              
         MVI   FIRST,C'Y'          INDICATES TOP OF PAGE                        
         LA    RE,DAYTMLST+L'DAYTMLST                                           
         CR    R2,RE                                                            
         BNL   XIT                                                              
         OC    0(5,R2),0(R2)       NEXT DAY/TIME                                
         BNZ   LAY20                                                            
         SPACE 1                                                                
         B     XIT                                                              
         SPACE 3                                                                
DAYBITS  DC    X'7C402010080402017F03'                                          
*                                                                               
* BITS REPRESENT M-F,MON,TUE,WED,THUR,FRI,SAT,SUN,M-SU,SA-SU                    
         EJECT                                                                  
*  PROCESS A PAV RECORD                                                         
         SPACE 1                                                                
FILL     NTR1                                                                   
         USING BUFFD,R6                                                         
         L     RF,AIO2                                                          
         USING SPDEMLKD,RF                                                      
         L     RE,SPLKDBLK         SET UP NEW DBLOCK                            
         MVC   DBLOCK(256),0(RE)                                                
         DROP  RF                                                               
         ZIC   R2,COUNT            SAVE RATING SERVICE MARKET                   
         SLL   R2,1                                                             
         LA    R2,SVACTRMK(R2)                                                  
         MVC   0(2,R2),DBACTRMK                                                 
         SPACE 1                                                                
FILL10   CLI   GOTPROG,C'D'        DO WE HAVE PROGRAM WE WANT                   
         BE    PREPX               YES, GET NEXT PROGRAM                        
         SPACE 1                                                                
         ZIC   R2,COUNT            STATION NUMBER                               
         MH    R2,=H'20'           FIND PLACE IN TABLE                          
         LA    R2,BUFFPROG(R2)     PROGS ARE 20 EACH                            
         ZIC   R3,COUNT                                                         
         SLL   R3,4                                                             
         LA    R3,BUFFDEM(R3)      DEMOS ARE 16 EACH                            
         SPACE 1                                                                
         GOTO1 DEFINE,PARAS,=C'PROGRAM',DBLOCK,WORK+10                          
         CLC   WORK+19(5),=C'(NOR)'   IF (NOR), ALWAYS EXCLUDE                  
         BE    PREPX                                                            
         SPACE 1                                                                
         BAS   RE,SELECT           NOW SEE IF WE WANT THIS ONE                  
         CLI   WANT,C'N'                                                        
         BE    PREPX                                                            
         OI    PRINTOPT,X'10'      INDICATE RECORDS TO PRINT                    
         MVC   BUFFBK,0(R5)        BOOK                                         
         MVC   0(16,R2),WORK+10    MOVE IN PROGRAM NAME                         
         SPACE 1                                                                
         TM    PRINTOPT,X'08'      SHOW WEEKS                                   
         BZ    FILL15                                                           
*                                                                               
         CLI   DBSELSRC,C'A'       IF ARB                                       
         BNE   FILL13                                                           
         CLC   BUFFBK+1(2),=X'5605'     AND BOOK IS BEFORE MAY86                
         BL    FILL15              DON'T SHOW WEEKS                             
*                                                                               
FILL13   MVC   16(4,R2),=C'0000'                                                
         TM    WORK,X'08'                                                       
         BZ    *+8                                                              
         MVI   16(R2),C'-'                                                      
         SPACE 1                                                                
         TM    WORK,X'04'                                                       
         BZ    *+8                                                              
         MVI   17(R2),C'-'                                                      
         SPACE 1                                                                
         TM    WORK,X'02'                                                       
         BZ    *+8                                                              
         MVI   18(R2),C'-'                                                      
         SPACE 1                                                                
         TM    WORK,X'01'                                                       
         BZ    *+8                                                              
         MVI   19(R2),C'-'                                                      
         SPACE 1                                                                
FILL15   L     R2,SVPRGTMQ         SAVE PROGRAM END QTR HR FOR BOOK             
         GOTO1 DEFINE,PARAS,=C'TIME',DBLOCK,WORK                                
         MVC   0(1,R2),WORK+1                                                   
         B     PREPX                                                            
         SPACE 1                                                                
FILLDEM  NTR1                                                                   
         ZIC   R3,COUNT                                                         
         SLL   R3,4                                                             
         LA    R3,BUFFDEM(R3)      DEMOS ARE 16 EACH                            
         LA    R2,DEMOS                                                         
         LA    R5,ELEM             NOW POINT R5 TO VALUES                       
         ZIC   RE,NDEMOS                                                        
         SPACE 2                                                                
FILL20   TM    PRINTOPT,X'20'      IF ROUNDED DEMOS AND                         
         BZ    FILL60                                                           
         CLI   1(R2),C'R'          RATING OR                                    
         BE    FILL30                                                           
         CLI   1(R2),C'E'          CANADIAN E RATING OR                         
         BE    FILL30                                                           
         CLI   1(R2),C'I'          IMPRESSIONS OR                               
         BE    FILL30                                                           
         CLI   1(R2),C'P'          PUTS OR                                      
         BE    FILL30                                                           
         CLI   1(R2),C'S'          SHARE OR                                     
         BE    FILL30                                                           
         CLI   1(R2),C'X'          TSA SHARE (INPUT AS T), THEN                 
         BNE   FILL60                                                           
         SPACE 1                                                                
FILL30   LH    R1,2(R5)            DROP DECIMAL                                 
         SR    R0,R0                                                            
         LA    R1,5(R1)                                                         
         D     R0,=F'10'                                                        
         STH   R1,0(R3)                                                         
         B     FILL80                                                           
         SPACE 1                                                                
FILL60   MVC   0(2,R3),2(R5)       IF NON-ROUNDED DEMO, USE AS IS               
         SPACE 2                                                                
FILL80   LA    R2,3(R2)            NEXT DEMO                                    
         LA    R3,2(R3)            NEXT DEMO AREA                               
         LA    R5,8(R5)            NEXT DEMO VALUE                              
         BCT   RE,FILL20                                                        
         SPACE 2                                                                
PREPX    B     XIT                                                              
         SPACE 2                                                                
         DROP  R6                                                               
         EJECT                                                                  
*      SELECTION RULES                                                          
* - ALWAYS TAKE 3 OR 4 WEEK PROGRAM                                             
* - ALWAYS TAKE 2 WEEK PROGRAM OVER 1 WEEK PROGRAM                              
* - IF 2/2 SPLIT, TAKE 2 WEEK PROGRAM THAT MATCHES TWOWEEK OPTION               
*    -IF 2/2 ALTERNATING SPLIT (ABAB), TAKE A IF TWOWEEK OPTION=F               
*                                           B IF TWOWEEK OPTION=L               
*    -IF 2/2 ALTERNATING SPLIT (ABBA), ALWAYS TAKE A                            
* - IF 1/1/1/1 SPLIT, TAKE 1 WEEK PROGRAM THAT MATCHES ONEWEEK OPTION           
         SPACE 2                                                                
SELECT   NTR1                                                                   
         BAS   RE,COUNTWK          SEE IF THIS IS WHAT WE WANT                  
         CLI   NWKS,3              IF 3 OR 4 WKS, TAKE IT                       
         BL    SEL10                                                            
         MVI   GOTPROG,C'D'        INDICATE DONE                                
         B     YESWANT                                                          
         SPACE 1                                                                
*************ARB BOOKS BEFORE MAY86 DON'T SHOW ACTUAL WEEKS********             
*******SO DON'T LET 1/1/1/1 OR 2/2 OPTIONS INFLUENCE SELECTION*****             
         SPACE 1                                                                
SEL10    MVC   BYTEA,ONEWEEK                                                    
         MVC   BYTEB,TWOWEEK                                                    
         CLI   DBSELSRC,C'A'                                                    
         BNE   SEL15                                                            
         CLC   1(2,R5),=X'5605'                                                 
         BNL   SEL15                                                            
***SET BYTEA AND BYTEB, BUT IT IS MEANINGLESS                                   
         MVI   BYTEA,X'01'         (DEFAULT TO 4TH WEEK)                        
         MVI   BYTEB,X'03'         (DEFAULT TO LAST 2 WEEKS)                    
         SPACE 1                                                                
SEL15    CLI   NWKS,1              IF 2 WEEKS                                   
         BE    SEL40                                                            
         CLI   GOTPROG,C'Y'        AND WE ALREADY HAVE SOMETHING                
         BNE   SEL30                                                            
         MVI   GOTPROG,C'D'        ONE OF THEM MUST BE GOOD                     
         SPACE 1                                                                
         CLI   SVWKS,2             IF HAD 1 WEEK, KEEP NEW 2 WEEK               
         BNE   YESWANT                                                          
         SPACE 1                                                                
         CLC   WORK(1),BYTEB       IF OLD WAS 2 WEEKS, KEEP                     
         BE    YESWANT             THE ONE THAT MATCHES THE OPTION              
         SPACE 1                                                                
         CLC   SVWORK(1),BYTEB                                                  
         BE    NOWANT                                                           
*   IF NEITHER MATCHES OPTION, MUST BE SOME FORM OF ALTERNATING SPLIT           
         TM    BYTEB,X'08'         IF THEY WANT FIRST 2 WEEKS                   
         BZ    SEL20                                                            
         TM    WORK,X'08'          GIVE PROG THAT RAN IN 1ST WEEK               
         BO    YESWANT                                                          
         B     NOWANT                                                           
         SPACE 1                                                                
SEL20    TM    WORK,X'01'          IF THEY WANT LAST 2 WEEKS                    
         BO    YESWANT             GIVE PROG THAT RAN IN LAST WEEK              
         B     NOWANT                                                           
         SPACE 1                                                                
SEL30    MVC   SVWKS,NWKS          SAVE 1ST 2 WK PROGRAM FOR NOW                
         MVC   SVWORK,WORK                                                      
         MVI   GOTPROG,C'Y'                                                     
         B     YESWANT                                                          
         SPACE 1                                                                
SEL40    CLI   SVWKS,2             IF HAD 2 WEEK, KEEP THAT                     
         BNE   SEL50                                                            
         MVI   GOTPROG,C'D'                                                     
         B     NOWANT                                                           
         SPACE 1                                                                
SEL50    CLC   WORK(1),BYTEA       ELSE, ONLY KEEP 1 WEEK IF IT                 
         BNE   NOWANT              MATCHES THE OPTION                           
         MVI   GOTPROG,C'Y'                                                     
         MVC   SVWKS,NWKS                                                       
         B     YESWANT                                                          
         SPACE 1                                                                
YESWANT  MVI   WANT,C'Y'                                                        
         B     XIT                                                              
         SPACE 1                                                                
NOWANT   MVI   WANT,C'N'                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT FROM BUFF                                       
         SPACE 3                                                                
SPLAT    NTR1                                                                   
         LA    R6,BUFF                                                          
         USING BUFFD,R6                                                         
         ZIC   R3,NBOOKS                                                        
         ZIC   R4,NUMSTAT                                                       
         SPACE 2                                                                
         BAS   RE,MANY             SEE HOW MANY LINES TO PRINT                  
         CLI   FIRST,C'Y'          TOP OF PAGE-IT MUST FIT                      
         BE    SP10                                                             
         ZIC   R1,LINE             ELSE, SEE IF BLOCK WILL FIT ON PAGE          
         ZIC   R0,ALLOWLIN                                                      
         AR    R1,R0                                                            
         IC    R0,MAXLINES                                                      
         CR    R1,R0                                                            
         BNH   SP10                IF THIS BLOCK WON'T FIT,                     
         MVI   FIRST,C'Y'                                                       
         MVI   ALLOWLIN,0                                                       
         BAS   RE,ALLSTARS         PRINT CLOSING STARS                          
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVI   FORCEHED,C'Y'       AND START NEW PAGE                           
         SPACE 1                                                                
SP10     BAS   RE,ALLSTARS                                                      
         GOTO1 SPOOL,PARAS,(R8)                                                 
         SPACE 1                                                                
SP20     MVI   FIRST,C'N'                                                       
         BAS   RE,DUPES            TAKE OUT DUPLICATE NAMES                     
         BAS   RE,DAYTM            FORMAT DAY AND TIME                          
         TM    PRINTOPT,X'10'      ANY RECORDS TO PRINT                         
         BO    SP30                                                             
         SR    R5,R5               NO, THEN MAKE BOX SIZE AS IF                 
         LA    R5,1(R5)            PROGRAM NAME                                 
         ZIC   RE,NDEMOS           AND 1 BOOK                                   
         LA    RE,1(RE)                                                         
         SRL   RE,1                                                             
         AR    R5,RE                                                            
*                                                                               
SP25     LA    R1,P                                                             
         BAS   RE,SUMSTARS                                                      
         GOTO1 SPOOL,PARAS,(R8)                                                 
         BCT   R5,SP25                                                          
         B     XIT                 NO, JUST PRINT DAY/TIME                      
         SPACE 1                                                                
SP30     BAS   RE,PROGS            YES, DIG OUT PROGRAMS TO P                   
         CLC   P,SPACES                                                         
         BE    SP50                                                             
         OC    P,SPACES                                                         
         LA    R1,P                                                             
         BAS   RE,SUMSTARS                                                      
         GOTO1 SPOOL,PARAS,(R8)                                                 
         SPACE 2                                                                
SP50     LA    R2,P+2                                                           
         OC    0(4,R6),0(R6)       BOOK                                         
         BZ    SP130                                                            
         TM    PRINTOPT,X'80'      1 BOOK                                       
         BO    SP70                                                             
         LA    R5,BUFFBK                                                        
         ZIC   R1,2(R5)                                                         
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   0(3,R2),0(R1)                                                    
         EDIT  (1,1(R5)),(2,3(R2))                                              
         CLI   3(R5),0             SPECIAL BOOK TYPE                            
         BE    SP70                                                             
         MVC   5(3,R2),=C'( )'                                                  
         MVC   6(1,R2),3(R5)                                                    
         SPACE 1                                                                
SP70     TM    PRINTOPT,X'08'      SHOW WEEKS                                   
         BZ    SP75                                                             
         LA    R1,4(R6)            POINT TO PROGRAMS IN BUFF                    
         LA    RE,P+11                                                          
         ZIC   RF,NUMSTAT          NUMBER OF STATIONS                           
SP73     MVC   17(2,RE),18(R1)     MOVE IN 2ND 2 WEEKS                          
         LA    RE,20(RE)                                                        
         LA    R1,20(R1)                                                        
         BCT   RF,SP73                                                          
         SPACE 1                                                                
SP75     BAS   RE,NUMBERS          DEMOS                                        
         CLC   P,SPACES                                                         
         BE    SP100                                                            
         OC    P,SPACES                                                         
         LA    R1,P                                                             
         BAS   RE,SUMSTARS                                                      
         LA    R1,P2               CHECK ALL 4 PRINT LINES FOR DATA             
         LA    R5,3                                                             
SP80     CLC   0(132,R1),SPACES                                                 
         BE    SP90                                                             
         OC    0(132,R1),SPACES                                                 
         BAS   RE,SUMSTARS                                                      
         LA    R1,132(R1)          NEXT PRINT LINE                              
         BCT   R5,SP80                                                          
SP90     GOTO1 SPOOL,PARAS,(R8)                                                 
         SPACE 1                                                                
SP100    TM    PRINTOPT,X'40'     INDEX BOOK                                    
         BZ    SP130                                                            
         MVC   P+3(5),=C'INDEX'                                                 
         BAS   RE,INDXBUFF        INDEX                                         
         BAS   RE,NUMBERS                                                       
         CLC   P,SPACES                                                         
         BE    SP130                                                            
         OC    P,SPACES                                                         
         LA    R1,P                                                             
         BAS   RE,SUMSTARS                                                      
         LA    R1,P2               CHECK ALL 4 PRINT LINES FOR DATA             
         LA    R5,3                                                             
SP110    CLC   0(132,R1),SPACES                                                 
         BE    SP120                                                            
         OC    0(132,R1),SPACES                                                 
         BAS   RE,SUMSTARS                                                      
         LA    R1,132(R1)          NEXT PRINT LINE                              
         BCT   R5,SP110                                                         
SP120    GOTO1 SPOOL,PARAS,(R8)                                                 
         SPACE 2                                                                
SP130    LA    R6,220(R6)          NEXT BOOK                                    
         CLI   LINE,50             IF NEAR BOTTOM OF PAGE                       
         BNH   SP135                                                            
         STC   R3,BYTEA                                                         
         CLI   BYTEA,1             AND MORE BOOKS TO PRINT                      
         BE    SP135                                                            
         BAS   RE,ALLSTARS         PRINT STARS TO CLOSE PAGE                    
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVI   FORCEHED,C'Y'       START NEW PAGE,                              
         BAS   RE,ALLSTARS         PRINT STARS AT TOP OF PAGE,                  
         GOTO1 SPOOL,PARAS,(R8)                                                 
         BAS   RE,DAYTM            AND FORMAT DAY AND TIME                      
SP135    BCT   R3,SP30                                                          
         BAS   RE,CLEARBUF                                                      
         B     XIT                                                              
         SPACE 1                                                                
         DROP  R6                                                               
         SPACE 2                                                                
DAYTBL   DC    C'M-F0MON1TUE2WED3THU4FRI5SAT6SUN7M-S8SA-9'                      
         DC    X'FF'                                                            
         SPACE 2                                                                
         EJECT                                                                  
*              ROUTINE TO TAKE OUT DUPLICATE NAMES                              
         SPACE 3                                                                
DUPES    NTR1                                                                   
         LA    R6,BUFF+4                                                        
         LA    R5,220(R6)                                                       
         BCT   R3,DUP10                                                         
         B     XIT                                                              
         SPACE 2                                                                
DUP10    BAS   RE,DUP20                                                         
         LA    R6,20(R6)                                                        
         LA    R5,20(R5)                                                        
         BCT   R4,DUP10                                                         
         B     XIT                                                              
         SPACE 2                                                                
DUP20    NTR1                                                                   
         SPACE 2                                                                
DUP30    CLC   0(20,R6),0(R5)                                                   
         BE    DUP40                                                            
         LR    R6,R5                                                            
         B     DUP50                                                            
         SPACE 2                                                                
DUP40    MVC   0(20,R5),SPACES                                                  
         SPACE 2                                                                
DUP50    LA    R5,220(R5)                                                       
         BCT   R3,DUP30                                                         
         B     XIT                                                              
         EJECT                                                                  
DAYTM    NTR1                                                                   
         ZIC   R1,SVDAY            PRINT DAY/TIME ON 1ST LINE                   
         SLL   R1,2                                                             
         LA    R1,DAYTBL(R1)                                                    
         MVC   P+1(3),0(R1)                                                     
         CLC   P+1(3),=C'SA-'      INSTEAD OF SA-SU,                            
         BNE   *+10                                                             
         MVC   P+1(3),=C'WKE'      PRINT WKE                                    
         SPACE 1                                                                
         XC    WORK(4),WORK        ONLY WANT START TIME                         
         MVC   WORK(2),STIM                                                     
         GOTO1 UNTIME,PARAS,WORK,P+5                                            
         B     XIT                                                              
         EJECT                                                                  
*              CALCULATION OF HOW MANY LINES WILL PRINT                         
         SPACE 1                                                                
MANY     NTR1                                                                   
         USING BUFFD,R6                                                         
         SR    R5,R5               COUNT LINES IN R5                            
         SPACE 2                                                                
MAN10    CLC   BUFFPROG(120),SPACES    ANY PROGS TO PRINT                       
         BE    *+8                                                              
         LA    R5,1(R5)                                                         
         OC    BUFFDEM(96),BUFFDEM                                              
         BZ    MAN20                                                            
         ZIC   RE,NDEMOS                                                        
         LA    RE,1(RE)                                                         
         SRL   RE,1                                                             
         AR    R5,RE                                                            
*                                                                               
         LA    R2,BUFF                                                          
         LA    R2,2324(R2)                                                      
         OC    0(96,R2),0(R2)     ANY INDEX DEMOS                               
         BZ    MAN20                                                            
         ZIC   RE,NDEMOS                                                        
         LA    RE,1(RE)                                                         
         SRL   RE,1                                                             
         AR    R5,RE                                                            
         SPACE 2                                                                
MAN20    LA    R6,220(R6)                                                       
         BCT   R3,MAN10                                                         
         LA    R5,2(R5)            + 2 LINES FOR TOP AND BOTTOM STARS           
         STC   R5,ALLOWLIN                                                      
         B     XIT                                                              
         SPACE 1                                                                
         DROP  R6                                                               
         EJECT                                                                  
*              BUFF HANDLING ROUTINES                                           
         SPACE 2                                                                
* INDEX BOOK INFO IS ALWAYS STORED AT BUFF+2200                                 
* (INDEX BOOK IS AT BOOK+72)                                                    
         SPACE 2                                                                
INDXBUFF NTR1                                                                   
         LA    R6,124(R6)          R6=A(LINE TO BE INDEXED)                     
         LA    R3,BUFF                                                          
         LA    R3,2324(R3)         R3=A(INDEX DEMOS)                            
         LA    R4,48               8 DEMOS X 6 STATIONS                         
         SPACE 2                                                                
IND10    L     R1,0(R6)                                                         
         XC    0(2,R6),0(R6)                                                    
         M     R0,=F'200'                                                       
         OC    0(2,R3),0(R3)                                                    
         BZ    IND20                                                            
         D     R0,0(R3)                                                         
         AH    R1,=H'1'                                                         
         SRL   R1,1                                                             
         STH   R1,0(R6)                                                         
         SPACE 2                                                                
IND20    LA    R6,2(R6)                                                         
         LA    R3,2(R3)                                                         
         BCT   R4,IND10                                                         
         B     XIT                                                              
         EJECT                                                                  
PROGS    NTR1                      DIG OUT PROGRAM NAMES                        
*    R6 POINTS AT BUFF                                                          
         LA    R6,4(R6)            NOW IT POINTS AT PROGRAM NAMES               
         LA    R3,P+11                                                          
         SPACE 2                                                                
PROG10   MVC   0(16,R3),0(R6)                                                   
         TM    PRINTOPT,X'08'      SHOW WEEKS                                   
         BZ    PROG20                                                           
         MVC   17(2,R3),16(R6)     MOVE IN 1ST 2 WEEKS                          
         B     PROG30                                                           
PROG20   GOTO1 CENTER,PARAS,(R3),19                                             
PROG30   LA    R6,20(R6)                                                        
         LA    R3,20(R3)                                                        
         BCT   R4,PROG10                                                        
         B     XIT                                                              
         EJECT                                                                  
CLEARBUF NTR1                                                                   
         XCEF  BUFF,2600                                                        
         LA    R6,BUFF                                                          
         USING BUFFD,R6                                                         
         LA    R3,12                                                            
         SPACE 2                                                                
CB10     MVC   BUFFPROG(120),SPACES                                             
         LA    R6,220(R6)                                                       
         BCT   R3,CB10                                                          
         B     XIT                                                              
         SPACE 1                                                                
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO OUTPUT THE NUMBERS                                    
         SPACE 3                                                                
* R2=PRINT LINE POINTER                                                         
* R3=1ST POSITION ON PRINT LINE FOR EACH STATION                                
* SAVER3=1ST PRINT LINE, 1ST POSITON FOR EACH STATION                           
* R4=NUMBER OF STATIONS                                                         
* R5=NUMBER OF DEMOS PER LINE, PER STATION                                      
* R6=DEMOS IN BUFFER AREA                                                       
* SAVER6=1 DEMO FOR THE STATION IN BUFFER AREA                                  
* BYTEA=NUMBER OF DEMOS LEFT TO DO                                              
* RF=DEMOS                                                                      
         SPACE 2                                                                
NUMBERS  NTR1                                                                   
*     R6 POINTS AT BUFF                                                         
         LA    R6,124(R6)          NOW POINT TO BUFF DEMOS                      
         ST    R6,SAVER6                                                        
         LA    R3,P+13                                                          
         ST    R3,SAVER3                                                        
         B     NUM20                                                            
         SPACE 1                                                                
NUM10    L     R3,SAVER3           MAIN PRINT LINE POSITION                     
         LA    R3,20(R3)                                                        
         ST    R3,SAVER3                                                        
         SPACE 1                                                                
         L     R6,SAVER6           MAIN DEMO POSTION IN BUFF                    
         LA    R6,16(R6)                                                        
         ST    R6,SAVER6                                                        
         SPACE 1                                                                
NUM20    MVC   BYTEA,NDEMOS                                                     
         LA    R5,2                2 DEMOS PER LINE                             
         LR    R2,R3                                                            
         LA    RF,DEMOS                                                         
         SPACE 1                                                                
NUM30    OC    0(2,R6),0(R6)                                                    
         BZ    NUM70                                                            
         EDIT  (2,0(R6)),(4,0(R2))                                              
         CLC   P+3(5),=C'INDEX'    IT'S A PCT - DON'T SHOW DECIMALS             
         BE    NUM70                                                            
         TM    PRINTOPT,X'20'      ROUNDED DEMOS                                
         BZ    NUM50                                                            
*                 FOR ROUNDED DEMOS -                                           
         CLI   0(R2),C' '          SHOW BIG NUMBERS AS MILLIONS                 
         BE    NUM70                                                            
         LH    R1,0(R6)                                                         
         LA    R1,500(R1)                                                       
         SR    R0,R0                                                            
         D     R0,=F'1000'                                                      
         EDIT  (R1),(3,0(R2))                                                   
         MVI   3(R2),C'M'                                                       
         B     NUM70                                                            
         SPACE 1                                                                
*        FOR NON-ROUNDED DEMOS, SHOW DECIMAL POINTS FOR -                       
NUM50    CLI   1(RF),C'R'          RATING                                       
         BE    NUM60                                                            
         CLI   1(RF),C'E'          CANADIAN E RATING                            
         BE    NUM60                                                            
         CLI   1(RF),C'P'          PUT                                          
         BE    NUM60                                                            
         CLI   1(RF),C'S'          SHARE                                        
         BE    NUM60                                                            
         CLI   1(RF),C'I'          IMPRESSIONS                                  
         BE    NUM60                                                            
         CLI   1(RF),C'X'          TSA SHARE (INPUT AS T)                       
         BNE   NUM70                                                            
         SPACE 1                                                                
NUM60    EDIT  (2,0(R6)),(6,0(R2)),1     GET DECIMAL POINT                      
         SPACE 2                                                                
NUM70    LA    R6,2(R6)                                                         
         LA    R2,9(R2)                                                         
         LA    RF,3(RF)            NEXT DEMO                                    
         ZIC   R1,BYTEA                                                         
         BCTR  R1,0                                                             
         STC   R1,BYTEA                                                         
         CLI   BYTEA,0             NO MORE DEMOS                                
         BE    NUM80                                                            
         BCT   R5,NUM30                                                         
         LA    R3,132(R3)                                                       
         LR    R2,R3                                                            
         LA    R5,2                                                             
         CLI   BYTEA,0                                                          
         BNE   NUM30                                                            
NUM80    BCT   R4,NUM10            NEXT STATION                                 
         B     XIT                                                              
         EJECT                                                                  
*  STAR ROUTINES                                                                
ALLSTARS NTR1                                                                   
         ZIC   R3,NUMSTAT                                                       
         MVC   P(11),STARS                                                      
         LA    R2,P+11                                                          
         SPACE 1                                                                
ALL20    MVC   0(20,R2),STARS                                                   
         LA    R2,20(R2)                                                        
         BCT   R3,ALL20                                                         
         B     XIT                                                              
         SPACE 3                                                                
SUMSTARS NTR1                                                                   
         LA    R2,10(R1)                                                        
         ZIC   R3,NUMSTAT                                                       
         MVI   0(R1),C'*'                                                       
         MVI   0(R2),C'*'                                                       
SUM20    MVI   20(R2),C'*'                                                      
         LA    R2,20(R2)                                                        
         BCT   R3,SUM20                                                         
         B     XIT                                                              
         SPACE 2                                                                
STARS    DC    20C'*'                                                           
         EJECT                                                                  
*              ROUTINE TO COUNT ACTIVE WEEKS *                                  
         SPACE 3                                                                
COUNTWK  NTR1                                                                   
         GOTO1 DEFINE,PARAS,=C'WEEK',DBLOCK,WORK                                
         MVI   NWKS,0                                                           
         ZIC   R1,WORK             GET WEEK BITS FROM DEFINE                    
         LA    R1,NUMTAB(R1)                                                    
         MVC   NWKS,0(R1)                                                       
         CLI   LAYSRCE,C'A'        ARBITRON                                     
         BNE   XIT                                                              
         CLC   1(2,R5),=X'5605'    BEFORE MAY86                                 
         BNL   XIT                 NEEDS                                        
         CLI   WORK,X'02'          FUDGE FOR ZEN                                
         BNE   XIT                                                              
         MVI   NWKS,3                                                           
         B     XIT                                                              
         SPACE 2                                                                
NUMTAB   DC    AL1(0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4)                             
         EJECT                                                                  
*  ROUTINE TO ROUND TIME TO NEAREST 1/2 HOUR                                    
         SPACE 2                                                                
RNDTIME  NTR1                                                                   
         LH    R1,STIM                                                          
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CH    R0,=H'15'                                                        
         BNE   *+6                                                              
         SR    R0,R0                                                            
         CH    R0,=H'45'                                                        
         BNE   *+8                                                              
         LA    R0,30                                                            
         MH    R1,=H'100'                                                       
         AR    R1,R0                                                            
         STH   R1,STIM                                                          
         SPACE 1                                                                
         LH    R1,ETIM                                                          
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CH    R0,=H'15'                                                        
         BNE   *+8                                                              
         LA    R0,30                                                            
         CH    R0,=H'45'                                                        
         BNE   *+8                                                              
         LA    R0,100                                                           
         MH    R1,=H'100'                                                       
         AR    R1,R0                                                            
         STH   R1,ETIM                                                          
         B     XIT                                                              
         EJECT                                                                  
* CONVERT START TIME (STIM) FROM BINARY MILITARY TO QUARTER HOURS,              
* GET NEXT HALF HOUR, AND CONVERT BACK TO BINARY MILITARY                       
* STORE NEXT HALF HOUR IN HALFHR                                                
         SPACE 2                                                                
ADDHLF   NTR1                                                                   
         MVC   HALF,STIM           START TIME                                   
         BAS   RE,TOQTRHR          CONVERT TO QTR HOUR                          
         MVC   STIMQ,BYTE          STORE START QTR HOUR                         
         ZIC   R3,STIMQ                                                         
         SPACE 1                                                                
         LA    R3,2(R3)            GET TO NEXT QTR HOUR                         
         STC   R3,HALFHRQ          AND SAVE IT                                  
         SPACE 1                                                                
         MVC   BYTE,HALFHRQ        HALF HOUR                                    
         BAS   RE,TOMILIT          CONVERT TO MILITARY                          
         MVC   HALFHR,HALF         AND SAVE IT                                  
         B     XIT                                                              
         SPACE 2                                                                
*  CONVERT TIME IN 'HALF' FROM MILITARY TO QUARTER HOURS                        
         SPACE 1                                                                
TOQTRHR  NTR1                                                                   
         LH    R1,HALF                                                          
         CH    R1,=H'600'                                                       
         BNL   *+8                                                              
         LA    R1,2400(R1)                                                      
         SH    R1,=H'600'                                                       
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         SLL   R1,2                HRS IN R1                                    
         LR    R3,R1               1/4S IN R3                                   
         SRDA  R0,32                                                            
         D     R0,=F'15'                                                        
         AR    R3,R1               R3 HAS TOTAL 1/4 HRS                         
         STC   R3,BYTE                                                          
         B     XIT                                                              
         SPACE 2                                                                
* CONVERT TIME IN 'BYTE' FROM QTR HOUR TO BINARY MILITARY TIME                  
         SPACE 1                                                                
TOMILIT  NTR1                                                                   
         SR    R0,R0                                                            
         ZIC   R1,BYTE                                                          
         D     R0,=F'4'                                                         
         MH    R1,=H'100'          HRS IN R1                                    
         AH    R1,=H'600'                                                       
         MH    R0,=H'15'           MINUTES IN R0                                
         AR    R1,R0                                                            
         C     R1,=F'2400'                                                      
         BNH   *+8                                                              
         S     R1,=F'2400'                                                      
         STH   R1,HALF                                                          
         B     XIT                                                              
         EJECT                                                                  
* HOOK ROUTINE FOR HEADLINE DETAILS                                             
*                                                                               
HOOK     NTR1                                                                   
         SPACE 1                                                                
         MVC   H3+7(3),LAYSRCE                                                  
         TM    PRINTOPT,X'40'      INDEX BOOK                                   
         BZ    HOOK5                                                            
         MVC   H3+75(12),=C'INDEX BOOK-'                                        
         MVC   H3+86(8),LAYNDX                                                  
         SPACE 1                                                                
HOOK5    TM    PRINTOPT,X'80'      1 BOOK                                       
         BZ    HOOK10                                                           
         MVC   H3+15(5),=C'BOOK-'  PRINTS IN HEADLINES                          
         MVC   H3+20(8),LAYBOOK                                                 
         B     HOOK20                                                           
*                                                                               
HOOK10   MVC   H6+1(4),=C'BOOK'   OTHERWISE, PRINTS IN BODY                     
         MVI   H5+9,C'/'                                                        
         SPACE 1                                                                
HOOK20   MVC   H3+29(5),=C'(TPT)'                                               
         SPACE 1                                                                
         LA    R2,MKTSV                                                         
         LA    R3,H5+11                                                         
         ZIC   R5,NUMSTAT                                                       
HOOK25   MVC   0(19,R3),0(R2)      PRINT MARKET NAME                            
         LA    R2,29(R2)                                                        
         LA    R3,20(R3)                                                        
         BCT   R5,HOOK25                                                        
         SPACE 1                                                                
         LA    R2,STATS                                                         
         LA    R3,H6+12                                                         
         ZIC   R5,NUMSTAT                                                       
HOOK30   MVC   0(16,R3),DASHES     PRINT STATION                                
         MVC   5(4,R3),0(R2)                                                    
         CLI   4(R2),C'T'          PRINT BAND (EXCEPT TV) OR SATELLITE          
         BE    HOOK40                                                           
         MVI   9(R3),C'-'                                                       
         MVC   10(1,R3),4(R2)                                                   
         SPACE 1                                                                
HOOK40   LA    R2,5(R2)                                                         
         LA    R3,20(R3)                                                        
         BCT   R5,HOOK30                                                        
*                                                                               
         LA    R3,H7+12            POINT WHERE 1ST DEMO PRINTS                  
         MVC   BYTEB,NUMSTAT                                                    
         LA    R2,DEMOS                                                         
         MVC   BYTEA,NDEMOS                                                     
HOOK60   LA    R5,2                                                             
         LR    R6,R3                                                            
HOOK70   CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         B     *+8                                                              
         MVI   1(R2),C'I'                                                       
         GOTO1 DEMOCON,PARAS,(0,(R2)),(2,WORK),(0,DBLOCK)                       
         CLI   1(R2),C'I'          AND CHANGE BACK FOR NEXT ROUND               
         B     *+8                                                              
         MVI   1(R2),C'T'                                                       
         SPACE 1                                                                
HOOK80   MVC   0(7,R6),WORK                                                     
         LA    R6,20(R6)                                                        
         ZIC   R1,BYTEB            NUMBER OF STATIONS                           
         BCTR  R1,0                                                             
         STC   R1,BYTEB                                                         
         CLI   BYTEB,0             ANY MORE                                     
         BNE   HOOK80                                                           
         ZIC   R1,BYTEA            NUMBER OF DEMOS                              
         BCTR  R1,0                                                             
         STC   R1,BYTEA                                                         
         CLI   BYTEA,0                                                          
         BE    HOOKX                                                            
         SPACE 1                                                                
         LA    R2,3(R2)                                                         
         MVC   BYTEB,NUMSTAT       REFRESH NUM. OF STATIONS                     
         LA    R6,9(R3)            POINT TO NEXT POSITION                       
         BCT   R5,HOOK70           DO NEXT DEMO                                 
         LA    R3,132(R3)          NEXT LINE                                    
         B     HOOK60                                                           
         SPACE 2                                                                
HOOKX    B     XIT                                                              
         SPACE 2                                                                
DASHES   DC    40C'-'                                                           
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
ERREND   GOTO1 VGETERR                                                          
         SPACE 2                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         B     XIT                                                              
         SPACE 2                                                                
GETEL    LR    R0,RE                                                            
         GOTO1 HELLO,PARAS,(C'G',SYSFIL),(ELCODE,(R4)),0,0                      
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         SPACE 2                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**  MY OWN ERROR MESSAGES                                                       
NOFOUND  DC    C'* ERROR * RECORD NOT FOUND - '                                 
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
*                                                                               
         SSPEC H1,1,C'MEDIA     SPOT T.V.'                                      
         SSPEC H1,49,C'LAYOUT REPORT'                                           
         SSPEC H1,77,AGYNAME                                                    
*                                                                               
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,49,13C'-'                                                     
         SSPEC H2,77,AGYADD                                                     
*                                                                               
         SSPEC H3,1,C'SOURCE-'                                                  
         SSPEC H3,77,RUN                                                        
         SSPEC H3,103,PAGE                                                      
*                                                                               
         SSPEC H5,02,C'DAY/TIME'                                                
         DC    X'00'                                                            
         EJECT                                                                  
*    DSECT TO COVER RECORDS IN BUFF                                             
BUFFD    DSECT                                                                  
BUFFREC  DS    0CL220                                                           
BUFFBK   DS    CL4                                                              
BUFFPROG DS    6CL20        16 CHAR PROG NAME, 4 CHAR WEEK INDICATOR            
BUFFDEM  DS    6CL16               8 DEMOS, 2 BYTES EACH                        
         SPACE 2                                                                
*   BUFF+2200 IS INDEX BOOK AREA                                                
*   BUFF+2204 IS INDEX BOOK PROGRAM NAMES                                       
*   BUFF+2324 IS INDEX BOOK DEMOS                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPRESWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER SCREEN                                                         
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESD4D                                                       
         EJECT                                                                  
*                                                                               
SPDEMLKD DSECT                                                                  
       ++INCLUDE SPDEMLK                                                        
         EJECT                                                                  
* SAVE AREA VALUES                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
NWKS     DS    CL1                 NUMBER OF WEEKS                              
SVWKS    DS    CL1                 NUMBER OF WEEKS FROM PREV PROGRAM            
SVWORK   DS    CL1                 ACTIVE WEEK BITS FROM PREV PROGRAM           
PRINTOPT DS    XL1                 X'80'  1 BOOK                                
*                                  X'40'  INDEX BOOK                            
*                                  X'20'  ROUNDED DEMOS                         
*                                  X'10'  RECORDS TO PRINT                      
*                                  X'08'  SHOW WEEKS                            
*                                  X'04'  DONE INDEX BOOK                       
HALFHR   DS    H                HALF HOUR AFTER START TIME (MILITARY)           
HALFHRQ  DS    X                   HALF HOUR (QTR HRS)                          
STIMQ    DS    X                   START TIME (QTR HRS)                         
ETIMQ    DS    X                   END TIME (QTR HRS)                           
NUMSTAT  DS    CL1                 NUMBER OF STATION                            
SVDAYTM  DS    F                   SAVE PLACE IN DAY/TIME LIST                  
SAVER3   DS    F                   SAVE R3                                      
SAVER6   DS    F                   SAVE R6                                      
SVDAY    DS    X                   SAVE DAY                                     
FIRST    DS    CL1                                                              
BYTEA    DS    X                                                                
BYTEB    DS    X                                                                
ONEWEEK  DS    X                   X'08' - USE WEEK 1                           
*                                  X'04' - USE WEEK 2                           
*                                  X'02' - USE WEEK 3                           
*                                  X'01' - USE WEEK 4                           
*                                                                               
TWOWEEK  DS    X                   X'0C' - USE 1ST 2 WEEKS                      
*                                  X'03' - USE 2ND 2 WEEKS                      
WANT     DS    C                                                                
GOTPROG  DS    C                   Y=HAVE A PROGRAM                             
*                                  D=HAVE THE PROGRAM I'M LOOKING FOR           
SVACTRMK DS    XL16                RATING SERVICE MARKETS (2 BYTES)             
PROGTMQ  DS    XL48                PROGRAM END QTR HR FOR EACH BK/STN           
*                                  6 STATIONS WITH 8 BOOKS EACH                 
SVPRGTMQ DS    F                   PROGTMQ POINTER                              
MKTSV    DS    CL144                                                            
SAVBK    DS    CL2                                                              
SVDEMOS  DS    CL60                                                             
MYDBLOCK DS    CL50                                                             
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
*                                                                               
NOW      EQU   X'40'                                                            
SOON     EQU   X'20'                                                            
OV       EQU   X'10'                                                            
STAHDRD  DSECT                                                                  
*SPGENSTA                                                                       
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
MKTHDRD  DSECT                                                                  
*SPGENMKT                                                                       
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
* DDCOMFACS/FAFACTS                                                             
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPRES04   11/12/03'                                      
         END                                                                    
