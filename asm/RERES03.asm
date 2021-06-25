*          DATA SET RERES03    AT LEVEL 034 AS OF 06/17/08                      
*PHASE T81903A,*                                                                
         TITLE 'T81903 - RERES03 - RESEARCH SEARCH REPORT'                      
*                                                                               
**********************************************************************          
*                                                                    *          
*  RERES03 - T81903 -- RESEARCH SEARCH REPORT                        *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  JAN10/90 (MRR) --- FORCE THIS REPORT TO SOON IFF STATIONS TIMES   *          
*                      DAYPARTS IS GREATER THAN 16                   *          
*                                                                    *          
*  APR12/90 (MRR) --- CALL FIXPAV IN DEMAND HOOK                     *          
*                                                                    *          
*  APR25/90 (MRR) --- ONLY CALL FIXPAV WHEN DECIMAL OPTION IS SET    *          
*                      FOR ROUNDED NUMBERS                           *          
*                                                                    *          
*  SEP25/90 (MRR) --- >FIXPAV IS IUNDEM                              *          
*                     >CALL IUNDEM FOR EVERY DEMO RECORD             *          
*                     >REMOVE 1-DEC/ROUNDED OPTION, OUTPUT IS 1-DEC  *          
*                     >CHANGE HEADER                                 *          
*                     >INCREASE COL WIDTH BY 1, #DEMS 6>5 OR 5>4     *          
*                                                                    *          
*  NOV15/90 (MRR) --- >FIX AUTO NOW TO SOON DUE TO HEADLINE CHANGE   *          
*                       FOR NOV/90 1-DECIMAL RELEASE                 *          
*                                                                    *          
*  JAN02/01 (FJD) --- >FIX UNIVERSE PRECISION                        *          
*                                                                    *          
**********************************************************************          
*                                                                               
T81903   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1903**,RA,RR=R2                                              
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
         LH    RF,=Y(BUFF-SYSD)                                                 
         LA    RF,SYSD(RF)         A(START OF PRINT BUFFER                      
         ST    RF,SBUFF                                                         
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
         XC    SYSSPARE(SAVELN),SYSSPARE   INIT SAVEAREA                        
         MVC   STAMP,=CL8'T81903'  STAMP SAVEAREA                               
         LA    R2,SERSRCEH         VALIDATE SOURCE                              
         GOTO1 VVALSRC                                                          
         CLI   SVSOURCE,C'S'                                                    
         BNE   *+14                                                             
         MVC   CONHEAD(L'TPSRC),TPSRC                                           
         B     MYEND                                                            
         SPACE 1                                                                
         LA    R2,SERBOOKH         VALIDATE BOOK                                
         MVI   MAX,10              DUMMY SO I CAN DO ERROR MSG                  
         GOTO1 VVALBOOK                                                         
         CLI   ACTUAL,1            REALLY ONLY 1 BOOK ALLOWED                   
         BNH   VKEY10                                                           
         MVC   CONHEAD(L'MANYBKS),MANYBKS    TOO MANY BOOKS                     
         B     MYEND                                                            
         SPACE 1                                                                
VKEY10   LA    R2,SERDEMOH         VALIDATE DEMOS                               
         MVI   MAX,20              DUMMY SO I CAN DO ERROR MSG                  
         MVI   NFLDS,1                                                          
         GOTO1 VVALDEM                                                          
         MVC   NUMDEMS(1),ACTUAL                                                
         LA    R3,DEMOS                                                         
         MVC   DEMMOD,1(R3)        SAVE 1ST DEMO MODIFIER                       
         CLI   ACTUAL,1                                                         
         BNH   *+8                                                              
         OI    PRINTOPT,X'10'      MORE THAN 1 DEMO                             
         CLI   ACTUAL,5            REALLY ALLOW 5 DEMOS                         
         BNH   VKEY20                                                           
         MVC   CONHEAD(L'MANYDEM5),MANYDEM5                                     
         B     MYEND                                                            
         SPACE 1                                                                
VKEY20   LA    R2,SERSTATH         VALIDATE STATION                             
         LA    R4,STATSV                                                        
         XC    STATSV,STATSV                                                    
         LA    R5,MKTSV                                                         
         XC    MKTSV(29),MKTSV     ONLY SAVE MAIN STATION MKT                   
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
         GOTO1 VVALSTA                                                          
         MVC   0(5,R4),ACTSTAT                                                  
         GOTO1 VVALMKT                                                          
         MVC   0(29,R5),WORK+8     MARKET NAME                                  
         SPACE 1                                                                
         LA    R2,SERPUREH         VALIDATE PURE NUMBER                         
         GOTO1 VVALPURE            PUTS PURE NO. IN DBSELPUR                    
         SPACE 1                                                                
         LA    R2,SERDAYH          VALIDATE DAY/DETAIL FIELDS                   
         GOTO1 VVALDYTM,PARAS,8    8 DAY/DETAIL FIELDS                          
         SPACE 1                                                                
         LA    R2,SERCOMPH         VALIDATE COMP STATIONS                       
         LA    R3,8                                                             
         LA    R4,STATSV+5                                                      
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
VKEY40   GOTO1 VVALSTA                                                          
         MVC   0(5,R4),ACTSTAT                                                  
         LA    R4,5(R4)                                                         
         BAS   RE,BUMP                                                          
         BCT   R3,*+8                                                           
         B     VKEY50                                                           
         CLI   5(R2),0             ANOTHER STATION?                             
         BNE   VKEY40                                                           
         SPACE 1                                                                
VKEY50   LA    R2,SERWKSH          POINT TO WEEKS                               
         XC    WEEKS,WEEKS         INIT WEEKS                                   
         CLI   5(R2),0             INPUT IS OPTIONAL                            
         BE    VKEY70                                                           
         OI    PRINTOPT,X'80'      WEEKS FILTER USED                            
         MVI   WEEKS+1,C' '                                                     
         SPACE 1                                                                
         MVI   ERROR,2             INVALID INPUT FIELD                          
         CLI   8(R2),C'1'          VALID ENTRIES ARE 1,2,3, OR 4                
         BL    ERREND                                                           
         CLI   8(R2),C'4'                                                       
         BH    ERREND                                                           
         SPACE 1                                                                
         ZIC   R1,8(R2)                                                         
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         STC   R1,WEEKS                                                         
         SPACE 1                                                                
         CLI   5(R2),1             FOLLOWED BY +, - OR BLANK                    
         BE    VKEY70                                                           
         CLI   9(R2),C'+'                                                       
         BE    VKEY60                                                           
         CLI   9(R2),C'-'                                                       
         BNE   ERREND                                                           
         SPACE 1                                                                
VKEY60   MVC   WEEKS+1(1),9(R2)                                                 
         SPACE 1                                                                
VKEY70   LA    R2,SERRANKH         POINT TO RANK MAX                            
         MVC   BUFFMAX,=H'100'     ASSUME MAX OF 100                            
         MVC   BUFFWRST,=X'FFFF'   INITIALIZE BUFFWRST                          
         SPACE 1                                                                
         CLI   5(R2),0             INPUT IS OPTIONAL                            
         BE    VKEY80                                                           
         SPACE 1                                                                
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BO    *+12                                                             
         MVI   ERROR,3                                                          
         B     ERREND                                                           
         SPACE 1                                                                
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         STH   R0,BUFFMAX                                                       
         SPACE 1                                                                
VKEY80   LA    R2,SEROPT1H         POINT TO OPTION 1 FIELD                      
         MVI   ERROR,2                                                          
         CLI   5(R2),0             INPUT IS OPTIONAL                            
         BE    VKEY90                                                           
         CLI   8(R2),C'N'          N IS VALID                                   
         BE    VKEY90                                                           
         CLI   8(R2),C'Y'          OR Y IS VALID                                
         BNE   ERREND                                                           
         OI    PRINTOPT,X'40'      NUMERIC OPTION ON                            
         SPACE 1                                                                
         CLI   NUMDEMS,4           THEN ONLY ALLOW 4 DEMOS MAX                  
         BNH   VKEY90                                                           
         MVC   CONHEAD(L'MANYDEM4),MANYDEM4                                     
         B     MYEND                                                            
         SPACE 1                                                                
VKEY90   LA    R2,SEROPT2H         POINT TO OPTION 2 FIELD                      
         MVI   ERROR,2                                                          
         CLI   5(R2),0             INPUT IS OPTIONAL                            
         BE    VKEY100                                                          
         CLI   8(R2),C'N'          N IS VALID                                   
         BE    VKEY100                                                          
         CLI   8(R2),C'Y'          OR Y IS VALID                                
         BNE   ERREND                                                           
         OI    PRINTOPT,X'20'      PERCENT OPTION ON                            
         SPACE 1                                                                
         CLI   NUMDEMS,4           THEN ONLY ALLOW 4 DEMOS MAX                  
         BNH   VKEY100                                                          
         MVC   CONHEAD(L'MANYDEM4),MANYDEM4                                     
         B     MYEND                                                            
         SPACE 1                                                                
VKEY100  EQU   *                                                                
         CLC   CONWHEN(4),=C'SOON' NO TESTING IF ALREADY SOON/OV/DDS            
         BE    VKEY299                                                          
         CLC   CONWHEN(2),=C'OV'                                                
         BE    VKEY299                                                          
         CLC   CONWHEN(3),=C'DDS'                                               
         BE    VKEY299                                                          
*                                                                               
         CLI   DDS,C'Y'                                                         
         BE    VKEY299                                                          
*                                                                               
         LA    R2,SERDAYH          COUNT DAY/TIME(S)                            
         SR    R3,R3                                                            
         LA    R5,8                LOOP FOR UP TO 8 DAY/TIME PAIRS              
VKEY210  EQU   *                                                                
         CLI   5(R2),0             INPUT?                                       
         BE    VKEY220             NO                                           
         LA    R3,1(R3)                                                         
         BAS   RE,BUMP             NOW POINTS TO TIME                           
         BAS   RE,BUMP             NOW POINTS TO DAY                            
         BCT   R5,VKEY210          LOOP                                         
VKEY220  EQU   *                                                                
         LA    R2,SERCOMPH         COUNT STATIONS                               
         SR    R4,R4                                                            
         LA    R5,8                LOOP FOR UP TO 8 STATIONS                    
VKEY230  EQU   *                                                                
         CLI   5(R2),0             INPUT?                                       
         BE    VKEY240                                                          
         LA    R4,1(R4)                                                         
         BAS   RE,BUMP             POINT TO THE NEXT STATION                    
         BCT   R5,VKEY230                                                       
VKEY240  EQU   *                                                                
         SR    R2,R2                                                            
         MR    R2,R4                                                            
         C     R3,=F'16'                                                        
         BNH   VKEY299                                                          
         LA    R2,CONWHENH         PUT CURSOR AT PRINT                          
         CLC   CONWHEN(3),=C'NOW'                                               
         BNE   VKEY290                                                          
         MVC   DUB(3),CONWHEN+4                                                 
         MVC   CONWHEN(5),=C'SOON,'                                             
         MVC   CONWHEN+5(3),DUB          DUB HELD NAME                          
         MVI   4(R2),X'84'                                                      
         MVI   5(R2),8                                                          
         OI    6(R2),X'81'                                                      
         MVI   7(R2),8                                                          
         MVI   WHEN,X'20'                                                       
         MVI   TWAWHEN,2                                                        
         B     VKEY299                                                          
VKEY290  EQU   *                                                                
         MVC   CONHEAD(L'MUSTSOON),MUSTSOON                                     
         B     MYEND                                                            
VKEY299  EQU   *                                                                
         SPACE 1                                                                
VKEYX    B     XIT                                                              
         EJECT                                                                  
* PRINT REPORT                                                                  
* R4 POINTS AT DBLOCKA1 - MAIN DBLOCK AREA                                      
*                                                                               
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         CLC   STAMP,=CL8'T81903'  STOP IF STORAGE NOT STAMPED                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RCSUBPRG,0                                                       
         TM    PRINTOPT,X'10'      MORE THAN 1 DEMO                             
         BO    *+8                                                              
         MVI   RCSUBPRG,1                                                       
         XC    BUFFLINE,BUFFLINE   COUNT NUMBER OF LINES ON PAGE                
         EJECT                                                                  
*   CONTROL I/O                                                                 
*  BUILD REST OF DBLOCK - (DBSELPUR ALREADY HAS PURE NO.)                       
         SPACE 1                                                                
         LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
         MVI   DBFUNCT,DBGETPUR                                                 
         MVC   DBSELSRC,SVSOURCE                                                
         MVC   DBSELSTA,STATSV                                                  
         MVC   DBSELBK,BOOK+1                                                   
*                                                                               
*  SPECIAL PATCH FOR WDVM (PRE 7/86)-WUSA CALL LETTER SWITCH                    
         CLC   DBSELSTA,=C'WUSAT'                                               
         BNE   PREP5                                                            
         CLC   DBSELBK,=X'5606'                                                 
         BH    PREP5                                                            
         MVC   DBSELSTA,=C'WDVMT'                                               
*                                                                               
PREP5    MVC   DBBTYPE,BOOK+3      BOOK TYPE                                    
         MVI   DBBEST,C'A'                                                      
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
*                                                                               
         GOTO1 DEMAND,DMCB,DBLOCKD,FILL,0  GET MAIN PROGRAM                     
         L     R2,SBUFF                                                         
         OC    0(4,R2),0(R2)       MAKE SURE WE GOT SOMETHING                   
         BNZ   PREP10                                                           
         LA    R2,SERPUREH                                                      
         MVI   ERROR,53            RECORD NOT FOUND                             
         B     ERREND                                                           
         SPACE 2                                                                
PREP10   XC    DBSELPUR,DBSELPUR      NOW DO THE OTHER STATIONS                 
         MVI   DBFUNCT,DBGETDEM                                                 
         LA    R3,8                UP TO 8 COMPETING STATIONS                   
         LA    R2,STATSV+5    LIST OF COMP STATIONS IN DBSELSTA FMT             
         SPACE 1                                                                
PREP20   OC    0(5,R2),0(R2)       ANOTHER STATION                              
         BNZ   PREP40                                                           
         SPACE 1                                                                
PREP30   OC    BUFFLINE(2),BUFFLINE   ANY LINES                                 
         BZ    XIT                                                              
         BAS   RE,RANK             RANK THEM                                    
         BAS   RE,SPLAT            AND PRINT REPORT                             
         B     XIT                                                              
         SPACE 2                                                                
PREP40   MVC   DBSELSTA,0(R2)      COMP STATION                                 
*                                                                               
*  SPECIAL PATCH FOR WDVM (PRE 7/86)-WUSA CALL LETTER SWITCH                    
         CLC   DBSELSTA,=C'WUSAT'                                               
         BNE   PREP45                                                           
         CLC   DBSELBK,=X'5606'                                                 
         BH    PREP45                                                           
         MVC   DBSELSTA,=C'WDVMT'                                               
*                                                                               
         SPACE 1                                                                
PREP45   LA    R6,DAYTMLST         POINT TO DAY/TIME LIST                       
         ZIC   R5,DAYTIMES         NO. OF ENTRIES IN LIST                       
PREP50   ZIC   R1,0(R6)            GET DAY                                      
         SRL   R1,4                MOVE INTO INDEX POSITION                     
         LA    R1,DAYBITS(R1)                                                   
         MVC   DBSELDAY,0(R1)                                                   
         CLI   DBSELDAY,3          IS IS SA-SU                                  
         BNE   *+8                                                              
         MVI   DBDAYOPT,C'Y'       INDICATES SA-SU AVE LINE                     
         MVC   DBSELTIM,1(R6)                                                   
         SPACE 1                                                                
         GOTO1 DEMAND,DMCB,DBLOCKD,FILL,0                                       
         CLI   DBERROR,X'10'       RECORD NOT FOUND                             
         BNE   PREP60                                                           
         MVC   CONHEAD(L'NOFOUND),NOFOUND                                       
         MVC   CONHEAD+L'NOFOUND(5),0(R2)   IDENTIFY STATION                    
         LA    R2,CONHEAD+L'NOFOUND+5                                           
         MVI   0(R2),C'/'                                                       
         MVC   1(9,R2),SERBOOK     AND BOOK                                     
         LA    R2,SERCOMPH         POINT CURSOR AT FIRST STATION                
         B     MYEND                                                            
*                                                                               
PREP60   OC    DBDIVSOR,DBDIVSOR   TEST FOR ANY RECORDS                         
         BZ    *+14                NO                                           
         CLI   DBERROR,X'80'       TEST FOR EOF                                 
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG                              
         SPACE 1                                                                
         LA    R6,5(R6)            NEXT DAY/TIME                                
         BCT   R5,PREP50                                                        
         LA    R2,5(R2)            NEXT STATION                                 
         BCT   R3,PREP20                                                        
         B     PREP30                                                           
         SPACE 3                                                                
DAYBITS  DC    X'7C402010080402017F03'                                          
*                                                                               
* BITS REPRESENT M-F,MON,TUE,WED,THUR,FRI,SAT,SUN,M-SU,SA-SU                    
         EJECT                                                                  
*  PROCESS A RECORD                                                             
         SPACE 1                                                                
*              ROUTINE TO BUILD A BUFF RECORD                                   
         SPACE 3                                                                
FILL     NTR1                                                                   
*                                                                               
         GOTO1 VIUNDEM,DMCB,DBLOCKD                                             
*                                                                               
         XC    WORK,WORK           BUILD A LINE IN WORK                         
         LA    R6,WORK                                                          
         USING BUFFD,R6                                                         
         SPACE 1                                                                
         OC    DEMOMAX,DEMOMAX     HAVE WE DONE MAIN STATION YET                
         BZ    FILL40              NO, SKIP FILTERS                             
         SPACE 1                                                                
*  FOR COMPETITIVE STATIONS, SEE IF IT MEETS WEEKS CRITERIA                     
         SPACE 1                                                                
         CLI   WEEKS,0                                                          
         BE    FILL40              NO WEEKS CRITERIA                            
         GOTO1 DEFINE,DMCB,=C'WEEK',DBLOCKD,OUTAREA                             
         MVI   NWKS,0                                                           
         ZIC   R1,OUTAREA                                                       
         LA    R1,NUMTAB(R1)                                                    
         MVC   NWKS,0(R1)                                                       
         CLI   SERSRCE,C'A'        FUDGE FOR ZEN IF ARBITRON                    
         BNE   FILL10                                                           
         CLC   BOOK+1(2),=X'5605'  AND BEFORE MAY/86                            
         BNL   FILL10                                                           
         CLI   OUTAREA,X'02'                                                    
         BNE   FILL10                                                           
         MVI   NWKS,3                                                           
         SPACE 1                                                                
FILL10   CLI   WEEKS+1,C'+'        X WEEKS OR MORE                              
         BE    FILL20                                                           
         CLI   WEEKS+1,C'-'        X WEEKS OR LESS                              
         BE    FILL30                                                           
         SPACE 1                                                                
         CLC   NWKS(1),WEEKS       X WEEKS ONLY                                 
         BNE   XIT                                                              
         B     FILL40                                                           
         SPACE 1                                                                
FILL20   CLC   NWKS(1),WEEKS                                                    
         BL    XIT                                                              
         B     FILL40                                                           
         SPACE 1                                                                
FILL30   CLC   NWKS(1),WEEKS                                                    
         BH    XIT                                                              
         B     FILL40                                                           
         SPACE 2                                                                
NUMTAB   DC    AL1(0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4)                             
         SPACE 2                                                                
FILL40   MVC   BUFFSTAT,DBSELSTA   STATION                                      
*                                                                               
*  SPECIAL PATCH FOR WDVM (PRE 7/86)-WUSA CALL LETTER SWITCH                    
         CLC   DBSELSTA,=C'WDVMT'                                               
         BNE   *+10                                                             
         MVC   BUFFSTAT,=C'WUSAT'                                               
*                                                                               
         GOTO1 DEFINE,DMCB,=C'PURE',DBLOCKD,OUTAREA                             
         MVC   BUFFINV(2),OUTAREA  PURE NUMBER-FOR DAY/TIME                     
         MVC   BUFFPURE(4),OUTAREA+3 PURE NUMBER                                
         GOTO1 (RF),(R1),=C'PROGRAM',DBLOCKD,OUTAREA                            
         MVC   BUFFPROG,OUTAREA    PROGRAM NAME                                 
         SPACE 1                                                                
         ZIC   R1,NUMDEMS                                                       
         MH    R1,=H'3'                                                         
         LA    R1,DEMOS(R1)                                                     
         MVI   0(R1),X'FF'                                                      
         LA    R2,DEMOS            POINT R2 AT DEMO LIST                        
         LA    R3,BUFFDEM1                                                      
         MVC   SAVBK,DBACTBK       PROTECT DBACTBK                              
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCKD,BLOCK1                           
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DBACTBK,SAVBK       RESTORE IT                                   
         LA    R5,BLOCK1           NOW POINT R5 TO VALUES                       
         ZIC   R4,NUMDEMS                                                       
         SPACE 2                                                                
FILL50   EQU   *                                                                
         MVC   0(2,R3),2(R5)                                                    
         SPACE 1                                                                
FILL75   OC    DEMOMAX,DEMOMAX     SAVE PRIMARY DEMO FOR MAIN PROG.             
         BNZ   FILL80                                                           
         MVC   DEMOMAX,0(R3)                                                    
         MVC   SVPROG,BUFFPROG                                                  
         MVI   BUFFFDG,X'FF'     FUDGE TO FORCE PRIMARY DEMO TO BE 1ST          
         SPACE 2                                                                
FILL80   LA    R2,3(R2)            NEXT DEMO                                    
         LA    R3,3(R3)            NEXT BUFF AREA                               
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         BCT   R4,FILL50                                                        
         EJECT                                                                  
*              NOW SEE IF IT QUALIFIES                                          
         SPACE 3                                                                
         L     R2,SBUFF                                                         
         CLC   0(26,R2),WORK       IF THIS ITEM IS REPEAT OF MAIN PROG          
         BE    XIT                 DON'T INCLUDE IT AGAIN                       
         SPACE 1                                                                
         CLC   BUFFDEM1,DEMOMAX    CAN'T BE MORE THAN MAX                       
         BH    XIT                                                              
         CLC   BUFFLINE,BUFFMAX    IS BUFF FULL                                 
         BE    FILL100                                                          
         CLC   BUFFDEM1,BUFFWRST   NO                                           
         BH    *+10                                                             
         MVC   BUFFWRST,BUFFDEM1   UPDATE WORST SO FAR                          
         LH    R1,BUFFLINE                                                      
         LA    R1,1(R1)                                                         
         STH   R1,BUFFLINE         UPDATE LINES SO FAR                          
         B     FILL110                                                          
         SPACE 2                                                                
FILL100  CLC   BUFFDEM1,BUFFWRST   TABLE IS FULL                                
         BL    XIT                 CHECK IF THIS IS WORSE THAN WORST            
         SPACE 2                                                                
FILL110  LH    R2,BUFFLINE         FIND N'TH POSITION                           
         BCTR  R2,0                                                             
         MH    R2,=H'46'                                                        
         L     RF,SBUFF                                                         
         LA    R2,0(R2,RF)                                                      
         MVC   0(46,R2),WORK       AND PUT IN THIS ITEM                         
         CLC   BUFFLINE,BUFFMAX                                                 
         BL    XIT                                                              
         LH    R2,BUFFLINE                                                      
         GOTO1 XSORT,PARAS,(1,SBUFF),(R2),46,2,28                               
         BCTR  R2,0                                                             
         MH    R2,=H'46'                                                        
         L     RF,SBUFF                                                         
         LA    R2,0(RF,R2)                                                      
         MVC   BUFFWRST,28(R2)                                                  
PREPX    B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*              ROUTINE TO RANK THE TABLE                                        
         SPACE 3                                                                
RANK     NTR1                                                                   
         USING BUFFD,R6                                                         
         L     R6,SBUFF                                                         
         ZIC   R3,NUMDEMS                                                       
         BCTR  R3,0                                                             
         MH    R3,=H'3'                                                         
         LA    R2,BUFFDEM1                                                      
         SR    R2,R6               DISPLACEMENT TO FIRST DEMO                   
         AR    R2,R3               DISPLACEMENT TO LAST                         
         LA    R3,DEMOS(R3)        R3=DEMO                                      
         LH    R5,BUFFLINE                                                      
         ZIC   R4,NUMDEMS                                                       
         TM    PRINTOPT,X'10'      MORE THAN 1 DEMO                             
         BZ    RANK40                                                           
         BCTR  R4,0                DO ALL EXCEPT PRIMARY DEMO                   
         SPACE 2                                                                
RANK20   GOTO1 XSORT,PARAS,(1,SBUFF),(R5),46,2,(R2)                             
         BAS   RE,RANK60                                                        
         SPACE 2                                                                
         SH    R2,=H'3'                                                         
         SH    R3,=H'3'                                                         
         BCT   R4,RANK20                                                        
*                                  DO PRIMARY DEMO ALONE                        
RANK40   GOTO1 XSORT,PARAS,(1,SBUFF),(R5),46,3,27                               
         BAS   RE,RANK60                                                        
         B     XIT                                                              
         SPACE 2                                                                
*                                  ROUTINE TO PUT IN RANK NUMBERS               
RANK60   NTR1                                                                   
         L     RF,SBUFF                                                         
         LA    R2,0(RF,R2)         R2=A(COLUMN)                                 
         LA    R3,1                R3=PLACE NUMBER                              
         LA    R4,1                R4=ACTUAL NUMBER                             
*                                  R5=N'LINES                                   
         SR    R1,R1               R1=CURRENT VALUE                             
         SPACE 2                                                                
RANK80   STC   R4,2(R2)                                                         
         CH    R1,0(R2)            IS THIS WORSE THAN PREVIOUS                  
         BE    RANK100                                                          
         LR    R3,R4               THEN SET PLACE TO ACTUAL                     
         LH    R1,0(R2)            AND SAVE THIS VALUE                          
         B     RANK120                                                          
         SPACE 2                                                                
RANK100  STC   R3,2(R2)            TIE - USE PREVIOUS PLACE                     
         SPACE 2                                                                
RANK120  LA    R2,46(R2)                                                        
         LA    R4,1(R4)                                                         
         BCT   R5,RANK80                                                        
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT A REPORT FROM BUFF                              
         SPACE 3                                                                
SPLAT    NTR1                                                                   
         L     R6,SBUFF                                                         
         USING BUFFD,R6                                                         
         LH    R3,BUFFLINE                                                      
         SPACE 2                                                                
         LA    R2,1                PRINT 1ST LINE IN OWN BLOCK                  
         B     *+8                                                              
SPLAT20  LA    R2,10               OTHERWISE, PRINT IN BLOCKS OF 10             
         CR    R3,R2               OR LESS                                      
         BH    *+6                                                              
         LR    R2,R3                                                            
         LA    R1,1(R2)                                                         
         STC   R1,ALLOWLIN         ALLOW FOR 1 MORE LINE                        
         SPACE 2                                                                
SPLAT40  MVI   SPACING,1                                                        
         BAS   RE,FORMAT                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         XC    0(46,R6),0(R6)                                                   
         BCTR  R3,0                                                             
         LA    R6,46(R6)                                                        
         MVI   ALLOWLIN,0                                                       
         BCT   R2,SPLAT40                                                       
         GOTO1 SPOOL,PARAS,(R8)                                                 
         CH    R3,=H'0'                                                         
         BNE   SPLAT20                                                          
         B     XIT                                                              
         SPACE 2                                                                
*                                  FILL A PRINT LINE                            
FORMAT   NTR1                                                                   
         LA    R3,P                                                             
         TM    PRINTOPT,X'10'      MORE THAN 1 DEMO                             
         BO    *+8                                                              
         LA    R3,23(R3)           NO, SO CENTER PRINT LINE                     
         MVC   0(4,R3),BUFFSTAT    STATION                                      
         CLI   BUFFSTAT+4,C'T'     ANY BAND (EXCEPT TV) OR STAELLITE            
         BE    FMT10                                                            
         MVI   4(R3),C'-'                                                       
         MVC   5(1,R3),BUFFSTAT+4  DISPLAY IT                                   
         SPACE 1                                                                
FMT10    MVC   9(4,R3),BUFFPURE    PURE NUMBER                                  
         MVC   15(16,R3),BUFFPROG  PROGRAM                                      
*                                                                               
         GOTO1 INVEDIT,PARAS,BUFFINV,WORK                                       
         MVC   32(3,R3),WORK       DAY                                          
         MVC   36(6,R3),WORK+3     TIME                                         
         SPACE 1                                                                
         LA    R3,53(R3)                                                        
         TM    PRINTOPT,X'40'      NUMERIC OPTION=                              
         BZ    FMT50                 MAIN PROGRAM PRIMARY DEMO MINUS            
         LH    R4,DEMOMAX             OTHER PROGRAM PRIMARY DEMO                
         LR    R5,R4                                                            
         SR    RE,RE                                                            
         ICM   RE,3,BUFFDEM1                                                    
         SR    R5,RE                                                            
         BZ    FMT40                                                            
         EDIT  (R5),(6,0(R3)),1                                                 
FMT40    EQU   *                                                                
         LA    R3,7(R3)                                                         
FMT50    EQU    *                                                               
         TM    PRINTOPT,X'20'      PERCENT OPTION =                             
         BZ    FMT70                 (MAIN PROGRAM PRIMARY DEMO MINUS           
         LH    R4,DEMOMAX                                                       
         LR    R5,R4                  OTHER PROGRAM PRIMARY DEMO)               
         SR    RE,RE                                                            
         ICM   RE,3,BUFFDEM1                                                    
         SR    R5,RE           DIVIDED BY OTHER PROGRAM PRIMARY DEMO            
         BZ    FMT60                                                            
         LTR   R4,R4                                                            
         BZ    FMT60                                                            
         OC    BUFFDEM1,BUFFDEM1                                                
         BZ    FMT60                                                            
         LR    R1,R5                                                            
         ICM   R2,3,BUFFDEM1                                                    
         M     R0,=F'2000'                                                      
         DR    R0,R2                                                            
         AH    R1,=H'1'                                                         
         SRL   R1,1                                                             
         EDIT  (R1),(5,0(R3)),1                                                 
         CH    R1,=H'9999'                                                      
         BL    *+10                                                             
         MVC   0(5,R3),=C'HIGH '                                                
FMT60    LA    R3,6(R3)                                                         
         SPACE 2                                                                
FMT70    LA    R2,BUFFDEM1         SET UP TO EDIT DEMOS                         
         LA    R4,DEMOS                                                         
         ZIC   R5,NUMDEMS                                                       
         SPACE 1                                                                
         LA    R3,P+43                                                          
         TM    PRINTOPT,X'10'      MORE THAN 1 DEMO                             
         BO    FMT80                                                            
         LA    R3,P+66                                                          
         SPACE 2                                                                
FMT80    EQU   *                                                                
         CLI   1(R4),C'U'              IF UNIVERSE,                             
         BE    FMT85                   THEN, NO DECIMAL                         
         EDIT  (2,0(R2)),(6,0(R3)),1   ELSE, SHOW DECIMAL                       
         B     FMT100                                                           
                                                                                
FMT85    EDIT  (2,0(R2)),(6,0(R3))                                              
                                                                                
FMT100   EQU   *                                                                
         EDIT  (1,2(R2)),(3,7(R3))                                              
         SPACE 2                                                                
         LA    R2,3(R2)                                                         
         LA    R4,3(R4)                                                         
         LA    R3,12(R3)                                                        
         SPACE 1                                                                
         TM    PRINTOPT,X'08'      X'08' = FIRST DEMO DONE                      
         BO    FMT110                                                           
         OI    PRINTOPT,X'08'      TURN ON INDICATOR                            
         TM    PRINTOPT,X'40'      NUMERIC OPTION                               
         BZ    *+8                                                              
         LA    R3,6(R3)                                                         
         TM    PRINTOPT,X'20'      PERCENT OPTION                               
         BZ    *+8                                                              
         LA    R3,6(R3)                                                         
FMT110   BCT   R5,FMT80                                                         
         NI    PRINTOPT,X'F7'      TURN OFF INDICATOR                           
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* HOOK ROUTINE FOR HEADLINE DETAILS                                             
*                                                                               
HOOK     NTR1                                                                   
         MVC   H4+112(3),SERSRCE    SOURCE                                      
         MVC   H4+116(9),SERBOOK    BOOK                                        
         MVC   H4+10(6),SERSTAT    STATION                                      
         MVC   H4+18(29),MKTSV     MARKET NAME                                  
         MVC   H5+10(16),SVPROG    PROGRAM NAME                                 
*                                                                               
*  IF WEEKS, SHOW IN HEADLINES                                                  
*                                                                               
         LA    R3,H5+98                                                         
         TM    PRINTOPT,X'80'      WEEKS ENTERED?                               
         BZ    HOOK10                                                           
         MVC   0(33,R3),=C'INCLUDES PROGRAMS AIRING    WEEKS'                   
         MVC   25(2,R3),WEEKS                                                   
         OI    25(R3),X'F0'                                                     
         CLC   25(2,R3),=C'1 '     IF 1 WEEK,                                   
         BNE   HOOK10                                                           
         MVC   27(6,R3),=C'WEEK  ' MAKE IT WEEK, NOT WEEKS                      
*                                                                               
HOOK10   LA    R3,H8+53                                                         
         LA    R5,H9+53                                                         
         TM    PRINTOPT,X'10'      MORE THAN 1 DEMO                             
         BO    HOOK20                                                           
         LA    R3,23(R3)                                                        
         LA    R5,23(R5)                                                        
HOOK20   TM    PRINTOPT,X'40'      NUMERIC OPTION                               
         BZ    HOOK40                                                           
         MVC   1(4,R3),=C'LEAD'                                                 
         MVC   0(5,R5),=C'-----'                                                
         LA    R3,6(R3)                                                         
         LA    R5,6(R5)                                                         
*                                                                               
HOOK40   TM    PRINTOPT,X'20'      PERCENT OPTION                               
         BZ    HOOK50                                                           
         MVC   2(3,R3),=C'PCT'                                                  
         MVC   1(4,R5),=C'LEAD'                                                 
*                                                                               
HOOK50   LA    R3,H8+43                                                         
         LA    R5,H9+43                                                         
         TM    PRINTOPT,X'10'      MORE THAN 1 DEMO                             
         BO    HOOK55                                                           
         LA    R3,23(R3)                                                        
         LA    R5,23(R5)                                                        
*                                                                               
HOOK55   LA    R2,DEMOS                                                         
         ZIC   R6,NUMDEMS                                                       
HOOK60   CLI   1(R2),C'T'                                                       
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
         LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         GOTO1 DEMOCON,PARAS,(0,(R2)),(7,WORK),(0,DBLOCKD)                      
         DROP  R4                                                               
         MVC   1(7,R3),WORK                                                     
         MVC   0(5,R5),WORK+7                                                   
         MVC   7(3,R5),=C'RNK'                                                  
HOOK70   LA    R2,3(R2)                                                         
         LA    R3,12(R3)                                                        
         LA    R5,12(R5)                                                        
         TM    PRINTOPT,X'08'      1ST DEMO DONE                                
         BO    HOOK90                                                           
         OI    PRINTOPT,X'08'      TURN ON INDICATOR                            
         TM    PRINTOPT,X'40'      NUMERIC OPTION                               
         BZ    HOOK80                                                           
         LA    R3,6(R3)                                                         
         LA    R5,6(R5)                                                         
HOOK80   TM    PRINTOPT,X'20'      PCT OPTION                                   
         BZ    HOOK90                                                           
         LA    R3,6(R3)                                                         
         LA    R5,6(R5)                                                         
HOOK90   BCT   R6,HOOK60                                                        
         NI    PRINTOPT,X'F7'      TURN OFF INDICATOR                           
         SPACE 2                                                                
HOOKX    B     XIT                                                              
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
ERREND   GOTO1 VERRXIT                                                          
         SPACE 2                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         B     XIT                                                              
         SPACE 2                                                                
         GETEL (R6),34,ELCODE                                                   
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
INVTIM   DC    C'** ERROR ** INVALID TIME'                                      
INVDAY1  DC    C'* ERROR * INVALID DAY/DETAIL'                                  
MANYDEM4 DC    C'* ERROR * TOO MANY DEMOS - LIMIT IS 4'                         
MANYDEM5 DC    C'* ERROR * TOO MANY DEMOS - LIMIT IS 5'                         
MANYBKS  DC    C'* ERROR * TOO MANY BOOKS - LIMIT IS 1'                         
NOFOUND  DC    C'** ERROR ** RECORD NOT FOUND - '                               
TPSRC    DC    C'* ERROR * SRC IS ONLY VALID FOR TIME PERIOD'                   
MUSTSOON DC    C'* THIS REQUEST MUST BE DONE AS SOON *'                         
         EJECT                                                                  
*                                                                               
*        REPORT HEADLINE SPECS                                                  
*                                                                               
HEDSPECS DS    0H                                                               
         SPROG 0,1                                                              
         PSPEC H1,001,AGYNAME                                                   
         PSPEC H1,053,C'COMPETITIVE PROGRAM SEARCH'                             
         PSPEC H1,099,REPORT                                                    
         PSPEC H1,115,PAGE                                                      
         PSPEC H2,001,REQUESTOR                                                 
         PSPEC H2,053,26C'-'                                                    
         PSPEC H1,099,RUN                                                       
         PSPEC H4,001,C'STATION -'                                              
         PSPEC H4,099,C'SOURCE BOOK -'                                          
         PSPEC H5,001,C'PROGRAM -'                                              
         SPACE 1                                                                
         SPROG 0                                                                
         PSPEC H8,01,C'STATION  PURE      PROGRAM      DAY  TIME'               
         PSPEC H9,01,C'-------   NO.  ---------------  --- ------'              
         SPACE 1                                                                
         SPROG 1                                                                
         PSPEC H8,24,C'STATION  PURE      PROGRAM      DAY  TIME'               
         PSPEC H9,24,C'-------   NO.  ---------------  --- ------'              
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER RECORDS IN BUFF                                   
         SPACE 3                                                                
BUFFD    DSECT                                                                  
BUFFREC  DS    0CL46                                                            
BUFFSTAT DS    CL5                                                              
BUFFPURE DS    CL4                                                              
BUFFPROG DS    CL16                                                             
BUFFINV  DS    CL2                                                              
BUFFFDG  DS    CL1                 FUDGE FOR PRIMARY DEMO TO RANK 1ST           
BUFFDEM1 DS    CL2                                                              
BUFFRNK1 DS    CL1                                                              
BUFFDEM2 DS    CL2                                                              
BUFFRNK2 DS    CL1                                                              
BUFFDEM3 DS    CL2                                                              
BUFFRNK3 DS    CL1                                                              
BUFFDEM4 DS    CL2                                                              
BUFFRNK4 DS    CL1                                                              
BUFFDEM5 DS    CL2                                                              
BUFFRNK5 DS    CL1                                                              
BUFFDEM6 DS    CL2                                                              
BUFFRNK6 DS    CL1                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE RERESWRK                                                       
         EJECT                                                                  
* DSECT TO COVER SCREEN                                                         
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RERESF3D                                                       
         EJECT                                                                  
* SAVE AREA VALUES                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE            LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
NWKS     DS    CL1                 NUMBER OF WEEKS                              
PRINTOPT DS    XL1                 X'80'  WEEKS FILTER                          
*                                  X'40'  NUMERIC OPTION                        
*                                  X'20'  PERCENT OPTION                        
*                                  X'10'  MORE THAN 1 DEMO                      
*                                  X'08'  1ST DEMO ALREADY DONE                 
*                                  X'04'  ROUNDED DEMOS                         
WEEKS    DS    CL2                 BYTE 1 - BINARY NO. OF WEEKS                 
*                                  BYTE 2 -WEEK MODIFIER (+,-,OR SPACE)         
BUFFLINE DS    H                   NUMBER OF LINES COUNTER                      
BUFFMAX  DS    H                   MAX NO. OF LINES                             
BUFFWRST DS    H                   WORST VALUE SO FAR                           
DEMOMAX  DS    H                   MAXIMUM DEMO VALUE                           
OUTAREA  DS    CL16                EXTRA WORK AREA                              
SVPROG   DS    CL16                SAVE MAIN PROGRAM NAME                       
SBUFF    DS    A                   A(START OF PRINT BUFFER)                     
STAMP    DS    CL8                 STORAGE STAMP                                
SAVELN   EQU   *-SYSSPARE          SAVE AREA LENGTH                             
         DS    CL(L'SYSSPARE-(*-SYSSPARE)) SPARE                                
*                                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034RERES03   06/17/08'                                      
         END                                                                    
