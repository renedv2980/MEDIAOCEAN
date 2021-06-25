*          DATA SET RERES02    AT LEVEL 050 AS OF 06/17/08                      
*PHASE T81902A,*                                                                
*INCLUDE DEMTIME                                                                
*INCLUDE UNTIME                                                                 
*INCLUDE TIMVAL                                                                 
         TITLE 'T81902 - RERES02 - FLEXI REPORT'                                
*                                                                               
**********************************************************************          
*                                                                    *          
*  RERES02 - T81902 -- RESEARCH FLEXI REPORT                         *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  JAN09/90 (MRR) --- FORCE THIS REPORT TO SOON IFF STATIONS TIMES   *          
*                      DAYPARTS IS GREATER THAN 16                   *          
*                                                                    *          
*  FEB26/90 (MRR) --- BY-PASS SOON TEST IFF DDS TERMINAL             *          
*                                                                    *          
*  APR12/90 (MRR) --- CALL PAVFIX IN THE DEMAND HOOK                 *          
*                                                                    *          
*  APR25/90 (MRR) --- ONLY CALL PAVFIX WHEN NON DECIMAL OPTION IS SET*          
*                                                                    *          
*  SEP25/90 (MRR) --- >PAVFIX IS IUNDEM                              *          
*                     >CALL IUNDEM FOR EACH DEMO RECORD              *          
*                     >REMOVE ROUNDED/1-DEC OPTION, ALL OUTPUT IS 1  *          
*                     >MOVE AROUND HEADLINE                          *          
*                     >INCREASE COL WIDTH, MAX # DEMS 6>5 AND 5>4    *          
*                                                                    *          
*  NOV15/90 (MRR) --- >FIX NOW TO SOON AUTO-SWITCH DUE TO CHANGE     *          
*                      IN BASE SCREEN WITH NOV/90 1-DEC RELEASE      *          
*                                                                    *          
*                                                                    *          
*  JAN02/01 (FJD) --- >FIX UNIVERSE PRECISION                        *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
*                                                                               
T81902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1902**,RA,RR=R2                                              
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
         LH    RF,=Y(BUFF-SYSD)    A(START OF PRINT BUFFER)                     
         LA    RF,SYSD(RF)                                                      
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
         LA    RE,SYSSPARE         INIT SYSSPARE                                
         LH    RF,=Y(SAVELN)                                                    
         XCEF                                                                   
         MVC   STAMP,=CL8'T81902'  STAMP SAVEAREA                               
         LA    R2,FLESRCEH         VALIDATE SOURCE                              
         GOTO1 VVALSRC                                                          
         CLI   SVSOURCE,C'S'                                                    
         BNE   *+14                                                             
         MVC   CONHEAD(L'TPSRC),TPSRC                                           
         B     MYEND                                                            
         SPACE 1                                                                
         LA    R2,FLEBOOKH         VALIDATE BOOK                                
         MVI   MAX,10              DUMMY SO I CAN DO ERROR MSG                  
         GOTO1 VVALBOOK                                                         
         CLI   ACTUAL,1            REALLY ONLY 1 BOOK ALLOWED                   
         BNH   VKEY10                                                           
         MVC   CONHEAD(L'MANYBKS),MANYBKS    TOO MANY BOOKS                     
         B     MYEND                                                            
         SPACE 1                                                                
VKEY10   LA    R2,FLEDEMOH         VALIDATE DEMOS                               
         MVI   MAX,20              DUMMY SO I CAN DO ERROR MSG                  
         MVI   NFLDS,1                                                          
         GOTO1 VVALDEM                                                          
         MVC   NUMDEMS(1),ACTUAL                                                
         LA    R3,DEMOS                                                         
         MVC   DEMMOD,1(R3)        SAVE 1ST DEMO MODIFIER (FOR COST)            
         CLI   ACTUAL,5            REALLY ALLOW 5 DEMOS                         
         BNH   VKEY20              (IF COST FIELD USED, ONLY ALLOW 4)           
         MVC   CONHEAD(L'MANYDEM5),MANYDEM5                                     
         B     MYEND                                                            
         SPACE 1                                                                
VKEY20   LA    R2,FLEDAYH          VALIDATE DAY/DETAIL FIELDS                   
         GOTO1 VVALDYTM,PARAS,8    8 DAY/DETAIL FIELDS                          
         SPACE 1                                                                
         LA    R2,FLESTATH         VALIDATE STATION                             
         LA    R3,8                                                             
         LA    R4,STATSV                                                        
         XC    STATSV,STATSV                                                    
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
VKEY30   STM   R3,R4,OUTAREA                                                    
         GOTO1 VVALSTA         <-- DOES NOT DO AN NTR1                          
         LM    R3,R4,OUTAREA                                                    
         MVC   0(5,R4),ACTSTAT                                                  
         LA    R4,5(R4)                                                         
         BAS   RE,BUMP                                                          
         BCT   R3,*+8                                                           
         B     VKEY40                                                           
         CLI   5(R2),0             ANOTHER STATION?                             
         BNE   VKEY30                                                           
         SPACE 1                                                                
VKEY40   XC    OUTAREA,OUTAREA                                                  
         LA    R2,FLEWKSH          POINT TO WEEKS                               
         XC    WEEKS,WEEKS         INIT WEEKS                                   
         CLI   5(R2),0             INPUT IS OPTIONAL                            
         BE    VKEY60                                                           
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
         BE    VKEY60                                                           
         CLI   9(R2),C'+'                                                       
         BE    VKEY50                                                           
         CLI   9(R2),C'-'                                                       
         BNE   ERREND                                                           
         SPACE 1                                                                
VKEY50   MVC   WEEKS+1(1),9(R2)                                                 
         SPACE 1                                                                
VKEY60   LA    R2,FLEMINDH         POINT TO MINIMUM DEMO                        
         CLI   5(R2),0             INPUT IS OPTIONAL                            
         BE    VKEY70                                                           
         OI    PRINTOPT,X'40'      MIN DEMO FILTER USED                         
         GOTO1 VVALDLVL            EDIT FIELD                                   
         MVC   DEMOMIN,WORK+2                                                   
         SPACE 1                                                                
VKEY70   MVC   DEMOMAX,=X'FFFF'                                                 
         LA    R2,FLEMAXDH         POINT TO MAXIMUM DEMO                        
         CLI   5(R2),0             INPUT IS OPTIONAL                            
         BE    VKEY80                                                           
         OI    PRINTOPT,X'20'      MAX DEMO FILTER USED                         
         GOTO1 VVALDLVL            EDIT FIELD                                   
         MVC   DEMOMAX,WORK+2                                                   
         SPACE 1                                                                
VKEY80   LA    R2,FLEOPT1H         POINT TO RANK MAX                            
         MVC   BUFFMAX,=H'100'     ASSUME MAX OF 100                            
         MVC   BUFFWRST,=X'FFFF'   INITIALIZE BUFFWRST                          
         SPACE 1                                                                
         CLI   5(R2),0             INPUT IS OPTIONAL                            
         BE    VKEY90                                                           
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
VKEY90   LA    R2,FLECOSTH         POINT TO COST FIELD                          
         CLI   5(R2),0             INPUT IS OPTIONAL                            
         BE    VKEY100                                                          
         SPACE 1                                                                
         OI    PRINTOPT,X'10'      COST COLUMN USED                             
         XC    DMCB+4(3),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)     FIELD LENGTH                                 
         GOTO1 CASHVAL,DMCB,8(R2)                                               
         CLI   DMCB,X'FF'                                                       
         BNE   *+12                                                             
         MVI   ERROR,2             INVALID INPUT                                
         B     ERREND                                                           
         SPACE 1                                                                
         MVC   COST(4),DMCB+4                                                   
         CLI   NUMDEMS,4           IF COST FIELD USED,                          
         BNH   VKEY100             ONLY ALLOW 4 DEMOS                           
         MVC   CONHEAD(L'MANYDEM4),MANYDEM4                                     
         LA    R2,FLEDEMOH                                                      
         B     MYEND                                                            
         SPACE 1                                                                
VKEY100  EQU   *                                                                
         LA    R2,FLEOPT3H         SHOW PURE NUMBER                             
         CLI   5(R2),0             DEFAULT OR N IS TO SHOW PURE NO.             
         BE    VKEY200                                                          
         CLI   8(R2),C'N'                                                       
         BE    VKEY200                                                          
         CLI   8(R2),C'Y'          DON'T SHOW PURE NO.                          
         BNE   ERREND                                                           
         OI    PRINTOPT,X'04'                                                   
         SPACE 1                                                                
VKEY200  EQU   *                                                                
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
         LA    R2,FLEDAYH          COUNT DAY/TIME(S)                            
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
         LA    R2,FLESTATH         COUNT STATIONS                               
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
         MVC   CONWHEN+5(3),DUB          DUB HELD THE NAME                      
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
         CLC   STAMP,=CL8'T81902'  STOP IF STORAGE NOT STAMPED                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RCSUBPRG,0                                                       
         XC    BUFFLINE,BUFFLINE   COUNT NUMBER OF LINES ON PAGE                
         EJECT                                                                  
*   CONTROL I/O                                                                 
         SPACE 2                                                                
         LA    R3,8                UP TO 8 STATIONS                             
         LA    R2,STATSV           LIST OF STATIONS IN DBSELSTA FMT             
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
*  BUILD REST OF DBLOCK                                                         
         SPACE 1                                                                
PREP40   LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
***      LA    R1,IO               NETWORK RECDS ARE 2000 BYTES                 
         LA    R1,MYIOAREA          RNT IN CNTRLR USES IO+1000                  
         ST    R1,DBAREC                                                        
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBSELSRC,SVSOURCE                                                
         MVC   DBSELSTA,0(R2)                                                   
         MVC   DBSELBK,BOOK+1                                                   
*                                                                               
*  SPECIAL PATCH FOR WDVM (PRE 7/86)-WUSA CALL LETTER SWITCH                    
         CLC   DBSELSTA,=C'WUSAT'                                               
         BNE   PREP45                                                           
         CLC   DBSELBK,=X'5606'                                                 
         BH    PREP45                                                           
         MVC   DBSELSTA,=C'WDVMT'                                               
*                                                                               
PREP45   MVC   DBBTYPE,BOOK+3      BOOK TYPE                                    
         MVI   DBBEST,C'A'                                                      
         SPACE 1                                                                
         LA    R6,DAYTMLST         POINT TO DAY/TIME LIST                       
         ZIC   R5,DAYTIMES         NO. OF ENTRIES IN LIST                       
PREP50   ZIC   R1,0(R6)            GET DAY                                      
         SRL   R1,4                MOVE INTO INDEX POSITION                     
         LA    R1,DAYBITS(R1)                                                   
         MVC   DBSELDAY,0(R1)                                                   
         CLI   DBSELDAY,3          IS IS SA-SU                                  
         BNE   *+8                                                              
         MVI   DBDAYOPT,C'Y'       INDICATES SA-SU AVE LINE                     
         MVC   DBSELTIM,1(R6)                                                   
                                                                                
         CLI   SVMEDIA,C'N'                                                     
         BNE   PREP55                                                           
         MVI   DBBEST,C'L'                                                      
         CLI   0(R6),X'FF'         ASK FOR ALL FOR ALL                          
         BNE   *+8                                                              
         MVI   DBSELDAY,0          GET ALL DAYS =0 ON REP-NETWORK               
*                                                                               
PREP55   LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
*                                                                               
         SPACE 1                                                                
         GOTO1 DEMAND,DMCB,DBLOCKD,FILL                                         
         CLI   DBERROR,X'10'       RECORD NOT FOUND                             
         BNE   PREP60                                                           
         MVC   CONHEAD(L'NOFOUND),NOFOUND                                       
         MVC   CONHEAD+L'NOFOUND(5),0(R2) IDENTIFY STATION                      
         LA    R2,CONHEAD+L'NOFOUND+5                                           
         MVI   0(R2),C'/'                                                       
         MVC   1(9,R2),FLEBOOK     AND BOOK                                     
         LA    R2,FLESTATH         POINT CURSOR AT FIRST STATION                
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
NETBITS  DC    X'7C402010080402017F0380'    <-- 80 = VAR?                       
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
*  SEE IF IT MEETS WEEKS CRITERIA                                               
         SPACE 1                                                                
         CLI   WEEKS,0                                                          
         BE    FILL40              NO WEEKS CRITERIA                            
         GOTO1 DEFINE,DMCB,=C'WEEK',DBLOCKD,OUTAREA                             
         MVI   NWKS,0                                                           
         ZIC   R1,OUTAREA                                                       
         LA    R1,NUMTAB(R1)                                                    
         MVC   NWKS,0(R1)                                                       
         CLI   FLESRCE,C'A'        FUDGE FOR ZEN IF ARBITRON                    
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
         GOTO1 DEFINE,DMCB,=C'PURE',DBLOCKD,OUTAREA                             
         MVC   BUFFINV(2),OUTAREA  PURE NUMBER-FOR DAY/TIME                     
         MVC   BUFFPURE(4),OUTAREA+3 PURE NUMBER                                
         GOTO1 (RF),(R1),=C'PROGRAM',DBLOCKD,OUTAREA                            
         MVC   BUFFPROG,OUTAREA    PROGRAM NAME                                 
*                                                                               
         CLI   SVMEDIA,C'N'                                                     
         BNE   FILL45                                                           
         GOTO1 DEFINE,PARAS,=C'TIME',DBLOCKD,OUTAREA                            
         XC    DUB,DUB                                                          
         MVC   DUB(2),OUTAREA+2    EXTRACT START TIME                           
         MVC   OUTAREA,SPACES                                                   
         GOTO1 UNTIME,(R1),DUB,OUTAREA                                          
         MVC   BUFFTIME,OUTAREA    SET TIME IN BUFFER                           
         GOTO1 DEFINE,PARAS,=C'DAY',DBLOCKD,OUTAREA                             
         MVC   BUFFDAY,OUTAREA+2  EXTRACT 3 BYTE ALPHA DAY                      
*                                                                               
FILL45   ZIC   R1,NUMDEMS                                                       
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
         MVC   0(2,R3),2(R5)       OTHERWISE, USE AS IS                         
         SPACE 1                                                                
FILL75   OC    COST,COST           HANDLE COST FACILITY                         
         BZ    FILL80                                                           
         OC    BUFFCOST,BUFFCOST   ONLY FOR 1ST DEMO                            
         BNZ   FILL80                                                           
         L     R1,COST             IN PENNIES                                   
         LH    R0,0(R3)            DEMO IS ONLY A HALFWORD                      
         MR    R0,R0                                                            
         STCM  R1,15,BUFFCOST                                                   
         SPACE 2                                                                
FILL80   LA    R2,3(R2)            NEXT DEMO                                    
         LA    R3,3(R3)            NEXT BUFF AREA                               
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         BCT   R4,FILL50                                                        
         EJECT                                                                  
*              NOW SEE IF IT QUALIFIES                                          
         SPACE 3                                                                
         CLC   BUFFDEM1,DEMOMIN    DOES IT MEET MIN. REQUIREMENT                
         BL    XIT                                                              
         CLC   BUFFDEM1,DEMOMAX    AND MAX REQUIREMENT                          
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
         MH    R2,=Y(BUFFRECQ)                                                  
         L     RF,SBUFF            A(BUFFER START)                              
         LA    R2,0(R2,RF)                                                      
         MVC   0(BUFFRECQ,R2),WORK       AND PUT IN THIS ITEM                   
         CLC   BUFFLINE,BUFFMAX                                                 
         BL    XIT                                                              
         LH    R2,BUFFLINE                                                      
         GOTO1 XSORT,PARAS,(1,SBUFF),(R2),BUFFRECQ,2,BUFFDEM1-BUFFREC           
         BCTR  R2,0                                                             
         MH    R2,=Y(BUFFRECQ)                                                  
         L     RF,SBUFF                                                         
         LA    R2,0(R2,RF)                                                      
         MVC   BUFFWRST,BUFFDEM1-BUFFREC(R2)                                    
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
         ZIC   R4,NUMDEMS                                                       
         LH    R5,BUFFLINE                                                      
         SPACE 2                                                                
RAN10    GOTO1 XSORT,PARAS,(1,SBUFF),(R5),BUFFRECQ,2,(R2)                       
         BAS   RE,RAN40                                                         
         SPACE 2                                                                
         SH    R2,=H'3'                                                         
         SH    R3,=H'3'                                                         
         BCT   R4,RAN10                                                         
         B     XIT                                                              
         SPACE 2                                                                
*                                  ROUTINE TO PUT IN RANK NUMBERS               
RAN40    NTR1                                                                   
         L     RF,SBUFF                                                         
         LA    R2,0(R2,RF)         R2=A(COLUMN)                                 
         LA    R3,1                R3=PLACE NUMBER                              
         LA    R4,1                R4=ACTUAL NUMBER                             
*                                  R5=N'LINES                                   
         SR    R1,R1               R1=CURRENT VALUE                             
         SPACE 2                                                                
RAN50    STC   R4,2(R2)                                                         
         CH    R1,0(R2)            IS THIS WORSE THAN PREVIOUS                  
         BE    RAN60                                                            
         LR    R3,R4               THEN SET PLACE TO ACTUAL                     
         LH    R1,0(R2)            AND SAVE THIS VALUE                          
         B     RAN70                                                            
         SPACE 2                                                                
RAN60    STC   R3,2(R2)            TIE - USE PREVIOUS PLACE                     
         SPACE 2                                                                
RAN70    LA    R2,BUFFRECQ(R2)                                                  
         LA    R4,1(R4)                                                         
         BCT   R5,RAN50                                                         
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
SPL20    LA    R2,10               PRINT IN BLOCKS OF 10                        
         CR    R3,R2               OR LESS                                      
         BH    *+6                                                              
         LR    R2,R3                                                            
         LA    R1,1(R2)                                                         
         STC   R1,ALLOWLIN         ALLOW FOR 1 MORE LINE                        
         SPACE 2                                                                
SPL40    MVI   SPACING,1                                                        
         BAS   RE,FORMAT                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         XC    0(BUFFRECQ,R6),0(R6)                                             
         BCTR  R3,0                                                             
         LA    R6,BUFFRECQ(R6)                                                  
         MVI   ALLOWLIN,0                                                       
         BCT   R2,SPL40                                                         
         GOTO1 SPOOL,PARAS,(R8)                                                 
         CH    R3,=H'0'                                                         
         BNE   SPL20                                                            
         B     XIT                                                              
         SPACE 2                                                                
*                                  FILL A PRINT LINE                            
FORMAT   NTR1                                                                   
         MVC   P(4),BUFFSTAT       STATION                                      
         CLI   BUFFSTAT+4,C'T'     ANY BAND (EXCEPT TV) OR SATELLITE            
         BE    FMT10                                                            
         MVI   P+4,C'-'                                                         
         MVC   P+5(1),BUFFSTAT+4   DISPLAY IT                                   
FMT10    MVC   P+8(16),BUFFPROG    PROGRAM                                      
         CLI   SVMEDIA,C'N'          BYPASS FOR NETWORK                         
         BNE   *+20                                                             
         MVC   P+25(3),BUFFDAY                                                  
         MVC   P+29(6),BUFFTIME                                                 
         B     FMT15                                                            
*                                                                               
         GOTO1 INVEDIT,PARAS,BUFFINV,WORK                                       
         MVC   P+25(3),WORK        DAY                                          
         MVC   P+29(6),WORK+3      TIME                                         
         SPACE 1                                                                
FMT15    LA    R3,P+36                                                          
         TM    PRINTOPT,X'04'      SUPPRESS PURE NO.                            
         BO    FMT20                                                            
         MVC   0(4,R3),BUFFPURE                                                 
         LA    R3,6(R3)                                                         
         SPACE 1                                                                
FMT20    TM    PRINTOPT,X'10'      COST FIELD                                   
         BZ    FMT40                                                            
         ICM   R1,15,BUFFCOST                                                   
         CLI   DEMMOD,C'T'         IF PRECISION DEMOS, THEN                     
         BE    FMT30               D, T, Q, & P ARE ALREADY INTEGERS            
         CLI   DEMMOD,C'Q'                                                      
         BE    FMT30                                                            
         CLI   DEMMOD,C'P'                                                      
         BE    FMT30                                                            
* SLA, AH, & SRA ARE FOR ROUNDING                                               
         SR    R0,R0                                                            
         SLA   R1,1                                                             
         D     R0,=F'10'           OTHERWISE, DIVIDE BY 10                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         SPACE 1                                                                
FMT30    EDIT  (R1),(9,0(R3)),2,FLOAT=$                                         
         SPACE 1                                                                
         LA    R3,10(R3)           POINT TO WHERE DEMOS SHOULD PRINT            
         SPACE 2                                                                
FMT40    LA    R2,BUFFDEM1         SET UP TO EDIT DEMOS                         
         LA    R4,DEMOS                                                         
         ZIC   R5,NUMDEMS                                                       
         SPACE 2                                                                
FMT50    EQU   *                                                                
         CLI   1(R4),C'U'                IF UNIVERSE                            
         BE    FMT60                     THEN NO DECIMAL POINT                  
         CLI   1(R4),C'D'                                                       
         BE    FMT60                                                            
         EDIT  (2,0(R2)),(6,0(R3)),1     DEMO VALUE-1 DECIMAL                   
         B     FMT70                                                            
FMT60    EDIT  (2,0(R2)),(6,0(R3))       UNIVERSE VALUE- NO DECIMAL             
                                                                                
FMT70    EDIT  (1,2(R2)),(3,7(R3))       RANK                                   
                                                                                
         SPACE 2                                                                
         LA    R2,3(R2)                                                         
         LA    R3,12(R3)                                                        
         LA    R4,3(R4)                                                         
         BCT   R5,FMT50                                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* HOOK ROUTINE FOR HEADLINE DETAILS                                             
*                                                                               
HOOK     NTR1                                                                   
         MVC   H4+112(3),FLESRCE    SOURCE                                      
         MVC   H4+116(9),FLEBOOK    BOOK                                        
*                                                                               
*  IF WEEKS, MIN OR MAX DEMO, OR COST FILTERS, SHOW IN HEADLINES                
*                                                                               
         LA    R3,H4                                                            
         TM    PRINTOPT,X'80'      WEEKS ENTERED?                               
         BZ    HOOK10                                                           
         MVC   0(33,R3),=C'INCLUDES PROGRAMS AIRING    WEEKS'                   
         MVC   25(2,R3),WEEKS                                                   
         OI    25(R3),X'F0'                                                     
         CLC   25(2,R3),=C'1 '     IF 1 WEEKS                                   
         BNE   HOOK10                                                           
         MVC   27(6,R3),=C'WEEK  ' MAKE IT WEEK, NOT WEEKS                      
*                                                                               
HOOK10   LA    R3,H5                                                            
         TM    PRINTOPT,X'40'      MIN DEMO ENTERED?                            
         BZ    HOOK20                                                           
         MVC   0(22,R3),=C'MINIMUM DEMO LEVEL IS '                              
         LA    R2,FLEMINDH                                                      
         ZIC   R1,5(R2)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   23(0,R3),FLEMIND                                                 
         LA    R3,H6                                                            
         SPACE 1                                                                
HOOK20   TM    PRINTOPT,X'20'      MAX DEMO ENTERED?                            
         BZ    HOOK25                                                           
         MVC   0(22,R3),=C'MAXIMUM DEMO LEVEL IS '                              
         LA    R2,FLEMAXDH                                                      
         ZIC   R1,5(R2)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   23(0,R3),FLEMAXD                                                 
*                                                                               
HOOK25   LA    R3,H5+98                                                         
         TM    PRINTOPT,X'10'      COST FIELD ENTERED?                          
         BZ    HOOK30                                                           
         MVC   0(7,R3),=C'CPM - $'                                              
         CLI   DEMMOD,C'R'                                                      
         BNE   *+8                                                              
         MVI   2(R3),C'P'                                                       
         L     R2,COST                                                          
         EDIT  (R2),(10,7(R3)),2,ALIGN=LEFT                                     
         SPACE 1                                                                
HOOK30   LA    R3,H8+36                                                         
         LA    R5,H9+36                                                         
         TM    PRINTOPT,X'04'      SUPPRESS PURE NO.                            
         BO    HOOK40                                                           
         MVC   0(4,R3),=C'PURE'                                                 
         MVC   0(4,R5),=C'----'                                                 
         LA    R3,6(R3)                                                         
         LA    R5,6(R5)                                                         
*                                                                               
HOOK40   TM    PRINTOPT,X'10'      COST FIELD                                   
         BZ    HOOK50                                                           
         MVC   0(9,R3),=C'PROJECTED'                                            
         MVC   2(4,R5),=C'COST'                                                 
         LA    R3,10(R3)                                                        
         LA    R5,10(R5)                                                        
*                                                                               
HOOK50   LA    R2,DEMOS                                                         
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
         BCT   R6,HOOK60                                                        
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
INVTIM   DC    C'* ERROR * INVALID TIME'                                        
INVDAY1  DC    C'* ERROR * INVALID DAY/DETAIL'                                  
MANYDEM4 DC    C'* ERROR * TOO MANY DEMOS - LIMIT IS 4'                         
MANYDEM5 DC    C'* ERROR * TOO MANY DEMOS - LIMIT IS 5'                         
MANYBKS  DC    C'* ERROR * TOO MANY BOOKS - LIMIT IS 1'                         
NOFOUND  DC    C'** ERROR ** RECORD NOT FOUND - '                               
TPSRC    DC    C'* ERROR * SRC IS ONLY VALID FOR TIME PERIOD'                   
MUSTSOON DC    C'* THIS REQUEST MUST BE DONE AS SOON *'                         
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
         SPROG 0,1                                                              
         PSPEC H1,001,AGYNAME                                                   
         PSPEC H1,053,C'TIME PERIOD RANKER (FLEXI)'                             
         PSPEC H1,099,REPORT                                                    
         PSPEC H1,115,PAGE                                                      
         PSPEC H2,053,26C'-'                                                    
         PSPEC H2,001,REQUESTOR                                                 
         PSPEC H2,099,RUN                                                       
         PSPEC H4,099,C'SOURCE BOOK -'                                          
         SPACE 1                                                                
         SPROG 0                                                                
         PSPEC H8,01,C'STATION     PROGRAM      DAY  TIME'                      
         PSPEC H9,01,C'------- ---------------  ---  ----'                      
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER RECORDS IN BUFF                                   
         SPACE 3                                                                
BUFFD    DSECT                                                                  
BUFFREC  DS    0C                                                               
BUFFSTAT DS    CL5                                                              
BUFFPROG DS    CL16                                                             
BUFFDAY  DS    CL3                                                              
BUFFTIME DS    CL6                                                              
BUFFINV  DS    CL2                                                              
BUFFPURE DS    CL4                                                              
BUFFCOST DS    CL4                                                              
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
BUFFRECQ EQU   *-BUFFREC                                                        
*                                                                               
         EJECT                                                                  
       ++INCLUDE RERESWRK                                                       
         EJECT                                                                  
* DSECT TO COVER SCREEN                                                         
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RERESF2D                                                       
         EJECT                                                                  
* SAVE AREA VALUES                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE            LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
COST     DS    F                   FOR CPP/CPM -SIGNED PENNIES (BINARY)         
NWKS     DS    CL1                 NUMBER OF WEEKS                              
PRINTOPT DS    XL1                 X'80'  WEEKS FILTER                          
*                                  X'40'  MINIMUM DEMO FILTER                   
*                                  X'20'  MAXIMUM DEMO FILTER                   
*                                  X'10'  COST FILTER                           
*                                  X'08'  ROUND DEMOS                           
*                                  X'04'  SUPPRESS PURE NO.                     
WEEKS    DS    CL2                 BYTE 1 - BINARY NO. OF WEEKS                 
*                                  BYTE 2 -WEEK MODIFIER (+,-,OR SPACE)         
BUFFLINE DS    H                   NUMBER OF LINES COUNTER                      
BUFFMAX  DS    H                   MAX NO. OF PROGRAMS                          
BUFFWRST DS    H                   WORST VALUE SO FAR                           
DEMOMIN  DS    H                   MINIMUM DEMO VALUE                           
DEMOMAX  DS    H                   MAXIMUM DEMO VALUE                           
         DS   0F                                                                
OUTAREA  DS    CL16                EXTRA WORK AREA                              
SBUFF    DS    A                   A(START OF PRINT BUFFER)                     
STAMP    DS    CL8                 STORAGE STAMP                                
MYIOAREA DS    XL2000              NETWORK RECDS MAY BE 2000 BYTES              
SAVELN   EQU   *-SYSSPARE          SAVE AREA LENGTH                             
         DS    CL(L'SYSSPARE-(*-SYSSPARE)) SPARE                                
*                                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050RERES02   06/17/08'                                      
         END                                                                    
