*          DATA SET RERES01S   AT LEVEL 026 AS OF 05/01/02                      
*PHASE T81901B,*                                                                
*INCLUDE DEMTIME                                                                
*INCLUDE UNTIME                                                                 
*INCLUDE TIMVAL                                                                 
         TITLE 'T81901 - RERES01 - REP RESEARCH/PAV TAPE LISTING'               
*                                                                               
*********************************************************************           
*                                                                   *           
*       RERES01 --- VALIDATE PAV TAPE REPORT REQUEST SCREEN AND     *           
*                    GENERATE THE REPORT                            *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* FEB16/89 (MRR) --- ADD DAY FILTER, ADD 'PARTIAL AIRINGS' FEATURE, *           
*                     AND ALLOW FOR TEN STATIONS (UP FROM EIGHT)    *           
*                                                                   *           
* JAN11/90 (MRR) --- FORCE THIS REPORT TO SOON IFF STATIONS > 4     *           
*                                                                   *           
* FEB26/90 (MRR) --- ADD SWITCH TO SOON IN LIEU OF ERR MSG AND      *           
*                     BY-PASS TEST IF DDS TERMINAL                  *           
*                                                                   *           
* APR09/90 (MRR) --- CALL FIXPAV IN THE DEMAND HOOK                 *           
*                                                                   *           
* SEP25/90 (MRR) --- >SET DBLOCK TO 1 DECIMAL PRINTING              *           
*                    >FIXPAV IS NOW IUNDEM                          *           
*                    >CHANGE HEADER TO STANDARD                     *           
*                    >COLUMN INCREASE FROM 6 TO 7, # DEMS 11 > 9    *           
*                                                                   *           
* NOV15/90 (MRR) --- >FIX AUTO NOW TO SOON SWITCH DUE TO HEAD SCRN  *           
*                     CHANGE FOR NOV/90 1-DEC RELEASE               *           
*                                                                   *           
* NOV20/90 (MRR) --- >MOVE DEMOS OVER ONE COLUMN                    *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
T81901   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1901**,RA,RR=R2                                              
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
         LH    R3,=Y(BUFF-SYSD)  GET START OF ADDRESS                           
         LA    R3,SYSD(R3)                                                      
         ST    R3,SBUFF                                                         
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
*                                                                               
         LA    RE,SYSSPARE         INIT SYSSPARE                                
         LH    RF,=Y(SAVELN)                                                    
         XCEF                                                                   
*                                                                               
         MVC   STAMP,=CL8'T81901'  STAMP SAVEAREA                               
         LA    R2,PURSRCEH         VALIDATE SOURCE                              
         GOTO1 VVALSRC                                                          
         CLI   SVSOURCE,C'S'                                                    
         BNE   *+14                                                             
         MVC   CONHEAD(L'TPSRC),TPSRC                                           
         B     MYEND                                                            
         SPACE 1                                                                
         LA    R2,PURBOOKH         VALIDATE BOOK                                
         MVI   MAX,10              DUMMY SO I CAN DO ERROR MSG                  
         GOTO1 VVALBOOK                                                         
         CLI   ACTUAL,1            REALLY ONLY 1 BOOK ALLOWED                   
         BNH   VKEY5                                                            
         MVC   CONHEAD(L'MANYBKS),MANYBKS    TOO MANY BOOKS                     
         B     MYEND                                                            
         SPACE 1                                                                
VKEY5    LA    R2,PURDEMOH         VALIDATE DEMOS                               
         MVI   NUMDEMS,1           PRESET TO 1 DEMO                             
         MVC   DEMO(3),=X'00D901'  PRESET TO RATING                             
         CLI   5(R2),0                                                          
         BE    VKEY10                                                           
         MVI   MAX,20              DUMMY SO I CAN DO ERROR MSG                  
         MVI   NFLDS,1                                                          
         GOTO1 VVALDEM                                                          
         CLI   ACTUAL,9            REALLY ALLOW 9 DEMOS                         
         BNH   VKEY7                                                            
         MVC   CONHEAD(L'MANYDEM),MANYDEM                                       
         B     MYEND                                                            
         SPACE 1                                                                
VKEY7    MVC   NUMDEMS,ACTUAL      SAVE NUMBER OF DEMOS                         
         CLI   ACTUAL,2                                                         
         BNH   VKEY10                                                           
         OI    PRINTOPT,X'80'      NOTE - MORE THAN 2 DEMOS                     
         SPACE 1                                                                
VKEY10   LA    R2,PURSTATH         VALIDATE STATION                             
         LA    R3,10                                                            
         LA    R4,STATSV                                                        
         LA    R5,MKTSV                                                         
         XC    STATSV,STATSV                                                    
         XC    MKTSV(144),MKTSV                                                 
         XC    MKTSV+144(144),MKTSV+144                                         
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
VKEY20   GOTO1 VVALSTA                                                          
         MVC   0(5,R4),ACTSTAT                                                  
         CLI   SVMEDIA,C'N'                                                     
         BE    VKEY25                                                           
         GOTO1 VVALMKT                                                          
         MVC   0(29,R5),WORK+8     SAVE MARKET NAME                             
VKEY25   LA    R4,5(R4)                                                         
         LA    R5,29(R5)                                                        
         BAS   RE,BUMP                                                          
         BCT   R3,*+8                                                           
         B     VKEY40                                                           
         CLI   5(R2),0             ANOTHER STATION?                             
         BNE   VKEY20                                                           
         SPACE 1                                                                
VKEY40   LA    R2,PURSTIMH         START TIME                                   
         MVC   STIM,=H'600'        DEFAULT                                      
         CLI   5(R2),0                                                          
         BE    VKEY45                                                           
         GOTO1 VVALTIM                                                          
         MVC   STIM,WORK                                                        
         OI    PRINTOPT,X'20'      START TIME FILTER                            
         SPACE 1                                                                
VKEY45   LA    R2,PURETIMH         END TIME                                     
         MVC   ETIM,=H'545'        DEFAULT                                      
         CLI   5(R2),0                                                          
         BE    VKEY50                                                           
         GOTO1 VVALTIM                                                          
         MVC   ETIM,WORK                                                        
         OI    PRINTOPT,X'10'      END TIME FILTER                              
         SPACE 1                                                                
VKEY50   MVI   UP,2                DEFAULT IS 2 UP                              
         TM    PRINTOPT,X'80'      BUT, IF MORE THAN 2 DEMOS (X'80')            
         BZ    *+8                                                              
         MVI   UP,1                MUST PRINT 1 UP ON PAGE                      
         SPACE 1                                                                
         LA    R2,PUROPT2H         SPACING OPTION                               
         MVI   ERROR,2             INVALID INPUT                                
         MVI   GAP,1               DEFAULT SINGLE SPACING                       
         CLI   5(R2),0                                                          
         BE    VKEY60                                                           
         CLI   8(R2),C'1'                                                       
         BE    VKEY60                                                           
         CLI   8(R2),C'2'                                                       
         BNE   ERREND                                                           
         MVI   GAP,2                                                            
         SPACE 1                                                                
VKEY60   EQU   *                   VALIDATE DAY FILTER (OPTIONAL)               
         MVI   DAYBITS,0           NETWORK: DEFAULT TO ALL PRGMS                
         LA    R2,PURDAYFH                                                      
         CLI   5(R2),0                                                          
         BE    VKEY70                                                           
         CLI   SVMEDIA,C'N'                                                     
         BE    VKEY66                                                           
         LA    R3,DAYLIST                                                       
VKEY65   CLC   0(3,R3),PURDAYF                                                  
         BE    VKEY70                                                           
         LA    R3,3(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   VKEY65                                                           
         GOTO1 VVALDAY                                                          
         B     VKEY70                                                           
*                                                                               
VKEY66   EQU   *                   NETWORK DAY CODE VALIDATION                  
         LA    R3,NETDAYTB                                                      
         LA    R0,NETDAYS                                                       
         CLC   1(3,R3),PURDAYF    TEST BITS VS. TABLE                           
         BE    *+20                                                             
         LA    R3,L'NETDAYTB(R3)                                                
         BCT   R0,*-14                                                          
         MVI   ERROR,2             INVALID FLD                                  
         B     ERREND                                                           
         MVC   DAYBITS,0(R3)                                                    
         SPACE 1                                                                
VKEY70   EQU   *                   VALIDATE 'PARTIAL AIRINGS' FLAG              
         LA    R2,PURPARTH                                                      
         CLI   5(R2),0                                                          
         BE    VKEY200                                                          
         CLI   8(R2),C'N'                                                       
         BE    VKEY200                                                          
         CLI   8(R2),C'Y'                                                       
         BE    VKEYX                                                            
VKEY75   EQU   *                                                                
         MVC   CONHEAD(L'MUSTBEYN),MUSTBEYN                                     
         B     MYEND                                                            
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
         LA    R2,PURSTATH         COUNT STATIONS                               
         SR    R3,R3                                                            
         LA    R4,10               LOOP FOR UP TO 10 STATIONS                   
VKEY230  EQU   *                                                                
         CLI   5(R2),0             INPUT?                                       
         BE    VKEY240                                                          
         LA    R3,1(R3)                                                         
         BAS   RE,BUMP             POINT TO THE NEXT STATION                    
         BCT   R4,VKEY230                                                       
VKEY240  EQU   *                                                                
         C     R3,=F'4'                                                         
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
VKEYX    EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
* PRINT DATA TYPE REPORT                                                        
* R4 POINTS AT DBLOCKA1 - MAIN DBLOCK AREA                                      
*                                                                               
PREP     EQU   *                                                                
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         CLC   STAMP,=CL8'T81901'  STOP IF STORAGE NOT STAMPED                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RCSUBPRG,0          2 UP (DEFAULT FORMAT)                        
         CLI   UP,1                                                             
         BNE   *+8                                                              
         MVI   RCSUBPRG,1          1 UP FORMAT                                  
         MVI   COUNT,0             COUNT NUMBER OF LINES ON PAGE                
         SPACE 1                                                                
         L     R2,SBUFF            CLEAR OUT BUFFER AREA                        
         LA    R3,48                                                            
CLEAR    MVC   0(110,R2),SPACES                                                 
         LA    R2,110(R2)                                                       
         BCT   R3,CLEAR                                                         
         EJECT                                                                  
*   CONTROL I/O                                                                 
         SPACE 2                                                                
         LA    R2,PURSTATH         STATIONS ON SCREEN                           
         ST    R2,SAVE2                                                         
         LA    R3,10               UP TO 10 STATIONS                            
         LA    R5,STATSV           LIST OF STATIONS IN DBSELSTA FMT             
         LA    R6,MKTSV            LIST OF MARKET NAMES                         
         ST    R6,SAVER6                                                        
         SPACE 1                                                                
*  BUILD REST OF DBLOCK                                                         
         SPACE 1                                                                
PURE     LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         LA    R1,MYIOAREA                                                      
         ST    R1,DBAREC                                                        
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBSELSRC,SVSOURCE                                                
         MVC   DBSELSTA,0(R5)                                                   
         MVC   DBSELBK,BOOK+1                                                   
*                                                                               
*  SPECIAL PATCH FOR WDVM (PRE 7/86)-WUSA CALL LETTER SWITCH                    
         CLC   DBSELSTA,=C'WUSAT'                                               
         BNE   PURE10                                                           
         CLC   DBSELBK,=X'5606'                                                 
         BH    PURE10                                                           
         MVC   DBSELSTA,=C'WDVMT'                                               
*                                                                               
PURE10   EQU   *                                                                
         MVC   DBBTYPE,BOOK+3      BOOK TYPE                                    
         MVI   DBBEST,C'A'                                                      
         MVI   DBSELDAY,X'FF'                                                   
*                                                                               
         CLI   DBSELMED,C'N'       NETWORK?                                     
         BNE   PURE15                                                           
         MVC   DBSELDAY,DAYBITS    EVERYTHING=0                                 
         MVI   DBSELDUR,X'FF'      GET < 15 MIN PRGMS TOO                       
         MVI   DBBEST,C'L'         GET PRGMS FOR SPEC DAY/ROTATOR               
*                                                                               
PURE15   LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
*                                                                               
         CLI   PURPART,C'Y'                                                     
         BE    PURE20                                                           
         MVC   DBSELTIM(2),STIM                                                 
         MVC   DBSELTIM+2(2),ETIM                                               
         B     PURE30                                                           
PURE20   EQU   *                                                                
         MVC   DBSELTIM(2),=H'600'                                              
         MVC   DBSELTIM+2(2),=H'545'                                            
         GOTO1 =V(HRTOQH),PARAS,STIM,RQSTSQTR,RR=RELO                           
         GOTO1 =V(HRTOQH),PARAS,ETIM,RQSTEQTR,RR=RELO                           
*                                                                               
PURE30   EQU   *                                                                
         GOTO1 DEMAND,DMCB,DBLOCKD,FILL                                         
         CLI   DBERROR,X'10'       RECORD NOT FOUND                             
         BNE   PURE50                                                           
         MVC   CONHEAD(L'NOFOUND),NOFOUND                                       
         MVC   CONHEAD+L'NOFOUND(5),0(R5) IDENTIFY STATION                      
         LA    R2,CONHEAD+L'NOFOUND+5                                           
         MVI   0(R2),C'/'                                                       
         MVC   1(8,R2),PURBOOK     AND BOOK                                     
         LA    R2,PURSTATH                                                      
         B     MYEND                                                            
*                                                                               
PURE50   EQU   *                                                                
         CLI   DBERROR,X'80'       TEST FOR EOF                                 
         BNE   PURE60              SKIP LISTING                                 
         OC    DBDIVSOR,DBDIVSOR   TEST FOR ANY RECORDS                         
         BZ    PURE60              NO                                           
         BAS   RE,SPLAT                                                         
         MVC   PAGE,=H'1'                                                       
         BCT   R3,*+8                                                           
         B     XIT                                                              
PURE60   EQU   *                                                                
         BAS   RE,BUMP             NEXT STATION ON SCREEN                       
         LA    R5,5(R5)            NEXT DBSELSTA FORMATTED STATION              
         LA    R6,29(R6)           NEXT MARKET NAME                             
         ST    R2,SAVE2                                                         
         ST    R5,SAVER5                                                        
         ST    R6,SAVER6                                                        
         OC    0(5,R5),0(R5)       ANY MORE?                                    
         BNZ   PURE                                                             
         B     XIT                                                              
         EJECT                                                                  
*  PROCESS A RECORD                                                             
         SPACE 1                                                                
FILL     NTR1                                                                   
*                                                                               
         GOTO1 VIUNDEM,DMCB,DBLOCKD                                             
*                                                                               
         CLI   PURPART,C'Y'        PROCESS PARTIAL AIRINGS FILTER               
         BNE   FILLT50                                                          
         GOTO1 DEFINE,PARAS,=C'TIME',DBLOCKD,WORK                               
         CLI   PURSTIMH+5,0                                                     
         BE    FILLT20                                                          
         CLC   WORK+1(1),RQSTSQTR  COMP SHOW END TIME TO RQST START             
         BNH   XIT                 SHOW ENDS ON OR BEFORE RQST, SKIP IT         
FILLT20  EQU   *                                                                
         CLI   PURETIMH+5,0                                                     
         BE    FILLT50                                                          
         CLC   WORK+0(1),RQSTEQTR  COMP SHOW START TO RQST END                  
         BNL   XIT                 SHOW STARTS ON OR AFTER RQST,SKIP IT         
FILLT50  EQU   *                                                                
         CLI   PURDAYFH+5,0        DAY FILTER?                                  
         BE    FILL1               NO, MOVE ON                                  
         GOTO1 DEFINE,PARAS,=C'DAY',DBLOCKD,WORK                                
         CLC   WORK+2(3),PURDAYF                                                
         BNE   XIT                 NOT THE REQUESTED DAY, SKIP IT               
FILL1    EQU   *                                                                
         ZIC   R5,COUNT                                                         
         L     R3,SBUFF                                                         
         CLI   UP,2                                                             
         BNE   FILL10                                                           
         CLI   COUNT,96                                                         
         BNE   FILL5                                                            
         BAS   RE,SPLAT                                                         
         SR    R5,R5                                                            
FILL5    EQU   *                                                                
         CLI   COUNT,48                                                         
         BL    FILL40                                                           
         L     R3,SBUFF                                                         
         LA    R3,57(R3)                                                        
         SH    R5,=H'48'                                                        
         B     FILL40                                                           
         SPACE 1                                                                
FILL10   CLI   COUNT,48                                                         
         BNE   FILL40                                                           
         BAS   RE,SPLAT                                                         
         SR    R5,R5                                                            
         SPACE 1                                                                
FILL40   ZIC   R1,COUNT            ADJUST COUNT                                 
         ZIC   R0,GAP                                                           
         AR    R1,R0                                                            
         STC   R1,COUNT                                                         
         MH    R5,=H'110'          BUMP DOWN BY LINE COUNT                      
         AR    R3,R5                                                            
         GOTO1 DEFINE,PARAS,=C'TIME',DBLOCKD,WORK                               
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(2),WORK+2       EXTRACT START TIME                           
         MVC   WORK,SPACES                                                      
         GOTO1 UNTIME,(R1),DUB,WORK                                             
*                                                                               
         CLC   LASTIME,WORK                                                     
         BE    FILL45                                                           
         MVC   0(6,R3),WORK        SHOW TIME IF DIFFERENT                       
FILL45   EQU   *                                                                
         MVC   LASTIME,WORK                                                     
         GOTO1 DEFINE,PARAS,=C'DAY',DBLOCKD,WORK                                
         MVC   12(3,R3),WORK+2     EXTRACT 3 BYTE ALPHA DAY                     
*                                                                               
FILL50   EQU   *                                                                
         GOTO1 (RF),(R1),=C'PURE',DBLOCKD,WORK                                  
         MVC   7(4,R3),WORK+3      EXTRACT EDITED PURE NUMBER                   
*                                                                               
         GOTO1 (RF),(R1),=C'PROGRAM',DBLOCKD,WORK                               
         MVC   16(16,R3),WORK                                                   
         SPACE 2                                                                
         BAS   RE,COUNTWK          PRINT WEEKS AND                              
         EDIT  (1,NWKS),(1,34(R3))                                              
*                                                                               
         CLC   12(3,R3),=C'VAR'    BLANK OUT QTR HRS                            
         BE    FILL60              FOR VAR AND TYP                              
         CLC   12(3,R3),=C'TYP'                                                 
         BE    FILL60                                                           
*                                                                               
* TOTAL PROGRAM DURATION                                                        
*                                                                               
         GOTO1 DEFINE,PARAS,=C'TOTDUR',DBLOCKD,WORK                             
         ZIC   R2,WORK                                                          
         EDIT  (R2),(3,37(R3))                                                  
         SPACE 2                                                                
FILL60   MVC   SAVBK,DBACTBK                                                    
         ZIC   R1,NUMDEMS                                                       
         MH    R1,=H'3'                                                         
         LA    R1,DEMOS(R1)                                                     
         MVI   0(R1),X'FF'                                                      
         LA    R2,DEMOS                                                         
         LA    R6,40(R3)                                                        
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCKD,BLOCK1                           
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DBACTBK,SAVBK       RESTORE IT                                   
         LA    R5,BLOCK1           NOW POINT R5 TO VALUES                       
         ZIC   R1,NUMDEMS                                                       
         SPACE 2                                                                
FILL70   EDIT  (4,0(R5)),(6,0(R6)),1   1 DECIMAL POINT FOR                      
         LA    R2,3(R2)            NEXT DEMO                                    
         LA    R6,7(R6)            NEXT DEMO AREA                               
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         BCT   R1,FILL70                                                        
         B     XIT                                                              
         EJECT                                                                  
*  OUTPUT A PAGE OF PRINTING                                                    
SPLAT    NTR1                                                                   
         L     R2,SBUFF                                                         
         LA    R3,48                                                            
         SPACE 1                                                                
SPLAT20  CLC   0(110,R2),SPACES                                                 
         BE    SPLAT40                                                          
         MVC   P(110),0(R2)                                                     
         GOTO1 SPOOL,PARAS,(R8)                                                 
         SPACE 2                                                                
SPLAT40  MVC   0(110,R2),SPACES                                                 
         LA    R2,110(R2)                                                       
         BCT   R3,SPLAT20                                                       
         MVI   COUNT,0                                                          
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO COUNT ACTIVE WEEKS *                                  
         SPACE 3                                                                
COUNTWK  NTR1                                                                   
         GOTO1 DEFINE,PARAS,=C'WEEK',DBLOCKD,WORK                               
         MVI   NWKS,0                                                           
         ZIC   R1,WORK             GET WEEK BITS FROM DEFINE                    
         LA    R1,NUMTAB(R1)                                                    
         MVC   NWKS,0(R1)                                                       
         CLI   DBSELMED,C'N'       NETWORK?                                     
         BNE   *+16                                                             
         CLI   NWKS,0              DEFAULT IS IT RAN 1 WK                       
         BNE   *+8                                                              
         MVI   NWKS,1                                                           
         CLI   PURSRCE,C'A'        IF ARBITRON                                  
         BNE   XIT                                                              
         CLC   BOOK+1(2),=X'5605'  AND BEFORE MAY86                             
         BNL   XIT                 THEN                                         
         CLI   WORK,X'02'          FUDGE FOR ZEN                                
         BNE   XIT                                                              
         MVI   NWKS,3                                                           
         B     XIT                                                              
         SPACE 2                                                                
NUMTAB   DC    AL1(0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4)                             
         EJECT                                                                  
* HOOK ROUTINE FOR HEADLINE DETAILS                                             
*                                                                               
HOOK     NTR1                                                                   
         L     R2,SAVE2                                                         
         L     R6,SAVER6                                                        
         MVC   H4+13(6),8(R2)      STATION CALL LETTERS                         
         MVC   H4+21(29),0(R6)     MARKET NAME                                  
         MVC   H4+112(3),PURSRCE   SOURCE                                       
         MVC   H4+116(8),PURBOOK   BOOK                                         
*                                                                               
*  IF START OR END TIME FILTER, SHOW IN HEADLINES                               
*                                                                               
         LA    R3,H5                                                            
         TM    PRINTOPT,X'20'      START TIME ENTERED?                          
         BZ    HOOK10                                                           
         XC    DUB,DUB                                                          
         MVC   DUB(2),STIM                                                      
         MVC   0(12,R3),=C'START TIME -'                                        
         GOTO1 UNTIME,PARAS,DUB,13(R3)                                          
         LA    R3,21(R3)                                                        
*                                                                               
HOOK10   TM    PRINTOPT,X'10'      END TIME ENTERED?                            
         BZ    HOOK20                                                           
         XC    DUB,DUB                                                          
         MVC   0(10,R3),=C'END TIME -'                                          
         MVC   DUB(2),ETIM                                                      
         GOTO1 UNTIME,PARAS,DUB,11(R3)                                          
         LA    R3,19(R3)                                                        
*                                                                               
HOOK20   EQU   *                                                                
         TM    PRINTOPT,X'30'                                                   
         BZ    HOOK30                                                           
         CLI   PURPART,C'Y'                                                     
         BNE   HOOK30                                                           
         MVC   0(27,R3),=C'*PARTIAL AIRINGS REQUESTED*'                         
         LA    R3,28(R3)                                                        
*                                                                               
HOOK30   EQU   *                                                                
         CLI   PURDAYFH+5,0                                                     
         BZ    HOOK40                                                           
         MVC   0(12,R3),=C'DAY FILTER ='                                        
         MVC   14(3,R3),PURDAYF                                                 
*                                                                               
HOOK40   EQU   *                                                                
         LA    R3,H7+41            NOW POINT TO WHERE                           
         LA    R5,H8+41            1ST DEMO SHOULD PRINT                        
         LA    R2,DEMOS                                                         
         ZIC   R6,NUMDEMS                                                       
HOOK60   CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
         GOTO1 DEMOCON,PARAS,(0,(R2)),(5,WORK),(0,DBLOCKD)                      
         CLI   1(R2),C'I'          RESET FOR NEXT TIME                          
         BNE   *+8                                                              
         MVI   1(R2),C'T'                                                       
*                                                                               
         MVC   0(5,R3),WORK        FORMAT - W1849SHARE                          
         MVC   0(5,R5),WORK+5                                                   
         CLI   UP,2                                                             
         BNE   HOOK70                                                           
         MVC   57(5,R3),WORK       2 UP FORMAT                                  
         MVC   57(5,R5),WORK+5                                                  
HOOK70   LA    R2,3(R2)                                                         
         LA    R3,7(R3)                                                         
         LA    R5,7(R5)                                                         
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
         EJECT                                                                  
*                                                                               
*        ADDRESSES, PATCH AREA, CONSTANTS AND LIT POOL                          
*                                                                               
RELO     DS    A                                                                
*                                                                               
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
*                                                                               
* CONSTANTS                                                                     
*                                                                               
NETDAYTB DS    0CL4                                                             
         DC    X'40',C'MON'                                                     
         DC    X'20',C'TUE'                                                     
         DC    X'10',C'WED'                                                     
         DC    X'08',C'THU'                                                     
         DC    X'04',C'FRI'                                                     
         DC    X'02',C'SAT'                                                     
         DC    X'01',C'SUN'                                                     
         DC    X'03',C'WKE'                                                     
         DC    X'7C',C'M-F'                                                     
         DC    X'7F',C'M-S'                                                     
         DC    X'90',C'VAR'                                                     
NETDAYS  EQU   (*-NETDAYTB)/L'NETDAYTB                                          
*                                                                               
DAYLIST  DC    C'MONTUEWEDTHUFRISATSUNM-FM-SWKEVARTYP'                          
         DC    XL3'00'                                                          
*   MY OWN ERROR MESSAGES                                                       
MANYBKS  DC    C'* ERROR * TOO MANY BOOKS - LIMIT IS 1'                         
MANYDEM  DC    C'* ERROR * TOO MANY DEMOS - LIMIT IS 9'                         
NOFOUND  DC    C'** ERROR ** RECORD NOT FOUND - '                               
TPSRC    DC    C'* ERROR * SRC IS ONLY VALID FOR TIME PERIOD'                   
MUSTBEYN DC    C'* INPUT MUST BE A Y OR AN N *'                                 
MUSTSOON DC    C'* THIS REQUEST MUST BE DONE AS SOON *'                         
*                                                                               
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
         SPROG 0,1                                                              
         PSPEC H1,001,AGYNAME                                                   
         PSPEC H1,048,C'LISTING OF PURE PROGRAMMING INVENTORY'                  
         PSPEC H1,099,REPORT                                                    
         PSPEC H1,115,PAGE                                                      
         PSPEC H2,001,REQUESTOR                                                 
         PSPEC H2,048,37C'-'                                                    
         PSPEC H2,099,RUN                                                       
         PSPEC H4,001,C'STATION   -'                                            
         PSPEC H4,099,C'SOURCE BOOK -'                                          
         SPACE 1                                                                
         SPROG 0            2 UP (DEFAULT)                                      
         PSPEC H7,01,C'TIME   PURE DAY PROGRAM          WKS QTR'                
         PSPEC H7,58,C'TIME   PURE DAY PROGRAM          WKS QTR'                
         PSPEC H8,01,C'----    NO. --- -------          --- HRS'                
         PSPEC H8,58,C'----    NO. --- -------          --- HRS'                
         SPACE 1                                                                
         SPROG 1            1 UP                                                
         PSPEC H7,01,C'TIME   PURE DAY PROGRAM          WKS QTR'                
         PSPEC H8,01,C'----    NO. --- -------          --- HRS'                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
*        RERESWRK --- REP RESEARCH PROG WORK AREA                               
*                                                                               
       ++INCLUDE RERESWRK                                                       
         EJECT                                                                  
*                                                                               
* DSECT TO COVER SCREEN                                                         
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RERESF1D                                                       
         EJECT                                                                  
* SAVE AREA VALUES                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE            LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
SAVE2    DS    F                   SAVE R2                                      
SAVER5   DS    F                   SAVE R5                                      
SAVER6   DS    F                   SAVE R6                                      
DAYBITS  DS    X                   SAVE ACTUAL BITS FOR DAY                     
COST     DS    F                   FOR CPP/CPM                                  
UP       DS    CL1                 1 UP OR 2 ON PAGE                            
LASTIME  DS    CL6                 LAST TIME PRINTED                            
NWKS     DS    CL1                 NUMBER OF WEEKS                              
PRINTOPT DS    XL1                 X'80'  MORE THAN 2 DEMOS                     
*                                  X'40'  UNUSED                                
*                                  X'20'  START TIME FILTER                     
*                                  X'10'  END TIME FILTER                       
SBUFF    DS    A                   START OF PRINT BUFFER                        
*                                                                               
STAMP    DS    CL8                 STORAGE STAMP                                
MYIOAREA DS    XL2000              NETWORK RECDS MAY BE 2000 BYTES              
SAVELN   EQU   *-SYSSPARE          SAVE AREA LENGTH                             
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026RERES01S  05/01/02'                                      
         END                                                                    
