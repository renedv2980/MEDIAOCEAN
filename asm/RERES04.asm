*          DATA SET RERES04    AT LEVEL 102 AS OF 04/24/09                      
*PHASE T81904C                                                                  
*INCLUDE REDEMLKU                                                               
*INCLUDE GETKSRC                                                                
         TITLE 'T81904 - RERES04 - COMBINED LAYOUT/TREND REPORT'                
*                                                                               
*******************************************************************             
*                                                                 *             
*        RERES04 - T81904 - PRODUCE RESEARCH LAYOUT REPORT        *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* FEB22/89 (MRR) --- TRAP TIME PERIOD DATA END-OF-FILE (MEANING   *             
*                    END OF BROADCAST DAY FOR THAT STATION) AND   *             
*                    THEN PUT A PROPER MESSAGE ON THE REPORT AND  *             
*                    RETURN THE USER THE 'NORMAL' REPORT SPOOLED  *             
*                    MESSAGE                                      *             
*                                                                 *             
* JAN10/90 (MRR) --- LIMIT REQUESTS SO THAT IF THE PRODUCT OF     *             
*                    BOOKS, STATIONS AND DAYPARTS IS MORE THAN    *             
*                    50, THIS MODULE CHAGES NOW TO SOON.          *             
*                                                                 *             
* FEB26/90 (MRR) --- BY-PASS SOON TEST IFF DDS TERMINAL           *             
*                                                                 *             
* APR12/90 (MRR) --- CALL FIXPAV FOR PAV RECORDS IN THE DEMAND    *             
*                     HOOK                                        *             
*                                                                 *             
* APR25/90 (MRR) --- BYPASS FIXPAV CALL UNLESS DECIMAL OPT IS SET *             
*                     TO ROUNDED                                  *             
*                                                                 *             
* MAY21/90 (MRR) --- BUG IN INDEX BOOK PROCESSING, STOP LOOP.     *             
*                                                                 *             
* SEP25/90 (MRR) --- >FIXPAV IS IUNDEM                            *             
*                    >CALL IUNDEM FOR EVERY DEMO RECORD           *             
*                    >CHANGE HEADER                               *             
*                    >REMOVE ROUNDED OPTION, ALWAYS PRINT 1-DEC   *             
*                                                                 *             
* NOV15/90 (MRR) --- >FIX AUTO NOW TO SOON DUE TO HEADER SCREEN   *             
*                     CHANGE FOR NOV90 1-DEC RELEASE              *             
*                                                                 *             
* JUN01/08 (KUI) --- 2-CHAR BOOKTYPE SUPPORT                      *             
*                                                                 *             
* APR22/09 (SMY) --- ADD ACOMFACS TO GETKSRC CALLS                *             
*                     (PART OF NEW INVENTORY KEY SUPPORT)         *             
*                                                                 *             
*******************************************************************             
T81904   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1904**,RA,R8,RR=R2                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
*                                                                               
         LH    RF,=Y(BUFF-SYSD)                                                 
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
**********************************************************************          
*VKEY -  VALIDATE KEY                                                           
**********************************************************************          
*                                                                               
VKEY     DS    0H                                                               
         MVI   PRINTOPT,0          CLEAR OUT PRINTOPT                           
         LA    RE,SYSSPARE                                                      
         LHI   RF,SAVELN                                                        
         XCEF  (RE),(RF)           CLEAR SYSSPARE FOR LEN IN RF                 
         MVC   STAMP,=CL8'T81904'  STAMP SAVEAREA                               
         LA    R2,LAYSRCEH         VALIDATE SOURCE                              
         GOTO1 VVALSRC                                                          
*                                                                               
         LA    R2,LAYFILEH         VALIDATE FILE                                
         GOTO1 VVALFILE            FILE IS IN SVFILE                            
         CLI   SVFILE,C'P'         PAV INVALID FOR SRC                          
         BNE   VKEY3                                                            
         CLI   SVSOURCE,C'S'       'SRC' IS VALID ONLY FOR TIME PERIOD          
         BNE   VKEY3                                                            
         MVC   CONHEAD(L'TPSRC),TPSRC                                           
         B     MYEND                                                            
*                                                                               
VKEY3    LA    R2,LAYBOOKH         VALIDATE BOOK                                
         MVI   MAX,10              DUMMY SO I CAN DO ERROR MSG                  
         BAS   RE,MYBKVAL                                                       
*                                                                               
VKEY4    MVC   NUMBOOK,ACTUAL      SAVE NUMBER OF BOOKS                         
         CLI   ACTUAL,1                                                         
         BNE   *+8                                                              
         OI    PRINTOPT,X'80'      1 BOOK                                       
         CLI   ACTUAL,8            REALLY 8 BOOKS ALLOWED                       
         BNH   VKEY5                                                            
         MVC   CONHEAD(L'MANYBKS),MANYBKS    TOO MANY BOOKS                     
         B     MYEND                                                            
*                                                                               
VKEY5    LA    R2,LAYDEMOH         VALIDATE DEMOS                               
         MVI   MAX,20              DUMMY SO I CAN DO ERROR MSG                  
         MVI   NFLDS,1                                                          
         GOTO1 VVALDEM                                                          
         CLI   ACTUAL,8            REALLY ALLOW 8 DEMOS                         
         BNH   VKEY6                                                            
         MVC   CONHEAD(L'MANYDEM),MANYDEM                                       
         B     MYEND                                                            
         SPACE 1                                                                
VKEY6    BRAS  RE,CHKUNIV                                                       
         SPACE 1                                                                
VKEY7    MVC   NUMDEMS,ACTUAL      SAVE NUMBER OF DEMOS                         
         SPACE 1                                                                
VKEY10   LA    R2,LAYSTATH         VALIDATE STATION                             
         LA    R3,6                                                             
         LA    R4,STATSV                                                        
         XC    STATSV,STATSV                                                    
         SR    R5,R5               COUNT NUMBER OF STATIONS                     
         LA    R6,MKTSV                                                         
         XC    MKTSV(144),MKTSV                                                 
         XC    MKTSV+144(144),MKTSV+144                                         
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
VKEY20   DS    0H                                                               
         GOTO1 VVALSTA                                                          
         MVC   0(5,R4),ACTSTAT                                                  
         CLI   SVMEDIA,C'N'                                                     
         BE    VKEY25                                                           
         GOTO1 VVALMKT                                                          
         MVC   0(29,R6),WORK+8     SAVE MARKET NAME                             
VKEY25   LA    R5,1(R5)                                                         
         LA    R4,5(R4)                                                         
         LA    R6,29(R6)                                                        
         BAS   RE,BUMP                                                          
         BCT   R3,*+8                                                           
         B     VKEY30                                                           
         CLI   5(R2),0             ANOTHER STATION?                             
         BNE   VKEY20                                                           
         SPACE 1                                                                
VKEY30   STC   R5,NUMSTAT                                                       
         LA    R2,LAYDAYH          VALIDATE DAY/DETAIL AND TIME FIELDS          
         GOTO1 VVALDYTM,PARAS,8    8 DAY/DETAIL FIELDS                          
         SPACE 1                                                                
         LA    R2,LAYNDXH          INDEX BOOK                                   
         CLI   5(R2),0             OPTIONAL INPUT FIELD                         
         BE    VKEY150                                                          
         MVI   MAX,20              DUMMY SO I CAN DO ERROR MSG                  
         MVC   BOOK+76(4),BOOK     SAVE 1ST BOOK VALUE                          
         GOTO1 VVALBOOK                                                         
         MVC   BOOK+72(4),BOOK     SAVE INDEX BOOK HERE                         
         MVC   BOOK(4),BOOK+76     RESTORE 1ST BOOK                             
         OI    PRINTOPT,X'40'      INDICATE INDEX BOOK                          
         CLI   ACTUAL,1            REALLY ONLY 1 BOOK ALLOWED                   
         BE    VKEY150                                                          
         MVC   CONHEAD(L'MANYBKS),MANYBKS                                       
         B     MYEND                                                            
         SPACE 1                                                                
VKEY150  LA    R2,LAYOPT1H         OPTIONS VALIDATION                           
         MVI   ONEWEEK,X'01'       1ST, SET DEFAULTS                            
         MVI   TWOWEEK,X'03'                                                    
         MVI   OPTFLG1,X'00'                                                    
                                                                                
         CLI   5(R2),0             CHECK FOR INPUT                              
         BE    VKEY200             IF NONE, SKIP VALIDATION                     
         BRAS  RE,VALOPTA          SCAN FIRST OPTIONS FIELD                     
         SPACE 1                                                                
*                                                                               
VKEY200  EQU   *                   UPGRADE?                                     
         XC    UPGELEM,UPGELEM                                                  
         LA    R2,LAYUPG1H                                                      
         CLI   5(R2),0             ANY INPUT TO UPGD FIELD?                     
         BE    VKEY202             NO                                           
         CLI   EST,0               YES -> WAS AN EST RQSTD?                     
         BNE   VKEY204             YES, GO VALIDATE UPGD EXPRESSION             
         MVC   CONHEAD(L'NOEST),NOEST  NO -) ERR: NO EST BK REQUESTED           
         B     MYEND                                                            
*                                                                               
VKEY202  CLI   EST,0               EST REQSTD W/OUT UPGD EXPR?                  
         BE    VKEY205                                                          
         MVC   CONHEAD(L'MISUPG),MISUPG    MISSING UPGD EXPRESSION              
         B     MYEND                                                            
*                                                                               
VKEY204  BAS   RE,VUPGR            VALIDATE ADDL OPTNS LINE                     
*                                                                               
VKEY205  EQU   *                                                                
         LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         MVI   DBTAPEP,0                                                        
         TM    RMPPROF+RMPIMPSB,RMPIMPSA                                        
         BZ    *+8                                                              
         MVI   DBTAPEP,C'Y'                                                     
         DROP  R4                                                               
*                                                                               
VKEY210  EQU   *                                                                
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
         LA    R2,LAYDAYH          COUNT DAY/TIME(S)                            
         SR    R3,R3                                                            
         LA    R5,8                LOOP FOR UP TO 8 DAY/TIME PAIRS              
VKEY215  EQU   *                                                                
         CLI   5(R2),0             INPUT?                                       
         BE    VKEY220             NO                                           
         LA    R3,1(R3)                                                         
         BAS   RE,BUMP             NOW POINTS TO TIME                           
         BAS   RE,BUMP             NOW POINTS TO DAY                            
         BCT   R5,VKEY215          LOOP                                         
VKEY220  EQU   *                                                                
         LA    R2,LAYSTATH         COUNT STATIONS                               
         SR    R4,R4                                                            
         LA    R5,6                LOOP FOR UP TO 6 STATIONS                    
VKEY230  EQU   *                                                                
         CLI   5(R2),0             INPUT?                                       
         BE    VKEY240                                                          
         LA    R4,1(R4)                                                         
         BAS   RE,BUMP             POINT TO THE NEXT STATION                    
         BCT   R5,VKEY230                                                       
VKEY240  EQU   *                                                                
         SR    R2,R2                                                            
         MR    R2,R4                                                            
         ZIC   R4,NUMBOOK          GET NUMBER OF BOOKS                          
         MR    R2,R4                                                            
         C     R3,=F'50'                                                        
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
*                                                                               
VKEY299  EQU   *                                                                
         SPACE 1                                                                
VKEYX    B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* PREP - PRINT DATA TYPE REPORT                                                 
* R4 POINTS AT DBLOCKA1 - MAIN DBLOCK AREA                                      
**********************************************************************          
*                                                                               
PREP     L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
                                                                                
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         CLC   STAMP,=CL8'T81904'  STOP IF STORAGE NOT STAMPED                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RCSUBPRG,0                                                       
         MVI   STATFLG1,0          INIT STATUS FLAGS                            
         MVI   FIRST,C'Y'          INDICATES TOP OF PAGE                        
         BAS   RE,CLEARBUF                                                      
         B     LAY00                                                            
*                                                                               
PREPX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*LAY00 - MAIN PROGRAM CONTROL I/O                                               
**********************************************************************          
LAY00    DS    0H                                                               
         LA    R2,DAYTMLST         LIST OF DAY/TIMES                            
         ST    R2,SVDAYTM                                                       
         SPACE 1                                                                
LAY20    MVC   SVDAY,0(R2)         SAVE DAY FOR PRINTING                        
         XC    PROGTMQ,PROGTMQ                                                  
*        MVC   STIM,1(R2)          START TIME                                   
*        MVC   ETIM,3(R2)          END TIME                                     
         MVC   DUB(2),1(R2)                                                     
         MVC   DUB+2(2),3(R2)                                                   
         BAS   RE,RNDTIME          ROUND TIMES TO NEAREST 1/2 HOUR              
         MVC   STIM,DUB            START TIME                                   
         MVC   ETIM,DUB+2          END TIME                                     
         MVC   HALF,ETIM                                                        
         BAS   RE,TOQTRHR          CONVERT END TIME TO QTR HR                   
         MVC   ETIMQ,BYTE          AND SAVE                                     
         OI    STATFLG1,QTRHRQ     INIT ITERATION TRACKER                       
         SPACE 1                                                                
LAY30    TM    OPTFLG1,QTRPTQ      USER CHOSE 1/4 HR REPORTING?                 
         BNO   LAY32                                                            
         BAS   RE,ADDQTR           YES, GO TO DEMAND WITH 1/4 HR CHUNKS         
         B     LAY34                                                            
                                                                                
LAY32    BAS   RE,ADDHLF           NO, GO TO DEMAND WITH 1/2 HR CHUNKS          
                                                                                
LAY34    MVI   COUNT,0             COUNT STATIONS THROUGH LOOP                  
         LA    R3,STATSV           LIST OF STATIONS IN DBSELSTA FMT             
         LA    R2,SVACTRMK         RATING SERVICE MARKET                        
         SPACE 1                                                                
LAY40    LA    R5,BOOK             LIST OF BOOKS                                
         MVI   BKCTR,1             WHICH BOOK WERE DOING IN LIST                
         L     R6,SBUFF                                                         
*                                                                               
         CLI   SVFILE,C'T'         TIME PERIOD USES REDEMLKU                    
         BE    TPLKUP              NOT DEMAND                                   
*                                                                               
         ZIC   R1,COUNT            DO WE WANT THIS 1/2 HR                       
         SLL   R1,3                                                             
         LA    R1,PROGTMQ(R1)                                                   
         ST    R1,SVPRGTMQ                                                      
         CLC   0(1,R1),STIMQ       FOR THIS BOOK                                
         BNH   LAY50               YES                                          
         ZIC   RE,COUNT            NO, FIND CORRECT PLACE IN TABLE              
         LR    RF,RE                                                            
         SLL   RE,4                                                             
         LA    RE,124(RE,R6)       AND INDICATE NO DEMOS WITH X'FF'             
         MVC   0(16,RE),XFF     (CONTRAST TO DEMO WITH VALUE OF ZERO)           
         B     LAY70               FOR THIS BOOK                                
         SPACE 1                                                                
*  BUILD REST OF DBLOCK                                                         
         SPACE 1                                                                
LAY50    MVI   GOTPROG,0           CLEAR GOT PROGRAM INDICATOR                  
         MVI   SVWKS,0             AND SAVE WEEKS INDICATOR                     
         LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
         MVC   DBSELBK,1(R5)       BOOK                                         
         MVC   DBBTYPE,3(R5)       BOOK TYPE                                    
         MVC   DBSELSTA,0(R3)                                                   
         CLC   DBSELSTA,=C'WUSAT'   PATCH FOR WDVM (PRE 7/86) --                
         BNE   *+20                   -- WUSA CALL LETTER SWITCH                
         CLC   DBSELBK,=X'5606'                                                 
         BH    *+10                                                             
         MVC   DBSELSTA,=C'WDVMT'                                               
         ZIC   R1,SVDAY                                                         
         SRL   R1,4                                                             
         LA    R1,DAYBITS(R1)                                                   
         MVC   DBSELDAY,0(R1)                                                   
         CLI   SVFILE,C'I'         INV IS IN TPT DAY FORMAT                     
         BNE   *+10                                                             
         MVC   DBSELDAY,SVDAY                                                   
         CLI   DBSELDAY,3          IS IS SA-SU                                  
         BNE   *+8                                                              
         MVI   DBDAYOPT,C'Y'       INDICATES SA-SU AVERAGE LINE                 
         MVC   DBSELTIM(2),STIM                                                 
         MVC   DBSELTIM+2(2),HALFHR                                             
*                                                                               
LAY55    MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBSELSRC,SVSOURCE                                                
         MVC   DBSELRMK,0(R2)      RATING SERVICE MARKET                        
         MVI   DBBEST,C'A'                                                      
*                                                                               
LAY56    CLI   DBSELMED,C'U'       MEDIA = (U)PGRADE FOR INV FILE               
         BNE   LAY57                                                            
         MVC   DBFILE,=C'IUN'                                                   
         XC    IBLK,IBLK                                                        
         MVC   IBLK+3(1),0(R5)     BOOKVAL BITS                                 
         MVC   IBLK+4(1),3(R5)       BOOKTYPE                                   
         GOTO1 =V(GETKSRC),DMCB,(C'B',IBLK),OBLK,ACOMFACS,RR=RELO               
         CLI   DMCB+4,0            ANY ERRORS?                                  
         BE    *+14                INVALID FIELD                                
         MVC   CONHEAD(L'INVBOK),INVBOK   INVALID BOOK                          
         B     MYEND                                                            
         MVC   DBSTYPE,OBLK+2      SET RINVKSRC                                 
         LA    RF,DBEXTRA1                                                      
         XC    DBEXTRA1,DBEXTRA1                                                
         MVC   0(4,RF),=C'RINV'                                                 
         ST    RF,DBEXTEND                                                      
         B     LAY58                                                            
*                                                                               
LAY57    LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
*                                                                               
LAY58    L     RF,DBCOMFCS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     R1,0(R1)                                                         
         TM    FATFLAG-FACTSD(R1),X'01'       ON MEANS OFFLINE                  
         BO    LAY60                                                            
         SPACE 1                                                                
         LH    RE,FATIOCNT-FACTSD(R1)   IF ONLINE, AND IO COUNT IS              
         LA    RE,50(RE)                WITHIN 50 OF LIMIT, THEN                
         CH    RE,FATMAXIO-FACTSD(R1)                                           
         BNH   LAY60                                                            
         LA    R2,LAYSRCEH                                                      
         MVC   CONHEAD(L'TOOBIG),TOOBIG      REQUEST IS TOO BIG                 
         B     MYEND                                                            
*                                                                               
LAY60    DS    0H                                                               
         GOTO1 DEMAND,DMCB,DBLOCKD,PVFILL                                       
*                                                                               
LAY62    CLI   DBERROR,X'10'       RECORD NOT FOUND                             
         BNE   LAY65                                                            
         MVC   CONHEAD(L'NOFOUND),NOFOUND                                       
         MVC   CONHEAD+L'NOFOUND(5),0(R3)    SHOW STATION                       
         L     RE,ASPOOLD                                                       
         USING SPOOLD,RE                                                        
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
*                                                                               
         GOTO1 GETBTYPE,DMCB,(3(R5),0)                                          
         CLI   DMCB,0                                                           
         BE    LAY63                                                            
                                                                                
         MVI   5(R2),C'('                                                       
                                                                                
         ZIC   R1,DMCB                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R2),DMCB+2                                                   
                                                                                
         LA    R2,7(R1,R2)                                                      
         MVI   0(R2),C')'                                                       
*                                                                               
LAY63    LA    R2,LAYSTATH                                                      
         B     MYEND                                                            
         DROP  RE                                                               
         SPACE 1                                                                
LAY65    EQU   *                                                                
         MVI   DBDAYOPT,0          RESET FOR NEXT TIME                          
         OC    DBDIVSOR,DBDIVSOR   TEST FOR ANY RECORDS                         
         BZ    LAY69               NO                                           
         CLI   DBERROR,X'80'       TEST FOR EOF                                 
         BE    LAY70                                                            
         DC    H'0'                 SOMETHING WRONG                             
LAY69    EQU   *                                                                
         ZIC   RE,COUNT            FIND CORRECT PLACE IN TABLE                  
         SLL   RE,4                                                             
         LA    RE,124(RE,R6)       AND INDICATE NO DEMOS WITH X'FF'             
         MVC   0(16,RE),XFF    (CONTRAST TO DEMO WITH VALUE OF ZERO)            
         SPACE 1                                                                
LAY70    EQU   *                                                                
         LA    RF,BOOK+72          IF WE JUST DID THE INDEX BOOK,               
         CR    RF,R5                GET OUT                                     
         BE    LAY80                                                            
*                                                                               
         LA    R5,4(R5)            NEXT BOOK                                    
         ZIC   R1,BKCTR                                                         
         LA    R1,1(R1)                                                         
         STC   R1,BKCTR                                                         
         LA    R6,220(R6)          NEXT BUFFER AREA                             
         OC    0(4,R5),0(R5)                                                    
         BZ    LAY75                                                            
         SPACE 1                                                                
         CLI   SVFILE,C'T'         TIME PERIOD USES RDEMLKU                     
         BE    TPLKUP              NOT DEMAND                                   
         SPACE 1                                                                
         L     R1,SVPRGTMQ                                                      
         LA    R1,1(R1)            TIME FOR NEXT BOOK                           
         ST    R1,SVPRGTMQ                                                      
         CLC   0(1,R1),STIMQ       DO WE WANT THIS 1/2 HR FOR THIS BOOK         
         BNH   LAY50               YES                                          
         ZIC   RE,COUNT            NO, FIND CORRECT PLACE IN TABLE              
         SLL   RE,4                                                             
         LA    RE,124(RE,R6)       AND INDICATE NO DEMOS WITH X'FF'             
         MVC   0(16,RE),XFF     (CONTRAST TO DEMO WITH VALUE OF ZERO)           
         B     LAY70               FOR THIS BOOK                                
         SPACE 1                                                                
LAY75    TM    PRINTOPT,X'04'      HAVE WE DONE INDEX BOOK YET                  
         BO    LAY80                                                            
         OI    PRINTOPT,X'04'      NOW WE HAVE                                  
         TM    PRINTOPT,X'40'      IS THERE AN INDEX BOOK                       
         BZ    LAY80                                                            
         TM    PRINTOPT,X'10'      ONLY DO INDEX IF                             
         BZ    LAY80               RECORDS FOR THIS 1/2 HR.                     
*                                                                               
         LA    R5,BOOK+72          POINT TO INDEX BOOK                          
         L     R6,SBUFF                                                         
         LA    R6,2200(R6)         AND TO INDEX BUFF DEMOS AREA                 
         SPACE 1                                                                
         CLI   SVFILE,C'T'         TIME PERIOD USES RDEMLKU,                    
         BE    TPLKUP              NOT DEMAND                                   
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
         BAS   RE,SPLAT            PRINT WHAT WE'VE GOT SO FAR                  
                                                                                
         XI    STATFLG1,QTRHRQ     FLIP ITERATION BIT                           
                                                                                
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
         BAS   RE,ALLSTARS         PRINT CLOSING STARS                          
         GOTO1 SPOOL,PARAS,ASPOOLD                                              
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
         MVI   FORCEHED,C'Y'       STARTS NEW PAGE                              
         MVI   FIRST,C'Y'          INDICATES TOP OF PAGE                        
         OC    0(5,R2),0(R2)       NEXT DAY/TIME                                
         BNZ   LAY20                                                            
         DROP  R4                                                               
         B     XIT                                                              
         SPACE 3                                                                
DAYBITS  DC    X'7C402010080402017F03'                                          
*                                                                               
* BITS REPRESENT M-F,MON,TUE,WED,THUR,FRI,SAT,SUN,M-SU,SA-SU                    
         EJECT                                                                  
*******************************************************************             
*TPLKUP -  TIME PERIOD LOOK UP                                                  
*******************************************************************             
         SPACE 2                                                                
TPLKUP   DS    0H                                                               
*                                                                               
TPLK10   LA    R4,BLOCK                                                         
         USING REDBLKUD,R4                                                      
         XC    REDLBLKU(REDLBLKL),REDLBLKU                                      
         MVI   REDLMOD,REDLRD      READ RECORDS FROM DEMAND                     
         MVC   REDLAFAC,ACOMFACS                                                
         MVC   REDLADFN,DEFINE                                                  
         MVC   REDLIUN,VGETIUN                                                  
         MVC   REDLOFMT,AOFORMAT                                                
         MVC   REDLREP,AGENCY      FOR MARKET SECURITY                          
         MVC   REDLSTA,0(R3)       STATION                                      
         MVC   REDLMED,SVMEDIA     N=NETOWRK, T=TV                              
         MVC   REDLFIL,SVFILE                                                   
         MVC   REDLSRC,SVSOURCE                                                 
         LA    R1,DEMOS                                                         
         ST    R1,REDLALST                                                      
         LA    R1,BLOCK1                                                        
         ST    R1,REDLAVAL                                                      
         TM    RMPPROF+RMPIMPSB,RMPIMPSA                                        
         BZ    *+8                                                              
         OI    REDLOPT,X'40'                                                    
         CLC   1(2,R5),=X'450C'    DEC69=DUMMY BK --> GET LATEST BK             
         BE    *+16                                                             
         MVC   REDLDBK,1(R5)       BOOK                                         
         MVC   REDLBTY,3(R5)       BOOK TYPE                                    
*                                                                               
*  SPECIAL PATCH FOR WDVM (PRE 7/86)-WUSA CALL LETTER SWITCH                    
         CLC   REDLSTA,=C'WUSAT'                                                
         BNE   TPLK30                                                           
         CLC   REDLDBK,=X'5606'                                                 
         BH    TPLK30                                                           
         MVC   REDLSTA,=C'WDVMT'                                                
*                                                                               
TPLK30   MVC   REDLDAY,SVDAY       TP DAY DOESN'T NEED ADJUSTING                
         MVC   REDLSTIM,STIM                                                    
         MVC   REDLETIM,HALFHR                                                  
         XC    REDLPROG,REDLPROG                                                
         MVI   NODEMFLG,0                                                       
         XC    REDUPGEL,REDUPGEL                                                
         LA    R1,UPGELEM                                                       
         CLC   BKCTR,EST           IS THIS AN EST BK?                           
         BNE   *+8                                                              
         ST    R1,REDUPGEL                                                      
*                                                                               
         GOTO1 =V(REDEMLKU),DMCB,(R4),RR=RELO                                   
*                                                                               
TPLK35   TM    REDLDBER,X'90'       RECORD NOT FOUND OR END OF FILE             
         BZ    TPLK90                                                           
         TM    REDLDBER,X'10'      EOF ?                                        
         BO    TPLK40              NO, ITS A REAL ERROR                         
         MVC   REDLPROG(16),=C'*** OFF AIR  ***'                                
         MVI   NODEMFLG,1          NO DEMOS TO PRINT                            
         B     TPLK90                                                           
TPLK40   EQU   *                                                                
         MVC   CONHEAD(L'NOFOUND),NOFOUND                                       
         MVC   CONHEAD+L'NOFOUND(5),0(R3)   SHOW STATION                        
         LA    R2,CONHEAD+L'NOFOUND+6                                           
         MVI   0(R2),C'/'                                                       
         LA    R2,1(R2)                                                         
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
         ZIC   R1,2(R5)            AND BOOK                                     
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   0(3,R2),0(R1)                                                    
         EDIT  (1,1(R5)),(2,3(R2))                                              
         CLI   3(R5),0             SPECIAL BOOK TYPE                            
         BE    TPLK60                                                           
*                                                                               
         GOTO1 GETBTYPE,DMCB,(3(R5),0)                                          
         CLI   DMCB,0                                                           
         BE    TPLK60                                                           
                                                                                
         MVI   5(R2),C'('                                                       
                                                                                
         ZIC   R1,DMCB                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R2),DMCB+2                                                   
                                                                                
         LA    R2,7(R1,R2)                                                      
         MVI   0(R2),C')'                                                       
*                                                                               
         DROP  R4                                                               
         SPACE 1                                                                
TPLK60   LA    R2,LAYSTATH                                                      
         B     MYEND                                                            
         SPACE 1                                                                
TPLK90   BAS   RE,TPFILL                                                        
         OI    PRINTOPT,X'10'      RECORDS TO PRINT                             
*                                                                               
         B     LAY70               RTN TO MAIN LP (NEXT TIME/BK ETC)            
*                                                                               
         EJECT                                                                  
**********************************************************************          
*TPFILL-  'HOOK' LIKE ROUTINE FOR TPT RECORDS (NOT A DEMAND HOOK)               
**********************************************************************          
TPFILL   NTR1                                                                   
         USING BUFFD,R6                                                         
         USING REDBLKUD,R4                                                      
*                                                                               
         MVC   BUFFBK,0(R5)        BOOK                                         
         CLC   BKCTR,EST                                                        
         BNE   *+10                                                             
         MVC   BUFFBK,=C'EST '                                                  
         ZIC   R5,COUNT                                                         
         MH    R5,=H'20'                                                        
         LA    R5,BUFFPROG(R5)                                                  
         ZIC   R3,COUNT                                                         
         SLL   R3,4                                                             
         LA    R3,BUFFDEM(R3)                                                   
         MVC   0(16,R5),REDLPROG                                                
*                                                                               
         TM    PRINTOPT,X'08'      ****TEMP FIX FOR SHOW WEEKS****              
         BO    TPFL10              ****TEMP FIX FOR SHOW WEEKS****              
         CLI   12(R5),C'('         NEVER SHOW WEEKS AS (X)                      
         BNE   TPFL10              ****TEMP FIX FOR SHOW WEEKS****              
         CLI   14(R5),C')'                                                      
*        BNE   TPFL5                                                            
         BNE   TPFL10              ****TEMP FIX FOR SHOW WEEKS****              
         MVC   12(3,R5),=C'   '                                                 
         B     TPFL10              ****TEMP FIX FOR SHOW WEEKS****              
         SPACE 1                                                                
TPFL5    TM    PRINTOPT,X'08'      OPTIONS TO SHOW WEEKS AS - AND 0             
         BZ    TPFL10                                                           
         BAS   RE,COUNTWK                                                       
*                                                                               
         MVC   16(4,R5),=C'++++'                                                
         TM    WORK,X'08'                                                       
         BZ    *+8                                                              
         MVI   16(R5),C'-'                                                      
         SPACE 1                                                                
         TM    WORK,X'04'                                                       
         BZ    *+8                                                              
         MVI   17(R5),C'-'                                                      
         SPACE 1                                                                
         TM    WORK,X'02'                                                       
         BZ    *+8                                                              
         MVI   18(R5),C'-'                                                      
         SPACE 1                                                                
         TM    WORK,X'01'                                                       
         BZ    *+8                                                              
         MVI   19(R5),C'-'                                                      
         SPACE 1                                                                
TPFL10   CLI   NODEMFLG,1          DO WE WANT TO DO DEMOS?                      
         BNE   TPFL15                                                           
         ZIC   RE,COUNT                                                         
         SLL   RE,4                                                             
         LA    RE,124(RE,R6)                                                    
         MVC   0(16,RE),XFF                                                     
         B     XIT                                                              
*                                                                               
TPFL15   LA    R2,DEMOS                                                         
         LA    R5,BLOCK1                                                        
         ZIC   RE,NUMDEMS                                                       
         SPACE 2                                                                
TPFL20   EQU   *                                                                
         MVC   0(2,R3),2(R5)       IF NON-ROUNDED DEMO, USE AS IS               
         SPACE 2                                                                
TPFL80   LA    R2,3(R2)            NEXT DEMO                                    
         LA    R3,2(R3)            NEXT DEMO AREA                               
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         BCT   RE,TPFL20                                                        
         SPACE 2                                                                
         B     XIT                                                              
         SPACE 2                                                                
         DROP  R4,R6                                                            
         EJECT                                                                  
**********************************************************************          
*PVFILL - DEMAND HOOK FOR PAV (PRORGRAM) RECORDS                                
**********************************************************************          
PVFILL   NTR1                                                                   
         USING BUFFD,R6                                                         
         USING DBLOCKD,R4                                                       
         CLI   DBSELMED,C'N'       NETWORK OR IUN DON'T DO IUNDEM               
         BE    FILL5                                                            
         CLI   DBSELMED,C'U'                                                    
         BE    FILL5                                                            
         GOTO1 VIUNDEM,DMCB,DBLOCKD                                             
*                                                                               
FILL5    ZIC   R2,COUNT            SAVE RATING SERVICE MARKET                   
         SLL   R2,1                                                             
         LA    R2,SVACTRMK(R2)                                                  
         MVC   0(2,R2),DBACTRMK                                                 
         SPACE 1                                                                
FILL10   CLI   GOTPROG,C'D'        DO WE HAVE PROGRAM WE WANT                   
         BE    PVFILX              YES, GET NEXT PROGRAM                        
         SPACE 1                                                                
         ZIC   R2,COUNT            STATION NUMBER                               
         MH    R2,=H'20'           FIND PLACE IN TABLE                          
         LA    R2,BUFFPROG(R2)     PROGS ARE 20 EACH                            
         ZIC   R3,COUNT                                                         
         SLL   R3,4                                                             
         LA    R3,BUFFDEM(R3)      DEMOS ARE 16 EACH                            
*                                                                               
         CLI   DBSELMED,C'U'       INV HAS PRG NAME IN DBEXTED                  
         BNE   FILL11                                                           
         ICM   R1,15,DBEXTEND                                                   
         USING DBXINVWK,R1                                                      
         CLC   0(4,R1),=C'RINV'                                                 
         BNE   FILL12                                                           
         MVC   WORK+10(L'DBXIPROG),DBXIPROG                                     
         B     FILL12                                                           
         DROP  R1                                                               
*                                                                               
FILL11   GOTO1 DEFINE,PARAS,=C'PROGRAM',DBLOCKD,WORK+10                         
         CLI   DBSELMED,C'N'                                                    
         BE    *+14                                                             
         CLC   WORK+19(5),=C'(NOR)'   IF (NOR), ALWAYS EXCLUDE                  
         BE    PVFILX                                                           
*                                                                               
FILL12   BAS   RE,SELECT           NOW SEE IF WE WANT THIS ONE                  
         CLI   WANT,C'N'                                                        
         BE    PVFILX                                                           
         OI    PRINTOPT,X'10'      INDICATE RECORDS TO PRINT                    
         MVC   0(16,R2),WORK+10    MOVE IN PROGRAM NAME                         
         MVC   BUFFBK,0(R5)        BOOK                                         
         CLC   BKCTR,EST                                                        
         BNE   *+10                                                             
         MVC   BUFFBK(4),=C'EST '                                               
*                                                                               
         TM    PRINTOPT,X'08'      SHOW WEEKS                                   
         BZ    FILL15                                                           
         CLI   SVSOURCE,C'A'       IF ARB                                       
         BNE   *+14                                                             
         CLC   BUFFBK+1(2),=X'5605'     AND BOOK IS BEFORE MAY86                
         BL    FILL15              DON'T SHOW WEEKS                             
         MVC   16(4,R2),=C'++++'                                                
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
FILL15   DS    0H                                                               
         TM    PRINTOPT,X'40'      IS THERE AN INDEX BOOK                       
         BO    FILL18              YES, DON'T SAVE END QTR HOUR                 
         GOTO1 DEFINE,PARAS,=C'TIME',DBLOCKD,WORK                               
         MVC   LASTTIME(2),WORK+4  STORE END TIME    <-- ROUND ????             
         SPACE 1                                                                
FILL18   DS    0H                                                               
         CLI   DBSELMED,C'N'       FOR NETWORK AND IUN ALWAYS CHG TO            
         BE    FILL20                INV FORMAT                                 
         CLI   DBSELMED,C'U'                                                    
         BE    FILL20                                                           
         CLC   BKCTR,EST           ALSO IF THERE'S A PAV UPGRADE                
         BNE   FILL70                                                           
         OC    UPGELEM,UPGELEM     UPGRADE REQUESTED?                           
         BZ    FILL70              NO                                           
FILL20   BAS   RE,CHGINV           RECD ->INV FMT & GET DEMOS                   
         B     FILL75                                                           
*                                                                               
FILL70   MVC   SAVBK,DBACTBK                                                    
         ZIC   R1,NUMDEMS                                                       
         MH    R1,=H'3'                                                         
         LA    R1,DEMOS(R1)                                                     
         MVI   0(R1),X'FF'                                                      
         LA    R2,DEMOS                                                         
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCKD,BLOCK1                           
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DBACTBK,SAVBK       RESTORE IT                                   
*                                                                               
FILL75   LA    R5,BLOCK1           NOW POINT R5 TO VALUES                       
         LA    R2,DEMOS                                                         
         ZIC   RE,NUMDEMS                                                       
*                                                                               
FILL80   EQU   *                                                                
         MVC   0(2,R3),2(R5)       IF NON-ROUNDED DEMO, USE AS IS               
         SPACE 2                                                                
FILL85   LA    R2,3(R2)            NEXT DEMO                                    
         LA    R3,2(R3)            NEXT DEMO AREA                               
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         BCT   RE,FILL80                                                        
*                                                                               
PVFILX   B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
**********************************************************************          
*CHGINV - CALL LKUP ROUTINE TO PROCESS RECORD                                   
**********************************************************************          
CHGINV   NTR1                                                                   
         LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         LA    R3,BLOCK            BUILD REDBLOCK                               
         USING REDBLKUD,R3                                                      
         XC    REDLBLKU(REDLBLKL),REDLBLKU                                      
         MVI   REDLMOD,REDLPRC     PROCESS RECORD IN OUR IOAREA                 
         STCM  R4,15,REDLDBLK         PASS ADDRESS OF DBLCOK                    
         MVC   REDLAFAC,ACOMFACS                                                
         MVC   REDLADFN,DEFINE                                                  
         MVC   REDLIUN,VGETIUN                                                  
         MVC   REDLOFMT,AOFORMAT                                                
         MVC   REDLREP,AGENCY      FOR MARKET SECURITY                          
         MVC   REDLSTA,DBACTSTA    STATION                                      
         MVC   REDLMED,SVMEDIA     N=NETOWRK, T=TV                              
         MVC   REDLFIL,SVFILE                                                   
         MVC   REDLSRC,SVSOURCE                                                 
         LA    R1,DEMOS                                                         
         ST    R1,REDLALST                                                      
         LA    R1,BLOCK1                                                        
         ST    R1,REDLAVAL                                                      
         TM    RMPPROF+RMPIMPSB,RMPIMPSA                                        
         BZ    *+8                                                              
         OI    REDLOPT,X'40'                                                    
         MVC   REDLDBK,DBACTBK     BOOK                                         
         MVC   REDLBTY,DBBTYPE     BOOK TYPE                                    
         MVC   REDLDAY,DBACTDAY                                                 
         BAS   RE,TDURTIM          CALC PRG TIMES FROM TOT DURATION             
         MVC   REDLTIM(2),WORK+2    START TIME                                  
         MVC   REDLTIM+2(2),WORK+4  END TIME                                    
         OC    REDLTIM(2),REDLTIM                                               
         BNZ   *+10                                                             
         MVC   REDLTIM(2),=H'2400'                                              
         OC    REDLTIM+2(2),REDLTIM+2                                           
         BNZ   *+10                                                             
         MVC   REDLTIM+2(2),=H'2400'                                            
         XC    REDUPGEL,REDUPGEL   UPGRADE?                                     
         LA    R1,UPGELEM                                                       
         CLC   BKCTR,EST           IS THIS AN EST BK?                           
         BNE   *+8                                                              
         ST    R1,REDUPGEL                                                      
         GOTO1 =V(REDEMLKU),DMCB,(R3),RR=RELO                                   
         TM    REDLDBER,X'90'      RECORD NOT FND OR END OF FILE                
         BZ    CHGINVX                                                          
         TM    REDLDBER,X'10'      EOF ?                                        
         BO    CHGINVX             NO, ITS A REAL ERROR                         
*                                                                               
CHGINVX  B    XIT                                                               
         EJECT                                                                  
**********************************************************************          
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
         CLI   SVSOURCE,C'A'                                                    
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
         L     R6,SBUFF                                                         
         USING BUFFD,R6                                                         
         ZIC   R3,NUMBOOK                                                       
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
*        ZIC   R4,NUMSTAT                                                       
         SPACE 2                                                                
         BAS   RE,MANY             SEE HOW MANY LINES TO PRINT                  
         CLI   FIRST,C'Y'          TOP OF PAGE-IT MUST FIT                      
         BE    SP10                                                             
         ZIC   R1,LINE             ELSE, SEE IF BLOCK WILL FIT ON PAGE          
         ZIC   R0,ALLOWLIN                                                      
         AR    R1,R0                                                            
         IC    R0,MAXLINES                                                      
         CR    R1,R0               IF THIS BLOCK WON'T FIT,                     
         BH    SP05                THEN START NEW PAGE                          
         TM    OPTFLG1,PBHQ        IF USER REQUEST 1/2HR PG BREAKS              
         BNO   SP10                                                             
         TM    OPTFLG1,QTRPTQ      CHECK IF THEY'RE DOING 1/4HR RPT             
         BNO   SP05                NO, JUST START NEW PAGE                      
         TM    STATFLG1,QTRHRQ     YES, CHK ITERATION BIT                       
         BNO   SP10                  (1=ON 1/2 HR, 0=ON 1/4HR)                  
*                                  IF, ITERATION ON 1/2 HR                      
SP05     MVI   FIRST,C'Y'          THEN START NEW PAGE                          
         MVI   ALLOWLIN,0                                                       
         BAS   RE,ALLSTARS         PRINT CLOSING STARS                          
         GOTO1 SPOOL,PARAS,ASPOOLD                                              
         MVI   FORCEHED,C'Y'       AND START NEW PAGE                           
         SPACE 1                                                                
SP10     BAS   RE,ALLSTARS                                                      
         GOTO1 SPOOL,PARAS,ASPOOLD                                              
         SPACE 1                                                                
SP20     MVI   FIRST,C'N'                                                       
         BAS   RE,DUPES            TAKE OUT DUPLICATE NAMES                     
         BAS   RE,DAYTM            FORMAT DAY AND TIME                          
         TM    PRINTOPT,X'10'      ANY RECORDS TO PRINT                         
         BO    SP30                                                             
         SR    R5,R5               NO, THEN MAKE BOX SIZE AS IF                 
         LA    R5,1(R5)            PROGRAM NAME                                 
         ZIC   RE,NUMDEMS          AND 1 BOOK                                   
         LA    RE,1(RE)                                                         
         SRL   RE,1                                                             
         AR    R5,RE                                                            
*                                                                               
SP25     LA    R1,P                                                             
         BAS   RE,SUMSTARS                                                      
         GOTO1 SPOOL,PARAS,ASPOOLD                                              
         BCT   R5,SP25                                                          
         B     XIT                 NO, JUST PRINT DAY/TIME                      
         SPACE 1                                                                
SP30     BAS   RE,PROGS            YES, DIG OUT PROGRAMS TO P                   
         CLC   P,SPACES                                                         
         BE    SP50                                                             
         OC    P,SPACES                                                         
         LA    R1,P                                                             
         BAS   RE,SUMSTARS                                                      
         GOTO1 SPOOL,PARAS,ASPOOLD                                              
         SPACE 2                                                                
SP50     LA    R2,P+2                                                           
         OC    0(4,R6),0(R6)       BOOK                                         
         BZ    SP130                                                            
         TM    PRINTOPT,X'80'      1 BOOK                                       
         BO    SP70                                                             
         LA    R5,BUFFBK                                                        
         CLC   BUFFBK,=C'EST '                                                  
         BNE   *+14                                                             
         MVC   0(4,R2),BUFFBK                                                   
         B     SP70                                                             
*MTA--                                                                          
         CLI   SVFILE,C'I'                                                      
         BNE   SP60                                                             
         CLI   2(R5),0                                                          
         BNE   *+14                                                             
         MVC   0(3,R2),=C'EST'                                                  
         B     SP65                                                             
         XC    IBLK,IBLK                                                        
         MVC   IBLK+3(1),0(R5)     BOOKVAL BITS                                 
         MVC   IBLK+4(1),3(R5)     BOOKTYPE                                     
         GOTO1 =V(GETKSRC),DMCB,(C'B',IBLK),OBLK,ACOMFACS,RR=RELO               
         CLI   DMCB+4,0            ANY ERRORS?                                  
         BNE   SP60                INVALID FIELD                                
         MVC   0(1,R2),OBLK+1                                                   
         CLI   OBLK+1,C' '         IF ACTUAL BK, DON'T BUMP                     
         BE    *+8                                                              
         LA    R2,1(R2)            BUMP OUTPUT PTR TO DO REST OF BK             
*                                                                               
SP60     ZIC   R1,2(R5)            CONVERT MONTH                                
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   0(3,R2),0(R1)                                                    
SP65     EDIT  (1,1(R5)),(2,3(R2))                                              
         CLI   3(R5),0             SPECIAL BOOK TYPE                            
         BE    SP70                                                             
         MVC   5(3,R2),=C'( )'                                                  
         MVC   6(1,R2),3(R5)                                                    
*                                                                               
         GOTO1 GETBTYPE,DMCB,(3(R5),0)                                          
         CLI   DMCB,0                                                           
         BE    SP70                                                             
                                                                                
         ZIC   R1,DMCB                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R2),DMCB+2                                                   
                                                                                
         LA    R2,7(R1,R2)                                                      
         MVI   0(R2),C')'                                                       
*                                                                               
         SPACE 1                                                                
SP70     TM    PRINTOPT,X'08'      SHOW WEEKS                                   
         BZ    SP75                                                             
         LA    R1,4(R6)            POINT TO PROGRAMS IN BUFF                    
         LA    RE,P+13                                                          
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
SP90     GOTO1 SPOOL,PARAS,ASPOOLD                                              
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
SP120    GOTO1 SPOOL,PARAS,ASPOOLD                                              
         SPACE 2                                                                
SP130    LA    R6,220(R6)          NEXT BOOK                                    
         CLI   LINE,50             IF NEAR BOTTOM OF PAGE                       
         BNH   SP135                                                            
         STC   R3,BYTEA                                                         
         CLI   BYTEA,1             AND MORE BOOKS TO PRINT                      
         BE    SP135                                                            
         BAS   RE,ALLSTARS         PRINT STARS TO CLOSE PAGE                    
         GOTO1 SPOOL,PARAS,ASPOOLD                                              
         MVI   FORCEHED,C'Y'       START NEW PAGE,                              
         BAS   RE,ALLSTARS         PRINT STARS AT TOP OF PAGE,                  
         GOTO1 SPOOL,PARAS,ASPOOLD                                              
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
         ZIC   R0,NUMSTAT                                                       
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
         L     R6,SBUFF                                                         
         LA    R6,4(R6)                                                         
         LA    R5,220(R6)                                                       
         BCT   R3,DUP10                                                         
         B     XIT                                                              
         SPACE 2                                                                
DUP10    BAS   RE,DUP20                                                         
         LA    R6,20(R6)                                                        
         LA    R5,20(R5)                                                        
         BCT   R0,DUP10                                                         
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
         DROP  R4                                                               
         EJECT                                                                  
DAYTM    NTR1                                                                   
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
         CLI   SVFILE,C'T'       TP  PRINTS TIME IN BODY OF REPORT              
         BE    DT10                                                             
         CLI   SVFILE,C'I'       INV PRINTS TIME IN BODY OF REPORT              
         BE    DT10                                                             
         ZIC   R1,SVDAY            PRINT DAY/TIME ON 1ST LINE                   
         SRL   R1,2                ACTUALLY SRL 4, THEN SLL 2                   
         LA    R1,DAYTBL(R1)                                                    
         MVC   P+1(3),0(R1)                                                     
         CLC   P+1(3),=C'SA-'      INSTEAD OF SA-SU,                            
         BNE   *+10                                                             
         MVC   P+1(3),=C'WKE'      PRINT WKE                                    
         SPACE 1                                                                
         XC    WORK(4),WORK        ONLY WANT START TIME                         
         MVC   WORK(2),STIM                                                     
         GOTO1 UNTIME,PARAS,WORK,P+5                                            
         B     DTX                                                              
         SPACE 1                                                                
DT10     XC    WORK(4),WORK        ONLY WANT START TIME                         
         MVC   WORK(2),STIM                                                     
         GOTO1 UNTIME,PARAS,WORK,P+1                                            
         SPACE 1                                                                
DTX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              CALCULATION OF HOW MANY LINES WILL PRINT                         
         SPACE 1                                                                
MANY     NTR1                                                                   
         USING BUFFD,R6                                                         
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
         SR    R5,R5               COUNT LINES IN R5                            
         SPACE 2                                                                
MAN10    CLC   BUFFPROG(120),SPACES    ANY PROGS TO PRINT                       
         BE    *+8                                                              
         LA    R5,1(R5)                                                         
         OC    BUFFDEM(96),BUFFDEM                                              
         BZ    MAN20                                                            
         ZIC   RE,NUMDEMS                                                       
         LA    RE,1(RE)                                                         
         SRL   RE,1                                                             
         AR    R5,RE                                                            
*                                                                               
         L     R2,SBUFF                                                         
         LA    R2,2324(R2)                                                      
         OC    0(96,R2),0(R2)     ANY INDEX DEMOS                               
         BZ    MAN20                                                            
         ZIC   RE,NUMDEMS                                                       
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
         DROP  R4,R6                                                            
         EJECT                                                                  
*              BUFF HANDLING ROUTINES                                           
         SPACE 2                                                                
* INDEX BOOK INFO IS ALWAYS STORED AT BUFF+2200                                 
* (INDEX BOOK IS AT BOOK+72)                                                    
         SPACE 2                                                                
INDXBUFF NTR1                                                                   
         LA    R6,124(R6)          R6=A(LINE TO BE INDEXED)                     
         L     R3,SBUFF                                                         
         LA    R3,2324(R3)         R3=A(INDEX DEMOS)                            
         LA    R4,48               8 DEMOS X 6 STATIONS                         
         SPACE 2                                                                
IND10    SR    R1,R1                                                            
         ICM   R1,3,0(R6)                                                       
*                                                                               
         CLC   0(2,R6),XFF         NO DEMO ON LINE TO BE INDEXED                
         BE    IND20               SO DON'T DO INDEXING                         
*                                                                               
         XC    0(2,R6),0(R6)                                                    
         M     R0,=F'200'                                                       
*                                                                               
         OC    0(2,R3),0(R3)       INDEX DEMO VALUE IS ZERO                     
         BZ    IND20                                                            
         CLC   0(2,R3),XFF         NO INDEX DEMO, SO                            
         BNE   IND15                                                            
         MVC   0(2,R6),XFF         INDICATE NO DEMO ON PRINT LINE               
         B     IND20                                                            
*                                                                               
IND15    SR    RE,RE                                                            
         ICM   RE,3,0(R3)                                                       
         DR    R0,RE                                                            
         AH    R1,=H'1'                                                         
         SRL   R1,1                                                             
         STCM  R1,3,0(R6)                                                       
         SPACE 2                                                                
IND20    LA    R6,2(R6)                                                         
         LA    R3,2(R3)                                                         
         BCT   R4,IND10                                                         
         B     XIT                                                              
         EJECT                                                                  
*    R6 POINTS AT BUFF                                                          
PROGS    NTR1                      DIG OUT PROGRAM NAMES                        
         ZIC   R0,NUMSTAT                                                       
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
         LA    R6,4(R6)            NOW IT POINTS AT PROGRAM NAMES               
         LA    R3,P+13                                                          
         SPACE 2                                                                
PROG10   MVC   0(16,R3),0(R6)                                                   
         TM    PRINTOPT,X'08'      SHOW WEEKS                                   
         BZ    PROG20                                                           
         MVC   17(2,R3),16(R6)     MOVE IN 1ST 2 WEEKS                          
         B     PROG30                                                           
PROG20   GOTO1 CENTER,PARAS,(R3),19                                             
PROG30   LA    R6,20(R6)                                                        
         LA    R3,20(R3)                                                        
         BCT   R0,PROG10                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
CLEARBUF NTR1                                                                   
         L     R6,SBUFF                                                         
         XCEF  (R6),2600                                                        
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
         L     R6,SBUFF                                                         
         USING BUFFD,R6                                                         
         LA    R3,12                                                            
         SPACE 2                                                                
CB10     MVC   BUFFPROG(120),SPACES                                             
         LA    R6,220(R6)                                                       
         BCT   R3,CB10                                                          
         B     XIT                                                              
         SPACE 1                                                                
         DROP  R4,R6                                                            
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
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
         MVC   TMP,NUMSTAT         NUMBER OF STATIONS                           
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
         LA    R6,124(R6)          NOW POINT TO BUFF DEMOS                      
         ST    R6,SAVER6                                                        
         LA    R3,P+15                                                          
         ST    R3,SAVER3                                                        
         B     NUM20                                                            
         SPACE 1                                                                
NUM10    STC   R1,TMP                                                           
         L     R3,SAVER3           MAIN PRINT LINE POSITION                     
         LA    R3,20(R3)                                                        
         ST    R3,SAVER3                                                        
         SPACE 1                                                                
         L     R6,SAVER6           MAIN DEMO POSTION IN BUFF                    
         LA    R6,16(R6)                                                        
         ST    R6,SAVER6                                                        
         SPACE 1                                                                
NUM20    MVC   BYTEA,NUMDEMS                                                    
         LA    R5,2                2 DEMOS PER LINE                             
         LR    R2,R3                                                            
         LA    RF,DEMOS                                                         
         SPACE 1                                                                
NUM30    CLC   0(2,R6),XFF    DISTINGUISH BTWN ZERO VALUE & NO DEMO             
         BNE   NUM40 (X'FF' IS NO DEMO)                                         
                                                                                
         MVC   0(2,R2),SPACES   INSERT SPACES                                   
         B     NUM70                                                            
         SPACE 1                                                                
                                                                                
NUM40    EDIT  (2,0(R6)),(4,2(R2)),ZERO=NOBLANK                                 
         CLC   P+3(5),=C'INDEX'    IT'S A PCT - DON'T SHOW DECIMALS             
         BE    NUM70                                                            
         EDIT  (2,0(R6)),(6,0(R2)),1     GET DECIMAL POINT                      
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
NUM80    ZIC   R1,TMP                                                           
         BCT   R1,NUM10            NEXT STATION                                 
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*  STAR ROUTINES                                                                
ALLSTARS NTR1                                                                   
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
         ZIC   R3,NUMSTAT                                                       
         CHI   R3,5              IF MORE THAN 5 STATIONS,                       
         BH    ALL30              THEN,USE ALTERNATE ROUTINE                    
ALL10    MVC   P(13),STARS        ELSE USE STANDARD ROUTINE                     
         LA    R2,P+13             AND FILL LINE WITH STARS                     
         SPACE 1                                                                
ALL20    MVC   0(20,R2),STARS      FOR LENGTH OF EACH STATION                   
         LA    R2,20(R2)                                                        
         BCT   R3,ALL20                                                         
         B     ALLX                                                             
*                               **ALTERNATE ROUTINE** (11/00 FJD)               
ALL30    SHI   R3,1               DECREMENT COUNTER                             
         MVC   P(13),STARS                                                      
         LA    R2,P+13                                                          
         SPACE 1                                                                
ALL40    MVC   0(20,R2),STARS    PUT 20 STARS IN FOR EACH STATION UP            
         LA    R2,20(R2)         TO THE 5TH                                     
         BCT   R3,ALL40                                                         
         MVC   0(19,R2),STARS    PUT 19 IN FOR THE SIXTH STATION                
*                                (AVOIDS OVERWRITE INTO NEXT LINE)              
ALLX     XIT1                                                                   
         DROP  R4                                                               
         SPACE 3                                                                
SUMSTARS NTR1                                                                   
         LA    R2,12(R1)                                                        
         ZIC   R3,NUMSTAT                                                       
         CHI   R3,6              IF MORE THAN 5 STATIONS,                       
         BL    SUM10                                                            
         SHI   R3,1              THEN, DECREMENT FOR BCT LOOP                   
SUM10    MVI   0(R1),C'*'             (TO AVOID WRITING PAST EOL                
         MVI   0(R2),C'*'              11/00,FJD)                               
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
         LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         GOTO1 DEFINE,PARAS,=C'WEEK',DBLOCKD,WORK                               
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
         DROP  R4                                                               
         SPACE 2                                                                
NUMTAB   DC    AL1(0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4)                             
         EJECT                                                                  
***********************************************************************         
*  CALCULATE MILITARY ST/END TIMES FROM TOTDUR                                  
*  ON EXIT:    WORK   = SQH      (FROM DEFINE 'TIME' CALL)                      
*              WORK+1 = EQH      (UNADJUSTED: FROM DEFINE 'TIME' CALL)          
*              WORK+2 = STTIME   (FROM DEFINE 'TIME' CALL)                      
*              WORK+4 = ENDTIME  (STTIME + (TOTDUR/WKS*DAYS))                   
*              WORK+6 = DURATION (TOTDUR/WKS*DAYS)                              
***********************************************************************         
         SPACE 2                                                                
TDURTIM  NTR1                                                                   
         USING DBLOCKD,R4                                                       
         GOTO1 DEFINE,PARAS,=C'TOTDUR',DBLOCKD,WORK                             
         SR    R3,R3                                                            
         IC    R3,WORK             R3 = TOT DURATION                            
         MH    R3,=H'15'           TOT QHR DUR * 15 MIN                         
         BAS   RE,COUNTWK                                                       
         SR    RF,RF                                                            
         IC    RF,NWKS             RF = # WEEKS                                 
         CLI   NWKS,0                                                           
         BNE   *+8                 IF # WEEKS ACTIVE = 0,                       
         LA    RF,1                DEFAULT TO 1 WK ACTIVE                       
         L     RE,DBAREC                                                        
         SR    R1,R1                                                            
         IC    R1,PRDW-PRKEY(RE)   EXTRACT DAY BITS FROM KEY                    
         SRL   R1,4                STRIP OFF WEEKS                              
         MH    R1,=Y(L'DAYTAB)     NOTE: VAR = 7 DAYS                           
         LA    R1,DAYTAB(R1)                                                    
         SR    RE,RE                                                            
         IC    RE,4(R1)            RE = # DAYS                                  
         MR    RE,RE               RF= #DAYS * # WEEKS                          
         SR    R2,R2                                                            
         DR    R2,RF               R3 = PRG DUR = TOTDUR/(DAYS*WEEKS)           
*                                                                               
         GOTO1 DEFINE,PARAS,=C'TIME',DBLOCKD,WORK                               
         SR    R0,R0               ADD DUR TO MILITY STTIME = END TIME          
         SR    R1,R1                                                            
         ICM   R1,3,WORK+2         START TIME                                   
         D     R0,=F'100'          MINUTES IN R0, HOURS IN R1                   
         LR    RF,R3               DURATION IN MINUTES                          
         SR    RE,RE                                                            
         D     RE,=F'60'           MINUTES IN RE, HOURS IN RF                   
         AR    R0,RE               ADD MINUTES TOGETHER                         
         CH    R0,=H'60'           TEST IF SUM GT OR EQ TO 1 HOUR               
         BL    *+12                NO                                           
         SH    R0,=H'60'           SUBTRACT 60 MINUTES                          
         LA    RF,1(RF)            AND ADD 1 TO HOURS                           
         AR    R1,RF               ADD HOURS TOGETHER                           
         MH    R1,=H'100'          HOURS X 100 FOR MILITARY TIME                
         AR    R1,R0               ADD MINUTES TO HOURS                         
         CH    R1,=H'2400'         TEST FOR PAST MIDNIGHT                       
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         STCM  R1,3,WORK+4         END TIME                                     
         STC   R3,WORK+6           MINUTES                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO ROUND TIME TO NEAREST 1/2 HOUR                                    
*        DUB(2) = START TIME, DUB+2(2) = END TIME                               
***********************************************************************         
         SPACE 2                                                                
RNDTIME  NTR1                                                                   
         LH    R1,DUB              PICK UP START TIME                           
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
         STH   R1,DUB                                                           
         SPACE 1                                                                
         LH    R1,DUB+2            END TIME                                     
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
         STH   R1,DUB+2                                                         
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
*                                                                               
*    FOLLOWING IS A KLUGE TO INCREMENT BY 1/4HR INSTEAD OF 1/2                  
*                                                                               
ADDQTR   NTR1                                                                   
         MVC   HALF,STIM           START TIME                                   
         BAS   RE,TOQTRHR          CONVERT TO QTR HOUR                          
         MVC   STIMQ,BYTE          STORE START QTR HOUR                         
         ZIC   R3,STIMQ                                                         
         SPACE 1                                                                
         LA    R3,1(R3)                                                         
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
*********************************************************************           
*MYVALBK - BK VALIDATION TO ALLOW EST BOOK                                      
*********************************************************************           
MYBKVAL  ST    RE,SVRE             RETURN ADDRESS                               
         MVC   DMCB+8(4),=C',=,('                                               
         OI    MAX,X'80'           RETURN DISP INTO STRING OF FIELD             
         GOTO1 SCANNER,DMCB,(R2),(MAX,BLOCK)                                    
         ZIC   R0,DMCB+4           R0 = # SCANNER ENTRIES                       
         LTR   R0,R0                                                            
         BNZ   *+14                                                             
         MVC   CONHEAD(L'INVBOK),INVBOK   YES -> INVALID BOOK                   
         B     MYEND                                                            
*                                                                               
         LA    R6,BLOCK                                                         
         LA    R1,1                BK CNTR TO FIND WHICH IS EST BK              
MYBKV5   CLI   0(R6),3                                                          
         BNE   *+14                                                             
         CLC   12(3,R6),=C'EST'                                                 
         BE    MYBKV10                                                          
         LA    R1,1(R1)            BUMP BK CTR                                  
         LA    R6,32(R6)           BUMP TO NEXT SCANNER ENTRY                   
         BCT   R0,MYBKV5                                                        
         LA    R1,0                NO EST BKS IN LIST                           
*                                                                               
MYBKV10  STC   R1,EST              SAVE WHICH BK IN BK LIST IS EST BK           
         ZIC   R1,0(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,MYWRK,0(R2)      DUMMY SCREEN HEADER                          
         CLI   EST,0               NO EST BK IN LIST -> DON'T DUMMY FLD         
         BE    MYBKV15                                                          
         IC    R1,4(R6)            DISPL TO 'EST' ENTRY                         
         LA    R5,MYWRK+8                                                       
         AR    R1,R5               PT TO 'EST' IN DUMMY FLD                     
         MVC   0(3,R1),=C'D69'     DUMMY TO DEC69                               
MYBKV15  XC    BKTYPE,BKTYPE       VALIDATE BOOK                                
         XC    BOOKS,BOOKS                                                      
         XC    WORK,WORK                                                        
         MVI   MAX,10                                                           
         MVC   DUB,SVSOURCE                                                     
         CLI   SVMEDIA,C'N'                                                     
         BNE   *+8                                                              
         MVI   DUB,C'N'                                                         
         GOTO1 BOOKVAL,DMCB,(DUB,MYWRK),(MAX,WORK),                    X        
               (C'B',SCANNER),BKTYPE,(C'C',ACOMFACS)                            
         CLI   4(R1),0             ERRORS?                                      
         BNE   *+14                                                             
         MVC   CONHEAD(L'INVBOK),INVBOK   YES -> INVALID BOOK                   
         B     MYEND                                                            
*                                                                               
         MVC   ACTUAL,4(R1)        # BOOK IN BOOK LIST                          
         ZIC   R1,ACTUAL                                                        
         LA    R3,BOOKS            CREATE THE BOOK LIST                         
         LA    R4,BKTYPE                                                        
         LA    R5,WORK             LIST OF BKS THAT BKVAL RETURNED              
MYBKV20  MVC   0(3,R3),0(R5)       MOVE IN BOOK                                 
         MVC   3(1,R3),0(R4)       SAVE BKTYPE IN BOOK+3                        
         LA    R3,4(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,3(R5)                                                         
         BCT   R1,MYBKV20                                                       
*                                                                               
MYBKVALX L     RE,SVRE             RETURN TO MAIN PARMS VALIDATIONS             
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
*VUPGR   - VALIDATE UPGRADE LINE                                                
*********************************************************************           
VUPGR    ST    RE,SVRE             RETURN ADDRESS                               
         LA    R2,LAYUPG1H                                                      
         MVI   MAX,5                                                            
         MVI   ERROR,2             INVALID INPUT FIELD                          
         GOTO1 SCANNER,DMCB,(20,(R2)),(MAX,BLOCK),0                             
         ZIC   R0,DMCB+4           R0 = # SCANNER ENTRIES                       
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         XC    ESTBK,ESTBK                                                      
         XC    UPGELEM,UPGELEM                                                  
         LA    R6,BLOCK                                                         
*                                                                               
VUPGR10  CLC   =C'UPT ',12(R6)     VALID OPTIONS:  UPT                          
         BE    VUPGR20                                                          
         CLC   =C'BK ',12(R6)                      BK                           
         BE    VUPGR30                                                          
         B     ERREND              INVALID INPUT                                
*                                                                               
VUPGR20  DS    0H                  UPGRADE EXPRESSION                           
         CLI   UPGELEM,0           DUPLICATED INPUT FIELD?                      
         BE    *+14                                                             
         MVC   CONHEAD(L'DUPLFLD),DUPLFLD                                       
         B     MYEND                                                            
         XC    MYWRK,MYWRK         BUILD DUMMY SCREEN HDR                       
         MVC   MYWRK(8),0(R2)      FLD HDR                                      
         ZIC   R1,1(R6)                                                         
         STC   R1,MYWRK+5          L'2ND HALF OF 'UPT=' FLD                     
         BCTR  R1,0                                                             
         EXMVC R1,MYWRK+8,22(R6)   MOVE IN UPGRADE EXPRESSION                   
         LA    R1,9(R1)                                                         
         STC   R1,MYWRK            L'ENTIRE DUMMY FLDHDR                        
*                                                                               
         GOTO1 UPVAL,DMCB,MYWRK,UPGELEM,(C'/',ACOMFACS)                         
         CLI   0(R1),0             NOTHING RETURNED = ERROR                     
         BNE   VUPGR50                                                          
         MVC   CONHEAD(L'INVUPGD),INVUPGD   INVALID UPGD EXPRESSION             
         B     MYEND                                                            
*                                                                               
VUPGR30  DS    0H                  BK FIELD                                     
         CLI   ESTBK,0             DUPLICATED INPUT FIELD?                      
         BE    *+14                                                             
         MVC   CONHEAD(L'DUPLFLD),DUPLFLD                                       
         B     MYEND                                                            
         XC    MYWRK,MYWRK         BUILD DUMMY SCREEN HDR                       
         MVC   MYWRK(8),0(R2)      FLD HDR                                      
         ZIC   R1,1(R6)                                                         
         STC   R1,MYWRK+5          L'2ND HALF OF 'BK=' FLD                      
         BCTR  R1,0                                                             
         EXMVC R1,MYWRK+8,22(R6)   MOVE IN BK                                   
         LA    R1,9(R1)                                                         
         STC   R1,MYWRK            L'ENTIRE DUMMY FLDHDR                        
         MVC   DUB(1),SVSOURCE                                                  
         CLI   SVMEDIA,C'N'                                                     
         BNE   *+8                                                              
         MVI   DUB,C'N'                                                         
         GOTO1 BOOKVAL,DMCB,(DUB,MYWRK),(1,ELEM),                      X        
               (C'B',SCANNER),BKTYPE,(C'C',ACOMFACS)                            
         CLI   4(R1),1             ONLY 1 BK ALLOWED 0 OR >1 = ERROR            
         BE    *+14                                                             
         MVC   CONHEAD(L'INVBOK),INVBOK   YES -> INVALID BOOK                   
         B     MYEND                                                            
         MVC   ESTBK,ELEM                                                       
         MVC   ESTBK+3(1),BKTYPE                                                
         LA    R1,BOOKS                                                         
         ZIC   RE,EST                                                           
         BCTR  RE,0                                                             
         SLL   RE,2                DISP TO BK=EST-1*4                           
         AR    R1,RE                                                            
         MVC   0(4,R1),ESTBK       MOVE IN EST BOOK                             
         B     VUPGR50                                                          
*                                                                               
VUPGR50  LA    R6,42(R6)           BUMP TO NXT SCANR ENTRY TO VALIDATE          
         BCT   R0,VUPGR10                                                       
*                                                                               
         L     RE,SVRE             RETURN ADDRESS                               
         OC    UPGELEM,UPGELEM                                                  
         BNZ   *+14                                                             
         MVC   CONHEAD(L'MISUPG),MISUPG    MISSING UPGD EXPRESSION              
         B     MYEND                                                            
*                                                                               
         CLI   SVFILE,C'P'         FOR PAV, BK= IS REQD                         
         BE    *+12                                                             
         CLI   SVFILE,C'I'         FOR INV, BK= IS REQD                         
         BNE   VUPGRX                                                           
         CLI   ESTBK,0                                                          
         BNE   VUPGRX                                                           
         MVC   CONHEAD(L'MISBK),MISBK     MISSING BK= EXPRESSION                
         B     MYEND                                                            
*                                                                               
VUPGRX   L     RE,SVRE             RETURN ADDRESS                               
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
*HOOK - HOOK ROUTINE FOR HEADLINE DETAILS                                       
*********************************************************************           
*                                                                               
HOOK     NTR1                                                                   
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
         SPACE 1                                                                
         MVC   H3+7(3),LAYSRCE                                                  
         SPACE 1                                                                
         TM    PRINTOPT,X'40'      INDEX BOOK                                   
         BZ    HOOK5                                                            
         MVC   H3+98(11),=C'INDEX BOOK-'                                        
         MVC   H3+109(8),LAYNDX                                                 
         SPACE 1                                                                
HOOK5    TM    PRINTOPT,X'80'      1 BOOK                                       
         BZ    HOOK7                                                            
         MVC   H3+25(5),=C'BOOK-'  PRINTS IN HEADLINES                          
         MVC   H3+30(8),LAYBOOK                                                 
*                                                                               
HOOK7    OC    UPGELEM,UPGELEM     DISPLAY UPT= IF PRESENT                      
         BZ    HOOK10                                                           
         ZIC   R1,LAYUPG1H+5                                                    
         BCTR  R1,0                                                             
         EXMVC R1,H3+43,LAYUPG1                                                 
*                                                                               
HOOK10   MVC   H3+11(5),=C'(PAV)'                                               
         CLI   SVFILE,C'P'                                                      
         BE    HOOK20                                                           
         MVC   H3+11(5),=C'(TPT)'  TPT OR INV                                   
         CLI   SVFILE,C'I'                                                      
         BNE   *+10                                                             
         MVC   H3+11(5),=C'(INV)'  INV                                          
*                               FORMAT DAY FOR TIME PERIOD                      
         MVC   H5+4(3),=C'DAY'                                                  
         MVC   H6+2(7),=C'.......'                                              
         TM    SVDAY,X'40'                                                      
         BZ    *+8                                                              
         MVI   H6+2,C'M'                                                        
         TM    SVDAY,X'20'                                                      
         BZ    *+8                                                              
         MVI   H6+3,C'T'                                                        
         TM    SVDAY,X'10'                                                      
         BZ    *+8                                                              
         MVI   H6+4,C'W'                                                        
         TM    SVDAY,X'08'                                                      
         BZ    *+8                                                              
         MVI   H6+5,C'T'                                                        
         TM    SVDAY,X'04'                                                      
         BZ    *+8                                                              
         MVI   H6+6,C'F'                                                        
         TM    SVDAY,X'02'                                                      
         BZ    *+8                                                              
         MVI   H6+7,C'S'                                                        
         TM    SVDAY,X'01'                                                      
         BZ    *+8                                                              
         MVI   H6+8,C'S'                                                        
*                                                                               
         CLI   SVFILE,C'I'                                                      
         BNE   HOOK20                                                           
         CLI   SVDAY,X'7C'                                                      
         BNE   *+10                                                             
         MVC   H6+2(7),=CL7'M-F'                                                
         CLI   SVDAY,X'7F'                                                      
         BNE   *+10                                                             
         MVC   H6+2(7),=CL7'M-SU'                                               
         CLI   SVDAY,X'7E'                                                      
         BNE   *+10                                                             
         MVC   H6+2(7),=CL7'M-SA'                                               
         CLI   SVDAY,X'03'                                                      
         BNE   *+10                                                             
         MVC   H6+2(7),=CL7'WKE '                                               
*                                                                               
         SPACE 1                                                                
HOOK20   LA    R2,MKTSV                                                         
         LA    R3,H5+13                                                         
         ZIC   R5,NUMSTAT                                                       
HOOK25   MVC   0(19,R3),0(R2)      PRINT MARKET NAME                            
         LA    R2,29(R2)                                                        
         LA    R3,20(R3)                                                        
         BCT   R5,HOOK25                                                        
         SPACE 1                                                                
         LA    R2,STATSV                                                        
         LA    R3,H6+14                                                         
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
         LA    R3,H7+14            POINT WHERE 1ST DEMO PRINTS                  
         MVC   BYTEB,NUMSTAT                                                    
         LA    R2,DEMOS                                                         
         MVC   BYTEA,NUMDEMS                                                    
HOOK60   LA    R5,2                                                             
         LR    R6,R3                                                            
HOOK70   CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
*                                                                               
         CLI   SVMEDIA,C'N'                                                     
         BNE   HOOK75                                                           
         LA    RE,DBLOCKA2                                                      
         USING DBLOCKD,RE                                                       
         XC    DBLOCKA2,DBLOCKA2                                                
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'U'                                                    
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBSELAGY,AGENCY                                                  
         DROP  RE                                                               
         GOTO1 DEMOCON,PARAS,(0,(R2)),(2,WORK),(0,DBLOCKA2)                     
         B     HOOK76                                                           
*                                                                               
HOOK75   GOTO1 DEMOCON,PARAS,(0,(R2)),(2,WORK),(0,DBLOCKA1)                     
*                                                                               
HOOK76   CLI   1(R2),C'I'          AND CHANGE BACK FOR NEXT ROUND               
         BNE   *+8                                                              
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
         DROP  R4                                                               
         SPACE 2                                                                
DASHES   DC    40C'-'                                                           
XFF      DC    20X'FF'                                                          
         EJECT                                                                  
**********************************************************************          
* COMMON ROUTINES                                                               
**********************************************************************          
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
MANYBKS  DC    C'* ERROR * TOO MANY BOOKS - LIMIT IS 8'                         
MANYDEM  DC    C'* ERROR * TOO MANY DEMOS - LIMIT IS 8'                         
TOOBIG   DC    C'* ERROR * PLEASE SHORTEN REQUEST OR RUN OVERNIGHT'             
PAVONLY  DC    C'* ERROR * PAV OPTION ONLY'                                     
MISUPG   DC    C'* ERROR * REQUIRED FIELD IS MISSING - UPGRADE'                 
MISBK    DC    C'* ERROR * BK= PARAMETER REQUIRED'                              
NOFOUND  DC    C'* ERROR * RECORD NOT FOUND - '                                 
TPSRC    DC    C'* ERROR * SRC IS ONLY VALID FOR TIME PERIOD'                   
NETSRC   DC    C'* ERROR * SOUCE MUST BE NSI FOR NETWORK FILE'                  
MUSTSOON DC    C'* THIS REQUEST MUST BE DONE AS SOON *'                         
NOEST    DC    C'* ERROR * INVALID INPUT- NO EST BK RQSTD'                      
NOESPAV  DC    C'* ERROR * NO ESTIMATES/UPGRADES FOR PAV'                       
DUPLFLD  DC    C'* ERROR * DUPLICATED INPUT FIELD'                              
INVUPGD  DC    C'* ERROR * INVALID UPGRADE EXPRESSION'                          
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,001,AGYNAME                                                   
         PSPEC H1,060,C'LAYOUT REPORT'                                          
         PSPEC H1,099,REPORT                                                    
         PSPEC H1,115,PAGE                                                      
         PSPEC H2,001,REQUESTOR                                                 
         PSPEC H2,060,13C'-'                                                    
         PSPEC H2,099,RUN                                                       
         PSPEC H3,001,C'SOURCE-'                                                
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
INVBOK   DC    C'* ERROR * INVALID BOOK'                                        
*                                                                               
DEMOSHR  DS    0XL3                                                             
         DC    X'81',C'S',AL1(1)                                                
         DC    X'81',C'S',AL1(2)                                                
         DC    X'81',C'S',AL1(3)                                                
         DC    X'FF'                                                            
*                                                                               
NETFILES DS    0CL7                   DBSELSRC/ FILE/ DBFILE                    
         DC    C'H',C'NHT',C'NAD'     NHT FILE USE DBFILE=NAD                   
         DC    C'D',C'NAD',C'NAD'     NAD FILE                                  
         DC    C'C',C'CNN',C'NTI'     CABLE FILE                                
         DC    C'P',C'PNN',C'NTI'     NTI PKTPC FILE                            
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
DAYTAB   DS    0CL5                   TABLE COPIED FROM DEFINE                  
         DC    X'7C',C'M-F',X'05'  0                                            
         DC    X'40',C'MON',X'01'  1                                            
         DC    X'20',C'TUE',X'01'  2                                            
         DC    X'10',C'WED',X'01'  3                                            
         DC    X'08',C'THU',X'01'  4                                            
         DC    X'04',C'FRI',X'01'  5                                            
         DC    X'02',C'SAT',X'01'  6                                            
         DC    X'01',C'SUN',X'01'  7                                            
         DC    X'7F',C'M-S',X'07'  8                                            
         DC    X'7F',C'VAR',X'07'  9                                            
         DC    X'7F',C'VAR',X'07'  A                                            
         DC    X'7F',C'VAR',X'07'  B                                            
         DC    X'7F',C'VAR',X'07'  C                                            
         DC    X'7F',C'TYP',X'07'  D                                            
         DC    X'03',C'WKE',X'02'  E                                            
         DC    X'7F',C'VAR',X'07'  F                                            
         DC    X'FF',C'VAR',X'07'  TABLE TERMINATOR (ADYTM FUNCTION)            
         EJECT                                                                  
         DROP  RB                                                               
***********************************************************************         
*                                                                               
*  VALOPTA - VALIDATION FOR 1ST OPTIONS FIELD (R2 POINT TO LAYOPT1H)            
*                                                                               
***********************************************************************         
VALOPTA  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,BLOCK                                                         
         XCEF  (R4),480        CLEAR SCANBLOCK                                  
         MVI   MAX,15          BLOCK IS ONLY 480 LENGTH                         
         GOTO1 SCANNER,DMCB,(R2),(MAX,BLOCK),0                                  
         ZICM  R3,DMCB+4,1     ANY RETURNED LINES?                              
         BZ    VOPTAINV        IF NOT, THEN ERROR                               
                                                                                
                                                                                
         MVI   OPTCHK1,0               INIT OPTIONS CHECK BYTE                  
         USING SCANBLKD,R4                                                      
VALOPA10 CLI   SC1STLEN,0              CHECK FOR ENTRY                          
         BE    VOPTAX                  IF NONE, END                             
         CLC   =CL10'1SPLIT',SC1STFLD  USER CHOSE 1SPLIT?                       
         BNE   VALOPA20                                                         
         TM    OPTCHK1,X'01'           ALREADY SET?                             
         BO    VOPTCNFL                  YES, CONFLICT ERROR                    
         OI    OPTCHK1,X'01'             NO,  SET IT NOW                        
         CLI   SC2NDLEN,1              1 CHARACTER PARAM?                       
         BNE   VOPTAERR                 NO, ERROR                               
         CLI   SC2NDFLD,C'1'           1ST WEEK?                                
         BNE   VALOPA12                 NO                                      
         MVI   ONEWEEK,X'08'            YES                                     
         B     VOPTANXT                                                         
VALOPA12 CLI   SC2NDFLD,C'2'           2ND WEEK?                                
         BNE   VALOPA14                 NO                                      
         MVI   ONEWEEK,X'04'            YES                                     
         B     VOPTANXT                                                         
VALOPA14 CLI   SC2NDFLD,C'3'           3RD WEEK?                                
         BNE   VALOPA16                 NO                                      
         MVI   ONEWEEK,X'02'            YES                                     
         B     VOPTANXT                                                         
VALOPA16 CLI   SC2NDFLD,C'4'           4TH WEEK?                                
         BNE   VOPTAERR                 NO, ERROR MUST BE 1,2,3 OR 4            
         MVI   ONEWEEK,X'01'            YES                                     
         B     VOPTANXT                                                         
*                                                                               
VALOPA20 CLC   =CL10'2SPLIT',SC1STFLD  USER CHOSE 2SPLIT?                       
         BNE   VALOPA30                                                         
         TM    OPTCHK1,X'02'           ALREADY SET?                             
         BO    VOPTCNFL                  YES, CONFLICT ERROR                    
         OI    OPTCHK1,X'02'             NO,  SET IT NOW                        
         CLI   SC2NDLEN,1              1 CHARACTER PARAM?                       
         BNE   VOPTAERR                 NO, ERROR                               
         CLI   SC2NDFLD,C'F'           FIRST 2 WEEKS?                           
         BNE   VALOPA22                 NO                                      
         MVI   TWOWEEK,X'0C'            YES                                     
         B     VOPTANXT                                                         
*                                                                               
VALOPA22 CLI   SC2NDFLD,C'L'           LAST  2 WEEKS?                           
         BNE   VOPTAERR                 NO, ERROR MUST BE F,OR L                
         MVI   ONEWEEK,X'03'            YES                                     
         B     VOPTANXT                                                         
*                                                                               
VALOPA30 CLC   =CL10'WEEKS',SC1STFLD   USER CHOSE WEEKS?                        
         BNE   VALOPA40                                                         
         TM    OPTCHK1,X'04'           ALREADY SET?                             
         BO    VOPTCNFL                  YES, CONFLICT ERROR                    
         OI    OPTCHK1,X'04'             NO,  SET IT NOW                        
         CLI   SC2NDLEN,1              1 CHARACTER PARAM?                       
         BNE   VOPTAERR                 NO, ERROR                               
         CLI   SC2NDFLD,C'Y'           SHOW WEEKS = YES?                        
         BNE   VALOPA32                 NO                                      
         OI    PRINTOPT,X'08'           YES                                     
         B     VOPTANXT                                                         
VALOPA32 CLI   SC2NDFLD,C'N'           SHOW WEEKS = NO?                         
         BNE   VOPTAERR                 NO, ERROR,MUST BE Y OR N                
         NI    PRINTOPT,X'FF'-X'08'     YES, TURN OFF BIT                       
         B     VOPTANXT                                                         
*                                                                               
VALOPA40 CLC   =CL10'PBH',SC1STFLD     USER CHOSE PAGE BREAK BY 1/2HR?          
         BNE   VALOPA50                                                         
         TM    OPTCHK1,X'10'           ALREADY SET?                             
         BO    VOPTCNFL                  YES, CONFLICT ERROR                    
         OI    OPTCHK1,X'10'             NO,  SET IT NOW                        
         CLI   SC2NDLEN,1              1 CHARACTER PARAM?                       
         BNE   VOPTAERR                 NO, ERROR                               
         CLI   SC2NDFLD,C'Y'           PAGE BREAK = YES?                        
         BNE   VALOPA42                 NO                                      
         OI    OPTFLG1,PBHQ             YES                                     
         B     VOPTANXT                                                         
VALOPA42 CLI   SC2NDFLD,C'N'           PAGE BREAK = NO?                         
         BNE   VOPTAERR                 NO, ERROR, MUST BE Y OR N               
         NI    OPTFLG1,X'FF'-PBHQ       YES, ENSURE BIT IS OFF                  
         B     VOPTANXT                                                         
*                                                                               
VALOPA50 CLC   =CL10'QTR',SC1STFLD     USER CHOSE 1/4 HOUR REPORTING?           
         BNE   VOPTNAME                  NO, NO MATCH ON AVAILABLE OPTS         
         TM    OPTCHK1,X'20'           ALREADY SET?                             
         BO    VOPTCNFL                  YES, CONFLICT ERROR                    
         OI    OPTCHK1,X'20'             NO,  SET IT NOW                        
         CLI   SC2NDLEN,1              1 CHARACTER PARAM?                       
         BNE   VOPTAERR                 NO, ERROR                               
         CLI   SC2NDFLD,C'Y'           1/4 HR REPORTING = YES?                  
         BNE   VALOPA52                 NO                                      
         OI    OPTFLG1,QTRPTQ           YES                                     
         B     VOPTANXT                                                         
VALOPA52 CLI   SC2NDFLD,C'N'           1/4 HR REPORTING = NO?                   
         BNE   VOPTAERR                 NO, ERROR, MUST BE Y OR N               
         NI    OPTFLG1,X'FF'-QTRPTQ     YES, ENSURE BIT IS OFF                  
*                                                                               
VOPTANXT LA    R4,SCBLKLQ(R4)          BUMP TO NEXT SCANBLOCK ENTRY             
         BCT   R3,VALOPA10             REPEAT LOOP FOR ALL ENTRIES              
         DROP  R4                                                               
                                                                                
VOPTAX   XIT1                                                                   
                                                                                
                                                                                
VOPTCNFL XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OPTCONFL),OPTCONFL                                     
         J     MYEND                                                            
VOPTNAME XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OPTNAMER),OPTNAMER                                     
         J     MYEND                                                            
VOPTAERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADOPT),BADOPT                                         
         J     MYEND                                                            
VOPTAINV XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVOPT),INVOPT                                         
         J     MYEND                                                            
*                                                                               
*                                                                               
OPTCONFL DC    C'* ERROR * CONFLICTING OPTIONS ENTERED'                         
OPTNAMER DC    C'* ERROR * BAD OPTION NAME ENTERED'                             
BADOPT   DC    C'* ERROR * BAD OPTION PARAMETER ENTERED'                        
INVOPT   DC    C'* ERROR * INVALID OPTION FIELD '                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
*    CHKUNIV ROUTINE -CHECKS IF UNIVERSE DEMO REQUESTED AND EXITS WITH          
*                     ERROR IF FOUND                                            
***********************************************************************         
CHKUNIV  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R1,DEMOS                                                         
CHKU10   CLI   0(R1),X'FF'                                                      
         BE    CHKUX                                                            
         CLI   1(R1),C'U'                                                       
         BE    CHKUERR                                                          
         LA    R1,3(R1)                                                         
         B     CHKU10                                                           
                                                                                
CHKUX    XIT1                                                                   
                                                                                
CHKUERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'UNIVERR),UNIVERR                                       
         J     MYEND                                                            
                                                                                
UNIVERR  DC    C'* ERROR * UNIVERSES NOT SUPPORTED IN THIS REPORT'              
                                                                                
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*                                                                               
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
         PRINT OFF                                                              
       ++INCLUDE RERESWRK                                                       
         EJECT                                                                  
* DSECT TO COVER SCREEN                                                         
         ORG   CONTAGH                                                          
       ++INCLUDE RERESF4D                                                       
         EJECT                                                                  
         PRINT ON                                                               
* SAVE AREA VALUES                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE            LOCAL WORKING STORAGE                        
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
SVRE     DS    F                   SAVE RE                                      
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
NODEMFLG DS    XL1                 NO DEMO FLAG                                 
LASTTIME DS    H                   CURRENT PAV SHOW END TIME                    
SBUFF    DS    A                   A(START OF PRINT BUFFER)                     
STAMP    DS    CL8                 STORAGE STAMP                                
MYWRK    DS    XL100                                                            
TMP      DS    X                                                                
DMKIVRQ  EQU   (RINVPEL-RINVREC)-(DRFRSTEL-DRKEY)                               
SHRS     DS    3F                                                               
BKCTR    DS    X                                                                
EST      DS    X                   ESTIMATED BOOK                               
ESTBK    DS    XL4                 UPGRADE BOOK FOR EST                         
IBLK     DS    XL5                                                              
OBLK     DS    XL5                                                              
STATFLG1 DS    XL1                 GENERAL STATUS FLAGS                         
QTRHRQ   EQU   X'01'               FLIP FLOP BIT 2 TRACK LOOP ITERATION         
OPTCHK1  DS    XL1                 OPTS CHECK BYTE-AVOIDS DOUBLE SET            
OPTFLG1  DS    XL1                 OPTIONS FLAG 1                               
PBHQ     EQU   X'01'               PAGE BREAK ON 1/2 HR                         
QTRPTQ   EQU   X'02'               QUARTER HOUR REPORTING                       
                                                                                
SAVELN   EQU   *-SYSSPARE          SAVE AREA LENGTH                             
         DS    CL(L'SYSSPARE-(*-SYSSPARE)) SPARE                                
*                                                                               
         PRINT OFF                                                              
REDBLKUD DSECT                    *** NEW DSECT FOR REDEMLKU (UPGDS)            
       ++INCLUDE REDEMLKUD                                                      
         EJECT                                                                  
* RERMPPROF/DDCOMFACS/FAFACTS                                                   
       ++INCLUDE RERMPPROF                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'102RERES04   04/24/09'                                      
         END                                                                    
