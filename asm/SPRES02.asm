*          DATA SET SPRES02    AT LEVEL 049 AS OF 05/01/02                      
*PHASE T20F02A,+0                                                               
*----------------------------------------------------------------------         
* 10/28/92 - PRINT OUT M-F, M-S AND S-S DAYS INSTEAD OF LAST DAY.  (GH)         
*----------------------------------------------------------------------         
         TITLE 'T20F02 - FLEXI'                                                 
T20F02   CSECT                                                                  
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
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*        VALIDATE KEY                                                           
*                                                                               
VKEY     MVI   BOXOPT,C'Y'                                                      
*                                                                               
         MVI   PRINTOPT,0          CLEAR OUT PRINTOPT                           
         LA    R2,FLESRCEH         VALIDATE SOURCE                              
         GOTO1 VVALSRC                                                          
         SPACE 1                                                                
         LA    R2,FLEBOOKH         VALIDATE BOOK                                
         MVI   MAX,10              DUMMY SO I CAN DO ERROR MSG                  
         GOTO1 ANY                                                              
         GOTO1 VVALBOOK                                                         
         CLI   NBOOKS,1            REALLY ONLY 1 BOOK ALLOWED                   
         BNH   VKEY10                                                           
         MVI   ERROR,MANYBKS       TOO MANY BOOKS                               
         B     ERREND                                                           
         SPACE 1                                                                
VKEY10   LA    R2,FLEDEMOH         VALIDATE DEMOS                               
         MVI   MAX,20              DUMMY SO I CAN DO ERROR MSG                  
         MVI   NFLDS,1                                                          
         GOTO1 ANY                                                              
         GOTO1 VVALDEM                                                          
         LA    R3,DEMOS                                                         
         MVC   DEMMOD,1(R3)        SAVE 1ST DEMO MODIFIER (FOR COST)            
* THE SIXTH DEMO GAVE PROBLEMS                                                  
         CLI   NDEMOS,5            REALLY ALLOW 5 DEMOS                         
         BNH   VKEY20              (IF COST FIELD USED, ONLY ALLOW 5)           
         MVI   ERROR,MANYDEM                                                    
         B     ERREND                                                           
         SPACE 1                                                                
VKEY20   LA    R2,FLEDAYH          VALIDATE DAY/DETAIL FIELDS                   
         GOTO1 VVALDYTM,PARAS,8    8 DAY/DETAIL FIELDS                          
         SPACE 1                                                                
         LA    R2,FLESTATH         VALIDATE STATION                             
         LA    R3,8                                                             
         LA    R4,STATS                                                         
         XC    STATS,STATS                                                      
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
VKEY30   GOTO1 VVALDSTA                                                         
         MVC   0(5,R4),ACTSTAT                                                  
         LA    R4,5(R4)                                                         
         BAS   RE,BUMP                                                          
         BCT   R3,*+8                                                           
         B     VKEY40                                                           
         CLI   5(R2),0             ANOTHER STATION?                             
         BNE   VKEY30                                                           
         SPACE 1                                                                
VKEY40   LA    R2,FLEWKSH          POINT TO WEEKS                               
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
         CLI   NDEMOS,5           IF COST FIELD USED,                           
         BNH   VKEY100             ONLY ALLOW 5 DEMOS                           
         MVI   ERROR,MANYDEM                                                    
         B     ERREND                                                           
         SPACE 1                                                                
VKEY100  B     VKEY112                                                          
VKEY112  OI    PRINTOPT,X'04'                                                   
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
         MVI   RCSUBPRG,0                                                       
         XC    BUFFLINE,BUFFLINE   COUNT NUMBER OF LINES ON PAGE                
         EJECT                                                                  
*   CONTROL I/O                                                                 
         SPACE 2                                                                
         LA    R3,8                UP TO 8 STATIONS                             
         LA    R2,STATS            LIST OF STATIONS IN DBSELSTA FMT             
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
PREP40   LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBSELSTA,0(R2)                                                   
         XC    DBSELMK,DBSELMK                                                  
         MVC   DBSELBK,BOOKS+1                                                  
         MVC   DBBTYPE,BOOKS+3      BOOK TYPE                                   
         MVI   DBBEST,C'A'                                                      
         SPACE 1                                                                
         LA    R6,DAYTMLST         POINT TO DAY/TIME LIST                       
         ZIC   R5,NDAYTMS          NO. OF ENTRIES IN LIST                       
PREP50   ZIC   R1,0(R6)            GET DAY                                      
         LA    R1,DAYBITS(R1)                                                   
         MVC   DBSELDAY,0(R1)                                                   
         CLI   DBSELDAY,3          IS IS SA-SU                                  
         BNE   *+8                                                              
         MVI   DBDAYOPT,C'Y'       INDICATES SA-SU AVE LINE                     
         MVC   DBSELTIM,1(R6)                                                   
         XC    LASTPROG,LASTPROG                                                
         XC    FRSTPROG,FRSTPROG                                                
         XC    FACTOR,FACTOR                                                    
         XC    ACCUMDEM,ACCUMDEM                                                
         SPACE 1                                                                
         GOTO1 DEMAND,DMCB,DBLOCK,FILL                                          
         CLI   DBERROR,X'10'       RECORD NOT FOUND                             
         BNE   PREP60                                                           
         MVC   LASTPROG(4),=F'-1'                                               
         MVC   CONHEAD(L'NOFOUND),NOFOUND                                       
         MVC   CONHEAD+L'NOFOUND(5),0(R2) IDENTIFY STATION                      
         LA    R2,CONHEAD+L'NOFOUND+5                                           
         MVI   0(R2),C'/'                                                       
         MVC   1(8,R2),FLEBOOK     AND BOOK                                     
         LA    R2,FLESTATH         POINT CURSOR AT FIRST STATION                
         MVC   WORK,CONHEAD        BECAUSE GETERR WANTS IT THERE                
         B     MYEND                                                            
*                                                                               
PREP60   OC    DBDIVSOR,DBDIVSOR   TEST FOR ANY RECORDS                         
         BZ    *+14                NO                                           
         CLI   DBERROR,X'80'       TEST FOR EOF                                 
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG                              
         MVI   LASTPROG,X'FF'      SEND OUT LAST PROGRAM                        
         BAS   RE,FILL                                                          
*                                                                               
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
* SEE IF A PROGRAM BREAK                                                        
         CLI   LASTPROG,X'FF'      LAST TIME HOOK                               
         BE    FILL90                                                           
         GOTO1 DEFINE,DMCB,=C'PROGRAM',DBLOCK,THISPROG                          
         OC    FRSTPROG,FRSTPROG                                                
         BNZ   *+10                                                             
         MVC   FRSTPROG,THISPROG                                                
         IC    RF,DBACTSQH                                                      
         BCTR  RF,0                                                             
         STC   RF,ENDQTRHR                                                      
*                                                                               
         OC    LASTPROG,LASTPROG   FIRST TIME FOR HOOK                          
         BNZ   *+14                                                             
         XC    WORK,WORK           CLEAR THE BUFFER AREA                        
         B     *+14                                                             
*                                                                               
         CLC   LASTPROG,THISPROG   PROGRAM NAME                                 
         BNE   FILL90                                                           
*                                                                               
FILL1    LA    R6,WORK                                                          
         USING BUFFD,R6                                                         
         SPACE 1                                                                
*  SEE IF IT MEETS WEEKS CRITERIA                                               
         SPACE 1                                                                
         CLI   WEEKS,0                                                          
         BE    FILL40              NO WEEKS CRITERIA                            
         GOTO1 DEFINE,DMCB,=C'WEEK',DBLOCK,OUTAREA                              
         NI    OUTAREA,X'0F'                                                    
         MVI   NWKS,0                                                           
         ZIC   R1,OUTAREA                                                       
         LA    R1,NUMTAB(R1)                                                    
         MVC   NWKS,0(R1)                                                       
         CLI   FLESRCE,C'A'        FUDGE FOR ZEN IF ARBITRON                    
         BNE   FILL10                                                           
         CLC   BOOKS+1(2),=X'5605'  AND BEFORE MAY/86                           
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
         LH    R1,FACTOR                                                        
         AH    R1,DBFACTOR         GETIUN MULTIPLIES BY THIS                    
         STH   R1,FACTOR                                                        
         MVC   CURFACTR,DBFACTOR                                                
         GOTO1 DEFINE,DMCB,=C'PURE',DBLOCK,OUTAREA                              
         MVC   BUFFPURE(4),OUTAREA+3 PURE NUMBER                                
*                                                                               
         GOTO1 (RF),(R1),=C'TIME',DBLOCK,OUTAREA                                
         CLC   LASTPROG,THISPROG                                                
         BE    *+10                                                             
         MVC   BUFFINV(1),OUTAREA  PURE NUMBER-FOR DAY/TIME                     
         GOTO1 (RF),(R1),=C'DAY',DBLOCK,OUTAREA                                 
*                                                                               
         CLC   LASTPROG,THISPROG                                                
         BNE   FILL40B                                                          
         CLC   DBACTEQC,ENDQTRHR                                                
         BNE   FILL40X                                                          
         CLC   FRSTPROG,THISPROG                                                
         BNE   FILL40X                                                          
         TM    DBACTDAY,X'7C'      M-F/M-S                                      
         BNO   *+8                                                              
         MVI   BUFFINV+1,0         START OFF WITH M-F                           
*                                                                               
         CLI   DBACTDAY,X'7C'      M-F?                                         
         BNE   *+12                                                             
         MVI   BUFFINV+1,0                                                      
         B     FILL40X                                                          
*                                                                               
         CLI   DBACTDAY,X'7F'      M-SU?                                        
         BNE   FILL40A                                                          
         CLI   OUTAREA+1,7                                                      
         BNE   FILL40X                                                          
         MVI   BUFFINV+1,X'80'                                                  
         B     FILL40X                                                          
*                                                                               
FILL40A  CLI   DBACTDAY,X'03'      SA-SU?                                       
         BNE   FILL40X                                                          
         MVI   BUFFINV+1,X'E0'                                                  
         B     FILL40X                                                          
*                                                                               
FILL40B  IC    R1,OUTAREA+1                                                     
         SLL   R1,4                                                             
         STC   R1,BUFFINV+1                                                     
*                                                                               
FILL40X  DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'PROGRAM',DBLOCK,OUTAREA                           
         MVC   BUFFPROG,OUTAREA    PROGRAM NAME                                 
         MVC   LASTPROG,OUTAREA                                                 
         MVC   MYDBLOCK(200),DBLOCK                                             
         CLI   DBSELMED,C'T'                                                    
         BNE   FILL41                                                           
         LA    RF,LIOS                                                          
         L     RE,AIO2                                                          
         XCEF                                                                   
         GOTO1 VGETIUN,DMCB,(4,DBLOCK),AIO2                                     
         MVC   CURFACTR,=H'1'      IUN ALREADY WEIGHTS                          
         L     RF,AIO2                                                          
         USING IUNREC,RF                                                        
         MVC   NEWRTG(LENVALS),OLDRTG                                           
         MVC   NEWIMP(LENVALS),OLDIMP                                           
         MVC   NEWHPT(LENVALS),OLDHPT                                           
         MVC   NEWTOT(LENVALS),OLDTOT                                           
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBACTSRC,C'N'                                                    
         MVC   DBFILE,=C'IUN'                                                   
         MVI   DBINTMED,C'U'                                                    
         MVI   DBINTFIL,C'I'                                                    
         LA    RF,DUB              FUDGE A MINIMAL RECORD                       
         SH    RF,=H'20'                                                        
         ST    RF,DBAREC                                                        
         LA    RF,DUB+2                                                         
         ST    RF,DBAQUART                                                      
         XC    DUB,DUB                                                          
         MVC   DUB(2),=H'25'                                                    
         MVC   DUB+2(3),=X'200200'                                              
         MVC   IUBLOCK,IUNBLK      CONTROL THE FORMULAS                         
         MVC   IUBLOCK+7(2),=X'520B'                                            
         CLC   SAVBK,=X'5801'                                                   
         BL    *+10                                                             
         MVC   IUBLOCK+7(2),=X'530B'                                            
         GOTO1 DEMEL,DMCB,(C'C',IUBLOCK),DBLOCK,AIO2                            
         L     RF,AIO2                                                          
         LA    RF,2(RF)                                                         
         ST    RF,DBAQUART                                                      
         SPACE 1                                                                
FILL41   ZIC   R1,NDEMOS                                                        
         MH    R1,=H'3'                                                         
         LA    R1,DEMOS(R1)                                                     
         MVI   0(R1),X'FF'                                                      
         LA    R2,DEMOS            POINT R2 AT DEMO LIST                        
         LA    R3,ACCUMDEM                                                      
         MVC   SAVBK,DBACTBK       PROTECT DBACTBK                              
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCK,ELEM                              
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DBACTBK,SAVBK       RESTORE IT                                   
         MVC   DBLOCK(200),MYDBLOCK                                             
*                                                                               
         LA    R5,ELEM             NOW POINT R5 TO VALUES                       
         ZIC   R4,NDEMOS                                                        
         SPACE 2                                                                
FILL50   TM    PRINTOPT,X'08'      IF ROUNDED DEMOS                             
         BZ    FILL70                                                           
         CLI   1(R2),C'R'          AND RATING                                   
         BE    FILL60                                                           
         CLI   1(R2),C'E'          OR CANADIAN E RATING                         
         BE    FILL60                                                           
         CLI   1(R2),C'I'          OR SPOT IMP                                  
         BE    FILL60                                                           
         CLI   1(R2),C'S'          OR SHARE                                     
         BE    FILL60                                                           
         CLI   1(R2),C'X'          OR TSA SHARE (INPUT AS T),                   
         BNE   FILL70                                                           
         SPACE 1                                                                
FILL60   L     R1,0(R5)            THEN DROP DECIMAL                            
         SR    R0,R0                                                            
         LA    R1,5(R1)                                                         
         D     R0,=F'10'                                                        
         MH    R1,CURFACTR                                                      
         CLI   1(R2),C'S'          SHARES NOT ALREADY WIEGHTED                  
         BNE   *+8                                                              
         MH    R1,DBFACTOR         SO WIEGHT THEM NOW                           
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         ICM   R1,15,0(R3)         ADD IN RUNNING TOTALS                        
         AR    R1,R0                                                            
         STCM  R1,15,0(R3)                                                      
         B     FILL75                                                           
         SPACE 1                                                                
FILL70   L     R1,0(R5)            OTHERWISE, USE AS IS                         
         MH    R1,CURFACTR                                                      
         CLI   1(R2),C'S'          SHARES NOT ALREADY WIEGHTED                  
         BNE   *+8                                                              
         MH    R1,DBFACTOR         SO WIEGHT THEM NOW                           
         CLI   1(R2),C'U'          UNIV NOT ALREADY WIEGHTED                    
         BNE   *+8                                                              
         MH    R1,DBFACTOR         SO WIEGHT THEM NOW                           
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         ICM   R1,15,0(R3)         ADD IN RUNNING TOTALS                        
         AR    R1,R0                                                            
         STCM  R1,15,0(R3)                                                      
         SPACE 1                                                                
FILL75   B     FILL80          *** HANDLE COST FACILITY LATER ***               
*                                                                               
         OC    COST,COST           HANDLE COST FACILITY                         
         BZ    FILL80                                                           
         OC    BUFFCOST,BUFFCOST   ONLY FOR 1ST DEMO                            
         BNZ   FILL80                                                           
         L     R1,COST             IN PENNIES                                   
         L     R0,0(R3)            DEMO                                         
         MR    R0,R0                                                            
         STCM  R1,15,BUFFCOST                                                   
         SPACE 2                                                                
FILL80   LA    R2,3(R2)            NEXT DEMO                                    
         LA    R3,4(R3)            NEXT BUFF AREA                               
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         BCT   R4,FILL50                                                        
         B     XIT                                                              
         EJECT                                                                  
*              NOW SEE IF IT QUALIFIES                                          
         SPACE 3                                                                
FILL90   OC    FACTOR,FACTOR       ANYTHING THERE                               
         BZ    FILLX                                                            
         LA    R6,WORK             FIRST UNWEIGHT THE DEMOS                     
         LA    R1,BUFFDEM1                                                      
         LA    R2,ACCUMDEM                                                      
         LA    R0,6                                                             
         LH    R4,FACTOR                                                        
FILL91   SR    RE,RE                                                            
         LR    RE,RF                                                            
         ICM   RE,15,0(R2)         GET WEIGHTED VALUE                           
         BZ    FILL92                                                           
         SRDA  RE,31                                                            
         DR    RE,R4               DIVIDE BY FACTOR                             
         BM    *+8                                                              
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         STCM  RF,3,0(R1)          SAVE AVERAGE VALUE                           
         MVI   2(R1),0             AND CLEAR RANK                               
FILL92   LA    R1,3(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,FILL91                                                        
*                                                                               
         CLC   BUFFDEM1,DEMOMIN    DOES IT MEET MIN. REQUIREMENT                
         BL    FILLX                                                            
         CLC   BUFFDEM1,DEMOMAX    AND MAX REQUIREMENT                          
         BH    FILLX                                                            
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
         BL    FILLX               CHECK IF THIS IS WORSE THAN WORST            
         SPACE 2                                                                
FILL110  OC    COST,COST           HANDLE COST FACILITY                         
         BZ    FILL112                                                          
         L     R1,COST             IN PENNIES                                   
         SR    R0,R0                                                            
         ICM   R0,3,BUFFDEM1       DEMO                                         
         MR    R0,R0                                                            
         STCM  R1,15,BUFFCOST                                                   
         SPACE 2                                                                
FILL112  LH    R2,BUFFLINE         FIND N'TH POSITION                           
         BCTR  R2,0                                                             
         MH    R2,=H'49'                                                        
         LA    R2,BUFF(R2)                                                      
         MVC   0(49,R2),WORK       AND PUT IN THIS ITEM                         
         CLC   BUFFLINE,BUFFMAX                                                 
         BL    FILLX                                                            
         LH    R2,BUFFLINE                                                      
         GOTO1 XSORT,PARAS,(1,BUFF),(R2),49,2,33                                
         BCTR  R2,0                                                             
         MH    R2,=H'49'                                                        
         LA    R2,BUFF(R2)                                                      
         MVC   BUFFWRST,33(R2)                                                  
*                                                                               
FILLX    XC    WORK,WORK                                                        
         XC    FACTOR,FACTOR                                                    
         XC    ACCUMDEM,ACCUMDEM                                                
         CLI   LASTPROG,X'FF'      EOF CALL                                     
         BE    PREPX                                                            
         B     FILL1                                                            
*                                                                               
PREPX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO RANK THE TABLE                                        
         SPACE 3                                                                
RANK     NTR1                                                                   
         USING BUFFD,R6                                                         
         LA    R6,BUFF                                                          
         ZIC   R3,NDEMOS                                                        
         BCTR  R3,0                                                             
         MH    R3,=H'3'                                                         
         LA    R2,BUFFDEM1                                                      
         SR    R2,R6               DISPLACEMENT TO FIRST DEMO                   
         AR    R2,R3               DISPLACEMENT TO LAST                         
         LA    R3,DEMOS(R3)        R3=DEMO                                      
         ZIC   R4,NDEMOS                                                        
         LH    R5,BUFFLINE                                                      
         SPACE 2                                                                
RAN10    GOTO1 XSORT,PARAS,(1,BUFF),(R5),49,2,(R2)                              
         BAS   RE,RAN40                                                         
         SPACE 2                                                                
         SH    R2,=H'3'                                                         
         SH    R3,=H'3'                                                         
         BCT   R4,RAN10                                                         
         B     XIT                                                              
         SPACE 2                                                                
*                                  ROUTINE TO PUT IN RANK NUMBERS               
RAN40    NTR1                                                                   
         LA    R2,BUFF(R2)         R2=A(COLUMN)                                 
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
RAN70    LA    R2,49(R2)                                                        
         LA    R4,1(R4)                                                         
         BCT   R5,RAN50                                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT A REPORT FROM BUFF                              
         SPACE 3                                                                
SPLAT    NTR1                                                                   
         LA    R6,BUFF                                                          
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
         XC    0(49,R6),0(R6)                                                   
         BCTR  R3,0                                                             
         LA    R6,49(R6)                                                        
         MVI   ALLOWLIN,0                                                       
         BCT   R2,SPL40                                                         
         GOTO1 SPOOL,PARAS,(R8)                                                 
         CH    R3,=H'0'                                                         
         BNE   SPL20                                                            
         B     XIT                                                              
         SPACE 2                                                                
*                                  FILL A PRINT LINE                            
FORMAT   NTR1                                                                   
         MVC   P+1(4),BUFFSTAT       STATION                                    
         CLI   BUFFSTAT+4,C'T'     ANY BAND (EXCEPT TV) OR SATELLITE            
         BE    FMT10                                                            
         MVI   P+5,C'-'                                                         
         MVC   P+6(1),BUFFSTAT+4   DISPLAY IT                                   
FMT10    MVC   P+9(16),BUFFPROG    PROGRAM                                      
*                                                                               
         GOTO1 INVEDIT,PARAS,BUFFINV,WORK                                       
         MVC   P+26(3),WORK        DAY                                          
         MVC   P+30(6),WORK+3      TIME                                         
         SPACE 1                                                                
         LA    R3,P+38                                                          
         TM    PRINTOPT,X'04'      SUPPRESS PURE NO.                            
         BO    FMT20                                                            
         MVC   0(4,R3),BUFFPURE                                                 
         LA    R3,5(R3)                                                         
         SPACE 1                                                                
FMT20    TM    PRINTOPT,X'10'      COST FIELD                                   
         BZ    FMT40                                                            
         ICM   R1,15,BUFFCOST                                                   
         TM    PRINTOPT,X'08'      ROUNDED DEMOS                                
         BO    FMT30                                                            
         CLI   DEMMOD,C'T'         IF PRECISION DEMOS, THEN                     
         BE    FMT30               T,Q, & P ARE ALREADY INTEGERS                
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
         ZIC   R5,NDEMOS                                                        
         SPACE 2                                                                
FMT50    TM    PRINTOPT,X'08'      ROUNDED DECIMAL                              
         BO    FMT55                                                            
         CLI   1(R4),C'I'          SPOT IMPS                                    
         BE    FMT60                                                            
         CLI   1(R4),C'S'          SHARE                                        
         BE    FMT60                                                            
         CLI   1(R4),C'R'          RATING                                       
         BE    FMT60                                                            
         CLI   1(R4),C'P'          PUT                                          
         BE    FMT60                                                            
         CLI   1(R4),C'E'          CANADIAN E RATING                            
         BE    FMT60                                                            
         CLI   1(R4),C'X'          TSA SHARE (INPUT AS T)                       
         BE    FMT60                                                            
FMT55    EDIT  (2,0(R2)),(5,0(R3)) NO DECIMAL                                   
         B     FMT70                                                            
         SPACE 2                                                                
FMT60    BCTR  R3,0                                                             
         EDIT  (2,0(R2)),(6,0(R3)),1     DEMO VALUE-1 DECIMAL                   
         LA    R3,1(R3)                                                         
         SPACE 2                                                                
FMT70    EDIT  (1,2(R2)),(3,6(R3))       RANK                                   
         SPACE 2                                                                
         LA    R2,3(R2)                                                         
         LA    R3,11(R3)                                                        
         LA    R4,3(R4)                                                         
         BCT   R5,FMT50                                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* HOOK ROUTINE FOR HEADLINE DETAILS                                             
*                                                                               
HOOK     NTR1                                                                   
         MVC   H5+90(3),FLESRCE    SOURCE                                       
         MVC   H5+94(8),FLEBOOK    BOOK                                         
*                                                                               
*  IF WEEKS, MIN OR MAX DEMO, OR COST FILTERS, SHOW IN HEADLINES                
*                                                                               
HOOK05   LA    R3,H4                                                            
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
HOOK25   LA    R3,H5+75                                                         
         TM    PRINTOPT,X'10'      COST FIELD ENTERED?                          
         BZ    HOOK30                                                           
         MVC   0(7,R3),=C'CPM - $'                                              
         CLI   DEMMOD,C'R'                                                      
         BE    *+12                                                             
         CLI   DEMMOD,C'E'                                                      
         BNE   *+8                                                              
         MVI   2(R3),C'P'                                                       
         L     R2,COST                                                          
         EDIT  (R2),(10,7(R3)),2,ALIGN=LEFT                                     
         SPACE 1                                                                
* SET UP COLUMNS FOR BOXES                                                      
HOOK30   LA    R4,RESTITC          POINT TO BOX SET UP COLUMNS                  
*                                                                               
         MVI   0(R4),C'L'                                                       
         MVI   08(R4),C'C'                                                      
         MVI   25(R4),C'C'                                                      
         MVI   29(R4),C'C'                                                      
*                                                                               
HOOK35   LA    R4,37(R4)                                                        
         LA    R3,H8+38                                                         
         LA    R5,H9+38                                                         
         TM    PRINTOPT,X'04'      SUPPRESS PURE NO.                            
         BO    HOOK40                                                           
         MVC   0(4,R3),=C'PURE'                                                 
         MVI   0(R4),C'C'                                                       
         LA    R3,5(R3)                                                         
         LA    R4,5(R4)                                                         
         LA    R5,5(R5)                                                         
*                                                                               
HOOK40   TM    PRINTOPT,X'10'      COST FIELD                                   
         BZ    HOOK50                                                           
         MVC   0(9,R3),=C'PROJECTED'                                            
         MVC   2(4,R5),=C'COST'                                                 
         MVI   0(R4),C'C'                                                       
         LA    R3,10(R3)                                                        
         LA    R4,10(R4)                                                        
         LA    R5,10(R5)                                                        
*                                                                               
HOOK50   LA    R2,DEMOS                                                         
         ZIC   R6,NDEMOS                                                        
HOOK60   CLI   1(R2),C'T'                                                       
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
         GOTO1 DEMOCON,PARAS,(0,(R2)),(7,WORK),(0,DBLOCK)                       
         MVC   1(7,R3),WORK                                                     
         MVC   0(5,R5),WORK+7                                                   
         MVC   6(3,R5),=C'RNK'                                                  
         MVI   0(R4),C'C'                                                       
HOOK70   LA    R2,3(R2)                                                         
         LA    R3,11(R3)                                                        
         LA    R4,11(R4)                                                        
         LA    R5,11(R5)                                                        
         BCT   R6,HOOK60                                                        
         MVI   0(R4),C'R'                                                       
         SPACE 2                                                                
* SET UP THE BOXES                                                              
         L     R4,ABOX             HANDLE BOXES IF WE'RE OFF LINE               
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    HOOKX                                                            
*                                                                               
         MVI   BOXYORN,C'Y'        GLOBAL OPTION TO SUPPRESS                    
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
*                                                                               
         MVC   BOXCOLS,RESTITC                                                  
*                                                                               
HOOKX    B     XIT                                                              
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
NOFOUND  DC    C'** ERROR ** RECORD NOT FOUND - '                               
IUNBLK   DC    C'IUN'              DBFILE                                       
         DC    C'T'                MEDIA                                        
         DC    C'IUN'              INTERNAL VALUES                              
         DC    AL1(82)                                                          
         DC    AL1(11)                                                          
         DC    X'00'                                                            
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
*                                                                               
         SSPEC H1,1,C'MEDIA     SPOT T.V.'                                      
         SSPEC H1,42,C'TIME PERIOD RANKER (FLEXI)'                              
         SSPEC H1,77,AGYNAME                                                    
*                                                                               
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,42,26C'-'                                                     
         SSPEC H2,77,AGYADD                                                     
*                                                                               
         SSPEC H4,77,MEDIA                                                      
*                                                                               
         SSPEC H5,77,C'SOURCE BOOK -'                                           
         SSPEC H5,103,PAGE                                                      
         SPACE 1                                                                
         SPROG 0                                                                
         SSPEC H8,01,C' STATION     PROGRAM      DAY  TIME'                     
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER RECORDS IN BUFF                                   
         SPACE 3                                                                
BUFFD    DSECT                                                                  
BUFFREC  DS    0CL49                                                            
BUFFSTAT DS    CL5                                                              
BUFFPROG DS    CL16                                                             
BUFFINV  DS    CL2                                                              
BUFFINV2 DS    CL2                                                              
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
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPRESWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER SCREEN                                                         
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESD2D                                                       
         EJECT                                                                  
* SAVE AREA VALUES                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
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
OUTAREA  DS    CL16                EXTRA WORK AREA                              
IUBLOCK  DS    CL10                IUN BLOCK                                    
SAVBK    DS    CL2                                                              
FACTOR   DS    H                                                                
CURFACTR DS    H                                                                
LASTPROG DS    CL16                                                             
THISPROG DS    CL16                CURRENT PROGRAM IN FILL HOOK                 
FRSTPROG DS    CL16                FIRST PROGRAM IN FILL HOOK                   
ENDQTRHR DS    CL16                END QUARTER HOUR                             
ACCUMDEM DS    CL24                                                             
MYDBLOCK DS    CL200                                                            
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
         EJECT                                                                  
IUNREC   DSECT                                                                  
UPREC    DS    0F                                                               
***********************************************************************         
*                                  ORIGINAL BOOK VALUES               *         
***********************************************************************         
OLDUNV   DS    (NUMVALS)F          UNIVERSES                          *         
OLDUNVX  EQU   *                                                      *         
***********************************************************************         
OLDRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   OLDRTG+(DISPHOM*4)                                               
UORHOMES DS    F                                                      *         
         ORG                                                                    
OLDIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
OLDRTGX  EQU   *                                                      *         
***********************************************************************         
OLDHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   OLDHPT+(DISPHOM*4)                                               
UOPHOMES DS    F                                                      *         
         ORG                                                                    
OLDTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
         ORG   OLDTOT+(DISPHOM*4)                                               
UOQHOMES DS    F                                                      *         
         ORG                                                                    
OLDHPTX  EQU   *                                                      *         
***********************************************************************         
*                                  NEW VALUES                         *         
NEWUNV   EQU   OLDTOT              DEFINE ORIGIN FOR SPGETIUN CALL    *         
*                                                                     *         
***********************************************************************         
NEWRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   NEWRTG+(DISPHOM*4)                                               
UNRHOMES DS    F                                                      *         
         ORG                                                                    
NEWIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
NEWRTGX  EQU   *                                                      *         
***********************************************************************         
NEWHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   NEWHPT+(DISPHOM*4)                                               
UNPHOMES DS    F                                                      *         
         ORG                                                                    
NEWTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
NEWHPTX  EQU   *                                                      *         
***********************************************************************         
*                                  OTHER VALUES                       *         
***********************************************************************         
HOMSHR   DS    3F                  ORIGINAL HOMES SHARES              *         
HOMSHRX  EQU   *                                                      *         
HOMSHRLN EQU   *-HOMSHR                                               *         
***********************************************************************         
LUNV     DS    (NUMVALS)F          LOONEYVERSES                       *         
LUNVX    EQU   *                                                      *         
***********************************************************************         
UPRECX   DS    0F                                                               
*                                                                               
NUMVALS  EQU   32                                                               
DISPHOM  EQU   20                                                               
LENVALS  EQU   NUMVALS*4                                                        
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
*                                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049SPRES02   05/01/02'                                      
         END                                                                    
