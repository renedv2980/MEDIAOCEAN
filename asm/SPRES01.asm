*          DATA SET SPRES01    AT LEVEL 005 AS OF 05/01/02                      
*PHASE T20F01A,+0                                                               
         TITLE 'T20F01 - PURE PROGRAMMING INVENTORY LISTING'                    
T20F01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1F01**,RA,RR=R2                                              
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
         LA    R2,PURSRCEH         VALIDATE SOURCE                              
         GOTO1 VVALSRC                                                          
         SPACE 1                                                                
         LA    R2,PURBOOKH         VALIDATE BOOK                                
         MVI   MAX,10              DUMMY SO I CAN DO ERROR MSG                  
         GOTO1 VVALBOOK                                                         
         CLI   NBOOKS,1            REALLY ONLY 1 BOOK ALLOWED                   
         BNH   VKEY5                                                            
         MVI   ERROR,MANYBKS       TOO MANY BOOKS                               
         B     ERREND                                                           
         SPACE 1                                                                
VKEY5    LA    R2,PURDEMOH         VALIDATE DEMOS                               
         MVI   NDEMOS,1            PRESET TO 1 DEMO                             
         MVC   DEMOS(3),=X'00D901' PRESET TO RATING                             
         CLI   5(R2),0                                                          
         BE    VKEY10                                                           
         MVI   MAX,20              DUMMY SO I CAN DO ERROR MSG                  
         MVI   NFLDS,1                                                          
         GOTO1 VVALDEM                                                          
         CLI   NDEMOS,11           REALLY ALLOW 11 DEMOS                        
         BNH   VKEY7                                                            
         MVI   ERROR,MANYDEM                                                    
         B     ERREND                                                           
         SPACE 1                                                                
VKEY7    CLI   NDEMOS,2                                                         
         BNH   VKEY10                                                           
         OI    PRINTOPT,X'80'      NOTE - MORE THAN 2 DEMOS                     
         SPACE 1                                                                
VKEY10   LA    R2,PURSTATH         VALIDATE STATION                             
         LA    R3,8                                                             
         LA    R4,STATS                                                         
         LA    R5,MKTSV                                                         
         XC    STATS,STATS                                                      
         XC    MKTSV(144),MKTSV                                                 
         XC    MKTSV+144(144),MKTSV+144                                         
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
VKEY20   GOTO1 VVALSTA                                                          
         MVC   0(5,R4),ACTSTAT                                                  
*GET MARKET VALUE                                                               
         L     R6,AIO1                                                          
         USING STAHDRD,R6                                                       
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,ACTMKT                                                        
         GOTO1 VGETMKT                                                          
*                                                                               
         L     R6,AIO1                                                          
         USING MKTHDRD,R6                                                       
         MVC   0(24,R5),MKTNAME    SAVE MARKET NAME                             
*                                                                               
         LA    R4,5(R4)                                                         
         LA    R5,24(R5)                                                        
         BAS   RE,BUMP                                                          
         BCT   R3,*+8                                                           
         B     VKEY30                                                           
         CLI   5(R2),0             ANOTHER STATION?                             
         BNE   VKEY20                                                           
         SPACE 1                                                                
VKEY30   CLI   DBSELMED,C'R'                                                    
         BNE   VKEY35                                                           
*                                                                               
         MVC   STIM,=H'500'        RADIO DEFAULT                                
         MVC   ETIM,=H'445'        RADIO DEFAULT                                
         B     VKEY40                                                           
*                                                                               
VKEY35   MVC   STIM,=H'600'        TELEVISION DEFAULT                           
         MVC   ETIM,=H'545'        TELEVISION DEFAULT                           
         SPACE 1                                                                
VKEY40   LA    R2,PURSTIMH         START TIME                                   
         CLI   5(R2),0                                                          
         BE    VKEY45                                                           
         GOTO1 VVALTIM                                                          
         MVC   STIM,WORK                                                        
         OI    PRINTOPT,X'20'      START TIME FILTER                            
         SPACE 1                                                                
VKEY45   LA    R2,PURETIMH         END TIME                                     
         CLI   5(R2),0                                                          
         BE    VKEY50                                                           
         GOTO1 VVALTIM                                                          
         MVC   ETIM,WORK                                                        
         OI    PRINTOPT,X'10'      END TIME FILTER                              
         SPACE 1                                                                
VKEY50   MVI   BOXOPT,C'Y'                                                      
         MVI   UP,2                DEFAULT IS 2 UP                              
         TM    PRINTOPT,X'80'      BUT, IF MORE THAN 2 DEMOS (X'80')            
         BZ    *+8                                                              
         MVI   UP,1                MUST PRINT 1 UP ON PAGE                      
         SPACE 1                                                                
         LA    R2,PUROPT2H         SPACING OPTION                               
         MVI   ERROR,2             INVALID INPUT                                
         MVI   GAP,1               DEFAULT SINGLE SPACING                       
         CLI   5(R2),0                                                          
         BE    VKEYX                                                            
         CLI   8(R2),C'1'                                                       
         BE    VKEYX                                                            
         CLI   8(R2),C'2'                                                       
         BNE   ERREND                                                           
         MVI   GAP,2                                                            
         SPACE 1                                                                
VKEYX    B     XIT                                                              
         EJECT                                                                  
* PRINT DATA TYPE REPORT                                                        
* R4 POINTS AT DBLOCKA1 - MAIN DBLOCK AREA                                      
*                                                                               
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         MVI   MAXLINES,59                                                      
         MVI   RCSUBPRG,0          2 UP (DEFAULT FORMAT)                        
         CLI   UP,1                                                             
         BNE   *+8                                                              
         MVI   RCSUBPRG,1          1 UP FORMAT                                  
         MVI   COUNT,0             COUNT NUMBER OF LINES ON PAGE                
         SPACE 1                                                                
         LA    R2,BUFF             CLEAR OUT BUFFER AREA                        
         LA    R3,48                                                            
CLEAR    MVC   0(110,R2),SPACES                                                 
         LA    R2,110(R2)                                                       
         BCT   R3,CLEAR                                                         
         EJECT                                                                  
*   CONTROL I/O                                                                 
         SPACE 2                                                                
         LA    R2,PURSTATH         STATIONS ON SCREEN                           
         ST    R2,SAVE2                                                         
         LA    R3,8                UP TO 8 STATIONS                             
         LA    R5,STATS            LIST OF STATIONS IN DBSELSTA FMT             
         LA    R6,MKTSV            LIST OF MARKET NAMES                         
         ST    R6,SAVER6                                                        
         SPACE 1                                                                
*  BUILD REST OF DBLOCK                                                         
         SPACE 1                                                                
PURE     LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBSELSTA,0(R5)                                                   
         MVC   DBSELBK,BOOKS+1                                                  
         MVC   DBBTYPE,BOOKS+3      BOOK TYPE                                   
         MVI   DBBEST,C'A'                                                      
         MVI   DBSELDAY,X'7F'                                                   
         MVC   DBSELTIM(2),STIM                                                 
         MVC   DBSELTIM+2(2),ETIM                                               
         GOTO1 DEMAND,DMCB,DBLOCK,FILL                                          
         CLI   DBERROR,X'10'       RECORD NOT FOUND                             
         BNE   PURE1                                                            
         MVC   WORK(L'NOFOUND),NOFOUND                                          
         MVC   WORK+L'NOFOUND(5),0(R5) IDENTIFY STATION                         
         LA    R2,WORK+L'NOFOUND+5                                              
         MVI   0(R2),C'/'                                                       
         MVC   1(8,R2),PURBOOK     AND BOOK                                     
         LA    R2,PURSTATH                                                      
         B     MYEND                                                            
*                                                                               
PURE1    CLI   DBERROR,X'80'       TEST FOR EOF                                 
         BNE   PURE2               SKIP LISTING                                 
         OC    DBDIVSOR,DBDIVSOR   TEST FOR ANY RECORDS                         
         BZ    PURE2               NO                                           
         BAS   RE,SPLAT                                                         
         MVC   PAGE,=H'1'                                                       
         BCT   R3,*+8                                                           
         B     XIT                                                              
         SPACE 1                                                                
PURE2    BAS   RE,BUMP             NEXT STATION ON SCREEN                       
         LA    R5,5(R5)            NEXT DBSELSTA FORMATTED STATION              
         LA    R6,24(R6)           NEXT MARKET NAME                             
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
         ZIC   R5,COUNT                                                         
         LA    R3,BUFF                                                          
         CLI   UP,2                                                             
         BNE   FILL10                                                           
         CLI   COUNT,96                                                         
         BNE   *+10                                                             
         SR    R5,R5                                                            
         BAS   RE,SPLAT                                                         
         CLI   COUNT,48                                                         
         BL    FILL40                                                           
         LA    R3,BUFF                                                          
         LA    R3,55(R3)                                                        
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
         GOTO1 DEFINE,PARAS,=C'TIME',DBLOCK,WORK                                
*                                                                               
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),WORK+6    SAVE PRGRAM QTR HOURS                        
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(2),WORK+2       EXTRACT START TIME                           
         MVC   WORK,SPACES                                                      
         GOTO1 UNTIME,(R1),DUB,WORK                                             
*                                                                               
         CLC   LASTIME,WORK                                                     
         BE    *+10                                                             
         MVC   1(6,R3),WORK        SHOW TIME IF DIFFERENT                       
         MVC   LASTIME,WORK                                                     
         GOTO1 DEFINE,PARAS,=C'DAY',DBLOCK,WORK                                 
         MVC   13(3,R3),WORK+2     EXTRACT 3 BYTE ALPHA DAY                     
*   CALCULATE DAYS X QTR HOURS                                                  
         LA    R2,7                                                             
         CLI   WORK,X'7F'          M-SU                                         
         BNE   *+12                                                             
         MH    R2,HALF                                                          
         B     FILL50                                                           
         LA    R2,5                                                             
         CLI   WORK,X'7C'          M-F                                          
         BNE   *+12                                                             
         MH    R2,HALF                                                          
         B     FILL50                                                           
         LA    R2,2                                                             
         CLI   WORK,X'03'          SA-SU                                        
         BNE   FILL55              ELSE, SINGLE DAY                             
         MH    R2,HALF                                                          
FILL50   STH   R2,HALF                                                          
*                                                                               
FILL55   GOTO1 (RF),(R1),=C'PURE',DBLOCK,WORK                                   
         MVC   8(4,R3),WORK+3      EXTRACT EDITED PURE NUMBER                   
*                                                                               
         GOTO1 (RF),(R1),=C'PROGRAM',DBLOCK,WORK                                
         MVC   17(16,R3),WORK                                                   
         SPACE 2                                                                
         BAS   RE,COUNTWK          PRINT WEEKS AND                              
         EDIT  (1,NWKS),(1,35(R3))                                              
*                                                                               
         CLC   13(3,R3),=C'VAR'    BLANK OUT QTR HRS                            
         BE    FILL58              FOR VAR AND TYP                              
         CLC   13(3,R3),=C'TYP'                                                 
         BE    FILL58                                                           
*                                                                               
         ZIC   R2,NWKS           TOTAL QTR HRS=DAYS X QTR HRS X WEEKS           
         MH    R2,HALF                                                          
         EDIT  (R2),(3,38(R3))                                                  
         SPACE 2                                                                
FILL58   MVC   SAVBK,DBACTBK                                                    
         MVC   SVSOURCE,DBSELSRC                                                
         MVC   MYDBLOCK(100),DBLOCK                                             
         CLI   DBSELMED,C'T'                                                    
         BNE   FILL59                                                           
         LA    RF,LIOS                                                          
         L     RE,AIO2                                                          
         XCEF                                                                   
         GOTO1 VGETIUN,DMCB,(4,DBLOCK),AIO2                                     
         L     RF,AIO2                                                          
         USING IUNREC,RF                                                        
         MVC   NEWRTG(LENVALS),OLDRTG                                           
         MVC   NEWIMP(LENVALS),OLDIMP                                           
         MVC   NEWHPT(LENVALS),OLDHPT                                           
         MVC   NEWTOT(LENVALS),OLDTOT                                           
         XC    WORK,WORK                                                        
         MVC   WORK+20(2),=H'25'                                                
         MVC   WORK+23(2),=X'2002'                                              
         MVC   SAVEAQH,DBAQUART                                                 
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBACTSRC,C'N'                                                    
         MVC   DBFILE,=C'IUN'                                                   
         MVI   DBINTMED,C'U'                                                    
         MVI   DBINTFIL,C'I'                                                    
         LA    RF,WORK                                                          
         ST    RF,DBAREC                                                        
         LA    RF,WORK+23                                                       
         ST    RF,DBAQUART                                                      
         MVC   IUBLOCK,IUNBLK                                                   
         MVC   IUBLOCK+7(2),=X'520B'                                            
         CLC   SAVBK,=X'5801'                                                   
         BL    *+10                                                             
         MVC   IUBLOCK+7(2),=X'530B'                                            
         GOTO1 DEMEL,DMCB,(C'C',IUBLOCK),DBLOCK,AIO2                            
         L     RF,AIO2                                                          
         LA    RF,2(RF)                                                         
         ST    RF,DBAQUART                                                      
         SPACE 1                                                                
FILL59   ZIC   R1,NDEMOS                                                        
         MH    R1,=H'3'                                                         
         LA    R1,DEMOS(R1)                                                     
         MVI   0(R1),X'FF'                                                      
         LA    R2,DEMOS                                                         
         LA    R6,42(R3)                                                        
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCK,ELEM                              
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DBACTBK,SAVBK       RESTORE IT                                   
         LA    R5,ELEM             NOW POINT R5 TO VALUES                       
         ZIC   R1,NDEMOS                                                        
         SPACE 2                                                                
FILL60   EDIT  (4,0(R5)),(5,0(R6)),1   1 DECIMAL POINT FOR                      
         CLI   1(R2),C'R'          RATING                                       
         BE    FILL80                                                           
         CLI   1(R2),C'S'          SHARE                                        
         BE    FILL80                                                           
         CLI   1(R2),C'X'          TSA                                          
         BE    FILL80                                                           
         EDIT  (4,0(R5)),(5,0(R6))   ALL ELSE, INTEGER                          
         SPACE 2                                                                
FILL80   LA    R2,3(R2)            NEXT DEMO                                    
         LA    R6,6(R6)            NEXT DEMO AREA                               
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         BCT   R1,FILL60                                                        
         MVC   DBLOCK(100),MYDBLOCK                                             
         EJECT                                                                  
PREPX    B     XIT                                                              
         EJECT                                                                  
*  OUTPUT A PAGE OF PRINTING                                                    
SPLAT    NTR1                                                                   
         LA    R2,BUFF                                                          
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
         GOTO1 DEFINE,PARAS,=C'WEEK',DBLOCK,WORK                                
         NI    WORK,X'0F'                                                       
         MVI   NWKS,0                                                           
         ZIC   R1,WORK             GET WEEK BITS FROM DEFINE                    
         LA    R1,NUMTAB(R1)                                                    
         MVC   NWKS,0(R1)                                                       
         CLI   PURSRCE,C'A'        IF ARBITRON                                  
         BNE   XIT                                                              
         CLC   BOOKS+1(2),=X'5605'  AND BEFORE MAY86                            
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
         BAS   RE,HOOKHEAD                                                      
         BAS   RE,HOOKBOX                                                       
         B     XIT                                                              
         SPACE 2                                                                
HOOKHEAD NTR1                                                                   
         L     R2,SAVE2                                                         
         L     R6,SAVER6                                                        
         MVC   H4+10(6),8(R2)      STATION CALL LETTERS                         
         MVC   H4+17(24),0(R6)     MARKET NAME                                  
         MVC   H5+90(3),PURSRCE    SOURCE                                       
         MVC   H5+94(8),PURBOOK    BOOK                                         
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
         BZ    HOOK30                                                           
         XC    DUB,DUB                                                          
         MVC   0(10,R3),=C'END TIME -'                                          
         MVC   DUB(2),ETIM                                                      
         GOTO1 UNTIME,PARAS,DUB,11(R3)                                          
         SPACE 2                                                                
HOOK30   MVC   H8,SPACES                                                        
         MVC   H9,SPACES                                                        
         MVC   H10,SPACES                                                       
         MVC   H8(41),=C'  TIME  PURE DAY PROGRAM          WKS QTR'             
         MVC   H9+9(3),=C'NO.'                                                  
         SPACE 1                                                                
         LA    R3,H8+42            NOW POINT TO WHERE                           
         LA    R5,H9+42            1ST DEMO SHOULD PRINT                        
         LA    R2,DEMOS                                                         
         ZIC   R6,NDEMOS                                                        
         SPACE 1                                                                
HOOK60   GOTO1 DEMOCON,PARAS,(0,(R2)),(5,WORK),(0,DBLOCK)                       
*                                                                               
         MVC   0(5,R3),WORK        FORMAT                                       
         MVC   0(5,R5),WORK+5                                                   
         LA    R2,3(R2)                                                         
         LA    R3,6(R3)                                                         
         LA    R5,6(R5)                                                         
         BCT   R6,HOOK60                                                        
         CLI   UP,2                                                             
         BNE   XIT                                                              
         MVC   H8+55(55),H8                                                     
         MVC   H9+55(55),H9                                                     
         B     XIT                                                              
         EJECT                                                                  
HOOKBOX  NTR1                                                                   
         L     R4,ABOX             HANDLE BOXES IF WE'RE OFF LINE               
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
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
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXCOLS                                                       
         MVI   0(R2),C'L'                                                       
         MVI   07(R2),C'C'                                                      
         MVI   12(R2),C'C'                                                      
         MVI   16(R2),C'C'                                                      
         MVI   33(R2),C'C'                                                      
         MVI   37(R2),C'C'                                                      
         SPACE 1                                                                
         LA    R2,BOXCOLS+41                                                    
         ZIC   R3,NDEMOS           NOW SET UP FOR DEMOS                         
         SPACE 1                                                                
HB2      MVI   0(R2),C'C'                                                       
         LA    R2,6(R2)                                                         
         BCT   R3,HB2                                                           
         MVI   0(R2),C'R'                                                       
         CLI   UP,2                PROLIFERATE IF 2 UP                          
         BNE   XIT                                                              
         MVC   BOXCOLS+55(55),BOXCOLS                                           
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
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
**   MY OWN ERROR MESSAGES   **                                                 
NOFOUND  DC    C'** ERROR ** RECORD NOT FOUND -'                                
IUNBLK   DC    C'IUN'              DBFILE                                       
         DC    C'T'                MEDIA                                        
         DC    C'IUN'              INTERNAL VALUES                              
IUNBLKB  DC    AL1(82)                                                          
         DC    AL1(11)                                                          
         DC    X'00'                                                            
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
*                                                                               
         SSPEC H1,1,C'MEDIA     SPOT T.V.'                                      
         SSPEC H1,43,C'PURE PROGRAMMING LISTING'                                
         SSPEC H1,77,AGYNAME                                                    
*                                                                               
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,43,24C'-'                                                     
         SSPEC H2,77,AGYADD                                                     
*                                                                               
         SSPEC H4,1,C'STATION -'                                                
         SSPEC H4,77,MEDIA                                                      
*                                                                               
         SSPEC H5,77,C'SOURCE BOOK -'                                           
         SSPEC H5,103,PAGE                                                      
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPRESWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER SCREEN                                                         
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESD1D                                                       
         EJECT                                                                  
* SAVE AREA VALUES                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
SAVE2    DS    F                   SAVE R2                                      
SAVER5   DS    F                   SAVE R5                                      
SAVER6   DS    F                   SAVE R6                                      
SAVEAQH  DS    F                   SAVE DBAQUART                                
COST     DS    F                   FOR CPP/CPM                                  
UP       DS    CL1                 1 UP OR 2 ON PAGE                            
LASTIME  DS    CL6                 LAST TIME PRINTED                            
NWKS     DS    CL1                 NUMBER OF WEEKS                              
PRINTOPT DS    XL1                 X'80'  MORE THAN 2 DEMOS                     
*                                  X'40'  UNUSED                                
*                                  X'20'  START TIME FILTER                     
*                                  X'10'  END TIME FILTER                       
SVSOURCE DS    CL1                 SAVE SOURCE CODE                             
SAVBK    DS    CL2                 SAVE BOOK                                    
IUBLOCK  DS    CL10                                                             
MKTSV    DS    CL288               MARKET SAVE AREA 12 MARKETS                  
MYDBLOCK DS    CL100                                                            
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
         EJECT                                                                  
STAHDRD  DSECT                                                                  
*SPGENSTA                                                                       
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
MKTHDRD  DSECT                                                                  
*SPGENMKT                                                                       
       ++INCLUDE SPGENMKT                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPRES01   05/01/02'                                      
         END                                                                    
