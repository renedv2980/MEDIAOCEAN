*          DATA SET SPRES95    AT LEVEL 053 AS OF 05/01/02                      
*PHASE T20F95A,*                                                                
*INCLUDE SPGETIUN                                                               
*INCLUDE BINSRCH                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T20F95 - OVERNIGHT RESEARCH'                                    
T20F95  CSECT                                                                   
         PRINT NOGEN                                                            
         NMOD1 0,T20F95,RR=R2                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING T20F95+4096,RA                                                   
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     R7,=A(RESWK)                                                     
         USING RESWK,R7                                                         
*                                                                               
         ST    R2,RELO                                                          
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         SPACE 1                                                                
         CLI   DDS,C'Y'                                                         
         BE    RES10                                                            
         CLC   DBFILE,=C'PAV'                                                   
         BNE   *+10                                                             
         MVC   DBFILE,=C'TP '                                                   
*                                                                               
         SPACE 1                                                                
RES10    MVI   SHARCALC,C'N'                                                    
         MVI   DBFUNCT,DBGETCTL                                                 
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         TM    DBCSHR,DBOTOSHR     TEST COMPUTE SHARES                          
         B     *+8                 ***************  NOP ************            
         MVI   SHARCALC,C'Y'       INDICATE SHARE COMP REQUIRED                 
*                                                                               
         MVI   DBFUNCT,DBGETDEM    RESTORE DEMO LOOK-UP FN                      
         MVC   DBAREC,AIO2         SET IO AREA ADDRESS                          
*                                                                               
         GOTO1 =A(INIT),DMCB,(RC)                                               
         EJECT                                                                  
* PROCESS (NEXT) STATION                                                        
         SPACE 1                                                                
PROCSTA  DS    0H                                                               
         MVC   DBBTYPE,SURVOPT     SPECIAL SURVEYS                              
         MVC   DBSELWKN,WEEKOPT    WEEK OPTION                                  
         XC    SIDMKT,SIDMKT                                                    
*                                                                               
         LA    R2,STATS            POINT TO STATION LIST                        
         ZIC   R3,NSTATS           AND COUNT                                    
*                                                                               
         CLI   MGROPT,C'Y'         TEST PROCESS BY MKTGRP                       
         BNE   NSTA10              NO                                           
*                                                                               
         XC    SVMGRKEY,SVMGRKEY   INDICATE FIRST TIME                          
*                                                                               
NSTA2    XC    DBSELSTA,DBSELSTA   CLEAR                                        
         XC    ACTSTAT,ACTSTAT                                                  
*                                                                               
         GOTO1 =A(NEXTMGR),DMCB,(RC)  GET NEXT MARKET                           
*                                                                               
         OC    SIDMKT,SIDMKT       TEST ANY MORE                                
         BZ    RPT                                                              
         BAS   RE,PROCBK                                                        
         B     NSTA2                                                            
         EJECT                                                                  
NSTA10   DS    0H                                                               
         XC    SORTSTAT,SORTSTAT                                                
         MVC   SORTSTAT(5),0(R2)   MOVE STATION                                 
         LA    R1,SORTSTAT+4                                                    
         CLI   0(R1),C'T'                                                       
         BNE   NSTA12                                                           
         MVI   0(R1),C' '                                                       
         B     NSTA14                                                           
*                                                                               
NSTA12   MVC   1(1,R1),0(R1)                                                    
         MVI   0(R1),C'-'                                                       
         LA    R1,2(R1)                                                         
*                                                                               
NSTA14   OC    5(2,R2),5(R2)       TEST SPILL  MARKET                           
         BZ    NSTAX                                                            
         MVI   0(R1),C'/'                                                       
         SR    R0,R0                                                            
         ICM   R0,3,5(R2)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R1),DUB                                                      
*                                                                               
NSTAX    MVC   ACTSTAT,0(R2)       MOVE CURRENT STATION                         
         MVC   ACTMKT,5(R2)        MOVE CURRENT MARKET                          
         MVC   DBSELSTA,0(R2)                                                   
         MVC   DBSELMK,5(R2)       OPTIONAL SWEEP MARKET NUMBER                 
*                                                                               
         BAS   RE,PROCBK                                                        
*                                                                               
         LA    R2,7(R2)                                                         
         BCT   R3,NSTA10                                                        
*                                                                               
         B     RPT                 GO PRINT REPORT                              
         EJECT                                                                  
* PROCESS BOOKS *                                                               
         SPACE 1                                                                
PROCBK   NTR1                                                                   
         LA    R2,BOOKS                                                         
         LA    R3,1                REQUEST BOOK SEQUENCE                        
         SPACE 1                                                                
NEXTBK   XC    SORTBOOK,SORTBOOK                                                
         MVC   SORTBOOK(3),1(R2)   MOVE YEAR/MONTH/TYPE                         
         MVC   DBSELBK,1(R2)                                                    
         MVC   DBBTYPE,3(R2)       SET SURVEY TYPE                              
         MVC   THISSIDP,SIDPER                                                  
         STC   R3,SORTRQBK         SET BOOK SEQUENCE NUMBER                     
         MVC   SORTRQBK+1(1),0(R2) CARRY TYPE OF BOOK                           
         MVC   THISBOOK,0(R2)                                                   
         MVI   ANYUP,C'N'                                                       
         TM    0(R2),X'20'         ESTIMATED BOOK                               
         BNO   NEXTBK2                                                          
         OC    UPOPT,UPOPT         SET UPGRADE ACTIVE                           
         BZ    *+8                                                              
         MVI   ANYUP,C'Y'                                                       
         OC    BOOKOPT,BOOKOPT                                                  
         BZ    NEXTBK2                                                          
         MVC   DBSELBK,BOOKOPT+1   USE BK= BOOK                                 
         SPACE 1                                                                
NEXTBK2  BAS   RE,PROCDT                                                        
         SPACE 1                                                                
         LA    R3,1(R3)                                                         
         LA    R2,4(R2)                                                         
         OC    0(4,R2),0(R2)                                                    
         BNZ   NEXTBK                                                           
         B     XIT                                                              
         EJECT                                                                  
* CONTROL MULTIPLE DAY/TIMES *                                                  
         SPACE 1                                                                
PROCDT   NTR1                                                                   
         LA    R2,DAYTMLST         SUPPORT MULTIPLE DAY/TIME                    
         CLI   NDAYTMS,0                                                        
         BNE   *+8                                                              
         MVI   NDAYTMS,1                                                        
         ZIC   R3,NDAYTMS                                                       
         SPACE 1                                                                
NEXTDT   MVC   DBSELDAY(5),0(R2)                                                
         MVC   SORTDP,0(R2)        SAVE START-END TIME                          
         ZIC   R1,DBSELDAY         ADJUST TO SPOT-TYPE DAY                      
         LA    R1,SPOTDAYS(R1)                                                  
         CLI   DBSELDAY,X'FF'      UNLESS ALL SELECTED                          
         BE    *+14                                                             
         MVC   DBSELDAY(1),0(R1)                                                
         B     NEXTDT2                                                          
         CLC   DBFILE,=C'PAV'      FOR PAV FF=ALL                               
         BE    NEXTDT2                                                          
         MVI   DBSELDAY,X'7F'      OTHERS LIKE 7F (M-S)                         
         B     NEXTDT4                                                          
         SPACE 1                                                                
NEXTDT2  DS    0H                                                               
         OC    DBSELTIM(2),DBSELTIM                                             
         BNZ   NEXTDT4                                                          
         MVC   DBSELTIM(2),=H'600' SET ALL TIME TO 6A-2A                        
         MVC   DBSELTIM+2(2),=H'200'                                            
         SPACE 1                                                                
NEXTDT4  DS    0H                                                               
         GOTO1 =A(SPOTDEM),DMCB,(RC)                                            
*                                                                               
         LA    R2,5(R2)                                                         
         BCT   R3,NEXTDT                                                        
         B     XIT                                                              
         EJECT                                                                  
* READ OUTPUT FILE AND PRINT REPORT *                                           
         SPACE 1                                                                
RPT      CLI   DOWNOPT,C'D'        TEST FOR DOWNLOAD                            
         BNE   RPT3                                                             
         GOTO1 =A(DOWN),DMCB,0     0=START OF REPORT                            
         CLI   ANYSORT,C'N'                                                     
         BNE   RPT2                                                             
         GOTO1 =A(DOWN),DMCB,4     4=NO DATA MESSAGE                            
         GOTO1 =A(DOWN),DMCB,5     5=END OF REPORT                              
         B     XIT                                                              
RPT2     MVI   FORCEHED,C'Y'       DEAL WITH DOWNLOAD HEADS                     
         MVI   SKIPSPEC,C'Y'                                                    
         XC    HEADHOOK,HEADHOOK                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*        L     RF,AHOOK                                                         
*        BASR  RE,RF                                                            
         SPACE 1                                                                
RPT3     CLI   ANYSORT,C'N'        TEST FOR NO DATA                             
         BE    XIT                                                              
*                                                                               
         MVI   ANYSORT,C'N'                                                     
         MVI   ANYRANK,C'N'                                                     
         MVI   SORTFRST,C'Y'                                                    
         XC    MYRANK,MYRANK                                                    
         XC    MYTIE,MYTIE                                                      
         XC    MYVALUE,MYVALUE                                                  
         XC    SORTMKT,SORTMKT                                                  
         XC    SORTSTAT,SORTSTAT                                                
         XC    MGR3,MGR3           FORCE MKTGRP READ                            
*                                                                               
         GOTO1 AGETSORT,DMCB,(RC)  SORT THE RECORDS                             
*                                                                               
         MVI   LASTKEY,X'FF'                                                    
         MVC   LASTKEY+1(L'LASTKEY-1),LASTKEY                                   
*                                                                               
         L     RE,=A(SOUT)                                                      
         LA    R0,RPTEND           GET EOD ADDRESS                              
         STCM  R0,7,33(RE)         SET IN DCB                                   
         OPEN  (SOUT,(INPUT))      NOW OPEN INPUT FILE                          
         B     RPT12                                                            
         SPACE 1                                                                
RPT10    MVC   LASTKEY,SORTKEY     SAVE PREVIOUS KEY                            
         SPACE 1                                                                
RPT12    LA    R0,SORTLEN          GET THE NEXT RECORD                          
         L     R1,=A(SOUT)                                                      
         GET   (R1),(R0)                                                        
*                                                                               
         LA    R2,DETS                                                          
         LA    R3,LASTKEY                                                       
         LA    R4,SORTKEY                                                       
         MVC   HEADP,SPACES                                                     
         LA    RE,HEADP                                                         
         A     RE,DISP                                                          
         ST    RE,NXTHEADP                                                      
         LA    RE,P                                                             
         A     RE,DISP                                                          
         ST    RE,NXTP                                                          
         ZIC   R0,NDETS                                                         
         XC    MYAPRANK,MYAPRANK                                                
*                                                                               
         CLI   EBOPT,C'Y'                                                       
         BE    RPT50                                                            
         EJECT                                                                  
RPT16    CLC   0(15,R4),=15X'FF'                                                
         BE    RPT100                                                           
         ZIC   R5,1(R2)            INDEX INTO DATALIST                          
         BCTR  R5,0                                                             
         MH    R5,=Y(L'DATALIST)                                                
         A     R5,ADTALIST                                                      
         CLI   DOWNOPT,C'D'                                                     
         BE    RPT18                                                            
         CLI   0(R2),C'R'          RANK PRINTS ANYWAY                           
         BE    RPT18                                                            
         CLI   0(R2),C'C'          TEST COMMENT                                 
         BNE   RPT16X                                                           
         GOTO1 =A(ROWS),DMCB,(RC),NXTP  NEVER FORMAT INTO HEADP                 
         B     RPT24                 SO DO IT MYSELF AND SKIP HEADP             
*                                                                               
RPT16X   DS    0H                  CALCULATE LENGTH FOR COMPARE                 
         LA    RE,DETS-4                                                        
         SR    RE,R2               GIVES NUM DETAILS X 4                        
         LPR   RE,RE                                                            
         SLL   RE,2                GIVES NUM DETAILS X 16                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SORTKEY(0),LASTKEY                                               
         BE    RPT20                                                            
         SPACE 1                                                                
RPT18    DS    0H                  SELECTIVELY FORMAT INTO P                    
         GOTO1 =A(ROWS),DMCB,(RC),NXTP                                          
         SPACE 1                                                                
RPT20    DS    0H                  ALWAYS FORMAT INTO HEADP                     
         GOTO1 =A(ROWS),DMCB,(RC),NXTHEADP                                      
         SPACE 1                                                                
RPT24    ZIC   RF,12(R5)           FIELD LENGTH (OUTPUT)                        
         L     RE,NXTP                                                          
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         LA    RE,1(RE,RF)                                                      
         ST    RE,NXTP                                                          
         L     RE,NXTHEADP                                                      
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         LA    RE,1(RE,RF)                                                      
         ST    RE,NXTHEADP                                                      
         LA    R2,4(R2)            NEXT DETAIL                                  
         LA    R3,16(R3)           NEXT KEY FIELD (PREVIOUS)                    
         LA    R4,16(R4)           NEXT KEY FIELD (CURRENT)                     
         BCT   R0,RPT16                                                         
         EJECT                                                                  
         MVI   AVSW,C'A'                                                        
         LA    RE,P                                                             
         ST    RE,NXTP                                                          
         BAS   RE,COLS                                                          
         L     R1,ARANK                                                         
         LTR   R1,R1                                                            
         BZ    RPT26                                                            
         CLC   4(4,R1),=F'1'       WAS A RAW RANK OF 1 SET?                     
         BNE   RPT26                                                            
         XC    MYRANK,MYRANK       THEN RESET VALUES                            
         XC    MYTIE,MYTIE                                                      
         XC    MYVALUE,MYVALUE                                                  
         SPACE 1                                                                
RPT26    OC    MINOPT,MINOPT       CHECK FOR MINIMUM OPTION                     
         BZ    RPT40                                                            
         CLC   MINTWO,MINOPT                                                    
         BL    RPT48                                                            
         SPACE 1                                                                
RPT40    OC    MAXOPT,MAXOPT       AND FOR MAXIMUM OPTION                       
         BZ    RPT44                                                            
         CLC   MAXTWO,MAXOPT                                                    
         BH    RPT48                                                            
         SPACE 1                                                                
RPT44    BAS   RE,SETRANK          GO AND FINALIZE THE RANKING                  
         OC    TOPOPT,TOPOPT                                                    
         BZ    RPT46                                                            
         CLC   MYRANK,TOPOPT                                                    
         BH    RPT48                                                            
         SPACE 1                                                                
RPT46    CLI   DOWNOPT,C'D'                                                     
         BNE   *+12                                                             
         BAS   RE,DOWNLINE                                                      
         B     RPT47                                                            
         SPACE 1                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
RPT47    MVC   HEADP,SPACES                                                     
         B     RPT10                                                            
         SPACE 1                                                                
RPT48    MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
         MVC   HEADP,SPACES                                                     
         B     RPT12                                                            
         EJECT                                                                  
* SPECIAL ROUTINES FOR EB FORMAT *                                              
         SPACE 1                                                                
RPT50    DS    0H                                                               
         LA    RE,BUFF             FORMAT ALL PRINT DATA TO BUFF                
         A     RE,DISP                                                          
         ST    RE,NXTP                                                          
*                                                                               
         LA    RF,5                                                             
         MVC   0(132,RE),SPACES                                                 
         LA    RE,132(RE)                                                       
         BCT   RF,*-10                                                          
         SPACE 1                                                                
* TEST TO PRINT ALL DETAIL *                                                    
         SPACE 1                                                                
         L     RE,AEBDTLX          POINT BEYOND LAST BLOCKED DTL                
         LA    RF,DETS                                                          
         SR    RE,RF               GIVES NUM DETAILS X 4                        
         SLL   RE,2                GIVES NUM DETAILS X 16                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SORTKEY(0),LASTKEY  TEST CHANGE IN ANY BLK DTL                   
         BE    RPT52                                                            
         MVI   LASTKEY,X'FF'       ELSE FORCE ALL TO PRINT                      
         MVC   LASTKEY+1(L'LASTKEY-1),LASTKEY                                   
*                                                                               
RPT52    DS    0H                                                               
         C     R2,AEBDTL           TEST BLOCKED DETAIL                          
         BNL   RPT58               YES                                          
*                                                                               
         CLC   0(15,R4),=15X'FF'                                                
         BE    RPT100                                                           
*                                                                               
         ZIC   R5,1(R2)            INDEX INTO DATALIST                          
         BCTR  R5,0                                                             
         MH    R5,=AL2(L'DATALIST)                                              
         A     R5,ADTALIST                                                      
*                                                                               
         ZIC   RE,8(R5)            FIELD LENGTH (INPUT)                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),0(R4)       TEST FIELD CHANGED                           
         BE    RPT56               NO - DO NOT FORMAT                           
*                                                                               
         GOTO1 =A(ROWS),DMCB,(RC),NXTP                                          
*                                                                               
RPT56    ZIC   RF,12(R5)           FIELD LENGTH (OUTPUT)                        
         L     RE,NXTP                                                          
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         LA    RE,1(RE,RF)                                                      
         ST    RE,NXTP                                                          
         LA    R2,4(R2)            NEXT DETAIL                                  
         LA    R3,16(R3)           NEXT KEY FIELD (PREVIOUS)                    
         LA    R4,16(R4)           NEXT KEY FIELD (CURRENT)                     
         BCT   R0,RPT52                                                         
         SPACE 1                                                                
* BLOCKED DETAILS *                                                             
         SPACE 1                                                                
RPT58    LA    RE,HEADP            USE HEADP AS TEMP WORK                       
         ST    RE,NXTP                                                          
         MVC   0(132,RE),SPACES                                                 
*                                                                               
RPT60    DS    0H                                                               
         ZIC   R5,1(R2)            INDEX INTO DATALIST                          
         BCTR  R5,0                                                             
         MH    R5,=AL2(L'DATALIST)                                              
         A     R5,ADTALIST                                                      
*                                                                               
         C     R2,AEBDTLX          TEST INPUT DETAIL                            
         BNL   RPT66               NO                                           
*                                                                               
         ZIC   RE,8(R5)            FIELD LENGTH (INPUT)                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),0(R4)       TEST FIELD CHANGED                           
         BE    RPT64               NO - DO NOT FORMAT                           
*                                                                               
RPT62    DS    0H                                                               
         GOTO1 =A(ROWS),DMCB,(RC),NXTP                                          
*                                                                               
RPT64    ZIC   RF,12(R5)           FIELD LENGTH (OUTPUT)                        
         L     RE,NXTP                                                          
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         LA    RE,1(RE,RF)         NEXT PRINT POSITION                          
         ST    RE,NXTP                                                          
         LA    R2,4(R2)            NEXT DETAIL                                  
         LA    R3,16(R3)           NEXT KEY FIELD (PREVIOUS)                    
         LA    R4,16(R4)           NEXT KEY FIELD (CURRENT)                     
         BCT   R0,RPT60                                                         
*                                                                               
RPT66    DS    0H                                                               
         GOTO1 SQUASHER,DMCB,HEADP,132                                          
         LA    RE,BUFF                                                          
         A     RE,DISP                                                          
         A     RE,EBDISP                                                        
         MVC   0(30,RE),HEADP      MOVE TO BUFFER                               
         MVC   HEADP,SPACES                                                     
*                                                                               
         LA    RE,BUFF             SET UP TO PRINT REMAINING DATA               
         LA    RE,132(RE)                                                       
         A     RE,DISP                                                          
         A     RE,EBDISP                                                        
         ST    RE,NXTP                                                          
         B     RPT70                                                            
*                                                                               
RPT68    DS    0H                                                               
         ZIC   R5,1(R2)            INDEX INTO DATALIST                          
         BCTR  R5,0                                                             
         MH    R5,=AL2(L'DATALIST)                                              
         A     R5,ADTALIST                                                      
*                                                                               
RPT70    DS    0H                                                               
         GOTO1 =A(ROWS),DMCB,(RC),NXTP                                          
*                                                                               
         L     RE,NXTP                                                          
         LA    RE,132(RE)          NEXT PRINT POSITION                          
         ST    RE,NXTP                                                          
         LA    R2,4(R2)            NEXT DETAIL                                  
         LA    R3,16(R3)           NEXT KEY FIELD (PREVIOUS)                    
         LA    R4,16(R4)           NEXT KEY FIELD (CURRENT)                     
         BCT   R0,RPT68                                                         
*                                                                               
         MVI   AVSW,C'A'                                                        
         LA    RE,BUFF                                                          
         ST    RE,NXTP                                                          
         BAS   RE,COLS                                                          
         SPACE 1                                                                
* MOVE DATA FROM BUFF TO PRINT LINES *                                          
         SPACE 1                                                                
         LA    R4,P1                                                            
         LA    R5,BUFF                                                          
         LA    R0,4                                                             
RPT72    MVC   0(132,R4),0(R5)                                                  
         OC    0(132,R4),SPACES                                                 
         LA    R4,132(R4)                                                       
         LA    R5,132(R5)                                                       
         BCT   R0,RPT72                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLC   0(132,R5),SPACES                                                 
         BE    RPT74                                                            
         MVC   0(132,R4),0(R5)     MOVE LINE 5                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
RPT74    B     RPT10                                                            
         EJECT                                                                  
* CONTROL TOTALLING AND AVERAGES *                                              
         SPACE 1                                                                
RPT100   L     R5,NXTP                                                          
         MVC   P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)     SPACE 1                                      
         CLI   15(R4),C'A'                                                      
         BE    RPT102                                                           
         CLI   15(R4),C'B'                                                      
         BNE   RPT104                                                           
         SPACE 1                                                                
RPT102   MVI   AVSW,C'A'                                                        
         MVC   0(5,R5),=C'(AVE)'   SHOW AVERAGES                                
         LA    RE,P                                                             
         ST    RE,NXTP                                                          
         BAS   RE,COLS                                                          
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
RPT104   CLI   15(R4),C'T'                                                      
         BE    RPT106                                                           
         CLI   15(R4),C'B'                                                      
         BNE   RPT10                                                            
         SPACE 1                                                                
RPT106   MVI   AVSW,C'T'                                                        
         MVC   0(5,R5),=C'(TOT)'   AND/OR TOTALS                                
         LA    RE,P                                                             
         ST    RE,NXTP                                                          
         BAS   RE,COLS                                                          
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     RPT10                                                            
         SPACE 1                                                                
RPTEND   DS    0H                                                               
         CLOSE (SOUT)                                                           
         L     R1,=A(SOUT)                                                      
         FREEPOOL (R1)                                                          
         CLI   DOWNOPT,C'D'                                                     
         BNE   XIT                                                              
         GOTO1 =A(DOWN),DMCB,5                                                  
         B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO SET RANK NUMBER IN PRINT LINE *                                    
         SPACE 1                                                                
SETRANK  NTR1                                                                   
         ICM   R2,15,ARANK         ARE ADDRESSES ACTIVE                         
         BZ    XIT                                                              
         ICM   R5,15,MYAPRANK                                                   
         LTR   R5,R5                                                            
         BZ    XIT                                                              
         L     R1,MYRANK           YES SO ADD 1 TO RANK                         
         LA    R1,1(R1)                                                         
         ST    R1,MYRANK                                                        
         CLC   0(4,R2),MYVALUE     IS THERE A CHANGE IN VALUE                   
         BE    SETR2                                                            
         MVC   MYTIE,MYRANK        YES SO RESET TIE                             
         MVC   MYVALUE,0(R2)       AND SAVE NEW VALUE                           
         B     SETR4                                                            
         SPACE 1                                                                
SETR2    CLI   DOWNOPT,C'D'        BYPASS FOR DOWNLOAD                          
         BE    XIT                                                              
         MVI   3(R5),C'='          SHOW EQUAL SIGN FOR TIES                     
         SPACE 1                                                                
SETR4    CLI   DOWNOPT,C'D'        BYPASS FOR DOWNLOAD                          
         BE    XIT                                                              
         EDIT  (4,MYTIE),(3,0(R5)) AND EDIT RANK NUM                            
         B     XIT                                                              
         EJECT                                                                  
* FORMAT THE COLUMNS *                                                          
         SPACE 1                                                                
COLS     NTR1                                                                   
         LA    R2,AVDEMOS                                                       
         MVI   MINMAX,1                                                         
         MVI   AVCOUNT,0                                                        
         LA    R3,SORTDEMS                                                      
         L     R4,NXTP                                                          
         A     R4,DEMDISP                                                       
         ZIC   R5,NDEMOS                                                        
         MVC   SPACING,SPACOPT     USER SELECTED SPACING (1-3)                  
         CLI   STACKOPT,C'Y'       STACK OPTION                                 
         BNE   COLS2                                                            
         CLI   SPACING,2                                                        
         BH    *+8                                                              
         MVI   SPACING,2           AT LEAST DOUBLE SPACE                        
         LA    R1,1(R4)                                                         
         MVC   0(5,R1),=C'RTGS.'                                                
         LA    R1,132(R1)                                                       
         MVC   0(5,R1),=C'EXTS.'                                                
         CLI   DBSELMED,C'C'                                                    
         BNE   *+8                                                              
         LA    R1,132(R1)                                                       
         MVC   0(5,R1),=C'IMPS.'                                                
         LA    R1,132(R1)                                                       
         CLI   CPPOPT,C'Y'                                                      
         BNE   *+10                                                             
         MVC   0(3,R1),=C'CPP'                                                  
         LA    R4,6(R4)                                                         
         SPACE 1                                                                
COLS2    CLI   AVOPT,C'Y'          SHOW DATA FOR AVAIL FORMAT                   
         BNE   COLS4                                                            
         CLI   SPACING,2                                                        
         BH    *+8                                                              
         MVI   SPACING,2           DOUBLE SPACE AT LEAST FOR AVAILS             
         MVC   001(5,R4),=C'DEMOS'                                              
         MVC   133(5,R4),=C'SHARE'                                              
         MVC   265(5,R4),=C'H/P/T'                                              
         CLI   CPPOPT,C'Y'                                                      
         BNE   *+10                                                             
         MVC   397(5,R4),=C'CPP/M'                                              
         LA    R4,6(R4)                                                         
         B     COLS6                                                            
         SPACE 1                                                                
COLS4    CLI   CPPOPT,C'Y'                                                      
         BNE   COLS6                                                            
         CLI   SPACING,2                                                        
         BH    *+8                                                              
         MVI   SPACING,2           DOUBLE SPACE AT LEAST FOR CPP/M              
         MVC   001(5,R4),=C'DEMOS'                                              
         MVC   133(5,R4),=C'CPP/M'                                              
         LA    R4,6(R4)                                                         
         SPACE 1                                                                
COLS6    OC    0(4,R3),0(R3)                                                    
         BNZ   COLS8                                                            
         CLC   MINMAX,MINCOL                                                    
         BNE   *+10                                                             
         XC    MINTWO,MINTWO                                                    
         CLC   MINMAX,MAXCOL                                                    
         BNE   *+10                                                             
         XC    MAXTWO,MAXTWO                                                    
         SPACE 1                                                                
COLS8    L     R1,0(R3)                                                         
         SR    R0,R0                                                            
         LM    RE,RF,SORTCNT       COUNT, WEIGHT TO RE,RF                       
         CLC   DBFILE,=C'TP '                                                   
         BNE   *+8                                                              
         SRL   RE,1                DIVIDE COUNT BY 2 FOR TP                     
         CLI   AVSW,C'T'           MULTIPLY TOTALS BY COUNT                     
         BNE   *+6                                                              
         MR    R0,RE                                                            
         LTR   RF,RF               UNWEIGHT                                     
         BZ    COLS16                                                           
         TM    0(R2),X'20'         CPP/CPM DON'T NEED UNWEIGHTING               
         BNO   COLS14                                                           
         CLI   2(R2),X'FF'                                                      
         BNE   COLS16                                                           
         SPACE 1                                                                
COLS14   SLDA  R0,1                                                             
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         SPACE 1                                                                
COLS16   L     R0,0(R3)                                                         
         ST    R1,0(R3)                                                         
         BAS   RE,EDTDEM                                                        
         ST    R0,0(R3)                                                         
* PRINT FLAG TO LEFT OF DEMO IF OVERRIDDEN *                                    
         OC    SORTDMOV,SORTDMOV   TEST ANY OVERRIDES                           
         BZ    COLS18                                                           
         ZIC   R1,NDEMOS           FIND THE INDICATOR BYTE                      
         SR    R1,R5                                                            
         LA    R1,SORTDMOV(R1)                                                  
         CLI   0(R1),0                                                          
         BE    COLS18                                                           
         LA    R1,7(R4)            POINT TO END OF EDITED DEMO                  
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R1,*-8                                                           
         MVI   0(R1),C'*'          SET OVRD FLAG                                
         SPACE 1                                                                
COLS18   LA    R2,3(R2)                                                         
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         CLI   MULTIOPT,C'Y'                                                    
         BNE   COLS19                                                           
         LA    R4,5(R4)                                                         
COLS19   AI    MINMAX,1                                                         
         AI    AVCOUNT,1           IF AVAIL ACTIVE                              
         CLC   AVCOUNT,ACTNDEM     AND WE'VE DONE ENOUGH COLUMNS                
         BNE   COLS20                                                           
         MVI   AVCOUNT,0                                                        
         ZIC   R1,ACTNDEM                                                       
         ZIC   R0,WDEMS                                                         
         MR    R0,R0                                                            
         SR    R4,R1                                                            
         LA    R4,132(R4)          GET DOWN TO NEXT LINE                        
         SPACE 1                                                                
COLS20   BCT   R5,COLS6                                                         
         B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO EDIT A DEMO VALUE                                                  
* ON ENTRY R2=A(3 BYTE DEMO EXPRESSION)                                         
*          R3=A(4 BYTE DEMO VALUE)                                              
*          R4=A(8 BYTE OUTPUT DEMO VALUE)                                       
*                                                                               
EDTDEM   NTR1                                                                   
         CLI   MULTIOPT,C'Y'       TEST MULTI-RANK                              
         BNE   EDTDEM1                                                          
         MVC   9(4,R4),4(R3)       MOVE EBCDIC RANK NUMBER                      
*                                                                               
EDTDEM1  L     R0,0(R3)                                                         
         MVI   MULT,1                                                           
         TM    0(R2),X'20'         EDIT COST/CPP/CPM                            
         BNO   EDTDEM2                                                          
         LTR   R0,R0                                                            
         BZ    EDTDEMX                                                          
         EDIT  (R0),(8,(R4)),2,FLOAT=$                                          
         CLI   0(R4),C' '                                                       
         BE    EDTDEMX                                                          
         LR    RF,R0               JUST SHOW $ IF TOO BIG                       
         LA    RF,50(RF)                                                        
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         EDIT  (RF),(8,(R4)),FLOAT=$                                            
         B     EDTDEMX                                                          
         SPACE 1                                                                
EDTDEM2  MVC   DUB(1),DBFILE                                                    
         MVC   DUB+1(1),1(R2)                                                   
         MVI   DUB+2,0                                                          
         LA    R1,EDITTABT         R1=A(EDIT TABLE)                             
         CLI   DBSELMED,C'T'                                                    
         BE    EDTDEM4                                                          
         CLI   DBSELMED,C'C'                                                    
         BE    EDTDEM4                                                          
         LA    R1,EDITTABR                                                      
*                                  SEARCH TABLE FOR DEMO                        
EDTDEM4  CLI   0(R1),X'FF'         TEST EOL                                     
         BE    EDTDEM6                                                          
         CLC   0(2,R1),DUB         MATCH FILE/DEMO MODIFIER                     
         BE    *+12                                                             
         LA    R1,L'EDITTABT(R1)                                                
         B     EDTDEM4                                                          
         MVC   DUB+2(1),2(R1)      EXTRACT EDIT VALUES                          
*                                                                               
EDTDEM6  TM    DUB+2,X'80'         TEST DEMO NEEDS SCALING                      
         BZ    *+8                                                              
         MH    R0,=H'10'                                                        
         TM    DUB+2,X'02'         TEST EDIT TO 2 DECIMALS                      
         BO    EDTDEM8                                                          
         TM    DUB+2,X'01'         TEST EDIT TO 1 DECIMAL                       
         BO    EDTDEM10                                                         
         EDIT  (R0),(8,0(R4))                                                   
         MVI   MULT,100                                                         
         B     EDTDEMX                                                          
         SPACE 1                                                                
EDTDEM8  EDIT  (R0),(8,0(R4)),2,ZERO=BLANK                                      
         B     EDTDEMX                                                          
         SPACE 1                                                                
EDTDEM10 EDIT  (R0),(8,0(R4)),1,ZERO=BLANK                                      
         MVI   MULT,10                                                          
         B     EDTDEMX                                                          
         SPACE 1                                                                
EDTDEMX  ZIC   R1,MULT                                                          
         MR    R0,R0                                                            
         CLC   MINMAX,MINCOL       FROM SPECIFIED MINIMUN COL NO.               
         BNE   *+8                                                              
         ST    R1,MINTWO           PASS BACK COLUMN 1 TO 2 DEC PLACES           
         CLC   MINMAX,MAXCOL       AND THE SAME FOR MAX                         
         BNE   *+8                                                              
         ST    R1,MAXTWO                                                        
         B     XIT                                                              
         SPACE 1                                                                
* TABLE FOR DEMO EDITING ROUTINE *                                              
         SPACE 1                                                                
EDITTABT DS    0XL3                FILE/DEMO MODIFIER/EDIT RULES                
         SPACE 1                                                                
         DC    C'TI',X'01'         TIME PERIOD                                  
         DC    C'TT',X'01'                                                      
         DC    C'TR',X'01'                                                      
         DC    C'TP',X'01'                                                      
         DC    C'TS',X'01'                                                      
         DC    C'TQ',X'01'                                                      
         DC    C'TX',X'01'                                                      
         DC    C'TE',X'01'                                                      
         DC    C'PI',X'01'         PAV                                          
         DC    C'PT',X'01'                                                      
         DC    C'PR',X'01'                                                      
         DC    C'PP',X'81'                                                      
         DC    C'PS',X'01'                                                      
         DC    C'PQ',X'01'                                                      
         DC    C'PX',X'01'                                                      
         DC    X'FF',X'00'                                                      
         SPACE 1                                                                
EDITTABR DS    0XL3                FILE/DEMO MODIFIER/EDIT RULES                
         SPACE 1                                                                
         DC    C'TI',X'00'         TIME PERIOD                                  
         DC    C'TT',X'00'                                                      
         DC    C'TR',X'01'                                                      
         DC    C'TP',X'01'                                                      
         DC    C'TS',X'01'                                                      
         DC    C'TQ',X'00'                                                      
         DC    C'TX',X'01'                                                      
         DC    C'TE',X'00'                                                      
         DC    X'FF',X'00'                                                      
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
* HEADLINE ROUTINES *                                                           
         SPACE 1                                                                
HOOK     NTR1                                                                   
         OC    P,HEADP             SHOW FULL DETAILS                            
*                                                                               
         MVC   H1+41(39),=C'S P O T   R E S E A R C H   R E P O R T'            
         CLC   USERTTL,SPACES                                                   
         BNH   *+10                                                             
         MVC   H1+41(50),USERTTL                                                
         GOTO1 CENTER,DMCB,H1+41,50                                             
         GOTO1 UNDERLIN,DMCB,(50,H1+41),H2+41                                   
*                                                                               
         CLI   SIDOPT,C'Y'                                                      
         BNE   HOOK2                                                            
         MVC   H3+50(32),=C'PERIOD FROM JAN01/86 TO DEC31/86'                   
         GOTO1 DATCON,DMCB,(3,SVSIDPER),(5,H3+62)                               
         GOTO1 DATCON,DMCB,(3,SVSIDPER+3),(5,H3+74)                             
*                                                                               
HOOK2    LA    R1,=CL10'SPOT T.V.'                                              
         CLI   DBSELMED,C'T'                                                    
         BE    HOOK4                                                            
         CLI   DBSELMED,C'C'                                                    
         BE    HOOK4                                                            
         LA    R1,=CL10'SPOT RADIO'                                             
         CLI   DBSELMED,C'R'                                                    
         BE    HOOK4                                                            
         LA    R1,=C'**********'                                                
HOOK4    MVC   H1+10(10),0(R1)                                                  
         SPACE 1                                                                
         L     RF,ATWA                                                          
         USING T20FFFD,RF                                                       
         SPACE 1                                                                
         MVC   H4+10(8),RESSRCE         SOURCE                                  
         MVC   H4(6),=C'SOURCE'                                                 
         MVC   H5+10(40),RESBOOK        BOOK                                    
         DROP  RF                                                               
*                                                                               
         MVC   H5(4),=C'BOOK'                                                   
         OC    BOOKS+4(4),BOOKS+4  TEST MULTIPLE BOOKS                          
         BZ    *+8                                                              
         MVI   H5+4,C'S'                                                        
*                                                                               
         LA    R2,H6               STATION/MARKET                               
         CLI   NSTATS,1            ONLY IF 1 SPECIFIED                          
         BNE   HOOK8                                                            
         CLI   MGROPT,C'Y'         TEST MKTGRPS                                 
         BE    HOOK8               YES - MKT WILL PRINT LATER                   
         CLI   STATS,0             TEST STATION PRESENT                         
         BNE   HOOK6               YES - OBVIOUSLY NOT MARKET                   
* PRINT MARKET NAME                                                             
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),DBSELMED                                                
         CLI   DBSELMED,C'C'                                                    
         BNE   *+8                                                              
         MVI   DBSELMED,C'T'                                                    
         SR    R0,R0                                                            
         ICM   R0,3,STATS+5                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB                                                     
         MVC   KEY+6(2),AGENCY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO1                     
         L     R6,AIO1                                                          
         CLC   KEY(8),0(R6)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(6,R2),=C'MARKET'                                               
         MVC   10(4,R2),KEY+2                                                   
         MVC   15(24,R2),18(R6)                                                 
         B     HOOK7                                                            
*                                                                               
HOOK6    CLI   SIDOPT,C'Y'                                                      
         BE    HOOK8                                                            
         SPACE 1                                                                
         MVC   0(7,R2),=C'STATION'                                              
         MVC   10(10,R2),SORTSTAT                                               
*                                                                               
HOOK7    LA    R2,132(R2)                                                       
         SPACE 1                                                                
HOOK8    CLI   DAYTMLST,0          DAY IF ONE SPECIFIED                         
         BE    HOOK9                                                            
         CLI   DAYTMLST,X'FF'                                                   
         BE    HOOK9                                                            
         CLI   NDAYTMS,1                                                        
         BH    HOOK9                                                            
         MVC   0(3,R2),=C'DAY'                                                  
*                                                                               
         L     RF,ATWA                                                          
         USING T20FFFD,RF                                                       
         MVC   10(8,R2),RESDAY                                                  
         DROP  RF                                                               
         LA    R2,132(R2)                                                       
         SPACE 1                                                                
HOOK9    CLI   MGROPT,C'Y'         TEST MKTGRPS                                 
         BNE   HOOK10                                                           
         LA    R2,H5                                                            
         MVC   45(43,R2),MGR1BK                                                 
         LA    R2,132(R2)                                                       
         CLC   MGR1LEN,MGR2LEN     TEST 2 LEVELS                                
         BE    HOOK9X              NO                                           
         MVC   45(43,R2),MGR2BK                                                 
         LA    R2,132(R2)                                                       
         CLC   MGR2LEN,MGR3LEN     TEST 3 LEVELS                                
         BE    HOOK9X              NO                                           
         MVC   45(43,R2),MGR3BK                                                 
         LA    R2,132(R2)                                                       
HOOK9X   DS    0H                                                               
         MVC   45(6,R2),=C'MARKET'                                              
         SR    R0,R0               MARKET NUMBER                                
         ICM   R0,3,SORTMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  58(4,R2),DUB                                                     
* LOOK UP MARKET NAME                                                           
         GOTO1 =V(BINSRCH),MTABPARS,SORTMKT                                     
         CLI   0(R1),1                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RE,0(R1)                                                         
         MVC   64(24,R2),4(RE)                                                  
         SPACE 1                                                                
* HEADINGS *                                                                    
         SPACE 1                                                                
HOOK10   DS    0H                                                               
         L     R5,ABOX                                                          
         USING BOXD,R5                                                          
         MVC   BOXCOLS,SPACES           INITIALIZE BOXES                        
         MVC   BOXROWS,SPACES                                                   
         LA    R4,BOXCOLS-1                                                     
         A     R4,DISP                                                          
         MVI   0(R4),C'L'                                                       
         MVI   BOXWT,1                                                          
         MVC   BOXYORN,BOXOPT                                                   
         MVI   BOXOFF,0                                                         
         MVI   BOXROWS+8,C'T'                                                   
         MVI   BOXROWS+11,C'M'                                                  
         ZIC   R1,MAXLINES                                                      
         LA    R1,BOXROWS(R1)                                                   
         MVI   0(R1),C'B'                                                       
         MVI   BOXINIT,0                                                        
*                                                                               
         LA    R2,DETS                                                          
         LA    R3,H10                                                           
         A     R3,DISP                                                          
         ZIC   R0,NDETS                                                         
*                                                                               
HOOK12   CLI   EBOPT,C'Y'                                                       
         BNE   *+12                                                             
         C     R2,AEBDTL           REACHED FIRST BLOCKED DETAIL                 
         BNL   HOOK16              YES                                          
*                                                                               
         ZIC   R5,1(R2)            PICK UP DATA NUMBER                          
         BCTR  R5,0                                                             
         MH    R5,=Y(L'DATALIST)                                                
         A     R5,ADTALIST                                                      
         CLI   12(R5),0                                                         
         BE    HOOK14                                                           
         MVC   0(8,R3),0(R5)       OUTPUT COLUMN HEADER                         
         CLI   BOXOPT,C'Y'                                                      
         BE    HOOK14                                                           
         GOTO1 UNDERLIN,DMCB,(8,(R3)),132(R3),8                                 
         SPACE 1                                                                
HOOK14   ZIC   R1,12(R5)           OUTPUT LENGTH                                
         LTR   R1,R1                                                            
         BZ    *+12                                                             
         LA    R3,1(R1,R3)         +1                                           
         LA    R4,1(R1,R4)                                                      
         CLI   12(R5),0            TEST 0 OUTPUT LENGTH                         
         BE    *+8                                                              
         MVI   0(R4),C'C'                                                       
*                                                                               
         LA    R2,4(R2)                                                         
         BCT   R0,HOOK12                                                        
         B     HOOK20                                                           
*                                                                               
HOOK16   ST    R3,FULL             SAVE START ADDRESS                           
         LA    R3,BLOCK            POINT TO DUMMY AREA                          
         MVC   0(132,R3),SPACES                                                 
*                                                                               
HOOK17   ZIC   R5,1(R2)            PICK UP DATA NUMBER                          
         BCTR  R5,0                                                             
         MH    R5,=Y(L'DATALIST)                                                
         A     R5,ADTALIST                                                      
         CLI   12(R5),0                                                         
         BE    HOOK18                                                           
         MVC   0(4,R3),0(R5)       MOVE ABBREVIATED TITLE                       
         CLI   0(R3),C' '                                                       
         BNE   *+10                                                             
         MVC   0(4,R3),1(R5)       IF CHAR 1 = C' ', USE CHARS 2-5              
*                                                                               
         LA    R3,3(R3)            POINT TO LAST CHAR MOVED                     
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C'-'                                                       
         LA    R3,2(R3)                                                         
*                                                                               
HOOK18   LA    R2,4(R2)            NEXT DETAIL                                  
         C     R2,AEBDTLX          TEST FOR LAST BLOCKED DETAIL                 
         BNL   *+8                                                              
         BCT   R0,HOOK17                                                        
*                                                                               
         BCTR  R3,0                                                             
         MVI   0(R3),C' '                                                       
         LA    R0,BLOCK                                                         
         SR    R0,R3                  GET OUTPUT WIDTH                          
         LPR   R0,R0                                                            
         L     R3,FULL                GET SAVED OUTPUT ADDRESS                  
         OC    BLOCK(30),SPACES                                                 
         GOTO1 CHOPPER,DMCB,((R0),BLOCK),(30,(R3)),(C'P',2)                     
         CLC   BLOCK+30(30),SPACES    TEST USED MORE THAN ONE LINE              
         BNE   HOOK19                 YES                                       
         CLI   BOXOPT,C'Y'                                                      
         BE    HOOK19                                                           
         GOTO1 UNDERLIN,DMCB,(30,(R3)),132(R3)                                  
HOOK19   LA    R3,31(R3)                                                        
         LA    R4,31(R4)                                                        
         SPACE 1                                                                
HOOK20   LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'C'                                                       
         CLI   AVOPT,C'Y'          AVAIL OPTION                                 
         BE    HOOK22                                                           
         CLI   STACKOPT,C'Y'       OR STACK OPTION                              
         BE    HOOK22                                                           
         CLI   CPPOPT,C'Y'         OR CPP OPTION                                
         BNE   HOOK24                                                           
         SPACE 1                                                                
HOOK22   MVC   0(4,R3),=C'DATA'    NEEDS ROOM TO DESCRIBE DATA                  
         LA    R3,6(R3)                                                         
         LA    R4,6(R4)                                                         
         MVI   0(R4),C'C'                                                       
         EJECT                                                                  
HOOK24   LA    R2,DEMOS                                                         
         ZIC   R0,ACTNDEM                                                       
         SPACE 1                                                                
HOOK30   MVC   DUB(3),0(R2)                                                     
         CLC   DUB(3),=X'20C9FF'   SPECIAL CATEGORY FOR COST                    
         BNE   HOOK32                                                           
         MVC   3(4,R3),=C'COST'                                                 
         MVI   133(R3),0                                                        
         B     HOOK40                                                           
         SPACE 1                                                                
HOOK32   CLI   DUB+1,C'T'          CHANGE T TO I                                
         BNE   HOOK34                                                           
         MVI   DUB+1,C'I'                                                       
         SPACE 1                                                                
HOOK34   DS    0H                                                               
         GOTO1 DEMOCON,DMCB,(0,DUB),(7,WORK),DBLOCK                             
         GOTO1 CENTER,DMCB,WORK,7                                               
*                                                                               
         CLI   MULTIOPT,C'Y'                                                    
         BNE   HOOK36                                                           
         MVC   2(7,R3),WORK                                                     
         MVC   140(3,R3),=C'RNK'                                                
         B     HOOK38                                                           
*                                                                               
HOOK36   MVC   0(7,R3),WORK                                                     
*                                                                               
HOOK38   MVI   133(R3),0                                                        
         CLI   STACKOPT,C'Y'                                                    
         BE    HOOK40                                                           
         MVC   133(5,R3),WORK+7                                                 
         CLC   133(5,R3),=C'*****'                                              
         BNE   *+10                                                             
         MVC   133(5,R3),=C'(000)'                                              
         TM    DUB,X'20'           SHOW CPP/CPM                                 
         BNO   HOOK40                                                           
         MVC   134(3,R3),=C'CPP'                                                
         CLI   DUB+1,C'R'                                                       
         BE    HOOK40                                                           
         MVC   134(3,R3),=C'CPM'                                                
         SPACE 1                                                                
HOOK40   LA    R2,3(R2)                                                         
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         CLI   MULTIOPT,C'Y'                                                    
         BNE   HOOK42                                                           
         LA    R3,5(R3)                                                         
         LA    R4,5(R4)                                                         
HOOK42   MVI   0(R4),C'C'                                                       
         BCT   R0,HOOK30                                                        
         MVI   0(R4),C'R'                                                       
         SPACE 1                                                                
         XIT1                                                                   
         SPACE 1                                                                
         GETEL (R5),DATADISP,ELCODE                                             
         EJECT                                                                  
*              SEND A FORMATTED LINE TO DOWNLOAD                                
*                                                                               
DOWNLINE NTR1                                                                   
         LA    R2,DETS                                                          
         LA    R5,P                                                             
         A     R5,DISP                                                          
         ZIC   R0,NDETS                                                         
DNLN30   ZIC   R3,1(R2)            INDEX INTO DATALIST                          
         BCTR  R3,0                                                             
         SLL   R3,4                                                             
         L     RE,=A(DATALIST)                                                  
         AR    R3,RE                                                            
         B     DNLN50                                                           
*        OC    AVETOTAD,AVETOTAD   TEST AVE/TOT LINE                            
*        BZ    DNLN50                                                           
*        C     R5,AVETOTAD         TEST AVE/TOT ADDRESS                         
*        BH    DNLN40                (BLANK TO THE RIGHT)                       
*        BE    DNLN50                (EMIT '(AVE)' OR '(TOT)')                  
*        ZIC   RE,12(R3)           FIELD LENGTH (OUTPUT)                        
*        LA    R1,1(R5,RE)                                                      
*        C     R1,AVETOTAD         TEST AVE/TOT ADDRESS                         
*        BE    DNLN50                (EMIT FIELD TO LEFT OF AVE/TOT)            
*NLN40   ZIC   R1,12(R3)                                                        
*        BCTR  R1,0                                                             
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        MVC   0(0,R5),SPACES      BLANK OUT OTHER FIELDS                       
*                                                                               
DNLN50   CLI   12(R3),0            NO OUTPUT LEN                                
         BE    DNLN51                                                           
         GOTO1 =A(DOWN),DMCB,1     1=PRINT TEXT FIELD                           
*                                                                               
         ZIC   RE,12(R3)           FIELD LENGTH (OUTPUT)                        
         LA    R5,1(R5,RE)                                                      
DNLN51   LA    R2,4(R2)                                                         
         BCT   R0,DNLN30           NEXT DETAIL                                  
*                                                                               
*        XC    AVETOTAD,AVETOTAD   SET AVE/TOT ADDRESS TO ZERO                  
*                                                                               
         LA    R5,P                                                             
         A     R5,DEMDISP                                                       
         ZIC   R0,NDEMOS                                                        
DNLN70   EQU   *                                                                
         GOTO1 =A(DOWN),DMCB,2     2=PRINT NUMERIC FIELD                        
*                                                                               
         LA    R5,8(R5)            9-BYTE COLUMN (HARD CODED)                   
         BCT   R0,DNLN70           NEXT DEMO                                    
*                                                                               
         GOTO1 =A(DOWN),DMCB,3     3=END OF LINE                                
*                                                                               
         MVC   P,SPACES            CLEAR THE PRINT LINE                         
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************                                   
* LOOK UP DEMOS FOR A STATION/BOOK/DAY/TIME *                                   
*********************************************                                   
         SPACE 1                                                                
SPOTDEM  NMOD1 0,SPOTDEM                                                        
         L     RC,0(R1)                                                         
*                                                                               
         CLI   SIDOPT,C'Y'         TEST SID                                     
         BNE   SPD8                NO                                           
*                                                                               
SPD4     BAS   RE,NEWSID                                                        
         B     SPOTDEMX                                                         
         SPACE 1                                                                
SPD8     BAS   RE,GETDEM                                                        
         B     SPOTDEMX                                                         
*                                                                               
SPOTDEMX XIT1                                                                   
         EJECT                                                                  
**********************                                                          
* NEW SID PROCESSING *                                                          
**********************                                                          
         SPACE 1                                                                
NEWSID   NTR1                                                                   
         L     R1,ASIDBUFF                                                      
         LA    R0,(SIDBUFFX-SIDBUFF)/256                                        
         XC    0(256,R1),0(R1)                                                  
         LA    R1,256(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         L     R4,ASIDBUFF         INITIALIZE BLOCK                             
         USING SRBLKD,R4                                                        
*                                                                               
         MVC   SRASIR,ASIDREC                                                   
         MVC   SRACOM,ACOMFACS                                                  
         MVC   SRACLPAC,CLPACK                                                  
         MVC   SRAMSUNP,MSUNPK                                                  
         MVC   SRADYUNP,UNDAY                                                   
         MVC   SRAUNTIM,UNTIME                                                  
*                                                                               
         MVC   SRSELSCH,NSIDSCHM                                                
         MVC   SRSELAM,BAGYMD      PICK UP SELECTIONS                           
         MVC   SRSELAGY,AGENCY                                                  
         MVC   SRSELMED,DBSELMED                                                
         CLI   SRSELMED,C'C'                                                    
         BNE   *+8                                                              
         MVI   SRSELMED,C'T'                                                    
         MVC   SRSELPER,NSIDPER                                                 
         MVC   SRSELYR,NSIDYEAR                                                 
*                                                                               
         MVC   SRSELSTA,SIDSTAT    MAY BE USING SID STATION                     
         OC    SIDSTAT,SIDSTAT                                                  
         BNZ   NSID6                                                            
         OC    ACTSTAT,ACTSTAT     SPECIFIC STATION                             
         BZ    NSID4                                                            
         MVC   WORK(4),=4X'F0'                                                  
         MVC   DUB(5),ACTSTAT                                                   
         CLI   DUB+4,C' '                                                       
         BH    *+8                                                              
         MVI   DUB+4,C'T'                                                       
         GOTO1 MSPACK,DMCB,WORK,DUB,WORK+4                                      
         MVC   SRSELSTA,WORK+6                                                  
         B     NSID6                                                            
         SPACE 1                                                                
NSID4    MVC   SRSELMKT,ACTMKT     SET REQUESTED MARKET                         
         CLI   MGROPT,C'Y'                                                      
         BNE   *+10                                                             
         MVC   SRSELMKT,SIDMKT     OR NEXT MKT IN MKTGRP                        
         SPACE 1                                                                
NSID6    MVC   SRSELDPT,DPLIST                                                  
         MVC   SRSELPRG,PTLIST                                                  
         MVC   SRSELSLN,SLNOPT                                                  
         MVC   SRSELTIM,SORTDP+1                                                
*                                                                               
         CLI   SORTDP,X'FF'        TEST DAY=ALL                                 
         BE    NSID8                                                            
         ZIC   R1,SORTDP                                                        
         LA    R1,SPOTDAYS(R1)     CONVERT TO SPOT DAY                          
         MVC   SRSELDAY,0(R1)                                                   
         SPACE 1                                                                
NSID8    DS    0H                                                               
         GOTO1 RANSID,DMCB,(R4)                                                 
*                                                                               
         CLI   TRACEOPT,C'Y'                                                    
         BNE   NSID8A                                                           
         L     RF,ATWA                                                          
         USING T20FFFD,RF                                                       
         L     RF,TWADCONS                                                      
         USING TWADCOND,RF                                                      
         L     RF,TPRNTBL                                                       
         DROP  RF                                                               
         GOTO1 (RF),DMCB,(6,=C'RANSID'),(R4),C'DUMP',1024,=C'1D'                
*                                                                               
NSID8A   CLI   SRMODE,SRONEREC                                                  
         BNE   SPOTDEMX                                                         
         MVC   SVSIDPER,SRSTART    SAVE START/END DATES                         
         MVC   SORTDPT,SRDPTNM     DAYPART EXPANSION                            
         MVC   SORTTYP,SRPRGNM     PROGRAM TYPE EXPANSION                       
*                                                                               
         MVC   SORTPER,SRACTPER       PERIOD NUMBER (WITH X'80')                
         MVC   SORTPER+1(4),SRSELPER  PERIOD DESCRIPTION                        
*                                                                               
         MVC   SORTUPDT,SPACES                                                  
         CLI   ANYUP,C'Y'          UPGRADE CONTROL                              
         BNE   NSID24                                                           
*                                                                               
         MVC   SORTPROG,SRACTPRO   USER CAN SPECIFY PROGRAM(S)                  
         CLI   SORTPROG,C' '                                                    
         BH    *+10                                                             
         MVC   SORTPROG,=CL16'VARIOUS'                                          
*                                                                               
         MVC   BLOCK(32),SPACES                                                 
         MVC   BLOCK(16),SRUPDATA                                               
         CLI   SRUPBOOK,0          IS A FROM BOOK SPECIFIED?                    
         BE    NSID10                                                           
         MVC   BLOCK+17(3),=C'BK='                                              
         ZIC   R1,SRUPBOOK+1       YES - SO SHOW THAT AS WELL                   
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   BLOCK+20(3),0(R1)                                                
         EDIT  (1,SRUPBOOK),(2,BLOCK+23)                                        
         SPACE 1                                                                
NSID10   CLI   OVEROPT,C'N'        OPTION TO SUPPRESS OVERRIDES                 
         BE    NSID22                                                           
         OC    SROVELEM,SROVELEM   ANY DEMO OVERRIDES?                          
         BZ    NSID22                                                           
         MVI   BLOCK+30,C'*'       THEN SHOW AN ASTERISK                        
         SPACE 1                                                                
NSID22   GOTO1 SQUASHER,DMCB,BLOCK,32                                           
         MVC   SORTUPDT,BLOCK                                                   
         SPACE 1                                                                
NSID24   LA    RE,SPOTDAYS         NEED REP-TYPE DAY                            
         SR    RF,RF                                                            
         SPACE 1                                                                
NSID26   CLC   0(1,RE),SRACTDAY                                                 
         BE    NSID28                                                           
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   NSID26                                                           
         LA    RF,9                                                             
         SPACE 1                                                                
NSID28   STC   RF,SORTDP                                                        
         MVC   SORTDP+1(4),SRACTTIM                                             
         MVC   SORTCMT,SRACTCOM                                                 
         MVC   SORTDAY(1),SORTDP                                                
         MVC   SORTTIME,SORTDP+1                                                
         OC    SRERMNM,SPACES                                                   
*        GOTO1 CHOPPER,DMCB,(24,SRERMNM),(16,SORTMKT),1                         
         MVC   SORTMKT(16),SRERMNM                                              
*                                                                               
         MVC   DBSELDAY,SRACTDAY   CONTROL DEMAND                               
         MVC   DBSELTIM,SRACTTIM                                                
         MVC   DBSELSTA,ACTSTAT                                                 
         MVC   DBSELMK,ACTMKT      (SPILL)                                      
         OC    DBSELSTA,DBSELSTA   STATION CAN COME FROM SID RECORD             
         BNZ   NSID34                                                           
*                                                                               
         XC    SORTSTAT,SORTSTAT                                                
         CLI   MGROPT,C'Y'              TEST MKTGRPS                            
         BNE   NSID30                                                           
         MVC   SORTMKT,SRACTMKT         MOVE MARKET NUMBER                      
         SPACE 1                                                                
         MVC   WORK(4),SRACTMKT         ADD NUMBER/NAME TO TABLE                
         MVC   WORK+4(24),SRERMNM                                               
         GOTO1 =V(BINSRCH),MTABPARS,(X'01',WORK)                                
*                                                                               
NSID30   MVC   SORTSTAT(5),SRERSTA                                              
         CLI   SORTSTAT+4,C' '                                                  
         BE    NSID32                                                           
         MVC   SORTSTAT+5(1),SORTSTAT+4                                         
         MVI   SORTSTAT+4,C'-'                                                  
*                                                                               
NSID32   MVC   DBSELSTA,SRERSTA                                                 
         XC    DBSELMK,DBSELMK                                                  
         SPACE 1                                                                
NSID34   LA    R1,SRACTEF1         MAY BE MORE THAN 1 COST                      
         LA    R0,4                                                             
         CLI   EFFOPT,0            OPTION TO SHOW ONLY N'TH EFFECTIVE           
         BE    NSID38                                                           
         ZIC   RE,EFFOPT                                                        
         BCTR  RE,0                                                             
         MH    RE,=H'7'                                                         
         AR    R1,RE                                                            
         LA    R3,1                                                             
         B     NSID38                                                           
         SPACE 1                                                                
NSID36   OC    0(7,R1),0(R1)                                                    
         BZ    NSID8                                                            
*                                                                               
NSID38   MVC   SORTEFF,0(R1)                                                    
         MVC   COST,3(R1)                                                       
*                                                                               
         BAS   RE,GETDEM                                                        
*                                                                               
         LA    R1,7(R1)                                                         
         BCT   R0,NSID36                                                        
         B     NSID8                                                            
         EJECT                                                                  
* GET SPOT DEMOS *                                                              
         SPACE 1                                                                
GETDEM   NTR1                                                                   
         CLI   ANYUP,C'Y'          TEST FOR UPGRADES                            
         BE    GETD10                                                           
         SPACE 1                                                                
         XC    NEXTPROG,NEXTPROG   CLEAR XQH PROGRAM NAME                       
         MVI   XQHNUM,0            AND RESET QH NUMBER                          
         GOTO1 DEMAND,DMCB,DBLOCK,GETDEMHK                                      
         B     SPOTDEMX                                                         
         SPACE 1                                                                
* DEMAND RECORD PROCESSING HOOK *                                               
         SPACE 1                                                                
GETDEMHK NTR1                                                                   
         LA    R4,SORTPROG                                                      
         MVI   DBSELXQH,C'N'                                                    
         CLI   DBSELXQH,C'Y'                                                    
         BNE   GDHK2                                                            
*                                                                               
         LA    R4,NEXTPROG                                                      
         OC    NEXTPROG,NEXTPROG                                                
         BNZ   GDHK1                                                            
         GOTO1 DEFINE,DMCB,=C'PROGRAM',DBLOCK,(R4)                              
GDHK1    MVC   SORTPROG,NEXTPROG                                                
         BAS   RE,SPOTPIN          SAVE PROGRAM NAME FROM LAST TIME             
*                                                                               
GDHK2    DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'PROGRAM',DBLOCK,(R4)                              
*                                                                               
         CLI   DBSELXQH,C'Y'                                                    
         BE    *+8                                                              
         BAS   RE,SPOTPIN                                                       
*                                                                               
         ZIC   RE,XQHNUM                                                        
         LA    RE,1(RE)                                                         
         CLC   DBACTSQC,DBACTEQC     TEST REC COVERS MULT QHS                   
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,XQHNUM                                                        
*                                                                               
         GOTO1 DEFINE,DMCB,=C'DAY',DBLOCK,BLOCK                                 
         MVC   SORTDAY(1),BLOCK+1     REP DAY CODE                              
         MVC   SORTDAY+1(1),BLOCK     SPOT DAY CODE                             
*                                                                               
         GOTO1 (RF),(R1),=C'TIME'                                               
         MVC   SORTTIME,BLOCK+2                                                 
         BAS   RE,FUDGTIME                                                      
*                                                                               
         GOTO1 (RF),(R1),=C'PURE'                                               
         MVC   SORTCODE,BLOCK+3                                                 
*                                                                               
         GOTO1 (RF),(R1),=C'TYPE'                                               
         MVC   SORTFILT,BLOCK                                                   
*                                                                               
         GOTO1 (RF),(R1),=C'WEEK'                                               
         XC    SORTWEEK,SORTWEEK                                                
         MVC   SORTWEEK(4),BLOCK+1                                              
         EJECT                                                                  
         CLI   IUNSORT,C'Y'                                                     
         BNE   GDHK10                                                           
         SPACE 1                                                                
* EXTRACT IUN DEMOS INTO WORK AREA *                                            
         SPACE 1                                                                
         DS    0H                                                               
         LA    R4,SORTDEMS+(IUNOLD-IUNWK)  CLEAR LOOK-UP AREA                   
         LA    R5,4                        RTGS/IMPS/PUTS/TOTS                  
         XC    0(4*IUNDEMS,R4),0(R4)                                            
         LA    R4,4*IUNDEMS(R4)                                                 
         BCT   R5,*-10                                                          
*                                                                               
         LA    R4,SORTDEMS         GET LOOK-UP AREA ADDRESS                     
         GOTO1 =V(SPGETIUN),DMCB,(4,DBLOCK),(R4),EXDEMOS                        
         SPACE 1                                                                
* NOW NEED TO MULTIPLY UNIVERSES BY DBFACTOR FOR CONSISTENCY *                  
         SPACE 1                                                                
         LA    R0,IUNDEMS                                                       
         LA    R4,SORTDEMS                                                      
*                                                                               
GDHK8    L     R1,0(R4)                                                         
         MH    R1,DBFACTOR         ADJUST TO SCALE                              
         ST    R1,0(R4)            AND SET IN SORT REC                          
         LA    R4,4(R4)                                                         
         BCT   R0,GDHK8                                                         
         SPACE 1                                                                
* LOOK UP VUTS (HUTS USED TO DERIVE SHARES) *                                   
         SPACE 1                                                                
         GOTO1 DEMOUT,DMCB,(C'L',VUTLIST),DBLOCK,SORTVUTS                       
         OC    SORTVUTS(4),SORTVUTS      ANY VUTS                               
         BNZ   GDHK8A                                                           
         GOTO1 (RF),(R1),(C'L',PUTLIST)  NO - USE PUTS                          
*                                                                               
GDHK8A   LA    R0,3                                                             
         LA    R4,SORTVUTS                                                      
*                                                                               
GDHK9    SR    RE,RE               WEIGHT AND ROUND THE VUTS                    
         L     RF,0(R4)                                                         
         A     RF,=F'5'                                                         
         D     RE,=F'10'                                                        
         M     RE,=F'10'                                                        
         MH    RF,DBFACTOR                                                      
         ST    RF,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,GDHK9                                                         
         B     GDHK12                                                           
*                                                                               
VUTLIST  DC    X'81',C'V',AL1(1)                                                
         DC    X'81',C'V',AL1(2)                                                
         DC    X'81',C'V',AL1(3)                                                
         DC    X'FF'                                                            
PUTLIST  DC    X'81',C'P',AL1(1)                                                
         DC    X'81',C'P',AL1(2)                                                
         DC    X'81',C'P',AL1(3)                                                
         DC    X'FF'                                                            
         B     SPOTDEMX                                                         
         SPACE 2                                                                
* NOT EXTRACTING IUN VALUES - CALL DEMOUT *                                     
         SPACE 1                                                                
GDHK10   DS    0H                                                               
         GOTO1 DEMOUT,DMCB,(C'L',EXDEMOS),DBLOCK,SORTDEMS                       
         SPACE 1                                                                
* WEIGHT DEMOS BY NUMBER OF QUARTER HOURS *                                     
         SPACE 1                                                                
         LA    R1,SORTDEMS                                                      
         ZIC   R0,NEXDEMS          NUMBER OF EXTRACT DEMOS                      
*                                                                               
GDHK11   L     RE,0(R1)                                                         
         MH    RE,DBFACTOR                                                      
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,GDHK11                                                        
         SPACE 1                                                                
* DEMOS EXTRACTED - BUILD KEY FIELDS AND PUT TO SORT *                          
         SPACE 1                                                                
GDHK12   DS    0H                                                               
         GOTO1 =A(BLDSRT),DMCB,(RC)                                             
         B     SPOTDEMX                                                         
         EJECT                                                                  
* GET UPGRADE DEMOS *                                                           
         SPACE 1                                                                
GETD10   L     R5,AUPBLOCK                                                      
         USING SPDEMUPD,R5         R5=A(UPGRADE BLOCK)                          
         XC    SPDEMUPD(SPDEMUPL),SPDEMUPD                                      
         MVC   SPUPAREC,AIO1                                                    
         MVC   SPUPAFAC,ACOMFACS                                                
         MVC   SPUPAGY,AGENCY                                                   
         MVC   SPUPMED,DBSELMED                                                 
         MVC   SPUPSTA,DBSELSTA                                                 
         MVC   SPUPSPL,DBSELMK     SWEEP MARKET NUMBER                          
         MVC   SPUPDAY,DBSELDAY                                                 
         MVC   SPUPTIM,DBSELTIM                                                 
         MVC   SPUPFIL,UPFILE                                                   
         MVC   SPUPSRC,DBSELSRC                                                 
         MVC   SPUPFBK,BOOKOPT+1                                                
         MVC   SPUPTYPE(8),UPOPT+4                                              
         MVC   SPUPAOVR,SIDOVER                                                 
         MVC   SPUP2YRP,PUTOPT     SET FOR 1/2 YEAR PUTS                        
         MVC   SPUP2YRR,RTGOPT       AND RTGS                                   
*                                                                               
         CLI   SIDUPOPT,C'Y'       OPTION TO TAKE UPGRADE FROM SID              
         BNE   GETD12                                                           
         SPACE 1                                                                
** NEWSID **                                                                    
         SPACE 1                                                                
GETD11   L     R4,ASIDBUFF                                                      
         USING SRBLKD,R4                                                        
         CLI   SRUPFILE,0          BUT ONLY IF SOMETHING THERE                  
         BE    SPOTDEMX                                                         
*                                                                               
         MVC   SPUPFIL,SRUPFILE                                                 
         MVC   SPUPUDAY,SRUPDAY                                                 
         MVC   SPUPUTIM,SRUPTIM                                                 
         MVC   SPUPTYPE(8),SRUPEXP                                              
         OC    SRUPBOOK,SRUPBOOK                                                
         BZ    *+10                                                             
         MVC   SPUPFBK,SRUPBOOK                                                 
         SPACE 1                                                                
GETD12   MVC   UPDEMOS,EXDEMOS                                                  
         GOTO1 SPDEMUP,DMCB,SPDEMUPD,UPDEMOS,SORTDEMS                           
         CLI   0(R1),0             TEST FOR ERRORS                              
         BNE   SPOTDEMX                                                         
*                                                                               
         MVC   SORTDAY(1),SORTDP    FORCE DAY/TIME = SID DAY/TIME               
         MVC   SORTDAY+1(1),DBSELDAY  AND PRESERVE ACTUAL DAY                   
         MVC   SORTTIME,SORTDP+1                                                
*                                                                               
GETD14   DS    0H                                                               
         CLI   SIDUPOPT,C'Y'       TEST UPGRADES FROM SID                       
         BNE   GETD16                                                           
         CLI   OVEROPT,C'N'        TEST SUPPRESS OVRDS                          
         BE    GETD16                                                           
         CLI   ANYUP,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,GETOVRD                                                       
GETD16   DS    0H                                                               
         GOTO1 =A(BLDSRT),DMCB,(RC)  BUILD SORT KEY AND PUT TO SORT             
         B     SPOTDEMX                                                         
*                                                                               
GETOVRD  NTR1                                                                   
         SPACE 1                                                                
GETOVRD4 L     R4,ASIDBUFF          * NEW SID *                                 
         USING SRBLKD,R4                                                        
         OC    SROVELEM,SROVELEM                                                
         BZ    GETOVRDX                                                         
*                                                                               
GETOVRD8 LA    R2,SORTDMOV                                                      
         LA    R1,DEMOS                                                         
         ZIC   R0,NDEMOS                                                        
*                                                                               
         BAS   RE,GETVAL                                                        
         LA    R2,1(R2)                                                         
         LA    R1,3(R1)                                                         
         BCT   R0,*-12                                                          
GETOVRDX XIT1                                                                   
         SPACE 1                                                                
GETVAL   NTR1                                                                   
         L     R6,ASIDREC                                                       
         LA    R6,24(R6)           POINT TO FIRST ELEMENT                       
         CLI   PPROF1,C'O'         TEST OLDSID                                  
         BE    *+8                 YES                                          
         L     R6,SROVELEM                                                      
GETVAL2  CLI   0(R6),0             TEST END OF OVRD ELEMS                       
         BE    GETVALX                                                          
         CLI   0(R6),X'DE'         TEST OVRD ELEM                               
         BE    GETVAL6                                                          
GETVAL4  ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GETVAL2                                                          
*                                                                               
GETVAL6  CLC   2(2,R6),1(R1)       MATCH DEMO MOD/NUM                           
         BNE   GETVAL4                                                          
         MVI   0(R2),1             SET FLAG                                     
* NOW FIND FIELD IN SORTDEMS AND SET OVRD VALUE *                               
         LA    R4,UPDEMOS                                                       
         LA    R5,SORTDEMS                                                      
*                                                                               
GETVAL10 CLI   0(R4),0             MAKE SURE NOT CPP, ETC.                      
         BNE   GETVAL12                                                         
         CLC   1(2,R4),2(R6)       MATCH MOD/NUM                                
         BNE   GETVAL12                                                         
         XC    0(4,R5),0(R5)       CLEAR OLD VALUE                              
         MVC   2(2,R5),4(R6)       SET NEW VALUE                                
         B     GETVALX                                                          
*                                                                               
GETVAL12 LA    R4,3(R4)                                                         
         LA    R5,4(R5)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   GETVAL10                                                         
GETVALX  XIT1                                                                   
         EJECT                                                                  
* MISCELLANEOUS SPOT ROUTINES *                                                 
         SPACE 1                                                                
* ROUTINE TO ADJUST TIME FOR TP 1/2 HOUR *                                      
         SPACE 1                                                                
FUDGTIME NTR1                                                                   
         CLC   DBFILE(2),=C'TP'                                                 
         BNE   SPOTDEMX                                                         
         CLI   QRTOPT,C'Y'         OPTION TO SHOW 1/4 HOUR                      
         BE    SPOTDEMX                                                         
         CLI   DPOPT,C'Y'          DAYPART OPTION                               
         BNE   FUDGET2                                                          
         MVC   SORTDAY(1),SORTDP      USE SELECTED DAY FOR DAY                  
         MVC   SORTDAY+1(1),DBSELDAY  AND PRESERVE ACTUAL DAY                   
         MVC   SORTTIME,SORTDP+1   USE DAYPART FOR TIME                         
         B     SPOTDEMX                                                         
         SPACE 1                                                                
FUDGET2  LH    R1,SORTTIME         ROUND DOWN START TIME                        
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
         STH   R1,SORTTIME                                                      
         SPACE 1                                                                
         LH    R1,SORTTIME+2       ROUND UP END TIME                            
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
         STH   R1,SORTTIME+2                                                    
         B     SPOTDEMX                                                         
         EJECT                                                                  
SPOTPIN  NTR1                                                                   
         CLI   COMPOPT,C'Y'        DO WE HAVE COMPRESS OPTION ON                
         BNE   SPOTDEMX                                                         
         XC    PROGKEY,PROGKEY     YES - BUILD A KEY                            
         MVC   PROGKSTA,DBSELSTA                                                
         MVC   PROGKDAY,DBSELDAY                                                
         MVC   PROGKTIM,DBSELTIM                                                
         MVC   PROGKBK,DBSELBK                                                  
         CLI   DBSELXQH,C'Y'       TEST EXTENDED QH FEATURE                     
         BNE   *+12                                                             
         CLI   XQHNUM,2            TEST SAVE PROGRAM THIS TIME                  
         BL    SPOTPIN6            NO                                           
         SPACE 1                                                                
         L     R2,APRGBUFF                                                      
         SPACE 1                                                                
SPOTPIN2 CLI   0(R2),0             LOOK FOR A NEW ENTRY                         
         BNE   SPOTPIN4                                                         
         SPACE 1                                                                
SPOTPIN3 MVC   0(16,R2),PROGKEY                                                 
         MVC   16(16,R2),SORTPROG                                               
         MVC   SORTPROG,PROGKEY                                                 
         B     SPOTDEMX                                                         
         SPACE 1                                                                
SPOTPIN4 CLC   PROGKEY,0(R2)       LOOK FOR A MATCH                             
         BNE   SPOTPIN8                                                         
         OC    16(16,R2),16(R2)    IS ANYTHING THERE (XQH FEATURE)              
         BZ    SPOTPIN3                                                         
         CLC   SORTPROG,16(R2)     IS PROGRAM THERE ALREADY                     
         BE    SPOTPIN6                                                         
         MVI   24(R2),C'/'         NO SO SHOW START/END                         
         MVC   25(7,R2),SORTPROG                                                
         SPACE 1                                                                
SPOTPIN6 MVC   SORTPROG,PROGKEY                                                 
         B     SPOTDEMX                                                         
         SPACE 1                                                                
SPOTPIN8 LA    R2,32(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BE    SPOTDEMX                                                         
         B     SPOTPIN2                                                         
         LTORG                                                                  
         EJECT                                                                  
* BUILD SORT RECORD KEYS AND PUT RECORDS TO SORT *                              
         SPACE 1                                                                
BLDSRT   NMOD1 0,BLDSRT                                                         
         L     RC,0(R1)                                                         
*                                                                               
         MVC   SORTCNT,=F'1'       SET COUNT = 1                                
         MVC   SORTWT,=F'1'        SET WEIGHT = 1                               
         CLI   ANYUP,C'Y'                                                       
         BE    *+12                                                             
         LH    R0,DBFACTOR                                                      
         ST    R0,SORTWT           SET WEIGHT = DBFACTOR  (QHS)                 
*                                                                               
         L     R1,COST             CARRY WEIGHTED COST                          
         M     R0,SORTWT                                                        
         ST    R1,SORTWCST                                                      
*                                                                               
         TM    SORTRQBK+1,X'20'    TEST ESTIMATED BOOK                          
         BZ    BLDSRT2A                                                         
         CLI   EBOPT,C'Y'                                                       
         BNE   BLDSRT2A                                                         
         CLC   SORTPROG,=CL16'VARIOUS'                                          
         BNE   BLDSRT2B                                                         
         MVC   SORTPROG,SPUPPRG                                                 
         B     BLDSRT2B                                                         
*                                                                               
BLDSRT2A CLI   ANYUP,C'Y'          CANT SEE PROGRAM DETAILS IN UPGRADE          
         BNE   BLDSRT2B                                                         
         CLI   SIDOPT,C'Y'         UNLESS USING SID PROGRAMS                    
         BE    BLDSRT2B                                                         
         MVC   SORTPROG,=CL16'VARIOUS'                                          
         SPACE 1                                                                
BLDSRT2B DS    0H                                                               
         BAS   RE,FIGLEN           WORK OUT LENGTH IN 1/4 HOURS                 
         XC    SORTDATE,SORTDATE                                                
         MVC   SORTMNTH,SORTBOOK   FOR SPOT, BOOK=MONTH                         
*                                                                               
         TM    SORTRQBK+1,X'20'    TEST ESTIMATED BOOK                          
         BZ    BLDSRT2C                                                         
         CLI   EBOPT,C'Y'                                                       
         BNE   BLDSRT2C                                                         
         MVC   SORTBOOK+2(1),SPUPFIL   SET FILE (P/T)                           
         MVC   SORTBOOK+3(8),SPUPTYPE  SET UPGRADE VALUE                        
         MVC   SORTBOOK+11(2),SPUPFBK   AND SHARE BOOK                          
*                                                                               
BLDSRT2C MVC   SORTQURT,SORTMNTH                                                
         ZIC   R1,SORTQURT+1                                                    
         BCTR  R1,0                                                             
         SR    R0,R0                                                            
         D     R0,=F'3'                                                         
         STC   R1,SORTQURT+1                                                    
         STC   R1,SORTQ2                                                        
         MVC   SORTQ2+1(1),SORTQURT                                             
         MVC   SORTYEAR,SORTQURT                                                
         LH    R1,SORTTIME         ENSURE EARLY AM AFTER MIDNIGHT               
         CH    R1,=H'600'                                                       
         BNL   *+8                                                              
         LA    R1,2400(R1)                                                      
         STH   R1,SORTTIME                                                      
         SPACE 1                                                                
         LA    R3,SORTKEY                                                       
         LA    R2,DETS                                                          
         ZIC   R0,NDETS                                                         
         SPACE 1                                                                
BLDSRT4  ZIC   R1,1(R2)            DATA NUMBER                                  
         BCTR  R1,0                                                             
         MH    R1,=Y(L'DATALIST)                                                
         A     R1,ADTALIST                                                      
         L     R4,8(R1)            ADDRESS                                      
         ZIC   R5,8(R1)            LENGTH                                       
         SR    R5,R5                                                            
         ICM   R5,1,8(R1)                                                       
         BZ    BLDSRT4X                                                         
         XC    0(16,R3),0(R3)                                                   
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)                                                    
*                                                                               
BLDSRT4X LA    R2,4(R2)                                                         
         LA    R3,16(R3)                                                        
         BCT   R0,BLDSRT4                                                       
*                                                                               
         MVI   SORTKID,C'X'        INDICATE NON-IUN DATA                        
         CLI   IUNSORT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   SORTKID,C'I'        INDICATE IUN DATA                            
         CLI   ANYUP,C'Y'                                                       
         BNE   *+8                                                              
         MVI   SORTKID,C'U'        INDICATE UPGRADE DATA                        
         MVI   SORTKID+1,0                                                      
         MVC   SORTKMKT,DBACTRMK   SET MARKET NUMBER                            
*                                                                               
         CLI   ANYSORT,C'Y'        TEST PREVIOUS SORT DATA                      
         BE    BLDSRT5             YES                                          
         MVI   ANYSORT,C'Y'        SET FLAG FOR SORT DATA                       
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
*                                                                               
BLDSRT5  DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTKEY                                      
*                                                                               
         XC    SORTDMOV,SORTDMOV    CLEAR OVERRIDE FLAGS                        
*                                                                               
         BC    15,BLDSRT6           *** USE FOR NOP ***                         
         CLI   TRACEOPT,C'Y'                                                    
         BNE   BLDSRT6                                                          
         L     RF,ATWA                                                          
         USING T20FFFD,RF                                                       
         L     RF,TWADCONS                                                      
         USING TWADCOND,RF                                                      
         L     RF,TPRNTBL                                                       
         DROP  RF                                                               
* SET TRACE LEN = 804 TO PRINT IUN RECORDS, 160 FOR KEY ONLY                    
         GOTO1 (RF),DMCB,(3,=C'OUT'),SORTKEY,C'DUMP',804,=C'1D'                 
         SPACE 1                                                                
BLDSRT6  DS    0H                                                               
         ZIC   R2,NDETS                                                         
         SH    R2,=H'2'            BACK UP TO NEXT TO LAST DETAIL               
         SLL   R2,2                BECAUSE NO TOTALS FOR LAST DETAIL            
         LA    R2,DETS(R2)                                                      
*                                                                               
         ZIC   R3,NDETS                                                         
         BCTR  R3,0                                                             
         SLL   R3,4                X 16                                         
         LA    R3,SORTKEY(R3)      NOTE FF'S GO IN LAST ONE                     
*                                                                               
         ZIC   R0,NDETS            SET COUNTER                                  
         BCTR  R0,0                                                             
         LTR   R0,R0                                                            
         BZ    BLDSRT12                                                         
         SPACE 1                                                                
BLDSRT8  DS    0H                                                               
         ZIC   R5,5(R2)            INDEX INTO DATALIST FOR NEXT ENTRY           
         BCTR  R5,0                                                             
         MH    R5,=Y(L'DATALIST)                                                
         A     R5,ADTALIST                                                      
         CLI   8(R5),0             TEST NO INPUT LENGTH                         
         BE    BLDSRT10            YES - SKIP                                   
         MVC   0(15,R3),=15X'FF'   GENERATE AVE/TOT RECORDS                     
         MVC   15(1,R3),3(R2)      MOVE A/T/B                                   
         CLI   15(R3),0                                                         
         BE    BLDSRT10                                                         
         GOTO1 SORTER,DMCB,=C'PUT',SORTKEY                                      
         SPACE 1                                                                
BLDSRT10 SH    R2,=H'4'                                                         
         SH    R3,=H'16'                                                        
         BCT   R0,BLDSRT8                                                       
         SPACE 1                                                                
BLDSRT12 CLI   REPOPT,C'N'         REPORT AVE/TOT                               
         BE    BLDSRTX                                                          
         MVC   0(15,R3),=15X'FF'                                                
         MVC   15(1,R3),REPOPT                                                  
         BASR  RE,RF                                                            
BLDSRTX  XIT1                                                                   
         EJECT                                                                  
* FIGURE OUT LENGTH IN 1/4 HRS FROM TIME *                                      
         SPACE 1                                                                
FIGLEN   NTR1                                                                   
         MVC   SORTQHRS,=F'2'                                                   
         OC    SORTTIME+2(2),SORTTIME+2                                         
         BZ    BLDSRTX                                                          
         XC    DUB,DUB                                                          
         LA    R2,SORTTIME                                                      
         LA    R3,DUB+3                                                         
         BAS   RE,GETQ                                                          
         LA    R2,2(R2)                                                         
         LA    R3,DUB+7                                                         
         BAS   RE,GETQ                                                          
         LM    R0,R1,DUB           START R0, END R1                             
         SR    R1,R0                                                            
         BNP   BLDSRTX                                                          
         ST    R1,SORTQHRS                                                      
         B     BLDSRTX                                                          
         SPACE 1                                                                
GETQ     NTR1                                                                   
         LH    R1,0(R2)            MILITARY TIME TO 1/4 HOUR                    
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CH    R1,=H'6'                                                         
         BNL   *+8                                                              
         LA    R1,24(R1)                                                        
         SH    R1,=H'6'                                                         
         SLL   R1,2                                                             
         LR    R2,R1                                                            
         SRDA  R0,32                                                            
         D     R0,=F'15'                                                        
         AR    R2,R1                                                            
         STC   R2,0(R3)                                                         
         B     BLDSRTX                                                          
         LTORG                                                                  
         EJECT                                                                  
* READ RECORDS FROM SORT AND WRITE TO SEQUENTIAL DISK FILE                      
* IF RANKING, DO ANOTHER SORT TO DETERMINE RANK NUMBERS                         
         SPACE 1                                                                
GETSORT  DS    0D                                                               
         NMOD1 0,*GETSORT                                                       
         L     RC,0(R1)                                                         
         SPACE 1                                                                
         OPEN  (SOUT,(OUTPUT))                                                  
         SPACE 1                                                                
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         B     GS10                                                             
*                                                                               
GS4      GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R6,15,DMCB+4                                                     
         BZ    GS20                                                             
         SPACE 1                                                                
GS8      CLC   SORTKEY,0(R6)              TEST SAME KEY                         
         BE    GS12                                                             
*                                                                               
         CLC   SORTKEY(L'SORTKEY-2),0(R6) TEST SAME UP TO DATA TYPE             
         BNE   GS9                        NO - DIFFERENCE IS IN ROWS            
         EJECT                                                                  
* WHEN AVERAGES ARE TAKEN ACROSS REAL BOOKS AND UPGRADES, RECORDS ARE           
* NOT IN COMPATIBLE FORMATS. IUNOUT CONVERTS IUN DATA TO UPGRADE                
* (DEMOUT) FORMAT SO RECORDS CAN BE ADDED.                                      
         SPACE 1                                                                
         BAS   RE,IUNOUT           CONVERT IUN DATA TO UPGRADE FORMAT           
         B     GS12                AND CONTINUE                                 
*                                                                               
GS9      BAS   RE,PUTOUT            NO - PUT OUT THE SAVED RECORD               
*                                                                               
GS10     XC    SORTXTRA,SORTXTRA                                                
         LA    R0,4                                                             
         LA    R1,SORTDEMS                                                      
         XC    0(256,R1),0(R1)                                                  
         LA    R1,256(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         BC    00,GS12             USE TO NOP TRACE                             
         CLI   TRACEOPT,C'Y'                                                    
         BNE   GS12                                                             
*                                                                               
         L     RF,ATWA                                                          
         USING T20FFFD,RF                                                       
         L     RF,TWADCONS                                                      
         USING TWADCOND,RF                                                      
         L     RF,TPRNTBL                                                       
         DROP  RF                                                               
         GOTO1 (RF),DMCB,(7,=C'GETSORT'),(R6),C'DUMP',256,=C'1D'                
*                                                                               
GS12     MVC   SORTKEY,0(R6)               SAVE NEW KEY                         
         MVC   SORTCMT,SORTCMT-SORTKEY(R6) SAVE NEW COMMENT                     
         MVC   SORTDMOV,SORTDMOV-SORTKEY(R6) AND DEMO OVRD FLAGS                
*                                                                               
         LA    R6,SORTXTRA-SORTKEY(R6)  POINT TO FIRST COLUMN                   
         LA    R2,SORTXTRA                                                      
         L     R0,NUMCOLS                                                       
         CLI   SORTKID,C'I'        TEST IUN DATA                                
         BNE   *+8                 NO                                           
         LA    R0,8+5*IUNDEMS+3    UNVS/RTGS/PUTS/IMPS/TOTS/VUTS + 8            
         SPACE 1                                                                
GS14     L     R1,0(R2)                                                         
         A     R1,0(R6)                                                         
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R6,4(R6)                                                         
         BCT   R0,GS14                                                          
         B     GS4                                                              
         EJECT                                                                  
GS20     BAS   RE,PUTOUT           PUT LAST RECORD                              
*                                                                               
         GOTO1 SORTER,DMCB,=C'END'                                              
         CLOSE (SOUT)                                                           
         FREEPOOL SOUT                                                          
         SPACE 1                                                                
* ALL RECORDS WRITTEN TO OUTPUT FILE - TEST RANKING REQUIRED *                  
         SPACE 1                                                                
         LA    R2,DETS                                                          
         LA    R3,SORTKEY                                                       
         SR    R4,R4               R4=NUMBER OF SORT FIELD                      
         ZIC   R0,NDETS                                                         
         SPACE 1                                                                
GS22     ST    R3,ARANK            SAVE ADDRESS OF RANK COLUMN                  
         STC   R4,NRANKCOL         AND COLUMN NUMBER                            
         SPACE 1                                                                
GS24     CLI   0(R2),C'R'          SEE IF RANK IS ACTIVE                        
         BE    GS30                                                             
         MVC   LDEMCNT,ACTNDEM                                                  
         CLI   1(R2),23            SEE IF RANK IS ACTIVE                        
         BE    GS100                                                            
         LA    R2,4(R2)                                                         
         LA    R3,16(R3)                                                        
         LA    R4,1(R4)                                                         
         BCT   R0,GS22                                                          
         B     GSXIT                                                            
         EJECT                                                                  
* RANKING - NEED ANOTHER SORT *                                                 
         SPACE 1                                                                
GS30     DS    0H                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         LA    R0,GS50                                                          
         L     RE,=A(SOUT)                                                      
         STCM  R0,7,33(RE)         SET EOF ROUTINE ADDRESS                      
         OPEN  (SOUT,(INPUT))      AND OPEN THE FILE                            
         SPACE 1                                                                
GS32     LA    R0,SORTLEN                                                       
         GET   SOUT,(R0)                                                        
*                                                                               
         L     R2,ARANK            POSITION TO RANK IN KEY                      
         CLI   0(R2),X'FF'         DONT MODIFY A TOTAL                          
         BE    GS48                                                             
         TM    DEMOS,X'20'         FOR CPP/M                                    
         BZ    GS34                DON'T NEED TO UNWEIGHT                       
         MVC   0(4,R2),SORTDEMS                                                 
         B     GS48                                                             
         SPACE 1                                                                
GS34     L     R0,SORTDEMS         COMPUTE AVERAGE VALUE                        
         SRDA  R0,31                                                            
         ICM   RF,15,SORTWT                                                     
         BNZ   GS44                                                             
         SR    R1,R1                                                            
         B     GS46                                                             
         SPACE 1                                                                
GS44     DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         SPACE 1                                                                
GS46     AH    R1,=H'1'            INSURE NON-ZERO COMPLEMENT                   
         LCR   R1,R1                                                            
         ST    R1,0(R2)                                                         
         SPACE 1                                                                
GS48     GOTO1 SORTER,DMCB,=C'PUT',SORTKEY                                      
         B     GS32                                                             
         EJECT                                                                  
* ALL RECORDS READ - RETRIEVE FROM SORT AND ASSIGN RANKS *                      
* WRITE ALL DATA BACK TO SEQ OUTPUT FILE                 *                      
         SPACE 1                                                                
GS50     DS    0H                                                               
         XC    LASTVAL,LASTVAL                                                  
         MVC   RANKNO,=F'1'                                                     
*                                                                               
         CLOSE (SOUT)              CLOSE SEQ FILE                               
         FREEPOOL SOUT                                                          
         OPEN  (SOUT,(OUTPUT))     AND OPEN FOR OUTPUT                          
         SPACE 1                                                                
GS52     GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R3,15,DMCB+4                                                     
         BZ    GS70                                                             
         SPACE 1                                                                
GS60     MVI   DMCB+8,0                                                         
         LA    R2,SORTKEY          MOVE RECORD INTO MY AREA                     
*                                                                               
         MVC   0(L'SORTKEY,R2),0(R3)      FIRST THE KEY                         
         LA    R2,L'SORTKEY(R2)                                                 
         LA    R3,L'SORTKEY(R3)                                                 
*                                                                               
         MVC   0(L'SORTCMT,R2),0(R3)      THEN THE COMMENTS                     
         LA    R2,L'SORTCMT(R2)                                                 
         LA    R3,L'SORTCMT(R3)                                                 
*                                                                               
         MVC   0(L'SORTDMOV,R2),0(R3)     THEN THE OVRD FLAGS                   
         LA    R2,L'SORTDMOV(R2)                                                
         LA    R3,L'SORTDMOV(R3)                                                
         SPACE 1                                                                
         L     R1,NUMCOLS                                                       
         SLL   R1,2                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8              THEN THE COLUMNS                             
         B     *+10                                                             
         MVC   0(0,R2),0(R3)                                                    
         SPACE 1                                                                
         L     R2,ARANK            INSPECT RANK COLUMN                          
         CLC   0(15,R2),=15X'FF'                                                
         BNE   GS62                                                             
         MVC   RANKNO,=F'1'        TOTAL RECORD - RESET RANK CONTROL            
         B     GS66                                                             
         SPACE 1                                                                
GS62     CLI   NRANKCOL,0          TEST RANK ON WHOLE REPORT                    
         BE    GS64                                                             
         ZIC   R1,NRANKCOL         GET RANK COL NUMBER                          
         SLL   R1,4                X COL WIDTH                                  
         BCTR  R1,0                                                             
         EX    R1,*+8              COMPARE UP TO RANK COL                       
         B     *+10                                                             
         CLC   LASTRFLD(0),SORTKEY                                              
         BE    GS64                                                             
         MVC   RANKNO,=F'1'        IF CONTROL BREAK, RESET RANK                 
         SPACE 1                                                                
GS64     LA    RE,SORTKEY                                                       
         MVC   LASTRFLD,0(RE)      SAVE ALL VALUES                              
         SPACE 1                                                                
         MVC   4(4,R2),RANKNO      MOVE RANK NUMBER AFTER RANK VALUE            
         L     R3,RANKNO                                                        
         LA    R3,1(R3)                                                         
         ST    R3,RANKNO                                                        
*                                                                               
GS66     MVC   SORTLEN,SOUTLEN     PUT RANKED RECORD TO WORK FILE               
         LA    R0,SORTLEN                                                       
         PUT   SOUT,(R0)                                                        
         B     GS52                                                             
*                                                                               
GS70     DS    0H                                                               
         CLOSE (SOUT)                                                           
         FREEPOOL SOUT                                                          
         B     GSXIT                                                            
         EJECT                                                                  
* RANKING - NEED ANOTHER SORT *                                                 
         SPACE 1                                                                
GS100    DS    0H                                                               
         ZIC   R5,LDEMCNT                                                       
         BCTR  R5,0                                                             
         SLL   R5,3                                                             
         LA    R5,SORTDEMS(R5)                                                  
*                                                                               
         ZIC   R3,LDEMCNT                                                       
         BCTR  R3,0                                                             
         MH    R3,=H'3'                                                         
         LA    R3,DEMOS(R3)                                                     
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         LA    R0,GS170                                                         
         L     RE,=A(SOUT)                                                      
         STCM  R0,7,33(RE)         SET EOF ROUTINE ADDRESS                      
         OPEN  (SOUT,(INPUT))      AND OPEN THE FILE                            
         SPACE 1                                                                
GS110    LA    R0,SORTLEN                                                       
         GET   SOUT,(R0)                                                        
*                                                                               
         L     R2,ARANK            POSITION TO RANK IN KEY                      
         CLC   0(15,R2),=15X'FF'   DONT MODIFY A TOTAL                          
         BE    GS160                                                            
         TM    0(R3),X'20'         FOR CPP/M                                    
         BZ    GS120               DON'T NEED TO UNWEIGHT                       
         MVC   0(4,R2),0(R5)                                                    
         B     GS160                                                            
         SPACE 1                                                                
GS120    ICM   R0,15,0(R5)         COMPUTE AVERAGE VALUE                        
         SRDA  R0,31                                                            
         ICM   RF,15,SORTWT                                                     
         BNZ   GS140                                                            
         SR    R1,R1                                                            
         B     GS150                                                            
         SPACE 1                                                                
GS140    DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         SPACE 1                                                                
GS150    AH    R1,=H'1'            INSURE NON-ZERO COMPLEMENT                   
         LCR   R1,R1                                                            
         ST    R1,0(R2)                                                         
         SPACE 1                                                                
GS160    GOTO1 SORTER,DMCB,=C'PUT',SORTKEY                                      
         B     GS110                                                            
         EJECT                                                                  
* ALL RECORDS READ - RETRIEVE FROM SORT AND ASSIGN RANKS *                      
* WRITE ALL DATA BACK TO SEQ OUTPUT FILE                 *                      
         SPACE 1                                                                
GS170    DS    0H                                                               
         XC    LASTVAL,LASTVAL                                                  
         MVC   RANKNO,=F'1'                                                     
         MVC   MYTIE,=F'1'                                                      
*                                                                               
         CLOSE (SOUT)              CLOSE SEQ FILE                               
         FREEPOOL SOUT                                                          
         OPEN  (SOUT,(OUTPUT))     AND OPEN FOR OUTPUT                          
         SPACE 1                                                                
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R3,15,DMCB+4                                                     
         BZ    GS260                                                            
         SPACE 1                                                                
         MVC   SORTKEY,0(R3)        FIRST THE KEY                               
         LA    R3,L'SORTKEY(R3)                                                 
*                                                                               
         MVC   SORTCMT,0(R3)        THEN THE COMMENTS                           
         LA    R3,L'SORTCMT(R3)                                                 
*                                                                               
         MVC   SORTDMOV,0(R3)       THEN THE OVRD FLAGS                         
         LA    R3,L'SORTDMOV(R3)                                                
*                                                                               
         L     R1,NUMCOLS                                                       
         SLL   R1,2                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                THEN THE COLUMNS                           
         B     *+10                                                             
         MVC   SORTXTRA(0),0(R3)                                                
         SPACE 1                                                                
GS180    L     R2,=A(NEXTKEY)                                                   
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R3,15,DMCB+4                                                     
         BNZ   GS190                                                            
         XC    0(164,R2),0(R2)                                                  
         B     GS195                                                            
         SPACE 1                                                                
GS190    MVC   0(L'NEXTKEY,R2),0(R3)       FIRST THE KEY                        
         LA    R2,L'NEXTKEY(R2)                                                 
         LA    R3,L'NEXTKEY(R3)                                                 
*                                                                               
         MVC   0(L'SORTCMT,R2),0(R3) THEN THE COMMENTS                          
         LA    R2,L'SORTCMT(R2)                                                 
         LA    R3,L'SORTCMT(R3)                                                 
*                                                                               
         MVC   0(L'SORTDMOV,R2),0(R3) THEN THE OVRD FLAGS                       
         LA    R2,L'SORTDMOV(R2)                                                
         LA    R3,L'SORTDMOV(R3)                                                
*                                                                               
         L     R1,NUMCOLS                                                       
         SLL   R1,2                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                      THEN THE COLUMNS                     
         B     *+10                                                             
         MVC   0(0,R2),0(R3)                                                    
         SPACE 1                                                                
GS195    L     R2,ARANK            INSPECT RANK COLUMN                          
         LA    R4,SORTRECL(R2)     LASTKEY RANK LOCATION                        
         LA    R6,SORTRECL(R4)     NEXTKEY RANK LOCATION                        
         SPACE 1                                                                
         CLC   0(15,R2),=15X'FF'                                                
         BNE   GS200                                                            
         MVC   RANKNO,=F'1'        TOTAL RECORD - RESET RANK CONTROL            
         MVC   MYTIE,=F'1'                                                      
         B     GS250                                                            
         SPACE 1                                                                
GS200    CLI   NRANKCOL,0          TEST RANK ON WHOLE REPORT                    
         BE    GS210                                                            
         ZIC   R1,NRANKCOL         GET RANK COL NUMBER                          
         SLL   R1,4                X COL WIDTH                                  
         BCTR  R1,0                                                             
         EX    R1,*+8              COMPARE UP TO RANK COL                       
         B     *+10                                                             
         CLC   LASTKEY(0),SORTKEY                                               
         BE    GS210                                                            
         MVC   RANKNO,=F'1'        IF CONTROL BREAK, RESET RANK                 
         MVC   MYTIE,=F'1'                                                      
         SPACE 1                                                                
GS210    MVC   4(4,R2),RANKNO      MOVE RANK NUMBER AFTER RANK VALUE            
* TEST EQUAL CONDITION BETWEEN THE RECORDS                                      
         CLC   0(4,R2),0(R4)                                                    
         BE    GS240                                                            
*                                                                               
         CLC   0(4,R2),0(R6)                                                    
         BE    GS230                                                            
*                                                                               
* NO EQUAL CONDITION EXISTS MOVE RANK NUMBER OUT                                
* R5 POINTS TO ONE OF THE DEMOS IN THE SORTDEM FIELD                            
*                                                                               
         EDIT  (4,RANKNO),(3,4(R5))                                             
         B     GS250                                                            
*                                                                               
* EQUAL CONDITION EXISTS WITH PRIOR RECORD MOVE TIE NUMBER OUT                  
* R5 POINTS TO ONE OF THE DEMOS IN THE SORTDEM FIELD                            
*                                                                               
GS230    MVC   MYTIE,RANKNO                                                     
*                                                                               
GS240    EDIT  (4,MYTIE),(3,4(R5))                                              
         MVI   7(R5),C'='                                                       
*                                                                               
GS250    L     R3,RANKNO                                                        
         LA    R3,1(R3)                                                         
         ST    R3,RANKNO                                                        
         MVC   SORTLEN,SOUTLEN     PUT RANKED RECORD TO WORK FILE               
         LA    R0,SORTLEN                                                       
         PUT   SOUT,(R0)                                                        
*                                                                               
         BC    00,GS255            USE TO NOP TRACE                             
         CLI   TRACEOPT,C'Y'                                                    
         BNE   GS255                                                            
*                                                                               
         L     RF,ATWA                                                          
         USING T20FFFD,RF                                                       
         L     RF,TWADCONS                                                      
         USING TWADCOND,RF                                                      
         L     RF,TPRNTBL                                                       
         DROP  RF                                                               
         LH    R0,SORTLEN                                                       
         GOTO1 (RF),DMCB,(4,=C'SOUT'),SORTLEN,C'DUMP',(R0),=C'1D'               
*                                                                               
GS255    L     R2,=A(NEXTKEY)                                                   
         OC    0(164,R2),0(R2)                                                  
         BZ    GS260                                                            
* MOVE SORTREC TO LASTREC                                                       
         MVC   LASTKEY,SORTKEY             FIRST THE KEY                        
         MVC   LASTCMT,SORTCMT                                                  
         MVC   LASTDMOV,SORTDMOV                                                
         L     R1,NUMCOLS                                                       
         SLL   R1,2                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                      THEN THE COLUMNS                     
         B     *+10                                                             
         MVC   LASTXTRA(0),SORTXTRA                                             
         SPACE 1                                                                
* MOVE NEXTKEY TO SORTKEY                                                       
         L     R6,=A(NEXTKEY)                                                   
         MVC   SORTKEY,0(R6)       FIRST THE KEY                                
         LA    R6,L'SORTKEY(R6)                                                 
*                                                                               
         MVC   SORTCMT,0(R6)       THEN THE COMMENTS                            
         LA    R6,L'SORTCMT(R6)                                                 
*                                                                               
         MVC   SORTDMOV,0(R6)      THEN THE OVRD FLAGS                          
         LA    R6,L'SORTDMOV(R6)                                                
*                                                                               
         L     R1,NUMCOLS                                                       
         SLL   R1,2                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8              THEN THE COLUMNS                             
         B     *+10                                                             
         MVC   SORTXTRA(0),0(R6)                                                
         B     GS180                                                            
         SPACE 1                                                                
GS260    DS    0H                                                               
         CLOSE (SOUT)                                                           
         FREEPOOL SOUT                                                          
*                                                                               
         ZIC   R5,LDEMCNT                                                       
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BNH   GSXIT                                                            
         STC   R5,LDEMCNT                                                       
         B     GS100                                                            
         EJECT                                                                  
* ROUTINE TO PUT RECORD TO WORK FILE                                            
* IF IUN SORT ACTIVE - UNWEIGHT AND EXTRACT REQUESTED DEMOS FIRST               
         SPACE 1                                                                
PUTOUT   NTR1                                                                   
         CLC   SORTWT,=F'1'        TEST NEED TO UNWEIGHT                        
         BE    PUTOUT2X                                                         
*                                                                               
         LA    R0,IUNDEMS*5+3+8    ALL SORT DEMOS + SORTXTRA + VUTS             
         BCTR  R0,0                LESS 1 FOR COUNT                             
         MVC   SORTCNT,=F'1'                                                    
         L     R2,SORTWT           GET WEIGHTING FACTOR                         
         LA    R1,SORTWT                                                        
         SPACE 1                                                                
* UNWEIGHT AND ROUND VALUES *                                                   
         SPACE 1                                                                
PUTOUT2  SR    RE,RE                                                            
         L     RF,0(R1)                                                         
         AR    RF,RF               X 2                                          
         DR    RE,R2                                                            
         AH    RF,=H'1'                                                         
         SRL   RF,1                                                             
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,PUTOUT2                                                       
*                                                                               
PUTOUT2X CLI   SORTKID,C'I'        TEST IUN DATA                                
         BNE   PUTOUT20            NO - CONTINUE                                
* BUILD IUN WORK AREA                                                           
         LA    R0,5                UNVS/RTGS/PUTS/IMPS/TOTS                     
         L     RE,=A(IUNWK)                                                     
         LA    RF,SORTDEMS                                                      
*                                                                               
PUTOUT3  MVC   0(4*IUNDEMS,RE),0(RF)   MOVE VALUES                              
         LA    RE,4*IUNDEMS(RE)                                                 
         LA    RF,4*IUNDEMS(RF)                                                 
         BCT   R0,PUTOUT3                                                       
*                                                                               
         LA    R0,4                    SET NEW DATA = OLD DATA                  
         L     RE,=A(IUNOLD)                                                    
         L     RF,=A(IUNNEW)                                                    
*                                                                               
PUTOUT4  MVC   0(4*IUNDEMS,RF),0(RE)                                            
         LA    RE,4*IUNDEMS(RE)                                                 
         LA    RF,4*IUNDEMS(RF)                                                 
         BCT   R0,PUTOUT4                                                       
*                                                                               
         L     RE,=A(IUNVUT)                                                    
         MVC   0(12,RE),SORTVUTS       MOVE CAREFULLY PRESERVED VUTS            
         SPACE 1                                                                
* COMPUTE HOME SHARES AS RATING/VUT OR RATING/HUT *                             
         SPACE 1                                                                
PUTOUT7  LA    R0,3                NOW COMPUTE HMSHR = RTG/VUT                  
         SR    R2,R2               R2=INDEX REG                                 
*                                                                               
PUTOUT8  L     RF,=A(IRTGOLD)                                                   
         L     RF,IUNHMDSP(RF,R2)                                               
         M     RE,=F'1000'                                                      
         L     R1,=A(IUNVUT)                                                    
         L     R1,0(R1,R2)                                                      
         CLI   SHARCALC,C'Y'                                                    
         BNE   PUTOUT9                                                          
         L     R1,=A(IPUTOLD)                                                   
         L     R1,IUNHMDSP(R1,R2)  GET ACTUAL PUT                               
*                                                                               
PUTOUT9  LTR   R1,R1                                                            
         BNZ   PUTOUT10                                                         
         SR    RF,RF                                                            
         B     PUTOUT12                                                         
*                                                                               
PUTOUT10 DR    RE,R1                                                            
*                                                                               
PUTOUT12 L     RE,=A(IUNVUT)                                                    
         ST    RF,0(RE,R2)         SET SHARE VALUE IN IUN WORK                  
*                                                                               
         LA    R2,4(R2)                                                         
         BCT   R0,PUTOUT8                                                       
         SPACE 1                                                                
*                                  CALL DEMAINT TO BUILD IUN RECORD             
         SPACE 1                                                                
         L     RE,=A(SAVDBLOK)                                                  
         MVC   0(256,RE),DBLOCK    SAVE DBLOCK                                  
*                                                                               
         LA    R0,4                TRY TO KEEP DEMEL HAPPY                      
         L     R1,=A(IUNREC)                                                    
         XC    0(256,R1),0(R1)                                                  
         LA    R1,256(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         MVC   DBFILE,=C'PAV'                                                   
         L     RE,=A(IUNREC)                                                    
         ST    RE,DBAREC                                                        
         LA    RE,23(RE)           POINT TO FIRST DEMO ELEMENT                  
         MVI   0(RE),0             SET END OF RECORD                            
         ST    RE,DBAQUART         SET A(QH) ELEMENT                            
         LA    R0,IUNWKLEN/4                                                    
         STH   R0,DBNUMVLS                                                      
*                                                                               
         MVC   WORK(10),OFORMAT    SET PROPER FORMULAS                          
         LA    RE,SORTKEY                                                       
         LA    RF,DETS                                                          
         ZIC   R0,NDETS                                                         
         CLI   1(RF),BOOKFLD       BOOK FIELD                                   
         BE    *+20                                                             
         LA    RE,16(RE)           NEXT SORT KEY                                
         LA    RF,4(RF)            NEXT DETAIL DESC                             
         BCT   R0,*-16                                                          
         LA    RE,DBSELBK-2        OH WELL, DEFAULT TO SOMETHING                
*                                                                               
         CLC   2(2,RE),=X'5801'    SWITCH FORMULAS AT JAN/88                    
         BL    *+10                                                             
         MVC   WORK+7(2),=X'530B'                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAINT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,OREP,DBLOCK,A(IUNWK),WORK                              
         B     PUTOUT14                                                         
*                                                                               
OREP     DC    C'REP'                                                           
OFORMAT  DC    C'PAVUIUN',X'520B00'                                             
         EJECT                                                                  
* CALL DEMOUT TO EXTRACT REQUESTED DEMOS                                        
         SPACE 1                                                                
PUTOUT14 DS    0H                                                               
         GOTO1 DEMOUT,DMCB,(C'L',EXDEMOS),DBLOCK,SORTDEMS                       
*                                                                               
         L     RE,=A(SAVDBLOK)                                                  
         MVC   DBLOCK,0(RE)            AND RESTORE DBLOCK                       
*                                                                               
PUTOUT20 LA    R2,EXDEMOS                                                       
         LA    R3,SORTDEMS                                                      
         ZIC   R4,NEXDEMS                                                       
         SPACE 1                                                                
PUTOUT25 DS    0H                                                               
         CLC   0(3,R2),=X'20C9FF'  COST COLUMN                                  
         BNE   PUTOUT35                                                         
         MVC   0(4,R3),SORTWCST                                                 
         SPACE 1                                                                
PUTOUT35 LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,PUTOUT25                                                      
         SPACE 1                                                                
         LA    R3,SORTDEMS                                                      
         ZIC   R4,NEXDEMS                                                       
         SRL   R4,1                                                             
         SPACE 1                                                                
PUTOUT40 OC    0(4,R3),0(R3)       IF FIRST OPERAND IS ZERO                     
         BNZ   *+10                                                             
         XC    4(4,R3),4(R3)       MAKE THE SECOND ZERO                         
         LA    R3,8(R3)                                                         
         BCT   R4,PUTOUT40                                                      
*                                                                               
         BAS   RE,COMPCPP                                                       
*                                                                               
         MVC   SORTLEN,SOUTLEN                                                  
         LA    R0,SORTLEN                                                       
         PUT   SOUT,(R0)                                                        
*                                                                               
         BC    00,PUTOUT42         USE TO NOP TRACE                             
         CLI   TRACEOPT,C'Y'                                                    
         BNE   PUTOUT42                                                         
*                                                                               
         L     RF,ATWA                                                          
         USING T20FFFD,RF                                                       
         L     RF,TWADCONS                                                      
         USING TWADCOND,RF                                                      
         L     RF,TPRNTBL                                                       
         DROP  RF                                                               
         LH    R0,SORTLEN                                                       
         GOTO1 (RF),DMCB,(4,=C'SOUT'),SORTLEN,C'DUMP',(R0),=C'1D'               
*                                                                               
PUTOUT42 B     GSXIT                                                            
         EJECT                                                                  
* ROUTINE TO EXTRACT IUN DEMOS AND MAINTAIN WEIGHTS                             
* CALLED BY GETSORT SO AVERAGE/TOTAL RECORDS CAN BE ADDED WHEN                  
* DATA IS IN INCOMPATIBLE FORMATS                                               
         SPACE 1                                                                
IUNOUT   NTR1                                                                   
         CLC   SORTWT,=F'1'        TEST NEED TO UNWEIGHT                        
         BE    IUNOUT2X                                                         
*                                                                               
         LA    R0,IUNDEMS*5+3+8    ALL SORT DEMOS                               
         BCTR  R0,0                ADJUST FOR COUNT                             
         MVC   SORTCNT,=F'1'                                                    
         L     R2,SORTWT                                                        
         LA    R1,SORTWT                                                        
*                                  UNWEIGHT AND ROUND VALUES                    
IUNOUT2  SR    RE,RE                                                            
         L     RF,0(R1)                                                         
         AR    RF,RF               X 2                                          
         DR    RE,R2                                                            
         AH    RF,=H'1'                                                         
         SRL   RF,1                                                             
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,IUNOUT2                                                       
         SPACE 1                                                                
* BUILD IUN WORK AREA                                                           
         SPACE 1                                                                
IUNOUT2X DS    0H                                                               
         LA    R0,5                UNVS/RTGS/PUTS/IMPS/TOTS                     
         L     RE,=A(IUNWK)                                                     
         LA    RF,SORTDEMS                                                      
*                                                                               
IUNOUT3  MVC   0(4*IUNDEMS,RE),0(RF)   MOVE VALUES                              
         LA    RE,4*IUNDEMS(RE)                                                 
         LA    RF,4*IUNDEMS(RF)                                                 
         BCT   R0,IUNOUT3                                                       
*                                                                               
         LA    R0,4                    SET NEW DATA = OLD DATA                  
         L     RE,=A(IUNOLD)                                                    
         L     RF,=A(IUNNEW)                                                    
*                                                                               
IUNOUT4  MVC   0(4*IUNDEMS,RF),0(RE)                                            
         LA    RE,4*IUNDEMS(RE)                                                 
         LA    RF,4*IUNDEMS(RF)                                                 
         BCT   R0,IUNOUT4                                                       
*                                                                               
         L     RE,=A(IUNVUT)                                                    
         MVC   0(12,RE),SORTVUTS       MOVE CAREFULLY PRESERVED VUTS            
         SPACE 1                                                                
* COMPUTE HOME SHARES AS RATING/VUT OR RATING/HUT *                             
         SPACE 1                                                                
IUNOUT7  LA    R0,3                NOW COMPUTE HMSHR = RTG/VUT                  
         SR    R2,R2               R2=INDEX REG                                 
*                                                                               
IUNOUT8  L     RF,=A(IRTGOLD)                                                   
         L     RF,IUNHMDSP(RF,R2)                                               
         M     RE,=F'1000'                                                      
         L     R1,=A(IUNVUT)                                                    
         L     R1,0(R1,R2)                                                      
         CLI   SHARCALC,C'Y'                                                    
         BNE   IUNOUT9                                                          
         L     R1,=A(IPUTOLD)                                                   
         L     R1,IUNHMDSP(R1,R2)  GET ACTUAL PUT                               
*                                                                               
IUNOUT9  LTR   R1,R1                                                            
         BNZ   IUNOUT10                                                         
         SR    RF,RF                                                            
         B     IUNOUT12                                                         
*                                                                               
IUNOUT10 DR    RE,R1                                                            
*                                                                               
IUNOUT12 L     RE,=A(IUNVUT)                                                    
         ST    RF,0(RE,R2)         SET SHARE VALUE IN IUN WORK                  
*                                                                               
         LA    R2,4(R2)                                                         
         BCT   R0,IUNOUT8                                                       
         SPACE 1                                                                
*                                  CALL DEMAINT TO BUILD IUN RECORD             
         SPACE 1                                                                
         L     RE,=A(SAVDBLOK)                                                  
         MVC   0(256,RE),DBLOCK    SAVE DBLOCK                                  
*                                                                               
         LA    R0,4                TRY TO KEEP DEMEL HAPPY                      
         L     R1,=A(IUNREC)                                                    
         XC    0(256,R1),0(R1)                                                  
         LA    R1,256(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         MVC   DBFILE,=C'PAV'                                                   
         L     RE,=A(IUNREC)                                                    
         ST    RE,DBAREC                                                        
         LA    RE,23(RE)           POINT TO FIRST DEMO ELEMENT                  
         MVI   0(RE),0             SET END OF RECORD                            
         ST    RE,DBAQUART         SET A(QH) ELEMENT                            
         LA    R0,IUNWKLEN/4                                                    
         STH   R0,DBNUMVLS                                                      
*                                                                               
         MVC   WORK(10),OFORMAT    SET PROPER FORMULAS                          
         LA    RE,SORTKEY                                                       
         LA    RF,DETS                                                          
         ZIC   R0,NDETS                                                         
         CLI   1(RF),BOOKFLD       BOOK FIELD                                   
         BE    *+20                                                             
         LA    RE,16(RE)           NEXT SORT KEY                                
         LA    RF,4(RF)            NEXT DETAIL DESC                             
         BCT   R0,*-16                                                          
         LA    RE,DBSELBK-2        OH WELL, DEFAULT TO SOMETHING                
*                                                                               
         CLC   2(2,RE),=X'5801'    SWITCH FORMULAS AT JAN/88                    
         BL    *+10                                                             
         MVC   WORK+7(2),=X'530B'                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAINT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,OREP,DBLOCK,A(IUNWK),WORK                              
         B     IUNOUT14                                                         
         EJECT                                                                  
* CALL DEMOUT TO EXTRACT REQUESTED DEMOS                                        
         SPACE 1                                                                
IUNOUT14 DS    0H                                                               
         GOTO1 DEMOUT,DMCB,(C'L',EXDEMOS),DBLOCK,SORTDEMS                       
*                                                                               
         MVI   SORTKID,C'U'        INDICATE DATA NOW UPGRADE FORMAT             
*                                                                               
         L     RE,=A(SAVDBLOK)                                                  
         MVC   DBLOCK,0(RE)            AND RESTORE DBLOCK                       
         SPACE 1                                                                
* MULTIPLY BY NUMBOOKS -1 = NUMBER OF REAL BOOKS FOR WEIGHTING                  
         SPACE 1                                                                
         ZIC   R0,NEXDEMS                                                       
         AH    R0,=H'8'            ADJUST FOR SORTXTRA                          
         ZIC   R1,NBOOKS                                                        
         BCTR  R1,0                                                             
         LA    R2,SORTCNT                                                       
*                                                                               
IUNOUT20 L     RF,0(R2)                                                         
         MR    RE,R1                                                            
         ST    RF,0(R2)                                                         
*                                                                               
IUNOUT22 LA    R2,4(R2)                                                         
         BCT   R0,IUNOUT20                                                      
*                                                                               
IUNOUT30 BC    15,IUNOUT42         USE TO NOP TRACE                             
         CLI   TRACEOPT,C'Y'                                                    
         BNE   IUNOUT42                                                         
*                                                                               
         L     RF,ATWA                                                          
         USING T20FFFD,RF                                                       
         L     RF,TWADCONS                                                      
         USING TWADCOND,RF                                                      
         L     RF,TPRNTBL                                                       
         DROP  RF                                                               
         LH    R0,SORTLEN                                                       
         GOTO1 (RF),DMCB,(6,=C'IUNOUT'),SORTLEN,C'DUMP',(R0),=C'1D'             
*                                                                               
IUNOUT42 B     GSXIT                                                            
         EJECT                                                                  
* ROUTINE TO COMPUTE CPP/CPM IN DEMO COLUMNS                                    
         SPACE 1                                                                
COMPCPP  NTR1                                                                   
         LA    R2,AVDEMOS                                                       
         LA    R3,SORTDEMS                                                      
         ZIC   R5,NDEMOS                                                        
         SPACE 1                                                                
CPP2     TM    0(R2),X'20'         PICK OUT RELEVANT COLUMNS                    
         BNO   CPPEND                                                           
         CLI   2(R2),X'FF'                                                      
         BE    CPPEND                                                           
         SR    R1,R1                                                            
         OC    SORTWCST,SORTWCST                                                
         BZ    CPP4                                                             
         OC    0(4,R3),0(R3)                                                    
         BZ    CPP4                                                             
         L     R1,SORTWCST         PICK UP COST                                 
         SR    R0,R0                                                            
         CLI   1(R2),C'R'          SELECT APPROPRIATE SCALE                     
         BNE   *+8                                                              
         LA    R0,20                                                            
         CLI   1(R2),C'I'                                                       
         BNE   *+8                                                              
         LA    R0,20                                                            
         CLI   1(R2),C'T'                                                       
         BNE   *+8                                                              
         LA    R0,20                                                            
         MR    R0,R0                                                            
         D     R0,0(R3)            DIVIDE BY WEIGHTED DEMOS                     
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         SPACE 1                                                                
CPP4     ST    R1,0(R3)                                                         
         SPACE 1                                                                
CPPEND   LA    R2,3(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R5,CPP2                                                          
         SPACE 1                                                                
GSXIT    XIT1                                                                   
         EJECT                                                                  
* DCB FOR INTERNAL FILE AND OTHER ODDMENTS                                      
         SPACE 1                                                                
         PRINT NOGEN                                                            
SOUT     DCB   RECFM=VB,BLKSIZE=4000,DSORG=PS,MACRF=(PM,GM),           X        
               LRECL=2000,DDNAME=SOUT,EODAD=*                                   
         PRINT NOGEN                                                            
         SPACE 1                                                                
LASTVAL  DC    F'0'                                                             
LASTRFLD DC    XL128'00'                                                        
NRANKCOL DC    X'00'                                                            
RANKNO   DC    F'1'                                                             
RANKTIE  DC    F'1'                                                             
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* INITIALIZATION ROUTINES *                                                     
         SPACE 1                                                                
INIT     NMOD1 0,**INIT                                                         
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         MVC   HEADHOOK,AHOOK                                                   
         MVC   SPECS,AHEDSPEC                                                   
*                                                                               
         MVC   COST,=F'10000'      PRESET $100 COST FOR TESTING                 
         XC    MTABCNT,MTABCNT     CLEAR MKT TABLE COUNTER                      
*                                                                               
IN2      DS    0H                                                               
         ZIC   R0,NDEMOS           GET NUMBER OF DEMOS                          
         ZIC   R1,WDEMS            GET DEMO WIDTH                               
         MR    R0,R0                                                            
         ZIC   R0,WDETS                                                         
         AR    R1,R0                                                            
         CLI   AVOPT,C'Y'          NEED ROOM TO DESCRIBE AVAIL DATA             
         BE    IN4                                                              
         CLI   STACKOPT,C'Y'       OR STACK                                     
         BE    IN4                                                              
         CLI   CPPOPT,C'Y'         OR CPP/M DATA                                
         BNE   *+8                                                              
         SPACE 1                                                                
IN4      LA    R1,6(R1)                                                         
         SPACE 1                                                                
         LA    R1,1(R1)                                                         
         LA    R2,132                                                           
         SR    R2,R1                                                            
         BNM   IN6                                                              
         ZIC   R0,NDEMOS           WONT FIT INTO 132 - TRIM DEMOS               
         BCTR  R0,0                                                             
         STC   R0,NDEMOS                                                        
         B     IN2                                                              
         SPACE 1                                                                
IN6      BP    *+8                                                              
         LA    R2,2                                                             
         SRL   R2,1                                                             
         ST    R2,DISP             RECORD DISPLACEMENTS                         
         ZIC   R3,WDETS                                                         
         AR    R2,R3                                                            
         ST    R2,DEMDISP                                                       
         SPACE 1                                                                
         LA    R1,L'SORTKEY        KEYLEN MIGHT BE VARIABLE SOMEDAY             
         LA    R1,L'SORTCMT(R1)                                                 
         LA    R1,L'SORTDMOV(R1)                                                
         LA    R2,SORTCARD+15                                                   
         EDIT  (R1),(3,(R2)),FILL=0                                             
         SPACE 1                                                                
         ZIC   R3,NDEMOS           NUMBER OF COLUMNS                            
         SLL   R3,1                2 COLUMNS FOR EACH DEMO                      
         LA    R2,1                                                             
         CLI   STACKOPT,C'Y'       TIMES 2 FOR US STACK                         
         BNE   IN8                                                              
         LA    R2,2                                                             
         CLI   DBSELMED,C'C'       OR 3 FOR CANADIAN STACK                      
         BNE   IN10                                                             
         LA    R2,3                                                             
         B     IN10                                                             
         SPACE 1                                                                
IN8      CLI   AVOPT,C'Y'          TIMES 3 FOR AVAILS                           
         BNE   IN10                                                             
         LA    R2,3                                                             
         SPACE 1                                                                
IN10     CLI   CPPOPT,C'Y'                                                      
         BNE   *+8                                                              
         LA    R2,1(R2)            PLUS ANOTHER LINE FOR CPP/CPM                
         SPACE 1                                                                
         ST    R2,NLINES                                                        
         MR    R2,R2                                                            
         LA    R3,8(R3)            N'DEMS +8                                    
         ST    R3,NUMCOLS                                                       
         SLL   R3,2                TIMES 4                                      
         AR    R1,R3               PLUS KEY = LENGTH                            
         ST    R1,LSORT                                                         
         LA    R2,RECCARD+21                                                    
         EDIT  (R1),(3,(R2)),FILL=0                                             
*                                                                               
         LA    R1,4(R1)            COMPUTE LENGTH OF OUTPUT RECS                
         SLL   R1,16                                                            
         ST    R1,SOUTLEN                                                       
*                                                                               
         MVI   IUNSORT,C'N'                                                     
         CLI   DBSELMED,C'T'       TEST TV (US)                                 
         BNE   IN20                                                             
         MVI   IUNSORT,C'Y'        INDICATE IUN SORT ACTIVE                     
         LA    R0,L'SORTKEY+L'SORTCMT+L'SORTDMOV+L'SORTXTRA+IUNLEN              
         LA    R2,RECCARD+21                                                    
         EDIT  (R0),(3,(R2)),FILL=0                                             
*                                                                               
IN20     DS    0H                                                               
         L     RE,APRGBUFF         CLEAR PROGRAM BUFFER                         
         L     RF,=F'128000'                                                    
         XCEF                                                                   
         EJECT                                                                  
* SET UP FOR AVAIL OR STACK OR CPP FORMAT *                                     
         SPACE 1                                                                
         MVC   ACTNDEM,NDEMOS      SAVE ACTUAL DEMOS                            
         MVC   AVDEMOS(L'DEMOS),DEMOS                                           
         OC    NLINES,NLINES                                                    
         BZ    IN26                                                             
         ZIC   R1,NDEMOS                                                        
         LR    R3,R1                                                            
         M     R0,NLINES           MORE DEMOS IF AVAIL OR CPP                   
         STC   R1,NDEMOS                                                        
         LR    R1,R3                                                            
         MH    R1,=H'3'            WIDTH OF EACH DEMO CHUNK                     
*                                                                               
         CLI   STACKOPT,C'Y'       STACK OPTION                                 
         BNE   IN22                                                             
* STACK OPTION *                                                                
         LA    R2,AVDEMOS                                                       
         LA    R3,SUBR             ONLY RATINGS ON LINE 1                       
         BAS   RE,AVADJUST                                                      
         AR    R2,R1                                                            
*                                                                               
         MVC   0(30,R2),AVDEMOS                                                 
         LA    R3,SUBE                                                          
         CLI   DBSELMED,C'C'       E ON CENTER LINE FOR CANADA                  
         BNE   *+10                                                             
         BAS   RE,AVADJUST                                                      
         AR    R2,R1                                                            
*                                                                               
         MVC   0(30,R2),AVDEMOS                                                 
         LA    R3,SUBI             IMPRESSIONS BELOW                            
         BAS   RE,AVADJUST                                                      
         AR    R2,R1                                                            
         CLI   CPPOPT,C'Y'                                                      
         BNE   IN26                                                             
         B     IN24                                                             
         SPACE 1                                                                
IN22     LA    R2,AVDEMOS                                                       
         AR    R2,R1                                                            
         CLI   AVOPT,C'Y'                                                       
         BNE   IN24                                                             
* AVAIL OPTION *                                                                
         MVC   0(30,R2),AVDEMOS    COPY DEMOS FOR SHARES                        
         LA    R3,SHARESUB                                                      
         BAS   RE,AVADJUST         AND ADJUST                                   
         AR    R2,R1                                                            
*                                                                               
         MVC   0(30,R2),AVDEMOS    THEN COPY TO PUTS/TOTS                       
         LA    R3,LEVELSUB                                                      
         BAS   RE,AVADJUST         AND ADJUST                                   
         AR    R2,R1                                                            
*                                                                               
         CLI   CPPOPT,C'Y'                                                      
         BNE   IN26                                                             
         SPACE 1                                                                
IN24     MVC   0(30,R2),AVDEMOS    COPY FOR CPP LINE                            
         LA    R3,CPPSUB                                                        
         BAS   RE,AVADJUST                                                      
         SPACE 1                                                                
IN26     ZIC   R1,NDEMOS           SET FF AT END OF DEM LIST                    
         MH    R1,=H'3'                                                         
         LA    R1,AVDEMOS(R1)                                                   
         MVI   0(R1),X'FF'                                                      
         B     IN30                                                             
         EJECT                                                                  
AVADJUST NTR1                                                                   
         ZIC   R0,ACTNDEM                                                       
         SPACE 1                                                                
AV2      LR    R4,R3                                                            
         CLC   0(3,R2),=X'20C9FF'                                               
         BNE   AV4                                                              
         CLC   0(2,R3),=C'RR'                                                   
         BNE   AV6                                                              
         SPACE 1                                                                
AV4      CLI   0(R4),C'$'                                                       
         BNE   *+8                                                              
         OI    0(R2),X'20'         TURN ON CPP/CPM BIT                          
         CLI   1(R2),C'T'          ADJUST IMPRESSIONS                           
         BE    AV8                                                              
         CLI   1(R2),C'I'                                                       
         BE    AV8                                                              
         LA    R4,1(R4)                                                         
         CLI   1(R2),C'R'          AND RATINGS                                  
         BE    AV8                                                              
         SPACE 1                                                                
AV6      MVC   0(3,R2),=X'00C9FF'  OTHERS MEANINGLESS                           
         B     AV10                                                             
         SPACE 1                                                                
AV8      CLI   0(R4),C'$'                                                       
         BE    AV10                                                             
         MVC   1(1,R2),0(R4)                                                    
         SPACE 1                                                                
AV10     LA    R2,3(R2)                                                         
         BCT   R0,AV2                                                           
         XIT1                                                                   
         SPACE 1                                                                
SUBR     DC    C'RR'                                                            
SUBE     DC    C'EE'                                                            
SUBI     DC    C'II'                                                            
SHARESUB DC    C'XS'                                                            
LEVELSUB DC    C'QP'                                                            
CPPSUB   DC    C'$$'                                                            
         EJECT                                                                  
* GENERATE FULL RANGE OF DEMOS *                                                
         SPACE 1                                                                
IN30     DS    0H                                                               
         ZIC   R0,NDEMOS                                                        
         SLL   R0,1                                                             
         STC   R0,NEXDEMS          (NUMBER OF EXTRACT DEMOS)                    
         SRL   R0,1                                                             
         LA    R2,AVDEMOS                                                       
         LA    R3,EXDEMOS                                                       
         SPACE 1                                                                
IN32     MVC   0(3,R3),0(R2)       ASSUME NORMAL (NO SECOND DEMO)               
         MVC   3(3,R3),=X'00C9FF'                                               
         CLI   1(R2),C'S'                                                       
         BNE   IN40                                                             
         SPACE 1                                                                
IN36     CLI   SHARCALC,C'Y'       IF OPTION IS SET ON                          
         BNE   IN40                                                             
         MVI   1(R3),C'R'          SHARE BECOMES RATINGS                        
         MVI   4(R3),C'P'          / PUTS                                       
         MVC   5(1,R3),2(R3)                                                    
         SPACE 1                                                                
IN40     LA    R2,3(R2)                                                         
         LA    R3,6(R3)                                                         
         BCT   R0,IN32                                                          
         MVI   0(R3),X'FF'                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* CONTROL MARKET GROUP READING                                                  
* ON ENTRY R2 POINTS TO REQUESTED MKTGRP                                        
* ON EXIT, SIDMKT IS NEXT MARKET TO PROCESS, ELSE X'0000'                       
         SPACE 1                                                                
NEXTMGR  NMOD1 0,**NMGR                                                         
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         XC    SIDMKT,SIDMKT                                                    
         OC    SVMGRKEY,SVMGRKEY   TEST FIRST TIME                              
         BNZ   NMGR12                                                           
         SPACE 1                                                                
* READ MGRDEF RECORD *                                                          
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+8(1),0(R2)      MGRPID                                       
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLI   DMCB+8,0                                                         
         BNE   *+14                                                             
         CLC   KEY(13),KEYSAVE     TEST KEY FOUND                               
         BE    *+6                                                              
         DC    H'0'                DIE ON ERRORS                                
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 =A(EXTMGR),DMCB,(RC)    GET TITLES & BREAK LENGTHS               
*                                                                               
NMGR2    XC    KEY,KEY             READ PASSIVE MKTGRP POINTERS                 
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+8(1),0(R2)                                                   
*                                                                               
         CLI   1(R2),C' '          TEST MARKET GROUP FILTER                     
         BNH   NMGR10                                                           
         MVC   WORK(4),1(R2)                                                    
         LA    R1,WORK                                                          
         LA    R0,4                                                             
NMGR8    CLI   0(R1),C'0'                                                       
         BNL   *+8                                                              
         MVI   0(R1),C'0'                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,NMGR8                                                         
         PACK  DUB(3),WORK(5)                                                   
         MVC   KEY+9(2),DUB                                                     
*                                                                               
NMGR10   GOTO1 HIGH                GET FIRST MKTGRP POINTER                     
         B     NMGR16                                                           
*                                                                               
NMGR12   MVC   KEY,SVMGRKEY        RESTORE PREVIOUS KEY                         
         GOTO1 HIGH                AND REREAD                                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NMGR14   GOTO1 SEQ                 GET NEXT MARKET LIST RECORD                  
*                                                                               
NMGR16   CLC   KEY(9),KEYSAVE                                                   
         BNE   NMGRX               EXIT IF ALL DONE                             
         MVC   SVMGRKEY,KEY        SAVE KEY OF THIS RECORD                      
*                                                                               
         CLI   1(R2),C'0'          TEST MARKET GROUP NUMBER PRESENT             
         BL    NMGR20                                                           
         LA    R1,4(R2)            POINT TO LAST BYTE OF MKTGRP                 
         LA    RE,3                                                             
NMGR18   CLI   0(R1),C'0'          FILTER ON MARKET GROUP NUMBER                
         BNL   *+10                                                             
         BCTR  R1,0                                                             
         BCT   RE,NMGR18                                                        
         UNPK  DUB,KEY+9(3)                                                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   1(0,R2),DUB+3                                                    
         BNE   NMGRX               EXIT IF NO MATCH                             
*                                                                               
NMGR20   DS    0H                                                               
         MVC   SIDMKT,SVMGRKEY+11  SET MARKET NUMBER FROM KEY                   
         SPACE 1                                                                
* NOW SET GROUP CODES                                                           
         SPACE 1                                                                
         MVC   WORK(1),0(R2)            GROUP ID                                
         UNPK  WORK+1(5),SVMGRKEY+9(3)  GROUP NUMBER                            
*                                                                               
NMGR22   MVC   MGR1,SPACES                                                      
         LA    R1,MGR1                                                          
         IC    RE,MGR1LEN          GET BREAK LENGTH                             
         EX    RE,NMGRMVC                                                       
*                                                                               
         MVC   MGR2,SPACES                                                      
         LA    R1,MGR2                                                          
         IC    RE,MGR2LEN                                                       
         EX    RE,NMGRMVC                                                       
*                                                                               
         MVC   MGR3,SPACES                                                      
         LA    R1,MGR3                                                          
         IC    RE,MGR3LEN                                                       
         EX    RE,NMGRMVC                                                       
*                                                                               
         MVC   SORTMGR1,MGR1                                                    
         MVC   SORTMGR2,MGR2                                                    
         MVC   SORTMGR3,MGR3                                                    
*                                                                               
NMGRX    XIT1                                                                   
*                                                                               
NMGRMVC  MVC   0(0,R1),WORK        MOVES ID + GROUP NUM                         
         LTORG                                                                  
         EJECT                                                                  
* ROUTINE EXTRACTS MKTGRP TITLES/BREAK LENGTHS FROM MGRDEF RECORD               
* OR CODES/NAMES FROM MKTGRP RECORDS                                            
         SPACE 1                                                                
EXTMGR   NMOD1 0,**EXTMGR                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R6,AIO                                                           
         USING MKGRECD,R6                                                       
*                                                                               
         MVC   WORK(1),MKGKMID                                                  
         UNPK  WORK+1(5),MKGKMGRP(3)                                            
*                                                                               
         LA    R6,24(R6)                                                        
*                                                                               
EXTMG2   CLI   0(R6),X'01'                                                      
         BNE   EXTMG10                                                          
         USING MKGEL01,R6                                                       
         MVC   MGR1BK,MKGBK1       BREAK NAMES                                  
         MVC   MGR2BK,MKGBK2                                                    
         MVC   MGR3BK,MKGBK3                                                    
* SET BREAK LENGTHS                                                             
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         IC    R4,MKGBK1LN                                                      
         STC   R4,MGR1LEN                                                       
         IC    R5,MKGBK2LN                                                      
         AR    R4,R5                                                            
         STC   R4,MGR2LEN                                                       
         IC    R5,MKGBK3LN                                                      
         AR    R4,R5                                                            
         STC   R4,MGR3LEN                                                       
         B     EXTMGX                                                           
         SPACE 1                                                                
EXTMG10  CLI   0(R6),X'10'                                                      
         BNE   EXTMG14                                                          
         USING MKGEL10,R6                                                       
*                                                                               
         MVC   MGR1NM,MKGNAM1      GROUP NAMES                                  
         MVC   MGR2NM,MKGNAM2                                                   
         MVC   MGR3NM,MKGNAM3                                                   
*                                                                               
EXTMG12  MVC   MGR1,SPACES                                                      
         LA    R1,MGR1                                                          
         ZIC   RE,MGR1LEN                                                       
         EX    RE,EXTMGMVC                                                      
*                                                                               
         MVC   MGR2,SPACES                                                      
         LA    R1,MGR2                                                          
         ZIC   RE,MGR2LEN                                                       
         EX    RE,EXTMGMVC                                                      
*                                                                               
         MVC   MGR3,SPACES                                                      
         LA    R1,MGR3                                                          
         ZIC   RE,MGR3LEN                                                       
         EX    RE,EXTMGMVC                                                      
         SPACE 2                                                                
EXTMG14  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   EXTMG2                                                           
EXTMGX   XIT1                                                                   
         SPACE 1                                                                
EXTMGMVC MVC   0(0,R1),WORK                                                     
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
*                                                                  *            
* FORMAT A ROW VALUE - 4(R1) IS OUTPUT ADDRESS                     *            
*                                                                  *            
********************************************************************            
         SPACE 1                                                                
ROWS     NMOD1 0,**ROWS                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R5,4(R1)                                                         
*                                                                               
         ZIC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,ROWBRNCH(R1)                                                  
         BR    R1                                                               
         SPACE 1                                                                
ROWBRNCH B     ROW2                STATION                                      
         B     ROW2                ...                                          
         B     ROW4                BOOK                                         
         B     ROW8                WEEK                                         
         B     ROW6                MONTH                                        
         B     ROW10               QUARTER                                      
         B     ROW12               YEAR                                         
         B     ROW14               PROGRAM                                      
         B     ROW16               DAY                                          
         B     ROW18               TIME                                         
         B     ROW21               ...                                          
         B     ROW20               RANK                                         
         B     ROW19               ...                                          
         B     ROW22               FILTERS                                      
         B     ROW24               DATE                                         
         B     ROW26               LENGTH                                       
         B     ROW28               DAYPART                                      
         B     ROW28               PROGTYP                                      
         B     ROW30               BUYING PERIOD                                
         B     ROW32               MARKET NAME                                  
         B     ROW34               UPGRADE EXPRESSION                           
         B     ROW36               EFFECTIVE DATE                               
         B     ROWX                MULTI                                        
         B     ROW38               MARKET GROUP                                 
         B     ROW40               COMMENT                                      
         B     ROW42               MARKET NUMBER (FOR MKTGRPS)                  
         SPACE 1                                                                
ROW2     MVC   0(10,R5),0(R4)      STATION/NETWORK                              
         MVC   SORTSTAT,0(R4)                                                   
         B     ROWX                                                             
         SPACE 1                                                                
ROW4     CLI   EBOPT,C'Y'                                                       
         BNE   ROW4X                                                            
         GOTO1 =A(EDITUP),DMCB,(RC)                                             
         B     ROWX                                                             
         SPACE 1                                                                
ROW4X    TM    1(R4),X'20'         CHECK FOR ESTIMATED BOOK                     
         BNO   *+8                                                              
         MVI   0(R5),C'E'          AND LABEL OUTPUT                             
         LA    R4,2(R4)                                                         
         SPACE 1                                                                
ROW6     EDIT  (1,(R4)),(2,4(R5))  EDIT YEAR                                    
         MVC   6(1,R5),2(R4)       MOVE SURVEY TYPE                             
         OI    6(R5),X'40'                                                      
*                                                                               
         ZIC   R1,1(R4)                                                         
         BCTR  R1,R0                                                            
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   1(3,R5),0(R1)                                                    
         CLI   1(R4),X'FF'         ESTIMATED                                    
         BNE   ROWX                                                             
         MVC   1(3,R5),=C'EST'                                                  
         B     ROWX                                                             
         SPACE 1                                                                
ROW8     MVC   0(4,R5),0(R4)       WEEKS                                        
         B     ROWX                                                             
         SPACE 1                                                                
ROW10    ZIC   R1,1(R4)            QUARTER                                      
         SLL   R1,2                                                             
         LA    R1,QUARTS(R1)                                                    
         MVC   0(4,R5),0(R1)                                                    
         EDIT  (1,(R4)),(2,5(R5))                                               
         B     ROWX                                                             
         SPACE 1                                                                
ROW12    MVC   0(2,R5),=C'19'      YEAR                                         
         EDIT  (1,(R4)),(2,2(R5))                                               
         B     ROWX                                                             
         SPACE 1                                                                
ROW14    BAS   RE,SPOTPOUT                                                      
         MVC   0(16,R5),0(R4)      PROGRAM                                      
         B     ROWX                                                             
         SPACE 1                                                                
ROW16    CLI   0(R4),X'FF'         TEST REP DAY CODE = ALL                      
         BNE   *+14                                                             
         MVC   2(3,R5),=C'ALL'                                                  
         B     ROWX                                                             
*                                                                               
         GOTO1 UNDAY,DMCB,1(R4),WORK  PRINT SPOT DAY CODE                       
         MVC   0(7,R5),WORK                                                     
         B     ROWX                                                             
         SPACE 1                                                                
ROW18    LH    R1,0(R4)            CONVERT BACK EARLY AM                        
         MVC   DUB(4),0(R4)                                                     
         CH    R1,=H'2400'                                                      
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         STH   R1,DUB                                                           
         GOTO1 UNTIME,DMCB,DUB,(R5)                                             
         OC    DUB(4),DUB                                                       
         BNZ   ROWX                                                             
         OC    UPOPT,UPOPT                                                      
         BZ    ROWX                                                             
         MVC   0(5,R5),=CL5'ALL'                                                
         B     ROWX                                                             
         SPACE 1                                                                
ROW19    DC    H'0'                                                             
         SPACE 1                                                                
ROW20    OC    MYAPRANK,MYAPRANK                                                
         BNZ   ROWX                                                             
         ST    R5,MYAPRANK         RANK - SAVE ADDRESS FOR LATER                
         B     ROWX                                                             
         SPACE 1                                                                
ROW21    DC    H'0'                                                             
         SPACE 1                                                                
ROW22    MVC   1(4,R5),0(R4)       FILTERS                                      
         B     ROWX                                                             
         SPACE 1                                                                
ROW24    GOTO1 DATCON,DMCB,(0,(R4)),(8,WORK)                                    
         MVC   0(5,R5),WORK        DATE                                         
         B     ROWX                                                             
         SPACE 1                                                                
ROW26    L     R1,0(R4)            LENGTH                                       
         MH    R1,=H'15'                                                        
         EDIT  (R1),(4,(R5))                                                    
         B     ROWX                                                             
         SPACE 1                                                                
ROW28    MVC   0(7,R5),0(R4)       DAYPART/PROGTYP                              
         B     ROWX                                                             
         SPACE 1                                                                
ROW30    MVC   2(1,R5),0(R4)       BUYING PERIOD                                
         OI    2(R5),X'F0'                                                      
         CLI   PPROF1,C'O'         TEST OLDSID                                  
         BE    ROWX                                                             
         MVC   1(4,R5),1(R4)       PERIOD DESC                                  
         B     ROWX                                                             
         SPACE 1                                                                
ROW32    OC    0(16,R4),SPACES     OTHERWISE CHOPPER GETS FUCKED                
         GOTO1 CHOPPER,DMCB,(16,(R4)),(16,(R5)),1                               
         B     ROWX                                                             
         SPACE 1                                                                
ROW34    MVC   0(16,R5),0(R4)                                                   
         GOTO1 SQUASHER,DMCB,(R5),16                                            
         B     ROWX                                                             
         SPACE 1                                                                
ROW36    OC    0(3,R4),0(R4)                                                    
         BZ    ROWX                                                             
         GOTO1 DATCON,DMCB,(3,0(R4)),(4,(R5))                                   
         B     ROWX                                                             
         SPACE 1                                                                
ROW38    DS    0H                  MARKET GROUP                                 
         MVC   SORTMGR1(15),0(R4)  MOVE MGR1/2/3                                
         CLC   MGR3,SORTMGR3       TEST FOR CHANGE                              
         BE    ROWX                NO                                           
* MUST READ NEW MKTGRP RECORD                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+8(1),SORTMGR3   MKTGRP ID                                    
         PACK  DUB,SORTMGR3+1(5)                                                
         MVC   KEY+9(2),DUB+5                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         GOTO1 =A(EXTMGR),DMCB,(RC)    GET CODES AND NAMES                      
         MVI   FORCEHED,C'Y'                                                    
         B     ROWX                                                             
         SPACE 1                                                                
ROW40    DS    0H                  FORMAT COMMENT                               
         OC    SORTCMT,SPACES                                                   
         GOTO1 CHOPPER,DMCB,(58,SORTCMT),(29,WORK),2,0                          
         MVC   0(29,R5),WORK                                                    
         CLI   EBOPT,C'Y'          EB ALREADY USES 4 LINES                      
         BE    ROWX                                                             
         MVC   132(29,R5),WORK+29                                               
         B     ROWX                                                             
         DC    H'0'                                                             
         SPACE 1                                                                
ROW42    CLC   SORTMKT,0(R4)       TEST NEW MKT (FOR MKTGRPS)                   
         BE    ROWX                                                             
         MVC   SORTMKT,0(R4)       SAVE NEW MARKET CODE                         
         MVI   FORCEHED,C'Y'       SET TO PRINT HEADLINES                       
         B     ROWX                                                             
         SPACE 1                                                                
ROWX     XIT1                                                                   
         SPACE 1                                                                
* SUBROUTINE TO EXTRACT PROGRAM NAME FROM BUFFER *                              
         SPACE 1                                                                
SPOTPOUT NTR1                                                                   
         CLI   COMPOPT,C'Y'        IF COMPRESSION IS ON                         
         BNE   SPOTPOX                                                          
         L     R2,APRGBUFF                                                      
         SPACE 1                                                                
SPOTPO2  CLI   0(R2),0                                                          
         BE    SPOTPOX                                                          
         CLI   0(R2),X'FF'                                                      
         BE    SPOTPOX                                                          
         CLC   0(16,R4),0(R2)      LOOK FOR A MATCH ON KEY                      
         BE    SPOTPO4                                                          
         LA    R2,32(R2)                                                        
         B     SPOTPO2                                                          
         SPACE 1                                                                
SPOTPO4  MVC   0(16,R4),16(R2)     MOVE OUT NAME                                
*                                                                               
SPOTPOX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT UPGRADE EXPRESSION INTO PRINTABLE FORMAT          *         
*                                                                     *         
*                                                                     *         
* NTRY - R4=A(EXPRESSION) - FILE/UPGRADE/FROM BOOK                    *         
*        R5=OUTPUT ADDRESS                                            *         
*                                                                     *         
* SEQ TYP BK  BK SRC UPG UPG UPG UPG UPG UPG UPG UPG FRB FRB          *         
*   0   1  2   3   4   5   6   7   8   9  10  11  12  13  14          *         
***********************************************************************         
         SPACE 1                                                                
EDITUP   NMOD1 0,EDITUP**                                                       
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         TM    1(R4),X'20'         TEST ESTIMATE BOOK                           
         BO    EDITUP1                                                          
         LA    R4,2(R4)                                                         
         MVC   0(5,R5),=C'BOOK='   SHOW BOOK IF NOT AN ESTIMATE                 
         LA    R5,5(R5)                                                         
         BAS   RE,EDITBK                                                        
         B     EDITUPX                                                          
*                                                                               
EDITUP1  OC    5(8,R4),5(R4)       TEST UPGRADE GIVEN                           
         BNZ   EDITUP2                                                          
         MVC   0(5,R5),=C'FILE='   SHOW SOURCE FILE IF NO UPGRADE               
         MVC   5(1,R5),4(R4)                                                    
         LA    R5,6(R5)                                                         
         B     EDITUP16                                                         
*                                                                               
EDITUP2  MVC   0(4,R5),=C'UPX='    EDIT UPGRADE EXPRESSION                      
         MVC   2(1,R5),4(R4)                                                    
         LA    R1,UPTAB            R1=A(UPGRADE TYPE TABLE)                     
*                                                                               
EDITUP4  CLI   0(R1),0             TEST E-O-T                                   
         BE    EDITUP6             YES - MUST BE DEMO UPGRADE                   
         CLC   5(2,R4),0(R1)       MATCH SPUPTYPE/SPUPSTYP                      
         BE    *+12                                                             
         LA    R1,L'UPTAB(R1)                                                   
         B     EDITUP4                                                          
         MVC   4(6,R5),2(R1)       MOVE OUT UPGRADE TYPE NAME                   
         B     EDITUP8                                                          
*                                                                               
EDITUP6  XC    DUB(4),DUB          DEMO UPGRADE                                 
         MVC   DUB+1(2),5(R4)                                                   
         MVI   DUB+3,X'FF'                                                      
         GOTO1 DEMOCON,DMCB,(1,DUB),(6,4(R5)),(C'S',DBLOCK)                     
*                                                                               
EDITUP8  LA    R5,10(R5)           FIND END OF UPGRADE NAME                     
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C'/'          ATTACH DELIMITER                             
         LA    R5,2(R5)                                                         
*                                                                               
         LA    R4,7(R4)            POINT TO FIRST UPGRADE VALUE                 
         LA    R3,3                R3=NUMBER OF VALUES                          
EDITUP10 OC    0(2,R4),0(R4)       TEST VALUE PRESENT                           
         BZ    EDITUP14                                                         
         CLC   0(2,R4),=H'500'     TEST BOOK OR VALUE                           
         BNH   *+12                                                             
         BAS   RE,EDITBK           FIELD IS A BOOK                              
         B     EDITUP12                                                         
         SR    R0,R0               FIELD IS A VALUE                             
         ICM   R0,3,0(R4)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R5),DUB                                                      
         CLI   0(R5),C'0'                                                       
         BNE   EDITUP12                                                         
         MVC   0(4,R5),1(R5)                                                    
         B     *-14                                                             
EDITUP12 LA    R5,6(R5)            FIND END OF EDITTED VALUE                    
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C'/'          ATTACH DELIMITER                             
         LA    R5,2(R5)                                                         
*                                                                               
EDITUP14 LA    R4,2(R4)            BUMP TO NEXT VALUE                           
         BCT   R3,EDITUP10         DO FOR NUMBER OF VALUES                      
         BCTR  R5,0                                                             
*                                                                               
EDITUP16 MVC   0(4,R5),=C',BK='    EDIT FROM BOOK                               
         LA    R5,4(R5)                                                         
         BAS   RE,EDITBK                                                        
*                                                                               
EDITUPX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT BOOK VALUE INTO PRINTABLE FORMAT                  *         
*                                                                     *         
* NTRY - R4=A(BOOK VALUE), R5=A(OUTPUT)                               *         
* EXIT - R5=A(NEXT AVAILABLE SPACE)                                   *         
***********************************************************************         
         SPACE 1                                                                
EDITBK   ZIC   R1,1(R4)            R1=MONTH                                     
         BCTR  R1,0                                                             
         LA    R0,L'MONTAB                                                      
         MR    R0,R0                                                            
         LA    R1,MONTAB(R1)                                                    
         MVC   0(L'MONTAB,R5),0(R1)                                             
         LA    R1,L'MONTAB-1(R5)                                                
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         ZIC   R0,0(R4)            R0=YEAR                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(2,R1),DUB                                                      
         BR    RE                                                               
         SPACE 1                                                                
UPTAB    DS    0XL8                SPUPTYPE/SPUTSTYP/UPGRADE NAME               
         DC    AL1(SPUPTRTG),X'0',C'RATING'                                     
         DC    AL1(SPUPTHUT),X'0',C'HUT   '                                     
         DC    AL1(SPUPTPUT),C'P',C'PUT   '                                     
         DC    AL1(SPUPTNDX),X'0',C'INDEX '                                     
         DC    AL1(SPUPTHPT),X'0',C'HPT   '                                     
         DC    X'00'                                                            
         SPACE 1                                                                
MONTAB   DS    0CL3                PRINTABLE MONTH VALUES                       
         DC    C'JANF  MARAPRM  JUNJ  AUGS  O  N  D  '                          
         LTORG                                                                  
         EJECT                                                                  
* WORK AREA *                                                                   
         SPACE 1                                                                
RESWK    DS    0D                                                               
         DC    CL8'**RESWK*'                                                    
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,NNN,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=NNN'                                   
DAYLIST  DC    C'M-FMONTUEWEDTHUFRISATSUNM-SVARS-S'                             
SPOTDAYS DC    X'7C402010080402017F0003FF'                                      
QUARTS   DC    C'1ST.2ND.3RD.4TH.'                                              
ANYSORT  DC    C'N'                                                             
ANYUP    DC    C'N'                                                             
SIDOVER  DC    A(0)                                                             
SVMGRKEY DS    XL13                                                             
XQHNUM   DS    XL1                                                              
NEXTPROG DS    CL16                                                             
         SPACE 1                                                                
MGR1LEN  DS    CL1                 MARKET GROUP LEVEL 1 LENGTH                  
MGR2LEN  DS    CL1                 MARKET GROUP LEVEL 2 LENGTH (1+2)            
MGR3LEN  DS    CL1                 MARKET GROUP LEVEL 3 LENGTH (1+2+3)          
         SPACE 1                                                                
MGR1BK   DS    CL12                MARKET GROUP LEVEL 1 BREAK NAME              
         DS    CL1                                                              
MGR1     DS    CL5                 MARKET GROUP LEVEL 1 CODE                    
         DS    CL1                                                              
MGR1NM   DS    CL24                MARKET GROUP LEVEL 1 GROUP NAME              
         SPACE 1                                                                
MGR2BK   DS    CL12                MARKET GROUP LEVEL 2 BREAK NAME              
         DS    CL1                                                              
MGR2     DS    CL5                 MARKET GROUP LEVEL 2 CODE                    
         DS    CL1                                                              
MGR2NM   DS    CL24                MARKET GROUP LEVEL 2 GROUP NAME              
         SPACE 1                                                                
MGR3BK   DS    CL12                MARKET GROUP LEVEL 3 BREAK NAME              
         DS    CL1                                                              
MGR3     DS    CL5                 MARKET GROUP LEVEL 3 CODE                    
         DS    CL1                                                              
MGR3NM   DS    CL24                MARKET GROUP LEVEL 3 GROUP NAME              
         SPACE 1                                                                
MTABPARS DC    A(0)                                                             
         DC    A(MKTTAB)                                                        
MTABCNT  DC    F'0'                                                             
         DC    A(28)                                                            
         DC    AL1(0),AL3(4)                                                    
         DC    A(12000/28)                                                      
         EJECT                                                                  
AHOOK    DC    A(HOOK)             A-TYPES                                      
AHEDSPEC DC    A(HEDSPECS)                                                      
ADTALIST DC    A(DATALIST)                                                      
AGETSORT DC    A(GETSORT)                                                       
AUPBLOCK DC    A(UPBLOCK)                                                       
AEBDREC  DC    A(EBDREC)                                                        
ASIDREC  DC    A(SIDREC)                                                        
ASIDBUFF DC    A(SIDBUFF)                                                       
APRGBUFF DC    A(PROGBUFF)                                                      
ADISPTAB DS    A                                                                
         SPACE 1                                                                
BOOKFLD  EQU   3                   FIELD NUMBER FOR BOOK FIELD                  
* X'80' IN EB COLUMN MEANS ALWAYS PRINT THIS DETAIL ON EB REPORT                
         DS    0D                                                               
DATALIST DS    0XL16       DATA LEN              OUT LEN   EB                   
         DC    C'STATION ',AL1(16),AL3(SORTSTAT),AL1(10),X'00',AL2(0)           
         DC    C'NETWORK ',AL1(06),AL3(SORTSTAT),AL1(07),X'00',AL2(0)           
         DC    C' BOOK   ',AL1(15),AL3(SORTRQBK),AL1(06),X'80',AL2(0)           
         DC    C'WEEK    ',AL1(07),AL3(SORTWEEK),AL1(05),X'00',AL2(0)           
         DC    C'MONTH   ',AL1(02),AL3(SORTMNTH),AL1(06),X'00',AL2(0)           
         DC    C'QUARTER ',AL1(02),AL3(SORTQURT),AL1(07),X'00',AL2(0)           
         DC    C'YEAR    ',AL1(01),AL3(SORTYEAR),AL1(04),X'00',AL2(0)           
         DC    C'PROGRAM ',AL1(16),AL3(SORTPROG),AL1(16),X'80',AL2(0)           
         DC    C'  DAY   ',AL1(02),AL3(SORTDAY),AL1(07),X'00',AL2(0)            
         DC    C'TIME    ',AL1(04),AL3(SORTTIME),AL1(11),X'00',AL2(0)           
         DC    C'CODE    ',AL1(06),AL3(SORTCODE),AL1(06),X'00',AL2(0)           
         DC    C'RANK    ',AL1(04),AL3(SORTRANK),AL1(05),X'00',AL2(0)           
         DC    C'NTI     ',AL1(04),AL3(SORTNTI),AL1(05),X'00',AL2(0)            
         DC    C'FILTER  ',AL1(04),AL3(SORTFILT),AL1(6),X'00',AL2(0)            
         DC    C'DATE    ',AL1(06),AL3(SORTDATE),AL1(5),X'00',AL2(0)            
         DC    C'LENGTH  ',AL1(04),AL3(SORTQHRS),AL1(6),X'00',AL2(0)            
         DC    C'DAYPART ',AL1(07),AL3(SORTDPT),AL1(7),X'00',AL2(0)             
         DC    C'PRGTYP  ',AL1(07),AL3(SORTTYP),AL1(7),X'00',AL2(0)             
         DC    C'PERIOD  ',AL1(05),AL3(SORTPER),AL1(6),X'00',AL2(0)             
         DC    C'MARKET  ',AL1(16),AL3(SORTMKT),AL1(16),X'00',AL2(0)            
         DC    C'UPGRADE ',AL1(16),AL3(SORTUPDT),AL1(16),X'00',AL2(0)           
         DC    C'EFFECT  ',AL1(03),AL3(SORTEFF),AL1(6),X'00',AL2(0)             
         DC    C'MULTI   ',AL1(00),X'00',AL2(0),AL1(0),X'00',AL2(0)             
         DC    C'MKTGRP  ',AL1(15),AL3(SORTMGR1),AL1(0),X'00',AL2(0)            
         DC    C'COMMENT ',AL1(00),X'00',AL2(0),AL1(30),X'80',AL2(0)            
*              FOLLOWING SPEC ONLY USED WHEN MKTGRPS ACTIVE                     
         DC    C'MGRPMKT ',AL1(04),AL3(SORTMKT),AL1(0),X'00',AL2(0)             
         SPACE 1                                                                
SORTMGR1 DS    CL5                                                              
SORTMGR2 DS    CL5                                                              
SORTMGR3 DS    CL5                                                              
SORTMKT  DS    CL16                                                             
SORTSTAT DS    CL16                                                             
SORTRQBK DS    CL2                 BK SEQ/BK TYPE                               
SORTBOOK DS    CL13                BK/UPGRADE (EB ONLY)                         
SORTDAY  DS    CL2                 REP DAY CODE/SPOT DAY CODE                   
SORTTIME DS    F                                                                
SORTRANK DC    F'0'                                                             
SORTQHRS DS    F                                                                
SORTPROG DS    CL16                                                             
SORTUPDT DS    CL16                                                             
SORTCODE DS    CL6                                                              
SORTMNTH DS    CL2                                                              
SORTQURT DS    CL2                                                              
SORTQ2   DS    CL2                                                              
SORTYEAR DS    CL1                                                              
SORTWEEK DS    CL7                                                              
SORTFILT DS    CL4                                                              
SORTDATE DS    CL6                                                              
SORTDP   DS    CL5                                                              
SORTDPT  DS    CL7                                                              
SORTTYP  DS    CL7                                                              
SORTPER  DS    CL5                 FOR NEWSID 1 BYTE NUM/4 ALPHA                
SORTEFF  DS    CL3                                                              
SORTFAD  DS    CL2                                                              
SORTNUM  DS    CL2                                                              
SORTNTI  DS    0C                                                               
*                                                                               
IUNSORT  DS    CL1                                                              
TRIM     DS    CL1                                                              
COLUMNS  DS    CL8                                                              
AVSW     DS    CL1                                                              
SORTFRST DS    CL1                                                              
ANYRANK  DS    CL1                                                              
LDEMCNT  DS    CL1                                                              
EQSW     DS    CL1                                                              
MONDATE  DS    CL6                                                              
DAYNUM   DS    CL1                                                              
ACTNDEM  DS    CL1                                                              
AVCOUNT  DS    CL1                                                              
NEXDEMS  DS    CL1                                                              
MINMAX   DS    CL1                                                              
MULT     DS    CL1                                                              
THISBOOK DS    CL3                                                              
THISSIDP DS    CL1                                                              
HEADP    DS    CL132                                                            
*                                                                               
PROGKEY  DS    0CL16                                                            
PROGKSTA DS    CL5                                                              
PROGKDAY DS    CL1                                                              
PROGKTIM DS    CL4                                                              
PROGKBK  DS    CL2                                                              
         DS    CL4                                                              
*                                                                               
MYRANK   DS    F                                                                
MYTIE    DS    F                                                                
MYVALUE  DS    F                                                                
MYAPRANK DS    A                                                                
SAVECNT  DS    F                                                                
SAVFUNCT DS    CL1                                                              
SHARCALC DS    CL1                                                              
SVSIDPER DS    CL6                                                              
         SPACE 1                                                                
RELO     DS    A                                                                
SAVER5   DS    A                                                                
SIDIO    DS    A                                                                
BOOKDISP DS    A                                                                
DEMDISP  DS    A                                                                
ARANK    DS    A                                                                
RANK     DS    F                                                                
DISP     DS    F                                                                
LSORT    DS    F                                                                
NUMCOLS  DS    F                                                                
COST     DS    F                                                                
HUT      DS    H                                                                
         DS    0D                                                               
         DC    CL8'AVDEMOS'                                                     
AVDEMOS  DS    CL256                                                            
         DS    0D                                                               
         DC    CL8'UPDEMOS'                                                     
UPDEMOS  DS    CL256                                                            
         DS    0D                                                               
         DC    CL8'EXDEMOS'                                                     
EXDEMOS  DS    CL256                                                            
SOUTLEN  DS    F                   LLLL.... TO PRECEDE VAR LEN RECORD           
MINTWO   DS    F                                                                
MAXTWO   DS    F                                                                
NLINES   DS    F                                                                
NXTP     DS    A                                                                
NXTHEADP DS    A                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*SORTREC'                                                    
SORTLEN  DS    CL4                 ALLOW SPACE FOR LENGTH                       
SORTREC  DS    0C                  *** START OF FIXED LEN SORTREC ***           
SORTKEY  DS    CL164                                                            
         ORG   *-4                                                              
SORTKMKT DS    CL2                 TO GUARANTEE SINGLE MKT DATA                 
SORTKID  DS    CL2                 I=IUNREC,U=UPGRADE (U SORTS LAST)            
*                                                                               
SORTCMT  DS    CL60                COMMENT                                      
*                                                                               
SORTDMOV DS    CL16                DEMO OVERRIDE FLAGS                          
*                                                                               
SORTXTRA DS    0CL32                                                            
SORTCNT  DS    CL4                                                              
SORTWT   DS    CL4                                                              
SORTWCST DS    CL4                                                              
         DS    CL20                                                             
*                                                                               
SORTDEMS DS    1024C                                                            
         ORG   SORTDEMS+IUNLEN-12                                               
SORTVUTS DS    CL12                                                             
SORTRECL EQU   *-SORTREC                                                        
         ORG                                                                    
         SPACE 1                                                                
LASTREC  DS    0C                                                               
LASTKEY  DS    CL164                                                            
LASTCMT  DS    CL60                                                             
LASTDMOV DS    CL16                DEMO OVERRIDE FLAGS                          
LASTXTRA DS    CL32                                                             
LASTDEMS DS    1024C                                                            
LASTRECL EQU   *-LASTREC                                                        
         SPACE 1                                                                
NEXTREC  DS    0C                                                               
NEXTKEY  DS    CL164                                                            
NEXTCMTG DS    CL60                                                             
NEXTDMOV DS    CL16                                                             
NEXTXTRA DS    CL32                                                             
NEXTDEMS DS    1024C                                                            
NEXTRECL EQU   *-NEXTREC                                                        
       ++INCLUDE SPRESWORKD                                                     
         EJECT                                                                  
         ORG CONTAGH                                                            
       ++INCLUDE SPRESD5D                                                       
         EJECT                                                                  
       ++INCLUDE SPRES95WRK                                                     
         EJECT                                                                  
********************************************                                    
* DOWNLOAD ROUTINE                                                              
*                                                                               
*                                                                               
DOWN     CSECT                                                                  
         NMOD1 0,**DOWNLD                                                       
*                                                                               
         LA    R4,DOWNWORK                                                      
         USING DLCBD,R4                                                         
         LA    R2,PRNTLN+1                                                      
         ST    R2,DLCBAPL          PRINT LINE                                   
         LA    R2,DOWNHOOK                                                      
         ST    R2,DLCBAPR          PRINT ROUTINE                                
*                                                                               
         L     R2,0(R1)            ACTION CODE                                  
         CH    R2,=H'2'                                                         
         BH    DOWN40                                                           
         CH    R2,=H'1'                                                         
         BL    DNLD1ST             0=START OF REPORT                            
         BE    DNTEXT              1=PRINT TEXT FIELD                           
         B     DNNUM               2=PRINT NUMERIC FIELD                        
DOWN40   CH    R2,=H'4'                                                         
         BL    DNLDEOL             3=END OF LINE                                
         BE    DNNODATA            4='NO DATA'                                  
         B     DNLDEND             5=END OF REPORT                              
*                                                                               
*        I/P - R5 CONTAINS ADDRESS OF PRINT LINE FIELD                          
DNNUM    MVI   DLCBTYP,DLCBNUM     TYPE (NUMERIC)                               
         LA    R1,8                HARD CODED 8 (WIDTH OF COLUMN-1)             
         B     DNTEXT10                                                         
*                                                                               
*        I/P - R5 CONTAINS ADDRESS OF PRINT LINE FIELD                          
*              12(1,R3) IS FIELD LENGTH                                         
DNTEXT   MVI   DLCBTYP,DLCBTXT     TYPE (TEXT)                                  
         ZIC   R1,12(R3)           USE COLUMN WIDTH                             
         BCTR  R1,0                                                             
DNTEXT10 MVI   DLCBACT,DLCBPUT     ACTION (PUT)                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),SPACES      TEST IF FIELD IS ALL SPACES                  
         BNE   DNTEXT30                                                         
         LA    R1,1                TRANSMIT MINIMUM LENGTH FIELD                
         CLI   DLCBTYP,DLCBNUM     TEST TYPE (NUMERIC)                          
         BNE   DNTEXT80            TRANSMIT ONE SPACE (TEXT)                    
         MVI   0(R5),C'0'                                                       
         B     DNTEXT80            TRANSMIT ONE ZERO (NUMERIC)                  
*                                                                               
DNTEXT30 LA    R6,0(R1,R5)         R6 - LAST BYTE OF FIELD                      
         LA    R1,1(R1)                                                         
DNTEXT40 CLI   0(R6),C' '          SCAN FOR TRAILING SPACES                     
         BNE   DNTEXT50                                                         
         BCTR  R6,0                                                             
         BCT   R1,DNTEXT40                                                      
         DC    H'0'                                                             
*                                                                               
DNTEXT50 CLI   DLCBTYP,DLCBTXT     TEST TYPE (TEXT)                             
         BE    DNTEXT80                                                         
         CLI   0(R5),C' '          SCAN FOR LEADING SPACES IN NUMERIC           
         BNE   DNTEXT80                                                         
         LA    R5,1(R5)                                                         
         BCT   R1,DNTEXT50                                                      
         DC    H'0'                                                             
DNTEXT80 STC   R1,DLCBLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD,0(R5)       MOVE PRINT LINE DATA TO DATA FIELD           
*                                                                               
DNLINK   LR    R1,R4                                                            
         L     RF,=V(DLFLD)                                                     
         BASR  RE,RF                                                            
*                                                                               
DOWNX    XIT1                                                                   
*                                                                               
         SPACE 3                                                                
DNLD1ST  DS    0H                                                               
         MVI   DLCBACT,DLCBSOR     ACTION (START OF REPORT)                     
         B     DNLINK                                                           
         SPACE 1                                                                
DNLDEND  MVI   DLCBACT,DLCBEOR     ACTION (END OF REPORT)                       
         B     DNLINK                                                           
         SPACE 1                                                                
DNNODATA MVC   DLCBFLD(7),=C'NO DATA'                                           
         MVI   DLCBLEN,7                                                        
         MVI   DLCBACT,DLCBPUT     ACTION (PUT)                                 
         MVI   DLCBTYP,DLCBTXT     TYPE (TEXT)                                  
         LR    R1,R4                                                            
         L     RF,=V(DLFLD)                                                     
         BASR  RE,RF                                                            
*              FALL THRU...        ISSUE END-OF-LINE                            
*                                                                               
DNLDEOL  MVI   DLCBACT,DLCBEOL     ACTION (END OF PRINT LINE)                   
*                                                                               
         LR    R1,R4                                                            
         L     RF,=V(DLFLD)                                                     
         BASR  RE,RF                                                            
         MVI   PRNTLN,X'40'                                                     
         MVC   PRNTLN+1(L'PRNTLN-1),PRNTLN                                      
         B     DOWNX                                                            
*                                                                               
         SPACE 1                                                                
DOWNHOOK NTR1                                                                   
         GOTO1 VPRINT,DMCB,PRNTLN,=C'BL01'                                      
         XIT1                                                                   
*                                                                               
         SPACE 1                                                                
DOWNWORK DS    XL200'00'                                                        
PRNTLN   DC    CL180' '                                                         
*                                                                               
         LTORG                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
*                                                                               
* BUFFER AREAS FOR RESEARCH                                                     
         SPACE 1                                                                
T20F95   CSECT                                                                  
*                                                                               
HEDSPECS SSPEC H1,1,C'MEDIA'                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,RUN                                                        
         SSPEC H5,97,REPORT                                                     
         SSPEC H5,114,PAGE                                                      
         DC    X'00'                                                            
         DS    0D                                                               
         DC    C'**UPBL**'                                                      
UPBLOCK  DC    100X'00'                                                         
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*MKTTAB*'                                                      
MKTTAB   DC    12000X'00'                                                       
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'SAVDBLOK'                                                      
SAVDBLOK DS    XL256                                                            
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*EBDREC*'                                                      
EBDREC   DC    2000X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*SIDREC*'                                                      
SIDREC   DC    2000X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*SIDBUFF'                                                      
SIDBUFF  DS    5XL256                                                           
SIDBUFFX EQU   *                                                                
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'PROGBUFF'                                                      
PROGBUFF DS    128000C                                                          
         DC    X'FF'                                                            
         EJECT                                                                  
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'*IUNREC*'                                                    
IUNREC   DS    1024C               IUN RECORD BUILT HERE                        
*                                                                               
IUNDEMS  EQU   32                                                               
IUNHMDSP EQU   80                                                               
         SPACE 1                                                                
**************** I U N   W O R K   A R E A  ***************                     
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'**IUNWK*'                                                    
IUNWK    DS    0C                                                               
*                                                                               
IUNVS    DS    (IUNDEMS)F                                                       
IUNVX    EQU   *                                                                
*                                                                               
IUNOLD   EQU   *                                                                
IRTGOLD  DS    (IUNDEMS)F                                                       
IIMPOLD  DS    (IUNDEMS)F                                                       
IPUTOLD  DS    (IUNDEMS)F                                                       
ITOTOLD  DS    (IUNDEMS)F                                                       
IUNOLDX  EQU   *                                                                
*                                                                               
IUNLEN   EQU   *-IUNWK+12          UNVS/RTGS/IMPS/PUTS/TOTS + VUTS              
*                                                                               
IUNNEW   EQU   *                                                                
IRTGNEW  DS    (IUNDEMS)F                                                       
IIMPNEW  DS    (IUNDEMS)F                                                       
IPUTNEW  DS    (IUNDEMS)F                                                       
ITOTNEW  DS    (IUNDEMS)F                                                       
IUNNEWX  EQU   *                                                                
*                                                                               
IUNXTRA  EQU   *                                                                
IUNVUT   EQU   *                                                                
IUNSHMS  DS    F                                                                
IUNSMETA DS    F                                                                
IUNSMETB DS    F                                                                
*                                                                               
ILUNVS   DC    (IUNDEMS)F'0'                                                    
IUNWKLEN EQU   *-IUNWK                                                          
         EJECT                                                                  
SRBLKD   DSECT                                                                  
       ++INCLUDE SPRANSIDD                                                      
         SPACE 2                                                                
*DEDEMTABD                                                                      
*DDCOMFACS                                                                      
*SPSIDIOD                                                                       
*SPDEMUPD                                                                       
*DDTWADCOND                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPSIDIOD                                                       
       ++INCLUDE SPDEMUPD                                                       
       ++INCLUDE SPGENMKG                                                       
       ++INCLUDE DDTWADCOND                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053SPRES95   05/01/02'                                      
         END                                                                    
