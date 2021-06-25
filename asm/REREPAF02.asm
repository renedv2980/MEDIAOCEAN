*          DATA SET REREPAF02  AT LEVEL 021 AS OF 05/01/02                      
*PHASE RE1502A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REDOSPL                                                                
*INCLUDE SORTER                                                                 
         TITLE 'REREP1502 - RE1502 - REP BLAIR/KING REPORT'                     
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP1502 - RE1502 - REP BLAIR/KING REPORT               *             
*                             A.K.A. ADVERTISER FORECAST REPORT   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
*  JUN/89-- (SNS) INITIAL RELEASE                                 *             
*                                                                 *             
*  JUL/89- (SNS) ADD SHARE OPTION TO REPORT (INCLUDES             *             
*           CHANGES TO STATION SHARE, MARKET, AND CALC.           *             
*           OF FORECAST AMT (NOW STATION AMT)                     *             
*                                                                 *             
*  AUG16/89 (SNS) -INCLUDE SPL AS RUN CALCULATIONS                *             
*                                                                 *             
*  DEC27/89 (MRR) - CHANGE DATAMANGER CALLS TO DEAL WITH DELETED  *             
*                    CONTRACTS                                    *             
*                                                                 *             
*  SEP04/90 (MRR) --- REQUEST QOPTION4 HAS MOVED TO THE 2ND REQST *             
*                      CARD                                       *             
*                                                                 *             
*  DEC18/91 (BU ) --- ADD SETUPMON FOR OLD FORMAT VALUEMON        *             
*                                                                 *             
*  JUN29/92 (BU ) --- REMOVE BLOCKSIZE FROM INPUT DCB: CAUSING    *             
*                     ABORT WHEN SORTFILE ON A DIFFERENT DEVICE   *             
*                     TYPE                                        *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
RE1502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1502,R9,R8,RR=RE                                           
         L     RA,0(R1)                                                         
         USING ADCONSD,RA                                                       
         LA    RF,FOOTLINE                                                      
         ST    RF,FOOTHOOK                                                      
         DROP  RA                                                               
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         ST    RE,RELO                                                          
*                                                                               
         EJECT                                                                  
         CLI   MODE,REQFRST                                                     
         BNE   GXIT                                                             
*                                                                               
*        GET QOPTION4 FROM THE 2ND REQUEST CARD AND PUT IT IN LOPTION4          
*                                                                               
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
*                                                                               
         L     R2,VXRQNUM                                                       
         CLI   0(R2),2             NEED 2 CARDS.                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,VXRQCARD         POINT TO THE CARDS AREA                      
         MVC   LOPTION4(1),80(R4)                                               
         DROP  R5                                                               
*                                                                               
*        OPEN INPUT DATA FILE                                                   
*                                                                               
         LA    R2,RRGFILE          OPEN INPUT RRG FILE                          
         OPEN  ((R2),(INPUT))                                                   
         SPACE 1                                                                
         LTR   RF,RF               OPEN OK                                      
         BZ    MAIN10                                                           
         ABEND 991                                                              
         SPACE                                                                  
* RRG FILE IS RECEIVED SORTED ON CURRENT BILLING                                
         SPACE                                                                  
MAIN10   EQU   *                                                                
         GOTO1 =A(SETMONDT),DMCB,(RC) SET OLD FORMAT MONTH TABLE                
         BAS   RE,INITIAL                                                       
         BAS   RE,BUILD           READ RRGFILE, BUILD RECORDS                   
         BAS   RE,RNKTBL          CALCULATES THIS YEARS RANK                    
         CLOSE (RRGFILE,)         CLOSE FILE                                    
*                                                                               
         XC    RECCT,RECCT        RESET FILE BLOCK COUNTER                      
         LA    R2,RRGFILE         OPEN INPUT RRG FILE AGAIN                     
         OPEN  ((R2),(INPUT))     SO CAN START AT TOP OF FILE                   
         LTR   RF,RF              OPEN OK                                       
         BZ    MAIN15                                                           
         ABEND 991                                                              
*                                                                               
MAIN15   BAS   RE,RESORT          SORTS FILE TO PRIOR FINAL                     
         BAS   RE,RNKLYR          CALCULATES LAST YEARS RANK                    
         BAS   RE,FINSORT         FINAL SORT FOR PRINTING PURPOSES              
         BAS   RE,GETOFFNM        BUILDS OFFICE TABLE                           
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLOSE (RRGFILE,)         FINAL CLOSE                                   
         MVI   SPACING,2                                                        
         MVI   OFFLTR,0           DO MAIN RPT WITHIN NY REPORT                  
         MVI   NYFLAG,0           DO NY REPORT=MAIN + ALL RECAPS                
         LA    R5,MYAIO           OPEN CONTROL FILE FOR FIRST ID                
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'REP',=C'NCTFILE X',(R5)                 
*                                                                               
         BAS   RE,SETQRTR         CHECK FOR EVEN QRTRS IN REQUEST PER           
         CLI   QOPTION3,C'W'      IF WORKSHEET                                  
         BE    MAIN20             SKIP SPL                                      
         CLI   QUARTNO,0          IF NOT EVEN QUARTER                           
         BE    MAIN20             SKIP                                          
         CLI   LOPTION4,C'Y'      IF NOT SHARE OPTION                           
         BNE   MAIN20             SKIP                                          
*                                                                               
         BAS   RE,DOSTLIST        SET UP STATION LIST FOR SPL AS RUN            
         BAS   RE,CALCSPL         CALCULATE SPL FOR EACH ENTRY                  
*                                                                               
MAIN20   MVC   SVOFF,=C'NY'                                                     
         BAS   RE,SETPRINT        ROUTINE CONTROL REMOTE PRINTING               
         BAS   RE,HEADLINE        PRINT HEADLINE DATA                           
         XC    BUDTOT,BUDTOT      CLEAR TOTALS                                  
         XC    CURBILL,CURBILL                                                  
         XC    PRIBILL,PRIBILL                                                  
         XC    PRIFIN,PRIFIN                                                    
         BAS   RE,PRNTLIN         PRINT REPORT FROM TABLE                       
         BAS   RE,DOTOTALS        PRINT TOTALS                                  
         BAS   RE,FOOTLINE        DO FOOTLINES                                  
*                                                                               
* TWO PASSES THROUGH : FIRST PASS PROCESSING RECAPS FOR MAIN REPORT             
* (AKA NY REPORT PRINTED HERE) NYFLAG=0,OFFLTR=1                                
* SECOND PASS: NYFLAG=1: DO ONLY OFFICE RECAPS WHICH ARE SENT                   
* TO PRINT QUEUE                                                                
         SPACE                                                                  
MAIN25   LA    R5,OFFTABLE        OFFICE TABLE                                  
*                                                                               
MAIN30   CLI   NYFLAG,1           DOING OFFICE RECAP?                           
         BNE   MAIN35             NO-MAIN REPORT+RECAPS                         
*                                                                               
         CLC   0(2,R5),=C'NY'                                                   
         BE    MAIN40             NY HAS RECAP FOR NY ALREADY                   
*                                                                               
MAIN35   MVC   SVOFF,0(R5)        CURRENT OFFICE PRINTING                       
         MVC   SVOFFNM,2(R5)      CURRENT OFFICE NAME                           
         MVI   FORCEHED,C'Y'      EJECT                                         
         MVC   PAGE,=H'1'                                                       
         MVI   OFFLTR,1           FLAG INDICATING W/IN MAIN REPORT              
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         BAS   RE,SETPRINT        ROUTINE CONTROL REMOTE PRINTING               
         BAS   RE,HEADLINE        PRINT HEADLINE DATA                           
*                                                                               
         BAS   RE,PRNTLIN         PRINT REPORT FROM TABLE                       
         BAS   RE,DOTOTALS        DO TOTALS                                     
         BAS   RE,FOOTLINE        DO FOOTLINES                                  
*                                                                               
MAIN40   LA    R5,L'OFFTABLE(R5)                                                
         OC    0(L'OFFTABLE,R5),0(R5)   END OF OFFICE TABLE?                    
         BNZ   MAIN30                                                           
         SPACE                                                                  
         CLI   QACCTOPT,C'R'      DON'T SEND TO PRINT QUEUE                     
         BE    MAINX                                                            
* NOW JUST PROCESS RECAP REPORTS FOR RESPECTIVE OFFICES                         
         CLI   NYFLAG,1                                                         
         BE    MAINX              DID ALL PHASES -EXIT                          
         MVI   NYFLAG,1           JUST DO RECAPS TO PRINT QUEUE                 
         B     MAIN25                                                           
MAINX    DS    0H                                                               
* MUST CLOSE PRINT QUEUE AND CLEAR REMOTE KEY -                                 
* SO LOGO END PAGES WILL NOT GO TO PRINT QUEUE BUT PRINT WITH MAIN              
* REPORT                                                                        
         SPACE                                                                  
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         L     RE,REMOTEC                                                       
         XC    0(18,RE),0(RE)                                                   
GXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
INITIAL  ST    RE,FULL                                                          
         XC    RECCT,RECCT         ZERO RECORD COUNTS                           
         XC    RNKMX,RNKMX                                                      
         XC    BLOCK(192),BLOCK    SET NO REC READ YET                          
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         SPACE                                                                  
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
********************************************************                        
* SETQRTR -----                                                                 
* CHECK IF DATES REQUESTED ARE IN FULL QUARTERS                                 
* IF NOT FULL QUARTERS THEN QUARTNO=0 AND BUDGET/FORECAST                       
* RECORDS ARE NOT READ                                                          
********************************************************                        
         SPACE                                                                  
SETQRTR  NTR1                                                                   
         LA    R1,QUARTBLE                                                      
         SR    R7,R7              QUARTER COUNTER                               
QRTR5    CLC   QSTART+2(2),1(R1)  CHECK START DATE WITH QUARTER START           
         BE    QRTR10                                                           
         LA    R1,6(R1)           NEXT QUARTER START                            
         CLI   0(R1),X'FF'        END OF QUARTERS                               
         BE    QRTR60                                                           
         B     QRTR5              CHECK NEXT QUARTER                            
*                                                                               
QRTR10   LA    RE,QUARTBLE        CHECK END DATE WITH QUARTER END               
QRTR20   CLC   QEND+2(2),4(RE)                                                  
         BE    QRTR30                                                           
         LA    RE,6(RE)                                                         
         CLI   3(RE),X'FF'        END OF QUARTERS                               
         BE    QRTR60                                                           
         B     QRTR20                                                           
*                HAVE EVEN # OF QUARTERS                                        
QRTR30   ZIC   R2,0(R1)           START OF QUARTER REQUESTED                    
         AH    R2,=H'2'           FIRST QUARTER ADD 2 MONTHS                    
         ZIC   R5,3(RE)           END OF QUARTER REQUESTED                      
QRTR35   CR    R2,R5                                                            
         BE    QRTR50             ONLY ONE QUARTER                              
         LA    R7,1(R7)           LET'S TRY TWO QUARTERS                        
         CH    R2,=H'12'          ARE WE IN DECEMBER?                           
         BNE   QRTR40                                                           
         LA    R2,3               RESET TO TRY MARCH                            
         B     QRTR35                                                           
QRTR40   AH    R2,=H'3'                                                         
         B     QRTR35                                                           
*                                                                               
QRTR50   LA    R7,1(R7)                                                         
QRTR60   STC   R7,QUARTNO                                                       
         B     GXIT                                                             
         EJECT                                                                  
******************************************************                          
* BUILD---                                                                      
* READ RRGFILE AND BUILD RECORDS IN RTABLE                                      
* READ WHOLE FILE OR TILL RANK MAX                                              
******************************************************                          
         SPACE 2                                                                
BUILD    NTR1                                                                   
         LA    R7,BLOCK                                                         
         USING GEND,R7                                                          
         LA    R4,RTABLE                                                        
         USING TABLED,R4                                                        
*                                                                               
         CLC   QOPTION1(2),SPACES      ANY RANK MAX?                            
         BNE   BLD05                   YES                                      
*                                                                               
         LA    R1,99              DEFAULT IS ALL                                
         ST    R1,RNKMX                                                         
         B     BLD10                                                            
*                                                                               
BLD05    PACK  DUB,QOPTION1(2)    TOP N REQUESTED                               
         CVB   R1,DUB             SAVE IN RANK MAX                              
         ST    R1,RNKMX                                                         
*                                                                               
BLD10    BAS   RE,GETRRG                                                        
         CLI   BLOCK+1,255        END OF FILE?                                  
         BE    GXIT                                                             
*                                                                               
         L     R1,GAMT1                                                         
         LTR   R1,R1                                                            
         BP    BLD15              USE IF POSITIVE                               
         LH    RE,RECCT           DON'T WANT                                    
         BCTR  RE,0               SO DECREMENT COUNT AND                        
         STH   RE,RECCT           GET NEXT                                      
         B     BLD10                                                            
*                                                                               
BLD15    LH    RE,RECCT           BLOCK #                                       
         L     RF,RNKMX           RANK MAX REQUESTED                            
         CR    RE,RF              RANK MAX REACHED?                             
         BH    GXIT                                                             
*                                                                               
BLD20    MVC   TADV,GADV          MOVE ADVERTISER,                              
         MVC   TOFF,GOFF          OFFICE,                                       
         MVC   TAMT1,GAMT1        CURRENT BILLING,                              
         ICM   R1,15,TAMT1                                                      
         BM    BLD25                                                            
         MVI   TSIGN1,X'FF'                                                     
BLD25    MVC   TAMT2,GAMT2        PRIOR BILLING, AND                            
         MVC   TAMT3,GAMT3        PRIOR FINAL                                   
         ICM   R1,15,TAMT3                                                      
         BM    BLD30              POSITVE -FF, NEGS -00, ZERO -00               
         MVI   TSIGN3,X'FF'       IN FINSORT -NEGS ON BOTTOM(DECENDING)         
BLD30    LA    R4,L'TLEN(R4)      INTO TABLE                                    
         B     BLD10                                                            
         DROP  R7                                                               
         EJECT                                                                  
* THIS ROUTINE READS A RRGFILE RECORD AND PUTS IT IN BLOCK                      
GETRRG   NTR1                                                                   
         USING GEND,R7                                                          
         GET   RRGFILE,BLOCK                                                    
*                                                                               
         LH    R1,RECCT           INCREMENT RECORD BLOCK COUNTER                
         LA    R1,1(R1)           FROM FILE                                     
         STH   R1,RECCT                                                         
         B     GXIT                                                             
         SPACE                                                                  
ENDRRG   MVI   BLOCK,255          END OF FILE - BLOCK OF FF'S                   
         MVC   BLOCK+1(191),BLOCK                                               
         B     GXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
**********************************************                                  
* RNKTBL ---                                                                    
* THIS ROUTINE CALCULATES THIS YEARS RANK BY                                    
* CHUGGING THROUGH THE TABLE                                                    
**********************************************                                  
         SPACE                                                                  
RNKTBL   NTR1                                                                   
         LA    R4,RTABLE                                                        
         USING TABLED,R4                                                        
         LA    R5,1               COUNTER                                       
         MVI   FRSTIME,1                                                        
*                                                                               
RNKT10   OC    0(L'TLEN,R4),0(R4)                                               
         BZ    GXIT                                                             
         STC   R5,TRANK1                                                        
*                                                                               
         CLI   FRSTIME,1                                                        
         BE    RNKT18             FIRST ONE GETS RANK 1                         
*                                                                               
RNKT16   CLC   SVAMT1,TAMT1                                                     
         BNE   RNKT20                                                           
         MVC   TRANK1,SVRANK      EQUAL AMTS SAME RANK                          
         B     RNKT20                                                           
*                                                                               
RNKT18   MVI   FRSTIME,0                                                        
*                                                                               
RNKT20   MVC   SVAMT1,TAMT1                                                     
         MVC   SVRANK,TRANK1                                                    
         LA    R4,L'TLEN(R4)                                                    
         LA    R5,1(R5)                                                         
         B     RNKT10                                                           
         DROP  R4                                                               
         EJECT                                                                  
************************************************                                
* RESORT----                                                                    
* READ RRGFILE AND SEND RECORDS TO SORTER                                       
* NEED TO SORT RECORDS BY FINAL PRIOR YEAR AMT                                  
*************************************************                               
         SPACE                                                                  
RESORT   NTR1                                                                   
         LA    R7,BLOCK                                                         
         USING GEND,R7                                                          
         LA    R6,MYWORK                                                        
         USING SRECD,R6                                                         
*                                                                               
RSRT10   BAS   RE,GETRRG                                                        
         CLI   BLOCK+1,255                                                      
         BE    GXIT                                                             
*                                                                               
         ICM   R1,15,GAMT3        TEST IF NEGATIVE NUM                          
         LTR   R1,R1                                                            
         BM    RSRT10             DON'T WANT                                    
*                                                                               
         XC    0(L'SLEN,R6),0(R6)                                               
         MVC   SADV,GADV          MOVE INFO TO SORT RECORD FOR PUT              
         MVC   SOFF,GOFF                                                        
         MVC   SAMT1,GAMT1                                                      
         ICM   R1,15,SAMT1                                                      
         BM    RSRT20                                                           
         MVI   SSIGN1,X'FF'                                                     
RSRT20   MVC   SAMT2,GAMT2                                                      
         MVC   SAMT3,GAMT3                                                      
         ICM   R1,15,SAMT3                                                      
         BM    RSRT30                                                           
         MVI   SSIGN3,X'FF'                                                     
RSRT30   GOTO1 =V(SORTER),DMCB,=C'PUT',MYWORK                                   
         B     RSRT10                                                           
         EJECT                                                                  
************************************************                                
* RNKLYR ----                                                                   
* READ RECORDS FROM SORTER( TOP N OR WHOLE FILE)-                               
* LOOPS THROUGH TABLE - IF MATCH ON ADV/OFF THEN MOVE                           
* RANK LAST YEAR AS POSITION FROM SORTER- NO MATCH                              
* GOES AT END OF TABLE                                                          
*************************************************                               
         SPACE                                                                  
RNKLYR   NTR1                                                                   
         XC    MYWORK,MYWORK                                                    
         LA    R6,MYWORK                                                        
         USING SRECD,R6                                                         
         LA    R5,1                                                             
         L     R2,RNKMX           CNTR FOR TOT # OF RECS                        
         MVI   FRSTIME,1                                                        
*                                                                               
RNKL05   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   RE,15,4(R1)                                                      
         BZ    GXIT                                                             
         MVC   SLEN,0(RE)                                                       
*                                                                               
         L     RF,RNKMX           REQUESTED NUMBER OF ADVERTISERS               
         CR    R5,RF                                                            
         BH    GXIT                                                             
*                                                                               
RNKL20   LA    R4,RTABLE                                                        
         USING TABLED,R4                                                        
*                                                                               
RNKL40   CLC   TADV,SADV                                                        
         BNE   RNKL80                                                           
         CLC   TOFF,SOFF                                                        
         BNE   RNKL80                                                           
         STC   R5,TRANK2          ADVERTISER WAS THERE LAST YEAR                
         CLI   FRSTIME,1                                                        
         BE    RNKL90             CAN'T BE EQUAL TO PREV SINCE FIRST            
         CLC   SVAMT3,SAMT3                                                     
         BNE   RNKL100                                                          
*                                                                               
         MVC   TRANK2,SVRANK                                                    
         B     RNKL100                                                          
*                                                                               
RNKL80   LA    R4,L'TLEN(R4)                                                    
         OC    0(L'TLEN,R4),0(R4)                                               
         BNZ   RNKL40                                                           
* PUT AT END OF TABLE                                                           
         STC   R5,TRANK2          INCASE FIRST WASN'T IN TABLE                  
         MVI   TRANK1,0                                                         
         MVC   TADV,SADV                                                        
         MVC   TOFF,SOFF                                                        
         ZIC   R1,SSIGN1                                                        
         STC   R1,TSIGN1                                                        
         MVC   TAMT1,SAMT1                                                      
         MVC   TAMT2,SAMT2                                                      
         ZIC   R1,SSIGN3                                                        
         STC   R1,TSIGN3                                                        
         MVC   TAMT3,SAMT3                                                      
         LA    R2,1(R2)           INCREMENT IF ADDING TO TABLE                  
         ST    R2,NUMRECS                                                       
*                                                                               
RNKL90   MVI   FRSTIME,0                                                        
RNKL100  MVC   SVRANK,TRANK2                                                    
         MVC   SVAMT3,SAMT3                                                     
         LA    R5,1(R5)                                                         
         B     RNKL05                                                           
         EJECT                                                                  
****************************************************************                
* FINSORT -- DOES FINAL SORT ON CURRENT BILLING *                               
*         -- THIS IS JUST FOR PRINTING PURPOSES (NOT REALISTIC)*                
***************************************************************                 
         SPACE                                                                  
FINSORT  NTR1                                                                   
         LA    R4,RTABLE                                                        
         USING TABLED,R4                                                        
         L     R2,NUMRECS         NOW TOTAL # OF RECS                           
         GOTO1 XSORT,DMCB,(C'N',(R4)),(R2),L'TLEN,5,8                           
         SPACE                                                                  
FIN10    CLI   TRANK1,0                                                         
         BE    FIN20                                                            
         MVC   SVRANK,TRANK1                                                    
         MVC   SVAMT1,TAMT1                                                     
         LA    R4,L'TLEN(R4)                                                    
         B     FIN10                                                            
*                                                                               
FIN20    OC    0(L'TLEN,R4),0(R4)                                               
         BZ    GXIT                                                             
FIN30    ZIC   R5,SVRANK          LAST RANK CALCULATED                          
         STC   R5,TRANK1          DEFAULT TO SAME RANK                          
*                                                                               
         CLC   TAMT1,SVAMT1       SHOULD BE SAME RANK?                          
         BE    FIN50              YES AMTS ARE SAME                             
         LA    R5,1(R5)           NO INCREMENT RANK                             
         STC   R5,TRANK1          BECAUSE AMTS ARE DIFFERENT                    
         STC   R5,SVRANK          SAME NEW RANK                                 
         MVC   SVAMT1,TAMT1       SAME NEW AMT                                  
*                                                                               
FIN50    LA    R4,L'TLEN(R4)                                                    
         B     FIN20                                                            
         EJECT                                                                  
*************************************************************                   
* PRINT HEADLINES - PERIOD DATES                                                
* NOTE : QOPTION6=QOPTION4 -- IT IS THE SHARE OPTION THE LAST                   
* CHARACTER IN QRECORD (EITHER Y/N)                                             
*************************************************************                   
         SPACE                                                                  
HEADLINE NTR1                                                                   
         MVC   HEAD3+9(5),QSTATION   MOVE STATION INTO HEADLINES                
         XC    KEY,KEY               READ MARKET RECORD                         
         MVI   KEY,X'8B'                                                        
         MVC   KEY+16(2),QREP                                                   
         MVC   KEY+18(5),QSTATION                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                   
         CLC   KEY(23),KEYSAVE                                                  
         BNE   HEAD01                                                           
         LA    R5,MYAIO                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,(R5),DMWORK           
         CLI   DMCB+8,X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   34(R5),X'01'                                                     
         BNE   HEAD01                                                           
         MVC   HEAD3+15(20),36(R5)     MOVE MARKET INTO HEADLINES               
*                                                                               
HEAD01   MVC   TEMPSTRT,QSTART    DON'T MESS UP QSTART                          
         MVC   TEMPSTRT+4(2),=X'F0F0'     FUDGE FOR DATCON                      
         GOTO1 DATCON,DMCB,(0,TEMPSTRT),(6,HEAD3+46)                            
         MVI   HEAD3+53,C'-'                                                    
         MVC   TEMPEND,QEND       DON'T MESS UP QEND                            
         MVC   TEMPEND+4(2),=X'F0F0'          FUDGE FOR DATCON                  
         GOTO1 DATCON,DMCB,(0,TEMPEND),(6,HEAD3+55)                             
         MVC   HEAD3+78(9),=C'RANK TOP:'                                        
         EDIT  RNKMX,(3,HEAD3+88),ALIGN=LEFT                                    
         CLI   QOPTION3,C'F'      ARE WE FINAL PHASE?                           
         BNE   HEAD30             NO-- CHECK W/P                                
*          YES --- FINAL PHASE                                                  
         MVC   HEAD4+78(5),=C'FINAL'                                            
         CLI   LOPTION4,C'Y'      ARE WE SHARE REPORT?                          
         BE    HEAD05             YES -- DIFFERENT SET OF HEADLINES             
*                                                                               
         MVI   RCSUBPRG,3         MAIN REPORT FOR FINAL WITH INDEX              
         CLI   OFFLTR,1           ARE WE OFFICE RECAP?                          
         BNE   HDFINXT            NO -GET OUT (FINAL PHASE EXIT)                
         MVI   RCSUBPRG,4         OFFICE RECAP/ADV CODE WITH INDEX              
         B     HDOFFXT            OFFICE EXIT                                   
*                                                                               
HEAD05   MVI   RCSUBPRG,7         MAIN RPT FOR FINAL WITH SHARE OPTION          
         CLI   OFFLTR,1           ARE WE OFFICE RECAP?                          
         BNE   HDFINXT            NO - GET OUT (FINAL PHASE EXIT)               
         MVI   RCSUBPRG,8         YES SUB IN ADV CODE                           
         B     HDOFFXT            OFFICE EXIT                                   
         SPACE                                                                  
HEAD30   DS    0H                                                               
         MVC   HEAD6+53(5),=C'PRIOR'                                            
         MVC   HEAD7+53(6),=C'PACING'                                           
         MVC   HEAD8+53(10),=C'----------'                                      
*                                                                               
         MVC   HEAD4+78(9),=C'WORKSHEET'                                        
         CLI   QOPTION3,C'P'      IS IT PRELIMINARY?                            
         BNE   HEAD35             NO --- MUST BE WORKSHEET                      
         MVC   HEAD4+78(11),=C'PRELIMINARY'                                     
*                                                                               
HEAD35   CLI   LOPTION4,C'Y'      SHARE REPORT?                                 
         BE    HEAD50             YES                                           
*                                                                               
         MVI   RCSUBPRG,1         MAIN REPORT FOR WORKSHEET/PRELIMINARY         
         CLI   OFFLTR,1           ARE WE OFFICE RECAP?                          
         BNE   HDWPXT             NO - GET OUT (W/P EXIT)                       
         MVI   RCSUBPRG,2         YES SUB IN ADV CODE                           
         B     HDOFFXT            OFFICE EXIT                                   
         SPACE                                                                  
HEAD50   DS    0H                 SHARE REPORT                                  
         MVI   RCSUBPRG,5         MAIN RPT FOR W/P FOR SHARE OPT                
         CLI   OFFLTR,1           ARE WE OFFICE RECAP?                          
         BNE   HDWPXT             NO -GET OUT (W/P EXIT)                        
         MVI   RCSUBPRG,6         YES SUB IN ADV CODE                           
*                                                                               
HDOFFXT  MVC   HEAD4+38(13),=CL13'OFFICE RECAP:'                                
         MVC   HEAD4+53(20),SVOFFNM                                             
*                                                                               
HDFINXT  DS    0H                                                               
HDWPXT   DS    0H                                                               
HEAD60   GOTO1 REPORT                                                           
         B     GXIT                                                             
         EJECT                                                                  
FOOTLINE NTR1                                                                   
         LA    R5,FOOT1                                                         
         MVI   FRSTIME,0          FLAG IF ANY FILTERS ON                        
         MVC   0(132,R5),SPACES                                                 
         MVC   0(17,R5),=C'(REQUEST DETAILS:'                                   
         LA    R5,18(R5)                                                        
         CLC   QASAT,SPACES                                                     
         BE    FT10                                                             
         MVC   0(11,R5),=C'AS AT DATE='                                         
         MVC   11(6,R5),QASAT                                                   
         LA    R5,18(R5)                                                        
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT10     CLC   QREGION,SPACES                                                   
         BE    FT20                                                             
         MVC   0(7,R5),=C'REGION='                                              
         MVC   7(2,R5),QREGION                                                  
         LA    R5,10(R5)                                                        
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT20     CLC   QOFFICE,SPACES                                                   
         BE    FT25                                                             
         MVC   0(7,R5),=C'OFFICE='                                              
         MVC   7(2,R5),QOFFICE                                                  
         LA    R5,10(R5)                                                        
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT25     CLI   QGROUP,C' '                                                      
         BE    FT30                                                             
         MVC   0(6,R5),=C'GROUP='                                               
         MVC   6(1,R5),QGROUP                                                   
         LA    R5,8(R5)                                                         
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT30     CLI   QSBGROUP,C' '                                                    
         BE    FT40                                                             
         MVC   0(8,R5),=C'SBGROUP='                                             
         MVC   8(1,R5),QSBGROUP                                                 
         LA    R5,10(R5)                                                        
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT40     CLC   QSTATION,SPACES                                                  
         BE    FT60                                                             
         MVC   0(8,R5),=C'STATION='                                             
         MVC   8(5,R5),QSTATION                                                 
         LA    R5,13(R5)                                                        
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT60     CLC   QAGENCY,SPACES                                                   
         BE    FT70                                                             
         MVC   0(7,R5),=C'AGENCY='                                              
         MVC   7(4,R5),QAGENCY                                                  
         LA    R5,12(R5)                                                        
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT70     CLC   QCLASS,SPACES                                                    
         BE    FT80                                                             
         MVC   0(6,R5),=C'QLASS='                                               
         MVC   6(2,R5),QCLASS                                                   
         LA    R5,9(R5)                                                         
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT80     CLC   QCATY,SPACES                                                     
         BE    FT100                                                            
         MVC   0(9,R5),=C'CATEGORY='                                            
         MVC   9(2,R5),QCATY                                                    
         LA    R5,12(R5)                                                        
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT100    DS    0H                                                               
         CLI   FRSTIME,0          ANY REQUEST FILTER?                           
         BE    FT110                                                            
         MVI   0(R5),C')'                                                       
         B     FT120                                                            
*                                                                               
FT110    MVC   0(5,R5),=C'NONE)'                                                
*                                                                               
FT120    MVI   FORCEFUT,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     GXIT                                                             
         EJECT                                                                  
***************************************************                             
* PRNTLIN ---                                                                   
* CHUGS THROUGH TABLE AND PRINTS LINES FROM TABLE                               
***************************************************                             
         SPACE 1                                                                
PRNTLIN  NTR1                                                                   
         LA    R6,P                                                             
         USING PLIND,R6                                                         
         LA    R4,RTABLE                                                        
         USING TABLED,R4                                                        
*                                                                               
PRNT02   CLI   OFFLTR,1           PRINTING INDIVIUDAL OFFICES?                  
         BNE   PRNT10             DO ALL OFFICES                                
PRNT05   CLC   SVOFF,TOFF         THIS OFFICE?                                  
         BE    PRNT10                                                           
         LA    R4,L'TLEN(R4)                                                    
         OC    0(L'TLEN,R4),0(R4)                                               
         BNZ   PRNT05                                                           
         B     GXIT                                                             
*                                                                               
PRNT10   EDIT  TRANK1,(3,PRANK1)      THIS YEARS RANK                           
         EDIT  TRANK2,(3,PRANK2)      LAST YEARS RANK                           
         SPACE                                                                  
* GET ADVERTISER NAME FROM CODE                                                 
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'                                                        
         MVC   KEY+21(4),TADV                                                   
         MVC   KEY+25(2),QREP                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                   
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PRNT20                                                           
         LA    R5,MYAIO                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,(R5),DMWORK           
         CLI   DMCB+8,X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PADV(20),36(R5)                                                  
         B     PRNT22                                                           
*                                                                               
PRNT20   MVC   PADV+5(14),=CL14'NAME NOT FOUND'                                 
         MVC   PADV(4),TADV                                                     
         SPACE                                                                  
PRNT22   CLI   OFFLTR,1           INDIVIDUAL OFFICES?                           
         BNE   PRNT25                                                           
         MVC   POFF,SPACES                                                      
         MVC   POFF+8(4),TADV     MOVE IN CODE INSTEAD                          
         B     PRNT35             OF OFFICE NAME                                
*                                                                               
* GET OFFICE NAME FROM TABLE                                                    
PRNT25   LA    RE,OFFTABLE                                                      
PRNT28   CLC   TOFF,0(RE)                                                       
         BE    PRNT30                                                           
         LA    RE,L'OFFTABLE(RE)                                                
         OC    0(L'OFFTABLE,RE),0(RE)                                           
         BNZ   PRNT28                                                           
         MVC   POFF+4(14),=CL14'NAME NOT TABLE'                                 
         MVC   POFF(2),TOFF                                                     
         B     PRNT35                                                           
*                                                                               
PRNT30   MVC   POFF,2(RE)         MOVE OFFICE NAME                              
*                                                                               
PRNT35   MVC   SVCMT,SPACES                                                     
         CLI   QOPTION3,C'W'      WORKSHEET RUN?                                
         BE    PRNT38             YES - NO FORECAST                             
* NOT WORKSHEET RUN - READ BUDGET/FORECAST RECORD                               
* BUT ONLY IF QUARTERS REQUESTED ARE FULL QUARTERS                              
* QUARTNO=0 NOT FULL QUARTER SET IN HEADLINE ROUTINE                            
         CLI   QUARTNO,0          IF NOT EVEN QUARTER                           
         BE    PRNT50             SKIP                                          
         BAS   RE,RDSBB           GET BUDGET AMTS                               
         SPACE                                                                  
         CLI   LOPTION4,C'Y'      SHARE OPTION?                                 
         BNE   PRNT36                                                           
*                                                                               
         EDIT  TSPL,(6,PSPL)      SPL PERCENT                                   
*                                                                               
         EDIT  SVBUD,(10,PSTA),ZERO=NOBLANK,ALIGN=RIGHT,FLOAT=-                 
         B     PRNT36B                                                          
*                                                                               
PRNT36   EDIT  SVBUD,(10,PBUD),ZERO=NOBLANK,ALIGN=RIGHT                         
         B     PRNT50                                                           
*                                                                               
PRNT36B  BAS   RE,CALSHR              CALCULATE SHARE,FORECAST,MARKET           
         L     R7,MKTTOT                                                        
         ICM   R1,15,SVMKT                                                      
         AR    R7,R1                  ADD MARKET TO TOTAL                       
         ST    R7,MKTTOT                                                        
         B     PRNT50                                                           
*                                                                               
PRNT38   CLI   LOPTION4,C'Y'           SHARE REPORT?                            
         BNE   PRNT45                                                           
         MVC   PSPL,=C'______'         SPL AS RUN                               
         MVC   PSHR,=C'___'            STATION SHARE                            
         MVC   PSTA,=C'__________'     STATION TOTAL                            
         MVC   PMKT,=C'__________'     MARKET TOTAL                             
         B     PRNT50                                                           
*                                                                               
PRNT45   MVC   PBUD,=C'__________'     KNOWN AS FORECAST&STATION TOT            
*                                                                               
PRNT50   L     R7,BUDTOT                                                        
         ICM   R1,15,SVBUD                                                      
         AR    R7,R1                   ADD BUDGET TO TOTAL                      
         ST    R7,BUDTOT                                                        
*                                                                               
         CLI   LOPTION4,C'Y'                                                    
         BNE   PRNT51                                                           
         EDIT  TAMT1,(10,PAMT12),ZERO=NOBLANK,ALIGN=RIGHT,FLOAT=-               
         B     PRNT52                                                           
*                                                                               
PRNT51   EDIT  TAMT1,(10,PAMT1),ZERO=NOBLANK,ALIGN=RIGHT,FLOAT=-                
PRNT52   L     R7,CURBILL                                                       
         ICM   R1,15,TAMT1        ADD CURRENT BILL TO CURBILL TOTAL             
         AR    R7,R1                                                            
         ST    R7,CURBILL                                                       
*                                                                               
         CLI   QOPTION3,C'F'                                                    
         BE    PRNT53             NO PRIOR PACING ON FINAL VERSION              
*                                                                               
         EDIT  TAMT2,(10,PAMT2),ZERO=NOBLANK,ALIGN=RIGHT,FLOAT=-                
         L     R7,PRIBILL                                                       
         ICM   R1,15,TAMT2        ADD PRIOR BILL TO PRIBILL TOTAL               
         AR    R7,R1                                                            
         ST    R7,PRIBILL                                                       
*                                                                               
PRNT53   EDIT  TAMT3,(10,PAMT3),ZERO=NOBLANK,ALIGN=RIGHT,FLOAT=-                
         L     R7,PRIFIN                                                        
         ICM   R1,15,TAMT3        ADD PRIOR FINAL TO TOTAL                      
         AR    R7,R1                                                            
         ST    R7,PRIFIN                                                        
*                                                                               
         CLI   QOPTION3,C'F'                                                    
         BNE   PRNT80                                                           
*  CALCULATE INDEX = (CURRENT PACING/FORECAST X 100)                            
* STEP 1 = CURRENTX100      STEP 2 = STEP 1 + FORECAST/2                        
* STEP 3 = STEP 2 /FORECAST                                                     
         SPACE                                                                  
         CLI   LOPTION4,C'Y'      SHARE REPORT                                  
         BNE   PRNT56                                                           
         MVC   PINDX+2(3),=C'N/A'   DEFAULT IF CAN'T FIGURE                     
         B     PRNT58                                                           
*                                                                               
PRNT56   MVC   PIND+2(3),=C'N/A'    DEFAULT IF CAN'T FIGURE                     
*                                                                               
PRNT58   XR    RE,RE              STEP 1                                        
         ICM   RF,15,TAMT1                                                      
         BNP   PRNT90             SKIP INDEX -IF PACING=ZERO OR NEG             
         M     RE,=F'100'                                                       
*                                                                               
         ICM   R1,15,SVBUD        STEP 2                                        
         BNP   PRNT90             SKIP INDEX -IF FORECAST=ZERO OR NEG           
         SRA   R1,1               DIVIDE BY 2                                   
         AR    RF,R1                                                            
*                                                                               
         ICM   R1,15,SVBUD        STEP 3                                        
         DR    RE,R1                                                            
         CLI   LOPTION4,C'Y'      SHARE REPORT                                  
         BNE   PRNT60                                                           
         MVC   PINDX+1(4),=C'HIGH'                                              
         B     PRNT65                                                           
PRNT60   MVC   PIND+1(4),=C'HIGH'                                               
*                                                                               
PRNT65   LA    R0,999                                                           
         CR    RF,R0                                                            
         BH    PRNT90                                                           
         CLI   LOPTION4,C'Y'                                                    
         BNE   PRNT70                                                           
         EDIT  (RF),(5,PINDX),ALIGN=RIGHT                                       
         B     PRNT90                                                           
*                                                                               
PRNT70   EDIT  (RF),(5,PIND),ALIGN=RIGHT                                        
         B     PRNT90             FINAL HAS NO COMMENTS                         
*                                                                               
PRNT80   MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         CLC   SVCMT,SPACES                                                     
         BE    PRNT100                                                          
         GOTO1 CHOPPER,DMCB,(140,SVCMT),(35,70(R6)),(C'P',4)                    
*                                                                               
PRNT90   MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
PRNT100  LA    R4,L'TLEN(R4)                                                    
         OC    0(L'TLEN,R4),0(R4)     END OF TABLE?                             
         BZ    GXIT                                                             
         CLI   LINE,50                LINES SO FAR                              
         BL    PRNT02                                                           
         BAS   RE,FOOTLINE                                                      
         MVI   FORCEHED,C'Y'          EJECT                                     
         BAS   RE,HEADLINE            RESET HEADS                               
         B     PRNT02                                                           
         EJECT                                                                  
*****************************************                                       
* CALSHR - CALCULATES ONE GIVEN THE OTHER                                       
*          TWO (SHARE, STATION , MARKET)                                        
*     MKT X SHR /100 = STATION                                                  
*     STATION X 100/ SHR = MARKET                                               
*     STATION X 100/ MARKET = SHR                                               
* IF ONLY STATION IS GIVEN - DOES NOTHING                                       
*****************************************                                       
         SPACE                                                                  
CALSHR   NTR1                                                                   
         ICM   RF,15,SVBUD        STATION                                       
         BZ    CSHR30             NEED TO CALCUATE STATION AMT                  
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,SVSHR                                                       
         BZ    CSHR60             NEED TO CALCULATE SHARE AMT                   
*         HAVE STATION, SHARE - NEED TO CALCUATE MARKET                         
         XR    RE,RE                                                            
         M     RE,=F'100'                                                       
         SRA   R1,1                                                             
         AR    RF,R1                                                            
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,SVSHR                                                       
         BZ    CSHRXIT            WRONG -SHOULDN'T BE MORE THAN 1 ZERO          
         DR    RE,R1                                                            
         STCM  RF,15,SVMKT        SAVE MARKET                                   
         EDIT  (RF),(10,PMKT),ZERO=NOBLANK,ALIGN=RIGHT                          
         EDIT  SVSHR,(3,PSHR),ZERO=NOBLANK,ALIGN=RIGHT                          
         EDIT  SVBUD,(10,PSTA),ZERO=NOBLANK,ALIGN=RIGHT                         
         B     CSHRXIT                                                          
         SPACE                                                                  
CSHR30   DS    0H                                                               
         ICM   RF,15,SVMKT                                                      
         BZ    CSHRXIT            SOMETHING WRONG - MORE THAN ONE ZERO          
         EDIT  SVMKT,(10,PMKT),ZERO=NOBLANK,ALIGN=RIGHT                         
         SR    R1,R1                                                            
         ICM   R1,1,SVSHR                                                       
         BZ    CSHRXIT            SOMETHING WRONG - MORE THAN ONE ZERO          
         MR    RE,R1                                                            
         A     RF,=F'50'          ROUNDING PURPOSES                             
         BNO   CSHR40                                                           
         A     RE,=F'1'           FOR OVERFLOW                                  
CSHR40   D     RE,=F'100'                                                       
         STCM  RF,15,SVBUD                                                      
         EDIT  (RF),(10,PSTA),ZERO=NOBLANK,ALIGN=RIGHT                          
         EDIT  SVSHR,(3,PSHR),ZERO=NOBLANK,ALIGN=RIGHT                          
         B     CSHRXIT                                                          
*                                                                               
CSHR60   DS    0H                                                               
         XR    RE,RE                                                            
         M     RE,=F'100'                                                       
         ICM   R1,15,SVMKT                                                      
         BZ    CSHRXIT            SOMETHING WRONG                               
         SRA   R1,1                                                             
         AR    RF,R1                                                            
*                                                                               
         ICM   R1,15,SVMKT                                                      
         DR    RE,R1                                                            
         EDIT  (RF),(3,PSHR),ZERO=NOBLANK,ALIGN=RIGHT                           
         EDIT  SVMKT,(10,PMKT),ALIGN=RIGHT,ZERO=NOBLANK                         
         EDIT  SVBUD,(10,PSTA),ZERO=NOBLANK,ALIGN=RIGHT                         
CSHRXIT  XIT1                                                                   
         EJECT                                                                  
*****************************************                                       
* RDSBB - READS SPECIAL BLAIR BUDGET RECS                                       
*****************************************                                       
         SPACE                                                                  
RDSBB    NTR1                                                                   
         USING TABLED,R4                                                        
         XC    KEY,KEY                                                          
         SR    R7,R7              GRAND TOTAL FOR STAT/FORE/BUDGET              
         SR    R3,R3              MINI TOTAL                                    
         SR    R2,R2              GRAND TOTAL FOR MARKET                        
         SR    R5,R5              MINI TOTAL FOR MARKET                         
* CHECK PERIOD                                                                  
         MVC   TEMPSTRT+4(2),=X'F0F0'                                           
         GOTO1 DATCON,DMCB,(0,TEMPSTRT),(2,COMPSTRT)                            
         GOTO1 DATCON,DMCB,(0,TEMPEND),(2,COMPEND)                              
         LA    R6,KEY                                                           
         USING RSBBKEY,R6                                                       
*                                                                               
RDSBB10  MVI   RSBBKTYP,X'2D'                                                   
         MVC   RSBBKREP,QREP                                                    
         MVC   RSBBKSTA,QSTATION                                                
         MVC   RSBBKADV,TADV                                                    
         MVC   RSBBKOFF,TOFF                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                   
*                                                                               
RDSBB12  CLC   KEY(25),KEYSAVE                                                  
         BNE   RDXIT                                                            
         CLI   QUARTNO,1                                                        
         BNE   RDSBB15              MORE THAN ONE QUARTER                       
*                                                                               
         CLC   COMPSTRT,KEY+25      RIGHT QUARTER?                              
         BNE   RDSBB60              CHECK NEXT RECORD                           
         B     RDSBB30              EQUAL DO GETREC                             
*                                                                               
RDSBB15  CLC   COMPSTRT,KEY+25                                                  
         BH    RDSBB60            NOT IN REQUESTED PERIOD                       
         CLC   COMPEND,KEY+25                                                   
         BL    RDSBB60            NOT IN REQUESTED PERIOD                       
*                                                                               
RDSBB30  DS    0H                                                               
         LA    R6,MYAIO                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,(R6),DMWORK           
         CLI   DMCB+8,X'00'       RECORD OKAY?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   RDSBB40                                                          
         ICM   R3,15,2(R6)        FORECAST                                      
         ICM   R2,15,6(R6)        MARKET                                        
*                                                                               
RDSBB40  CLI   QUARTNO,1                                                        
         BE    RDSBB50            ONLY ONE QUARTER -GET COMMENT & SHR           
*                                                                               
         AR    R7,R3              KEEP MINI-TOTAL OF ADVERTISER                 
         SR    R3,R3                                                            
*                                                                               
         AR    R5,R2              KEEP MINI MARKET TOTAL                        
         SR    R2,R2                                                            
         B     RDSBB60                                                          
*                                                                               
RDSBB50  DS    0H                                                               
         ZIC   R1,10(R6)          SAVE SHARE                                    
         STC   R1,SVSHR                                                         
*                                                                               
RDSBB55  LA    R6,MYAIO                                                         
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   RDXIT                                                            
         MVC   SVCMT,2(R6)                                                      
         B     RDXIT                                                            
*                                                                               
RDSBB60  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEY                   
         B     RDSBB12                                                          
*                                                                               
RDXIT    LTR   R3,R3                                                            
         BZ    RDX70                                                            
         STCM  R3,15,SVBUD                                                      
         B     RDX80                                                            
RDX70    STCM  R7,15,SVBUD                                                      
*                                                                               
RDX80    LTR   R2,R2                                                            
         BZ    RDX90                                                            
         STCM  R2,15,SVMKT                                                      
         B     GXIT                                                             
RDX90    STCM  R5,15,SVMKT                                                      
         B     GXIT                                                             
         EJECT                                                                  
*****************************************************                           
* GETOFFNM -- READS OFFICE RECORD AND GETS NAME FROM                            
*          -- CODE  IN TABLE                                                    
*****************************************************                           
         SPACE                                                                  
GETOFFNM NTR1                                                                   
         LA    R4,RTABLE                                                        
         USING TABLED,R4                                                        
         MVI   FRSTIME,1                                                        
         LA    R2,0               COUNTER FOR # OF OFFICES                      
*                                                                               
OFFNM10  OC    0(L'TLEN,R4),0(R4)                                               
         BNZ   OFFNM13                                                          
         LA    R5,OFFTABLE        POINT AGAIN TO BEGIN OF TABLE                 
         GOTO1 XSORT,DMCB,(0,(R5)),(R2),22,2,0   OFFICE CODE ORDER              
         B     GXIT                                                             
*                                                                               
OFFNM13  LA    R5,OFFTABLE                                                      
         CLI   FRSTIME,1                                                        
         BE    OFFNM20                                                          
*                                                                               
OFFNM15  CLC   0(2,R5),TOFF                                                     
         BE    OFFNM40                                                          
         LA    R5,L'OFFTABLE(R5)                                                
         OC    0(2,R5),0(R5)                                                    
         BNZ   OFFNM15                                                          
*                                                                               
OFFNM20  MVI   FRSTIME,0                                                        
         MVC   0(2,R5),TOFF                                                     
         LA    R2,1(R2)           INCREMENT OFFICE COUNTER                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'                                                        
         MVC   KEY+23(2),QREP                                                   
         MVC   KEY+25(2),0(R5)                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                   
         CLC   KEY(27),KEYSAVE                                                  
         BNE   OFFNM30                                                          
         LA    R3,MYAIO                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,(R3),DMWORK           
         CLI   DMCB+8,X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   2(20,R5),36(R3)                                                  
         B     OFFNM40                                                          
*                                                                               
OFFNM30  MVC   2(20,R5),=CL20'NAME NOT FOUND'                                   
*                                                                               
OFFNM40  LA    R4,L'TLEN(R4)                                                    
         B     OFFNM10                                                          
         EJECT                                                                  
* SETPRINT -- SETS REPORT PRINTING                                              
         SPACE                                                                  
SETPRINT NTR1                                                                   
         MVC   WORK(3),=C'BLR'                                                  
         MVC   WORK+3(2),SVOFF                                                  
*                                                                               
         LA    R4,MYAIO                                                         
         XC    0(25,R4),0(R4)                                                   
         MVI   0(R4),C'I'                                                       
         MVC   15(10,R4),SPACES                                                 
         MVC   15(6,R4),WORK                                                    
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R4),(R4),0                   
         CLI   DMCB+8,0                                                         
         BNE   SETNO              NO ID RECORD FOUND                            
         LA    R4,28(R4)                                                        
*                                                                               
SET10    CLI   0(R4),0                                                          
         BE    SETNO                                                            
         CLI   0(R4),X'02'                                                      
         BNE   SET20                                                            
         MVC   WORK+6(2),2(R4)                                                  
         B     SET50                                                            
*                                                                               
SET20    SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     SET10                                                            
*                                                                               
SETNO    MVC   WORK+6(2),=H'721'  DEFAULT TO BLRSTA                             
*                                                                               
SET50    L     R3,REMOTEC                                                       
         XC    0(18,R3),0(R3)     PRINT BLAIR NY LOCALLY                        
         CLI   NYFLAG,0           DOING MAIN REPORT                             
         BE    SET80              PRINT LOCALLY NY                              
*                                                                               
         USING REMOTED,R3                                                       
         MVC   REMOTDST,WORK+6                                                  
         MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(3),=C'1S '                                              
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTLPP,68                                                      
         MVI   REMOTCLS,C'K'                                                    
         MVI   REMOTRET,C'S'      RETAIN 48 LIVE, 12 DEAD                       
         MVC   REMOTJID,=C'AFR'                                                 
*                                                                               
SET80    B     GXIT                                                             
         EJECT                                                                  
************************************************************                    
* DOTOTALS -- CALCULATES TOTALS FOR BUDGET, CURRENT BILLING                     
* PRIOR BILLING AND PRIOR FINAL                                                 
************************************************************                    
         SPACE                                                                  
DOTOTALS NTR1                                                                   
         LA    R6,P                                                             
         USING PLIND,R6                                                         
         CLI   QOPTION3,C'F'      IF FINAL SKIP PRIOR PACING                    
         BE    DOTOT10                                                          
         OC    PRIBILL,PRIBILL                                                  
         BZ    DOTOT10                                                          
         EDIT  PRIBILL,(10,PAMT2),ALIGN=RIGHT,ZERO=NOBLANK,FLOAT=-              
*                                                                               
DOTOT10  OC    PRIFIN,PRIFIN                                                    
         BZ    DOTOT20                                                          
         EDIT  PRIFIN,(10,PAMT3),ALIGN=RIGHT,ZERO=NOBLANK,FLOAT=-               
*                                                                               
DOTOT20  DS    0H                                                               
         OC    CURBILL,CURBILL                                                  
         BZ    DOTOT30                                                          
         CLI   LOPTION4,C'Y'      SHARE REPORT                                  
         BE    DOTOT25            YES -                                         
         EDIT  CURBILL,(10,PAMT1),ALIGN=RIGHT,ZERO=NOBLANK,FLOAT=-              
         B     DOTOT30                                                          
*                                                                               
DOTOT25  EDIT  CURBILL,(10,PAMT12),ALIGN=RIGHT,ZERO=NOBLANK,FLOAT=-             
*                                                                               
DOTOT30  DS    0H                 FORECAST=BUDGET                               
         CLI   QOPTION3,C'W'                                                    
         BE    DOTOT40            FOR WORKSHEET -NO FORECAST                    
         CLI   LOPTION4,C'Y'      SHARE REPORT                                  
         BE    DOTOT35            YES -                                         
         EDIT  BUDTOT,(10,PBUD),ALIGN=RIGHT,ZERO=NOBLANK,FLOAT=-                
         B     DOTOT40                                                          
*                                                                               
DOTOT35  EDIT  BUDTOT,(10,PSTA),ALIGN=RIGHT,ZERO=NOBLANK,FLOAT=-                
         EDIT  MKTTOT,(10,PMKT),ALIGN=RIGHT,ZERO=NOBLANK,FLOAT=-                
* MUST CALC SHARE                                                               
         MVC   PSHR,=C'N/A'                                                     
         ICM   RF,15,BUDTOT                                                     
         BZ    DOTOT40                                                          
         XR    RE,RE                                                            
         M     RE,=F'100'                                                       
         ICM   R1,15,MKTTOT                                                     
         BZ    DOTOT40                                                          
         SRA   R1,1                                                             
         AR    RF,R1                                                            
         ICM   R1,15,MKTTOT                                                     
         DR    RE,R1                                                            
         CH    RF,=H'100'                                                       
         BNH   DOTOT39                                                          
         MVC   PSHR(4),=C'HIGH'                                                 
         B     DOTOT40                                                          
*                                                                               
DOTOT39  EDIT  (RF),(3,PSHR),ZERO=NOBLANK,ALIGN=RIGHT                           
*                                                                               
DOTOT40  GOTO1 REPORT                                                           
         XC    BUDTOT,BUDTOT      CLEAR TOTALS FOR NEXT REPORT                  
         XC    CURBILL,CURBILL                                                  
         XC    PRIBILL,PRIBILL                                                  
         XC    PRIFIN,PRIFIN                                                    
         XC    MKTTOT,MKTTOT                                                    
         B     GXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************                       
* DOSTLIST - SETS UP STATION LIST AND #OF STATIONS (2)  *                       
*          - STATION LIST IS MY STATION, EVERYBODY ELSE *                       
*********************************************************                       
         SPACE                                                                  
DOSTLIST DS    0H                                                               
         LA    R5,STALIST                                                       
         MVC   0(5,R5),QSTATION                                                 
         MVC   5(3,R5),=C'IND'                                                  
         BR    RE                                                               
         EJECT                                                                  
*********************************************************                       
* CALCSPL  - CHUGGS THROUGH TABLE AND CALLS SPLASRUN    *                       
*          - TO CALCULATE SPL VALUES AND SAVE IN TABLE  *                       
*********************************************************                       
         SPACE                                                                  
CALCSPL  NTR1                                                                   
         LA    R4,RTABLE                                                        
         USING TABLED,R4                                                        
*                                                                               
CALSP10  OC    0(L'TLEN,R4),0(R4)                                               
         BZ    GXIT                                                             
*                                                                               
         XC    SVSPLAMT,SVSPLAMT                                                
         BAS   RE,SPLASRUN                                                      
         MVC   TSPL,SVSPLAMT      SAVE RETURN SPL VALUE                         
         LA    R4,L'TLEN(R4)      NEXT ENTRY                                    
         B     CALSP10                                                          
         EJECT                                                                  
*************************************************************                   
*  SPLASRUN - READS THROUGH ALL CONTRACTS AND CALLS ROUTINE *                   
*             TO GET SPL AMT FOR THAT CONTRACT              *                   
*************************************************************                   
         SPACE                                                                  
SPLASRUN NTR1                                                                   
         L     R7,AMONARCH                                                      
         USING MONARCHD,R7                                                      
         L     R2,MONTABLE                                                      
         LA    R3,SPLTABLE                                                      
         GOTO1 =V(SETSPTBL),DMCB,(R2),(R3)  SET UP SPL TABLE                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'9C'                                                        
         MVC   KEY+2(2),QREP                                                    
         MVC   KEY+4(2),TOFF                                                    
         MVC   KEY+6(2),QGROUP    GROUP/SUBGROUP                                
         MVC   KEY+8(5),QSTATION                                                
         MVC   KEY+13(4),TADV                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                   
SPLRN40  CLC   KEY(17),KEYSAVE                                                  
         BNE   SPLRN80                                                          
*                                                                               
         LA    R6,MYAIO                                                         
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),=C'REPFILE',KEY+28,(R6),X        
               DMWORK                                                           
         CLI   DMCB+8,X'02'       RECORD DELETED?                               
         BE    SPLRN65            YES                                           
         CLI   DMCB+8,X'00'       RECORD OKAY                                   
         BE    SPLRN49                                                          
         CLI   RCONCNTL-RCONREC(R6),X'80'                                       
         BE    SPLRN65                                                          
         DC    H'0'                                                             
SPLRN49  EQU   *                                                                
         CLC   =C'PRINTIT',QUESTOR **TEST**                                     
         BNE   SPLRN49A                                                         
         GOTO1 TRACE,DMCB,(R6),0,=C'CONTRACT',8                                 
SPLRN49A EQU   *                                                                
         XC    DMCB(24),DMCB                SET UP MONACT                       
         CLC   QASAT,SPACES                                                     
         BNE   SPLRN50                                                          
         GOTO1 DATCON,DMCB,(5,WORK),(0,QASAT)                                   
*                                                                               
SPLRN50  GOTO1 DATCON,DMCB,(0,QASAT),(2,COMPEND)                                
         MVC   MONACT(2),=X'1000'  A LOW START DATE                             
         MVC   MONACT+2(2),COMPEND                                              
* CALCULATE LAST YEARS VALUES                                                   
         MVC   WORK(6),=C'800101'                                               
         MVC   WORK+6(6),QASAT                                                  
         LH    R1,WORK                                                          
         BCTR  R1,0                                                             
         CLI   WORK+1,C'0'                                                      
         BNE   *+8                                                              
         SH    R1,=H'246'                                                       
         STH   R1,WORK                                                          
         LH    R1,WORK+6                                                        
         BCTR  R1,0                                                             
         CLI   WORK+7,C'0'                                                      
         BNE   *+8                                                              
         SH    R1,=H'246'                                                       
         STH   R1,WORK+6                                                        
*                                                                               
         GOTO1 GETMDAY,DMCB,WORK,MONACT+4                                       
         GOTO1 (RF),(R1),WORK+6,MONACT+6                                        
*                                                                               
         LA    R1,MONACT                                                        
         ST    R1,DMCB+16                                                       
         MVC   DMCB+16(1),=C' '                                                 
         GOTO1 VALUEMON,DMCB,(R6),(R2),(X'40',MONINFO),MONFORC                  
*                                                                               
         CLC   =C'PRINTIT',QUESTOR **TEST**                                     
         BNE   SPLRN60                                                          
         GOTO1 TRACE,DMCB,(R2),120,=C'MONTABLE',8                               
         DROP  R7                                                               
SPLRN60  LA    R5,STALIST                                                       
         GOTO1 =V(DOSPL),DMCB,(R6),(R2),(R5),2,(R3),1                           
         CLC   =C'PRINTIT',QUESTOR **TEST**                                     
         BNE   SPLRN65                                                          
         GOTO1 TRACE,DMCB,(R3),120,=C'SPLTABLE',8                               
*                                                                               
SPLRN65  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEY                   
         B     SPLRN40                                                          
         SPACE                                                                  
SPLRN80  BAS   RE,ADDSTOT                                                       
*                                                                               
SPLRNXT  XIT1                                                                   
         EJECT                                                                  
******************************************************                          
**       ROUTINE RETURNS MONDAY DATE FOR WEEK GIVEN  *                          
*         INPUT = YYMMDD                             *                          
*         OUTPUT= 2-BYTE BINARY                      *                          
******************************************************                          
         SPACE 3                                                                
GETMDAY  NTR1                                                                   
         LM    R5,R6,0(R1)         R5=A(INPUT), R6=A(OUTPUT)                    
         CLC   0(6,R5),=C'870229' ****FOR LEAP YEAR 88*****                     
         BE    GETM10                       AND                                 
         CLC   0(6,R5),=C'910229' ****FOR LEAP YEAR 92*****                     
         BE    GETM10                       AND                                 
         CLC   0(6,R5),=C'950229'   ****FOR LEAP YEAR 96****                    
         BNE   *+8                                                              
GETM10   MVI   5(R5),C'8'       ***PREVIOUS YEAR MUST BE THE 28TH***            
         GOTO1 GETDAY,DMCB,(R5),FULL                                            
         CLC   FULL(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RE,RE                                                            
         IC    RE,DMCB             DAY NUMBER                                   
         BCTR  RE,R0                                                            
         LNR   RE,RE                                                            
         ST    RE,DMCB+8           PUT IN NUMBER OF DAYS TO SUBTRACT            
*                                                                               
         GOTO1 ADDAY,DMCB,(R5),DUB                                              
*                                                                               
         GOTO1 DATCON,(R1),DUB,(2,(R6))                                         
*                                                                               
         B     GXIT                                                             
         EJECT                                                                  
*************************************************************                   
*  ADDSTOT - POINTS TO SPLTABLE AND CALCULATES FINAL SPL    *                   
*             PER ENTRY IN TABLE                            *                   
*************************************************************                   
         SPACE                                                                  
ADDSTOT  NTR1                                                                   
         LA    R6,SPLTABLE                                                      
         XR    R5,R5                                                            
         XR    R7,R7                                                            
ADDS10   OC    0(4,R6),0(R6)                                                    
         BZ    ADDS50                                                           
*                                                                               
         ICM   R1,15,6(R6)         ADD UP ALL MY STATION TOTALS                 
         AR    R5,R1                                                            
         ICM   R1,15,10(R6)        LUMP ALL OTHER STATION TOTALS                
         AR    R7,R1                                                            
*                                                                               
ADDS30   LA    R6,L'SPLTABLE(R6)                                                
         B     ADDS10                                                           
*                                                                               
ADDS50   DS    0H                                                               
         LTR   R5,R5              IF MY STATION TOTAL                           
         BZ    SPLRNXT            IS ZERO- NO GOOD                              
         XR    R1,R1              GET GRAND TOTAL                               
         AR    R1,R5              MY STATION TOTAL +                            
         AR    R1,R7              EVERYBODY ELSE'S  TOTAL                       
         STCM  R1,15,TOTAMT                                                     
* NEED TO CALCULATE PERCENTAGE                                                  
         LR    RF,R5              MY STATION TOTAL                              
         XR    RE,RE                                                            
         M     RE,=F'100'                                                       
         SRA   R1,1                                                             
         AR    RF,R1                                                            
*                                                                               
         ICM   R1,15,TOTAMT                                                     
         DR    RE,R1                                                            
         STCM  RF,15,SVSPLAMT                                                   
         B     SPLRNXT                                                          
         EJECT                                                                  
TRACE    NTR1                                                                   
         LM    R2,R5,0(R1)                                                      
         MVC   BYTE,ELCODE                                                      
         GOTO1 REPORT                                                           
         LTR   R4,R4                                                            
         BZ    TR10                                                             
         MVI   P,C'-'                                                           
         MVC   P+1(131),P                                                       
*                                                                               
         LR    RE,R5                                                            
         SRL   RE,1                                                             
         LA    RF,66                                                            
         SR    RF,RE                                                            
         LA    RF,P(RF)                                                         
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)                                                    
         GOTO1 REPORT                                                           
*                                                                               
TR10     LTR   R3,R3                                                            
         BNZ   TR50                                                             
         GOTO1 =V(PRNTBL),DMCB,0,(R2),C'DUMP',34,=X'01C4'                       
*                                                                               
         LR    R6,R2                                                            
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         BNE   TR100                                                            
TR20     ZIC   R3,1(R6)                                                         
         GOTO1 =V(PRNTBL),DMCB,0,(R6),C'DUMP',(R3),=X'01C4'                     
         BAS   RE,NEXTEL                                                        
         BE    TR20                                                             
         B     TR100                                                            
*                                                                               
TR50     GOTO1 =V(PRNTBL),DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                     
*                                                                               
TR100    MVC   ELCODE,BYTE                                                      
*                                                                               
TRX      B     GXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
RRGFILE  DCB   DDNAME=RRGFILE,DSORG=PS,RECFM=FB,                       X        
               LRECL=192,MACRF=GM,EODAD=ENDRRG                                  
******>>       BLKSIZE=3072,LRECL=192,MACRF=GM,EODAD=ENDRRG                     
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(17,5,D),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=21'                                    
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
ELCODE   DC    X'00'                                                            
FRSTIME  DS    XL1                                                              
NYFLAG   DS    XL1                NY VERSION=0, OFFICE VERSIONS=1               
QUARTNO  DS    XL1                                                              
SVRANK   DS    XL1                                                              
OFFLTR   DS    XL1                INDICATING MAIN RPT OR INDIV. OFF             
SVSHR    DS    XL1                                                              
SVOFF    DS    CL2                OFFICE PRINTED                                
COMPSTRT DS    XL2                COMPRESSED REQUESTED START DATE               
COMPEND  DS    XL2                COMPRESSED REQUESTED END DATE                 
LOPTION4 DS    CL1                LOCAL COPY OF WHAT WAS QOPTION4               
RECCT    DS    H                                                                
RELO     DS    A                                                                
RNKMX    DS    F                  # OF ADVERTISERS REQUESTED TO RANK            
NUMRECS  DS    F                  TOTAL # OF RECORDS IN RTABLE                  
BUDTOT   DS    F                  BUDGET TOTAL                                  
CURBILL  DS    F                  CURRENT BILLING TOTAL                         
PRIBILL  DS    F                  PRIOR BILLING TOTAL                           
PRIFIN   DS    F                  PRIOR FINAL TOTAL                             
MKTTOT   DS    F                  MARKET TOTAL                                  
TOTAMT   DS    F                                                                
SVMKT    DS    XL4                                                              
SVBUD    DS    XL4                                                              
SVAMT1   DS    XL4                USED FOR COMPARE IN CALCULATING RANK          
SVAMT3   DS    XL4                USED FOR COMPARE IN CALCULATING RANK          
SVSPLAMT DS    XL4                                                              
TEMPSTRT DS    XL6                                                              
TEMPEND  DS    XL6                                                              
TIME     DS    CL8                                                              
SVOFFNM  DS    CL20               OFFICE NAME SAVED                             
STALIST  DS    CL45                                                             
SVCMT    DS    CL140              SAVE COMMENT                                  
MYWORK   DS    CL100                                                            
OFFTABLE DS    50CL22             TABLE OF ALL OFFICES IN TOP N                 
QUARTBLE DC    X'01',X'F0F1',X'03',X'F0F3'                                      
         DC    X'04',X'F0F4',X'06',X'F0F6'                                      
         DC    X'07',X'F0F7',X'09',X'F0F9'                                      
         DC    X'0A',X'F1F0',X'0C',X'F1F2'                                      
*******  DC    X'10',X'F1F0',X'12',X'F1F2'                                      
         DC    X'FF',X'FFFF',X'FF',X'FFFF'                                      
MYAIO    DS    1024C                                                            
BLOCK    DS    192C                                                             
         SPACE                                                                  
* ROWS - MONTH DATE (SIX CHARACTERS LONG)- FOLLOWED BY 10 COLUMNS               
*        OF 4 BYTES EACH ( REPRESENT STATION AMT)                               
*        LAST ROW - DATE FIELD MARKED WITH 0'S                                  
         SPACE                                                                  
SPLTABLE DS    25CL46                                                           
         SPACE                                                                  
RTABLE   DS    205CL27                                                          
         EJECT                                                                  
*                                                                               
*   SETMONDT ESTABLISHES THE OLD FORMAT MONTH TABLE EXPECTED BY THIS            
*      PROGRAM, WHICH USES THE ORIGINAL VALUEMON, RATHER THAN THE NEW           
*      FACILITY, VALUENEW                                                       
*                                                                               
         DS    0H                                                               
SETMONDT NMOD1 0,*SETMON*                                                       
         L     RC,0(R1)                                                         
         L     R2,MONTABLE                                                      
         LA    R3,24                                                            
         SPACE 2                                                                
CLEARMN  LR    R4,R2                                                            
         LR    R5,R3                                                            
         SPACE 2                                                                
CLEARMN2 XC    0(40,R4),0(R4)                                                   
         LA    R4,40(R4)                                                        
         BCT   R5,CLEARMN2                                                      
         SPACE 2                                                                
         LR    R4,R2                                                            
         LR    R5,R3                                                            
         BCTR  R5,0                                                             
         CLC   QSTART(12),SPACES                                                
         BE    SETEXIT                                                          
         CLC   QSTART,SPACES                                                    
         BE    UPTO                                                             
         MVC   0(4,R4),QSTART                                                   
         MVC   4(2,R4),=C'01'                                                   
         CLC   QEND,SPACES                                                      
         BE    SETEXIT                                                          
         B     MNTHLOOP                                                         
         SPACE 2                                                                
UPTO     MVC   WORK,QEND                                                        
         MVC   WORK+4(2),=C'01'    (NOW STEP BACK 22 1/2 MONTHS)                
         L     R6,=F'-670'                                                      
         GOTO1 ADDAY,DMCB,WORK,(R4),(R6)                                        
         MVC   4(2,R4),=C'01'                                                   
         SPACE 2                                                                
MNTHLOOP CLC   0(4,R4),QEND                                                     
         BE    SETEXIT                                                          
         LA    R6,40(R4)                                                        
         GOTO1 ADDAY,DMCB,(R4),(R6),45                                          
         MVC   4(2,R6),=C'01'                                                   
         LR    R4,R6                                                            
         BCT   R5,MNTHLOOP                                                      
SETEXIT  EQU   *                                                                
         BAS   RE,SETUP2                                                        
         XIT1                                                                   
         EJECT                                                                  
*              ADDITIONAL INFORMATION TO OPTIMISE VALUEMON                      
*                                                                               
         DS    0H                                                               
SETUP2   NTR1                                                                   
         PRINT GEN                                                              
         L     R7,AMONARCH                                                      
         USING MONARCHD,R7                                                      
*                                                                               
         L     RC,0(R1)                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK)                                  
         CLC   QASAT,SPACES                                                     
         BE    *+10                                                             
         MVC   WORK(6),QASAT                                                    
         LH    R2,WORK                                                          
         BCTR  R2,0                                                             
         CLI   WORK+1,C'0'                                                      
         BNE   *+8                                                              
         SH    R2,=H'246'                                                       
         STH   R2,WORK+6                                                        
         MVC   WORK+8(4),WORK+2                                                 
         SPACE                                                                  
*   CODE TO GET ROUND THE 53 WEEK YEAR PROBLEM                                  
*   DATES 9/24/84 - 12/30/84 MUST HAVE PRIOR WEEK BACKED UP ONE                 
*   DATES 12/31/84 - 12/29/85 MUST HAVE PRIOR WEEK FORWARD ONE                  
*                                                                               
         GOTO1 GETDAY,DMCB,WORK,FULL                                            
         CLC   WORK(6),=C'840924'  *                                            
         BL    SETUP2C             *                                            
         CLC   WORK(6),=C'841230'  *                                            
         BH    SETUP2A             *                                            
         CLI   DMCB,2              * 9/24/84 - 12/30/84 :                       
         BNH   SETUP2C             * WED - SUN SUBTRACT ONE WEEK                
         L     R2,=F'-7'           *           FROM PRIOR DATE                  
         B     SETUP2B             *                                            
*                                                                               
SETUP2A  CLC   WORK(6),=C'851229'  * 12/31/84 - 3/3/85 :                        
         BH    SETUP2C             * MON - TUES ADD ONE WEEK                    
         CLI   DMCB,2              *            TO PRIOR DATE                   
         BH    SETUP2C             *                                            
         BL    *+14                * 3/4/85 - 12/29/85 :                        
         CLC   WORK(6),=C'850305'  * MONDAY, ADD ONE WEEK                       
         BNL   SETUP2C             *         TO PRIOR DATE                      
         L     R2,=F'7'            *                                            
*                                                                               
SETUP2B  ST    R2,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,WORK+6,DUB    ADD/SUBTRACT A WEEK                     
         MVC   WORK+6(6),DUB                                                    
         SPACE 2                                                                
SETUP2C  GOTO1 GETMDAY2,DMCB,WORK,MONFORC                                       
         GOTO1 GETMDAY2,(R1),WORK+6,MONFORC+2                                   
         CLC   QSTART+4(2),SPACES                                               
         BE    SETUP4                                                           
         MVC   WORK(6),QSTART                                                   
         MVC   WORK+6(6),QEND                                                   
         LH    R2,WORK                                                          
         BCTR  R2,0                                                             
         CLI   WORK+1,C'0'                                                      
         BNE   *+8                                                              
         SH    R2,=H'246'                                                       
         STH   R2,WORK                                                          
         LH    R2,WORK+6                                                        
         BCTR  R2,0                                                             
         CLI   WORK+7,C'0'                                                      
         BNE   *+8                                                              
         SH    R2,=H'246'                                                       
         STH   R2,WORK+6                                                        
         GOTO1 GETMDAY2,DMCB,QSTART,MONACT                                      
         GOTO1 (RF),(R1),QEND,MONACT+2                                          
         GOTO1 (RF),(R1),WORK,MONACT+4                                          
         GOTO1 (RF),(R1),WORK+6,MONACT+6                                        
*                                                                               
SETUP4   L     R2,MONTABLE         NOW BUILD MONTHLY INFO LIST                  
         LA    R3,MONINFO                                                       
         XC    MONINFO,MONINFO                                                  
         LA    R4,24                                                            
         SPACE 2                                                                
SETUP6   CLI   0(R2),0                                                          
         BE    SETUP8                                                           
         PRINT GEN                                                              
         GOTO1 DATCON,DMCB,(0,(R2)),(3,(R3))                                    
         GOTO1 GETBROAD,DMCB,(R2),WORK                                          
         MVI   2(R3),28                                                         
         CLI   DMCB,5                                                           
         BNE   *+8                                                              
         MVI   2(R3),35                                                         
         MVC   WORK(6),0(R2)                                                    
         PACK  DUB,WORK(2)                                                      
         SP    DUB,=P'1'                                                        
         UNPK  WORK(2),DUB                                                      
         OI    WORK+1,X'F0'                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(3,4(R3))                                   
         GOTO1 GETBROAD,DMCB,WORK,WORK+6                                        
         PRINT NOGEN                                                            
         MVI   6(R3),28                                                         
         CLI   DMCB,5                                                           
         BNE   *+8                                                              
         MVI   6(R3),35                                                         
         SR    R5,R5               DAYS IN CAL. MONTH                           
         IC    R5,1(R3)                                                         
         LA    R5,DPMTABLE-1(R5)                                                
         MVC   3(1,R3),0(R5)                                                    
         MVC   7(1,R3),0(R5)                                                    
         LA    R2,40(R2)                                                        
         LA    R3,8(R3)                                                         
         BCT   R4,SETUP6                                                        
         SPACE 1                                                                
SETUP8   DS    0H                                                               
         LA    R2,MONINFO          SET UP DATE PARAMETERS FOR K FILTERS         
         MVC   CURDTST,0(R2)                                                    
         MVC   PRIDTST,4(R2)                                                    
         MVC   CURDTEND,0(R2)                                                   
         MVC   PRIDTEND,4(R2)                                                   
SETUP8A  CLI   8(R2),0                                                          
         BE    SETUPXIT                                                         
*                                                                               
         LA    R2,8(R2)                                                         
         CLC   CURDTST,0(R2)                                                    
         BL    SETUP8B                                                          
         MVC   CURDTST,0(R2)                                                    
         MVC   PRIDTST,4(R2)                                                    
         B     SETUP8A                                                          
SETUP8B  CLC   CURDTEND,0(R2)                                                   
         BH    SETUP8A                                                          
         MVC   CURDTEND,0(R2)                                                   
         MVC   PRIDTEND,4(R2)                                                   
         B     SETUP8A                                                          
         SPACE 1                                                                
*                                  TEST IF LAST YEAR'S DATA NEEDED              
SETUPXIT EQU   *                                                                
         XIT1                                                                   
         PRINT NOGEN                                                            
*                                                                               
DPMTABLE DC    AL1(31,28,31,30,31,30,31,31,30,31,30,31)                         
*                                                                               
         DROP  R7                                                               
         EJECT                                                                  
******************************************************                          
*   NOTE:  THIS IS A SECOND COPY OF ROUTINE GETMDAY  *                          
*        MOVED LOCALLY FOR ADDRESSABILITY            *                          
**       ROUTINE RETURNS MONDAY DATE FOR WEEK GIVEN  *                          
*         INPUT = YYMMDD                             *                          
*         OUTPUT= 2-BYTE BINARY                      *                          
******************************************************                          
         SPACE 3                                                                
GETMDAY2 NTR1                                                                   
         LM    R5,R6,0(R1)         R5=A(INPUT), R6=A(OUTPUT)                    
         CLC   0(6,R5),=C'870229' ****FOR LEAP YEAR 88*****                     
         BE    GETM210                      AND                                 
         CLC   0(6,R5),=C'910229' ****FOR LEAP YEAR 92*****                     
         BE    GETM210                      AND                                 
         CLC   0(6,R5),=C'950229'   ****FOR LEAP YEAR 96****                    
         BNE   *+8                                                              
GETM210  MVI   5(R5),C'8'       ***PREVIOUS YEAR MUST BE THE 28TH***            
         GOTO1 GETDAY,DMCB,(R5),FULL                                            
         CLC   FULL(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RE,RE                                                            
         IC    RE,DMCB             DAY NUMBER                                   
         BCTR  RE,R0                                                            
         LNR   RE,RE                                                            
         ST    RE,DMCB+8           PUT IN NUMBER OF DAYS TO SUBTRACT            
*                                                                               
         GOTO1 ADDAY,DMCB,(R5),DUB                                              
*                                                                               
         GOTO1 DATCON,(R1),DUB,(2,(R6))                                         
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
FILED    DSECT                                                                  
AREA     DS    2000C                                                            
         EJECT                                                                  
*** INCLUDE REREPWORKD                                                          
*** INCLUDE REREPMODES                                                          
*** INCLUDE DDREMOTED                                                           
*** INCLUDE REGENSBB                                                            
*** INCLUDE REGENCON                                                            
         PRINT OFF                                                              
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE REXADDRD                                                       
         EJECT                                                                  
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
       ++INCLUDE REGENSBB                                                       
         EJECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
       ++INCLUDE REMONARCHD                                                     
         EJECT                                                                  
* RRG FILE LINE DSECT                                                           
         SPACE                                                                  
GEND     DSECT                                                                  
         DS    CL18                                                             
GADV     DS    CL4                                                              
         DS    CL6                                                              
GOFF     DS    CL2                                                              
         DS    CL98                                                             
GAMT1    DS    CL4                                                              
GAMT2    DS    CL4                                                              
GAMT3    DS    CL4                                                              
         SPACE                                                                  
TABLED   DSECT                                                                  
TLEN     DS    0CL27                                                            
TRANK1   DS    XL1                                                              
TRANK2   DS    XL1                                                              
TADV     DS    CL4                                                              
TOFF     DS    CL2                                                              
TSIGN1   DS    XL1                                                              
TAMT1    DS    CL4                                                              
TSIGN2   DS    XL1                                                              
TAMT2    DS    CL4                                                              
TSIGN3   DS    XL1                                                              
TAMT3    DS    CL4                                                              
TSPL     DS    XL4                                                              
         SPACE                                                                  
* DSECT FOR RECORD SENT TO SORTER                                               
         SPACE                                                                  
SRECD    DSECT                                                                  
SLEN     DS    0CL21                                                            
SADV     DS    CL4                                                              
SOFF     DS    CL2                                                              
SSIGN1   DS    XL1                                                              
SAMT1    DS    CL4                                                              
SSIGN2   DS    XL1                                                              
SAMT2    DS    CL4                                                              
SSIGN3   DS    XL1                                                              
SAMT3    DS    CL4                                                              
         EJECT                                                                  
* OFFLINE REPORT                                                                
         SPACE                                                                  
PLIND    DSECT                                                                  
PLEN     DS    0CL200                                                           
         DS    CL2                                                              
PRANK1   DS    CL3                                                              
         DS    CL2                                                              
PRANK2   DS    CL3                                                              
         DS    CL1                                                              
PADV     DS    CL20                                                             
         DS    CL1                                                              
POFF     DS    CL20                                                             
         DS    CL1                                                              
PAMT2    DS    CL10                                                             
         DS    CL1                                                              
PAMT3    DS    CL10                                                             
         DS    CL1                                                              
PAMT1    DS    CL10                                                             
         DS    CL1                                                              
PBUD     DS    CL10                                                             
         DS    CL1                                                              
PIND     DS    CL4                                                              
         DS    CL19                                                             
         ORG   PAMT1                                                            
PSPL     DS    CL6                                                              
         DS    CL1                                                              
PAMT12   DS    CL10               PAMT1=PAMT1 TOO                               
         DS    CL1                                                              
PSHR     DS    CL3                                                              
         DS    CL1                                                              
PSTA     DS    CL10                                                             
         DS    CL1                                                              
PMKT     DS    CL10                                                             
         DS    CL1                                                              
PINDX    DS    CL4                                                              
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021REREPAF02 05/01/02'                                      
         END                                                                    
