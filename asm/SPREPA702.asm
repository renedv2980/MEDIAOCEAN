*          DATA SET SPREPA702  AT LEVEL 010 AS OF 03/23/11                      
*PHASE SPA702A  <====                                                           
*INCLUDE PRNTBL                                                                 
*INCLUDE SORTER                                                                 
         TITLE 'SPA702 - JWT MARKET ANALYSIS REPORT'                            
*==================================================================*            
* 10NOV97  MHER  SUPPORT A7=NO IN CLIENT OPTIONS TO EXCLUDE CLIENT *            
*==================================================================*            
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
*   SORT RECORD LAYOUT                                                *         
*                                                                     *         
* TYPE  SPR  MGR1  MGR2  MGR3   MKT   STA   OFC   CLT    M/Y          *         
* ----  ---  ----  ----  ----   ---   ---   ---   ---   -----         *         
*   0   1-2   3-4   5-6   7-8  9-10  11-13   14  15-16  17-18         *         
*                                      *      *           **          *         
*        * NOTE STA & OFFICE NOT NEEDED FOR THIS PROGRAM              *         
*                                                                     *         
*        ** NOTE Y/M INVERTED TO M/Y TO SORT MONTHS TOGETHER - THERE  *         
*           SHOULD NEVER BE MORE THAN 24 MONTHS ACTIVITY INTO THIS    *         
*           PROGRAM, AND THE FIRST MONTH SHOULD BE JANUARY.           *         
*                                                                     *         
*   REQUEST OPTIONS                                                   *         
*   ---------------                                                   *         
*     (58)  STA AFFILIATE      N=NBC,A=ABC,C=CBS,I=IND                *         
*                                                                     *         
*   0 (61)  SUMMARIES ONLY Y = SUPPRESS MARKET AND STATION DETAILS    *         
*                                                                     *         
*   1 (62)  CLIENT DETAIL  0 = SHOW ONLY CURR MONTH FOR EACH CLIENT   *         
*                          1 = SHOW ALL MONTHS FOR EACH CLIENT        *         
*                                                                     *         
*   3 (64)  OFFICE W/IN    Y = OFFICE TOTALS WITHIN STATION           *         
*           STATION            (OVERRIDES PROGPROF+0)                 *         
*                                                                     *         
*   4 (65)  SUPPRESS NET   Y = SUPPRESS NET                           *         
*                                                                     *         
*   5 (66)  CLIENT DETAIL  Y = SUPPRESS CLIENT DETAIL                 *         
*                              (OFFICE DETAILS PRINT REGARDLESS)      *         
*                                                                     *         
*   6 (67)  REPORT DETAIL  1 = UNPAID DETAILS + STATION TOTALS        *         
*                          2 = UNPAID DETAILS + UNPAID TOTALS ONLY    *         
*                                                                     *         
*   7 (68)  EXCLUDE CODE   1 = INCLUDE IF CPROF+13 = 1                *         
*           FILTER         2 = INCLUDE IF CPROF+13 = 2                *         
*                          3 = INCLUDE IF CPROF+13 = 1 OR 2           *         
*                                                                     *         
*   ** NOTE 1 **           IF QREP = ALL AND QREPTYP=S, ONLY SPECIAL  *         
*                          REP SUMMARY IS PRINTED                     *         
*                                                                     *         
*   ** NOTE 2 **           THIS PROGRAM WAS COPIED FROM SPREPA602,AND *         
*                          USES SPA603 AS ITS SUB-CONTROLLER          *         
*                                                                     *         
***********************************************************************         
         TITLE 'SPA702 - MARKET ANALYSIS REPORT'                                
SPA702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPA702,RC,RR=R8                                                
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         ST    R8,RELO                                                          
*                                                                               
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    SS60                                                             
         CLI   MODE,STALAST                                                     
         BE    SS100                                                            
         CLI   MODE,CLTFRST                                                     
         BE    SS40                                                             
         CLI   MODE,REQFRST                                                     
         BE    SS20                                                             
         CLI   MODE,REQLAST                                                     
         BE    SS200                                                            
         CLI   MODE,RUNFRST                                                     
         BE    SS10                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
* RUNFRST *                                                                     
         SPACE 1                                                                
SS10     DS    0H                                                               
         STM   R9,RC,HDHKR9                                                     
         LA    R0,SSHDHK                                                        
         ST    R0,HEADHOOK                                                      
         SPACE 1                                                                
* RELOCATE ADCONS *                                                             
         SPACE 1                                                                
         LA    R0,(ADCONSX-ADCONS)/4+1                                          
         LA    R1,ADCONS                                                        
*                                                                               
SS12     L     RE,0(R1)                                                         
         A     RE,RELO                                                          
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,SS12                                                          
         SPACE 1                                                                
* INITIALIZE BUFFALO FILE *                                                     
         SPACE 1                                                                
         LA    R0,SRREC                                                         
         ST    R0,BUFFIO                                                        
*                                                                               
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* REQFRST *                                                                     
         SPACE 1                                                                
SS20     DS    0H                                                               
         ZAP   BANKFCTR,=P'0'         CLEAR TIME BANK FACTOR                    
         XC    MGR1MASK(4),MGR1MASK   CLEAR MKTGRP MASKS                        
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',BUFFBUFF                                  
*                                                                               
         CLC   =C'TEST',QUESTOR    TEST TO TRACE SORT                           
         BNE   SS22                                                             
         MVC   TRCSAVE,VSORTER                                                  
         LA    RE,TRCSORT                                                       
         ST    RE,VSORTER                                                       
         SPACE 1                                                                
* BUILD LIST OF REQUESTED MONTHS (START/END DATES) *                            
         SPACE 1                                                                
SS22     XC    SSDATES,SSDATES                                                  
         MVI   QPRIOR,C'Y'                                                      
         MVI   QSUBSEQ,C'Y'                                                     
         NI    QOPT1,X'0F'         SET EQ TO BINARY BUFF LEVEL                  
*                                                                               
         LA    R4,SSDATES                                                       
         MVI   QPRIOR,C'N'                                                      
*                                                                               
SS24     MVC   WORK(12),SPACES                                                  
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+6(4),QEND                                                   
         GOTO1 MOBILE,DMCB,(24,WORK),(1,(R4))                                   
         MVC   BQSTART,0(R4)       SET START MONTH START DATE                   
         SR    R0,R0                                                            
         ICM   R0,3,2(R4)          GET YEAR & MONTH                             
         SRL   R0,9                DROP MONTH & DAY                             
         STC   R0,STRTYEAR         STORE START YEAR FOR LATER COMPARE           
         SPACE 1                                                                
* FIND END OF MONTH LIST *                                                      
         SPACE 1                                                                
SS26     CLI   4(R4),X'FF'                                                      
         BE    SS26X                                                            
         LA    R4,4(R4)                                                         
         B     SS26                                                             
SS26X    MVC   BQEND,2(R4)         SET END MONTH END DATE                       
*                                                                               
* COMPRESSED DATE = 2 BYTES, 0-6 = YR, 7-10 = MON, 11-15 = DAY                  
*                                                                               
         ICM   R0,3,BQEND          GET ENDING DATE                              
         SLL   R0,23               DROP HI-ORD 7 BITS (YEAR)                    
         SRL   R0,28               DROP LO-ORD 5 (DAY)                          
         STC   R0,CURMON           LEAVING CURRENT MONTH                        
*                                                                               
         LA    R4,4(R4)            POINT BEYOND LAST DATE                       
         XC    0(4,R4),0(R4)                                                    
         MVI   QSUBSEQ,C'N'                                                     
         EJECT                                                                  
* BUILD HEADLINE DATES AND SAVE *                                               
         SPACE 1                                                                
SS30     MVC   HLDATES(32),=C'PERIOD FROM JAN/82+P TO DEC/83+S'                 
*                                                                               
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,WORK,(6,HLDATES+12)                                  
         GOTO1 DATCON,DMCB,WORK,(20,WORK+20)                                    
         MVC   STPRTYR,WORK+20     SAVE START YEAR YYYY FOR HEAD HOOK           
         CLI   QPRIOR,C'Y'                                                      
         BE    *+10                                                             
         MVC   HLDATES+18(2),SPACES                                             
*                                                                               
         MVC   WORK(4),QEND                                                     
         MVC   WORK+4(2),=C'28'                                                 
         GOTO1 (RF),(R1),WORK,(6,HLDATES+24)                                    
         GOTO1 DATCON,DMCB,WORK,(20,WORK+20)                                    
         MVC   EDPRTYR,WORK+20     SAVE END YEAR YYYY FOR HEAD HOOK             
         CLI   QSUBSEQ,C'Y'                                                     
         BE    *+10                                                             
         MVC   HLDATES+30(2),SPACES                                             
*                                                                               
***NOT   DONE ABOVE FOR Y2K                                                     
***NOT   MVC   STPRTYR,HLDATES+16  SAVE START YEAR FOR HEAD HOOK                
***NOT   MVC   EDPRTYR,HLDATES+28  ALSO END YEAR                                
*                                                                               
         GOTO1 SQUASHER,DMCB,HLDATES,32                                         
         GOTO1 CENTER,DMCB,HLDATES,32                                           
         SPACE 1                                                                
* LOAD SORT *                                                                   
         SPACE 1                                                                
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD,0                                  
         SPACE 1                                                                
* PASS DUMMY E-O-F RECORD *                                                     
         SPACE 1                                                                
         XC    SRREC,SRREC                                                      
         MVC   SRKEY,XFF                                                        
         GOTO1 VSORTER,DMCB,=C'PUT',SRREC                                       
*                                                                               
         B     EXIT                                                             
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,20,BI,A),WORK=1'                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=36'                                    
         EJECT                                                                  
* CLIENT FIRST *                                                                
SS40     CLI   QMGR,C' '           TEST MKTGRPS                                 
         BE    SS44                NO                                           
         OC    MGR1MASK,MGR1MASK   TEST MASK SET                                
         BNZ   SS44                                                             
         SPACE 1                                                                
* SET MASKS FOR MKTGRPS *                                                       
         SPACE 1                                                                
         CLC   MGR3LEN,MGR2LEN     TEST 3 LEVELS OF MKTGRP                      
         BE    SS42                NO                                           
         ZIC   R0,MGR2LEN                                                       
         BAS   RE,SETMASK                                                       
         STH   R0,MGR2MASK                                                      
*                                                                               
SS42     CLC   MGR2LEN,MGR1LEN     TEST 2 LEVELS OF MKTGRP                      
         BE    SS44                                                             
         ZIC   R0,MGR1LEN                                                       
         BAS   RE,SETMASK                                                       
         STH   R0,MGR1MASK                                                      
         B     SS44                                                             
         SPACE 2                                                                
SETMASK  LTR   R0,R0                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    RF,MGRMASKS-2                                                    
         LA    RF,2(RF)                                                         
         BCT   R0,*-4                                                           
         LH    R0,0(RF)                                                         
         BR    RE                                                               
*                                                                               
MGRMASKS DC    X'0FFF'             FOR BREAK LEN = 3                            
         DC    X'00FF'                             2                            
         DC    X'000F'                             1                            
         EJECT                                                                  
SS44     CLI   QOPT3,C' '          TEST OFFICE TOTAL OVERRIDE                   
         BE    *+10                NO                                           
         MVC   PROGPROF(1),QOPT3                                                
         SPACE 1                                                                
         CLI   QOPT7,C' '          TEST CLIENT FILTER                           
         BE    EXIT                NO                                           
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         TM    COPT2,COP2EXA7      EXCLUDE THIS CLIENT?                         
         BO    SS46                                                             
*                                                                               
         MVC   BYTE,CPROF+13                                                    
         NI    BYTE,X'0F'                                                       
         ZIC   RE,QOPT7                                                         
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    BYTE,0 * EXECUTED *                                              
         BZ    EXIT                                                             
SS46     MVI   MODE,CLTLAST                                                     
         B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****************************                                                   
*                           *                                                   
*   BUY RECORD PROCESSING   *                                                   
*                           *                                                   
*****************************                                                   
         SPACE 1                                                                
SS60     DS    0H                                                               
         MVI   DMINBTS,X'08'       SET TO PASS DELETED RECORDS                  
         MVI   DMOUTBTS,X'FD'      DO NOT TEST X'02'                            
         GOTO1 GETBUY                                                           
*                                                                               
         TM    BUYREC+15,X'80'     TEST DELETED                                 
         BO    EXIT                YES - IGNORE                                 
*                                                                               
         CLC   =CL3'ALL',QREP      TEST ALL SPECIAL REP REQUEST                 
         BNE   *+14                                                             
         OC    BDREP,BDREP                                                      
         BZ    EXIT                                                             
*                                                                               
         MVI   BDTIME,0            SUPPRESS PIGGYBACKS                          
*                                                                               
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
*                                                                               
SS62     BAS   RE,NEXTEL                                                        
         BNE   SS80                                                             
         MVC   DUB(2),2(R6)        MOVE ELEMENT DATE                            
         SPACE 1                                                                
* TEST FOR PRIOR OR SUBSEQUENT *                                                
         SPACE 1                                                                
         CLC   2(2,R6),BQSTART     TEST PRIOR TO REQ START                      
         BNL   SS64                NO                                           
         CLI   QPRIOR,C'Y'         TEST INCLUDE PRIOR                           
         BNE   SS62                                                             
         MVC   DUB(2),=X'0101'     SET FOR PRIOR POSTING                        
         B     SS66                                                             
*                                                                               
SS64     CLC   2(2,R6),BQEND       TEST AFTER REQ END                           
         BNH   SS66                NO                                           
         CLI   QSUBSEQ,C'Y'        TEST INCLUDE SUBSEQ                          
         BNE   SS62                                                             
         MVC   DUB(2),=X'FEFE'     SET FOR SUBSEQ POSTING                       
*                                                                               
SS66     CLI   QOPT6,C'2'          TEST UNPAID ITEMS ONLY                       
         BNE   SS70                NO                                           
         OC    4(2,R6),4(R6)       TEST PAID                                    
         BNZ   SS62                YES - SKIP                                   
         EJECT                                                                  
* ELEMENT IS IN REQUEST PERIOD *                                                
         SPACE 1                                                                
SS70     DS    0H                                                               
         ZIC   R0,3(R8)            GET BUYLINE PRD CODE                         
         GOTO1 GETRATE,DMCB,((R0),SPOTS),(R8),(R6)                              
         SPACE 1                                                                
* FIND CORRECT BUCKETS - DATE IS IN DUB(2) *                                    
         SPACE 1                                                                
SS72     LA    R4,SSDATES                                                       
         L     R5,ABUYTOTS                                                      
*                                                                               
SS74     CLC   DUB(2),2(R4)        COMPARE TO MONTH END DATE                    
         BNH   SS76                IF LOW OR EQUAL, POST HERE                   
         LA    R4,4(R4)                                                         
         LA    R5,20(R5)           INTEGER ACCUMS ARE 20 BYTES                  
         CLI   0(R4),0                                                          
         BNE   SS74                                                             
         DC    H'0'                                                             
*                                                                               
SS76     L     R0,0(R5)                                                         
         A     R0,SPOTS                                                         
         ST    R0,0(R5)                                                         
*                                                                               
         LM    R0,R1,4(R5)                                                      
         A     R0,GROSS                                                         
         A     R1,NET                                                           
         STM   R0,R1,4(R5)                                                      
*                                                                               
         OC    4(2,R6),4(R6)       TEST PAID                                    
         BZ    SS62                                                             
*                                                                               
         LM    R0,R1,12(R5)                                                     
         A     R0,GROSS                                                         
         A     R1,NET                                                           
         STM   R0,R1,12(R5)                                                     
*                                                                               
         B     SS62                                                             
         EJECT                                                                  
*                                                                               
*        BUYTOTS ARE TOTALS ACCUMULATED FROM ALL ELEMENTS OF 1 BUY REC          
*        AND ARE ONLY USED TO PUT OUT SPECIAL REP RECORDS INTO THE SORT         
*        IF THE SPECIAL REP OPTION IS ACTIVE.                                   
*                                                                               
* ADD BUYTOTS TO ESTTOTS                                                        
*                                                                               
SS80     DS    0H                                                               
         LA    R0,26                                                            
         L     R4,ABUYTOTS                                                      
         L     R5,AESTTOTS                                                      
*                                                                               
SS82     OC    0(20,R4),0(R4)      TEST FOR DATA                                
         BZ    SS90                NO                                           
*                                                                               
         L     RE,0(R4)                                                         
         A     RE,0(R5)                                                         
         ST    RE,0(R5)                                                         
*                                                                               
         LM    RE,RF,4(R4)                                                      
         A     RE,4(R5)                                                         
         A     RF,8(R5)                                                         
         STM   RE,RF,4(R5)                                                      
*                                                                               
         LM    RE,RF,12(R4)                                                     
         A     RE,12(R5)                                                        
         A     RF,16(R5)                                                        
         STM   RE,RF,12(R5)                                                     
*                                                                               
SS90     LA    R4,20(R4)                                                        
         LA    R5,20(R5)                                                        
         BCT   R0,SS82                                                          
         SPACE 3                                                                
* CLEAR BUY TOTALS *                                                            
         SPACE 1                                                                
SS96     L     R5,ABUYTOTS                                                      
         XC    0(200,R5),0(R5)                                                  
         XC    200(200,R5),200(R5)                                              
         XC    400(120,R5),400(R5)                                              
         B     EXIT                                                             
         EJECT                                                                  
******************************************                                      
*                                        *                                      
* END OF STATION - GENERATE SORT RECORDS *                                      
*                                        *                                      
******************************************                                      
         SPACE 1                                                                
SS100    CLI   QREPTYPE,C'S'       TEST SPECIAL REP REQUEST                     
         BE    SS104               YES                                          
*                                                                               
         BAS   RE,GENKEY                                                        
*                                                                               
         LA    R4,SSDATES                                                       
         L     R5,AESTTOTS                                                      
*                                                                               
SS102    OC    0(20,R5),0(R5)                                                   
         BZ    *+8                                                              
         BAS   RE,GENREC                                                        
         LA    R4,4(R4)                                                         
         LA    R5,20(R5)                                                        
         CLI   0(R4),0                                                          
         BNE   SS102                                                            
*                                                                               
SS104    L     R5,AESTTOTS                                                      
         XC    0(200,R5),0(R5)                                                  
         XC    200(200,R5),200(R5)                                              
         XC    400(120,R5),400(R5)                                              
         B     EXIT                                                             
         EJECT                                                                  
*********************************************                                   
*                                           *                                   
* SUBROUTINE TO GENERATE SORT RECORD KEYS   *                                   
*                                           *                                   
*********************************************                                   
         SPACE 2                                                                
GENKEY   NTR1                                                                   
*                                                                               
         XC    SRREC,SRREC                                                      
         MVC   SRMGR1,XFF                                                       
         MVC   SRMGR2,XFF                                                       
         MVC   SRMGR3,XFF                                                       
         CLI   QMGR,C' '           TEST MKTGRP REQUEST                          
         BE    GENKEY2                                                          
         SR    RE,RE                                                            
         ICM   RE,3,BUYREC+4                                                    
         AR    RE,RE                                                            
         A     RE,SVMKTADR         ADD A(MKTGRP ASSGN TABLE)                    
         OC    0(2,RE),0(RE)                                                    
         BNZ   *+8                                                              
         LA    RE,=X'9999'         POINT TO UNDEFINED MKTGRP                    
         MVC   SRMGR1,0(RE)                                                     
         OC    SRMGR1,MGR1MASK                                                  
         CLC   MGR1LEN,MGR2LEN     TEST 2 LEVELS                                
         BE    GENKEY2             NO                                           
         MVC   SRMGR2,0(RE)                                                     
         OC    SRMGR2,MGR2MASK                                                  
         CLC   MGR2LEN,MGR3LEN     TEST 3 LEVELS                                
         BE    GENKEY2             NO                                           
         MVC   SRMGR3,0(RE)                                                     
*                                                                               
GENKEY2  MVC   SRMKT,BUYREC+4      MARKET                                       
         MVC   SRSTA,XFF           STATION NOT NEEDED FOR THIS REPORT           
*                                                                               
*        CAN NOT SUPPRESS MARKET FOR THIS REPORT                                
*                                                                               
*        CLI   QSUMONLY,C'Y'                                                    
*        BNE   *+10                                                             
*        MVC   SRMKT(5),XFF                                                     
         MVI   SROFC,X'FF'                                                      
         CLI   PROGPROF,C'Y'       TEST OFFICE WITHIN STATION                   
         BNE   GENKEY4                                                          
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         MVC   SROFC,COFFICE                                                    
         DROP  R6                                                               
*                                                                               
GENKEY4  MVC   SRCLT,BUYKEY+1                                                   
         CLI   QOPT5,C'Y'          TEST SUPPRESS CLIENT DETAIL                  
         BNE   GENKEYX                                                          
         MVC   SRCLT,XFF           YES - SUPPRESS CLT (BUT NOT OFC)             
*                                                                               
GENKEYX  B     EXIT                                                             
         EJECT                                                                  
*********************************************                                   
*                                           *                                   
* SUBROUTINE TO GENERATE SORT DATA RECORDS. *                                   
* R4 POINTS TO DATE LIST ENTRY              *                                   
* R5 POINTS TO ACCUMULATOR SET              *                                   
*                                           *                                   
*********************************************                                   
         SPACE 2                                                                
GENREC   NTR1                                                                   
*                                                                               
         L     R0,0(R5)            NUMBER OF SPOTS                              
         CVD   R0,DUB                                                           
         ZAP   SRLSPT,DUB                                                       
*                                                                               
         L     R0,4(R5)            $ ORDERED GROSS                              
         CVD   R0,DUB                                                           
         ZAP   SRLORD,DUB                                                       
*                                                                               
*        L     R0,8(R5)            $ ORDERED NET                                
*        CVD   R0,DUB                                                           
*        ZAP   SRORDN,DUB                                                       
*                                                                               
*        L     R0,12(R5)           $ PAID GROSS                                 
*        CVD   R0,DUB                                                           
*        ZAP   SRPAIDG,DUB                                                      
*                                                                               
*        L     R0,16(R5)           $ PAID NET                                   
*        CVD   R0,DUB                                                           
*        ZAP   SRPAIDN,DUB                                                      
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,2(R4)          GET COMPRESSED YR/MO/DA                      
         SRDL  RE,9                SHIFT MO/DAY                                 
         STC   RE,SRMONYR+1        STORE YEAR                                   
         SRL   RF,28               DROP DAY                                     
         STC   RF,SRMONYR          STORE MONTH                                  
         GOTO1 VSORTER,DMCB,=C'PUT',SRREC                                       
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
****************************                                                    
*                          *                                                    
*  SORT OUTPUT PROCESSING  *                                                    
*                          *                                                    
****************************                                                    
         SPACE 2                                                                
SS200    MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         XC    SVMGR,SVMGR         ZERO SAVED MARKET GROUP                      
         SPACE 1                                                                
* CLEAR COUNTERS *                                                              
         SPACE 1                                                                
         XC    CNTRS,CNTRS                                                      
         SPACE 1                                                                
* GET FIRST SORTED RECORD *                                                     
         SPACE 1                                                                
         GOTO1 VSORTER,DMCB,=C'GET',0                                           
         L     RE,4(R1)            GET ADDR OF 1ST OUTPUT REC                   
         CLI   0(RE),X'FF'         TEST E-O-F ON FIRST READ                     
         BE    SS232X                                                           
         MVC   SRREC(20),0(RE)     STORE 1ST OUTPUT REC KEY IN OUR AREA         
         MVC   SRLSPT(32),=4PL8'0' ZERO CTRS FROM SORT INPUT                    
         MVC   SRNEW,0(RE)         SAVE KEY AND COUNTERS                        
         B     SS204                                                            
         SPACE 2                                                                
* GET NEXT SORTED RECORD *                                                      
         SPACE 1                                                                
SS202    DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'GET',0                                           
         L     RE,4(R1)            GET ADDR OF OUTPUT REC                       
         MVC   SRNEW,0(RE)         MOVE IT TO 'NEXT REC' AREA                   
*                                                                               
         CLC   SRKEY(18),SRNEW     THIS REC EQ TO LAST (EXCEPT YEAR)            
         BNE   SS210               NO, GO DEAL WITH CHANGE OF KEYS              
         SPACE 1                                                                
* KEYS EQUAL - ADD 'NEW' REC TO OTHERS OF SAME KEY                              
         SPACE 1                                                                
* ADD TO THIS YEAR OR LAST YEAR BUCKETS                                         
         SPACE 1                                                                
SS204    LA    R4,SRLSPT           POINT TO THIS YEAR                           
         CLC   SRNEW+18(1),STRTYEAR IS THIS LAST YEAR                           
         BE    SS206               NOPE                                         
         LA    R4,SRTSPT                                                        
         SPACE 1                                                                
SS206    LA    R5,SRLSPT-SRREC+SRNEW                                            
         AP    0(8,R4),0(8,R5)                                                  
         AP    8(8,R4),8(8,R5)                                                  
         B     SS202                                                            
         EJECT                                                                  
*******************************************                                     
*                                         *                                     
* KEYS UNEQUAL - GENERATE BUFFALO RECORDS *                                     
*                                         *                                     
*******************************************                                     
         SPACE 1                                                                
SS210    DS    0H                                                               
         SPACE 1                                                                
* GENERATE CLIENT DETAILS *                                                     
         SPACE 1                                                                
         MVC   BUFFOFC,XFF                                                      
         MVC   BUFFCLT,SRCLT                                                    
         MVC   BUFFMY,SRMONYR                                                   
         MVI   BUFFMY+1,X'FF'      ALWAYS HAVE SAME YEAR                        
         ZAP   BUFLSPT,SRLSPT                                                   
         ZAP   BUFTSPT,SRTSPT                                                   
         LA    R0,2                NUMBER OF CTRS TO ROUND & STORE              
         LA    R4,SRLORD           FIRST CTR TO ROUND & STORE                   
         LA    R5,BUFLORD                                                       
SS214    AP    0(8,R4),=P'50'      ROUND TO NEAREST $                           
         MVN   6(1,R4),7(R4)       DROP CENTS                                   
         ZAP   0(8,R5),0(7,R4)     STORE ROUNDED $                              
         LA    R4,16(,R4)                                                       
         LA    R5,16(,R5)                                                       
         BCT   R0,SS214                                                         
         BAS   RE,PUTBUFF                                                       
         CLI   QOPT5,C'Y'          SUPPRESS CLIENT DETAIL                       
         BE    SS220               YES - DON'T ADD DUP "ALL" CLT TOT            
         SPACE 1                                                                
         MVC   BUFFCLT,XFF         PUT OUT 'ALL' CLIENT TOTALS                  
         BAS   RE,PUTBUFF          ADD TO BUFFALO BY MONTH                      
         EJECT                                                                  
******************************                                                  
*                            *                                                  
*   TEST FOR CONTROL BREAK   *                                                  
*                            *                                                  
******************************                                                  
         SPACE 1                                                                
SS220    CLC   SRKEY+1(13),SRNEW+1 SPR/MGR1-2-3/MKT/STA                         
         BE    SS234                                                            
         BAS   RE,ENDSTA                                                        
*                                                                               
         CLC   SRKEY+1(10),SRNEW+1 SPR/MGR1-2-3/MKT                             
         BE    SS234                                                            
         BAS   RE,ENDMKT                                                        
*                                                                               
SS226    CLC   SRMGR3,XFF         TEST DUMMY MKTGRP                             
         BE    SS228                                                            
         CLC   SRKEY+1(8),SRNEW+1 SPR/MGR1-2-3                                  
         BE    SS234                                                            
         BAS   RE,ENDMGR3                                                       
*                                                                               
SS228    CLC   SRMGR2,XFF         TEST DUMMY MKTGRP                             
         BE    SS230                                                            
         CLC   SRKEY+1(6),SRNEW+1 SPR/MGR1-2                                    
         BE    SS234                                                            
         BAS   RE,ENDMGR2                                                       
*                                                                               
SS230    CLC   SRMGR1,XFF         TEST DUMMY MKTGRP                             
         BE    SS232                                                            
         CLC   SRKEY+1(4),SRNEW+1 SPR/MGR1                                      
         BE    SS234                                                            
         BAS   RE,ENDMGR1                                                       
*                                                                               
SS232    CLC   SRKEY+1(2),SRNEW+1 SPR                                           
         BE    SS234                                                            
*                                                                               
         OC    SRKEY+1(2),SRKEY+1 TEST REP PRESENT                              
         BZ    *+8                NO                                            
         BAS   RE,ENDSPR          ELSE END REP                                  
*                                                                               
         CLI   SRNEW,X'FF'        TEST E-O-F                                    
         BNE   SS234                                                            
         BAS   RE,ENDRPT          YES - REPORT TOTALS NOW                       
SS232X   GOTO1 VSORTER,DMCB,=C'END'                                             
         B     EXIT                                                             
*                                                                               
SS234    MVC   SRREC(20),SRNEW     MOVE NEW REC TO PROCESS                      
         MVC   SRLSPT(32),=4PL8'0' ZERO OUT EXCESS BUCKETS                      
         B     SS204                                                            
         EJECT                                                                  
ENDSTA   NTR1                                                                   
*                                                                               
         MVI   STASW,C'N'          SET STATION NOT PRINTED                      
         MVI   MODE,STALAST                                                     
         MVI   BUFFLVL,1                                                        
*                                                                               
         CLI   QMGR,C' '           TEST MKTGRPS                                 
         BE    ENDSTA4             NO                                           
         LA    R1,SRMGR3                                                        
         CLC   MGR3LEN,MGR2LEN     TEST 3 LEVELS                                
         BNE   ENDSTA2             YES                                          
         LA    R1,SRMGR2                                                        
         CLC   MGR2LEN,MGR1LEN     TEST 2 LEVELS                                
         BNE   ENDSTA2                                                          
         LA    R1,SRMGR1                                                        
*                                                                               
ENDSTA2  BAS   RE,GETMGR                                                        
*                                                                               
ENDSTA4  CLI   QSUMONLY,C'Y'       TEST SUMMARIES ONLY                          
         BE    ENDSTAX                                                          
         SPACE 1                                                                
         BAS   RE,GETMKT           READ MARKET NAME                             
*                                                                               
ENDSTAX  XC    OFCCNTR(8),OFCCNTR                                               
         GOTO1 BUFFALO,DMCB,=C'ADD',BUFFBUFF,1,(X'80',2)                        
         BAS   RE,CLRBUFF                                                       
         B     EXIT                                                             
         EJECT                                                                  
ENDMKT   NTR1                                                                   
*                                                                               
         MVI   STASW,C'N'          SET STATION NOT PRINTED                      
         MVI   MODE,MKTLAST                                                     
         MVI   BUFFLVL,2                                                        
*                                                                               
         CLI   QSUMONLY,C'Y'       TEST SUMMARIES ONLY                          
         BE    ENDMKTX                                                          
*                                                                               
         BAS   RE,SSPRT                                                         
         BNE   ENDMKTX                                                          
*                                                                               
         L     RE,MKTCNTR                                                       
         LA    RE,1(RE)                                                         
         ST    RE,MKTCNTR                                                       
*                                                                               
ENDMKTX  XC    STACNTR,STACNTR                                                  
         SPACE 1                                                                
* ADD TO HIGHER LEVEL ACCUMS AS NEEDED *                                        
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         LA    R1,WORK+12                                                       
         CLI   QMGR,C' '           TEST MKTGRP REQ                              
         BE    ENDMKTX2            NO                                           
         LA    RE,3                                                             
         CLC   MGR3LEN,MGR2LEN     TEST 3 LEVELS                                
         BE    *+12                NO                                           
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RE,4                                                             
         CLC   MGR2LEN,MGR1LEN     TEST 2 LEVELS                                
         BE    *+12                NO                                           
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
*                                                                               
         LA    RE,5                ALWAYS AT LEAST 1 LEVEL                      
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         EJECT                                                                  
ENDMKTX2 LA    RE,7                SET TO ADD TO RPT TOTALS                     
         OC    SRSPR,SRSPR         TEST SPECIAL REP PRESENT                     
         BZ    *+6                                                              
         BCTR  RE,0                SET TO ADD TO REP TOTALS                     
         ST    RE,0(R1)                                                         
*                                                                               
         MVI   0(R1),X'80'         SET EOL FLAG                                 
         GOTO1 BUFFALO,WORK,=C'ADD',BUFFBUFF,2                                  
*                                                                               
         BAS   RE,CLRBUFF                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
ENDMGR3  NTR1                                                                   
*                                                                               
         MVI   STASW,C'N'          SET STATION NOT PRINTED                      
         MVI   FORCEHED,C'Y'                                                    
         MVI   MODE,MGR3LAST                                                    
         MVI   BUFFLVL,3                                                        
         CLC   MKTCNTR,=F'1'                                                    
         BE    *+12                                                             
         BAS   RE,SSPRT                                                         
         BNE   ENDMGR3X                                                         
*                                                                               
         L     RE,MG3CNTR                                                       
         LA    RE,1(RE)                                                         
         ST    RE,MG3CNTR                                                       
*                                                                               
ENDMGR3X XC    MKTCNTR(8),MKTCNTR                                               
         BAS   RE,CLRBUFF                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         SPACE 2                                                                
ENDMGR2  NTR1                                                                   
*                                                                               
         MVI   STASW,C'N'          SET STATION NOT PRINTED                      
         MVI   FORCEHED,C'Y'                                                    
         MVI   MODE,MGR2LAST                                                    
         MVI   BUFFLVL,4                                                        
         LA    R1,MG3CNTR                                                       
         CLC   MGR3LEN,MGR2LEN     TEST 3 MKTGRP LEVELS                         
         BNE   *+8                 YES                                          
         LA    R1,MKTCNTR                                                       
         CLC   0(4,R1),=F'1'                                                    
         BE    *+12                                                             
         BAS   RE,SSPRT                                                         
         BNE   ENDMGR2X                                                         
*                                                                               
         L     RE,MG2CNTR                                                       
         LA    RE,1(RE)                                                         
         ST    RE,MG2CNTR                                                       
*                                                                               
ENDMGR2X XC    MG3CNTR(12),MG3CNTR                                              
         BAS   RE,CLRBUFF                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
ENDMGR1  NTR1                                                                   
*                                                                               
         MVI   STASW,C'N'          SET STATION NOT PRINTED                      
         MVI   FORCEHED,C'Y'                                                    
         MVI   MODE,MGR1LAST                                                    
         MVI   BUFFLVL,5                                                        
         LA    R1,MG2CNTR                                                       
         CLC   MGR2LEN,MGR1LEN     TEST 2 MKTGRP LEVELS                         
         BNE   *+8                 YES                                          
         LA    R1,MKTCNTR                                                       
         CLC   0(4,R1),=F'1'                                                    
         BE    *+12                                                             
         BAS   RE,SSPRT                                                         
         BNE   ENDMGR1X                                                         
*                                                                               
ENDMGR1X XC    MG2CNTR(16),MG2CNTR                                              
         BAS   RE,CLRBUFF                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         SPACE 2                                                                
ENDSPR   NTR1                                                                   
         MVI   STASW,C'N'          SET STATION NOT PRINTED                      
         MVI   MODE,REQLAST                                                     
         MVI   BUFFLVL,6                                                        
         BAS   RE,SSPRT                                                         
*                                                                               
ENDSPRX  MVI   FORCEHED,C'Y'                                                    
         BAS   RE,CLRBUFF                                                       
         B     EXIT                                                             
         SPACE 2                                                                
ENDRPT   NTR1                                                                   
         MVI   STASW,C'N'          SET STATION NOT PRINTED                      
         CLI   QREPTYPE,C'S'       TEST SPECIAL REP REQUEST                     
         BE    ENDRPTX             YES - NO REPORT TOTALS                       
         CLC   =CL3'ALL',QMKT                                                   
         BNE   ENDRPTX                                                          
         CLC   =CL3'ALL',QSTA                                                   
         BNE   ENDRPTX                                                          
         SPACE 1                                                                
* PRINT REPORT TOTALS *                                                         
         SPACE 1                                                                
         MVI   MODE,REQLAST                                                     
         MVI   BUFFLVL,7                                                        
         MVC   SRKEY,XFF           SET KEY FOR REPORT TOTALS                    
         BAS   RE,SSPRT                                                         
*                                                                               
ENDRPTX  BAS   RE,CLRBUFF                                                       
         B     EXIT                                                             
         EJECT                                                                  
SSPRT    NTR1                                                                   
*                                                                               
         LA    R2,P                                                             
         USING PLINED,R2                                                        
*                                                                               
SSPRT1X  XC    CLTCNTR,CLTCNTR                                                  
         XC    OFCCNTR,OFCCNTR                                                  
*                                                                               
         MVI   BUFFEOF,C'N'                                                     
         XC    BUFFCNTR,BUFFCNTR                                                
         MVC   BUFFTOTS,=4PL8'0'                                                
         L     R4,ABUFFER                                                       
*                                                                               
         BAS   RE,HIGHBUFF                                                      
         BNE   EXIT                EXIT WITH CC NOT EQ                          
         MVC   SVBUFF,BUFFOFC      SAVE BUFFALO KEY                             
         B     SSPRT4                                                           
*                                                                               
SSPRT2   BAS   RE,SEQBUFF                                                       
         BE    SSPRT2X                                                          
         MVI   BUFFEOF,C'Y'        SET EOF FLAG                                 
         B     SSPRT8                                                           
*                                                                               
SSPRT2X  CLC   BUFFOFC(3),SVBUFF   TEST SAME OFFICE/CLIENT                      
         BNE   SSPRT8                                                           
         SPACE 1                                                                
* MOVE REC TO BUFFER *                                                          
         SPACE 1                                                                
SSPRT4   MVC   0(L'BUFFREC,R4),BUFFOFC MOVE OFC/CLT/MON/YR/DATA                 
         LA    R4,L'BUFFREC(,R4)                                                
         SPACE 1                                                                
* BUMP COUNT OF RECORDS IN BUFFER                                               
         SPACE 1                                                                
         LH    RE,BUFFCNTR                                                      
         LA    RE,1(RE)                                                         
         STH   RE,BUFFCNTR                                                      
         SPACE 1                                                                
* ADD TO TOTALS *                                                               
         SPACE 1                                                                
         LA    R1,BUFFTOTS                                                      
         AP    0(8,R1),BUFLSPT     LAST YEARS SPOTS                             
         AP    8(8,R1),BUFLORD     LAST YEARS ORDERS                            
         AP    16(8,R1),BUFTSPT    THIS YEARS SPOTS                             
         AP    24(8,R1),BUFTORD    THIS YEARS ORDERS                            
*                                                                               
         B     SSPRT2                                                           
         EJECT                                                                  
* PRINT CONTENTS OF BUFFER *                                                    
         SPACE 1                                                                
SSPRT8   MVI   PSTAR,C' '                                                       
*                                                                               
         CLC   SVBUFFCL,XFF        TEST CLIENT TOTALS                           
         BNE   *+14                                                             
         CLC   CLTCNTR,=F'1'       TEST EXACTLY ONE CLIENT                      
         BE    SSPRT32             YES - SKIP                                   
*                                                                               
         CLI   SVBUFFOF,X'FF'      TEST OFFICE TOTALS                           
         BNE   *+14                                                             
         CLC   OFCCNTR,=F'1'       TEST EXACTLY ONE OFFICE                      
         BE    SSPRT32             YES - SKIP                                   
         SPACE 1                                                                
* DECODE CLIENT *                                                               
         SPACE 1                                                                
SSPRT10  CLI   QCLT,C'*'           TEST OFFICE REQUEST                          
         BE    *+14                                                             
         CLC   =CL3'ALL',QCLT      TEST ALL CLIENT REQUEST                      
         BNE   SSPRT14             NO - ALWAYS PRINT CLIENT                     
         CLC   SVBUFFCL,XFF        TEST POSSIBLE OFFICE TOTAL                   
         BNE   SSPRT14             NO                                           
         CLC   QOPT1,BUFFLVL       THIS LEVEL PRINTING DETAIL                   
         BNH   SSPRT12             YES, NO NEED FOR EXTRA LINE                  
         MVI   P1,0                FORCE BLANK LINE TO PRINT                    
         LA    R2,P2                                                            
SSPRT12  MVC   PCLT,=CL3'ALL'                                                   
         CLI   SVBUFFOF,X'FF'      TEST ALL OFFICE TOTAL                        
         BE    SSPRT20             YES                                          
         SPACE 1                                                                
* PRINT OFFICE NUMBER *                                                         
         SPACE 1                                                                
         MVC   PCLT,=C'OFF'                                                     
         MVC   CLT(1),SVBUFFOF                                                  
         B     SSPRT20                                                          
*                                                                               
SSPRT14  MVC   PCLT,SPACES                                                      
         GOTO1 CLUNPK,DMCB,SVBUFFCL,PCLT                                        
*                                                                               
SSPRT20  LH    RE,BUFFCNTR                                                      
         LA    RE,2(RE)                                                         
         STC   RE,ALLOWLIN                                                      
*                                                                               
         L     R4,ABUFFER                                                       
         LH    R5,BUFFCNTR                                                      
         LTR   R5,R5               TEST ANY LINES IN BUFFER                     
         BP    SSPRT22                                                          
         CLC   BUFFTOTS,=4PL8'0'   TEST ANY TOTALS                              
         BE    SSPRT30             NO                                           
         B     SSPRT24             GO PRINT TOTALS                              
         EJECT                                                                  
         USING BRECD,R4                                                         
SSPRT22  DC    0H'0'                                                            
         AP    LYCUMORD,BRECLORD   ADD LAST YEARS ORDER $ TO CUMM               
         AP    LYCUMSPT,BRECLSPT   ADD LAST YEARS SPOTS TO CUMM                 
         AP    TYCUMORD,BRECTORD   ADD THIS YEARS ORDER $ TO CUMM               
         AP    TYCUMSPT,BRECTSPT   ADD THIS YEARS SPOTS TO CUME                 
*                                                                               
         CLC   QOPT1,BUFFLVL       THIS LESS THAN BUFF LEVEL FOR DETAIL         
         BNH   SSPRT23A            YES, PRT ALL MONTHS                          
*                                                                               
         CLC   SVBUFFCL,XFF        THIS 'ALL' TOTALS                            
         BE    SSPRT23A            YES, PRINT ALL MONTHS FOR ALL LEVELS         
*                                                                               
         MVI   ALLOWLIN,1          RESET ALLOWLIN TO 1                          
         CLC   BRECMY(1),CURMON    CURRENT MONTH?                               
         BL    SSPRT23B            NO BYPASS PRINT                              
         BE    SSPRT23A            YES, PRINT IT                                
*                                                                               
* HAVE TO CATCH NO CURRENT MONTH, BUT ONLY HIGHER                               
*                                                                               
         CLI   STASW,C'Y'          HAS CLIENT BEEN PRINTED                      
         BE    SSPRT23B            YES DO NOT PRINT AGAIN                       
         SP    LYCUMORD,BRECLORD   SUB LAST YEARS ORDER $ TO CUMM               
         SP    LYCUMSPT,BRECLSPT   SUB LAST YEARS SPOTS TO CUMM                 
         SP    TYCUMORD,BRECTORD   SUB THIS YEARS ORDER $ TO CUMM               
         SP    TYCUMSPT,BRECTSPT   SUB THIS YEARS SPOTS TO CUME                 
         ZAP   BRECLORD,=P'0'      ZERO OUT THIS MONTH                          
         ZAP   BRECLSPT,=P'0'                                                   
         ZAP   BRECTORD,=P'0'                                                   
         ZAP   BRECTSPT,=P'0'                                                   
         MVC   BRECMY(1),CURMON    & MAKE LOOK LIKE CURR MON                    
*                                                                               
SSPRT23A BAS   RE,SSFMT                                                         
         LA    R2,P1               RESTORE PRINTER LINE POINTER                 
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVI   STASW,C'Y'          SET STATION PRINTED FLAG                     
*                                                                               
SSPRT23B LA    R4,L'BUFFREC(,R4)                                                
         BCT   R5,SSPRT22                                                       
         EJECT                                                                  
* HAVE TO CK IF ALL ACTIVITY WAS PRIOR TO CURRENT MONTH                         
         SPACE 1                                                                
         CLC   QOPT1,BUFFLVL       TEST PRINT CLIENT DETAIL OPTION              
         BNH   SSPRT23E            YES - ALL MONTHS WERE PRINTED                
*                                                                               
         CLC   SVBUFFCL,XFF        THIS 'ALL' TOTALS                            
         BE    SSPRT23E            YES, ALL MONTHS WERE PRINTED                 
*                                                                               
         CLI   STASW,C'Y'          WAS CLIENT PRINTED                           
         BE    SSPRT23E            YES                                          
         ZAP   BRECLORD,=P'0'      ZERO OUT THIS MONTH                          
         ZAP   BRECLSPT,=P'0'                                                   
         ZAP   BRECTORD,=P'0'                                                   
         ZAP   BRECTSPT,=P'0'                                                   
         MVC   BRECMY(1),CURMON    & MAKE LOOK LIKE CURR MON                    
         BAS   RE,SSFMT                                                         
         GOTO1 REPORT                                                           
         MVI   STASW,C'Y'          SET STATION PRINTED FLAG                     
SSPRT23E MVC   PCLT,SPACES                                                      
         CLC   BUFFCNTR,=H'1'                                                   
         BE    SSPRT30                                                          
*                                                                               
SSPRT24  L     R4,ABUFFER          POINT TO FIRST LINE IN BUFFER                
*                                                                               
         CLC   QOPT1,BUFFLVL       THIS LESS THAN BUFF LEVEL FOR DETAIL         
         BH    SSPRT30             YES, BYPASS TOTAL PRINTING                   
         MVC   3(2,R4),XFF                                                      
         MVC   5(40,R4),BUFFTOTS                                                
         MVI   PSTAR,C'*'                                                       
*                                                                               
         BAS   RE,SSFMT                                                         
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
SSPRT30  DS    0H                                                               
         ZIC   R0,MAXLINES                                                      
         BCTR  R0,0                                                             
         CLM   R0,1,LINE                                                        
         BNH   SSPRT32                                                          
         CLC   QOPT1,BUFFLVL       THIS LESS THAN BUFF LEVEL FOR DETAIL         
         BH    SSPRT32             YES, BYPASS BLANK LINES                      
         CLI   STASW,C'Y'          NO BLK LINE IF NOTHING PRINTED               
         BNE   SSPRT32                                                          
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
SSPRT32  MVI   STASW,C'N'                                                       
         MVC   LYCUMORD(32),=4PL8'0'                                            
         CLI   BUFFEOF,C'Y'        TEST REACHED EOF                             
         BE    SSPRTX                                                           
         EJECT                                                                  
* BUMP COUNTERS *                                                               
         SPACE 1                                                                
         CLI   PROGPROF,C'Y'       TEST OFFICES                                 
         BNE   SSPRT34             NO                                           
         CLC   SVBUFFOF,BUFFOFC    TEST CHANGE OF OFFICE                        
         BE    SSPRT34             NO                                           
         L     RE,OFCCNTR                                                       
         LA    RE,1(RE)                                                         
         ST    RE,OFCCNTR                                                       
*                                                                               
SSPRT34  CLC   SVBUFFCL,BUFFCLT    TEST CHANGE OF CLIENT                        
         BE    SSPRT36             NO                                           
         CLC   SVBUFFCL,XFF        TEST ALL CLIENT DATA                         
         BE    SSPRT36                                                          
         L     RE,CLTCNTR                                                       
         LA    RE,1(RE)                                                         
         ST    RE,CLTCNTR                                                       
         SPACE 1                                                                
* RESET CORE BUFFER PARAMETERS *                                                
         SPACE 1                                                                
SSPRT36  L     R4,ABUFFER                                                       
         XC    BUFFCNTR,BUFFCNTR                                                
         MVC   BUFFTOTS,=4PL8'0'                                                
         MVC   SVBUFF,BUFFOFC      SAVE NEW OFC/CLT                             
*                                                                               
         CLI   PROGPROF,C'Y'       TEST OFFICES                                 
         BNE   SSPRT4                                                           
         CLC   BUFFOFC(3),XFF      TEST ALL OFFICE/ALL CLIENT TOTAL             
         BNE   SSPRT4              NO                                           
         CLC   OFCCNTR,=F'1'       TEST 1 ACTIVE OFFICE                         
         BNE   SSPRT4              NO - GO PRINT                                
*                                                                               
SSPRTX   CR    RE,RE               EXIT WITH CC EQ                              
         B     EXIT                                                             
         EJECT                                                                  
* ON ENTRY R4 POINTS TO LINE IN BUFFER *                                        
         SPACE 1                                                                
SSFMT    NTR1                                                                   
         USING BRECD,R4                                                         
*                                                                               
         CLC   BRECMY,=X'0101'     TEST PRIOR                                   
         BNE   *+14                                                             
         MVC   PMONTH(3),=C'PRI'                                                
         B     SSFMT20                                                          
*                                                                               
         CLC   BRECMY,=X'FEFE'                                                  
         BNE   *+14                                                             
         MVC   PMONTH(3),=C'SUB'                                                
         B     SSFMT20                                                          
*                                                                               
         CLC   BRECMY,=X'FFFF'                                                  
         BNE   *+14                                                             
         MVC   PMONTH(3),=C'TOT'                                                
         B     SSFMT20                                                          
*                                                                               
         MVI   PZERO,C' '                                                       
         CLC   CURMON,BRECMY       THIS CURRENT MONTH                           
         BL    SSFMT10                                                          
         MVI   PZERO,C'0'                                                       
SSFMT10  MVC   WORK(1),BRECMY+1    PUT IN YEAR                                  
         MVC   WORK+1(1),BRECMY    AND MONTH                                    
         GOTO1 DATCON,DMCB,(3,WORK),(6,WDATE)                                   
         MVC   PMONTH,WDATE        ONLY PRINT MONTH OF MONTH/YEAR               
*                                                                               
*        ADD TO CUMM (YEAR TO DATE) CTRS                                        
*                                                                               
* EDIT LAST/THIS YEARS MONTHLY ORDERED $ & INDEX                                
*                                                                               
SSFMT20  EDIT  (P8,BRECLORD),(11,PLYMORD),0,COMMAS=YES                          
         CP    BRECLORD,=P'0'                                                   
         BL    *+20                                                             
         BH    *+10                                                             
         MVC   PLYMORD+L'PLYMORD-1(1),PZERO                                     
         MVC   PLYMORD+L'PLYMORD(1),PSTAR                                       
*                                                                               
         EDIT  (P8,BRECTORD),(11,PTYMORD),0,COMMAS=YES                          
         CP    BRECTORD,=P'0'                                                   
         BL    *+20                                                             
         BH    *+10                                                             
         MVC   PTYMORD+L'PTYMORD-1(1),PZERO                                     
         MVC   PTYMORD+L'PTYMORD(1),PSTAR                                       
*                                                                               
         CP    BRECLORD,=P'0'                                                   
         BE    SSFMT30                                                          
         ZAP   WORK(16),BRECTORD                                                
         MP    WORK(16),=P'1000'                                                
         DP    WORK(16),BRECLORD                                                
         ZAP   DUB,WORK(8)                                                      
         SR    RE,RE                                                            
         CVB   RF,DUB                                                           
         A     RF,=F'5'                                                         
         D     RE,=F'10'                                                        
*                                                                               
         C     RF,=F'0'                                                         
         BE    SSFMT30                                                          
         EDIT  (RF),(3,PC$INX)                                                  
         MVC   PC$INX+3(1),PSTAR                                                
         C     RF,=F'999'          IF PERCENT HIGHER THAN 999                   
         BNH   SSFMT30                                                          
         MVC   PC$INX(3),=CL3'HI '                                              
*                                                                               
SSFMT30  DC    0H'0'                                                            
*                                                                               
* EDIT LAST/THIS YEARS MONTHLY TRANS & INDEX                                    
*                                                                               
         EDIT  (P8,BRECLSPT),(6,PLYMTRN),0,COMMAS=NO,MINUS=NO                   
         CP    BRECLSPT,=P'0'                                                   
         BL    *+20                                                             
         BH    *+10                                                             
         MVC   PLYMTRN+L'PLYMTRN-1(1),PZERO                                     
         MVC   PLYMTRN+L'PLYMTRN(1),PSTAR                                       
*                                                                               
         EDIT  (P8,BRECTSPT),(6,PTYMTRN),0,COMMAS=NO,MINUS=NO                   
         CP    BRECTSPT,=P'0'                                                   
         BL    *+20                                                             
         BH    *+10                                                             
         MVC   PTYMTRN+L'PTYMTRN-1(1),PZERO                                     
         MVC   PTYMTRN+L'PTYMTRN(1),PSTAR                                       
*                                                                               
         CP    BRECLSPT,=P'0'                                                   
         BE    SSFMT40                                                          
         ZAP   WORK(16),BRECTSPT                                                
         MP    WORK(16),=P'1000'                                                
         DP    WORK(16),BRECLSPT                                                
         ZAP   DUB,WORK(8)                                                      
         SR    RE,RE                                                            
         CVB   RF,DUB                                                           
         A     RF,=F'5'                                                         
         D     RE,=F'10'                                                        
*                                                                               
         C     RF,=F'0'                                                         
         BE    SSFMT40                                                          
         EDIT  (RF),(3,PCTRINX)                                                 
         MVC   PCTRINX+3(1),PSTAR                                               
         C     RF,=F'999'          IF PERCENT HIGHER THAN 999                   
         BNH   SSFMT40                                                          
         MVC   PCTRINX(3),=CL3'HI '                                             
SSFMT40  DC    0H'0'                                                            
*                                                                               
* EDIT LAST/THIS YEARS CUME ORDERED $ & INDEX                                   
*                                                                               
         EDIT  (P8,LYCUMORD),(11,PLYYORD),0,COMMAS=YES                          
         CP    LYCUMORD,=P'0'                                                   
         BL    *+20                                                             
         BH    *+10                                                             
         MVC   PLYYORD+L'PLYYORD-1(1),PZERO                                     
         MVC   PLYYORD+L'PLYYORD(1),PSTAR                                       
*                                                                               
         CLC   CURMON,BRECMY       THIS CURRENT MONTH                           
         BL    SSFMT44                                                          
         EDIT  (P8,TYCUMORD),(11,PTYYORD),0,COMMAS=YES                          
         CP    TYCUMORD,=P'0'                                                   
         BL    *+20                                                             
         BH    *+10                                                             
         MVC   PTYYORD+L'PTYYORD-1(1),PZERO                                     
         MVC   PTYYORD+L'PTYYORD(1),PSTAR                                       
*                                                                               
SSFMT44  DC    0H'0'                                                            
         CP    LYCUMORD,=P'0'                                                   
         BE    SSFMT50                                                          
         ZAP   WORK(16),TYCUMORD                                                
         MP    WORK(16),=P'1000'                                                
         DP    WORK(16),LYCUMORD                                                
         ZAP   DUB,WORK(8)                                                      
         SR    RE,RE                                                            
         CVB   RF,DUB                                                           
         A     RF,=F'5'                                                         
         D     RE,=F'10'                                                        
*                                                                               
         C     RF,=F'0'                                                         
         BE    SSFMT50                                                          
         EDIT  (RF),(3,PY$INX)                                                  
         MVC   PY$INX+3(1),PSTAR                                                
         C     RF,=F'999'          IF PERCENT HIGHER THAN 999                   
         BNH   SSFMT50                                                          
         MVC   PY$INX(3),=CL3'HI '                                              
*                                                                               
SSFMT50  DC    0H'0'                                                            
*                                                                               
         EDIT  (P8,LYCUMSPT),(7,PLYYTRN),0,COMMAS=NO,MINUS=NO                   
         CP    LYCUMSPT,=P'0'                                                   
         BL    *+20                                                             
         BH    *+10                                                             
         MVC   PLYYTRN+L'PLYYTRN-1(1),PZERO                                     
         MVC   PLYYTRN+L'PLYYTRN(1),PSTAR                                       
*                                                                               
         CLC   CURMON,BRECMY       THIS CURRENT MONTH                           
         BL    SSFMT54                                                          
         EDIT  (P8,TYCUMSPT),(7,PTYYTRN),0,COMMAS=NO,MINUS=NO                   
         CP    TYCUMSPT,=P'0'                                                   
         BL    *+20                                                             
         BH    *+10                                                             
         MVC   PTYYTRN+L'PTYYTRN-1(1),PZERO                                     
         MVC   PTYYTRN+L'PTYYTRN(1),PSTAR                                       
*                                                                               
SSFMT54  DC    0H'0'                                                            
         CP    LYCUMSPT,=P'0'                                                   
         BE    SSFMT60                                                          
         ZAP   WORK(16),TYCUMSPT                                                
         MP    WORK(16),=P'1000'                                                
         DP    WORK(16),LYCUMSPT                                                
         ZAP   DUB,WORK(8)                                                      
         SR    RE,RE                                                            
         CVB   RF,DUB                                                           
         A     RF,=F'5'                                                         
         D     RE,=F'10'                                                        
*                                                                               
         C     RF,=F'0'                                                         
         BE    SSFMT60                                                          
         EDIT  (RF),(3,PYTRINX)                                                 
         MVC   PYTRINX+3(1),PSTAR                                               
         C     RF,=F'999'          IF PERCENT HIGHER THAN 999                   
         BNH   SSFMT60                                                          
         MVC   PYTRINX(3),=CL3'HI '                                             
SSFMT60  EQU   *                                                                
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP R2                                                                
         EJECT                                                                  
* SUBROUTINE TO TRACE SORT CALLS *                                              
         SPACE 1                                                                
TRCSORT  NTR1                                                                   
*                                                                               
         L     RE,0(R1)                                                         
         CLI   0(RE),C'P'          TEST PUT                                     
         BNE   TRCSORT4                                                         
         L     R4,4(R1)                                                         
         GOTO1 VPRNTBL,TRCP1,=C'PUT',(R4),C'DUMP',56,=C'1D'                     
*                                                                               
TRCSORT2 L     RF,TRCSAVE                                                       
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
TRCSORT4 CLI   0(RE),C'G'          TEST GET                                     
         BNE   TRCSORT2                                                         
         L     RF,TRCSAVE          GO TO SORTER FOR RECORD                      
         BASR  RE,RF                                                            
         L     R4,4(R1)            GET OUTPUT REC ADDRESS                       
         GOTO1 VPRNTBL,TRCP1,=C'GET',(R4),C'DUMP',56,=C'1D'                     
         B     EXIT                                                             
*                                                                               
TRCSAVE  DC    A(0)                SAVE AREA FOR A(SORTER)                      
TRCP1    DC    6F'0'                                                            
         SPACE 2                                                                
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   0(1,R6),ELCDLO                                                   
         BL    NEXTEL                                                           
         CLC   0(1,R6),ELCDHI                                                   
         BH    NEXTEL                                                           
         CR    RE,RE                                                            
         BR    RE                  EXIT WITH CC EQUAL                           
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  EXIT WITH CC NOT EQUAL                       
         EJECT                                                                  
* READ NEW MARKET RECORD *                                                      
         SPACE 1                                                                
GETMKT   NTR1                                                                   
         CLC   SRMKT,BMKT          TEST SAME MKT AS PREVIOUS                    
         BE    EXIT                                                             
         MVC   BMKT,SRMKT          SAVE IT                                      
         CLC   SRMKT,XFF                                                        
         BE    EXIT                                                             
         SPACE 1                                                                
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         SR    R0,R0                                                            
         ICM   R0,3,SRMKT                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB                                                     
         MVC   KEY+6(2),QAGY                                                    
         GOTO1 HIGHMKT                                                          
         L     R6,ADMARKET                                                      
         USING MKTRECD,R6                                                       
         CLC   KEY(8),0(R6)                                                     
         BE    *+10                                                             
         MVC   MKTNAME,=CL24'*** UNKNOWN ***'                                   
         B     EXIT                                                             
         EJECT                                                                  
* READ NEW MKTGRP NAMES -- R1 POINTS TO BINARY MKTGRP *                         
         SPACE 1                                                                
GETMGR   NTR1                                                                   
         CLC   0(2,R1),SVMGR       TEST SAME MKTGRP                             
         BE    EXIT                                                             
         CLC   0(2,R1),XFF         TEST DUMMY MKTGRP                            
         BE    EXIT                                                             
         MVC   SVMGR,0(R1)         SAVE THIS MKTGRP                             
         SPACE 1                                                                
* READ NEW MKTGRP *                                                             
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+8(3),QMGR                                                    
         CLC   =X'9999',SVMGR                                                   
         BE    GETMGR2                                                          
         MVC   KEY+9(2),SVMGR                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETMKTGR                                                         
         B     EXIT                                                             
*                                                                               
GETMGR2  CLI   MGRLEN,1                                                         
         BNE   GETMGR4                                                          
*                                                                               
         MVC   MGR1N+1(4),=C'9999'                                              
         MVC   MGR2N+1(4),=C'9999'                                              
         MVC   MGR3N+1(4),=C'9999'                                              
         B     GETMGR6                                                          
*                                                                               
GETMGR4  MVC   MGR1N+2(4),=C'9999'                                              
         MVC   MGR2N+2(4),=C'9999'                                              
         MVC   MGR3N+2(4),=C'9999'                                              
*                                                                               
GETMGR6  MVC   MGR1NM,=CL24'*** UNDEFINED ***'                                  
         MVC   MGR2NM,=CL24'*** UNDEFINED ***'                                  
         MVC   MGR3NM,=CL24'*** UNDEFINED ***'                                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
PUTBUFF  MVI   BUFFLVL,0                                                        
         LA    R1,=C'PUT'                                                       
         B     BUFFX                                                            
*                                                                               
HIGHBUFF DS    0H                                                               
         XC    BUFFREC(5),BUFFREC  CLEAR KEY FOR RDHI                           
         LA    R1,=C'HIGH'                                                      
         B     BUFFX                                                            
*                                                                               
SEQBUFF  LA    R1,=C'SEQ'                                                       
         B     BUFFX                                                            
*                                                                               
BUFFX    NTR1                                                                   
         ST    R1,DMCB             SET COMMAND ADDR                             
*                                                                               
         ZIC   R0,BUFFLVL                                                       
         GOTO1 BUFFALO,DMCB,,BUFFBUFF,BUFFREC,(R0)                              
         TM    8(R1),X'80'         SET CC ON EXIT                               
         B     EXIT                                                             
*                                                                               
CLRBUFF  NTR1                                                                   
*                                                                               
         ZIC   R0,BUFFLVL                                                       
         GOTO1 BUFFALO,DMCB,=C'CLEAR',BUFFBUFF,(X'80',(R0))                     
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
******************************                                                  
*                            *                                                  
*     HEADHOOK PROCESSING    *                                                  
*                            *                                                  
******************************                                                  
         SPACE 1                                                                
         DROP  RB,RC                                                            
         DS    0F                                                               
         USING *,RF                                                             
SSHDHK   NTR1                                                                   
         LM    R9,RC,HDHKR9                                                     
         B     HDHK0                                                            
HDHKR9   DC    4F'0'                                                            
         DROP  RF                                                               
         USING SPA702,RB,RC                                                     
*                                                                               
HDHK0    MVC   H3+39(32),HLDATES                                                
         MVC   H9+16(4),STPRTYR                                                 
         MVC   H9+28(4),EDPRTYR                                                 
         MVC   H9+40(4),STPRTYR                                                 
         MVC   H9+47(4),EDPRTYR                                                 
         MVC   H9+64(4),STPRTYR                                                 
         MVC   H9+76(4),EDPRTYR                                                 
         MVC   H9+89(4),STPRTYR                                                 
         MVC   H9+97(4),EDPRTYR                                                 
*                                                                               
HDHK1    CLC   QREP,SPACES         TEST REP REQUEST                             
         BE    HDHK4               NO                                           
         MVC   FULL(3),QREP                                                     
         SPACE 1                                                                
* GET REP NAME *                                                                
         SPACE 1                                                                
HDHK2    XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(3),FULL                                                    
         MVC   KEY+5(2),QAGY                                                    
*                                                                               
         L     R6,ADREP                                                         
         USING REPRECD,R6                                                       
         CLC   KEY(7),0(R6)        TEST SAME AS PREVIOUS                        
         BE    HDHK3                                                            
*                                                                               
         GOTO1 HIGHREP                                                          
*                                                                               
HDHK3    MVC   WORK,SPACES                                                      
         MVC   WORK(11),=C'SPECIAL REP'                                         
         OC    SRSPR,SRSPR             TEST SPECIAL REP                         
         BNZ   *+10                    YES                                      
         MVC   WORK(11),=CL11'PAYING REP'                                       
*                                                                               
         MVC   WORK+12(3),KEY+2                                                 
         MVC   WORK+16(13),=C'** UNKNOWN **'                                    
         CLC   KEY(7),0(R6)                                                     
         BNE   *+10                                                             
         MVC   WORK+16(22),RNAME                                                
         GOTO1 SQUASHER,DMCB,WORK,40                                            
         MVC   H6+76(33),WORK                                                   
         DROP  R6                                                               
*                                                                               
HDHK4    MVC   WORK,SPACES                                                      
         CLC   =CL3'ALL',QCLT                                                   
         BE    HDHK10                                                           
*                                                                               
         CLI   QCLT,C'*'           TEST OFFICES                                 
         BNE   HDHK8               NO                                           
         CLI   QCLT+1,C'-'         TEST NEGATIVE FILTER                         
         BE    HDHK6                                                            
         MVC   WORK(27),=C'** OFFICE X CLIENTS ONLY **'                         
         MVC   WORK+10(1),QCLT+1                                                
         B     HDHK10X                                                          
*                                                                               
HDHK6    MVC   WORK(33),=C'** ALL CLIENTS EXCEPT OFFICE X **'                   
         MVC   WORK+29(1),QCLT+2                                                
         B     HDHK10X                                                          
*                                                                               
HDHK8    MVC   WORK(21),=C'** CLIENT XXX ONLY **'                               
         MVC   WORK+10(3),QCLT                                                  
*                                                                               
HDHK10   CLI   QOPT7,C' '                                                       
         BE    HDHK10X                                                          
         MVC   WORK(25),=C'CLIENT EXCLUSION CODE = X'                           
         MVC   WORK+24(1),QOPT7                                                 
*                                                                               
HDHK10X  CLC   WORK,SPACES                                                      
         BE    HDHK12                                                           
         GOTO1 SQUASHER,DMCB,WORK,33                                            
         GOTO1 CENTER,DMCB,WORK,33                                              
         MVC   H4+39(33),WORK                                                   
*                                                                               
HDHK12   CLI   QOPT6,C'1'                                                       
         BNE   HDHK14                                                           
         MVC   H5+39(31),=C'UNPAID DETAILS + ORDERED TOTALS'                    
         EJECT                                                                  
HDHK14   CLI   QOPT6,C'2'                                                       
         BNE   HDHK16                                                           
         MVC   H5+43(23),=C'** UNPAID ITEMS ONLY **'                            
*                                                                               
HDHK16   CLI   MODE,MKTLAST                                                     
         BH    HDHK20                                                           
         CLC   SRMKT,XFF                                                        
         BE    HDHK20                                                           
*                                                                               
HDHK18   MVC   WORK,SPACES                                                      
         MVC   WORK(6),=C'MARKET'                                               
         L     R6,ADMARKET                                                      
         USING MKTRECD,R6                                                       
         MVC   WORK+8(4),MKTKMKT                                                
         MVC   WORK+13(24),MKTNAME                                              
         OC    WORK,SPACES                                                      
         GOTO1 SQUASHER,DMCB,WORK,38                                            
         GOTO1 CENTER,DMCB,WORK,38                                              
         MVC   H6+36(38),WORK                                                   
*                                                                               
         DROP  R6                                                               
         SPACE 1                                                                
* REMOVE MKTNAME UNFORTUNATELY PRINTED BY MGROUP SSPEC *                        
         SPACE 1                                                                
HDHK20   LA    R0,4                                                             
         LA    R1,H7                                                            
HDHK22   CLC   =C'MARKET',0(R1)                                                 
         BE    HDHK24                                                           
         SH    R1,=H'132'                                                       
         BCT   R0,HDHK22                                                        
         B     *+10                                                             
HDHK24   MVC   0(37,R1),SPACES                                                  
*                                                                               
*        CLI   STASW,C'Y'          TEST STATION NAME REQUIRED                   
*        BNE   HDHK26                                                           
*        MVC   H11+1(11),=C'(CONTINUED)'                                        
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         DROP  R2                                                               
         EJECT                                                                  
* PRINT AFFILIATE FILTER *                                                      
         SPACE 1                                                                
HDHK26   CLI   QAFFIL,C' '                                                      
         BE    HDHK26X                                                          
         MVC   H7+1(25),=C'** NBC AFFILIATES ONLY **'                           
         CLI   QAFFIL,C'N'                                                      
         BE    HDHK26X                                                          
         MVC   H7+4(3),=C'ABC'                                                  
         CLI   QAFFIL,C'A'                                                      
         BE    HDHK26X                                                          
         MVC   H7+4(3),=C'CBS'                                                  
         CLI   QAFFIL,C'C'                                                      
         BE    HDHK26X                                                          
         MVC   H7+4(3),=C'IND'                                                  
         CLI   QAFFIL,C'I'                                                      
         BE    HDHK26X                                                          
         DC    H'0'                                                             
*                                                                               
HDHK26X  DS    0H                                                               
         EJECT                                                                  
HDHK30   CLI   QMGR,C' '           TEST MKTGRPS                                 
         BE    HDHK32              NO                                           
         LA    R1,MGR1BK                                                        
         CLI   MODE,MGR1LAST                                                    
         BE    HDHK34                                                           
         LA    R1,MGR2BK                                                        
         CLI   MODE,MGR2LAST                                                    
         BE    HDHK34                                                           
         LA    R1,MGR3BK                                                        
         CLI   MODE,MGR3LAST                                                    
         BE    HDHK34                                                           
*                                                                               
HDHK32   CLI   MODE,REQLAST                                                     
         BNE   HDHK40                                                           
         LA    R1,=CL12'REPORT'                                                 
*                                                                               
HDHK34   MVC   WORK,SPACES                                                      
         MVC   WORK(2),=C'**'                                                   
         MVC   WORK+3(12),0(R1)    MOVE MKTGRP TITLE                            
         MVC   WORK+16(10),=C' TOTALS **'                                       
         OC    WORK(26),SPACES                                                  
         GOTO1 SQUASHER,DMCB,WORK,26                                            
         GOTO1 CENTER,DMCB,WORK,26                                              
         MVC   H6+42(26),WORK                                                   
*                                                                               
HDHK40   CLI   QSTA+4,C' '                                                      
         BE    HDHK50                                                           
         CLI   QSTA+4,C'/'                                                      
         BNE   *+10                                                             
         MVC   H3(16),=C'** CABLE ONLY **'                                      
         CLI   QSTA+4,C'-'                                                      
         BNE   *+10                                                             
         MVC   H3(20),=C'** CABLE EXCLUDED **'                                  
*                                                                               
HDHK50   B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
SSDATES  DS    XL106               26 MONTHS + X'00'                            
QPRIOR   DC    X'00'                                                            
QSUBSEQ  DC    X'00'                                                            
ELCDLO   DC    X'00'                                                            
ELCDHI   DC    X'00'                                                            
PRISW    DC    X'00'                                                            
SUBSW    DC    X'00'                                                            
PSTAR    DC    X'00'                                                            
PZERO    DC    X'00'                                                            
SSTYPE   DC    X'00'                                                            
SVPMKT   DS    XL4                                                              
WDATE    DS    CL6                                                              
XFF      DC    20X'FF'                                                          
SVMGR    DC    XL4'00'                                                          
MGR1MASK DC    H'0'                                                             
MGR2MASK DC    H'0'                                                             
HLDATES  DS    CL32                                                             
*                                                                               
MKTBANK  DC    F'0'                                                             
STABANK  DC    F'0'                                                             
P8SV     DC    PL8'0'                                                           
BANKFCTR DC    PL8'0'                                                           
         SPACE 1                                                                
ADCONS   DS    0F                                                               
*                                                                               
VSORTER  DC    V(SORTER)                                                        
VPRNTBL  DC    V(PRNTBL)                                                        
ABUYTOTS DC    A(BUYTOTS)          *** INPUT PHASE ONLY                         
AESTTOTS DC    A(ESTTOTS)          *** INPUT PHASE ONLY                         
ABUFFER  DC    A(BUFFER)                                                        
*                                                                               
ADCONSX  EQU   *-1                                                              
         SPACE 1                                                                
         DS    0F                                                               
CNTRS    DS    0CL32                                                            
*                                                                               
SPRCNTR  DC    F'0'                                                             
MG1CNTR  DC    F'0'                                                             
MG2CNTR  DC    F'0'                                                             
MG3CNTR  DC    F'0'                                                             
MKTCNTR  DC    F'0'                                                             
STACNTR  DC    F'0'                                                             
OFCCNTR  DC    F'0'                                                             
CLTCNTR  DC    F'0'                                                             
LYTDTRN  DC    PL8'0'              LAST YEAR SPOTS (TRANS) CUMM                 
LYTDORD  DC    PL8'0'              LAST YEAR ORDERED $ CUMM                     
TYTDTRN  DC    PL8'0'              THIS YEAR SPOTS (TRANS) CUMM                 
TYTDORD  DC    PL8'0'              THIS YEAR ORDERED $ CUMM                     
         EJECT                                                                  
SRREC    DS    0CL36               *** SORT RECORD DEFINITION ***               
*                                                                               
SRKEY    DS    0CL20                                                            
SRTYPE   DS    CL1                 (FOR BUFFALO ONLY)                           
SRSPR    DS    CL2                                                              
SRMGR1   DS    CL2                                                              
SRMGR2   DS    CL2                                                              
SRMGR3   DS    CL2                                                              
SRMKT    DS    CL2                                                              
SRSTA    DS    CL3                                                              
SROFC    DS    CL1                                                              
SRCLT    DS    CL2                                                              
SRMONYR  DS    CL2                 CHANGED TO SORT MONTHS TOGETHER              
         DS    CL1                 SPARE                                        
*                                                                               
SRLSPT   DC    PL8'0' SPOTS (INTO SORT) - LAST YEARS SPOTS INTO BUFFALO         
SRLORD   DC    PL8'0' $ ORD (INTO SORT) - LAST YEARS $ ORD INTO BUFFALO         
SRTSPT   DC    PL8'0' NOT INTO SORT - THIS YEARS SPOTS INTO BUFFALO             
SRTORD   DC    PL8'0' NOT INTO SORT - THIS YEARS $ ORD INTO BUFFALO             
*                                                                               
SRNEW    DS    CL36                                                             
*                                                                               
SVBUFF   DS    0XL3                SAVE AREA FOR BUFFALO KEY                    
SVBUFFOF DS    XL1                                                              
SVBUFFCL DS    XL2                                                              
         SPACE 1                                                                
STRTYEAR DS    CL1                 START (LAST) YEAR FOR COMPARE                
STPRTYR  DS    CL4                 STARTING (LAST) PRINT YEAR                   
EDPRTYR  DS    CL4                 ENDING (THIS) PRINT YEAR                     
CURMON   DS    XL1                 ENDING (CURRENT) MONTH                       
         SPACE 1                                                                
LYCUMORD DC    PL8'0'              LAST YEARS ORDER $ CUMM                      
LYCUMSPT DC    PL8'0'              LAST YEARS SPOTS CUMM                        
TYCUMORD DC    PL8'0'              THIS YEARS ORDER $ CUMM                      
TYCUMSPT DC    PL8'0'              THIS YEARS SPOTS CUMM                        
         SPACE 1                                                                
         DS    0H                                                               
BUFFREC  DS    0XL40                                                            
*                                                                               
BUFFOFC  DS    XL1                                                              
BUFFCLT  DS    XL2                                                              
BUFFMY   DS    XL2                                                              
*                                                                               
BUFFDATA DS    0XL32                                                            
BUFLSPT  DS    PL8                                                              
BUFLORD  DS    PL8                                                              
BUFTSPT  DS    PL8                                                              
BUFTORD  DS    PL8                                                              
         DS    XL3                                                              
         EJECT                                                                  
BUFFEOF  DS    C                                                                
BUFFLVL  DS    C                                                                
BUFFCNTR DC    H'0'                                                             
BUFFTOTS DS    XL(L'BUFFDATA)                                                   
*                                                                               
BUFFER   DC    26XL(L'BUFFREC)'00'                                              
*                                                                               
BUYTOTS  DS    26XL20              INTEGER - FOR INPUT PHASE ONLY               
ESTTOTS  DS    26XL20              INTEGER - FOR INPUT PHASE ONLY               
         SPACE 2                                                                
* DSECT FOR DATA IN BUFFER *                                                    
         SPACE 1                                                                
BRECD    DSECT                                                                  
*                                                                               
BRECOFC  DS    XL1                                                              
BRECCLT  DS    XL2                                                              
BRECMY   DS    XL2                                                              
*                                                                               
BRECDATA DS    0XL32                                                            
BRECLSPT DS    PL8                 LAST YEAR SPOTS                              
BRECLORD DS    PL8                 LAST YEAR ORDERED ($ ONLY)                   
BRECTSPT DS    PL8                 THIS YEAR SPOTS                              
BRECTORD DS    PL8                 THIS YEAR ORDERED ($ ONLY)                   
         EJECT                                                                  
* BUFFALO FILE DEFINITION *                                                     
         SPACE 1                                                                
         BUFF  LINES=400,ROWS=7,COLUMNS=4,FLAVOR=PACKED,KEYLIST=(5,A)           
         SPACE 2                                                                
* DSECT FOR PRINT LINE DATA *                                                   
         SPACE 1                                                                
PLINED   DSECT                                                                  
PCLT     DS    CL3                 CLIENT                                       
         DS    CL2                                                              
PMONTH   DS    CL3                 MONTH                                        
         DS    CL1                                                              
PLYMORD  DS    CL11                LAST YEAR MONTHLY ORDERED $                  
         DS    CL1                                                              
PTYMORD  DS    CL11                THIS YEAR                                    
         DS    CL1                                                              
PC$INX   DS    CL3                 INDEX                                        
         DS    CL2                                                              
PLYMTRN  DS    CL6                 LAST YEAR MONTHLY TRANS                      
         DS    CL1                                                              
PTYMTRN  DS    CL6                 THIS YEAR                                    
         DS    CL1                                                              
PCTRINX  DS    CL3                 INDEX                                        
         DS    CL2                                                              
PLYYORD  DS    CL11                LAST YEAR CUME ORDERED $                     
         DS    CL1                                                              
PTYYORD  DS    CL11                THIS YEAR                                    
         DS    CL1                                                              
PY$INX   DS    CL3                 INDEX                                        
         DS    CL2                                                              
PLYYTRN  DS    CL7                 LAST YEAR CUME TRANS                         
         DS    CL1                                                              
PTYYTRN  DS    CL7                 THIS YEAR                                    
         DS    CL1                                                              
PYTRINX  DS    CL3                 INDEX                                        
*        PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
QOPT6    EQU   QOPT5+1                                                          
QOPT7    EQU   QOPT5+2                                                          
QSUMONLY EQU   QDPTMENU                                                         
*        PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPREPA702 03/23/11'                                      
         END                                                                    
