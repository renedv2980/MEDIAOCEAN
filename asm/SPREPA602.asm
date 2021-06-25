*          DATA SET SPREPA602  AT LEVEL 024 AS OF 08/25/05                      
*PHASE SPA602C                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE SORTER                                                                 
                                                                                
*===================================================================            
* 28JUL05 MHER SUPPORT FOR 2CHAR OFFICES                                        
*===================================================================            
                                                                                
***********************************************************************         
*                                                                     *         
*   SORT RECORD LAYOUT                                                *         
*                                                                     *         
* TYPE  SPR  MGR1  MGR2  MGR3   MKT   STA   OFC   CLT    Y/M          *         
* ----  ---  ----  ----  ----   ---   ---   ---   ---   -----         *         
*   0   1-2   3-4   5-6   7-8  9-10  11-13 14-15 16-17  18-19         *         
*                                                                     *         
*   REQUEST OPTIONS                                                   *         
*   ---------------                                                   *         
*     (50)     STATION GROUP FILTER ID  (QBOOK1)                      *         
*     (51-54)  STATION GROUP NUMBER                                   *         
*                                                                     *         
*     (58)  STA AFFILIATE      N=NBC,A=ABC,C=CBS,I=IND                *         
*                              * FOR CANADA MEANS REPORT AFF, NOT STA *         
*   0 (61)  SUMMARIES ONLY Y = SUPPRESS MARKET AND STATION DETAILS    *         
*                                                                     *         
*   1 (62)  PRIOR/SUBS     0 = NO PRIOR OR SUBSEQUENT (DEFAULT)       *         
*                          1 = INCLUDE PRIOR ONLY                     *         
*                          2 = INCLUDE SUBSEQUENT ONLY                *         
*                          3 = INCLUDE PRIOR AND SUBSEQUENT           *         
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
*                          3 = STATION TOTALS ONLY                    *         
*                                                                     *         
*   7 (68)  EXCLUDE CODE   1 = EXCLUDE IF CPROF+13 = 1                *         
*           FILTER         2 = EXCLUDE IF CPROF+13 = 2                *         
*                          3 = EXCLUDE IF CPROF+13 = 1 OR 2           *         
*                                                                     *         
*   ** NOTE 1 **           IF QREP = ALL AND QREPTYP=S, ONLY SPECIAL  *         
*                          REP SUMMARY IS PRINTED                     *         
*                                                                     *         
***********************************************************************         
         TITLE 'SPA602 - SPOTPAK STATION SUMMARIES'                             
SPA602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPA602,RC,RR=R8                                                
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
         CLI   MODE,STAFRST                                                     
         BE    SS50                                                             
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
         CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         GOTO1 VSORTER,DMCB,=C'END'                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
* RUNFRST *                                                                     
         SPACE 1                                                                
SS10     DS    0H                                                               
         L     RE,=A(HDHKR9)                                                    
         STM   R9,RC,0(RE)                                                      
         L     R0,=A(SSHDHK)                                                    
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
*                                                                               
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
         L     RE,ACLTTAB             RESET CLT BUFFER POINTER                  
         ST    RE,NEXTCLT                                                       
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
*                                                                               
         CLC   =C'TRCSORT',QUESTOR    TEST TO TRACE SORT                        
         BNE   SS22                                                             
         L     RE,=A(TRCSAVE)                                                   
         MVC   0(4,RE),VSORTER                                                  
         L     RE,=A(TRCSORT)                                                   
         ST    RE,VSORTER                                                       
         SPACE 1                                                                
* BUILD LIST OF REQUESTED MONTHS (START/END DATES) *                            
         SPACE 1                                                                
SS22     XC    SSDATES,SSDATES                                                  
         MVI   QPRIOR,C'Y'                                                      
         MVI   QSUBSEQ,C'Y'                                                     
*                                                                               
         MVC   SSDATES(4),=X'01010101' SET PRIOR                                
         LA    R4,SSDATES+4                                                     
         CLI   QOPT1,C'1'          TEST INCLUDE PRIOR                           
         BE    SS24                                                             
         CLI   QOPT1,C'3'          TEST INCLUDE PRIOR + SUBSEQ                  
         BE    SS24                                                             
         LA    R4,SSDATES                                                       
         MVI   QPRIOR,C'N'                                                      
*                                                                               
SS24     MVC   WORK(12),SPACES                                                  
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+6(4),QEND                                                   
         GOTO1 MOBILE,DMCB,(24,WORK),(1,(R4))                                   
         MVC   BQSTART,0(R4)       SET START MONTH START DATE                   
         SPACE 1                                                                
* FIND END OF MONTH LIST *                                                      
         SPACE 1                                                                
SS26     CLI   4(R4),X'FF'                                                      
         BE    SS26X                                                            
         LA    R4,4(R4)                                                         
         B     SS26                                                             
SS26X    MVC   BQEND,2(R4)         SET END MONTH END DATE                       
*                                                                               
         LA    R4,4(R4)            POINT BEYOND LAST DATE                       
         MVC   0(4,R4),=X'FEFEFEFE'  SET DATE FOR SUBS                          
         CLI   QOPT1,C'2'          TEST INCLUDE SUBS                            
         BE    SS30                                                             
         CLI   QOPT1,C'3'          TEST INCLUDE PRIOR AND SUBS                  
         BE    SS30                                                             
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
         CLI   QPRIOR,C'Y'                                                      
         BE    *+10                                                             
         MVC   HLDATES+18(2),SPACES                                             
*                                                                               
         MVC   WORK(4),QEND                                                     
         MVC   WORK+4(2),=C'28'                                                 
         GOTO1 (RF),(R1),WORK,(6,HLDATES+24)                                    
         CLI   QSUBSEQ,C'Y'                                                     
         BE    *+10                                                             
         MVC   HLDATES+30(2),SPACES                                             
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
         CLI   QRERATE,C'Y'        FILTER ON STATION GROUP                      
         BNE   EXIT                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING GRPRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   GRPKTYP,GRPKTYPQ                                                 
         MVI   GRPKSTYP,GRPKSTYQ                                                
         MVC   GRPKAGMD,BAGYMD                                                  
         MVC   GRPKID,QBOOK1                                                    
         GOTO1 HIGH                                                             
         CLC   GRPKEY,KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         LA    R6,GRPEL                                                         
         DROP  R6                                                               
         USING GRPBRKD,R6                                                       
*                                                                               
         MVC   SVGRPBK1,GRPBK1     BREAK 1 TITLE                                
         MVC   SVGRPBK2,GRPBK2     BREAK 2 TITLE                                
*                                                                               
         USING GRPRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   WORK(4),QBOOK1+1                                                 
         OC    WORK(4),=C'0000'                                                 
         PACK  WORK+5(3),WORK(5)                                                
         MVC   GRPKCODE,WORK+5                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(GRPKMSQL),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         USING GRPRECD,R6                                                       
         LA    R6,GRPEL                                                         
         DROP  R6                                                               
         USING GRPGRPD,R6                                                       
         MVC   SVPGNAM1,GRPGNAM1        GROUP 1 NAME                            
         MVC   SVPGNAM2,GRPGNAM2        GROUP 2 NAME                            
         DROP  R6                                                               
         B     EXIT                                                             
*                                                                               
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,20,BI,A),WORK=1'                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=60'                                    
         EJECT                                                                  
* CLIENT FIRST *                                                                
         SPACE 1                                                                
SS40     L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         CLI   CPROF+6,C'Y'        TEST DECODE FEATURE                          
         BNE   SS40X                                                            
* MUST SAVE CLIENT CODE                                                         
         L     RE,NEXTCLT                                                       
         L     R0,ACLTTABX                                                      
         BCTR  R0,0                                                             
         CR    RE,R0                                                            
         BH    SS40X                                                            
         MVC   0(2,RE),2(R6)       SAVE PACKED CLT CODE                         
         LA    RE,2(RE)                                                         
         ST    RE,NEXTCLT                                                       
         XC    0(2,RE),0(RE)       SET EOL FLAG                                 
*                                                                               
SS40X    CLI   QMGR,C' '           TEST MKTGRPS                                 
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
*                                                                               
         CLI   PROGPROF,C'Y'       TEST ANALYZE BY OFFICE                       
         BNE   *+8                                                              
         BRAS  RE,BLDOFFC          GO BUILD OFFICE TABLE                        
*                                                                               
         CLI   QOPT7,C' '          TEST CLIENT FILTER                           
         BE    SS46                NO                                           
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         MVC   BYTE,CPROF+13                                                    
         NI    BYTE,X'0F'                                                       
         ZIC   RE,QOPT7                                                         
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    BYTE,0 * EXECUTED *                                              
         BZ    SS46                                                             
         MVI   MODE,CLTLAST                                                     
*                                                                               
SS46     CLC   QCLT,=C'ALL'                                                     
         BNE   EXIT                                                             
         CLI   QREPTYPE,C'S'                                                    
         BNE   EXIT                                                             
*                                                                               
         L     R6,ADCLT                                                         
SS46A    CLI   CPROF+6,C'Y'        TEST DECODE FEATURE                          
         BNE   SS46B                                                            
* MUST SAVE CLIENT CODE                                                         
         L     RE,NEXTCLT                                                       
         L     R0,ACLTTABX                                                      
         BCTR  R0,0                                                             
         CR    RE,R0                                                            
         BH    EXIT                                                             
         MVC   0(2,RE),2(R6)       SAVE PACKED CLT CODE                         
         LA    RE,2(RE)                                                         
         ST    RE,NEXTCLT                                                       
         XC    0(2,RE),0(RE)       SET EOL FLAG                                 
*                                                                               
SS46B    MVC   KEY+4,=X'FFFFFFFFFFFFFFFFFF'                                     
         GOTO1 HIGH                                                             
         CLC   KEY(2),KEYSAVE      00/AM                                        
         BNE   EXIT                                                             
         GOTO1 GETCLT                                                           
         B     SS46A                                                            
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*======================================================*                        
*   STATION FIRST PROCESSING                           *                        
*======================================================*                        
         SPACE 1                                                                
SS50     DS    0H                                                               
         MVC   SSMKT,KEY+4         SAVE MARKET/STATION                          
         MVC   SSSTA,KEY+6                                                      
*                                                                               
         CLI   QMED,C'N'           TEST NETWORK REQUEST                         
         BNE   EXIT                                                             
         CLI   QAFFIL,C'*'         TEST CANADA ALL AFF REQ                      
         BNE   EXIT                                                             
*                                                                               
         MVI   DMINBTS,X'08'       SET TO PASS DELETED RECORDS                  
         MVI   DMOUTBTS,X'FD'      DO NOT TEST X'02'                            
         GOTO1 GETBUY                                                           
*                                                                               
         MVI   ELCDLO,X'68'        DIG OUT THE NETWORK ELEMENT                  
         MVI   ELCDHI,X'68'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         MVC   DUB(4),2(R6)                                                     
         MVI   DUB+4,C'N'                                                       
         GOTO1 MSPACK,DMCB,=C'0000',DUB,SSMKT                                   
         B     EXIT                                                             
         EJECT                                                                  
*======================================================*                        
*   BUY RECORD PROCESSING                              *                        
*======================================================*                        
         SPACE 1                                                                
SS60     DS    0H                                                               
         MVI   DMINBTS,X'08'       SET TO PASS DELETED RECORDS                  
         MVI   DMOUTBTS,X'FD'      DO NOT TEST X'02'                            
         GOTO1 GETBUY                                                           
*                                                                               
         CLI   KEY,3               TEST SYNDICATION KEY                         
         BE    *+14                                                             
         CLC   KEY+1(2),BUYREC+1   TEST SAME CLIENT (MILLER SHIT)               
         BNE   EXIT                                                             
*                                                                               
         TM    BUYREC+15,X'80'     TEST DELETED                                 
         BO    EXIT                YES - IGNORE                                 
*                                                                               
         CLI   QREPTYPE,C'S'                                                    
         BNE   SS61                                                             
         CLC   =C'ALL',QREP        TEST ALL SPECIAL REP REQUEST                 
         BNE   *+14                                                             
         OC    BDREP,BDREP                                                      
         BZ    EXIT                                                             
*                                                                               
SS61     MVI   BDTIME,0            SUPPRESS PIGGYBACKS                          
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
* ADD BUYTOTS TO ESTTOTS                                                        
*                                                                               
SS80     DS    0H                                                               
         CLI   PROGPROF+3,C'Y'     TEST SUPPRESS SPCL REP DOLS                  
         BNE   *+14                NO - CONTINUE                                
         OC    BDREP,BDREP         TEST SPECIAL REP PRESENT                     
         BNZ   SS92                YES - SKIP ADDITION                          
*                                                                               
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
         EJECT                                                                  
SS92     OC    BDREP,BDREP         TEST SPECIAL REP PRESENT                     
         BZ    SS96                                                             
         SPACE 1                                                                
* GENERATE SPECIAL REP RECORDS *                                                
         SPACE 1                                                                
         BAS   RE,GENKEY                                                        
         MVC   SRSPR,BDREP                                                      
*                                                                               
         LA    R4,SSDATES                                                       
         L     R5,ABUYTOTS                                                      
*                                                                               
SS94     OC    0(20,R5),0(R5)                                                   
         BZ    *+8                                                              
         BAS   RE,GENREC                                                        
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R5,20(R5)                                                        
         CLI   0(R4),0                                                          
         BNE   SS94                                                             
         SPACE 2                                                                
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
         MVC   SRSTA,BUYREC+6      STATION                                      
         CLI   QMED,C'N'           TEST NETWORK                                 
         BNE   GENKEY2X                                                         
         CLI   QAFFIL,C'*'         TEST ALL AFF REQ                             
         BNE   GENKEY2X                                                         
         MVC   SRMKT,SSMKT                                                      
         MVC   SRSTA,SSSTA                                                      
*                                                                               
GENKEY2X CLI   QSUMONLY,C'Y'                                                    
         BNE   GENKEY3                                                          
         MVC   SRMKT(5),XFF                                                     
         CLI   PROGPROF,C'Y'       TEST ANALYZE BY OFFICE                       
         BE    GENKEY3                                                          
         CLI   QMGR,C' '                                                        
         BNE   GENKEY3                                                          
         MVC   SRMKT,BUYREC+4      IF NO MKTGRPS, NEED MKT                      
*                                                                               
GENKEY3  MVC   SROFC,=X'FFFF'                                                   
         CLI   PROGPROF,C'Y'       TEST OFFICE WITHIN STATION                   
         BNE   GENKEY4                                                          
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         SR    RE,RE               INDEX TO 2 CHAR OFFICE CODE                  
         IC    RE,COFFICE                                                       
         MHI   RE,L'OFCTAB                                                      
         A     RE,AOFCTAB                                                       
         MVC   SROFC,0(RE)                                                      
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
         L     R0,0(R5)                                                         
         CVD   R0,DUB                                                           
         ZAP   SRSPOTS,DUB                                                      
*                                                                               
         L     R0,4(R5)                                                         
         CVD   R0,DUB                                                           
         ZAP   SRORDG,DUB                                                       
*                                                                               
         L     R0,8(R5)                                                         
         CVD   R0,DUB                                                           
         ZAP   SRORDN,DUB                                                       
*                                                                               
         L     R0,12(R5)                                                        
         CVD   R0,DUB                                                           
         ZAP   SRPAIDG,DUB                                                      
*                                                                               
         L     R0,16(R5)                                                        
         CVD   R0,DUB                                                           
         ZAP   SRPAIDN,DUB                                                      
*                                                                               
         MVC   SRYRMON,2(R4)       RECORD GETS MONTH END DATE                   
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
         XC    SVMGR,SVMGR                                                      
         SPACE 1                                                                
* CLEAR COUNTERS *                                                              
         SPACE 1                                                                
         XC    CNTRS,CNTRS                                                      
         SPACE 1                                                                
* GET FIRST SORTED RECORD *                                                     
         SPACE 1                                                                
         GOTO1 VSORTER,DMCB,=C'GET',0                                           
         L     RE,4(R1)                                                         
         MVC   SRREC,0(RE)                                                      
         CLI   0(RE),X'FF'         TEST E-O-F ON FIRST READ                     
         BE    SS232X                                                           
         SPACE 2                                                                
* GET NEXT SORTED RECORD *                                                      
         SPACE 1                                                                
SS202    DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'GET',0                                           
         L     RE,4(R1)                                                         
         MVC   SRNEW,0(RE)                                                      
*                                                                               
         CLC   SRKEY,SRNEW                                                      
         BNE   SS210                                                            
         SPACE 1                                                                
* KEYS EQUAL - ADD RECORDS *                                                    
         SPACE 1                                                                
         LA    R0,5                                                             
         LA    R4,SRSPOTS                                                       
         LA    R5,SRSPOTS-SRREC+SRNEW                                           
         AP    0(8,R4),0(8,R5)                                                  
         LA    R4,8(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R0,*-14                                                          
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
* GENERATE OFFICE/CLIENT DETAILS *                                              
         SPACE 1                                                                
         MVC   BUFFOFC,SROFC                                                    
         MVC   BUFFCLT,SRCLT                                                    
         MVC   BUFFYM,SRYRMON                                                   
         MVC   BUFFDATA,SRSPOTS                                                 
         BAS   RE,PUTBUFF                                                       
         SPACE 1                                                                
* GENERATE OFFICE TOTALS (IF NEEDED) *                                          
         SPACE 1                                                                
         CLI   PROGPROF,C'Y'                                                    
         BNE   SS212                                                            
         CLC   BUFFCLT,XFF                                                      
         BE    SS212                                                            
         MVC   BUFFCLT,XFF                                                      
         BAS   RE,PUTBUFF                                                       
         SPACE 1                                                                
* GENERATE STATION TOTALS *                                                     
         SPACE 1                                                                
SS212    CLI   BUFFOFC,X'FF'                                                    
         BNE   *+14                                                             
         CLC   BUFFCLT,XFF                                                      
         BE    SS220                                                            
         MVI   BUFFOFC,X'FF'                                                    
         MVC   BUFFCLT,XFF                                                      
         BAS   RE,PUTBUFF                                                       
         EJECT                                                                  
******************************                                                  
*                            *                                                  
*   TEST FOR CONTROL BREAK   *                                                  
*                            *                                                  
******************************                                                  
         SPACE 1                                                                
SS220    CLC   SRKEY+1(13),SRNEW+1       SPR/MGR1-2-3/MKT/STA                   
         BE    SS234                                                            
         BAS   RE,ENDSTA                                                        
*                                                                               
         CLC   SRKEY+1(10),SRNEW+1       SPR/MGR1-2-3/MKT                       
         BE    SS234                                                            
         BAS   RE,ENDMKT                                                        
*                                                                               
SS226    CLC   SRMGR3,XFF                TEST DUMMY MKTGRP                      
         BE    SS228                                                            
         CLC   SRKEY+1(8),SRNEW+1        SPR/MGR1-2-3                           
         BE    SS234                                                            
         BAS   RE,ENDMGR3                                                       
*                                                                               
SS228    CLC   SRMGR2,XFF                TEST DUMMY MKTGRP                      
         BE    SS230                                                            
         CLC   SRKEY+1(6),SRNEW+1        SPR/MGR1-2                             
         BE    SS234                                                            
         BAS   RE,ENDMGR2                                                       
*                                                                               
SS230    CLC   SRMGR1,XFF                TEST DUMMY MKTGRP                      
         BE    SS232                                                            
         CLC   SRKEY+1(4),SRNEW+1        SPR/MGR1                               
         BE    SS234                                                            
         BAS   RE,ENDMGR1                                                       
*                                                                               
SS232    CLC   SRKEY+1(2),SRNEW+1        SPR                                    
         BE    SS234                                                            
*                                                                               
         OC    SRKEY+1(2),SRKEY+1        TEST REP PRESENT                       
         BZ    *+8                       NO                                     
         BAS   RE,ENDSPR                 ELSE END REP                           
*                                                                               
         CLI   SRNEW,X'FF'               TEST E-O-F                             
         BNE   SS234                                                            
         BAS   RE,ENDRPT                 YES - REPORT TOTALS NOW                
SS232X   GOTO1 VSORTER,DMCB,=C'END'                                             
         B     EXIT                                                             
*                                                                               
SS234    MVC   SRREC,SRNEW               MOVE NEW REC TO PROCESS                
         B     SS202                                                            
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
ENDSTA4  BAS   RE,GETMKT           READ MARKET NAME                             
*                                                                               
         CLI   QSUMONLY,C'Y'       TEST SUMMARIES ONLY                          
         BE    ENDSTAX                                                          
*                                                                               
         BAS   RE,SSPRT                                                         
         BNE   ENDSTA6                                                          
*                                                                               
         L     RE,STACNTR                                                       
         LA    RE,1(RE)                                                         
         ST    RE,STACNTR                                                       
*                                                                               
ENDSTA6  CLI   PROGPROF+2,C'Y'     TEST PRINT TIME BANKS                        
         BNE   ENDSTAX             NO                                           
         CLI   QREPTYPE,C'S'       TEST SPECIAL REP REQ                         
         BNE   ENDSTAX             NO                                           
         CLC   =C'000',QREP        TEST SINGLE REP REQUEST                      
         BH    ENDSTAX                                                          
         CP    BANKFCTR,=P'0'      TEST FIRST TIME                              
         BNZ   ENDSTA8                                                          
         EJECT                                                                  
* READ REP RECORD *                                                             
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(3),QREP                                                    
         MVC   KEY+5(2),QAGY                                                    
*                                                                               
         L     R6,ADREP                                                         
         USING REPRECD,R6                                                       
         GOTO1 HIGHREP                                                          
         CLC   KEY(13),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,RCADJF                                                      
         CVD   R0,DUB                                                           
         ZAP   BANKFCTR,DUB                                                     
         DROP  R6                                                               
         EJECT                                                                  
* PRINT TIME BANK DATA (DFS ONLY) *                                             
         SPACE 1                                                                
ENDSTA8  XC    STABANK,STABANK                                                  
         XC    KEY,KEY                                                          
         MVI   KEY,3                                                            
         PACK  DUB,QREP                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,KEY+1                                                       
         MVC   BYTE,BAGYMD                                                      
         NI    BYTE,X'F0'          DROP MEDIA                                   
         OC    KEY+1(1),BYTE       'OR' INTO KEY                                
         MVC   KEY+3(5),SRMKT      MOVE MKT/STA                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST FOUND COVERAGE REC                      
         BNE   ENDSTA14                                                         
         L     R8,ADBUY                                                         
         ST    R8,AREC                                                          
         GOTO1 GET                                                              
         LA    R6,24(R8)                                                        
*                                                                               
ENDSTA10 CLI   0(R6),0                                                          
         BE    ENDSTAX                                                          
         CLI   0(R6),X'11'         TEST BAL FWD ELEM                            
         BE    ENDSTA12                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ENDSTA10                                                         
*                                                                               
ENDSTA12 ICM   R0,15,8(R6)         BAL FWD DOLLARS                              
         ST    R0,STABANK          SAVE IN STATION                              
         A     R0,MKTBANK          AND ADD TO MKT TOTAL                         
         ST    R0,MKTBANK                                                       
*                                                                               
ENDSTA14 BAS   RE,PRTBANK                                                       
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
         CLI   PROGPROF,C'Y'       TEST ANALYZE BY OFFICE                       
         BE    ENDMKT1             YES - INGORE MKTGRP CONDITION                
         CLI   QMGR,C' '           TEST MKTGRP REQUEST                          
         BE    ENDMKT2             NO - ALWAYS PRINT MKT TOTALS                 
*                                                                               
ENDMKT1  CLI   QSUMONLY,C'Y'       TEST SUMMARIES ONLY                          
         BE    ENDMKTX                                                          
*                                                                               
ENDMKT2  CLC   STACNTR,=F'1'                                                    
         BE    *+12                                                             
         BAS   RE,SSPRT                                                         
         BNE   ENDMKT6                                                          
*                                                                               
         L     RE,MKTCNTR                                                       
         LA    RE,1(RE)                                                         
         ST    RE,MKTCNTR                                                       
*                                                                               
ENDMKT6  CP    BANKFCTR,=P'0'      TEST TIME BANK REQUEST                       
         BE    ENDMKTX             NO                                           
         MVC   STABANK,MKTBANK     MOVE MKTBANK TO STABANK                      
         BAS   RE,PRTBANK                                                       
         XC    MKTBANK(8),MKTBANK  CLEAR MKT/STA ACCUMS                         
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
         SPACE 2                                                                
* PRINT TIME BANK DATA *                                                        
         SPACE 1                                                                
PRTBANK  NTR1                                                                   
*                                                                               
         CP    BANKFCTR,=P'0'                                                   
         BE    EXIT                                                             
*                                                                               
         MVI   P1,0                SET TO SKIP A LINE                           
         MVC   P2+18(27),=C'** MARKET TIME BANK DATA **'                        
         CLI   MODE,MKTLAST                                                     
         BE    *+10                                                             
         MVC   P2+18(28),=C'** STATION TIME BANK DATA **'                       
         MVC   P3+28(9),=C'OPEN BAL'                                            
         L     R0,STABANK                                                       
         EDIT  (R0),(15,P3+38),2,COMMAS=YES,MINUS=YES                           
*                                                                               
         MVC   P4+28(9),=C'USAGE ...'                                           
         ZAP   WORK(16),BUFFTOTS+8(8) GET GROSS DOLLARS                         
         MP    WORK(16),=P'10000'                                               
         DP    WORK(16),BANKFCTR      GROSS UP DOLLARS                          
         ZAP   P8SV,WORK(8)                                                     
         EDIT  (P8,P8SV),(15,P4+38),2,COMMAS=YES,MINUS=YES                      
*                                                                               
         MVC   P5+28(9),=C'CLOSE BAL '                                          
         L     R0,STABANK                                                       
         CVD   R0,DUB                                                           
         SP    DUB,P8SV                                                         
         ZAP   P8SV,DUB                                                         
         EDIT  (P8,P8SV),(15,P5+38),2,COMMAS=YES,MINUS=YES                      
*                                                                               
         MVI   SPACING,2           SKIP A LINE AFTER SUMMARY                    
         GOTO1 REPORT                                                           
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
         CLC   =C'ALL',QMKT                                                     
         BNE   ENDRPTX                                                          
         CLC   =C'ALL',QSTA                                                     
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
         MVC   PSTA,=CL8'* ALL *'                                               
         CLC   SRSTA,XFF                                                        
         BE    SSPRT1X                                                          
         CLI   MODE,STALAST                                                     
         BH    SSPRT1X                                                          
         SPACE 1                                                                
         MVC   PSTA,SPACES                                                      
         GOTO1 MSUNPK,DMCB,(X'80',SRMKT),MKT,PSTA                               
         CLI   PSTA,C'0'           TEST CABLE STATION (NUMERIC)                 
         BL    *+12                NO                                           
         MVI   PSTA+4,C'/'                                                      
         B     SSPRT1X                                                          
*                                                                               
         LA    R1,PSTA+3                                                        
         CLI   0(R1),C' '          TEST ONLY 3 CALL LETTERS                     
         BNE   *+6                                                              
         BCTR  R1,0                                                             
*                                                                               
         CLI   PSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   PSTA+4,C'T'                                                      
         CLI   PSTA+4,C'T'                                                      
         BNE   *+10                                                             
         MVC   1(3,R1),=C'-TV'                                                  
         CLI   PSTA+4,C'A'                                                      
         BNE   *+10                                                             
         MVC   1(3,R1),=C'-AM'                                                  
         CLI   PSTA+4,C'F'                                                      
         BNE   *+10                                                             
         MVC   1(3,R1),=C'-FM'                                                  
*                                                                               
SSPRT1X  MVC   SVPSTA,PSTA         SAVE STATION                                 
*                                                                               
         XC    CLTCNTR,CLTCNTR                                                  
         XC    OFCCNTR,OFCCNTR                                                  
*                                                                               
         MVI   BUFFEOF,C'N'                                                     
         XC    BUFFCNTR,BUFFCNTR                                                
         MVC   BUFFTOTS,=5PL8'0'                                                
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
SSPRT2X  CLC   BUFFOFC(4),SVBUFF   TEST SAME OFFICE/CLIENT                      
         BNE   SSPRT8                                                           
         EJECT                                                                  
* MOVE REC TO BUFFER *                                                          
         SPACE 1                                                                
SSPRT4   MVC   0(L'BUFFREC,R4),BUFFOFC     MOVE OFC/CLT/YR/MON/DATA             
         AHI   R4,L'BUFFREC                                                     
*                                                                               
         LH    RE,BUFFCNTR         BUMP RECORD COUNTER                          
         AHI   RE,1                                                             
         STH   RE,BUFFCNTR                                                      
*                                                                               
         LA    R1,BUFFTOTS                                                      
         AP    0(8,R1),BUFFSPTS                                                 
         AP    8(8,R1),BUFFORDG                                                 
         AP    16(8,R1),BUFFORDN                                                
         AP    24(8,R1),BUFFPDG                                                 
         AP    32(8,R1),BUFFPDN                                                 
*                                                                               
         CLI   QOPT6,C'1'          TEST UNPAID DETAILS + ORDERED TOTALS         
         BNE   SSPRT2                                                           
         CP    BUFFORDN,BUFFPDN    TEST ANY UNPAID DOLLARS                      
         BNE   SSPRT2              YES                                          
* DELETE LINE FROM BUFFER *                                                     
         AHI   R4,-(L'BUFFREC)                                                  
         LH    RE,BUFFCNTR                                                      
         BCTR  RE,0                                                             
         STH   RE,BUFFCNTR                                                      
         B     SSPRT2                                                           
         EJECT                                                                  
* PRINT CONTENTS OF BUFFER *                                                    
                                                                                
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
         BE    SSPRT11                                                          
         CLI   QCLT,C'$'           TEST OFFICE LIST REQUEST                     
         BE    SSPRT11                                                          
         CLC   =C'ALL',QCLT        TEST ALL CLIENT REQUEST                      
         BNE   SSPRT12             NO - ALWAYS PRINT CLIENT                     
SSPRT11  MVC   PCLT+1(5),=C'*ALL*'                                              
         CLC   SVBUFFCL,XFF        TEST POSSIBLE OFFICE TOTAL                   
         BNE   SSPRT12             NO                                           
         CLI   SVBUFFOF,X'FF'      TEST ALL OFFICE TOTAL                        
         BE    SSPRT20             YES                                          
                                                                                
* PRINT OFFICE NUMBER *                                                         
                                                                                
         MVC   PCLT-2(7),=C'OFFICE '                                            
         MVC   PCLT+5(2),SVBUFFOF  MOVE OFFICE CODE                             
         B     SSPRT20                                                          
*                                                                               
SSPRT12  MVC   PCLT,SPACES                                                      
*                                                                               
         L     RE,ACLTTAB                                                       
         MVI   BYTE,0                                                           
SSPRT14A CLC   SVBUFFCL,0(RE)                                                   
         BE    SSPRT14B                                                         
         BL    SSPRT14X                                                         
         LA    RE,2(RE)                                                         
         CLI   0(RE),0                                                          
         BNE   SSPRT14A                                                         
         B     SSPRT14X                                                         
*                                                                               
SSPRT14B MVI   BYTE,C'Y'           SET DECODE BYTE                              
*                                                                               
SSPRT14X DS    0H                                                               
         GOTO1 CLUNPK,DMCB,(BYTE,SVBUFFCL),PCLT+2                               
*                                                                               
SSPRT20  LH    RE,BUFFCNTR                                                      
         CHI   RE,1                                                             
         BE    *+12                                                             
         AHI   RE,1                                                             
         STC   RE,ALLOWLIN                                                      
*                                                                               
         L     R4,ABUFFER                                                       
         LH    R5,BUFFCNTR                                                      
         LTR   R5,R5               TEST ANY LINES IN BUFFER                     
         BP    SSPRT22                                                          
         CLC   BUFFTOTS,=5PL8'0'   TEST ANY TOTALS                              
         BE    SSPRT30             NO                                           
         B     SSPRT24             GO PRINT TOTALS                              
         EJECT                                                                  
SSPRT22  CLI   QOPT6,C'3'          TEST STATION TOTALS ONLY                     
         BE    SSPRT24             YES - GO PRINT TOTALS                        
         BAS   RE,SSFMT                                                         
*                                                                               
         GOTO1 REPORT                                                           
         MVI   STASW,C'Y'          SET STATION PRINTED FLAG                     
*                                                                               
         AHI   R4,L'BUFFREC                                                     
         BCT   R5,SSPRT22                                                       
*                                                                               
         CLI   QOPT6,C'1'          TEST UNPAID DETAILS + ORD TOTS               
         BE    SSPRT24             YES - ALWAYS PRINT TOTALS                    
         CLC   BUFFCNTR,=H'1'                                                   
         BE    SSPRT30                                                          
*                                                                               
SSPRT24  L     R4,ABUFFER          POINT TO FIRST LINE IN BUFFER                
         MVC   4(2,R4),XFF                                                      
         MVC   8(40,R4),BUFFTOTS                                                
         MVI   PSTAR,C'*'                                                       
         BAS   RE,SSFMT                                                         
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
SSPRT30  DS    0H                                                               
         ZIC   R0,MAXLINES                                                      
         BCTR  R0,0                                                             
         CLM   R0,1,LINE                                                        
         BNH   SSPRT32                                                          
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
SSPRT32  CLI   BUFFEOF,C'Y'        TEST REACHED EOF                             
         BE    SSPRTX                                                           
         SPACE 1                                                                
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
         EJECT                                                                  
* RESET CORE BUFFER PARAMETERS *                                                
         SPACE 1                                                                
SSPRT36  L     R4,ABUFFER                                                       
         XC    BUFFCNTR,BUFFCNTR                                                
         MVC   BUFFTOTS,=5PL8'0'                                                
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
         CLC   BRECYM,=X'0101'     TEST PRIOR                                   
         BNE   *+14                                                             
         MVC   PDATE(5),=C'PRIOR'                                               
         B     SSFMT2                                                           
*                                                                               
         CLC   BRECYM,=X'FEFE'                                                  
         BNE   *+14                                                             
         MVC   PDATE(6),=C'SUBSEQ'                                              
         B     SSFMT2                                                           
*                                                                               
         CLC   BRECYM,=X'FFFF'                                                  
         BNE   *+14                                                             
         MVC   PDATE(7),=C'*TOTAL*'                                             
         B     SSFMT2                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(2,BRECYM),(6,PDATE)                                 
*                                                                               
SSFMT2   EDIT  (P8,BRECSPTS),(8,PSPOTS)                                         
         MVC   PSPOTS+8(1),PSTAR                                                
*                                                                               
         EDIT  (P8,BRECORDG),(15,PORDG),2,COMMAS=YES,MINUS=YES                  
         CP    BRECORDG,=P'0'                                                   
         BL    *+10                                                             
         MVC   PORDG+14(1),PSTAR                                                
         SPACE 1                                                                
* TEST TO SUPPRESS NET COLUMNS *                                                
         SPACE 1                                                                
         CLI   QOPT4,C'Y'                                                       
         BE    EXIT                                                             
*                                                                               
         EDIT  (P8,BRECORDN),(15,PORDN),2,COMMAS=YES,MINUS=YES                  
         CP    BRECORDN,=P'0'                                                   
         BL    *+10                                                             
         MVC   PORDN+14(1),PSTAR                                                
*                                                                               
         EDIT  (P8,BRECPDN),(15,PPAIDN),2,COMMAS=YES,MINUS=YES                  
         CP    BRECPDN,=P'0'                                                    
         BL    *+10                                                             
         MVC   PPAIDN+14(1),PSTAR                                               
*                                                                               
         CP    BRECORDN,=P'0'                                                   
         BNH   SSFMT10                                                          
         CP    BRECPDN,=P'0'                                                    
         BNH   SSFMT10                                                          
*                                                                               
         ZAP   WORK(16),BRECPDN                                                 
         MP    WORK(16),=P'1000'                                                
         DP    WORK(16),BRECORDN                                                
         CP    WORK(8),=P'2147483647'  MAKE SURE IT'S POSITIVE                  
         BH    SSFMT10                                                          
         ZAP   DUB,WORK(8)                                                      
         SR    RE,RE                                                            
         CVB   RF,DUB                                                           
         A     RF,=F'5'                                                         
         D     RE,=F'10'                                                        
*                                                                               
         EDIT  (RF),(3,PPCTPAID)                                                
         MVC   PPCTPAID+3(1),PSTAR                                              
         SPACE 2                                                                
SSFMT10  SP    BRECORDN,BRECPDN                                                 
         EDIT  (P8,BRECORDN),(15,PUNPAIDN),2,COMMAS=YES,MINUS=YES               
         CP    BRECORDN,=P'0'                                                   
         BL    *+10                                                             
         MVC   PUNPAIDN+14(1),PSTAR                                             
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP R2                                                                
                                                                                
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
         CLI   QMGR,C'F'                                                        
         BH    *+10                                                             
         MVC   KEY+3(2),BCLT       MGR A-F NEED CLIENT                          
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
         MVC   MGR1N+1(4),=C'9999'                                              
         MVC   MGR2N+1(4),=C'9999'                                              
         MVC   MGR3N+1(4),=C'9999'                                              
         B     GETMGRX                                                          
*                                                                               
GETMGR4  MVC   MGR1N+2(4),=C'9999'                                              
         MVC   MGR2N+2(4),=C'9999'                                              
         MVC   MGR3N+2(4),=C'9999'                                              
*                                                                               
GETMGRX  MVC   MGR1NM,=CL24'*** UNDEFINED ***'                                  
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
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
NEXTCLT  DC    A(0)                                                             
SSDATES  DS    XL106               26 MONTHS + X'00'                            
SSMKT    DS    XL2                                                              
SSSTA    DS    XL3                                                              
QPRIOR   DC    X'00'                                                            
QSUBSEQ  DC    X'00'                                                            
ELCDLO   DC    X'00'                                                            
ELCDHI   DC    X'00'                                                            
PRISW    DC    X'00'                                                            
SUBSW    DC    X'00'                                                            
PSTAR    DC    X'00'                                                            
SSTYPE   DC    X'00'                                                            
SVPSTA   DS    XL8                                                              
XFF      DC    20X'FF'                                                          
SVMGR    DC    XL4'00'                                                          
MGR1MASK DC    H'0'                                                             
MGR2MASK DC    H'0'                                                             
HLDATES  DS    CL32                                                             
*                                                                               
SVGRPBK1 DS    CL(L'GRPBK1)        BREAK 1 TITLE                                
SVGRPBK2 DS    CL(L'GRPBK2)        BREAK 2 TITLE                                
SVPGNAM1 DS    CL(L'GRPGNAM1)      GROUP 1 NAME                                 
SVPGNAM2 DS    CL(L'GRPGNAM2)      GROUP 2 NAME                                 
*                                                                               
         DS    0D                                                               
OFCWORK  DS    XL64                                                             
*                                                                               
MKTBANK  DC    F'0'                                                             
STABANK  DC    F'0'                                                             
P8SV     DC    PL8'0'                                                           
BANKFCTR DC    PL8'0'                                                           
*                                                                               
ADCONS   DS    0F                                                               
*                                                                               
VSORTER  DC    V(SORTER)                                                        
VPRNTBL  DC    V(PRNTBL)                                                        
ABUYTOTS DC    A(BUYTOTS)          *** INPUT PHASE ONLY                         
AESTTOTS DC    A(ESTTOTS)          *** INPUT PHASE ONLY                         
ABUFFER  DC    A(BUFFER)                                                        
ACLTTAB  DC    A(CLTTAB)                                                        
ACLTTABX DC    A(CLTTABX)                                                       
AOFCTAB  DC    A(OFCTAB)                                                        
AOFCTABX DC    A(OFCTABX)                                                       
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
         EJECT                                                                  
         DS    0D                                                               
SRREC    DS    0CL60               *** SORT RECORD DEFINITION ***               
*                                                                               
SRKEY    DS    0CL20                                                            
SRTYPE   DS    CL1                 (FOR BUFFALO ONLY)                           
SRSPR    DS    CL2                                                              
SRMGR1   DS    CL2                                                              
SRMGR2   DS    CL2                                                              
SRMGR3   DS    CL2                                                              
SRMKT    DS    CL2                                                              
SRSTA    DS    CL3                                                              
SROFC    DS    CL2                                                              
SRCLT    DS    CL2                                                              
SRYRMON  DS    CL2                                                              
*                                                                               
SRSPOTS  DS    PL8'0'                                                           
SRORDG   DS    PL8'0'                                                           
SRORDN   DS    PL8'0'                                                           
SRPAIDG  DS    PL8'0'                                                           
SRPAIDN  DS    PL8'0'                                                           
*                                                                               
SRNEW    DS    CL60                                                             
SRSAVE   DS    CL20                SAVE AREA FOR SRKEY                          
*                                                                               
SVBUFF   DS    0XL4                SAVE AREA FOR BUFFALO KEY                    
SVBUFFOF DS    XL2                                                              
SVBUFFCL DS    XL2                                                              
         DS    0D                                                               
         DC    CL8'*BUFFREC'                                                    
BUFFREC  DS    0XL48                                                            
*                                                                               
BUFFOFC  DS    XL2                                                              
BUFFCLT  DS    XL2                                                              
BUFFYM   DS    XL2                                                              
         DS    XL2                                                              
*                                                                               
BUFFDATA DS    0XL40                                                            
BUFFSPTS DS    PL8                                                              
BUFFORDG DS    PL8                                                              
BUFFORDN DS    PL8                                                              
BUFFPDG  DS    PL8                                                              
BUFFPDN  DS    PL8                                                              
         EJECT                                                                  
BUFFEOF  DS    C                                                                
BUFFLVL  DS    C                                                                
BUFFCNTR DC    H'0'                                                             
         DS    0D                                                               
BUFFTOTS DS    XL40                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*BUFFER*'                                                    
BUFFER   DC    26XL48'00'                                                       
*                                                                               
BUYTOTS  DS    26XL20              INTEGER - FOR INPUT PHASE ONLY               
ESTTOTS  DS    26XL20              INTEGER - FOR INPUT PHASE ONLY               
*                                                                               
         DS    0D                                                               
         DC    CL8'*CLTTAB*'                                                    
CLTTAB   DS    4096C               PACKED CLT CODES                             
CLTTABX  EQU   *                     THAT NEED SPECIAL DECODE                   
*                                                                               
         DS    0D                                                               
         DC    CL8'*OFCTAB*'                                                    
OFCTAB   DS    256CL10             2 BYTE CODE, 8 CHAR NAME                     
OFCTABX  EQU   *                                                                
         DC    2X'00'                                                           
         EJECT                                                                  
*===================================================================            
* CALL OFFICER TO BUILD 2 CHAR OFFICE TABLE AND GET NAMES                       
*===================================================================            
                                                                                
BLDOFFC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,AOFCTAB          CLEAR OFFICE TABLE                           
         LHI   R1,OFCTABX-OFCTAB                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    OFCWORK,OFCWORK                                                  
         LA    R4,OFCWORK                                                       
         USING OFFICED,R4                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,AGY                                                       
         L     RF,ADCONLST                                                      
         L     RF,VOFFICER-SPADCONS(RF)                                         
         GOTO1 (RF),DMCB,(C'2',(R4)),(X'F0',ACOMFACS),AOFCTAB                   
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===================================================================*           
* HEADHOOK PROCESSING                                               *           
*===================================================================*           
                                                                                
SSHDHK   NMOD1 0,SSHDHK                                                         
         LM    R9,RA,HDHKR9                                                     
         L     RC,HDHKRC                                                        
         B     HDHK0                                                            
HDHKR9   DC    A(0)                                                             
HDHKRA   DC    A(0)                                                             
HDHKRB   DC    A(0)                                                             
HDHKRC   DC    A(0)                                                             
*                                                                               
HDHK0    L     RE,=A(HLDATES)                                                   
         MVC   H3+39(32),0(RE)                                                  
*                                                                               
         CLI   QRERATE,C'Y'        FILTER ON STATION GROUP                      
         BNE   HDHK01                                                           
         L     RE,=A(SVGRPBK1)                                                  
         MVC   H6+76(L'SVGRPBK1),0(RE)      BREAK 1 TITLE                       
         L     RE,=A(SVPGNAM1)                                                  
         MVC   H6+90(L'SVPGNAM1),0(RE)      GROUP 1 NAME                        
         L     RE,=A(SVGRPBK2)                                                  
         MVC   H7+76(L'SVGRPBK2),0(RE)      BREAK 2 TITLE                       
         L     RE,=A(SVPGNAM2)                                                  
         MVC   H7+90(L'SVPGNAM2),0(RE)      GROUP 2 NAME                        
*                                                                               
HDHK01   CLI   QOPT4,C'Y'          TEST SUPPRESS NET                            
         BNE   *+16                                                             
         MVC   H8+57(75),SPACES                                                 
         MVC   H9+57(75),SPACES                                                 
*                                                                               
         L     RF,=A(SRSPR)                                                     
         OC    0(2,RF),0(RF)       TEST SPECIAL REP PRESENT                     
         BZ    HDHK1               NO                                           
         CLC   0(2,RF),=X'FFFF'    TEST REPORT TOTALS                           
         BE    HDHK1               YES - SKIP                                   
         L     RE,=A(SRHEAD)                                                    
         MVC   H1+40(29),0(RE)                                                  
         L     RE,=A(SRHEADU)                                                   
         MVC   H2+40(29),0(RE)                                                  
*                                                                               
         MVC   DUB(2),0(RF)        MOVE 2-BYTE REP CODE                         
         GOTO1 VRCPACK,DMCB,(C'U',DUB),FULL                                     
         B     HDHK2                                                            
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
         L     R1,=A(SPCLREP)                                                   
         CLI   QREPTYPE,C'S'                                                    
         BE    HDHK3X                                                           
         L     R1,=A(TMSHREP)                                                   
         CLI   QREPTYPE,C'T'                                                    
         BE    HDHK3X                                                           
         L     R1,=A(PYNGREP)                                                   
*                                                                               
HDHK3X   MVC   WORK(13),0(R1)                                                   
*                                                                               
         MVC   WORK+14(3),KEY+2                                                 
         L     R1,=A(UNKNOWN)                                                   
         MVC   WORK+20(13),0(R1)                                                
         CLC   KEY(7),0(R6)                                                     
         BNE   *+10                                                             
         MVC   WORK+20(22),RNAME                                                
         GOTO1 SQUASHER,DMCB,WORK,42                                            
         MVC   H6+76(33),WORK                                                   
         DROP  R6                                                               
*                                                                               
HDHK4    MVC   WORK,SPACES                                                      
         CLC   =C'ALL',QCLT                                                     
         BE    HDHK10                                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,QCLT+1           GET 1 BYTE OFFICE CODE                       
         MHI   RE,L'OFCTAB                                                      
         A     RE,AOFCTAB                                                       
         MVC   OFFICE1,0(RE)                                                    
*                                                                               
         SR    RE,RE                                                            
         IC    RE,QCLT+2                                                        
         MHI   RE,L'OFCTAB                                                      
         A     RE,AOFCTAB                                                       
         MVC   OFFICE2,0(RE)                                                    
*                                                                               
         CLI   QCLT,C'$'           TEST OFFICE LIST                             
         BNE   HDHK5               NO                                           
         L     RE,=A(OFLXHD)                                                    
         MVC   WORK(L'OFLXHD),0(RE)                                             
         MVC   WORK+15(2),OFFICE1                                               
         B     HDHK10X                                                          
*                                                                               
HDHK5    CLI   QCLT,C'*'           TEST OFFICES                                 
         BNE   HDHK8               NO                                           
         CLI   QCLT+1,C'-'         TEST NEGATIVE FILTER                         
         BE    HDHK6                                                            
         L     RE,=A(OFCXHD)                                                    
         MVC   WORK(L'OFCXHD),0(RE)                                             
         MVC   WORK+10(2),OFFICE1                                               
         B     HDHK10X                                                          
*                                                                               
HDHK6    L     RE,=A(OFCNXHD)                                                   
         MVC   WORK(L'OFCNXHD),0(RE)                                            
         MVC   WORK+29(2),OFFICE2                                               
         B     HDHK10X                                                          
*                                                                               
HDHK8    L     RE,=A(CLXXXHD)                                                   
         MVC   WORK(21),0(RE)                                                   
         MVC   WORK+10(3),QCLT                                                  
*                                                                               
HDHK10   CLI   QOPT7,C' '                                                       
         BE    HDHK10X                                                          
         L     RE,=A(CLXCDHD)                                                   
         MVC   WORK(25),0(RE)                                                   
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
         L     RE,=A(UNPORD)                                                    
         MVC   H5+39(31),0(RE)                                                  
*                                                                               
HDHK14   CLI   QOPT6,C'2'                                                       
         BNE   HDHK16                                                           
         L     RE,=A(UNPONLY)                                                   
         MVC   H5+43(23),0(RE)                                                  
         EJECT                                                                  
HDHK16   CLI   MODE,MKTLAST                                                     
         BH    HDHK20                                                           
         L     RF,=A(SRMKT)                                                     
         CLC   0(2,RF),=X'FFFF'                                                 
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
         CLI   STASW,C'Y'          TEST STATION NAME REQUIRED                   
         BNE   HDHK26                                                           
         MVC   H10+1(11),=C'(CONTINUED)'                                        
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         MVC   PSTA,SVPSTA                                                      
         DROP  R2                                                               
         EJECT                                                                  
* PRINT AFFILIATE FILTER *                                                      
         SPACE 1                                                                
HDHK26   CLI   QAFFIL,C' '                                                      
         BE    HDHK26X                                                          
         L     RE,=A(AFFONLY)                                                   
         MVC   H7+1(25),0(RE)                                                   
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
         MVC   H7+4(3),=C'FOX'                                                  
         CLI   QAFFIL,C'F'                                                      
         BE    HDHK26X                                                          
         MVC   H7+4(3),SPACES                                                   
         MVC   H7+5(1),QAFFIL                                                   
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
         L     RF,=A(SRSPR)                                                     
         CLC   0(2,RF),=X'FFFF'                                                 
         BE    HDHK34                                                           
         OC    0(2,RF),0(RF)       TEST SPECIAL REP SUMMARY                     
         BZ    *+8                 NO                                           
         LA    R1,=CL12'SPECIAL REP'                                            
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
HDHK40   CLI   PROGPROF+3,C'Y'     TEST SUPPRESS REP $                          
         BNE   HDHK42                                                           
         L     RF,=A(SRSPR)                                                     
         CLC   0(2,RF),=X'FFFF'    TEST REPORT TOTALS                           
         BE    HDHK41                                                           
         OC    0(2,RF),0(RF)       TEST SPECIAL REP REPORT                      
         BNZ   HDHK42                                                           
*                                                                               
HDHK41   L     RE,=A(SPRDOLS)                                                   
         MVC   MID1(39),0(RE)                                                   
*                                                                               
HDHK42   XIT1                                                                   
*                                                                               
SRHEAD   DC   CL29'SPECIAL REP STATION SUMMARY'                                 
SRHEADU  DC   CL29'---------------------------'                                 
SPCLREP  DC   CL13'SPECIAL REP'                                                 
TMSHREP  DC   CL13'TIMESHEET REP'                                               
PYNGREP  DC   CL13'PAYING REP'                                                  
UNKNOWN  DC   CL13'UNKNOWN'                                                     
OFLXHD   DC   CL33'** OFFICE LIST XX CLIENTS ONLY **'                           
OFCXHD   DC   CL28'** OFFICE XX CLIENTS ONLY **'                                
OFCNXHD  DC   CL34'** ALL CLIENTS EXCEPT OFFICE XX **'                          
CLXXXHD  DC   CL21'** CLIENT XXX ONLY **'                                       
CLXCDHD  DC   CL25'CLIENT EXCLUSION CODE = X'                                   
UNPORD   DC   CL31'UNPAID DETAILS + ORDERED TOTALS'                             
UNPONLY  DC   CL23'** UNPAID ITEMS ONLY **'                                     
AFFONLY  DC   CL25'** NBC AFFILIATES ONLY **'                                   
SPRDOLS  DC   CL39'** NOTE SPECIAL REP DOLLARS EXCLUDED **'                     
OFFICE1  DS   CL2                                                               
OFFICE2  DS   CL2                                                               
         LTORG                                                                  
         EJECT                                                                  
*===================================================================*           
* SUBROUTINE TO TRACE SORT CALLS                                    *           
*===================================================================*           
         SPACE 1                                                                
TRCSORT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,0(R1)                                                         
         CLI   0(RE),C'P'          TEST PUT                                     
         BNE   TRCSORT4                                                         
         L     R4,4(R1)                                                         
         GOTO1 VPRNTBL,TRCP1,=C'PUT',(R4),C'DUMP',56,=C'1D',           X        
               (C'P',PRINT)                                                     
*                                                                               
TRCSORT2 L     RF,TRCSAVE                                                       
         BASR  RE,RF                                                            
         B     TRCSORTX                                                         
*                                                                               
TRCSORT4 CLI   0(RE),C'G'          TEST GET                                     
         BNE   TRCSORT2                                                         
         L     RF,TRCSAVE          GO TO SORTER FOR RECORD                      
         BASR  RE,RF                                                            
         L     R4,4(R1)            GET OUTPUT REC ADDRESS                       
         GOTO1 VPRNTBL,TRCP1,=C'GET',(R4),C'DUMP',56,=C'1D',           X        
               (C'P',PRINT)                                                     
TRCSORTX XIT1                                                                   
*                                                                               
TRCSAVE  DC    A(0)                SAVE AREA FOR A(SORTER)                      
TRCP1    DC    6F'0'                                                            
         EJECT                                                                  
* DSECT FOR DATA IN BUFFER *                                                    
                                                                                
BRECD    DSECT                                                                  
*                                                                               
BRECOFC  DS    XL2                                                              
BRECCLT  DS    XL2                                                              
BRECYM   DS    XL2                                                              
         DS    XL2                                                              
*                                                                               
BRECDATA DS    0XL40                                                            
BRECSPTS DS    PL8                                                              
BRECORDG DS    PL8                                                              
BRECORDN DS    PL8                                                              
BRECPDG  DS    PL8                                                              
BRECPDN  DS    PL8                                                              
         EJECT                                                                  
         SPACE 1                                                                
* BUFFALO FILE DEFINITION *                                                     
         SPACE 1                                                                
         BUFF  LINES=400,ROWS=7,COLUMNS=5,FLAVOR=PACKED,KEYLIST=(8,A)           
         SPACE 2                                                                
* DSECT FOR PRINT LINE DATA *                                                   
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL1                                                              
PSTA     DS    CL8                                                              
         DS    CL1                                                              
PCLT     DS    CL7                                                              
         DS    CL1                                                              
PDATE    DS    CL6                                                              
         DS    CL2                                                              
PSPOTS   DS    CL8                                                              
         DS    CL3                                                              
PORDG    DS    CL15                                                             
         DS    CL2                                                              
PORDN    DS    CL15                                                             
         DS    CL2                                                              
PPAIDN   DS    CL15                                                             
         DS    CL3                                                              
PPCTPAID DS    CL3                                                              
         DS    CL2                                                              
PUNPAIDN DS    CL15                                                             
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
QOPT6    EQU   QOPT5+1                                                          
QOPT7    EQU   QOPT5+2                                                          
QSUMONLY EQU   QDPTMENU                                                         
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPGENGRP                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SPREPA602 08/25/05'                                      
         END                                                                    
