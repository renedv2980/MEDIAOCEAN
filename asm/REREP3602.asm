*          DATA SET REREP3602  AT LEVEL 040 AS OF 05/01/02                      
*PHASE RE3602C,*                                                                
         TITLE 'OFFICE SHARE SUMMARY'                                           
***********************************************************************         
* HISTORY OF CHANGES                                                  *         
***********************************************************************         
* OCT24/91 (BU ) --- CHANGES FOR NEW SPACEND FACILITIES AND LAYOUT    *         
*                                                                     *         
* DEC05/91 (BU ) --- INSTALL VALUENEW FACILITY                        *         
*                                                                     *         
* MAR25/92 (MRR) --- REMOVE FIXED STORE AND CALL COVAIL               *         
*                                                                     *         
* MAR27/92 (BU ) --- MAKE COMPATIBLE WITH VALU2NEW                    *         
*                    REREPRGEQU ---> REREPRGEQA                       *         
*                                                                     *         
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                  *         
*                                                                     *         
* APR22/97 (BU ) --- IF SINGLE STATION, DON'T DISPLAY TOTALS          *         
*                                                                     *         
* MAY22/97 (BU ) --- UPGRADE FOR YR 2000                              *         
*                                                                   * *         
* JAN23/98 (JRD) --- 4K CONTRACTS                                   * *         
*                                                                   * *         
* APR27/99 (BU ) --- REPLACE REPCOMPNEW WITH REPCOMPNW2 (NEW YR2000 * *         
*                    DATE FORMAT USAGE)                             * *         
*                                                                   * *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
RE3602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE3602,RR=RE                                                 
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         L     R9,ACCUMS                                                        
         EJECT                                                                  
*              CONTROL MODE SETTINGS                                            
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   OF1                                                              
         L     RE,ADCONLST                                                      
         USING ADCONSD,RE                                                       
         L     RF,COVAIL                                                        
         DROP  RE                                                               
         GOTO1 (RF),DMCB,C'GET',400000,400000                                   
         OC    P2,P2                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ACCUMS,P2                                                        
         L     RF,=F'400000'                                                    
         L     RE,ACCUMS                                                        
         XCEF                                                                   
         B     OFEXT                                                            
OF1      EQU   *                                                                
         CLI   MODE,REQFRST                                                     
         BNE   OF2                                                              
         MVI   RCSUBPRG,0                                                       
         CLI   QSEQ,C'S'                                                        
         BE    *+8                                                              
         MVI   RCSUBPRG,1                                                       
         BAS   RE,OFFLIST                                                       
         BAS   RE,INITIAL                                                       
         BAS   RE,INIT1                                                         
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     OFEXT                                                            
         SPACE 2                                                                
OF2      CLI   MODE,STAFRST                                                     
         BNE   OF4                                                              
         BAS   RE,RECOMP                                                        
         MVC   P(4),RSTAKSTA                                                    
         LA    R1,P+3                                                           
         OI    0(R1),C' '                                                       
         CLI   0(R1),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         MVI   0(R1),C'-'                                                       
         MVC   1(2,R1),=C'TV'                                                   
         CLI   RSTAKSTA+4,C' '                                                  
         BE    OFEXT                                                            
         MVC   1(1,R1),RSTAKSTA+4                                               
         MVI   2(R1),C'M'                                                       
         B     OFEXT                                                            
         SPACE 2                                                                
OF4      CLI   MODE,PROCCONT                                                    
         BNE   OF12                                                             
         SPACE 2                                                                
OF6      BAS   RE,STATPOST                                                      
         BAS   RE,OVERPOST                                                      
         BAS   RE,SUBPOST                                                       
         BAS   RE,GRUPPOST                                                      
         SPACE 1                                                                
         L     R5,ANEWMON          A(NEW MONTH TABLE)                           
MONT0002 EQU   *                                                                
         CLC   0(4,R5),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    MONT0006            FOUND - CONTINUE                             
         LA    R5,NEXTBUCK(R5)     BUMP TO NEXT BUCKET                          
         B     MONT0002            GO BACK FOR NEXT                             
MONT0006 CLI   0(R5),0                                                          
         BE    OFEXT                                                            
         CLC   0(4,R5),QFAEND      TABLE ENTRY VS REQ END   DATE                
         BH    OFEXT               TABLE > END DATE - EXIT                      
         LA    R2,3                POST ONE MONTH AT                            
         CLI   7(R5),C'N'          A TIME FOR OLD, NEW                          
         BE    MONT0010            COMPARABLE                                   
         LA    R2,2                                                             
         CLI   7(R5),C'O'                                                       
         BE    MONT0010                                                         
         LA    R2,1                                                             
         SPACE 1                                                                
MONT0010 ST    R5,TABS             A(MONTH IN PROGRESS)                         
         BAS   RE,ALLPOST                                                       
         LA    R5,NEXTBUCK(R5)                                                  
         BCT   R6,MONT0006                                                      
         B     OFEXT                                                            
         SPACE 2                                                                
OF12     CLI   MODE,STALAST                                                     
         BNE   OF13                                                             
         BAS   RE,STATOTS                                                       
         B     OFEXT                                                            
         SPACE 2                                                                
OF13     CLI   MODE,SUBLAST                                                     
         BNE   OF14                                                             
         CLC   QSTATION,SPACES                                                  
         BNE   OFEXT                                                            
         BAS   RE,SUBTOTS                                                       
         B     OFEXT                                                            
         SPACE 2                                                                
OF14     CLI   MODE,REQLAST                                                     
         BNE   OF16                                                             
         LH    R2,LINARA                                                        
         MH    R2,=H'2'                                                         
         BCTR  R2,0                                                             
         MH    R2,=H'48'                                                        
         LA    R2,0(R2,R9)         OVERALL TOTAL FOR COMPARABLE                 
         LH    R3,LINARA                                                        
         MH    R3,=H'5'                                                         
         BCTR  R3,0                                                             
         MH    R3,=H'48'                                                        
         LA    R3,0(R3,R9)         OVERALL TOTAL FOR ALL STATIONS               
         CLC   20(28,R2),20(R3)                                                 
         BE    OFEXT                                                            
         CLC   QSTATION,SPACES     ANY STATION FILTER?                          
         BNE   OFEXT               YES - SKIP THE TOTALS                        
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         BAS   RE,COMPTOTS                                                      
         BAS   RE,OLDTOTS                                                       
         BAS   RE,NEWTOTS                                                       
         BAS   RE,OVERTOTS                                                      
         MVI   RCSUBPRG,3                                                       
         MVI   FORCEFUT,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     OFEXT                                                            
         SPACE 2                                                                
OF16     CLI   MODE,GRUPLAST                                                    
         BNE   OF18                                                             
         CLC   QSTATION,SPACES                                                  
         BNE   OFEXT                                                            
         CLI   QSBGROUP,C' '                                                    
         BNE   OFEXT                                                            
         BAS   RE,GRUPTOTS                                                      
         B     OFEXT                                                            
         SPACE 2                                                                
OF18     CLI   MODE,SUBFRST                                                     
         BNE   OFEXT                                                            
         MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
OFEXT    XMOD1 1                                                                
         EJECT                                                                  
*              POSTING ROUTINES                                                 
         SPACE 3                                                                
STATPOST SR    R2,R2                                                            
         XC    TABS,TABS                                                        
         B     ALLPOST                                                          
         SPACE 2                                                                
OVERPOST LA    R2,4                                                             
         XC    TABS,TABS                                                        
         B     ALLPOST                                                          
         SPACE 2                                                                
SUBPOST  LA    R2,5                                                             
         XC    TABS,TABS                                                        
         B     ALLPOST                                                          
         SPACE 2                                                                
GRUPPOST LA    R2,6                                                             
         XC    TABS,TABS                                                        
         SPACE 2                                                                
ALLPOST  NTR1                                                                   
         MH    R2,LINARA           LINES IN AN AREA                             
         MH    R2,=H'48'                                                        
         AR    R2,R9                                                            
         LH    R3,NUMOFF                                                        
         SPACE 2                                                                
ALLPOST2 CLC   0(20,R2),ROFFNAME   FIND OFFICE MATCH                            
         BNE   ALLPOST4                                                         
         BAS   RE,POSTEM           AND ADD                                      
         SPACE 2                                                                
ALLPOST4 LA    R2,48(R2)                                                        
         BCT   R3,ALLPOST2                                                      
         BAS   RE,POSTEM           ADD INTO TOTALS LINE                         
         B     XIT                                                              
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
*              ADD FROM MONTABLE TO ACCUMS AT 20(R2)                            
         SPACE 3                                                                
POSTEM   NTR1                                                                   
         L     R3,ANEWMON          A(NEW MONTH TABLE)                           
POST0002 EQU   *                                                                
         CLC   0(4,R3),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST0004            FOUND                                        
         LA    R3,NEXTBUCK(R3)     BUMP TO NEXT BUCKET                          
         B     POST0002            GO BACK FOR NEXT                             
POST0004 EQU   *                                                                
         LA    R4,24                                                            
         OC    TABS,TABS           STATION, OVER,SUB, OR GROUP POST?            
         BZ    POST0006            YES - DO ALL BUCKETS                         
         L     R3,TABS             FOR OLD , NEW, COMPARABLE                    
*                                  A(NEW MONTH TABLE ENTRY)                     
         LA    R4,1                ONLY DO SINGLE BUCKET                        
*                                                                               
POST0006 CLI   0(R3),0             THIS YEAR ESTIMATE                           
         BE    XIT                                                              
         CLC   0(4,R3),QFAEND      TABLE ENTRY VS REQ END   DATE                
         BH    XIT                 TABLE > END DATE - EXIT                      
         LR    R6,R3               SET A(BUCKETS IN MONTH)                      
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROLS                          
*                                  CURRENT ESTIMATE BUCKET                      
         L     R5,TOTORD(R6)       CURR AS AT ORD AMT                           
         TM    FLAG6(R3),X'01'     ANY CURR INV?                                
         BZ    *+8                 NO                                           
         L     R5,CUASATIN(R6)     YES - CURR AS AT INV AMT                     
         A     R5,20(R2)                                                        
         ST    R5,20(R2)                                                        
         SPACE 2                                                                
*                                  PRIOR ESTIMATE BUCKET                        
         L     R5,PRASATOR(R6)     PRIOR AS AT EST AMT                          
         TM    FLAG6(R3),X'04'     ANY PRIOR INV (AS AT)?                       
         BZ    *+8                 NO                                           
         L     R5,PRASATIN(R6)     YES - PRIOR AS AT INV AMT                    
         A     R5,28(R2)                                                        
         ST    R5,28(R2)                                                        
         SPACE 2                                                                
*                                  PRIOR ACTUAL BUCKET                          
         L     R5,PRTOTORD(R6)     PRIOR TOT EST AMT                            
         TM    FLAG6(R3),X'02'     ANY PRIOR INV?                               
         BZ    *+8                 NO                                           
         L     R5,PRTOTINV(R6)     YES - PRIOR TOT INV AMT                      
         A     R5,36(R2)                                                        
         ST    R5,36(R2)                                                        
*                                                                               
         LA    R3,NEXTBUCK(R3)                                                  
         BCT   R4,POST0006         LOOP ON COUNT                                
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL OF TOTALLING ROUTINES                                    
         SPACE 3                                                                
STATOTS  SR    R2,R2                                                            
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         B     ALLTOTS4                                                         
         SPACE 2                                                                
SUBTOTS  LA    R2,5                                                             
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   P(6),=C'SUBGRP'                                                  
         MVC   P+7(1),RGRPKGRP+1                                                
         B     ALLTOTS4                                                         
         SPACE 2                                                                
GRUPTOTS LA    R2,6                                                             
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   P(5),=C'GROUP'                                                   
         MVC   P+6(1),RGRPKGRP                                                  
         B     ALLTOTS4                                                         
         SPACE 2                                                                
COMPTOTS LA    R2,1                                                             
         MVC   MID1+1(19),=C'COMPARABLE STATIONS'                               
         MVC   MID2+1(19),=25C'-'                                               
         B     ALLTOTS                                                          
         SPACE 2                                                                
OLDTOTS  LA    R2,2                                                             
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID1+1(12),=C'OLD STATIONS'                                      
         MVC   MID2+1(12),=25C'-'                                               
         B     ALLTOTS2                                                         
         SPACE 2                                                                
NEWTOTS  LA    R2,3                                                             
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID1+1(12),=C'NEW STATIONS'                                      
         MVC   MID2+1(12),=25C'-'                                               
         B     ALLTOTS2                                                         
         SPACE 2                                                                
OVERTOTS LA    R2,4                                                             
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID1+1(21),=C'ALL STATIONS COMBINED'                             
         MVC   MID2+1(21),=25C'-'                                               
         B     ALLTOTS2                                                         
         SPACE 2                                                                
ALLTOTS  MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
ALLTOTS2 MVI   FORCEMID,C'Y'                                                    
         EJECT                                                                  
*              NOW HANDLE THE BLOCK                                             
         SPACE 3                                                                
ALLTOTS4 NTR1                                                                   
         MH    R2,LINARA                                                        
         MH    R2,=H'48'                                                        
         AR    R2,R9                                                            
         LH    R3,LINARA                                                        
         BAS   RE,PERCENT                                                       
         LH    R8,NUMOFF                                                        
         GOTO1 XSORT,DMCB,(1,(R2)),(R8),48,4,20                                 
         MVI   ACTSW,C'N'                                                       
         SPACE 2                                                                
TOT2     OC    20(24,R2),20(R2)                                                 
         BZ    TOT4                                                             
         MVI   ACTSW,C'Y'                                                       
         MVC   P+9(20),0(R2)                                                    
         LA    R4,20(R2)                                                        
         LA    R5,P+32                                                          
         BAS   RE,TOT8                                                          
         LA    R4,28(R2)                                                        
         LA    R5,P+58                                                          
         BAS   RE,TOT8                                                          
         LA    R4,36(R2)                                                        
         LA    R5,P+84                                                          
         BAS   RE,TOT8                                                          
         GOTO1 REPORT                                                           
         XC    20(24,R2),20(R2)                                                 
         SPACE 2                                                                
TOT4     LA    R2,48(R2)                                                        
         BCT   R3,TOT2                                                          
         CLI   ACTSW,C'Y'                                                       
         BE    TOT6                                                             
         MVC   P,SPACES                                                         
         B     XIT                                                              
         SPACE 2                                                                
TOT6     GOTO1 REPORT                                                           
         B     XIT                                                              
         SPACE 2                                                                
TOT8     OC    0(4,R4),0(R4)                                                    
         BZ    TOT10                                                            
         EDIT  (4,(R4)),(11,(R5)),COMMAS=YES,FLOAT=-                            
         SPACE 2                                                                
TOT10    OC    4(4,R4),4(R4)                                                    
         BCR   8,RE                                                             
         EDIT  (4,4(R4)),(6,15(R5)),2                                           
         BR    RE                                                               
         EJECT                                                                  
*              COMPUTE PERCENTAGES FOR A BLOCK                                  
         SPACE 3                                                                
PERCENT  NTR1                                                                   
         LH    R4,NUMOFF                                                        
         MH    R4,=H'48'                                                        
         AR    R4,R2                                                            
         SPACE 2                                                                
PERCENT2 LA    R5,20(R2)                                                        
         LA    R6,20(R4)                                                        
         BAS   RE,PERCENT6                                                      
         LA    R5,28(R2)                                                        
         LA    R6,28(R4)                                                        
         BAS   RE,PERCENT6                                                      
         LA    R5,36(R2)                                                        
         LA    R6,36(R4)                                                        
         BAS   RE,PERCENT6                                                      
         LA    R2,48(R2)                                                        
         BCT   R3,PERCENT2                                                      
         B     XIT                                                              
         SPACE 2                                                                
PERCENT6 L     R1,0(R5)                                                         
         LTR   R1,R1                                                            
         BNPR  RE                                                               
         OC    0(4,R6),0(R6)                                                    
         BCR   8,RE                                                             
         M     R0,=F'20000'                                                     
         D     R0,0(R6)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,4(R5)                                                         
         BR    RE                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
OFFLIST  NTR1                                                                   
         LA    R2,OFFNAM                                                        
         SR    R3,R3                                                            
         SR    R4,R4                                                            
         L     R5,ASPACND4         A(1ST ENTRY IN SPACEND TABLE)                
OFFLST00 EQU   *                                                                
         CLI   0(R5),4                                                          
         BE    OFFLST04            OFFICE REC IN SPACEND FOUND                  
         IC    R4,1(R5)                                                         
         AR    R5,R4                                                            
         B     *-14                                                             
         SPACE 1                                                                
OFFLST04 EQU   *                                                                
         CLC   RCREPFL,2(R5)       SAME REP?                                    
         BNE   OFFLST16            NO  - KEEP LOOKING                           
OFFLST08 CLC   QOFFICE,SPACES                                                   
         BE    OFFLST12                                                         
         CLC   QOFFICE,6(R5)       COMPARE FOR OFFICE CODE                      
         BNE   OFFLST16            NOT FOUND                                    
         SPACE 1                                                                
OFFLST12 MVC   0(20,R2),8(R5)      FOUND - TAKE OFFICE NAME                     
         LA    R2,20(R2)                                                        
         AH    R3,=H'1'                                                         
         SPACE 1                                                                
OFFLST16 IC    R4,1(R5)                                                         
         AR    R5,R4                                                            
         CLI   0(R5),4                                                          
         BE    OFFLST04                                                         
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STH   R3,NUMOFF                                                        
         MVC   0(20,R2),SPACES                                                  
         AH    R3,=H'1'                                                         
         STH   R3,LINTYP                                                        
         B     XIT                                                              
         EJECT                                                                  
INITIAL  NTR1                                                                   
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFASTART)                                
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFAEND)                                  
*                                  CONVERT ORIGINAL EBCDIC DATE TO              
*                                     SPECIAL YR 2000 FORMAT                    
         MVC   NUMAREA,=H'7'                                                    
         LH    R3,NUMOFF           NUMBER OF OFFICES                            
         MH    R3,=H'48'                                                        
         STH   R3,SIZOFF           SIZE OF AREA LINE 1- X                       
         AH    R3,=H'48'                                                        
         STH   R3,SIZTYP           SIZE OF AREA LINE 1 - TOTAL                  
         LH    R3,LINTYP                                                        
         STH   R3,LINARA           LINES FOR ALL TYPES                          
         MH    R3,NUMAREA                                                       
         STH   R3,TOTLIN           TOTAL NUMBER OF LINES                        
         SPACE 1                                                                
         B     XIT                                                              
         SPACE 1                                                                
INIT1    NTR1                                                                   
         L     R2,ACCUMS                                                        
         LH    R3,NUMAREA          OFFICE NAMES TO TABLE                        
INIT3    LA    R4,OFFNAM                                                        
         LH    R5,NUMOFF                                                        
INIT4    MVC   0(20,R2),0(R4)                                                   
         XC    20(28,R2),20(R2)                                                 
         LA    R2,48(R2)                                                        
         LA    R4,20(R4)                                                        
         BCT   R5,INIT4                                                         
         MVC   0(20,R2),=CL20'** TOTALS **'                                     
         XC    20(28,R2),20(R2)                                                 
         LA    R2,48(R2)                                                        
         BCT   R3,INIT3                                                         
         B     XIT                                                              
         EJECT                                                                  
       ++INCLUDE REPCOMPNW2                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
RELO     DS    A                                                                
ACTSW    DC    C'N'                                                             
TABS     DS    F                   A(MONTH IN PROGRESS)                         
ACCUMS   DS    F                   A(ACCUMS)                                    
NUMAREA  DS    H                   NUMBER OF AREAS                              
NUMOFF   DS    H                   NUMBER OF OFFICES                            
SIZOFF   DS    H                   SIZE OF OFFICE AREA FOR A TYPE               
LINTYP   DS    H                   LINES PER TYPE                               
SIZTYP   DS    H                   SIZE OF TYPE AREA                            
LINARA   DS    H                   LINES IN AN AREA                             
TOTLIN   DS    H                   TOTAL NUMBER OF LINES                        
         SPACE 1                                                                
OFFNAM   DS    30CL20                                                           
*                                                                               
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
         EJECT                                                                  
       ++INCLUDE REMONARCHD                                                     
         SPACE 2                                                                
         EJECT                                                                  
*              DESIGN OF ACCUMULATORS                                           
         SPACE 3                                                                
*              LINES 1-40          STATION                                      
*                    41-80         COMPARABLE STATIONS                          
*                    81-120        OLD STATIONS                                 
*                    121-160       NEW STATIONS                                 
*                    161-200       ALL STATIONS                                 
*                                                                               
*              BYTES 1-20          OFFICE NAME                                  
*                    21-24         THIS YEAR ESTIMATE AMOUNT                    
*                    25-28                            SHARE                     
*                    29-32         LAST YEAR ESTIMATE AMOUNT                    
*                    33-36                            SHARE                     
*                    37-40         LAST YEAR ACTUAL   AMOUNT                    
*                    41-44                            SHARE                     
*                    45-48         SPARE                                        
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040REREP3602 05/01/02'                                      
         END                                                                    
