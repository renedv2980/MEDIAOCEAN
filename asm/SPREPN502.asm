*          DATA SET SPREPN502  AT LEVEL 095 AS OF 05/20/15                      
*PHASE SPN502A                                                                  
*INCLUDE MEDBDESC                                                               
*INCLUDE REVBUY                                                                 
*INCLUDE SPRPFOOT                                                               
*INCLUDE REPSPILL                                                               
*INCLUDE REPUDESC                                                               
*INCLUDE REPCALOV                                                               
         PRINT NOGEN                                                            
         TITLE 'SPREPN502- POL TIMESHEETS'                                      
* PWES 078 18MAR04 FIX BUG IN LVL76 - DON'T USE R4 AT M6CUTCHK                  
* PWES 077 17MAR04 REDUCE SOFT DEMO I/O OVERHEAD IN MEDGETBY                    
* PWES 076 01MAR04 CUTINS=O + FIX UNAL CUTINS & ZERO COST OVERRIDES             
* PWES 075 15JAN04 FIX LVL60 USE OF STATION FIELD (IS FILENAME!!!)              
* PWES 074 24JUL03 FIX 'ALL' EST ('ALL' STN/MKT) BYID REQUEST                   
* PWES 073 27JUN03 FIX LCLSPILL BUG WHERE BCT COUNTER INIT=0 (HI CPU)           
* PWES 072 26JUN03 MOVE CUTIN CSECT STORAGE BACK AS WAS (CAUSED DUMPS)          
* PWES 071 23JUN03 SORT BY PROG/DAY/TIME + FIX SORTED ALL NWK REQUESTS          
*                  + SOFTEN TABLES/DON'T DIE IF FILL + FIX SPOD PRINTNG         
*                  + SHOW RATINGS (INCLUDING SPILL) IN 'LOCAL DEMOS'            
*                  + SOFTEN PRINTING + REARRANGE STORAGE FOR Z/ARCHTECR         
* PWES 070 20MAR03 POST BUY OVERRIDE DEMOS VERSION                              
* PWES 066 22FEB02 CORRECT CUTIN MSUNPK CALL                                    
* PWES 065 17OCT01 HANDLE NEW CANADIAN CABLE STATION FORMAT + ROUTINE           
*                  HEADINGS/COMMENTS TO MAKE EASIER TO NAVIGATE CODE            
* TZIH 064 09OCT01 DEMOCON CALL CHANGE BATCH VII                                
* PWES 063 31MAY01 TOTALS FOR 'ALL' NWKS / MKT=0000                             
* BZEH 062 10MAY99 ??? BUY COMMENTS ???                                         
*                                                                               
**********************************************************************          
* BUFFALO VALUES                                                                
* RECORD KEY - USED HERE (IN ADDITION TO M2/M3/M4 VALUES !)                     
*   SPILL    = X'88' / X'91' (FOR SUMMARY ?)                                    
*   ORIG     = X'89' / X'92'                                                    
*   TOTAL    = X'90' / X'93'                                                    
*   M2/M3/M4 = X'21' / X'22' (X'41/42/61/62' NOT USED AS NOT MEDIA C)           
*                                                                               
* PROGPROF / OPTION SETTINGS                                                    
* +6  ---> QOPT1  CUT INS                                                       
* +7  ---> QOPT2  COST O/R  -- NETWORK LEVEL ONLY                               
* +11 ---> QOPT3  LOCAL DEMOS                                                   
*                                                                               
**********************************************************************          
         SPACE                                                                  
SPN502   CSECT                                                                  
         NMOD1 0,SPN502                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R2,=A(SPN5WK)                                                    
         USING SPN5WK,R2                                                        
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         STM   RA,RC,SP60RA                                                     
         ST    R2,SP60R2                                                        
         MVC   MKT,=C'0000'        NETWORK IN CANADA                            
         CLI   MODE,MKTLAST                                                     
         BL    BYPW                                                             
* NOTE: PATCH DURING TESTING SHOWS DON'T APPEAR TO GET MKTLAST !!!              
*        BNE   *+6                                                              
*        DC    H'0'                                                             
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         MVC   WEIGHT,SPWEIGHT                                                  
BYPW     DS    0H                                                               
         CLI   MODE,ESTFRST                                                     
         BNH   *+12                                                             
         CLI   QSTART,C' '         WAS REQUEST VALID                            
         BE    M34                 ALWAYS DO REQLAST                            
         GOTO1 =V(SPRPFOOT),DMCB,(RA)                                           
         CLI   MODE,RUNFRST                                                     
         BNE   M1                                                               
         LA    RE,CLRSTART         INITIALIZE WORK AREA                         
         LA    RF,CLREND                                                        
         SR    RF,RE                                                            
         XCEF                                                                   
         MVC   SVMAXLIN,MAXLINES                                                
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         MVI   FIRST,1                                                          
         B     EXIT                                                             
         SPACE 2                                                                
M1       CLI   MODE,MKTFRST                                                     
         BL    M2                                                               
         CLI   MODE,REQLAST        ALWAYS PROCESS REQLAST                       
         BE    M34                                                              
         CLI   ESTACT,0            ANY ESTIMATES FOR PRODUCT                    
         BE    EXIT                                                             
         EJECT                                                                  
M2       CLI   MODE,REQFRST                                                     
         BNE   M4                                                               
         MVC   N5OPTS,QOPT1                                                     
         LA    RF,PTSDESC                                                       
         ST    RF,APTSDESC                                                      
         LA    RE,MYHEAD                                                        
         ST    RE,HEADHOOK                                                      
         LA    RE,TRAPGTBY                                                      
         ST    RE,ATRAPBUY                                                      
*                                                                               
         BRAS  RE,RQFIRST                                                       
         BRAS  RE,RQFRSTA                                                       
         MVI   MODE,RUNFRST                                                     
         BAS   R9,GOTOSUB                                                       
         MVI   MODE,REQFRST                                                     
M2A      BAS   R9,GOTOSUB                                                       
         MVI   FIRST,0                                                          
         MVC   NUMWK,=F'60'                                                     
         B     EXIT                                                             
         EJECT                                                                  
M4       CLI   MODE,ESTFRST                                                     
         BNE   M5                                                               
         MVC   MSSPPROF,SPOTPROF   INITIALIZE MEDIA SMRY PROFILE                
         MVC   MSSTART,QSTART                                                   
         MVI   ESTACT,1                                                         
         MVI   PASS,0                                                           
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         MVC   BRDPOLSW,CPROF                                                   
         DROP  R6                                                               
         SPACE 2                                                                
         BRAS  RE,EFRSTC                                                        
         B     EXIT                                                             
         SPACE 2                                                                
M5       CLI   MODE,PRDFRST                                                     
         BNE   M6                                                               
         MVI   PASS,0                                                           
         MVI   ESTACT,0            RESET ESTIMATE ACTIVE SWITCH                 
         BAS   R9,GOTOSUB                                                       
         B     EXIT                                                             
         EJECT                                                                  
M6       CLI   MODE,PROCBUY                                                     
         BNE   M8                                                               
         MVI   SORTPASS,SORTINPQ   SET SORT FOR STORE                           
* TEST CONFIRMED ONLY                                                           
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         XC    CFDS(2),CFDS                                                     
         TM    BDCFD,1                                                          
         BZ    *+8                                                              
         MVI   CFDE,C')'                                                        
         TM    BDCFD,2                                                          
         BZ    *+8                                                              
         MVI   CFDS,C'('                                                        
         CLI   QPRGTYPE,C' '       PROGRAM TYPE FILTER                          
         BE    *+14                                                             
         CLC   BDPROGT,QPRGTYPE                                                 
         BNE   EXIT                                                             
         SPACE 2                                                                
         CLI   QOPT4,C' '          CHECK FOR REGION FILTER                      
         BE    *+14                                                             
         CLC   BDNRGN,QOPT4        MATCH THE REGION                             
         BNE   EXIT                                                             
         SPACE 2                                                                
         MVC   SVSPREP,BDREP                                                    
         XC    PGNOENT,PGNOENT                                                  
         MVI   EXTSW,C'N'                                                       
         GOTO1 =A(EXTRCT)                                                       
         OC    PGNOENT,PGNOENT                                                  
         BZ    EXIT                                                             
*                                                                               
M6A1SRT1 L     RE,=A(PGRID)                                                     
         LHI   RF,PGRIDX-PGRID                                                  
         XCEF                                                                   
         XC    PGNOENT,PGNOENT                                                  
         XC    HIATAB,HIATAB                                                    
         XC    PREMTAB,PREMTAB                                                  
M6A1SRT2 CLI   SORTREQ,1                                                        
         BNE   M6A1SRT3                                                         
         BRAS  RE,SORTC                                                         
         CLI   SORTPASS,SORTINPQ   BUILD PASS                                   
         BE    SORTX                YES - EXIT                                  
         CLI   SORTPASS,SORTENDQ   END OF SORT                                  
         BE    SORTX                YES - EXIT                                  
M6A1SRT3 DS    0H                                                               
         MVI   EXTSW,C'N'                                                       
         BRAS  RE,CUTIN                                                         
*                                                                               
         L     RE,=A(PGRID)        CLEAR OUT TABLES                             
         LHI   RF,PGRIDX-PGRID                                                  
         XCEF                                                                   
         XC    PGNOENT,PGNOENT                                                  
         XC    HIATAB,HIATAB                                                    
         XC    PREMTAB,PREMTAB                                                  
         MVI   EXTSW,C'S'          SET TO BUFFER SPOTS                          
*                                                                               
         CLI   QOPT1,C'O'          ONLY WANT SPOTS WITH CUTINS?                 
         BNE   M6CUTOK                                                          
         L     RF,=A(CISLIST)      YEP-CHECK FOR AN ACTIVE CUTIN                
         USING CICLISTD,RF                                                      
M6CUTCHK OC    CICLMS,CICLMS                                                    
         BZ    SORTX                                                            
         CLI   CICLPRD1,CICLNEGQ                                                
         BNE   M6CUTOK                                                          
         LA    RF,CICLSNLQ(RF)                                                  
         B     M6CUTCHK                                                         
         DROP  RF                                                               
M6CUTOK  EQU   *                                                                
*                                                                               
         BRAS  RE,EXTRCT                                                        
*                                                                               
M6NOSPL  CLI   SORTREQ,1                                                        
         BE    M6A1SR30                                                         
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
*                                                                               
M6A1SR30 OC    PGNOENT,PGNOENT                                                  
         BZ    SORTX                                                            
         CLI   IDSW,1              MULTIPLE IDS ON ONE PAGE                     
         BNE   M6A1SR31                                                         
         CLI   FBSTA,C'Y'          FIRST BUY ON CONTRACT                        
         BNE   M6A1SR31                                                         
         CLI   FORCEHED,C'Y'       HEADLINES WILL SET IT                        
         BE    M6A1SR31                                                         
         LA    RE,MID1             FIND PRINT POSITION                          
         CLI   0(RE),C' '                                                       
         BE    *+8                                                              
         LA    RE,132(RE)                                                       
         MVC   0(12,RE),BUYIDNAM                                                
         MVC   13(12,RE),BUYID                                                  
         MVI   FORCEMID,C'Y'                                                    
         SPACE 2                                                                
M6A1SR31 MVI   FBSTA,C'N'          RESET FIRST BUY ON CONTRACT                  
         CLI   PGCNDSW,0           CONDENSE REQUIRED                            
         BE    M6A1SRT4             NO - PRINT LINE                             
         L     R1,PGNOENT                                                       
         LTR   R1,R1                                                            
         BZ    SORTX                                                            
M6A1SRT4 CLI   DETOFLAG,YESSETQ    FLAGGING OVERRIDES?                          
         BNE   M6AA1                                                            
         CLI   PBDREP,C'N'         POST BUY DEMO O/RIDES                        
         BNE   *+12                                                             
         CLI   QBOOK1,C' '         STD REPORT                                   
         BNE   M6AA1                                                            
*                                                                               
         XC    OVRFLAG(4),OVRFLAG                                               
         L     RF,=A(LOCLTAB)      CHECK LOCAL STATIONS FOR O/R                 
         USING LOCDEFD,RF                                                       
M6AA     OC    LOCMS,LOCMS                                                      
         BZ    M6AA1                                                            
         TM    LOCOVRD,LOC1OVRQ                                                 
         BZ    *+8                                                              
         MVI   OVRFLAG,OVRDINDQ                                                 
         TM    LOCOVRD,LOC2OVRQ                                                 
         BZ    *+8                                                              
         MVI   OVRFLAG+1,OVRDINDQ                                               
         TM    LOCOVRD,LOC3OVRQ                                                 
         BZ    *+8                                                              
         MVI   OVRFLAG+2,OVRDINDQ                                               
         TM    LOCOVRD,LOC4OVRQ                                                 
         BZ    *+8                                                              
         MVI   OVRFLAG+3,OVRDINDQ                                               
         LA    RF,LOCDEFLQ(RF)                                                  
         B     M6AA                                                             
         DROP  RF                                                               
*                                                                               
M6AA1    DS    0H                                                               
*        DROP  R6                                                               
         MVI   BUYACT,1                                                         
         MVI   MKTACT,1                                                         
M6A      DS    0H                                                               
         DROP  R5                                                               
         CLI   QPROG,C'U'                                                       
         BNE   M6ANOR                                                           
         GOTO1 VREVBUY,DMCB,(RA)                                                
M6ANOR   MVI   SPLPRINT,0                                                       
         MVI   P1,0                                                             
         ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         CH    RE,=H'7'                                                         
         BH    HLOK                                                             
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
HLOK     DS    0H                  *** CALL PRINT ROUTINES ***                  
         OC    PGNOENT,PGNOENT                                                  
         BZ    SORTX                                                            
         MVI   SPLPRINT,0                                                       
         MVI   CIPRTSW,0           SET TO REPLICATE IN HL                       
         GOTO1 =V(VMDBDESC),DMCB,(RA),PRTLINE                                   
         BRAS  RE,GETCAP                                                        
         MVI   CURRSORT,X'FF'                                                   
         LA    R5,PRTLINE                                                       
         L     RF,DDESC            MOVE IN DESCRIPTION                          
         BASR  R9,RF                                                            
*                                                                               
         L     RE,=A(PLAREA)       CLEAR PRINT LINE SAVE AREA                   
         LHI   RF,PLAREAX-PLAREA                                                
         XCEF                                                                   
         L     R6,DSTAGRID         SET GRID ADDRESS                             
M6AAB    BAS   RE,CSDEMCP          GET DEMOS                                    
         GOTO1 VEDTDEMS            EDIT DEMO/CPP                                
         BAS   R9,BTSPD            MOVE DEMOS/CPP TO PRINT LINE                 
         BAS   R9,BGRID                                                         
*                                                                               
         MVI   CIPRTSW,1           DON'T REPLICATE BDESC IN HL ROUTINE          
         BRAS  RE,PRCOSTOV                                                      
*                                                                               
         BRAS  RE,PRCUTIN                                                       
         SPACE 2                                                                
         CLI   QOPT3,C'N'                                                       
         BE    SORTX                                                            
         BRAS  RE,PRLOCL                                                        
         SPACE 2                                                                
         B     SORTX                                                            
         SPACE 2                                                                
SORTX    MVI   CIPRTSW,0           REPLICATE BDESC IN HL ROUTINE                
         CLI   SORTREQ,1                                                        
         BNE   EXIT                                                             
         CLI   SORTPASS,SORTGETQ   GET PASS                                     
         BNE   EXIT                 NO - EXIT                                   
         CLC   CURRSORT,NEXTSORT                                                
         BE    M6A1SRT2                                                         
         B     M6A1SRT1                                                         
         EJECT                                                                  
M8       CLI   MODE,STAFRST                                                     
         BNE   M10                                                              
* WAS NO STAFRST CALL FROM 03 - THIS ADDED TO ENSURE 02 RESETS ITS              
* STORAGE POINTERS FOR NON ACTIVE STATION (AS PER A STALAST)                    
         MVI   PASS,0                1ST PASS FOR NEW STATION                   
         MVC   PASSQST(12),PASSTAB   BEGIN WITH 1ST PERIOD                      
         MVC   SVMID1,SPACES                                                    
         XC    PDNCNTR,PDNCNTR                                                  
         XC    SSCNTR,SSCNTR                                                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
M10      CLI   MODE,STALAST                                                     
         BNE   M12                                                              
         CLI   SORTREQ,1           SORT REQUIRED                                
         BNE   M10A1                                                            
         MVI   SORTPASS,SORTGETQ    YES - SET SORT PASS FOR EXTRACT             
         MVI   MODE,PROCBUY                                                     
         GOTO1 SORT2                                                            
         XC    PDNCNTR,PDNCNTR                                                  
         XC    SSCNTR,SSCNTR                                                    
         MVI   MODE,STALAST                                                     
M10A1    DS    0H                                                               
*        CLI   BUYACT,0                                                         
*        BE    M10A12                                                           
         GOTO1 VSTATOT                                                          
         OC    OPTRPT,OPTRPT                                                    
         BZ    M10A12                                                           
         MVI   OPTRMODE,STALAST                                                 
         GOTO1 OPTRPT,DMCB,(RA)                                                 
M10A12   DS    0C                                                               
         BAS   R9,GOTOSUB                                                       
         CLI   IDSW,1              MULTIPLE IDS ON PAGE                         
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLI   MODE,REREAD                                                      
         BE    EXIT                                                             
         CLI   CONEND,C'Y'         IS IT END OF CONTRACT                        
         BNE   *+12                NO                                           
         CLI   IDTOTALS,C'Y'       DO WE WANT CONTRACT TOTALS                   
         BNE   EXIT                NO                                           
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         CLI   PASS,0                                                           
         BNE   EXIT                                                             
         MVI   MODE,MKTLAST        FORCE MARKET LAST                            
         B     M12                                                              
         SPACE 2                                                                
*        TRAP FINAL SORT EXIT                                                   
SORT2    NTR1                                                                   
         B     M6A1SRT1                                                         
         SPACE 2                                                                
M12      CLI   MODE,MKTLAST                                                     
         BNE   M16                                                              
         CLC   RTYPE,=C'RS '                                                    
         BE    M14A1                                                            
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
M14A     MVI   BUFCDE,X'90'                                                     
         MVI   LEVEL,1                                                          
         MVC   MCOUNT,=F'1'                                                     
         CLI   MODE,MKTLAST        MAY BE STALAST AT THIS POINT                 
         BE    *+12                                                             
         CLI   BUYACT,1            CHECK FOR STATION BUY ACTIVITY               
         BNE   M14B                                                             
*        CLI   MKTACT,1            CHECK FOR MARKET ACTIVITY                    
*        BNE   M14B                                                             
         MVI   BUFCDE,X'88'        SPILL                                        
         GOTO1 =A(MLASTC)                                                       
         MVI   BUFCDE,X'89'        ORIG                                         
         GOTO1 (RF)                                                             
         MVI   BUFCDE,X'90'        TOTAL                                        
         GOTO1 (RF)                                                             
         OC    SPBUFMKT,SPBUFMKT   PRINT OUT ORIGINATING MARKETS                
         BZ    M14A1A              IF THERE WAS ANY SPILL                       
         MVC   P1(11),=C'***SPILL***'                                           
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'02',SPBUFMKT),P1                       
         GOTO1 REPORT                                                           
         XC    P1,P1                                                            
M14A1A   MVI   BUYACT,0                                                         
M14MACHK CLI   MODE,MKTLAST                                                     
         BNE   M14B                                                             
M14A1    BAS   R9,GOTOSUB          (PRINT M2/M3/M4 - MEDIA SUMMARY)             
         MVI   MODE,STALAST        RESET TO STATION LAST                        
M14B     MVI   PASS,0                                                           
         CLI   IDSW,1              MULTIPLE IDS ON PAGE                         
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
M16      CLI   MODE,PROCGOAL                                                    
         BNE   M18                                                              
         CLI   BUYACT,1                                                         
         BNE   EXIT                                                             
         BAS   R9,GOTOSUB                                                       
         GOTO1 =A(GETGL)                                                        
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
M18      MVI   BUFCDE,X'90'                                                     
         CLI   MODE,PRDLAST                                                     
         BNE   M20                                                              
         MVI   LEVEL,2                                                          
         B     M32A                                                             
*                                                                               
M20      CLI   MODE,MGR1LAST                                                    
         BNE   M22                                                              
         MVI   LEVEL,3                                                          
         B     M32A                                                             
*                                                                               
M22      CLI   MODE,MGR2LAST                                                    
         BNE   M24                                                              
         MVI   LEVEL,4                                                          
         B     M32A                                                             
*                                                                               
M24      CLI   MODE,MGR3LAST                                                    
         BNE   M26                                                              
         MVI   LEVEL,5                                                          
         B     M32A                                                             
         SPACE 2                                                                
M26      MVI   BUFCDE,X'93'                                                     
         CLI   MODE,CLTLAST                                                     
         BNE   M28                                                              
         CLC   QPRD,=C'ALL'                                                     
         BNE   EXIT                                                             
         MVI   LEVEL,2                                                          
         B     M32A                                                             
*                                                                               
M28      CLI   MODE,PGR1LAST                                                    
         BNE   M30                                                              
         MVI   LEVEL,3                                                          
         B     M32A                                                             
*                                                                               
M30      CLI   MODE,PGR2LAST                                                    
         BNE   M32                                                              
         MVI   LEVEL,4                                                          
         B     M32A                                                             
M32      CLI   MODE,PGR3LAST                                                    
         BNE   M34                                                              
         MVI   LEVEL,5                                                          
M32A     BAS   RE,DOSUM                                                         
         B     EXIT                                                             
M34      CLI   MODE,REQLAST                                                     
         BNE   M36                                                              
         CLI   FOOT1,C' '                                                       
         BE    RESPRGM                                                          
         MVI   FORCEHED,C'N'                                                    
         MVI   P,0                                                              
         MVC   P2(132),FOOT1                                                    
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
RESPRGM  CLI   ESTACT,0            BYPASS SUBORDINATE IF NO ACTIVITY            
         BE    *+8                                                              
         BAS   R9,GOTOSUB                                                       
         LA    R6,CURPH01                                                       
         LA    R9,3                                                             
RESPRGM1 L     RF,8(R6)            RESTORE PROGRAMS IN CALLOV AREA              
         L     RE,0(R6)                                                         
         L     R1,4(R6)                                                         
         MOVE  ((RF),(R1)),(RE)                                                 
         LA    R6,12(R6)                                                        
         BCT   R9,RESPRGM1                                                      
         B     EXIT                                                             
         SPACE 2                                                                
M36      BAS   R9,GOTOSUB                                                       
EXIT     XIT1                                                                   
         EJECT                                                                  
GOTOSUB  CLI   PASS,0              PASS = 0                                     
         BNER  R9                   NO - BYPASS SUBPROGRAM                      
         CLI   SUBPSW,0                                                         
         BNE   *+10                                                             
         CLI   MODE,ESTFRST                                                     
         BHR   R9                                                               
         GOTO1 =A(GOSUBC)                                                       
         BR    R9                                                               
         EJECT                                                                  
* BUILD WEEKLY GRID AND SUM INTO STATION BUCKETS  R6=PRINT POSITION             
BGRID    LA    R5,MEDMON01                                                      
         LA    R8,STAGRID                                                       
         ST    R6,FULL             SAVE PRINT LINE ADDRESS                      
BGRID2   L     R4,4(R5)                                                         
         LA    RE,MEDMON13                                                      
         CR    R5,RE                                                            
         BH    BGRID5                                                           
         OC    0(4,R5),0(R5)                                                    
         BZ    BGRID4A                                                          
BGRID4   L     RE,0(R8)            SUM WEEKLY SPOTS                             
         A     RE,MEDBYSPT                                                      
         ST    RE,0(R8)                                                         
         LA    R6,4(R6)                                                         
         LA    R8,4(R8)                                                         
BGRID4A  LA    R5,12(R5)                                                        
         B     BGRID2                                                           
*                                                                               
BGRID5   CLI   MODE,PROCBUY        PRINT PREMPTIONS                             
         BNE   BGRIDX                                                           
         GOTO1 =A(PTSGRID),DMCB,(LENGRID,(RA))                                  
BGRIDX   BR    R9                                                               
         EJECT                                                                  
***********************************************************************         
* POOL TIMESHEET DESCRIPTION (I.E. BUY DETAILS PORTION)               *         
* - MOVE BUY DETAILS TO PRINT LINE(S) ADDRESSED BY R5                 *         
***********************************************************************         
         USING BDEXTD,R5                                                        
PTSDESC  L     RE,ADBUY                 SET BUY DESCRIPTION                     
         USING BUYREC,RE                                                        
         MVC   P2+31(1),BDNRGN                                                  
         DROP  RE                                                               
         MVC   P1(L'BDPEST),BDPEST                                              
         MVC   P2(L'BDPLIN),BDPLIN                                              
         MVC   P1+4(L'BDPBDATE),BDPBDATE                                        
         MVC   P1+3(1),CFDS                                                     
         MVC   P1+15(1),CFDE                                                    
         MVC   P1+16(L'BDPWKS),BDPWKS                                           
         CLI   BDPWIND,C'O'                                                     
         BE    *+10                                                             
         MVC   P1+18(L'BDPWIND),BDPWIND                                         
         MVC   P1+22(L'BDPDAY),BDPDAY                                           
         MVC   P1+30(L'BDPNPWK),BDPNPWK                                         
         MVC   P2+4(L'BDPTIME),BDPTIME                                          
         MVC   P2+17(L'BDPDPT),BDPDPT                                           
         MVC   P2+21(L'BDPSLN),BDPSLN                                           
         MVC   P3+4(L'BDPPROG),BDPPROG                                          
         MVC   P3+22(L'BDPPTYP),BDPPTYP                                         
         OC    BDPADJ,BDPADJ                                                    
         BZ    *+10                                                             
         MVC   P3+22(L'BDPADJ),BDPADJ                                           
         CLI   MEDSPILL,C'Y'                                                    
         BNE   *+14                                                             
         MVC   P3+25(8),=C'*SPILL* '                                            
         B     PTSNOCST                                                         
         CLI   DETOCOST,NOSETQ                                                  
         BE    PTSNOCST                                                         
         MVC   P3+25(8),BDPCOST+4                                               
         CLI   BDPCOST+3,C' '                                                   
         BE    *+10                                                             
         MVC   P3+25(8),BDPCOST+3                                               
         CLI   BDPCOST+2,C' '                                                   
         BE    *+10                                                             
         MVC   P3+25(8),BDPCOST+2                                               
         L     RE,ADBUY                                                         
         USING BUYREC,RE                                                        
PTSNOCST DS    0C                                                               
         OC    BDNTAX,BDNTAX                                                    
         BZ    PTSNCST1                                                         
         MVI   P4+25,C'T'                                                       
         SR    RF,RF                                                            
         ICM   RF,3,BDNTAX                                                      
         EDIT  (RF),(5,P4+26),3,ALIGN=LEFT                                      
         DROP  RE                                                               
PTSNCST1 MVI   P4,0                                                             
         OC    SVSPREP,SVSPREP                                                  
         BZ    PTSDESC1                                                         
         MVC   P4(21),SREPCAP                                                   
         GOTO1 VRCPACK,DMCB,(C'U',SVSPREP),P4+15                                
PTSDESC1 DS    0H                                                               
         CLI   QPROG,C'U'                                                       
         BNE   *+10                                                             
         MVC   P2+3(1),BDPAST                                                   
         MVI   P6,0                                                             
         SPACE 2                                                                
* FIND DEMO ELEMENT AND EXTRACT BOOK AND NAME                                   
         L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING NDELEM,RE                                                        
PTSDSC2  CLI   NDCODE,2                                                         
         BE    PTSDSC2A                                                         
         ZIC   R0,NDLEN                                                         
         AR    RE,R0                                                            
         B     PTSDSC2                                                          
PTSDSC2A MVC   HLDPNAM,NDPROG                                                   
         CLI   NDBOOK,0                                                         
         BE    PTSDSC2B                                                         
         GOTO1 DATCON,DMCB,(X'03',NDBOOK),(X'09',HLDBOOK)                       
PTSDSC2B DS    0H                                                               
         MVC   SVP1,P1                                                          
         MVC   SVP2,P2                                                          
         MVC   SVP3,P3                                                          
         MVC   SVP4,P4                                                          
         DROP  RE                                                               
         BR    R9                                                               
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* BUY TIMESHEET PRINT DEMOS (I.E. DEMOS PORTION)                      *         
* PRINT DEMOS AND CPP FOR BRS,BTS,BDS,SAL                             *         
***********************************************************************         
BTSPD    LA    R8,P1               SET PRINT LINE                               
         CLI   MODE,STALAST                                                     
         BNE   *+8                                                              
         LA    R8,P2                                                            
         CLI   DETODEMO,1          DEMOS REQUESTED                              
         BNE   BTSPDX                                                           
*                                                                               
         USING PPTSLIND,R8                                                      
         L     R6,NODEMS                                                        
         LA    RE,PLD1                                                          
         LA    RF,OVRFLAGS                                                      
         LA    R1,DNAMES                                                        
         CLI   FRMTDEMO,2          DEMOS 2 UP                                   
         BNE   P1PD                 NO - TRY FOR ONE UP                         
PTSPD1   MVC   PPTSNAM2,0(R1)      MOVE DEMOS TO PRINT LINE                     
         MVC   PPTSDEM2,PLD1-PLD1+1(RE)                                         
         MVC   PPTSFLG2,0(RF)                                                   
         CLI   DETOCPM,1                 ADD CPM IF DESIRED                     
         BNE   *+10                                                             
         MVC   PPTSCPM2,PLD1CP-PLD1+1(RE)                                       
         BCT   R6,*+8                                                           
         B     BTSPDX                                                           
         MVC   PPTSNAM1,L'DNAME1(R1)                                            
         MVC   PPTSDEM1,PLD2-PLD1+1(RE)                                         
         MVC   PPTSFLG1,L'OVRFLAG(RF)                                           
         CLI   DETOCPM,1                 ADD CPM IF DESIRED                     
         BNE   *+10                                                             
         MVC   PPTSCPM1,PLD2CP-PLD1+1(RE)                                       
         LA    RE,2*PLDEMNLQ(RE)                                                
         LA    RF,2*L'OVRFLAG(RF)                                               
         LA    R1,2*L'DNAME1(R1)                                                
         LA    R8,L'P(R8)                                                       
         BCT   R6,PTSPD1                                                        
         SPACE 2                                                                
BTSPDX   BR    R9                                                               
         SPACE 2                                                                
* PRINT DEMOS 1 UP                                                              
P1PD     MVC   PPTSNAM1,0(R1)      MOVE DEMOS - ONE UP                          
         MVC   PPTSDEM1,PLD1-PLD1+1(RE)                                         
         MVC   PPTSFLG1,0(RF)                                                   
         CLI   DETOCPM,1           ADD CPM IF DESIRED                           
         BNE   *+10                                                             
         MVC   PPTSCPM1,PLD1CP-PLD1+1(RE)                                       
         LA    RE,PLDEMNLQ(RE)                                                  
         LA    RF,L'OVRFLAG(RF)                                                 
         LA    R1,L'DNAME1(R1)                                                  
         LA    R8,L'P(R8)                                                       
         BCT   R6,P1PD                                                          
*                                                                               
         MVC   113(12,R8),HLDPNAM                                               
         MVC   126(6,R8),HLDBOOK                                                
         LA    R8,260(R8)                                                       
         MVI   0(R8),0                                                          
         BR    R9                                                               
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* CALCULATE AND SAVE DEMOS AND CPP/CPM                                *         
***********************************************************************         
         SPACE                                                                  
CSDEMCP  NTR1                                                                   
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         XC    SVD1(112),SVD1                                                   
         MVC   SVLOCAL+8(4),MEDBYSPT SPOTS FROM NETWORK                         
         XC    0(200,R4),0(R4)     CLEAR NETWORK TOTALS                         
         MVC   MEDBYD(12),SVLOCAL  SET TOTALS FROM LOCAL STATION                
         MVC   MEDBY1(112),SVLOCAL+12                                           
         OC    MEDBYD(12),MEDBYD   CHECK ANY $/EQV$/SPOTS                       
         BZ    CSDEMCPX                                                         
         L     RE,STASPOT                                                       
         A     RE,MEDBYSPT                                                      
         ST    RE,STASPOT                                                       
         L     RE,STACOST                                                       
         C     RE,=F'-1'           PRESERVE $ OVERFLOW                          
         BE    CSDEM10                                                          
         CLC   MEDBYD,=F'-1'                                                    
         BE    *+12                                                             
         A     RE,MEDBYD           DETECT $ OVERFLOW                            
         BNO   *+8                                                              
         L     RE,=F'-1'           FFS TO INDICATE $ OVERFLOW                   
         ST    RE,STACOST                                                       
CSDEM10  L     RE,STACOSTE                                                      
         C     RE,=F'-1'           PRESERVE $ OVERFLOW                          
         BE    CSDEM20                                                          
         CLC   MEDBYDEQ,=F'-1'                                                  
         BE    *+12                                                             
         A     RE,MEDBYDEQ         DETECT $ OVERFLOW                            
         BNO   *+8                                                              
         L     RE,=F'-1'           FFS TO INDICATE $ OVERFLOW                   
         ST    RE,STACOSTE                                                      
CSDEM20  L     RE,UNATOT                                                        
         A     RE,STAUNA                                                        
         ST    RE,STAUNA                                                        
         XC    UNATOT,UNATOT                                                    
*                                                                               
         LA    R0,MAXDEMOQ                                                      
         LA    RF,STADEMS                                                       
         LA    R6,MEDBY1                                                        
CSSTA    L     RE,STADEMO-STADEMS(RF)                                           
         A     RE,MEDBY1-MEDBY1(R6)                                             
         ST    RE,STADEMO-STADEMS(RF)                                           
         L     RE,STADEMOE-STADEMS(RF)                                          
         A     RE,MEDBY1EQ-MEDBY1(R6)                                           
         ST    RE,STADEMOE-STADEMS(RF)                                          
         LA    RF,STADMNLQ(RF)                                                  
         LA    R6,L'MEDBY1+L'MEDBY1EQ(R6)                                       
         BCT   R0,CSSTA                                                         
*                                                                               
         XC    PRTLINE,PRTLINE                                                  
         LA    R3,PRTLINE                                                       
         MVC   SUMDL(8),MEDBYD                                                  
         MVC   SUMD1(112),MEDBY1                                                
         GOTO1 VCALCPP,DMCB,MEDBYSPT                                            
CSDEMCPX XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DO SUMMARIES FOR VARIOUS BREAKS                                     *         
***********************************************************************         
         SPACE                                                                  
DOSUM    NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 MEDADDWT,DMCB,(RA)                                               
* FIX TO ENSURE GET POL PRODUCT SUMMARY                                         
         CLI   MKTACT,1            ONLY FORCE IF MKT ACTIVE                     
         BNE   DOSUM1                                                           
         CLC   QSTA(4),=CL4'ALL'   ALL NETWORK...                               
         BNE   DOSUM1                                                           
         CLC   QPRD,=C'POL'        POL REQUEST...                               
         BNE   DOSUM1                                                           
         CLI   SUBPSW,0            WITHOUT SUMMARY...                           
         BNE   DOSUM1                                                           
         CLI   MODE,PRDLAST        PROD LAST WILL NOT PRINT AS                  
         BE    *+12                SPDUPTOT=Y, WHEN SUMMARY =N !!!              
DOSUM1   CLI   SPDUPTOT,C'Y'                                                    
         BE    DOSUM2                                                           
         MVC   WEIGHT,SPWEIGHT                                                  
         GOTO1 VSUMMRY                                                          
DOSUM2   SR    R9,R9                                                            
         IC    R9,LEVEL                                                         
         L     R8,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFCDE,(R8)),(X'80',(R9))                
         MVI   FORCEHED,C'Y'                                                    
         BAS   R9,GOTOSUB                                                       
         MVI   FORCEHED,C'Y'                                                    
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* HEADLINE ROUTINES                                                   *         
***********************************************************************         
         DS    0D                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=SP60RB                                                      
         DROP  RF                                                               
         L     R2,SP60R2                                                        
         LM    RA,RC,SP60RA                                                     
         CLI   MODE,PROCBUY                                                     
         BNE   MYHDNOW                                                          
         MVC   MID1,SVMID1                                                      
MYHDNOW  DS    0H                                                               
         CLI   MODE,MGR1LAST                                                    
         BNH   *+10                                                             
         MVC   H7+58(20),SUMCAP                                                 
         GOTO1 =A(PTSHEAD)                                                      
         CLI   FRMTDEMO,1                                                       
         BNE   MYHEADX                                                          
         MVC   WORK(18),H10+104                                                 
         MVC   H10+104(18),H10+103                                              
         MVC   H10+113(18),WORK                                                 
         MVC   H11+93(20),SPACES                                                
         MVC   H12+93(20),SPACES                                                
         B     MYHEADX                                                          
MYHEAD4  DS    0H                                                               
MYHEADX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* TRAP FOR MEDGETBY TO PUT NETWORK SPOTS IN DETAIL ITEMS              *         
***********************************************************************         
         PRINT GEN                                                              
         DS    0D                                                               
         USING *,RF                                                             
TRAPGTBY NTR1  BASE=SP60RB                                                      
         DROP  RF                                                               
         LM    RA,RC,SP60RA                                                     
         L     R2,SP60R2                                                        
         L     RF,SVGETBY          RESTORE MEDGETBY                             
         GOTO1 (RF),(R1)                                                        
         L     R7,MEDBUFF                                                       
         L     R5,MEDAFRST                                                      
         LA    R8,MEDPERD                                                       
TRAPGB2  CR    R5,R8               END                                          
         BH    TRAPGBX                                                          
         OC    4(4,R5),4(R5)       ACTIVE SLOT                                  
         BZ    TRAPGB4             NO TRY NEXT                                  
         L     R4,4(R5)                                                         
         OC    MEDBYD(12),MEDBYD   ANY SPOTS OR DOLLARS                         
         BZ    TRAPGB4             NO TRY NEXT                                  
         XC    MEDBYSPT,MEDBYSPT                                                
         OC    MEDBYD,MEDBYD       ANY DOLLARS                                  
         BNZ   TRAPGB4             YES RELEASE IT                               
         L     RF,MEDBYDEQ         SET EQUIV DOLLARS TO FORCE IT OUT            
         AHI   RF,1                                                             
         ST    RF,MEDBYDEQ                                                      
TRAPGB4  LA    R5,12(R5)           NEXT SLOT                                    
         B     TRAPGB2                                                          
TRAPGBX  XIT1                                                                   
         PRINT NOGEN                                                            
         SPACE                                                                  
* CONSTANTS AND LITERAL POOL FOR MAIN CSECT                                     
SP60RA   DC    F'0'                                                             
SP60RB   DC    F'0'                                                             
SP60RC   DC    F'0'                                                             
SP60R2   DC    F'0'                                                             
SUMCAP   DC    CL20'*** SUMMARY ***   '                                         
SREPCAP  DC    C'***SPECIAL REP=   ***'                                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GOSUBC CSECT !                                                      *         
***********************************************************************         
         SPACE                                                                  
GOSUBC   NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         MVC   N5START,QSTART                                                   
         MVC   QSTART(12),MSSTART                                               
         MVC   SVOPTS,QOPT1                                                     
         MVC   QOPT1(7),MSOPT                                                   
         MVC   SVPROF,PROGPROF                                                  
         MVC   SVSPPROF,SPOTPROF                                                
         MVC   SPOTPROF,MSSPPROF                                                
         MVC   PROGPROF,MSPROF                                                  
         MVC   SVSPECS,SPECS       GO TO SUBPROGRAM                             
         MVC   SVSUPMKT,SPSUPMKT                                                
         MVC   SVMDTAB,MEDTABLE                                                 
         MVC   SPECS,SVPH01        SET SPECS                                    
         MVC   MEDTABLE,SVPH04                                                  
         OC    MSBFHOOK,MSBFHOOK                                                
         BZ    GOSUB01                                                          
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         MVC   BUFFHOOK,MSBFHOOK                                                
         DROP  RF                                                               
*                                                                               
GOSUB01  CLI   MODE,MKTLAST                                                     
         BL    GOTOSUB1                                                         
         MVI   FORCEMID,C'N'                                                    
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVI   FORCEHED,C'Y'                                                    
         B     GOTOSUB1                                                         
         SPACE 2                                                                
GOTOSUB1 MVC   SVRCSUB,RCSUBPRG                                                 
         L     RF,MEDBUFF          RESTORE MEDIA SUMMARY DATES                  
         L     RE,=A(SVMDBLK)                                                   
         LHI   R1,SVMDBLKX-SVMDBLK                                              
         MOVE  ((RF),(R1)),(RE)                                                 
         BAS   R9,SUMKILLR         KILL RATINGS FOR MEDIA SUMMARIES             
         MVC   SVHDHOOK,HEADHOOK                                                
         MVC   HEADHOOK,MSHDHOOK                                                
         MVC   RCSUBPRG,MSRCSUB                                                 
         MVC   SPSUPMKT,MSSUPMKT                                                
         L     RF,SVPH02                                                        
         GOTO1 (RF),DMCB,(RA)      GO TO MEDIA SUMMARIES                        
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         MVC   MSBFHOOK,BUFFHOOK                                                
         XC    BUFFHOOK,BUFFHOOK                                                
         DROP  RF                                                               
         MVC   MSSTART,QSTART                                                   
         MVC   QSTART(12),N5START                                               
         MVC   MSOPT,QOPT1                                                      
         MVC   MSOPT+4(1),QOPT5+1                                               
         MVC   QOPT1(7),SVOPTS                                                  
         MVC   QOPT1(7),N5OPTS                                                  
         MVC   MSSUPMKT,SPSUPMKT                                                
         MVC   SPSUPMKT,SVSUPMKT                                                
         MVC   PROGPROF,SVPROF                                                  
         MVC   MSSPPROF,SPOTPROF                                                
         MVC   SPOTPROF,SVSPPROF                                                
         L     RE,MEDBUFF                                                       
         L     RF,=A(SVMDBLK)                                                   
         LHI   R1,SVMDBLKX-SVMDBLK                                              
         MOVE  ((RF),(R1)),(RE)                                                 
         L     RE,=A(SVPRDBUF)                                                  
         L     RF,PRDBUFF                                                       
         LHI   R1,SVPRDBFX-SVPRDBUF                                             
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   MSHDHOOK,HEADHOOK                                                
         MVC   MSRCSUB,RCSUBPRG                                                 
         MVC   RCSUBPRG,SVRCSUB                                                 
         MVC   HEADHOOK,SVHDHOOK                                                
         MVC   SPECS,SVSPECS                                                    
         MVC   MEDTABLE,SVMDTAB                                                 
         XIT1                                                                   
*=====================================================================          
* REMOVE RATINGS FROM PRDBUFF SO MED SUMMARY ONLY REPORTS AUDS                  
* (MEDIA SUMMARY REPORTS SHOULD ONLY REPORT DEMOS OF MAIN TIMESHEET)            
* CHOSE TO KILL HERE PRIOR TO EACH MED SUM CALL AS A 2ND PRDBUFF SAVE           
* AREA REQUIRED A FURTHER 28K OF STORAGE                                        
*=====================================================================          
                                                                                
SUMKILLR CLI   N5OPTS+(QOPT3-QOPT1),C'B' SEE IF EXTRACTED RTGS FOR LOCL         
         BE    *+8                                                              
         CLI   N5OPTS+(QOPT3-QOPT1),C'R'                                        
         BNER  R9                                                               
         LA    R0,220              KILL RATINGS FROM MED SUMMARY AREA           
         L     RE,PRDBUFF                                                       
SKILLR10 CLI   0(RE),0             PRODUCT NOT ACTIVE                           
         BE    SKILLR15            NEXT PRODUCT                                 
         SPACE 2                                                                
         LA    R6,PTDEMMAX         CHECK DEMOS FOR RATING                       
         LA    R1,PTDEMO-PTBUFFD(RE)     START OF DEMOS                         
         LR    RF,R1                                                            
SKILLR12 CLI   PTDEMTYP-PTDEMNO(RF),RTGDEMOQ                                    
         BE    SKILLR13                                                         
         CLI   PTDEMTYP-PTDEMNO(RF),EXTDEMOQ                                    
         BE    SKILLR13                                                         
         CR    R1,RF               ARE POINTERS THE SAME                        
         BE    *+16                YES - LEAVE SLOT ALONE                       
         MVC   0(L'PTDEMNO,R1),0(RF)     SLIDE DEMOS                            
         XC    0(L'PTDEMNO,RF),0(RF)     CLEAR OPENED SLOT                      
         LA    R1,L'PTDEMNO(R1)          BUMP TO NEXT DEMO                      
         LA    RF,L'PTDEMNO(RF)                                                 
         B     SKILLR14                                                         
*                                                                               
SKILLR13 XC    0(L'PTDEMNO,RF),0(RF)     KILL RATING                            
         LA    RF,L'PTDEMNO(RF)          ADVANCE POINTER                        
SKILLR14 BCT   R6,SKILLR12         DO NEXT DEMO                                 
SKILLR15 AH    RE,PRDBUFLN         DO NEXT PRODUCT                              
         BCT   R0,SKILLR10                                                      
         BR    R9                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE                                                                  
RQFIRST  NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         CLI   FIRST,1                                                          
         BNE   NOTFRST                                                          
         L     RF,=A(PNTABLE)                                                   
         ST    RF,VPNTABLE                                                      
         L     RF,=A(SSTABLE)                                                   
         ST    RF,VSSTABLE                                                      
         L     RF,=V(SPRPFOOT)                                                  
         ST    RF,VFOOT                                                         
         L     RF,=A(GETBUF)                                                    
         ST    RF,VGETBUF                                                       
         L     RF,=A(CALCPP)                                                    
         ST    RF,VCALCPP                                                       
         L     RF,=A(STATOTC)                                                   
         ST    RF,VSTATOT                                                       
         L     RF,=A(EDTDEMSC)                                                  
         ST    RF,VEDTDEMS                                                      
         L     RF,=A(SUBPAREA)                                                  
         ST    RF,VSUBPARA                                                      
         L     RE,=A(GETREP)                                                    
         ST    RE,VGETREP                                                       
         L     RE,=V(REVBUY)                                                    
         ST    RE,VREVBUY                                                       
         L     RE,=A(COMPRNT)                                                   
         ST    RE,VCOMPRNT                                                      
         L     RE,=A(MRGPL)                                                     
         ST    RE,VMRGPL                                                        
         L     RE,=V(REPCALOV)                                                  
         ST    RE,REPCALOV                                                      
*                                                                               
NOTFRST  XC    OPTRPT,OPTRPT                                                    
*                                                                               
M2KR     LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         TM    PROFMTR,X'0F'                                                    
         BNZ   *+10                                                             
         MVC   PROFMTR,HALF    ??? SET MARKET TOTAL FROM PROFILE                
         L     RF,=A(SALSUM)       SET MARKET TOTAL REPORT                      
         CLI   PROFMTR,C'3'                                                     
         BNE   *+8                                                              
         L     RF,=A(BRSSUM)                                                    
         CLI   PROFMTR,C'2'                                                     
         BNE   *+8                                                              
         L     RF,=A(BTSSUM)                                                    
         CLI   PROFMTR,C'4'                                                     
         BNE   *+8                                                              
         L     RF,=A(BDSSUM)                                                    
         ST    RF,VSUMMRY                                                       
         L     RF,=V(REPUDESC)                                                  
         ST    RF,VUDESC                                                        
         MVC   MRPTTYP,PROFMTR                                                  
         SPACE 2                                                                
         MVC   STACAP(7),=C'STATION'                                            
         MVI   QCOMPARE,C'A'                                                    
         CLI   QRERATE,C'I'                                                     
         BNE   *+8                                                              
         MVI   QCOMPARE,C'B'                                                    
RQ1      DS    0H                                                               
         MVC   MAXLINES,SVMAXLIN                                                
         GOTO1 VFOOT,DMCB,(RA)                                                  
         CLI   FOOT1,C' '                                                       
         BE    RQ1NOFT                                                          
         ZIC   R0,MAXLINES                                                      
         SH    R0,=H'3'                                                         
         STC   R0,MAXLINES                                                      
         MVC   FOOT1,SPACES                                                     
RQ1NOFT  DS    0H                                                               
         SPACE 2                                                                
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         XC    SPOTPROF,SPOTPROF                                                
         MVI   FORCEHED,C'Y'                                                    
         MVI   PASS,0                                                           
         MVC   RTYPE(2),QPROG                                                   
         MVI   RTYPE+2,C' '                                                     
         LA    R1,H11+35                                                        
         ST    R1,AHDATES                                                       
         MVI   SPACESW,1                                                        
         L     RF,APTSDESC                                                      
         ST    RF,DDESC                                                         
         LA    RF,P1+34                                                         
         ST    RF,DSTAGRID                                                      
         LA    RF,P2+31                                                         
         ST    RF,PSTASPT                                                       
         LA    RF,P2+101                                                        
         ST    RF,PSTACOST                                                      
         LA    RF,P2+(PSTOGRID-PSTOTLND)                                        
         ST    RF,APSTAGRD                                                      
         MVI   PENNYSW,1                                                        
         MVC   MEDNUMWK,=F'60'                                                  
         CLI   MOPT1,C' '                                                       
         BE    *+10                                                             
         MVC   PROFSORT,MOPT1                                                   
         CLI   MOPT2,C' '                                                       
         BE    *+10                                                             
         MVC   PROFFRMT,MOPT2                                                   
         CLI   MOPT3,C' '                                                       
         BE    *+10                                                             
         MVC   PROFCNDS,MOPT3                                                   
         MVC   MOPT4,QOPT5                                                      
* THIS LOOKED WRONG - QOPT5=DETAIL OPTION SO WHY SET DATE OPTION ?              
*        CLI   MOPT4,C' '                                                       
*        BE    *+10                                                             
*        MVC   PROFDCTL,MOPT4                                                   
         CLI   QOPT1,C' '                                                       
         BNE   *+10                                                             
         MVC   QOPT1,PROGPROF+6          CUTINS                                 
         CLI   QOPT2,C' '                                                       
         BNE   *+10                                                             
         MVC   QOPT2,PROGPROF+7          COST OVERRIDES                         
         CLI   QOPT3,C' '                                                       
         BNE   *+10                                                             
         MVC   QOPT3,PROGPROF+11         LOCAL DEMOS                            
         MVC   N5OPTS,QOPT1                                                     
         MVC   SUBPROG1,=C'N5'                                                  
         MVC   SUBPROG2,=C'05'                                                  
         L     R6,SPECS                                                         
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         MVC   SPECS,DMCB+4                                                     
         LA    RE,PROGPROF                                                      
*                                                                               
         CLI   QRERATE,C'Y'        N5 DOES NOT USE STD RERATING                 
         BE    *+12                                                             
         CLI   QRERATE,C'O'                                                     
         BNE   RQPR0                                                            
*                                  CATER FOR POST BUY OVERRIDE DEMOS            
         MVC   PBDREP,QRERATE      SAVE TYPE OF POST BUY DEMOS REPORT           
* FORCING GETDEME TO RERATE ON AN IMPOSSIBLE DATE RETURNS ANY BUY O/R           
* AND BY RERATING ON PURCHASED ENSURES GET THOSE DEMOS IF NO BUY O/R            
         MVI   QRERATE,C'P'        (RERATE PURCHASED)                           
         MVC   QBOOK1,=C'8002'     (IMPOSSIBLE DATE)                            
         MVI   PROFBMS,0           SUMMARIES NOT CATER FOR POST BUY O/R         
         MVI   PROFPMS,0                                                        
         SPACE 2                                                                
RQPR0    LA    RF,DATEOTAB         SET UP OPTIONS                               
RQPR1    CLI   0(RF),X'FF'         DATE OPTIONS                                 
         BE    RQPR1X                                                           
         CLC   0(1,RF),PROFDCTL                                                 
         BE    RQPR1X                                                           
         LA    RF,3(RF)                                                         
         B     RQPR1                                                            
RQPR1X   MVC   DATEOPT,1(RF)                                                    
         SPACE 2                                                                
         LA    RF,SORTOTAB                                                      
RQPR2    CLI   0(RF),X'FF'         SORT OPTIONS                                 
         BE    RQPR2X                                                           
         CLC   0(1,RF),PROFSORT                                                 
         BE    RQPR2X                                                           
         LA    RF,2(RF)                                                         
         B     RQPR2                                                            
RQPR2X   MVC   SORTOPT,1(RF)                                                    
         SPACE 2                                                                
         LA    RF,FRMTOTAB                                                      
RQPR3    CLI   0(RF),X'FF'         FORMAT OPTIONS                               
         BE    RQPR3X                                                           
         CLC   0(1,RF),PROFFRMT                                                 
         BE    RQPR3X                                                           
         LA    RF,5(RF)                                                         
         B     RQPR3                                                            
RQPR3X   MVC   FRMTOPT,1(RF)                                                    
         ZIC   RF,FRMTWIDE         GET LENGTH OF EACH GRID SLOT                 
         ZIC   RE,FRMT#PTS         GET NUMBER FOR PTS REPORT                    
RQPR3X2  MR    RE,RE               GET LENGTH OF TOTAL GRID                     
         BCTR  RF,0                                                             
         STH   RF,MGLENLIN                                                      
*                                                                               
         LA    RF,CNDSOTAB                                                      
         LA    RE,PROGPROF                                                      
RQPR4    CLI   0(RF),X'FF'                                                      
         BE    RQPR4X                                                           
         CLC   0(1,RF),PROFCNDS                                                 
         BE    RQPR4X                                                           
         LA    RF,2(RF)                                                         
         B     RQPR4                                                            
RQPR4X   MVC   CNDSOPT,1(RF)                                                    
         EJECT                                                                  
*        SET UP OPTON SWITCHS                                                   
         MVC   VARFRMT,DATEOPT                                                  
         MVC   SCNDDTSW,DATEOPT+1                                               
         MVC   SORTFRMT,SORTOPT                                                 
         MVI   SORTREQ,0                                                        
         CLI   SORTFRMT,0                                                       
         BE    *+8                                                              
         MVI   SORTREQ,1                                                        
         MVI   SORTPASS,SORTINPQ                                                
         MVC   LENGRID,FRMTWIDE                                                 
         XC    NOINGRID,NOINGRID                                                
         MVC   NOINGRID+1(1),FRMT#PTS                                           
         MVC   PGCNDSW,CNDSOPT                                                  
*                                                                               
M2RPTX   DS    0H                                                               
         MVC   SVSGRID,DSTAGRID                                                 
         MVC   DSTAGRID,=A(PLAREA)                                              
         MVI   HADOFLOW,C'N'                                                    
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST (PART 2)                                                        
***********************************************************************         
         SPACE                                                                  
RQFRSTA  NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         MVC   DETOPTS,=X'01010101'  SET DETAIL OPTIONS                         
         CLI   MOPT4,C' '                                                       
         BNE   *+10                                                             
         MVC   MOPT4,PROFDPC       DETAIL PRINT CONTROL                         
         TM    MOPT4,X'F0'                                                      
         BNO   M21                                                              
         PACK  DUB,MOPT4                                                        
         CVB   R6,DUB                                                           
         SLL   R6,2                                                             
         LA    R6,DETOTAB(R6)                                                   
         MVC   DETOPTS,0(R6)                                                    
*                                                                               
M21      XC    SUMOPTS,SUMOPTS                                                  
         CLI   PROFMTR,C'0'                                                     
         BE    *+10                                                             
         MVC   SUMOPTS,=X'010101'                                               
         CLI   MOPT5,C' '                                                       
         BNE   *+10                                                             
         MVC   MOPT5,PROFMPC       MARKET PRINT CONTROL                         
         TM    MOPT5,X'F0'                                                      
         BNO   M2AA                                                             
         PACK  DUB,MOPT5                                                        
         CVB   R6,DUB                                                           
         MH    R6,=H'3'                                                         
         LA    R6,SUMOTAB(R6)                                                   
         MVC   SUMOPTS,0(R6)                                                    
M2AA     CLI   PBDREP,C'N'                                                      
         BE    *+10                                                             
         XC    SUMOPTS,SUMOPTS     FORCE NO RECAPS FOR POST BUY O/R             
*                                                                               
         MVI   SUBPSW,0                                                         
         MVC   MSOPT,SPACES        INITIALIZE MS OPTIONS                        
         MVC   SVOPTS,MOPT1        SAVE REPORT OPTIONS                          
         MVC   SUBPROG1,=C'M2'                                                  
         CLC   QPRD,=C'POL'        POL REQUEST                                  
         BNE   M2AA1                NO                                          
         TM    PROFPMS,X'0F'        YES - ANY MEDIA SUMMARY                     
         BZ    M2AA3                                                            
         MVC   SUBPROG1+1(1),PROFPMS                                            
         B     M2AA2                                                            
M2AA1    TM    PROFBMS,X'0F'                                                    
         BZ    M2AA3                                                            
         MVC   SUBPROG1+1(1),PROFBMS                                            
M2AA2    MVI   SUBPSW,1                                                         
M2AA3    MVC   WORK(12),=CL12'S000'   READ MS PROFILE                           
         MVC   WORK+4(3),SVAGY                                                  
         MVC   WORK+7(3),QCLT                                                   
         CLI   SUBPROG1,C'C'                                                    
         BNE   *+8                                                              
         MVI   SUBPROG1,C'M'                                                    
         MVC   WORK+2(2),SUBPROG1                                               
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  R6                                                               
         GOTO1 GETPROF,DMCB,WORK,MSPROF,DATAMGR                                 
         CLI   QRERATE,C'I'        FIX DEFAULT DATA COMPARE                     
         BNE   AFFCOMP                                                          
         CLI   MSPROF,C'B'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'A'                                                      
         CLI   MSPROF,C'D'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'C'                                                      
         CLI   MSPROF,C'F'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'E'                                                      
         B     COMPOK                                                           
         SPACE 2                                                                
AFFCOMP  CLI   MSPROF,C'A'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'B'                                                      
         CLI   MSPROF,C'C'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'D'                                                      
         CLI   MSPROF,C'E'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'F'                                                      
COMPOK   DS    0H                                                               
         XC    MSBFHOOK,MSBFHOOK                                                
         SPACE 2                                                                
         MVC   SUBPROG2,=C'01'                                                  
         L     R6,VSUBPARA                                                      
         ST    R6,CURPGPTR         SAT TO START OF SAVE AREA                    
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         L     RE,DMCB+4                                                        
         ST    RE,SVPH01                                                        
         MVC   CURPH01(4),CURPGPTR                                              
         MVC   CURPH01+4(8),DMCB                                                
         L     R6,CURPGPTR                                                      
         A     R6,DMCB                                                          
         ST    R6,CURPGPTR                                                      
         MVC   SUBPROG2,=C'02'                                                  
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         L     RE,DMCB+4                                                        
         ST    RE,SVPH02                                                        
         MVC   CURPH02(4),CURPGPTR                                              
         MVC   CURPH02+4(8),DMCB                                                
         L     R6,CURPGPTR                                                      
         A     R6,DMCB                                                          
         ST    R6,CURPGPTR                                                      
         MVC   SUBPROG2,=C'04'                                                  
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         MVC   CURPH04(4),CURPGPTR                                              
         MVC   CURPH04+4(8),DMCB                                                
         L     R6,CURPGPTR                                                      
         A     R6,DMCB                                                          
         ST    R6,CURPGPTR                                                      
         LA    R9,3                                                             
         LA    R6,CURPH01                                                       
SAVPRGM  L     RF,0(R6)                                                         
         L     RE,8(R6)                                                         
         L     R1,4(R6)                                                         
         MOVE  ((RF),(R1)),(RE)                                                 
         LA    R6,12(R6)                                                        
         BCT   R9,SAVPRGM                                                       
         L     RF,=A(SVMDBLK)                                                   
         L     RE,MEDBUFF                                                       
         LHI   R1,SVMDBLKX-SVMDBLK                                              
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   SVPH04,DMCB+4                                                    
         XC    SVP1,SVP1                                                        
         XC    SVP2,SVP2                                                        
         XC    SVP3,SVP3                                                        
         XC    SVP4,SVP4                                                        
         XC    SVMID1,SVMID1                                                    
         XC    SPBUFSTA,SPBUFSTA                                                
         XC    SPBUFMKT,SPBUFMKT                                                
         XC    STAGRID(136),STAGRID                                             
         XIT1                                                                   
         DROP  RE                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ESTIMATE FIRST                                                      *         
***********************************************************************         
         SPACE                                                                  
EFRSTC   NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
*****FETCH MARKET COMMENTS                                                      
         MVI   BCMTYPE,C'M'                                                     
         MVC   BCMCLT,BCLT                                                      
         MVC   BCMPGR,BPGR                                                      
         MVC   BCMPRD,BPRD                                                      
         MVC   BCMEST,BEST                                                      
         MVC   BCMMKT,BMKT                                                      
         GOTO1 GETCOM                                                           
         MVI   FORCECMT,C'Y'                                                    
         MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
         MVC   REASTART(3),QPRD    FORCE TO READ ALL PRODUCTS                   
         MVC   QPRD,=C'POL'                                                     
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
         MVC   QPRD,REASTART                                                    
EFPOLOK  DS    0C                                                               
         SPACE 2                                                                
         LA    R0,220              KILL RATINGS FROM EST HEADER                 
         L     RE,PRDBUFF                                                       
KILLR1   CLI   0(RE),0             PRODUCT NOT ACTIVE                           
         BE    KILLR5              NEXT PRODUCT                                 
         SPACE 2                                                                
* ROUTINE NOW EXTRACTS 1ST 4 AUD & RATING DEMOS                                 
         LA    R9,PTDEMMAX         CHECK DEMOS FOR RATING                       
         LA    R6,PTDEMO-PTBUFFD(RE)     START OF DEMOS                         
         SR    R1,R1               AUD COUNT                                    
         SR    RF,RF               RATING COUNT                                 
         XC    WORK,WORK                                                        
KILLR2   OC    PTDEMTYP-PTDEMNO(L'PTDEMNO,R6),PTDEMTYP-PTDEMNO(R6)              
         BZ    KILLR4                                                           
         CLI   PTDEMTYP-PTDEMNO(R6),RTGDEMOQ  RATING                            
         BE    KILLR3                                                           
         CLI   PTDEMTYP-PTDEMNO(R6),EXTDEMOQ  EXTENDED RATING                   
         BE    KILLR3                                                           
         CHI   R1,MAXDAUDQ                                                      
         BNL   KILLR4                                                           
         AHI   R1,1                                                             
         LR    R8,R1                                                            
         BCTR  R8,0                                                             
         MHI   R8,L'PTDEMNO                                                     
         LA    R8,WORK(R8)                                                      
         MVC   0(L'PTDEMNO,R8),0(R6)  COPY RATING                               
         B     KILLR4                                                           
*                                                                               
KILLR3   CHI   RF,MAXDRTGQ                                                      
         BNL   KILLR4                                                           
         AHI   RF,1                                                             
         LR    R8,RF                                                            
         BCTR  R8,0                                                             
         MHI   R8,L'PTDEMNO                                                     
         LA    R8,WORK+12(R8)                                                   
         MVC   0(L'PTDEMNO,R8),0(R6)  COPY AUDIENCE                             
KILLR4   LA    R6,L'PTDEMNO(R6)          BUMP TO NEXT DEMO                      
         BCT   R9,KILLR2           DO NEXT DEMO                                 
*                                                                               
         LA    R6,PTDEMO-PTBUFFD(RE)     START OF DEMOS                         
         XC    0(L'PTDEMO,R6),0(R6)                                             
         LTR   R1,R1               SET EXTRACTED AUDS                           
         BZ    KILLR4A                                                          
         MHI   R1,L'PTDEMNO                                                     
         LR    R8,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),WORK                                                     
         AR    R6,R8                                                            
KILLR4A  CLI   QOPT3,C'B'          SET EXTRACTED RTGS IF NEED FOR LOCAL         
         BE    *+12                                                             
         CLI   QOPT3,C'R'                                                       
         BNE   KILLR5                                                           
         LTR   RF,RF                                                            
         BZ    KILLR5                                                           
* THUS WM1535,EWM1535,MN1535,EMN1535 -> WM1535,MN1535,EWM1535,EMN1535           
         MHI   RF,L'PTDEMNO                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),WORK+12                                                  
KILLR5   AH    RE,PRDBUFLN         DO NEXT PRODUCT                              
         BCT   R0,KILLR1                                                        
*                                                                               
         L     R0,PRDBUFF                                                       
         LHI   R1,SVPRDBFX-SVPRDBUF                                             
         L     RE,=A(SVPRDBUF)                                                  
         LR    RF,R1                                                            
         MVCL  RE,R0               SAVE A COPY OF PRDBUFF                       
*                                                                               
         MVC   REASTART,QSTART     SET UP PASS DATES                            
         MVC   PASSQST(12),QSTART                                               
         MVC   PASSTAB(12),QSTART                                               
         SPACE 2                                                                
         MVC   HKPRDRD,MEDPRDRD    SAVE OLD MEDPRDRD ADDR.                      
         LA    RE,TRAPPRD          SET TO NOP                                   
         ST    RE,MEDPRDRD                                                      
         BRAS  RE,GOSUBC           DO MEDIA SUMMARIES                           
         MVC   MEDPRDRD,HKPRDRD    RESTORE MEDPRDRD                             
* SET NUMBER OF LEVELS                                                          
         LA    RF,LVCNTRL                                                       
         LA    RE,5                                                             
         NI    0(RF),X'7F'                                                      
         LA    RF,1(RF)                                                         
         BCT   RE,*-8                                                           
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         L     RE,BUFFROWS                                                      
         BCTR  RE,0                                                             
         MH    RE,=H'4'                                                         
         LA    RE,LVCNTRL(RE)                                                   
         OI    0(RE),X'80'                                                      
         DROP  RF                                                               
         SPACE 2                                                                
* CREATE WEEKLY TABLES FOR ALL REPORTS                                          
         MVC   MEDEXTAX,SPOTPROF+12                                             
         MVC   MEDNUMWK,=F'60'     CREATE BTS TABLES                            
         MVC   MEDNUMMO,=F'12'                                                  
         MVC   MEDNUMQT,=F'4'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
EFRSTA   DS    0H                                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   SVRDTE,MEDPERD      SAVE REQUEST DATES                           
         MVI   PASS,0                                                           
         MVI   MAXPASS,1                                                        
         CLI   VARFRMT,0           FIXED FORMAT                                 
         BNE   EFRSTX               NO - EXIT                                   
*                                                                               
         LA    RE,1                                                             
         LA    R6,PASSTAB                                                       
         L     R9,MEDAFRST                                                      
SETPASS  STC   RE,MAXPASS          SAVE HIGHEST PASS                            
         LH    R8,NOINGRID         SET TO NUMBER OF WEEKS IN PASS               
         GOTO1 DATCON,DMCB,(X'02',(R9)),(X'00',0(R6))                           
SETPASS1 GOTO1 DATCON,DMCB,(X'02',2(R9)),(X'00',6(R6))                          
SETPASS2 LA    R9,12(R9)                                                        
         C     R9,MEDALAST                                                      
         BH    SETPASSX                                                         
         CLI   0(R9),0                                                          
         BE    SETPASS2                                                         
         BCT   R8,SETPASS1                                                      
*                                                                               
         ZIC   RE,MAXPASS                                                       
         LA    RE,1(RE)            BUMP MAXPASS                                 
         LA    R6,12(R6)           BUMP DATE SAVE                               
         B     SETPASS                                                          
*                                                                               
SETPASSX MVC   PASSQST(12),PASSTAB                                              
EFRSTX   MVC   QSTART(12),PASSQST                                               
* INITIALIZE N5 PROFILES                                                        
         SPACE 2                                                                
         MVI   IDSW,0              RESET MULTIPLE IDS ON PAGE                   
         MVI   IDTOTALS,C'Y'       DEFAULT IS ID TOTALS                         
         CLI   QBYID,C'Y'          ID SEQUENCE REQUESTED                        
         BNE   M4A                                                              
         CLI   PROGPROF+14,C'N'    MULTIPLE IDS ON PAGE OPT.                    
         BNE   *+12                                                             
         MVI   IDSW,1              TURN ON MULTIPLE IDS ON PAGE                 
         MVI   IDTOTALS,C'N'       SUPPRESS ID TOTALS                           
         SPACE 2                                                                
M4A      EQU   *                   NO N5 SPILL PROFILE!                         
         MVI   SPOTPROF+5,0        & DON'T HONOR ANY SPOTPROF SETTING           
*4A      CLI   MOPT5+1,C' '        SPILL REPORTING OVERRIDE                     
*        BE    *+14                                                             
*        MVC   PROGPROF+15(1),MOPT5+1                                           
*        NI    PROGPROF+15,X'0F'                                                
*        CLI   PROGPROF+15,C'0'    SPILL OVERRIDE                               
*        BE    *+10                                                             
*        MVC   SPOTPROF+5(1),PROGPROF+15                                        
         CLI   SPOTPROF+1,0                                                     
         BNE   *+8                                                              
         MVI   SPOTPROF+1,C'N'                                                  
         CLI   SPOTPROF+5,20                                                    
         BL    *+8                                                              
         MVI   SPOTPROF+5,0                                                     
         SR    RE,RE                                                            
         IC    RE,BPRD                                                          
         CLI   BPRD,X'FF'          GET DEMO NAMES FOR PRODUCT                   
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RE,PTDEMO-PTBUFFD(RE,RF)                                         
         XC    DNAMES,DNAMES                                                    
         LA    R9,14                                                            
         CLI   BPRD,X'FF'                                                       
         BE    *+8                                                              
         LA    R9,MAXDEMOQ                                                      
         SPACE 2                                                                
* LOOK UP NEW FORMAT DEMO NAMES                                                 
NEWDNAM  ST    RE,FULL                                                          
         L     R6,ADBLOCK                                                       
         USING DBLOCK,R6                                                        
         XC    0(255,R6),0(R6)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
*        CANADA USES A DIFFERENT DEMO LIST                                      
         L     RF,ADCLT            SET UP FOR PARAMETER LIST                    
         CLI   CEXTRA-CLTHDR(RF),C'U'         TEST US DEMOS                     
         BE    NEWDNAM0                                                         
         L     RF,ADAGY                                                         
         LA    RF,AGYPROF+7-AGYHDR(RF)                                          
         CLI   0(RF),C'C'          TEST CANADIAN AGY                            
         BNE   NEWDNAM0                                                         
         MVI   DBSELMED,C'C'       SET CANADIAN MEDIA                           
         DROP  R6                                                               
NEWDNAM0 L     R8,ADEST                                                         
         USING ESTHDR,R8                                                        
         LA    R8,EUSRNMS                                                       
         DROP  R8                                                               
         L     R6,FULL                                                          
         GOTO1 DEMOCON,DMCB,((R9),(R6)),(2,DNAMES),(C'S',ADBLOCK),     X        
               (SPOTPROF+9,(R8))                                                
         L     RE,FULL                                                          
         SR    R1,R1               AUD DEMO COUNT                               
         SR    RF,RF               RTG DEMO COUNT                               
NEWDNAM1 CLI   PTDEMTYP-PTDEMNO(RE),0                                           
         BE    NEWDNAM2                                                         
         AHI   R1,1                                                             
         CLI   PTDEMTYP-PTDEMNO(RE),RTGDEMOQ                                    
         BE    *+12                                                             
         CLI   PTDEMTYP-PTDEMNO(RE),EXTDEMOQ                                    
         BNE   *+8                                                              
         AHI   RF,1                                                             
         LA    RE,L'PTDEMNO(RE)                                                 
         B     NEWDNAM1                                                         
NEWDNAM2 SR    R1,RF                                                            
         LTR   R1,R1                                                            
         BP    *+8                                                              
         LA    R1,1                                                             
         ST    R1,NODEMS                                                        
         ST    RF,NORTGS                                                        
         XIT1                                                                   
         SPACE 2                                                                
TRAPPRD  BR    RE                  NOP MEDPRDRD FOR MEDIA SUMMARIES             
         LTORG                                                                  
*          DATA SET SPREPD202  AT LEVEL 086 AS OF 08/14/09                      
         EJECT                                                                  
GETCAP   NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING PKGELEM,R5                                                       
         XC    PKGAREA,PKGAREA                                                  
*                                                                               
GETCAP1  CLI   PKGCODE,5                                                        
         BE    GETCAP2                                                          
         CLI   PKGCODE,0                                                        
         BE    GETCAP4                                                          
         SR    RE,RE                                                            
         IC    RE,PKGLEN                                                        
         AR    R5,RE                                                            
         B     GETCAP1                                                          
*                                                                               
GETCAP2  DS    0H                                                               
         MVC   BYTE,PKGIND                                                      
         NI    BYTE,X'0F'          DROP2-BYTE LINE FLAG                         
         CLI   BYTE,1                                                           
         BNE   *+14                                                             
         MVC   PKGAREA(7),=C'PKG MST'                                           
         B     GETCAPX                                                          
*                                                                               
         CLI   BYTE,3                                                           
         BNE   *+14                                                             
         MVC   PKGAREA(7),=C'ORB MST'                                           
         B     GETCAPX                                                          
*                                                                               
         CLI   BYTE,5                                                           
         BNE   *+14                                                             
         MVC   PKGAREA(7),=C'REV MST'                                           
         B     GETCAPX                                                          
*                                                                               
         MVC   PKGAREA(4),=C'MST='                                              
         LLC   R0,PKGLINES                                                      
         TM    PKGIND,X'10'        TEST 2-BYTE LINE NUMBERS                     
         BZ    *+8                                                              
         ICM   R0,3,PKGLINES                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PKGAREA+4(3),DUB                                                 
         B     GETCAPX                                                          
*                                                                               
GETCAP4  L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING BDELEM,RE                                                        
         CLI   BDMGDATE,X'C0'                                                   
         BNH   GETCAPX                                                          
         MVC   PKGAREA(7),=C'**MKGD '                                           
         MVC   PKGAREA+7(6),=C'GROUP '                                          
         MVC   PKGAREA+13(2),BDMGDATE                                           
         MVI   PKGAREA+15,C'*'                                                  
*                                                                               
GETCAPX  DS    0H                                                               
         DROP  R5,RE                                                            
         EJECT                                                                  
N5GETCM  L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING COMELEM,R5                                                       
         LA    RE,COMAREA                                                       
         LHI   RF,COMAREAX-COMAREA                                              
         XCEF                                                                   
         MVI   NUMCOM,0                                                         
N5GETCM2 CLI   CMCODE,0                                                         
         BE    N5GETCMX                                                         
         CLI   CMCODE,X'66'                                                     
         BE    N5GETCM4                                                         
N5GETCM3 SR    R0,R0                                                            
         IC    R0,CMLEN                                                         
         AR    R5,R0                                                            
         B     N5GETCM2                                                         
*                                                                               
N5GETCM4 LA    R7,PROGPROF                                                      
         USING PROFDSCT,R7                                                      
         CLI   PROFCC,C'1'                                                      
         BNE   N5GETCM5                                                         
         CLI   CMDATA,C'$'                                                      
         BE    *+8                                                              
         CLI   CMNUM,4                                                          
         BE    *+8                                                              
         CLI   CMNUM,5                                                          
         BNE   N5GETCM3                                                         
N5GETCM5 CLI   PROFCC,C'2'                                                      
         BNE   *+14                                                             
         CLC   CMDATA(8),=C'COMMENT-'                                           
         BNE   N5GETCM3                                                         
         DROP  R7                                                               
         LA    R4,COMAREA                                                       
         CLI   CMNUM,5             GET COMMENT SLOT                             
         BH    N5GETCM3                                                         
         SR    R7,R7                                                            
         IC    R7,CMNUM                                                         
         BCTR  R7,0                                                             
         MH    R7,=H'80'                                                        
         AR    R4,R7                                                            
         SR    R7,R7                                                            
         IC    R7,CMLEN                                                         
         SH    R7,=H'4'                                                         
         LTR   R7,R7                                                            
         BM    N5GETCM3                                                         
         ZIC   RE,NUMCOM           BUMP COMMENT COUNTER                         
         LA    RE,1(RE)                                                         
         STC   RE,NUMCOM                                                        
         EX    R7,*+8                                                           
         B     N5GETCM3                                                         
         MVC   0(0,R4),CMDATA                                                   
N5GETCMX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
GETSADDR NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         XC    SPBUFSTA,SPBUFSTA                                                
         MVI   SPLPRINT,1          SET TO PRINT ORIG.                           
         MVI   FBSTA,C'Y'                                                       
         MVI   INVFILM,0                                                        
         XC    STAUNA,STAUNA                                                    
         XC    UNATOT,UNATOT                                                    
         MVC   SAKYSAVE,KEY                                                     
         L     R6,ADREP                                                         
         USING REPREC,R6                                                        
         XC    REPKREP,REPKREP                                                  
         MVC   RNAME,SPACES                                                     
         MVC   REPNM,SPACES                                                     
         DROP  R6                                                               
         L     R6,ADSTAT                                                        
         USING STAREC,R6                                                        
         MVC   WORK,STRFREP                                                     
         XC    REPKREP,REPKREP                                                  
         XC    RNAME,RNAME                                                      
         CLC   STRFREP,=C'000'                                                  
         BNE   M82                                                              
         DROP  R6                                                               
         LA    R6,KEY                                                           
         USING ADDRREC,R6                                                       
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   ADDKTYPE,C'A'                                                    
         MVC   ADDKMED,QMED                                                     
         MVC   ADDKCALL,STA                                                     
         CLI   ADDKCALL+4,C' '                                                  
         BNE   *+10                                                             
         MVC   ADDKCALL+4(1),QMED                                               
         CLI   QMED,C'C'                                                        
         BNE   *+8                                                              
         MVI   ADDKCALL+4,C'C'                                                  
         MVC   ADDKAGY,AGY                                                      
         GOTO1 HIGHSTAD                                                         
         L     R6,ADSTATAD                                                      
         CLC   ADDKCALL(4),STA                                                  
         BE    M84                                                              
         MVC   ANAME(86),SPACES                                                 
         MVC   ANAME(15),=C'ADDRESS MISSING'                                    
         B     M84                                                              
M82      GOTO1 VGETREP             GET REP ADDRESS                              
M84      DS    0H                                                               
         CLC   PRVSTA,STAPRINT     SAME STATION                                 
         BNE   *+12                                                             
M84MID   CLI   IDSW,1              MULTIPLE IDS ON PAGE                         
         BE    M84MIDX                                                          
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         MVI   LINE,0                                                           
M84MIDX  DS    0H'0'                                                            
         MVC   PRVSTA,STAPRINT                                                  
         SPACE 2                                                                
         L     R6,ADSTATAD                                                      
         LA    RF,A3LINE                                                        
         SH    RF,=H'4'                                                         
M84CAN   CLI   0(RF),0                                                          
         BE    *+8                                                              
         CLI   0(RF),C' '                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         B     M84CAN                                                           
         LA    RF,1(RF)                                                         
         MVI   0(RF),C','                                                       
         MVC   1(2,RF),A3LINE                                                   
         MVC   A3LINE(L'ABIGZIP),ABIGZIP                                        
         DROP  R6                                                               
M84CREP  L     R6,ADSTAT                                                        
         USING STAREC,R6                                                        
         MVC   WORK,SCONREP                                                     
         CLC   SCONREP,=C'000'                                                  
         BE    M8EXIT                                                           
         GOTO1 VGETREP                                                          
M8EXIT   MVC   KEY,SAKYSAVE                                                     
         XIT1                                                                   
         DROP  R5                                                               
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ???                                                                           
***********************************************************************         
         SPACE                                                                  
COMPRNT  NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         LA    R1,PROGPROF                                                      
         USING PROFDSCT,R1                                                      
         CLI   PROFID,C'Y'                                                      
         BNE   GETIDX                                                           
         DROP  R1                                                               
         L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
GETID    CLI   0(RE),0                                                          
         BE    GETIDX                                                           
         CLI   0(RE),X'70'                                                      
         BE    GETID1                                                           
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     GETID                                                            
GETID1   L     R6,FULL                                                          
         CLI   0(R6),0                                                          
         BNE   *+8                                                              
         MVI   0(R6),C' '                                                       
         CLC   1(80,R6),0(R6)                                                   
         BE    GETID2                                                           
         LA    R6,132(R6)                                                       
         B     *-14                                                             
GETID2   MVC   11(12,R6),3(RE)                                                  
         MVI   10(R6),C'='                                                      
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         MVC   0(10,R6),CTITLE                                                  
         DROP RE                                                                
GETIDX   DS    0C                                                               
         LA    R4,COMAREA                                                       
         LA    R5,5                                                             
         L     R6,FULL                                                          
         CLI   0(R6),0                                                          
         BNE   *+8                                                              
         MVI   0(R6),C' '                                                       
         CLC   1(76,R6),0(R6)                                                   
         BE    COMPRNT1                                                         
         LA    R6,132(R6)                                                       
         B     *-14                                                             
COMPRNT1 OC    0(76,R4),0(R4)                                                   
         BZ    *+14                                                             
         MVC   0(76,R6),0(R4)                                                   
         LA    R6,132(R6)                                                       
         LA    R4,80(R4)                                                        
         BCT   R5,COMPRNT1                                                      
         CLI   COMAREA,0                                                        
         BE    *+8                                                              
         MVI   0(R6),0                                                          
         L     RE,ADBUY            LOOK FOR ORBIT ELEMENT                       
         LA    RE,24(RE)                                                        
         USING ORBELEM,RE                                                       
GETORB   CLI   0(RE),0             END                                          
         BE    GETORBX              YES -EXIT                                   
         CLI   0(RE),X'67'                                                      
         BE    GETORB1                                                          
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     GETORB                                                           
GETORB1  LA    R5,ORBDAY                                                        
         USING ORBDAY,R5                                                        
GETORB1A CLC   1(130,R6),0(R6)                                                  
         BE    *+12                                                             
         LA    R6,132(R6)                                                       
         B     GETORB1A                                                         
         ZIC   R7,1(RE)            GET END OF ELEMENT                           
         AR    R7,RE                                                            
         MVC   0(8,R6),=C'*ORBIT* '                                             
         LA    R6,9(R6)                                                         
GETORB2  LR    R4,R6                                                            
         LA    R3,4                                                             
         LA    R3,3                                                             
GETORB3  DS    0C                                                               
         LR    R9,R6                                                            
         CR    R5,R7                                                            
         BNL   GETORBX                                                          
         GOTO1 CODAY,DMCB,ORBDAY,(R6)                                           
         LA    R6,9(R6)                                                         
         GOTO1 UNTIME,DMCB,ORBTIME,(R6)                                         
         LA    R6,12(R6)                                                        
         MVC   0(7,R6),ORBDESC                                                  
         LA    R6,8(R6)                                                         
         CLI   DETODEMO,YESSETQ                                                 
         BNE   GETORB4                                                          
         SR    R0,R0                                                            
         ICM   R0,3,ORBDEM                                                      
         N     R0,=X'00003FFF'     DROP FLAGS                                   
         TM    ORBDEM,X'40'        TEST 2-DEC VALUE                             
         BO    GETORB3A                                                         
         EDIT  (R0),(6,(R6)),1                                                  
         B     GETORB4                                                          
*                                                                               
GETORB3A EDIT  (R0),(6,(R6)),2                                                  
*                                                                               
GETORB4  LA    R6,7(R6)                                                         
         GOTO1 SQUASHER,DMCB,(R9),36                                            
         LA    R5,16(R5)                                                        
         BCT   R3,GETORB3                                                       
         LR    R6,R4                                                            
         LA    R6,132(R6)                                                       
         MVI   0(R6),0                                                          
         B     GETORB2                                                          
GETORBX  DS    0C                                                               
         EJECT                                                                  
* SET UP UPGRADE EXPRESSION                                                     
         CLI   QBOOK1,C' '                                                      
         BNE   COMPEX                                                           
         CLI   PBDREP,C'N'                                                      
         BNE   COMPEX                                                           
         GOTO1 VUDESC,DMCB,(RA),(R6)                                            
COMPEX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET GOALS                                                           *         
***********************************************************************         
         SPACE                                                                  
GETGL    NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         MVC   MEDNUMWK,=F'60'                                                  
         MVC   MEDNUMMO,=F'12'                                                  
         MVC   MEDNUMQT,=F'0'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         GOTO1 MEDDATE,DMCB,(RA)                                                
         LA    RE,KEY                                                           
         L     RE,ADGOAL                                                        
         USING GOALREC,RE                                                       
         MVC   MEDBRAND,BPRD                                                    
         MVC   MEDSPTLN,GKEYSLN                                                 
         DROP  RE                                                               
         GOTO1 MEDGETGL,DMCB,(RA)                                               
         L     R9,WEIGHT                                                        
         GOTO1 MEDMKTWT,DMCB,(RA),(R9)                                          
         LA    R3,PRTLINE                                                       
         L     R5,MEDAFRST                                                      
M163     MVI   SUMCODE,X'90'                                                    
         MVC   SUMDPGNO(8),MEDDPGNO     SET DAYPART                             
         MVC   SUMSLN,MEDSPTLN                                                  
         L     R4,4(R5)                                                         
         LTR   R4,R4                                                            
         BZ    M165                                                             
         MVI   SUMRTYP,1                                                        
         LA    RF,MEDPERD                                                       
         CR    R5,RF                                                            
         BH    M16X                END                                          
         BNE   M163A                                                            
         MVI   SUMRTYP,3                                                        
         MVC   SUMDT,=X'FFFFFFFF'                                               
         B     M164                                                             
M163A    OC    0(4,R5),0(R5)                                                    
         BZ    M165                                                             
         LA    RF,MEDMON01                                                      
         CR    R5,RF               MONTHLY                                      
         BL    *+8                                                              
         MVI   SUMRTYP,2            YES-SET RECORD CODE                         
         MVC   SUMDT,0(R5)                                                      
M164     LA    RE,SUMKEY           SET UP DATA ITEM DISPLACEMENTS               
         USING SUMDATA,RE                                                       
         OC    MEDGLD(16),MEDGLD                                                
         BZ    M165                                                             
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         A     RE,BUFFLKEY                                                      
         DROP  RF                                                               
         XC    SUMDATA(SUMDATLQ),SUMDATA                                        
         MVC   SUMGDL,MEDGLD                                                    
         MVC   SUMGDLE,MEDGLDEQ                                                 
         MVC   SUMGD1,MEDGL1                                                    
         MVC   SUMGD1E,MEDGL1EQ                                                 
         LA    R8,PROGPROF                                                      
         USING PROFDSCT,R8                                                      
         CLI   PROFDPT,C'Y'        DAYPART ANALYSIS REQUIRED                    
         BNE   M164T                NO - PUT OUT TOTALS ONLY                    
         DROP  R8                                                               
         L     R8,BUFFBUFF                                                      
         BAS   R9,M16PUT                                                        
* PUT OUT PRODUCT GROUP DETAILS                                                 
         MVI   SUMCODE,X'91'                                                    
         BAS   R9,M16PUT                                                        
M164T    L     R8,BUFFBUFF                                                      
         MVC   SUMDPGNO(9),=9X'FF'                                              
         MVI   SUMCODE,X'90'                                                    
         BAS   R9,M16PUT                                                        
         MVI   SUMCODE,X'91'                                                    
         BAS   R9,M16PUT                                                        
* GET NEXT BUFFALO ITEM                                                         
M165     LA    R5,12(R5)                                                        
         B     M163                                                             
M16X     B     GOALX                                                            
         DROP  RE                                                               
M16PUT   CLI   SUMCODE,X'91'                                                    
         BER   R9                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',(R8),PRTLINE                                
         BR    R9                                                               
GOALX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET REP ADDRESS                                                     *         
***********************************************************************         
         SPACE                                                                  
GETREP   NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         MVC   RPKYSAVE,KEY                                                     
         LA    R6,KEY                                                           
         USING REPREC,R6                                                        
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   REPKTYPE,C'R'                                                    
         MVC   REPKMED,QMED                                                     
         MVC   REPKREP,WORK                                                     
         MVC   REPKAGY,AGY                                                      
         GOTO1 READREP                                                          
         L     R6,ADREP                                                         
         L     RE,ADSTAT                                                        
         USING STAREC,RE                                                        
         CLC   REPKREP,WORK                                                     
         BE    GETREP2                                                          
         MVC   RNAME,SPACES                                                     
         MVC   REPNM,SPACES                                                     
         B     GETRX                                                            
GETREP2  DS    0H                                                               
         CLC   REPKREP,STRFREP                                                  
         BNE   GETRX                                                            
         DROP  RE                                                               
         L     R6,ADREP                                                         
         L     RE,ADSTATAD                                                      
         USING ADDRREC,RE                                                       
         MVC   ANAME,RNAME                                                      
         MVC   A1LINE,R1LINE                                                    
         MVC   A2LINE,R2LINE                                                    
         MVC   A3LINE(8),R3LINE                                                 
         MVC   ABIGZIP,RBIGZIP                                                  
GETRX    MVC   KEY,RPKYSAVE                                                     
         XIT1                                                                   
         DROP  R6                                                               
         DROP  RE                                                               
         LTORG                                                                  
*>RPKYSAVE DS    CL32                                                           
         EJECT                                                                  
***********************************************************************         
* STATION TOTALS                                                      *         
***********************************************************************         
         SPACE                                                                  
STATOTC  NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
*                                                                               
         MVI   CONEND,C'N'         SET END OF STATION                           
         OC    SVIDADR,SVIDADR     CHECK FOR CONTRACTS ACTIVE                   
         BZ    STNOCON             NO CONTRACTS ACTIVE                          
         MVI   CONEND,C'Y'         SET CONTRACT END                             
         SPACE 2                                                                
         L     R5,BUYLIST          CHECK FOR END OF STATION                     
         CLC   SVIDADR(1),0(R5)                                                 
         BE    *+12                                                             
         LA    R5,13(R5)                                                        
         B     *-14                                                             
         CLI   13(R5),0            TEST ANY MORE IDS                            
         BNE   *+8                                                              
         MVI   CONEND,C'N'         SET END OF STATION                           
         CLI   IDTOTALS,C'Y'       WANT CONTRACT TOTALS                         
         BE    STNOCON             YES - TREAT AS END OF STATION                
         CLI   CONEND,C'Y'                                                      
         BE    STBYPID                                                          
         SPACE 2                                                                
         LA    RE,STAACC            NO - ADD TO OVERALL TOTALS                  
         LA    R1,STTACC                                                        
*>>>     LA    R0,11                                                            
         LA    R0,STAACCSQ                                                      
STCON1   L     R9,0(R1)                                                         
         C     R9,=F'-1'           PRESERVE ANY ($) OVERFLOW                    
         BE    STCON1A             - TOTAL ACCUM HAS OVERFLOW                   
         CLC   0(RE,4),=F'-1'                                                   
         BE    *+12                - PERIOD ACCUM HAS OVERFLOW                  
         A     R9,0(RE)                                                         
         BNO   *+8                 DETECT OVERFLOW                              
         L     R9,=F'-1'                                                        
         ST    R9,0(R1)                                                         
STCON1A  LA    R1,L'STTACC(R1)                                                  
         LA    RE,L'STAACC(RE)                                                  
         BCT   R0,STCON1                                                        
         SPACE 2                                                                
         MVC   MEDPERD,SVRDTE                                                   
         XC    STAGRID(224),STAGRID                                             
*>>>     MVC   STASPOT(44),STTSPOT                                              
*>>>     XC    STTSPOT(44),STTSPOT                                              
         MVC   STAACC(STAACCLQ),STTACC                                          
         XC    STTACC(STTACCLQ),STTACC                                          
         SPACE 2                                                                
STNOCON  DS    0H'0'                                                            
         CLI   INVFILM,0                                                        
         BE    STAINVFX                                                         
         MVC   P(45),=C'***THERE ARE    INVALID FILMS ON THIS STATION'          
         EDIT  INVFILM,(2,P+13)                                                 
         CLI   INVFILM,1                                                        
         BNE   *+10                                                             
         MVC   P(45),=C'*** THERE IS  1 INVALID FILM ON THIS STATION '          
         GOTO1 REPORT                                                           
STAINVFX DS    0C                                                               
         OC    STAUNA,STAUNA                                                    
         BZ    UNAEXIT                                                          
         EDIT  STAUNA,(4,P)                                                     
         MVC   P+5(17),=C'UNALLOCATED SPOTS'                                    
         GOTO1 REPORT                                                           
UNAEXIT  DS    0C                                                               
         XC    STAUNA,STAUNA                                                    
         MVC   MEDNUMWK,NUMWK      SET UP FOR MEDDATE                           
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMQT,=F'4'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDLCHNK,=F'200'                                                 
         MVC   QSTART(12),PASSQST  SET PASS START AND END                       
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   QSTART(12),REASTART RESTORE REQUEST START AND END                
STATOT1  OC    STASPOT,STASPOT                                                  
         BZ    M10B53                                                           
         MVI   P1,0                                                             
         ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         CH    RE,=H'5'                                                         
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         XC    P1,P1                                                            
         XC    P2,P2                                                            
         XC    P3,P3                                                            
         LA    R5,MEDPERD                                                       
         GOTO1 DATCON,DMCB,(X'02',(R5)),(X'08',P2+14)                           
         MVI   P2+22,C'-'                                                       
         GOTO1 DATCON,DMCB,(X'02',2(R5)),(X'08',P2+23)                          
         MVC   P2(7),STACAP                                                     
         MVC   P2(9),BIGSTA                                                     
         MVC   P2+10(3),=C'TOT'                                                 
         CLI   CONEND,C'Y'         END OF CONTRACT                              
         BNE   STATOT2                                                          
         CLI   QBYID,C'Y'                                                       
         BNE   STATOT2                                                          
         MVI   P2+12,C' '                                                       
         MVC   P2(12),BUYIDNAM                                                  
         MVC   P3(09),=C'**TOTAL**'                                             
         MVI   P5,0                                                             
         MVI   P6,0                                                             
STATOT2  DS    0C                                                               
         MVC   P4+25(6),=C'TLCSTS'                                              
         CLI   QMED,C'R'                                                        
         BE    *+8                                                              
         CLI   QMED,C'X'                                                        
         BNE   *+10                                                             
         MVC   P4+25(7),=C'BRDCSTS'                                             
         EDIT  STASPOT,(4,P4+20)                                                
         CLI   DETOCOST,NOSETQ     SUPPRESS COST                                
         BE    M10B                 YES                                         
         LA    R6,P3+14                                                         
         SPACE 2                                                                
M10A     L     RF,STACOST                                                       
         C     RF,=F'-1'                                                        
         BNE   M10AA                                                            
         MVC   0(5,R6),=C'*****'   $ TOTALLING OVERFLOW                         
* EVEN WITH MANY OVERFLOW CHECKS THE SALSUM TOTAL WAS STILL OVERFLOWING         
* PROBABLY BECAUSE BUFFALO DOES NOT TRAP ACCUMULATOR OVERFLOW!                  
* ALSO UNABLE TO DETECT OVERFLOW FOR RECAP/SUMMARY SO SET FLAG                  
         MVI   HADOFLOW,C'Y'       FLAG FOR RACAP/SUMMARY                       
         B     M10B                                                             
M10AA    C     RF,=F'99999999'                                                  
         BH    M101A                                                            
         EDIT  STACOST,(10,(R6)),2,MINUS=YES,FLOAT=$                            
         B     M10B                                                             
         SPACE 2                                                                
M101A    L     RF,STACOST          DROP PENNIES                                 
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,=F'100'                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(10,(R6)),,MINUS=YES,FLOAT=$                                
         SPACE 2                                                                
* BUILD GRID                                                                    
M10B     CLC   RTYPE,=C'RS '                                                    
         BE    M10B52                                                           
         LA    R5,STAGRID                                                       
         L     R6,APSTAGRD                                                      
         USING PSTOGRID,R6                                                      
         LHI   R8,PSTOGMXQ                                                      
         LA    R4,MEDMON01                                                      
M10B1    OC    0(4,R5),0(R5)                                                    
         BZ    M10B2                                                            
         GOTO1 DATCON,DMCB,(X'02',2(R4)),(X'08',WORK)                           
         MVC   PSTOGRID+1(3),WORK                                               
         AHI   R6,2*L'P                                                         
         L     R1,0(R5)                                                         
         EDIT  (R1),PSTOGRID                                                    
         SHI   R6,2*L'P                                                         
         LA    R6,4(R6)                                                         
         BCT   R8,M10B2                                                         
         B     M10B2A2                                                          
*                                                                               
M10B2    LA    R5,4(R5)                                                         
M10B2A1  LA    R4,L'MEDMON01(R4)                                                
         LA    RE,MEDMON13                                                      
         CR    R4,RE                                                            
         BH    M10B2A                                                           
         OC    0(4,R4),0(R4)                                                    
         BZ    M10B2A1                                                          
         B     M10B1                                                            
*                                                                               
M10B2A2  L     R6,APSTAGRD                                                      
         AHI   R6,4*L'P                                                         
         LHI   R1,PSTOGMXQ                                                      
         B     M10B1                                                            
         B     M10B2                                                            
         DROP  R6                                                               
*                                                                               
* CALCULATE DEMOS AND CPM                                                       
M10B2A   LA    R3,PRTLINE                                                       
         XC    PRTLINE,PRTLINE                                                  
         MVC   SUMDL(8),STACOST                                                 
*>>>     MVC   SUMD1(32),STADEMS                                                
         MVC   SUMD1(SUMDEMLQ),STADEMS                                          
         MVC   FULL,=F'1'                                                       
         GOTO1 VCALCPP,DMCB,FULL                                                
         GOTO1 VEDTDEMS                                                         
*                                                                               
         LA    R8,P2                                                            
         USING PSTOTLND,R8                                                      
         CLI   DETODEMO,1          DEMOS REQUESTED                              
         BNE   M10B52                                                           
***      ST    R8,FULL             MOVE DEMOS AND CPP TO PRINT                  
         LA    RE,PLD1                                                          
         LA    R1,DNAMES                                                        
         L     R6,NODEMS                                                        
         MVI   BYTE,C' '           SET FOR EQUIVALENCE FLAG                     
         CLC   SUMDL,SUMDLEQ                                                    
         BE    *+8                                                              
         MVI   BYTE,C'+'                                                        
M10B21   MVC   PSTONAM1,0(R1)                                                   
         MVC   PSTODEM1,0(RE)                                                   
         CLI   DETOCPM,YESSETQ     CPP/M REQUESTED                              
         BNE   *+16                                                             
         MVC   PSTOCPM1(L'PLD1CP-1),PLD1CP-PLD1+1(RE)                           
         MVC   PSTOEQIV,BYTE                                                    
         BCT   R6,*+8                                                           
         B     M10B52                                                           
         MVC   PSTONAM2,L'DNAME1(R1)                                            
         MVC   PSTODEM2,PLDEMNLQ(RE)                                            
         CLI   DETOCPM,YESSETQ     CPP/M REQUESTED                              
         BNE   *+10                                                             
         MVC   PSTOCPM2(L'PLD2CP-1),PLD2CP-PLD1+1(RE)                           
         LA    R1,2*L'DNAME1(R1)                                                
         LA    RE,2*PLDEMNLQ(RE)                                                
         LA    R8,L'P(R8)                                                       
         BCT   R6,M10B21                                                        
         DROP  R8                                                               
*                                                                               
M10B52   GOTO1 REPORT                                                           
         CLI   PASS,X'FF'                                                       
         BE    M10B54                                                           
         CLI   MAXPASS,1           ONLY ONE PASS                                
         BE    M10B54                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
STBYPID  LA    RE,STAACC            NO - ADD TO OVERALL TOTALS                  
         LA    R1,STTACC                                                        
*>>>     LA    R0,11                                                            
         LA    R0,STAACCSQ                                                      
M10B53A  L     R9,0(R1)                                                         
         C     R9,=F'-1'           PRESERVE ANY ($) OVERFLOW                    
         BE    M10B53B             - TOTAL ACCUM HAS OVERFLOW                   
         CLC   0(RE,4),=F'-1'                                                   
         BE    *+12                - PERIOD ACCUM HAS OVERFLOW                  
         A     R9,0(RE)                                                         
         BNO   *+8                 DETECT OVERFLOW                              
         L     R9,=F'-1'                                                        
         ST    R9,0(R1)                                                         
M10B53B  LA    R1,L'STTACC(R1)                                                  
         LA    RE,L'STAACC(RE)                                                  
         BCT   R0,M10B53A                                                       
         SPACE 2                                                                
* SET UP FOR NEXT PASS                                                          
M10B53   CLI   MAXPASS,1                                                        
         BE    M10B54                                                           
         CLI   PASS,X'FF'                                                       
         BE    M10B54                                                           
         SR    RE,RE                                                            
         IC    RE,PASS                                                          
         LA    RE,1(RE)                                                         
         STC   RE,PASS                                                          
         CLC   PASS,MAXPASS        TRUE END OF STATION                          
         BL    M10B6                                                            
         MVI   PASS,X'FF'                                                       
         CLC   QSTA(4),=CL4'ALL'   ALL NWKS - IGNORE STN GRAND TOTAL            
         BE    M10B55              (ONLY PRINTS FOR FIRST STATION!)             
         CLI   CONEND,C'Y'         END OF CONTRACT                              
         BNE   *+12                                                             
         CLI   IDTOTALS,C'Y'                                                    
         BNE   M10B55                                                           
         MVC   MEDPERD,SVRDTE                                                   
         XC    STAGRID(224),STAGRID                                             
***      MVC   STASPOT(STAACCLQ),STTSPOT                                        
***      XC    STTSPOT(STTACCLQ),STTSPOT                                        
         MVC   STAACC(STAACCLQ),STTACC                                          
         XC    STTACC(STTACCLQ),STTACC                                          
         B     STATOT1                                                          
*                                                                               
M10B54   MVI   PASS,0              YES - RESET AND EXIT                         
         OC    TAXAMT,TAXAMT                                                    
         BZ    M10B54A                                                          
         MVC   P(53),=C'***TAX OF XXXXX.XX EXCLUDED FROM THIS REPORT'           
         EDIT  TAXAMT,(8,P+10),2                                                
         GOTO1 REPORT                                                           
         XC    TAXAMT,TAXAMT                                                    
M10B54A  GOTO1 VFOOT,DMCB,(RA)                                                  
         CLI   QPROG,C'U'                                                       
         BNE   M10B55                                                           
         GOTO1 VREVBUY,DMCB,(RA)                                                
M10B55   DS    0H                                                               
         MVI   PASS,0                                                           
         MVC   WORK(12),PASSTAB                                                 
         MVC   PASSQST(12),PASSTAB                                              
         MVC   MID1,SPACES                                                      
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         XC    STAGRID(224),STAGRID                                             
         XC    STASPOT,STASPOT                                                  
         XC    STADEMS(STADMSLQ),STADEMS                                        
         XC    STACOST(8),STACOST                                               
         B     M10EXIT                                                          
*                                                                               
M10B6    MVI   MODE,REREAD                                                      
         SR    R6,R6                                                            
         IC    R6,PASS                                                          
         MH    R6,=H'12'                                                        
         LA    R6,PASSTAB(R6)                                                   
         USING PASSTABD,R6                                                      
         MVC   WORK(12),0(R6)                                                   
         MVC   PASSQST(12),0(R6)   SET PASS START AND END                       
         XC    STAGRID(224),STAGRID                                             
         XC    STASPOT,STASPOT                                                  
         XC    STADEMS(STADMSLQ),STADEMS                                        
         XC    STACOST(8),STACOST                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LINE                                                          
         IC    RF,MAXLINES                                                      
         SR    RF,RE                                                            
         L     R1,AHDATES                                                       
         CLI   FORCEHED,C'Y'                                                    
         BE    M10B7                                                            
         MVI   FORCEHED,C'Y'                                                    
         CHI   RF,14                                                            
         BL    M10EXIT                                                          
         MVI   FORCEHED,C'N'                                                    
         L     R1,APSTAGRD                                                      
M10B7    DS    0H                                                               
         MVI   ALLOWLIN,14                                                      
         MVI   FORCEMID,C'Y'                                                    
M10EXIT  GOTO1 DATCON,DMCB,WORK,(X'03',PASSSD3)                                 
         GOTO1 DATCON,DMCB,WORK,(X'02',PASSSD2)                                 
         GOTO1 DATCON,DMCB,WORK+6,(X'03',PASSED3)                               
         GOTO1 DATCON,DMCB,WORK+6,(X'02',PASSED2)                               
         ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         CHI   RE,8                                                             
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT DEMO/CPP                                                       *         
***********************************************************************         
         SPACE                                                                  
EDTDEMSC NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         LA    R4,SVDEMS                                                        
         LA    R5,PLDEMS                                                        
         XC    PLDEMS,PLDEMS                                                    
         LA    R6,14                                                            
EDTDEMS2 OC    0(4,R4),0(R4)                                                    
         BZ    EDTDEM4                                                          
         L     R8,0(R4)            GET DEMO VALUE                               
         C     R8,=F'9999'         DECIMAL PRECISION OK                         
         BH    EDTDEM3              NO - DIVIDE BY 10                           
         EDIT  (R8),(6,(R5)),1      YES - MAINTAIN PRECISION                    
         B     EDTDEM4                                                          
EDTDEM3  SRDA  R8,32               DROP DECIMAL PRECISION                       
         SLA   R9,1                                                             
         D     R8,=F'10'                                                        
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         EDIT  (R9),(6,(R5))                                                    
EDTDEM4  LA    R5,6(R5)            GET CPP                                      
         LA    R4,4(R4)                                                         
         L     R8,0(R4)                                                         
         C     R8,=F'9999'                                                      
         BH    EDTDEM4A                                                         
         LTR   R8,R8                                                            
         BZ    EDTDEM5                                                          
         EDIT  (R8),(6,(R5)),2                                                  
         B     EDTDEM5                                                          
EDTDEM4A SRDA  R8,32                                                            
         SLA   R9,1                                                             
         D     R8,=F'10'                                                        
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         C     R9,=F'9999'                                                      
         BH    EDTDEM4B                                                         
         EDIT  (R9),(6,(R5)),1                                                  
         B     EDTDEM5                                                          
EDTDEM4B LR    R8,R9                                                            
         SRDA  R8,32                                                            
         SLA   R9,1                                                             
         D     R8,=F'10'                                                        
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         EDIT  (R9),(6,(R5))                                                    
EDTDEM5  LA    R4,4(R4)                                                         
         LA    R5,6(R5)                                                         
         BCT   R6,EDTDEMS2                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* HANDLES PRINTING OF NETWORK TOTAL SECTION AT END OF 'MARKET'       *          
**********************************************************************          
         SPACE                                                                  
MLASTC   NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         MVC   WEIGHT,SPWEIGHT                                                  
         GOTO1 VSUMMRY                                                          
         L     R8,BUFFBUFF                                                      
         MVC   DMCB+8(20),LVCNTRL                                               
         GOTO1 BUFFALO,DMCB,=C'ADD',(BUFCDE,(R8))                               
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFCDE,(R8)),(X'80',1)                   
         ZIC   RE,BUFCDE                                                        
         LA    RE,3(RE)                                                         
         STC   RE,BUFCDE                                                        
         GOTO1 BUFFALO,DMCB,=C'ADD',(BUFCDE,(R8))                               
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFCDE,(R8)),(X'80',1)                   
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTRACT ???                                                         *         
***********************************************************************         
         SPACE                                                                  
EXTRCT   NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         MVC   QSTART(12),PASSQST                                               
* ENSURE HAVE RIGHT DATES FOR CUTIN SEARCH                                      
         GOTO1 DATCON,DMCB,PASSQST,(X'02',PASSSD2)                              
         GOTO1 DATCON,DMCB,PASSQST+6,(X'02',PASSED2)                            
         MVC   MEDNUMWK,NUMWK                                                   
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMQT,=F'0'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDLCHNK,=F'200'                                                 
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   MEDBRAND,BPRD                                                    
         MVC   BYTE,MEDSPTLN                                                    
         MVI   MEDSPTLN,0                                                       
         MVI   MEDEXTDM,14  <===                                                
EXTSL    XC    PSLIST(100),PSLIST                                               
         GOTO1 MEDPSL,DMCB,(RA),PSLIST                                          
         CLI   BPRD,X'FF'                                                       
         BE    EXTSLX                                                           
         LA    RE,PSLIST           GET SPOT LENGTH FOR PRODUCT                  
EXTSL1   CLI   0(RE),0                                                          
         BE    EXTSLX                                                           
         CLC   0(1,RE),BPRD                                                     
         BE    EXTSL2                                                           
         LA    RE,2(RE)                                                         
         B     EXTSL1                                                           
EXTSL2   MVC   MEDSPTLN,1(RE)                                                   
EXTSLX   DS    0C                                                               
         LA    R5,2                SET DEMO LOOKUP CODE                         
         CLI   QRERATE,C' '                                                     
         BE    EXTRCT2                                                          
         CLI   QRERATE,C'P'        SET FOR POST O/RIDES (RERATE)                
         BNE   *+8                                                              
         LA    R5,4                                                             
*                                                                               
EXTRCT2  GOTO1 MEDGETBY,DMCB,(RA),(R5)                                          
*                                                                               
         CLI   MEDSPILL,C'Y'                                                    
         BNE   EXTRCT2D                                                         
         CLI   SPOTPROF+5,0                                                     
         BE    EXTRCTX                                                          
*                                                                               
EXTRCT2D GOTO1 MEDMKTWT,DMCB,(RA),(R9)                                          
*                                                                               
         MVC   PTSSPILL,MEDSPILL                                                
         MVC   ASVPRD,MEDBRAND     GET 14 DEMOS FOR PERIOD                      
         MVC   ASVLCHNK,MEDLCHNK                                                
         MVC   ASVFRST,MEDAFRST                                                 
         MVC   ASVLAST,MEDALAST                                                 
         MVI   MEDEXTDM,14                                                      
         MVC   MEDBRAND,BPRD                                                    
         MVC   MEDLCHNK,=F'200'                                                 
         LA    RE,MEDPERD                                                       
         ST    RE,MEDAFRST                                                      
         ST    RE,MEDALAST                                                      
         CLI   BPRD,X'FF'                                                       
         BNE   BYPUNA                                                           
         L     R4,4(RE)            SAVE TAX DOLLARS WHEN DOING 14 DEMOS         
         MVC   SVMSTAX,MEDMSTAX     DEMO 13 IS OVERLAYED BY TAX TOTAL           
         MVC   SVMEDBYD,MEDBYD                                                  
         MVC   SVEXTAX,MEDEXTAX                                                 
         MVI   MEDEXTAX,C'N'                                                    
         MVI   MEDBRAND,219        COUNT UNALOCATED SPOTS                       
         LA    RF,2                                                             
         CLI   QRERATE,C'P'        CATER FOR POST O/RIDES HERE                  
         BNE   *+8                                                              
         LA    RF,4                                                             
         GOTO1 MEDGETBY,DMCB,(RA),(RF)                                          
         LA    RE,MEDPERD                                                       
         L     R4,4(RE)                                                         
         L     R1,MEDBYSPT                                                      
         A     R1,UNATOT                                                        
         CLI   EXTSW,C'Y'          ONLY ADD THE LOCALS                          
         BNE   *+8                                                              
         ST    R1,UNATOT                                                        
         MVC   MEDBRAND,BPRD                                                    
         GOTO1 MEDGETBY,DMCB,(RA),(R5)                                          
         MVC   MEDMSTAX,SVMSTAX    RESTORE TAX DOLLARS                          
         MVC   MEDBYD(8),SVMEDBYD                                               
         MVC   MEDEXTAX,SVEXTAX                                                 
         GOTO1 MEDMKTWT,DMCB,(RA),(R9)                                          
BYPUNA   DS    0C                                                               
         MVI   MEDEXTDM,MAXDEMOQ                                                
         MVC   MEDLCHNK,=F'200'                                                 
         MVC   MEDBRAND,ASVPRD                                                  
         MVC   MEDAFRST,ASVFRST                                                 
         MVC   MEDALAST,ASVLAST                                                 
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         MVC   BYTE,BDSEC                                                       
         DROP  R5                                                               
         CLC   QPROG,=C'RS'                                                     
         BNE   BYPUNB                                                           
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         OC    MEDBYD(12),MEDBYD                                                
         BZ    EXTRCTX                                                          
BYPUNB   DS    0H'0'                                                            
         BAS   RE,GETHP                                                         
         OC    UNATOT,UNATOT       ANY UNALLOCATED                              
         BNZ   HAVUNA               YES - OK TO PROCESS                         
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         OC    MEDBYD(12),MEDBYD   ANY SPOTS OR DOOLARS                         
         BZ    EXTRCTX              NO - EXIT                                   
HAVUNA   DS    0H'0'                                                            
         MVC   WEIGHT,SPWEIGHT                                                  
         CLI   EXTSW,C'N'          RELEASE INFORMATION TO BUFFALO               
         BE    EXTRCTX             NO - EXIT                                    
         XC    PRTLINE,PRTLINE                                                  
         LA    R3,PRTLINE                                                       
         L     R5,MEDAFRST         RELEASE DATA TO BUFFALO                      
         CLI   MRPTTYP,C'2'        RPT 2 DOESNT NEED WEEKLYS                    
         BNE   EXTRCT3                                                          
         LA    R5,MEDMON01                                                      
EXTRCT3  MVI   SUMCODE,X'90'                                                    
         MVC   SUMDPGNO(8),MEDDPGNO     SET DAYPART                             
         MVC   SUMSLN,BYTE                                                      
         MVC   MEDSPTLN,BYTE                                                    
         L     R4,4(R5)                                                         
         MVI   SUMRTYP,1                                                        
         LA    RF,MEDPERD                                                       
         CR    R5,RF               END                                          
         BH    EXTRCTX                                                          
         BNE   EXTRCT3A            WEEKLY OR MONTHLY                            
         OC    MEDBYD(12),MEDBYD                                                
         BZ    EXTRCT5              NO - EXIT                                   
         MVI   SUMRTYP,3                                                        
         MVC   SUMDT,=X'FFFFFFFF'                                               
         B     EXTRCT4                                                          
EXTRCT3A OC    0(4,R5),0(R5)       ACTIVE SLOT                                  
         BZ    EXTRCT5                                                          
         LA    RF,MEDMON01                                                      
         CR    R5,RF               MONTHLY                                      
         BL    EXTRCT3B                                                         
         MVI   SUMRTYP,2            YES-SET RECORD CODE                         
         CLI   MRPTTYP,C'2'        MONTHLYS FOR RPT 2 ONLY                      
         BE    EXTRCT3B                                                         
         LA    R5,MEDPERD                                                       
         B     EXTRCT3                                                          
EXTRCT3B MVC   SUMDT,0(R5)                                                      
EXTRCT4  LA    RE,SUMKEY           SET UP DATA ITEM DISPLACEMENTS               
         USING SUMDATA,RE                                                       
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         A     RE,BUFFLKEY                                                      
         LA    R8,MEDPERD                                                       
         CR    R5,R8                                                            
         BNE   *+16                                                             
         L     R8,TAXAMT                                                        
         A     R8,MEDMSTAX                                                      
         ST    R8,TAXAMT                                                        
         XC    SUMDATA(SUMDATLQ),SUMDATA                                        
         CLI   EXTSW,C'S'          SET UP SPOTS AND DOLLARS                     
         BNE   E4L                                                              
         MVC   SUMSPOTS,MEDBYSPT   MOVE IN DATA                                 
E4L      CLI   EXTSW,C'Y'          SET UP DEMOS                                 
         BNE   E4LEX                                                            
         MVC   SUMDL,MEDBYD                                                     
         MVC   SUMDLEQ,MEDBYDEQ                                                 
         MVC   SUMD1,MEDBY1                                                     
         MVC   SUMD1EQ,MEDBY1EQ                                                 
         MVC   SUMD2,MEDBY2                                                     
         MVC   SUMD2EQ,MEDBY2EQ                                                 
         MVC   SUMD3,MEDBY3                                                     
         MVC   SUMD3EQ,MEDBY3EQ                                                 
         MVC   SUMD4,MEDBY4                                                     
         MVC   SUMD4EQ,MEDBY4EQ                                                 
         MVC   SUMD5,MEDBY5                                                     
         MVC   SUMD5EQ,MEDBY5EQ                                                 
         MVC   SUMD6,MEDBY6                                                     
         MVC   SUMD6EQ,MEDBY6EQ                                                 
         MVC   SUMD7,MEDBY7                                                     
         MVC   SUMD7EQ,MEDBY7EQ                                                 
         MVC   SUMD8,MEDBY8                                                     
         MVC   SUMD8EQ,MEDBY8EQ                                                 
E4LEX    MVI   BUYACT,1                                                         
         LA    R8,PROGPROF                                                      
         USING PROFDSCT,R8                                                      
         CLI   PROFDPT,C'Y'        DAYPART ANALYSIS REQUIRED                    
         BNE   EXTRCT4T             NO - PUT OUT TOTALS ONLY                    
         DROP  R8                                                               
         CLI   MEDSPILL,C'Y'                                                    
         BNE   *+12                                                             
         CLI   SPOTPROF+5,1                                                     
         BE    EXTRCT4T                                                         
         L     R8,BUFFBUFF                                                      
         BAS   R9,EXTPUT                                                        
* PUT OUT PRODUCT GROUP DETAILS                                                 
         MVI   SUMCODE,X'93'                                                    
         BAS   R9,EXTPUT                                                        
EXTRCT4T L     R8,BUFFBUFF                                                      
         MVC   SUMDPGNO(9),=9X'FF'                                              
         MVI   SUMCODE,X'90'                                                    
         BAS   R9,EXTPUT                                                        
         MVI   SUMCODE,X'93'                                                    
         BAS   R9,EXTPUT                                                        
         CLI   SPOTPROF+5,2        DO WE NEED SPILL OR ORIG                     
         BE    EXTRCT5             NO                                           
         CLI   SPOTPROF+5,0                                                     
         BE    EXTRCT5                                                          
         CLI   MEDSPILL,C'Y'       IS THIS SPILL                                
         BE    EX4SPL              YES                                          
         MVI   SUMSLN,X'FE'        PUT OUT ORIG TOTALS                          
         MVI   SUMCODE,X'89'                                                    
         BAS   R9,EXTPUT                                                        
         MVI   SUMCODE,X'92'                                                    
         BAS   R9,EXTPUT                                                        
         B     EXTRCT5                                                          
EX4SPL   CLI   SPOTPROF+5,1        DO WE NEED SPILL TOTALS                      
         BE    *+8                                                              
         CLI   SPOTPROF+5,3                                                     
         BNE   EXTRCT5                                                          
         MVI   SUMSLN,X'FD'        PUT OUT SPILL TOTALS                         
         MVI   SUMCODE,X'88'                                                    
         BAS   R9,EXTPUT                                                        
         MVI   SUMCODE,X'91'                                                    
         BAS   R9,EXTPUT                                                        
* GET NEXT BUFFALO ITEM                                                         
EXTRCT5  LA    R5,12(R5)                                                        
         B     EXTRCT3                                                          
EXTPUT   CLI   SUMCODE,X'90'                                                    
         BHR   R9                                                               
*        OC    SUMSPOTS(16),SUMSPOTS                                            
         OC    SUMDATA(SUMDEND-SUMDATA),SUMDATA  CHECK ALL DEMOS!               
         BZR   R9                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',(R8),PRTLINE                                
         BR    R9                                                               
         SPACE 2                                                                
EXTRCTX  XIT1                                                                   
         DROP  RE                                                               
         EJECT                                                                  
* GETHP ROUTINE                                                                 
GETHP    NTR1                                                                   
         MVI   MGSW,0                                                           
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         L     R3,ADBUY                                                         
         USING BUYREC,R3                                                        
         L     R5,MEDAFRST                                                      
         MVC   STRDTE,0(R5)                                                     
         L     R5,MEDALAST                                                      
         MVC   ENDDTE,2(R5)                                                     
         OC    PGNOENT,PGNOENT                                                  
         BNZ   GETHP1                                                           
         XC    HPSNO,HPSNO                                                      
         MVC   LASTGSLT,=A(PGRID)                                               
         XC    PGNOENT,PGNOENT                                                  
GETHP1   LA    R5,BDELEM                                                        
         USING REGELEM,R5                                                       
REGNXT   ZIC   RE,RLEN                                                          
         AR    R5,RE                                                            
         CLI   0(R5),0                                                          
         BE    GETHPX                                                           
         CLI   RCODE,6                                                          
         BL    REGNXT                                                           
         CLI   RCODE,13                                                         
         BH    REGNXT                                                           
         CLC   RDATE,STRDTE                                                     
         BL    REGNXT                                                           
         CLC   RDATE,ENDDTE                                                     
         BH    REGNXT                                                           
         TM    RSTATUS,RSMINUSQ    X'80'                                        
         BO    REGNXT                                                           
         CLI   RCODE,11                                                         
         BL    GETHP2                                                           
         CLI   MEDBRAND,X'FF'      POL PRODUCT                                  
         BE    GHPPGRP             POL - CHECK FOR PRODUCT GROUPS               
         CLI   BRDPOLSW,C'0'       BRAND POL                                    
         BNE   GHPPGRP              YES - DONT CHECK PRODUCT                    
         CLI   RLEN,RLPOL1LQ       14             UNALLOCATED                   
         BL    REGNXT               BYPASS                                      
         BE    *+14                                                             
         CLC   MEDBRAND,RPPRD+L'RPALLOC  RIGHT BRAND                            
         BE    *+10                                                             
         CLC   MEDBRAND,RPPRD      WRONG BRAND                                  
         BNE   REGNXT              BYPASS                                       
*                                                                               
GHPPGRP  CLI   QPGR,C' '           PRODUCT GROUPS REQUESTED                     
         BE    GETHP2               NO - ALLOW ALL BRANDS                       
         CLI   RLEN,RLPOL1LQ       14             BYPASS UNALLOCATED            
         BL    REGNXT                                                           
         LA    R8,PSLIST                                                        
GHPPGRP2 CLI   0(R8),0                                                          
         BE    REGNXT                                                           
         CLC   RPPRD,0(R8)         PRODUCT IN LIST                              
         BE    GETHP2               YES - PROCESS                               
         LA    R8,2(R8)                                                         
         B     GHPPGRP2                                                         
         SPACE 2                                                                
GETHP2   LA    R8,WORK                                                          
         USING PGDWK,R8                                                         
         USING PGSORT,R8                                                        
         ZIC   RE,HPSNO                                                         
         LA    RE,1(RE)                                                         
         STC   RE,HPSNO                                                         
         XC    WORK,WORK                                                        
         CLC   RTYPE,=C'RS '                                                    
         BNE   GETHPFX                                                          
         ZIC   R9,1(R5)                                                         
         AR    R9,R5                                                            
         CLI   0(R9),0                                                          
         BE    GETHPFX                                                          
         CLI   0(R9),X'0F'                                                      
         BL    GETHPFX                                                          
GETHPF   CLI   0(R9),X'12'         FILM ELEMENT                                 
         BNE   GETHPF1                                                          
         USING FLMELEM,R9                                                       
         MVC   PGDFDAY,FLMDAY       YES - SAVE FILM                             
         MVC   PGDFNO,FLMNUM                                                    
         B     GETHPFX                                                          
GETHPF1  ZIC   R0,1(R9)                                                         
         AR    R9,R0                                                            
         CLI   0(R9),X'0F'                                                      
         BH    GETHPF                                                           
GETHPFX  DS    0H                                                               
         MVC   PGDWK,RDATE                                                      
         ST    R5,FULL                                                          
         MVC   PGDELAD,FULL        SAVE ELEMENT ADDRESS                         
         CLC   RTYPE,=C'RS '                                                    
         BNE   GETHP2A0                                                         
         TM    RSTATUS,RSMINSDQ    X'40'                                        
         BO    REGNXT                                                           
GETHP2A0 DS    0H                                                               
         MVI   PGDNOSP,1                                                        
         CLI   RCODE,10                                                         
         BH    *+10                                                             
         MVC   PGDNOSP,RNUM                                                     
         TM    BDSTAT,X'80'        POL RADIO                                    
         BZ    GETHP2A1                                                         
         ZIC   R1,RPCOST                                                        
         SRL   R1,2                                                             
         STC   R1,PGDNOSP                                                       
GETHP2A1 DS    0C                                                               
         CLC   RTYPE,=C'RS '                                                    
         BNE   GETHP2A6                                                         
         CLI   RCODE,10            BACK OUT MINUS SPOTS IF ROT. SKED            
         BH    GETHP2A6                                                         
         LR    R1,R5               SAVE ELEMENT ADDRESS                         
GETHP2A2 ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),7             OTO                                          
         BNE   GETHP2A3                                                         
         CLC   RDATE,2(R1)         SAME DATE AS ORIG                            
         BNE   GETHP2A3             NO-END OF REGELEMS                          
         ZIC   RE,RNUM             GET SPOT COUNT-THIS ELEM                     
         ZIC   R0,PGDNOSP          GET SPOT COUNT THIS DATE                     
         TM    6(R5),X'80'         MINUS SPOT                                   
         BO    *+12                 YES-DECREMENT SPOT COUNT                    
         AR    R0,RE                NO - INCREMENT SPOT COUNT                   
         LR    R1,R5                     SET TO NEXT DATE                       
         B     *+6                                                              
         SR    R0,RE                                                            
         STC   R0,PGDNOSP                                                       
         B     GETHP2A2                                                         
GETHP2A3 LR    R5,R1                                                            
GETHP2A6 DS    0C                                                               
         CLI   PGDNOSP,0           HAVE ALL SPOTS BEEN MINUSED                  
         BE    REGNXT               YES - GET NEXT ELEMENT                      
         CLI   SORTREQ,0                                                        
         BE    *+10                                                             
         XC    PGDELAD,PGDELAD                                                  
         MVC   PGDSBRN,RPPRD                                                    
         TM    RSTATUS,RSRATOVQ    X'20'       SAVE COST OVERRIDES              
         BZ    *+10                                                             
         MVC   PGD2COVR,RPCOST                                                  
         TM    BDSTAT,X'80'                                                     
         BZ    *+8                                                              
         NI    PGD2COVR,B'00000011'                                             
         CLI   RLEN,15                                                          
         BL    *+16                                                             
         MVC   PGD2BRN,RPPRD+L'RPALLOC                                          
         MVC   PGD2SLN,RPTIME+L'RPALLOC                                         
         CLI   RCODE,10                                                         
         BL    GETHP2A                                                          
         CLI   RLEN,13                                                          
         BH    GETHP2A                                                          
         CLC   RTYPE,=C'RS '                                                    
         BE    REGNXT                                                           
         MVI   PGDSBRN,X'FF'                                                    
         MVI   PGDSSLN,0                                                        
         MVC   PGDSNO,HPSNO                                                     
         TM    RSTATUS,RSMINSDQ    X'40'                                        
         BZ    *+8                                                              
         MVI   PGDIND,PGDIMISQ     X'01'        SET MISSED                      
         TM    RSTATUS,RSMINSDQ+RSMGONLQ  X'42'                                 
         BNO   *+12                                                             
         MVI   PGDIND,PGDIPREQ     X'04'        SET PREMPT                      
         B     GETHP2C                                                          
         SPACE 2                                                                
GETHP2A  TM    RSTATUS,RSHIATSQ    X'04'                                        
         BZ    GETHP2B                                                          
         MVI   PGDSBRN,X'FF'                                                    
         MVI   PGDSSLN,0                                                        
         MVC   PGDSNO,HPSNO                                                     
         MVI   PGDIND,PGDIHIAQ     X'08'        SET HIATUS INDICATOR            
         B     GETHP2C                                                          
         SPACE 2                                                                
GETHP2B  TM    RSTATUS,RSMINSDQ    X'40'                                        
         BZ    *+8                                                              
         MVI   PGDIND,PGDIMISQ     X'01'        SET MISSED INDICATOR            
         TM    RSTATUS,RSMINSDQ+RSMGONLQ  X'42'                                 
         BNO   *+8                                                              
         MVI   PGDIND,PGDIPREQ     X'04'        SET PREMPT INDICATOR            
         CLC   RTYPE,=C'RS '                                                    
         BNE   *+12                                                             
         CLI   PGDIND,0            RS BYPASSES MINUS AND HIATUS SPOTS           
         BNE   REGNXT                                                           
         CLI   RCODE,10                                                         
         BL    GETHP2B1                                                         
         CLI   RLEN,RLPOL1LQ       HANDLE UNALLOCATED                           
         BL    GETHP2B1                                                         
         MVC   PGDSBRN,RPPRD                                                    
         MVC   PGDSSLN,RPTIME                                                   
         MVC   PGDSNO,HPSNO                                                     
         B     GETHP2C                                                          
GETHP2B1 MVC   PGDSBRN,BPRD                                                     
         MVC   PGDSSLN,BYTE                                                     
         MVC   PGDSNO,HPSNO                                                     
         SPACE 2                                                                
GETHP2C  L     RE,LASTGSLT         SET TO LAST GRID SLOT                        
         L     RF,PGNOENT                                                       
         LA    RF,1(RF)                                                         
         ST    RF,PGNOENT                                                       
         STC   RF,PGLSLOT                                                       
         CLI   RCODE,10                                                         
         BH    *+10                                                             
         MVC   PGDSBRN,BPRD                                                     
         MVC   0(PGRIDLQ,RE),WORK                                               
         LA    RE,PGRIDLQ(RE)                                                   
         ST    RE,LASTGSLT                                                      
         B     REGNXT                                                           
         SPACE 2                                                                
GETHPX   CLI   VARFRMT,1           VARIABLE FORMAT                              
         BE    GETHPX2              YES - ALLOW VARIABLE SLOT                   
         L     R5,MEDAFRST         NO - FORCE TO DATE SLOT                      
         L     R1,PGNOENT                                                       
         LTR   R1,R1                                                            
         BZ    GETHPX2                                                          
         L     R8,=A(PGRID)                                                     
         LA    R2,1                                                             
GETHPS   CLC   PGDWK,0(R5)                                                      
         BNL   *+12                                                             
         LA    R2,1                                                             
         L     R5,MEDAFRST                                                      
         CLC   PGDWK,2(R5)         SET FIXED SLOT FOR WEEK                      
         BNH   GETHPS2                                                          
         LA    R5,12(R5)                                                        
         LA    R2,1(R2)                                                         
         B     GETHPS                                                           
GETHPS2  STC   R2,PGLSLOT                                                       
         LA    R8,PGRIDLQ(R8)                                                   
         BCT   R1,GETHPS                                                        
GETHPX2  XIT1                                                                   
         DROP  R8,R6,R5,R3                                                      
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET DATA FROM BUFFALO                                               *         
***********************************************************************         
         SPACE                                                                  
GETBUF   NTR1  BASE=*,LABEL=*                                                   
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SUMDSECT,R3                                                      
         CLI   BUFHI,1                                                          
         BNE   GETBUF1                                                          
         XC    MYBUFIO(30),MYBUFIO                                              
         MVC   MYBUFIO(1),BUFCDE                                                
         MVI   BUFHI,0                                                          
         L     R8,BUFFBUFF                                                      
         SR    R9,R9                                                            
         IC    R9,LEVEL                                                         
         GOTO1 BUFFALO,DMCB,=C'HIGH',(BUFCDE,(R8)),MYBUFIO,(R9)                 
         B     GETBUF2                                                          
GETBUF1  L     R8,BUFFBUFF                                                      
         SR    R9,R9                                                            
         IC    R9,LEVEL                                                         
         GOTO1 BUFFALO,DMCB,=C'SEQ',(BUFCDE,(R8)),MYBUFIO,(R9)                  
GETBUF2  CLC   MYBUFIO(1),BUFCDE                                                
         BNE   *+12                                                             
         TM    DMCB+8,X'80'                                                     
         BZ    GETBUF3                                                          
         XC    MYBUFIO(30),MYBUFIO                                              
         B     GETBUFX                                                          
GETBUF3  MVC   PRTLINE,MYBUFIO                                                  
         LA    R3,MYBUFIO                                                       
         XC    MYBUFIO(8),MYBUFIO                                               
         MVI   SUMRPT,1                                                         
         MVC   SUMKEY,PRTLINE                                                   
         LA    R8,PRTLINE                                                       
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         A     R8,BUFFLKEY                                                      
         DROP  RF                                                               
         MVC   SUMDATA(SUMDATLQ),0(R8)                                          
         CLI   SPOTPROF+1,C'N'                                                  
         BE    GETBUFX                                                          
         LA    R4,DNAMES           SET UP FOR UNWEIGHTING                       
         LA    R5,MAXDEMOQ                                                      
         LA    R6,SUMD1                                                         
GBUF4    CLI   SPOTPROF+1,C'D'     UNWEIGHT TOTALS                              
         BE    GBF4                                                             
         CLI   0(R4),EXTDEMOQ      EXTENDED RATINGS                             
         BE    *+8                                                              
         CLI   0(R4),C'R'                                                       
         BNE   GBUF5                                                            
GBF4     OC    SPWEIGHT,SPWEIGHT                                                
         BNZ   GBUF4A                                                           
         XC    0(8,R6),0(R6)                                                    
         B     GBUF5                                                            
GBUF4A   DS    0H                                                               
         L     RE,0(R6)                                                         
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         D     RE,SPWEIGHT                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R6)                                                         
         L     RE,4(R6)                                                         
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         D     RE,SPWEIGHT                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,4(R6)                                                         
GBUF5    LA    R4,7(R4)                                                         
         LA    R6,8(R6)                                                         
         BCT   R5,GBUF4                                                         
*                                                                               
         CLI   SPOTPROF+1,C'D'                                                  
         BE    GBF5                                                             
         CLI   DNAMES,EXTDEMOQ                                                  
         BE    *+8                                                              
         CLI   DNAMES,RTGDEMOQ                                                  
         BNE   GETBUFX                                                          
GBF5     OC    SPWEIGHT,SPWEIGHT                                                
         BNZ   GBUF5A                                                           
         XC    SUMGD1(8),SUMGD1                                                 
         B     GETBUFX                                                          
GBUF5A   DS    0H                                                               
         L     RE,SUMGD1                                                        
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         D     RE,SPWEIGHT                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,SUMGD1                                                        
         L     RE,SUMGD1E                                                       
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,SPWEIGHT                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,SUMGD1E                                                       
GETBUFX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* POOL TIMESHEET HEADLINES                                            *         
***********************************************************************         
         SPACE                                                                  
PTSHEAD  NTR1  BASE=*,LABEL=*                                                   
         CLI   QMGR,C' '           CHECK FOR MGR REQUEST                        
         BE    PTNOMGR                                                          
         MVC   H1+23(24),MGR1NM                                                 
         MVC   H2+23(24),MGR2NM                                                 
         MVC   H3+23(24),MGR3NM                                                 
PTNOMGR  DC    0H'0'                                                            
         MVC   BIGSTA(9),SPACES                                                 
         CLC   QSTA(4),=CL4'ALL'                                                
         BNE   *+12                                                             
         CLI   MODE,MGR1LAST                                                    
         BH    *+10                SUMMARY FOR ALL NWKS - NO NAME               
*        MVC   BIGSTA(4),STATION                                                
         MVC   BIGSTA(4),STA                                                    
         MVC   H1+58(15),=C'POOL TIME SHEET'                                    
         CLI   QPROG,C'U'                                                       
         BNE   *+10                                                             
         MVC   H1+58(15),=C'POOL TURNAROUND'                                    
         MVC   H2+58(15),DASH                                                   
         LA    R1,H1+56                                                         
         LA    R8,BIGSTA+8                                                      
         LA    R6,9                                                             
SCALLP   CLI   0(R8),C' '                                                       
         BE    SCALLP1                                                          
         MVC   0(1,R1),0(R8)                                                    
         MVI   132(R1),C'-'                                                     
         BCTR  R1,0                                                             
SCALLP1  BCTR  R8,0                                                             
         BCT   R6,SCALLP                                                        
*                                                                               
         CLI   SPOTPROF+12,C'Y'                                                 
         BNE   *+10                                                             
         MVC   H9(18),=C'***TAX EXCLUDED***'                                    
         CLI   MODE,STALAST        NO ROTATION HEADINGS ON RECAP/SUMRY          
         BH    AFFDONE                                                          
         BE    RSEX1                                                            
*                                                                               
         MVC   H10(32),DASH                                                     
         MVC   H10+9(15),=C'BUY DESCRIPTION'                                    
         MVC   H10+34(62),DASH                                                  
         MVC   H10+56(16),=C'ROTATION PATTERN'                                  
         MVC   H10+93(40),DASH                                                  
         MVC   H10+104(18),=C'DEMOGRAPHICS (XXX)'                               
         MVC   H10+118(3),=C'NSI'  SET RATING SERVICE                           
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         CLI   CPROF+3,X'F0'                                                    
         BE    RSEX                                                             
         MVC   H10+118(3),=C'ARB'                                               
         DROP  R6                                                               
         L     R6,ADAGY                                                         
         USING AGYRECD,R6                                                       
         CLI   AGYPROF+7,C'C'                                                   
         BNE   RSEX                                                             
         MVC   H10+118(3),=C'BBM'                                               
         DROP  R6                                                               
RSEX     DS    0H                                                               
         MVC   H11(32),=C'EST START-END   WKS  DAYS    N/W'                     
* ROTATION ONLY SHOWS AUDIENCE NOT RATINGS                                      
*        MVC   H11+93(39),=C'NAME     GRPS   CPP NAME     GRPS   CPP'           
*        MVC   H12+93(39),=C'         IMPS   CPM          IMPS   CPM'           
         MVC   H12+93(39),=C'NAME     IMPS   CPM NAME     IMPS   CPM'           
* SHOW IF DEMOS ARE POST BUY IN UNUSED SECTION OF H11                           
         CLI   PBDREP,C'N'                                                      
         BE    PTSH010                                                          
         LA    RE,H11+104                                                       
         LA    RF,H11+120                                                       
         CLI   FRMTDEMO,1                                                       
         BNE   *+12                                                             
         LA    RE,H11+113                                                       
         LA    RF,H11+122                                                       
         MVC   0(17,RE),=C'(POST-BUY VALUES)'                                   
         CLI   PBDREP,C'O'                                                      
         BE    PTSH010                                                          
         MVC   0(10,RF),=C' INCLUDED)'                                          
PTSH010  EQU   *                                                                
         MVC   H13+113(19),=C'RATED PROGRAM  BOOK'                              
         MVC   H12(32),=C'LIN TIME        DPT  LEN  REGION'                     
         MVC   H13+4(28),=C'PROGRAMMING      ADJ    COST'                       
         MVC   H14+4(8),=C'COMMENTS'                                            
*        CLI   MODE,STALAST                                                     
*        BH    AFFDONE                                                          
*        BE    RSEX1                                                            
         CLI   CIPRTSW,1           CUTINS DON'T RESTORE BDSEC                   
         BE    RSEX1                                                            
         OC    SVP1(L'SVP1+L'SVP2+L'SVP3+L'SVP4),SVP1   NULL 1ST PASS           
         BZ    RSEX1               DON'T PRINT BLANK LINES AT START             
         MVC   P(33),SVP1                                                       
         MVC   P2(33),SVP2                                                      
         MVC   P3(32),SVP3                                                      
         MVC   P4(32),SVP4                                                      
RSEX1    MVC   SVP1(80),SPACES                                                  
         MVC   SVP3(80),SPACES                                                  
         L     RE,ADSTATAD                                                      
         USING ADDRREC,RE                                                       
         MVI   H5+42,C' '                                                       
         MVI   H6+42,C' '                                                       
         MVI   H7+42,C' '                                                       
         MVI   H8+42,C' '                                                       
         MVC   H5+43(20),ANAME                                                  
         MVC   H6+43(24),A1LINE                                                 
         MVC   H7+43(24),A2LINE                                                 
         MVC   H8+43(L'ABIGZIP),A3LINE                                          
         DROP  RE                                                               
         L     RE,ADREP                                                         
         USING REPREC,RE                                                        
         OC    RNAME,RNAME         DO WE HAVE A REP NAME                        
         BZ    PTSHBYR              NO -BYPASS                                  
         CLC   RNAME,SPACES                                                     
         BE    PTSHBYR                                                          
         MVC   H5+96(3),=C'REP'                                                 
         MVC   H5+104(3),REPKREP                                                
         MVC   H5+108(L'RNAME),RNAME                                            
         DROP  RE                                                               
PTSHBYR  L     RE,ADSTAT                                                        
         USING STAREC,RE                                                        
         CLC   SCHNL,=C'    '                                                   
         BE    SCHDONE                                                          
         CLC   SCHNL,=C'0000'                                                   
         BE    SCHDONE                                                          
         OC    SCHNL,SCHNL                                                      
         BZ    SCHDONE                                                          
         MVC   H6+96(7),=C'CHANNEL'                                             
         MVC   H6+105(2),SCHNL                                                  
         CLI   QMED,C'R'                                                        
         BNE   SCHDONE                                                          
         MVC   H6+96(7),=C'FREQ   '                                             
         MVC   H6+103(4),SCHNL                                                  
SCHDONE  DS    0H                                                               
         OC    SNETWRK,SNETWRK                                                  
         BZ    AFFDONE                                                          
         CLC   SNETWRK,=C'000'                                                  
         BE    AFFDONE                                                          
         CLC   SNETWRK,=C'   '                                                  
         BE    AFFDONE                                                          
         MVC   H6+108(9),=C'AFFILIATE'                                          
         MVC   H6+118(3),SNETWRK                                                
AFFDONE  DS    0H                                                               
         SPACE 2                                                                
         CLI   MODE,STALAST        BYPASS CONTRACT GT. STALAST                  
         BH    PTHEXIT                                                          
         CLI   IDSW,1              MULTIPLE IDS ON PAGE                         
         BE    PTHID4                                                           
PTHID1   CLI   QBYID,C'Y'                                                       
         BNE   *+16                                                             
         MVC   H4(12),BUYIDNAM                                                  
         MVC   H4+13(12),BUYID                                                  
         B     PTHEXIT                                                          
         DROP  RE                                                               
         SPACE 2                                                                
PTHID4   MVC   MID1(12),BUYIDNAM                                                
         MVC   MID1+13(12),BUYID                                                
         MVI   FORCEMID,C'Y'                                                    
         SPACE 2                                                                
PTHEXIT  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* SALESPERSONS SUMMARY                                               *          
* - PERIOD ANALYSIS (TOTALS FOR REPORTING PERIOD)                    *          
* - CONTROLLED BY PROFMTR (SETTING=1, DFLT)                          *          
**********************************************************************          
         SPACE                                                                  
SALSUM   NTR1  BASE=*,LABEL=*                                                   
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SPN5WK,R2                                                        
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
*                                                                               
         XC    SVP1,SVP1                                                        
         OC    SUMOPTS,SUMOPTS                                                  
         BZ    SALSUMX                                                          
         MVI   BUFHI,1             SET FOR READ HI                              
         MVI   BUFRTYP,1                                                        
SALDPT   DS    0H                  DAYPART PRINT RETURN                         
         MVI   ALLOWLIN,6                                                       
         MVI   FORCEMID,C'Y'                                                    
         CLI   MODE,STALAST                                                     
         BNE   SALMODE                                                          
         MVC   MID1+56(19),=C'***STATION TOTAL***'                              
         CLI   CONEND,C'Y'         CAN THIS BE A CONTRACT TOTAL                 
         BNE   SALMODE             NO                                           
         CLI   QBYID,C'Y'                                                       
         BNE   SALMODE                                                          
         MVC   MID1+38(3),=C'***'                                               
         MVC   MID1+41(12),BUYIDNAM                                             
         MVC   MID1+54(12),BUYID                                                
         GOTO1 SQUASHER,DMCB,MID1+38,38                                         
SALMODE  CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
***      MVC   MID1+56(18),=C'***MARKET TOTAL***'                               
         MVC   MID1+56(19),=C'***NETWORK TOTAL***'                              
         CLI   MODE,PRDLAST                                                     
         BNE   SALDPT1                                                          
         MVC   MID1+56(19),=C'***PRODUCT TOTAL***'                              
         MVI   FORCEHED,C'Y'                                                    
SALDPT1  DS    0H                                                               
         CLI   SUMOSPOT,NOSETQ     SUPPRESS TELECASTS                           
         BE    SALSUMA              YES                                         
         MVI   MID1,0                                                           
         MVI   FORCEMID,C'Y'                                                    
         MVC   MID2+29(09),=C'NO TLCSTS'                                        
         CLI   QMED,C'R'                                                        
         BE    *+8                                                              
         CLI   QMED,C'X'                                                        
         BNE   *+10                                                             
         MVC   MID2+29(10),=C'NO BRDCSTS'                                       
SALSUMA  CLI   SUMODEMO,NOSETQ     SUPPRESS DEMOS                               
         BE    SALSUMB              YES                                         
         LA    R5,MID2                                                          
         USING PSALLIND,R5                                                      
         ICM   R6,15,NODEMS                                                     
         BZ    SALSUMB                                                          
         LA    R1,DNAME1                                                        
SALSUMA1 MVC   PSALRTG,0(R1)                                                    
         CLI   0(R1),EXTDEMOQ                                                   
         BE    *+8                                                              
         CLI   0(R1),RTGDEMOQ                                                   
         BE    *+16                                                             
         MVC   PSALAUD,0(R1)                                                    
         MVC   PSALAUDT,=C'(000)'                                               
         LA    R1,L'DNAME1(R1)                                                  
         LA    R5,L'PSALDATA(R5)                                                
         BCT   R6,SALSUMA1                                                      
         DROP  R5                                                               
*                                                                               
SALSUMB  CLI   SUMODOL,NOSETQ      SUPPRESS DOLLARS                             
         BE    *+10                 YES                                         
         MVC   MID2+110(7),=C'DOLLARS'                                          
         XC    SALSDATA,SALSDATA                                                
SALSUM1  GOTO1 VGETBUF                                                          
         LA    R3,MYBUFIO                                                       
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTS            
         BH    SALSUM1A                                                         
         CLI   SUMSLN,X'FD'        SPILL DATA                                   
         BE    SALSUM1                                                          
         CLI   SUMSLN,X'FE'        ORIG DATA                                    
         BE    SALSUM1                                                          
SALSUM1A DS    0H'0'                                                            
         CLI   SUMSLN,X'FD'        SET SPILL MID LINE                           
         BNE   *+10                                                             
         MVC   MID1+48(9),=C'***SPILL '                                         
         CLI   SUMSLN,X'FE'        SET ORIG MID LINE                            
         BNE   *+10                                                             
         MVC   MID1+48(9),=C'***ORIG. '                                         
         CLI   SUMRTYP,0                                                        
         BE    SALSUMX                                                          
         CLI   SUMRTYP,2                                                        
         BE    SALSUM1                                                          
         CLI   SUMRTYP,3           TOTAL RECORD                                 
         BE    SALSUM2              YES - PRINT SUMMARY                         
         OC    SUMSPOTS,SUMSPOTS   ANY SPOTS IN WEEK                            
         BZ    SALSUM1              NO - BYPASS RECORD                          
         L     RE,SALSWKS           YES - ACCUMMULATE WEEKLY AVERAGE            
         LA    RE,1(RE)                                                         
         ST    RE,SALSWKS                                                       
         L     RE,SALSSPT                                                       
         A     RE,SUMSPOTS                                                      
         ST    RE,SALSSPT                                                       
         L     RE,SALSD1                                                        
         A     RE,SUMD1                                                         
         ST    RE,SALSD1                                                        
         L     RE,SALSD2                                                        
         A     RE,SUMD2                                                         
         ST    RE,SALSD2                                                        
         L     RE,SALSD3                                                        
         A     RE,SUMD3                                                         
         ST    RE,SALSD3                                                        
         L     RE,SALSD4                                                        
         A     RE,SUMD4                                                         
         ST    RE,SALSD4                                                        
         L     RE,SALSDL                                                        
         A     RE,SUMDL                                                         
         ST    RE,SALSDL                                                        
         B     SALSUM1                                                          
*                                                                               
SALSUM2  OC    SALSWKS,SALSWKS                                                  
         BZ    SALSUM1                                                          
         LA    R5,SALSSPT                                                       
         LA    R6,6                #SALSDATA ITEMS (SPOT+DOLLAR+4*DEMO)         
         CLI   SUMDPGNO,X'FF'                                                   
         BNE   SALSUM2A                                                         
         MVI   MID1,0                                                           
         MVI   FORCEMID,C'Y'                                                    
         MVC   MID2(7),=C'MGR TOT'                                              
         CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
***      MVC   MID2(7),=C'MKT TOT'                                              
         MVC   MID2(7),=C'NWK TOT'                                              
         CLI   MODE,PRDLAST                                                     
         BNE   *+10                                                             
         MVC   MID2(7),=C'PRD TOT'                                              
         B     SALSUM3                                                          
*                                                                               
SALSUM2A MVC   MID2(3),SUMDPART                                                 
         EDIT  (1,SUMSLN),(3,MID2+4)                                            
         MVI   MID2+3,C'-'                                                      
         MVI   FORCEMID,C'Y'                                                    
         MVI   MID1,0                                                           
SALSUM3  L     RF,0(R5)            CALCULATE WEEKLY AVERAGES                    
         LTR   RF,RF                                                            
         BM    SALSUM3A                                                         
         SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,SALSWKS                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R5)                                                         
SALSUM3A LA    R5,4(R5)                                                         
         BCT   R6,SALSUM3                                                       
*                                                                               
* PRINT SALESPERSONS SUMMARY                                                    
         LA    R5,P1                                                            
         USING PSALLIND,R5                                                      
         MVC   PSALROW(5),=C'TOTAL'                                             
         CLI   SUMOSPOT,NOSETQ                                                  
         BE    SALSUM10                                                         
         EDIT  (4,SUMSPOTS),PSALSPOT                                            
SALSUM10 CLI   SUMODOL,NOSETQ                                                   
         BE    SALSUM11                                                         
         CLI   HADOFLOW,C'Y'                                                    
         BNE   *+14                                                             
         MVC   PSALDOL+L'PSALDOL-5(5),=C'*****'   $ TOTALLING OVERFLOW          
         B     SALSUM11                                                         
         EDIT  (4,SUMDL),PSALDOL,2,FLOAT=$                                      
SALSUM11 CLI   SUMODEMO,NOSETQ                                                  
         BE    SALSUM15                                                         
         L     R6,NODEMS                                                        
         LA    R1,SUMD1                                                         
SALSUM12 EDIT  (4,0(R1)),PSALDEMO,1                                             
         LA    R1,SUMDMNLQ(R1)                                                  
         LA    R5,L'PSALDATA(R5)                                                
         BCT   R6,SALSUM12                                                      
SALSUM15 GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         MVI   ALLOWLIN,0                                                       
*                                                                               
         LA    R5,P1                                                            
         MVC   PSALROW(8),=C'WKLY AVG'                                          
         CLI   SUMOSPOT,NOSETQ                                                  
         BE    SALSUM20                                                         
         EDIT  (4,SALSSPT),PSALSPOT                                             
SALSUM20 CLI   SUMODOL,NOSETQ                                                   
         BE    SALSUM21                                                         
         CLI   HADOFLOW,C'Y'                                                    
         BNE   *+14                                                             
         MVC   PSALDOL+L'PSALDOL-5(5),=C'*****'   $ TOTALLING OVERFLOW          
         B     SALSUM21                                                         
         EDIT  (4,SALSDL),PSALDOL,2,FLOAT=$                                     
SALSUM21 CLI   SUMODEMO,NOSETQ                                                  
         BE    SALSUM25                                                         
         L     R6,NODEMS                                                        
         LA    R1,SALSD1                                                        
SALSUM22 EDIT  (4,0(R1)),PSALDEMO,1                                             
         LA    R1,L'SALSD1(R1)                                                  
         LA    R5,L'PSALDATA(R5)                                                
         BCT   R6,SALSUM22                                                      
SALSUM25 GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         MVI   ALLOWLIN,6                                                       
SALSUMX  MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVI   FORCEMID,C'N'                                                    
         XC    SALSDATA,SALSDATA                                                
         CLI   SUMRTYP,0                                                        
         BE    *+12                                                             
         CLI   SUMDPGNO,X'FF'                                                   
         BNE   SALSUM1                                                          
         XIT1                                                                   
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* BRSSSUM                                                            *          
* - ??? NOT APPEAR TO BE REQUESTABLE !!!                             *          
* - CONTROLLED BY PROFMTR (SETTING=3)                                *          
**********************************************************************          
         SPACE                                                                  
BRSSUM   NTR1  BASE=*,LABEL=*                                                   
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SPN5WK,R2                                                        
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         OC    SUMOPTS,SUMOPTS                                                  
         BZ    BRSSUMX                                                          
         SPACE 2                                                                
*RECREATE TOTAL MEDTAB                                                          
         MVC   MEDNUMWK,=F'56'                                                  
         MVC   MEDNUMMO,=F'0'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVI   BUFHI,1                                                          
         MVI   BUFRTYP,1                                                        
         GOTO1 VGETBUF                                                          
         LA    R3,MYBUFIO                                                       
         MVI   BUFHI,1                                                          
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTSALS         
         BH    BRSMKT                                                           
         CLI   BUFCDE,X'88'        SPILL DATA                                   
         BE    BRSSUMX                                                          
         CLI   BUFCDE,X'89'        ORIG DATA                                    
         BE    BRSSUMX                                                          
BRSMKT   CLI   MODE,STALAST                                                     
         BNE   BRSMODE                                                          
         MVC   P+56(19),=C'***STATION TOTAL***'                                 
         CLI   CONEND,C'Y'         CAN THIS BE A CONTRACT TOTAL                 
         BNE   BRSMODE             NO                                           
         CLI   QBYID,C'Y'                                                       
         BNE   BRSMODE                                                          
         MVC   P+38(3),=C'***'                                                  
         MVC   P+41(12),BUYIDNAM                                                
         MVC   P+54(12),BUYID                                                   
         GOTO1 SQUASHER,DMCB,P+38,38                                            
BRSMODE  CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
***      MVC   P+56(18),=C'***MARKET TOTAL***'                                  
         MVC   P+56(19),=C'***NETWORK TOTAL***'                                 
         CLI   MODE,PRDLAST                                                     
         BNE   BRSDPT                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+56(19),=C'***PRODUCT TOTAL***'                                 
BRSDPT   DS    0H                                                               
         CLI   SUMSLN,X'FD'        SET SPILL MID LINE                           
         BNE   *+10                                                             
         MVC   P+48(9),=C'***SPILL '                                            
         CLI   SUMSLN,X'FE'        SET ORIG. MID LINE                           
         BNE   *+10                                                             
         MVC   P+48(9),=C'***ORIG. '                                            
         L     R5,MEDAFRST         CLEAR MEDBLOCK                               
         LA    R6,MEDPERD                                                       
CLRBRS   L     RE,4(R5)                                                         
         LTR   RE,RE                                                            
         BZ    CLRBRS2                                                          
         L     RF,MEDLCHNK                                                      
         XCEF                                                                   
CLRBRS2  LA    R5,12(R5)                                                        
         OC    4(4,R5),4(R5)                                                    
         BNZ   *+10                                                             
         XC    0(12,R5),0(R5)                                                   
         CR    R5,R6                                                            
         BH    CLRBRS3                                                          
         B     CLRBRS                                                           
*                                                                               
CLRBRS3  DS    0H                                                               
BRSSUM1  GOTO1 VGETBUF                                                          
         LA    R3,MYBUFIO                                                       
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTSALS         
         BH    BRSSUM1A                                                         
         CLI   SUMSLN,X'FD'        SPILL DATA                                   
         BE    BRSSUM1                                                          
         CLI   SUMSLN,X'FE'        ORIG DATA                                    
         BE    BRSSUM1                                                          
BRSSUM1A DS    0H'0'                                                            
         L     R5,MEDAFRST                                                      
         CLI   SUMRTYP,0                                                        
         BE    BRSSUMX                                                          
         CLI   SUMRTYP,2           BYPASS MONTHLY                               
         BE    BRSSUM1                                                          
         CLI   SUMRTYP,3           SET FOR PERIOD                               
         BNE   BRSSUM2                                                          
         LA    R5,MEDPERD                                                       
         B     BRSSUM3                                                          
BRSSUM2  CLC   2(2,R5),SUMDT+2     FIND CORRECT BUFFER SLOT                     
         BE    BRSSUM3                                                          
         LA    R5,12(R5)                                                        
         B     BRSSUM2                                                          
BRSSUM3  L     R4,4(R5)            INSERT DATA INTO MEDBLOCK                    
         MVC   MEDGLD,SUMGDL                                                    
         MVC   MEDGL1,SUMGD1                                                    
         MVC   MEDBYSPT,SUMSPOTS                                                
         MVC   MEDBY1,SUMD1                                                     
         MVC   MEDBYD,SUMDL                                                     
         CLI   SUMRTYP,3           PERIOD RECORD                                
         BE    *+8                                                              
         B     BRSSUM1                                                          
*                                                                               
* PRINT BRS SUMMARY                                                             
         L     R5,MEDAFRST                                                      
BRSSUM4  L     R4,4(R5)                                                         
         LTR   R4,R4                                                            
         BZ    BRSSUM8A                                                         
         LA    R8,15                                                            
         LA    R9,P2+11                                                         
         MVC   P2(7),=C'MKT TOT'                                                
BRSSUM41 MVC   P2(7),=C'*TOTAL*'                                                
         MVI   ALLOWLIN,10                                                      
         CLI   SUMDPGRP,X'FF'                                                   
         BE    BRSSUM4A                                                         
         MVC   P2(3),SUMDPART                                                   
         EDIT  (1,SUMSLN),(3,P2+4)                                              
         MVI   P2+3,C'-'                                                        
BRSSUM4A MVI   P3,0                                                             
         LA    R6,P4                                                            
         CLI   SUMOSPOT,NOSETQ                                                  
         BE    BRSSUM4B                                                         
         MVC   0(9,R6),=C'NO TLCSTS'                                            
         CLI   QMED,C'R'                                                        
         BE    *+8                                                              
         CLI   QMED,C'X'                                                        
         BNE   *+10                                                             
         MVC   0(9,R6),=C'NO BRDCSTS'                                           
         LA    R6,132(R6)                                                       
BRSSUM4B CLI   SUMODEMO,NOSETQ                                                  
         BE    *+14                                                             
         MVC   0(7,R6),DNAME1                                                   
         LA    R6,132(R6)                                                       
         CLI   SUMODOL,NOSETQ                                                   
         BE    *+14                                                             
         MVC   0(7,R6),=C'DOLLARS'                                              
         LA    R6,132(R6)                                                       
         CLI   SUMODEMO,NOSETQ                                                  
         BE    *+14                                                             
         MVC   0(9,R6),=C'GOAL DEMO'                                            
         LA    R6,132(R6)                                                       
         CLI   SUMODOL,NOSETQ                                                   
         BE    *+10                                                             
         MVC   0(6,R6),=C'GOAL $'                                               
BRSSUM5  LA    R6,MEDPERD                                                       
         OC    0(4,R5),0(R5)       SLOT ACTIVE                                  
         BZ    BRSSUM8A             NO - BYPASS                                 
         CR    R5,R6                                                            
         BL    BRSSUM6                                                          
         BH    BRSSUM9                                                          
         MVC   WORK(5),=C'TOTAL'                                                
         B     BRSSUM7                                                          
*                                                                               
BRSSUM6  GOTO1 DATCON,DMCB,(X'02',(R5)),(X'04',WORK)                            
BRSSUM7  DS    0H                                                               
         OC    MEDGLD,MEDGLD                                                    
         BNZ   *+14                                                             
         OC    MEDBYSPT,MEDBYSPT                                                
         BZ    BRSSUM8A                                                         
         MVC   3(5,R9),WORK                                                     
         ST    R9,PRTADDR                                                       
         LA    R9,132(R9)                                                       
         LA    R9,132(R9)                                                       
         CLI   SUMOSPOT,NOSETQ                                                  
         BE    BRSSUM7A                                                         
         EDIT  (4,MEDBYSPT),(5,3(R9))                                           
         LA    R9,132(R9)                                                       
BRSSUM7A CLI   SUMODEMO,NOSETQ                                                  
         BE    BRSSUM7B                                                         
         L     RF,MEDBY1                                                        
         SR    RE,RE                                                            
         SLL   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
         LA    R9,132(R9)                                                       
BRSSUM7B CLI   SUMODOL,NOSETQ                                                   
         BE    BRSSUM7C                                                         
         L     RF,MEDBYD                                                        
         SR    RE,RE                                                            
         SLL   RF,1                                                             
         D     RE,=F'100'                                                       
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
         LA    R9,132(R9)                                                       
BRSSUM7C CLI   SUMODEMO,NOSETQ                                                  
         BE    BRSSUM7D                                                         
         L     RF,MEDGL1                                                        
         SR    RE,RE                                                            
         SLL   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
         LA    R9,132(R9)                                                       
BRSSUM7D CLI   SUMODOL,NOSETQ                                                   
         BE    BRSSUM8                                                          
         L     RF,MEDGLD                                                        
         SR    RE,RE                                                            
         SLL   RF,1                                                             
         D     RE,=F'100'                                                       
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
*                                                                               
BRSSUM8  L     R9,PRTADDR          SET NEXT SLOT                                
         LA    R9,8(R9)                                                         
BRSSUM8A LA    R5,12(R5)                                                        
         L     R4,4(R5)                                                         
         LA    R6,MEDPERD                                                       
         CR    R5,R6                                                            
         BH    BRSSUM9                                                          
         OC    0(4,R5),0(R5)                                                    
         BZ    BRSSUM8A                                                         
         LA    R6,MEDMON01                                                      
         BCTR  R6,0                                                             
         CR    R5,R6                                                            
         BH    *+12                                                             
         BCT   R8,BRSSUM5                                                       
         B     BRSSUM9                                                          
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         BCT   R8,BRSSUM5                                                       
*                                                                               
BRSSUM9  GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         MVI   ALLOWLIN,0                                                       
         LA    R6,MEDPERD                                                       
         CR    R5,R6                                                            
         BNH   BRSSUM4                                                          
         B     BRSDPT                                                           
BRSSUMX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* BTS SUMMARY                                                        *          
* - MONTHLY ANALYSIS (TOTALS BY MONTH AND REPORTING PERIOD)          *          
* - CONTROLLED BY PROFMTR (SETTING=2)                                *          
**********************************************************************          
         SPACE                                                                  
BTSSUM   NTR1  BASE=*,LABEL=*                                                   
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SPN5WK,R2                                                        
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         USING PBTSLIND,R5                                                      
         OC    SUMOPTS,SUMOPTS                                                  
         BZ    BTSSUMX                                                          
*                                                                               
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTSALS         
         BH    BTSMKT                                                           
         CLI   BUFCDE,X'88'        SPILL DATA                                   
         BE    BTSSUMX                                                          
         CLI   BUFCDE,X'89'        ORIG DATA                                    
         BE    BTSSUMX                                                          
BTSMKT   MVI   BUFHI,1                                                          
         MVI   BUFRTYP,2                                                        
         GOTO1 VGETBUF                                                          
         LA    R3,MYBUFIO                                                       
         CLI   SUMRTYP,0                                                        
         BE    BTSSUMX                                                          
*                                                                               
         MVI   P1,0                                                             
         MVI   BUFHI,1             SET FOR READ HI                              
         MVI   BUFRTYP,2                                                        
         CLI   MODE,STALAST                                                     
         BNE   BTSMODE                                                          
         MVC   P+56(19),=C'***STATION TOTAL***'                                 
         CLI   CONEND,C'Y'         CAN THIS BE A CONTRACT TOTAL                 
         BNE   BTSMODE             NO                                           
         CLI   QBYID,C'Y'                                                       
         BNE   BTSMODE                                                          
         MVC   P+38(3),=C'***'                                                  
         MVC   P+41(12),BUYIDNAM                                                
         MVC   P+54(12),BUYID                                                   
         GOTO1 SQUASHER,DMCB,P+38,38                                            
BTSMODE  CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
***      MVC   P+56(18),=C'***MARKET TOTAL***'                                  
         MVC   P+56(19),=C'***NETWORK TOTAL***'                                 
         CLI   MODE,PRDLAST                                                     
         BNE   BTSDPT                                                           
         MVC   P+56(19),=C'***PRODUCT TOTAL***'                                 
         CLI   MODE,CLTLAST                                                     
         BNE   BTSDPT                                                           
         MVC   P+56(18),=C'***CLIENT TOTAL***'                                  
BTSDPT   DS    0H                                                               
         CLI   SUMSLN,X'FD'        SET SPILL MID LINE                           
         BNE   *+10                                                             
         MVC   P+48(9),=C'***SPILL '                                            
         CLI   SUMSLN,X'FE'        SET ORIG. MID LINE                           
         BNE   *+10                                                             
         MVC   P+48(9),=C'***ORIG. '                                            
         MVI   DPTSW,1                                                          
         MVI   ALLOWLIN,17                                                      
         L     RE,ACTMO                                                         
         LA    RE,5(RE)                                                         
         STC   RE,ALLOWLIN                                                      
         GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         MVI   ALLOWLIN,0                                                       
*                                  *** COLUMN HEADINGS ***                      
         LA    R5,MID1                                                          
         MVI   FORCEMID,C'Y'                                                    
         CLI   SUMOSPOT,NOSETQ                                                  
         BE    BTSSUMA                                                          
         MVC   PBTSSPOT+1(9),=C'NO TLCSTS'                                      
         CLI   QMED,C'R'                                                        
         BE    *+8                                                              
         CLI   QMED,C'X'                                                        
         BNE   *+10                                                             
         MVC   PBTSSPOT+1(9),=C'NO BRCSTS'                                      
         MVC   L'MID1+PBTSSPOT+1(9),DASH                                        
BTSSUMA  CLI   SUMODOL,NOSETQ                                                   
         BE    BTSSUMB                                                          
         MVC   PBTSDOL+1(7),=C'DOLLARS'                                         
         MVC   L'MID1+PBTSDOL+1(7),DASH                                         
BTSSUMB  CLI   SUMODEMO,NOSETQ                                                  
         BE    BTSSUMC                                                          
         LA    RF,DNAME1                                                        
         L     R0,NODEMS                                                        
BTSSUMB1 CLI   0(RF),0                                                          
         BE    BTSSUMC                                                          
         MVC  PBTSDATA+((L'PBTSDATA-L'DNAME1)/2)(L'DNAME1),0(RF) CENTRE         
         MVC   L'MID1+PBTSDATA(L'PBTSDATA-3),DASH                               
         MVC   L'MID1+PBTSDEMO+(L'PBTSDEMO-5)(4),=C'IMPS'                       
         MVC   L'MID1+PBTSCPM+(L'PBTSCPM-4)(3),=C'CPM'                          
         LA    R5,L'PBTSDATA(R5)                                                
         LA    RF,L'DNAME1(RF)                                                  
         BCT   R0,BTSSUMB1                                                      
*                                  *** COLUMN DATA ***                          
BTSSUMC  MVC   P1(5),=C'MONTH'                                                  
BTSSUM1  GOTO1 VGETBUF                                                          
         LA    R3,MYBUFIO                                                       
         CLI   SUMRTYP,0                                                        
         BE    BTSSUMX                                                          
         CLI   SUMRTYP,1                                                        
         BE    BTSSUM1                                                          
         OC    SUMSPOTS,SUMSPOTS                                                
         BZ    BTSSUM1                                                          
         CLI   DPTSW,1                                                          
         BNE   BTSSUM1A                                                         
         MVC   P1+7(3),=C'TOT'                                                  
         MVC   MID1(7),=C'*TOTAL*'                                              
         CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
***      MVC   MID1(7),=C'MKT TOT'                                              
         MVC   MID1(7),=C'NWK TOT'                                              
         CLI   MODE,PRDLAST                                                     
         BNE   *+10                                                             
         MVC   MID1(7),=C'PRD TOT'                                              
         CLI   SUMDPGNO,X'FF'                                                   
         BE    BTSSUM1A                                                         
         MVC   MID1(3),SUMDPART                                                 
         MVI   MID1+3,C'-'                                                      
         EDIT  (1,SUMSLN),(3,MID1+4)                                            
BTSSUM1A DS    0H                                                               
         MVI   DPTSW,0                                                          
         LA    R5,P                                                             
         MVC   PBTSMNTH,=C'TOT'                                                 
         CLI   SUMRTYP,3                                                        
         BE    BTSSUM2                                                          
         GOTO1 DATCON,DMCB,(X'02',SUMDT+2),(X'04',WORK)                         
         MVC   PBTSMNTH,WORK                                                    
BTSSUM2  CLI   SUMOSPOT,NOSETQ                                                  
         BE    BTSSUM2B                                                         
         EDIT  SUMSPOTS,(10,PBTSSPOT)                                           
BTSSUM2B CLI   SUMODOL,NOSETQ                                                   
         BE    BTSSUM2C                                                         
         CLI   HADOFLOW,C'Y'                                                    
         BNE   *+14                                                             
         MVC   PBTSDOL(5),=C'*****'                                             
         B     BTSSUM2C                                                         
         L     R9,SUMDL                                                         
         SR    R8,R8                                                            
         SLA   R9,1                                                             
         D     R8,=F'100'                                                       
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         EDIT  (R9),(9,PBTSDOL),MINUS=YES                                       
         SPACE 2                                                                
BTSSUM2C MVC   FULL,=F'1'          SET SPOT COUNT FOR NOT AVERAGING             
         MVC   WEIGHT,=F'1'                                                     
         GOTO1 VCALCPP,DMCB,FULL                                                
         LA    R4,SVDEMS                                                        
         L     R6,NODEMS                                                        
BTSSUM3  ICM   R8,15,0(R4)         GET DEMO VALUE                               
         BZ    BTSSUM5                                                          
         CLI   SUMODEMO,NOSETQ                                                  
         BE    BTSSUM7                                                          
         C     R8,=F'99999'        DECIMAL PRECISION OK                         
         BH    BTSSUM4              NO - DIVIDE BY 10                           
         EDIT  (R8),PBTSDEMO,1      YES - MAINTAIN PRECISION                    
         B     BTSSUM5                                                          
BTSSUM4  SRDA  R8,32               DROP DECIMAL PRECISION                       
         SLA   R9,1                                                             
         D     R8,=F'10'                                                        
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         EDIT  (R9),PBTSDEMO                                                    
*                                                                               
BTSSUM5  ICM   R8,15,SVD1CP-SVD1(R4)    SHOW ANY CPM IF REQUIRED                
         BZ    BTSSUM6                                                          
         CLI   SUMODOL,NOSETQ                                                   
         BE    BTSSUM6                                                          
         CLI   HADOFLOW,C'Y'                                                    
         BNE   *+14                                                             
         MVC   PBTSCPM(5),=C'*****'                                             
         B     BTSSUM6                                                          
         EDIT  (R8),PBTSCPM,2                                                   
         CLC   SUMDL,SUMDLEQ                                                    
         BE    *+8                                                              
         MVI   PBTSCPME,C'+'                                                    
         C     R8,=F'99999'                                                     
         BNH   BTSSUM6                                                          
         SRDA  R8,32                                                            
         SLA   R9,1                                                             
         D     R8,=F'10'                                                        
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         EDIT  (R9),PBTSCPM,1                                                   
BTSSUM6  LA    R4,SVDEMNLQ(R4)                                                  
         LA    R5,L'PBTSDATA(R5)                                                
         BCT   R6,BTSSUM3                                                       
*                                                                               
BTSSUM7  GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         CLI   SUMRTYP,3                                                        
         BNE   BTSSUM1                                                          
         CLI   SUMDPGNO,X'FF'                                                   
         BE    BTSSUMX                                                          
         B     BTSDPT                                                           
*                                                                               
BTSSUMX  MVI   FORCEMID,C'N'                                                    
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         XIT1                                                                   
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* BDS SUMMARY                                                        *          
* - WEEKLY ANALYSIS OF DEMOS                                         *          
* - CONTROLLED BY PROFMTR (SETTING=4)                                *          
**********************************************************************          
         SPACE                                                                  
BDSSUM   NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         OC    SUMOPTS,SUMOPTS                                                  
         BZ    BDSSUMX                                                          
         SPACE 2                                                                
* RECREATE TOTAL MEDTAB                                                         
         MVC   MEDNUMWK,=F'56'                                                  
         MVC   MEDNUMMO,=F'12'                                                  
         MVC   MEDNUMQT,=F'0'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVI   BUFHI,1                                                          
         MVI   BUFRTYP,1                                                        
         CLI   MODE,STALAST                                                     
         BNE   BDSMODE                                                          
         MVC   P+56(19),=C'***STATION TOTAL***'                                 
         CLI   CONEND,C'Y'         CAN THIS BE A CONTRACT TOTAL                 
         BNE   BDSMODE                                                          
         CLI   QBYID,C'Y'                                                       
         BNE   BDSMODE                                                          
         MVC   P+38(3),=C'***'                                                  
         MVC   P+41(12),BUYIDNAM                                                
         MVC   P+54(12),BUYID                                                   
         GOTO1 SQUASHER,DMCB,P+38,23                                            
BDSMODE  CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
*        MVC   P+56(18),=C'***MARKET TOTAL***'                                  
         MVC   P+56(19),=C'***NETWORK TOTAL***'                                 
BDSTEMP  CLI   MODE,PRDLAST                                                     
         BNE   *+14                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+56(19),=C'***PRODUCT TOTAL***'                                 
BDSDPT   DS    0H                                                               
         L     R5,MEDAFRST         CLEAR MEDBLOCK                               
         LA    R6,MEDPERD                                                       
CLRBDS   L     RE,4(R5)                                                         
         LTR   RE,RE                                                            
         BZ    CLRBDS2                                                          
         L     RF,MEDLCHNK                                                      
         XCEF                                                                   
CLRBDS2  LA    R5,12(R5)                                                        
         CR    R5,R6                                                            
         BH    CLRBDS3                                                          
         B     CLRBDS                                                           
*                                                                               
CLRBDS3  DS    0H                                                               
         L     R5,MEDAFRST                                                      
BDSSUM1  GOTO1 VGETBUF                                                          
         LA    R3,MYBUFIO                                                       
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTALS          
         BH    BDSSUM11                                                         
         CLI   SUMSLN,X'FD'        SPILL DATA                                   
         BE    BDSSUM1                                                          
         CLI   SUMSLN,X'FE'        ORIG DATA                                    
         BE    BDSSUM1                                                          
BDSSUM11 DS    0H'0'                                                            
         CLI   SUMSLN,X'FD'        SET SPILL MIDLINE                            
         BNE   *+10                                                             
         MVC   P+48(9),=C'***SPILL '                                            
         CLI   SUMSLN,X'FE'        SET ORIG MID LINE                            
         BNE   *+10                                                             
         MVC   P+48(9),=C'***ORIG. '                                            
         CLI   SUMRTYP,0                                                        
         BNE   *+14                                                             
         MVC   P,SPACES            CLEAR MKTLAST DEBRIS SET EARLIER!            
         B     BDSSUMX                                                          
         CLI   SUMRTYP,2           BYPASS MONTHLY                               
         BE    BDSSUM1                                                          
         CLI   SUMRTYP,3           SET FOR PERIOD                               
         BNE   BDSSUM2                                                          
         LA    R5,MEDPERD                                                       
         B     BDSSUM3                                                          
BDSSUM2  CLC   2(2,R5),SUMDT+2     FIND CORRECT BUFFER CSLOT                    
         BE    BDSSUM3                                                          
         LA    R5,12(R5)                                                        
         LA    RE,MEDPERD                                                       
         CR    R5,RE                                                            
         BH    BDSSUM1                                                          
         B     BDSSUM2                                                          
*                                                                               
BDSSUM3  L     R4,4(R5)            INSERT DATA INTO MEDBLOCK                    
         MVC   MEDBYSPT,SUMSPOTS                                                
         MVC   MEDBYD,SUMDL                                                     
         MVC   MEDBY1,SUMD1                                                     
         MVC   MEDBY2,SUMD2                                                     
         MVC   MEDBY3,SUMD3                                                     
         MVC   MEDBY4,SUMD4                                                     
         MVC   MEDBY5,SUMD5                                                     
         MVC   MEDBY6,SUMD6                                                     
         MVC   MEDBY7,SUMD7                                                     
         MVC   MEDBY8,SUMD8                                                     
         CLI   SUMRTYP,3           PERIOD RECORD                                
         BE    *+8                                                              
         B     BDSSUM1                                                          
*                                                                               
* PRINT SUMMARY                                                                 
         L     R5,MEDAFRST                                                      
BDSSUM4  L     R4,4(R5)                                                         
         LTR   R4,R4                                                            
         BZ    BDSSUM8A                                                         
         LA    R8,15                                                            
         LA    R9,P2+11                                                         
         CLI   SUMDPGRP,X'FF'                                                   
         BE    BDSSUM4A                                                         
         MVC   P2(3),SUMDPART                                                   
         EDIT  (1,SUMSLN),(3,P2+4)                                              
         MVI   P2+3,C'-'                                                        
BDSSUM4A MVI   P3,0                                                             
         LA    R6,P4                                                            
         MVI   ALLOWLIN,10                                                      
         CLI   SUMOSPOT,NOSETQ                                                  
         BE    *+14                                                             
         MVC   0(7,R6),=C'  SPOTS'                                              
         LA    R6,L'P(R6)                                                       
         CLI   SUMODOL,NOSETQ                                                   
         BE    *+14                                                             
         MVC   0(7,R6),=C'   COST'                                              
         LA    R6,L'P(R6)                                                       
         CLI   SUMODEMO,NOSETQ                                                  
         BE    BDSSUM5                                                          
         L     R0,NODEMS                                                        
         LA    RF,DNAMES                                                        
BDSSUM4B CLI   0(RF),0                                                          
         BE    BDSSUM5                                                          
         MVC   0(L'DNAME1,R6),0(RF)                                             
         LA    RF,L'DNAME1(RF)                                                  
         LA    R6,L'P(R6)                                                       
         BCT   R0,BDSSUM4B                                                      
         SPACE 2                                                                
BDSSUM5  LA    R6,MEDPERD                                                       
         OC    0(4,R5),0(R5)       SLOT ACTIVE                                  
         BZ    BDSSUM8A             NO - BYPASS                                 
         CR    R5,R6                                                            
         BL    BDSSUM6                                                          
         BH    BDSSUM9                                                          
         MVC   3(5,R9),=C'TOTAL'                                                
         B     BDSSUM7                                                          
*                                                                               
BDSSUM6  GOTO1 DATCON,DMCB,(X'02',(R5)),(X'04',WORK)                            
         MVC   3(5,R9),WORK                                                     
BDSSUM7  ST    R9,PRTADDR                                                       
         OC    MEDBYSPT,MEDBYSPT                                                
         BNZ   *+12                                                             
         LA    R8,1(R8)                                                         
         B     BDSSUM8A                                                         
         LA    R9,L'P(R9)                                                       
         LA    R9,L'P(R9)                                                       
         CLI   SUMOSPOT,NOSETQ     SPOTS                                        
         BE    BDSSUM7A                                                         
         EDIT  (4,MEDBYSPT),(5,3(R9))                                           
         LA    R9,L'P(R9)                                                       
BDSSUM7A CLI   SUMODOL,NOSETQ      COST                                         
         BE    BDSSUM7B                                                         
         CLI   HADOFLOW,C'Y'                                                    
         BNE   *+14                                                             
         MVC   0(5,R9),=C'*****'                                                
         B     BDSSM7A1                                                         
         L     RF,MEDBYD                                                        
         SR    RE,RE                                                            
         SLL   RF,1                                                             
         D     RE,=F'100'                                                       
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
BDSSM7A1 LA    R9,L'P(R9)                                                       
*                                                                               
BDSSUM7B CLI   SUMODEMO,NOSETQ     DEMOS                                        
         BE    BDSSUM8                                                          
         L     R1,NODEMS                                                        
         LA    R6,MEDBY1                                                        
BDSSUM7C L     RF,0(R6)                                                         
         SR    RE,RE                                                            
         SLL   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
         LA    R9,L'P(R9)                                                       
         LA    R6,L'MEDBY1+L'MEDBY1EQ(R6)                                       
         BCT   R1,BDSSUM7C                                                      
         SPACE 2                                                                
BDSSUM8  L     R9,PRTADDR          SET NEXT SLOT                                
         LA    R9,8(R9)                                                         
BDSSUM8A LA    R5,12(R5)                                                        
         L     R4,4(R5)                                                         
         LA    R6,MEDPERD                                                       
         CR    R5,R6                                                            
         BH    BDSSUM9                                                          
         OC    0(4,R5),0(R5)                                                    
         BZ    BDSSUM8A                                                         
         LA    R6,MEDMON01                                                      
         BCTR  R6,0                                                             
         CR    R5,R6                                                            
         BH    *+12                                                             
         BCT   R8,BDSSUM5                                                       
         B     BDSSUM9                                                          
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         BCT   R8,BDSSUM5                                                       
*                                                                               
BDSSUM9  GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         MVI   ALLOWLIN,0                                                       
         LA    R6,MEDPERD                                                       
         CR    R5,R6                                                            
         BNH   BDSSUM4                                                          
         B     BDSDPT                                                           
*                                                                               
BDSSUMX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CALCULATE CPP                                                       *         
* ENTRY - PARAM1 = SPOT COUNT FOR DEMO AVERAGE                        *         
***********************************************************************         
         SPACE                                                                  
CALCPP   NTR1  BASE=*,LABEL=*                                                   
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING MEDBLOCK,R7                                                      
         USING SUMDSECT,R3                                                      
         XC    SVDEMS(SVDEMSLQ),SVDEMS                                          
         CLI   SPOTPROF+1,C'N'                                                  
         BE    UNWTX                                                            
         CLI   SPOTPROF+1,0                                                     
         BE    UNWTX                                                            
         OC    WEIGHT,WEIGHT                                                    
         BZ    UNWTX                                                            
         LA    R4,DNAMES                                                        
*>>>     LA    R5,14                                                            
         LA    R5,MAXDEMOQ                                                      
         LA    R6,SUMD1                                                         
UNWT1    CLI   SPOTPROF+1,C'D'     UNWEIGHT DEMOS                               
         BE    UNW1                                                             
         CLI   0(R4),EXTDEMOQ                                                   
         BE    *+8                                                              
         CLI   0(R4),RTGDEMOQ                                                   
         BNE   UNWT2                                                            
UNW1     L     RE,0(R6)                                                         
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,WEIGHT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R6)                                                         
         L     RE,4(R6)                                                         
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,WEIGHT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,4(R6)                                                         
UNWT2    LA    R4,7(R4)                                                         
         LA    R6,8(R6)                                                         
         BCT   R5,UNWT1                                                         
*                                                                               
UNWTX    DS    0H                                                               
         LA    R0,MAXDEMOQ                                                      
         LA    R5,SVD1                                                          
         LA    R6,SUMD1                                                         
CALCPP1  L     RE,SUMDL            GET DOLLARS                                  
         CLI   CPPSW,C'D'                                                       
         BNE   *+8                                                              
         L     RE,SUMDLEQ          USE EQUIVALENCED DOLLARS                     
         L     R8,0(R6)            GET DEMOS                                    
         CLI   CPPSW,C'D'                                                       
         BE    *+8                                                              
         L     R8,4(R6)            USE EQUIVALENCED DEMOS                       
         CLI   MODE,PROCBUY                                                     
         BNE   *+12                                                             
         L     RE,SUMDL                                                         
         L     R8,0(R6)                                                         
         LTR   R8,R8                                                            
         BZ    CALCPP2                                                          
         LTR   RE,RE                                                            
         BM    CALCPP2             NEGATIVE COST - NO CPP POSSIBLE              
         SRDA  RE,32                                                            
         LTR   RF,RF                                                            
         BZ    CALCPP2                                                          
         CVD   RF,DUB                                                           
         ZAP   WORK(16),DUB                                                     
         MP    WORK(16),=PL8'20'   SCALE FOR DEMO DECIMAL/ROUNDING              
         CVD   R8,DUB                                                           
         DP    WORK(16),DUB                                                     
         CVB   RF,WORK                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
*        SLA   RF,1                                                             
*        MH    RF,=H'10'           SCALE FOR DEMO DECIMAL                       
*        DR    RE,R8                                                            
*        A     RF,=F'1'                                                         
*        SRA   RF,1                                                             
         ST    RF,SVD1CP-SVD1(R5)  SAVE CPP                                     
         SPACE 2                                                                
CALCPP2  L     R8,0(R6)            CALCULATE DEMO AVERAGES                      
         SRDA  R8,32                                                            
         SLA   R9,1                                                             
         L     RF,0(R1)                                                         
         OC    0(4,RF),0(RF)                                                    
         BZ    CALCPPX                                                          
         D     R8,0(RF)                                                         
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         ST    R9,0(R5)            SAVE DEMO                                    
         LA    R5,SVDEMNLQ(R5)            GET NEXT                              
         LA    R6,8(R6)                                                         
         BCT   R0,CALCPP1                                                       
*                                                                               
         OC    SUMGDL(16),SUMGDL                                                
         BZ    CALCPPX                                                          
         L     RE,SUMGDL                                                        
         CLI   CPPSW,C'D'                                                       
         BNE   *+8                                                              
         L     RE,SUMGDLE                                                       
         L     R8,SUMGD1                                                        
         CLI   CPPSW,C'D'                                                       
         BE    *+8                                                              
         L     RE,SUMGD1E                                                       
         LTR   R8,R8                                                            
         BZ    CALCPPX                                                          
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         DR    RE,R8                                                            
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,4(R5)                                                         
CALCPPX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT POOL BUYSHEET GRID (I.E. ROTATION PORTION)                    *         
*        0 - LENGTH OF GRID BLOCK                                     *         
*       1-3- A(SPWORK)                                                *         
***********************************************************************         
         SPACE                                                                  
PTSGRID  NMOD1 10,PTSGRID                                                       
         L     RA,0(R1)                                                         
         LA    RA,0(RA)                                                         
         LR    R3,RC                                                            
         USING PTSGD,R3                                                         
         MVC   GRIDLEN,0(R1)                                                    
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SPN5WK,R2                                                        
         USING PGRIDD,R6                                                        
         SPACE 2                                                                
* SORT SPOTS INTO DAY/BRAND/SPOT LENGTH/TYPE ORDER                              
         L     R6,=A(PGRID)        SET UP SORT KEYS                             
         LR    RE,R6                                                            
         USING PGSRT1D,RE                                                       
         L     R1,PGNOENT                                                       
         LTR   R1,R1                                                            
         BZ    PTSGRIDX                                                         
         LA    R7,1                                                             
PGCNDSRT XC    PGSORT,PGSORT                                                    
         MVC   PGDS1WK,PGDWK                                                    
         MVC   PGDS1DY,PGDFDAY                                                  
         XI    PGDS1DY,X'FF'                                                    
         MVI   PGDS1SLT,0                                                       
         CLI   PGCNDSW,1           CONDENSE REQUIRED                            
         BE    *+8                  YES - DONT SET SPOT NUMDBER                 
         STC   R7,PGDS1SLT          NO - SET SPOT NUMBER                        
         MVC   PGDSNO,PGDS1SLT                                                  
         MVC   PGDS1BR,PGDSBRN                                                  
         MVC   PGDS1SL,PGDSSLN                                                  
         MVC   PGDS1IND,PGDIND                                                  
         LA    R7,1(R7)            BUMP SPOT NUMBER                             
         LA    RE,PGRIDLQ(RE)                                                   
         LR    R6,RE                                                            
         BCT   R1,PGCNDSRT                                                      
         DROP  RE                                                               
         L     R8,PGNOENT                                                       
         L     R6,=A(PGRID)                                                     
         GOTO1 XSORT,DMCB,(R6),(R8),PGRIDLQ,PGSORTLQ,0                          
         SPACE 2                                                                
* CONDENSE LIKE SPOTS                                                           
         CLI   PGCNDSW,1           CONDENSE - REQUIRED                          
         BNE   PGCNDX               NO - CHECK FORMAT TYPE                      
*              TABLE SORTED NOW CONDENSE                                        
         L     R6,=A(PGRID)                                                     
PGCND1   OC    0(PGRIDLQ,R6),0(R6) END                                          
         BZ    PGCND2                                                           
         MVI   PGDSNO,1            SET SPOT NUMBER TO 1                         
         LA    R6,PGRIDLQ(R6)                                                   
         B     PGCND1                                                           
         SPACE 2                                                                
PGCND2   L     R6,=A(PGRID)                                                     
         LR    RE,R6                                                            
         LR    R4,R6                                                            
PGCND3   LA    R4,PGRIDLQ(R4)      SET POINTER TO NEXT SLOT                     
         OC    0(PGDLN1Q,R4),0(R4)                                              
         BZ    PGCND4                                                           
         CLC   0(L'PGSORT,RE),0(R4)     THIS SLOT TO NEXT                       
         BNE   PGCND3A                                                          
         LR    R6,R4               GET NUMBER OF SPOTS                          
         MVC   HLDNOSP,PGDNOSP                                                  
         LR    R6,RE                    POINT TO ADD SLOT                       
         ZIC   RF,PGDNOSP          BUMP BY NUMBER OF SPOTS                      
         ZIC   R0,HLDNOSP                                                       
         AR    RF,R0                                                            
         STC   RF,PGDSNO                                                        
         STC   RF,PGDNOSP                                                       
         LR    R6,R4                                                            
         MVI   PGSORT,X'FF'             ELIMINATE NEXT ELEMENT                  
         B     PGCND3                                                           
PGCND3A  LR    RE,R4                                                            
         B     PGCND3                                                           
PGCND4   L     R6,=A(PGRID)                                                     
         L     R8,PGNOENT                                                       
         GOTO1 XSORT,DMCB,(R6),(R8),PGRIDLQ,PGSORTLQ,0                          
         L     R6,=A(PGRID)             DELETE DUPS                             
         L     R1,PGNOENT                                                       
PGCND5   CLI   0(R6),0                                                          
         BE    PGCNDX                                                           
         CLI   0(R6),X'FF'                                                      
         BNE   PGCND6                                                           
         XC    0(PGRIDLQ,R6),0(R6)                                              
         BCTR  R1,0                                                             
         ST    R1,PGNOENT                                                       
PGCND6   LA    R6,PGRIDLQ(R6)                                                   
         B     PGCND5                                                           
         SPACE 2                                                                
PGCNDX   DS    0H                                                               
         EJECT                                                                  
* CHECK FOR VARIABLE FORMAT                                                     
         CLI   VARFRMT,1                                                        
         BNE   VAR2                                                             
         LA    R5,1                INITIALIZE SLOT NUMBER                       
         L     R6,=A(PGRID)                                                     
         SPACE 2                                                                
VARF1    OC    PGSORT,PGSORT                                                    
         BZ    VARFX                                                            
         STC   R5,PGLSLOT          SET SLOT NUMBER                              
         CLC   PGRIDLQ(10,R6),PGSORT                                            
         BNE   VARF2                                                            
         LA    R6,PGRIDLQ(R6)                                                   
         B     VARF1                                                            
VARF2    LA    R6,PGRIDLQ(R6)                                                   
         LA    R5,1(R5)                                                         
         B     VARF1                                                            
         SPACE 2                                                                
VAR2     CLI   VARFRMT,2           VARIABLE FORMAT WITH FIXED SLOTS             
         BNE   VARFX                                                            
         LA    R5,1                INITIALIZE SLOT NUMBER                       
         L     R6,=A(PGRID)                                                     
         SPACE 2                                                                
VAR2F1   OC    PGSORT,PGSORT                                                    
         BZ    VARFX                                                            
         STC   R5,PGLSLOT          SET SLOT NUMBER                              
         CLC   PGRIDLQ(2,R6),PGSORT SLOT BY DATE ONLY                           
         BNE   VAR2F2                                                           
         LA    R6,PGRIDLQ(R6)                                                   
         B     VAR2F1                                                           
VAR2F2   LA    R6,PGRIDLQ(R6)                                                   
         LA    R5,1(R5)                                                         
         B     VAR2F1                                                           
         SPACE 2                                                                
VARFX    DS    0H                                                               
         EJECT                                                                  
* SORT INTO SLOT NUMBER ORDER                                                   
         L     R6,=A(PGRID)                                                     
         LR    RE,R6                                                            
         USING PGSRT2D,RE                                                       
         L     R1,PGNOENT                                                       
PGSLTSRT XC    PGSORT,PGSORT                                                    
         LR    RE,R6                                                            
         MVC   PGDS2SLT,PGLSLOT                                                 
         MVC   PGDS2WK,PGDWK                                                    
         MVC   PGDS2DY,PGDFDAY                                                  
         XI    PGDS2DY,X'FF'                                                    
         MVI   PGDS2SNO,0                                                       
         CLI   PGCNDSW,1                                                        
         BE    *+10                                                             
         MVC   PGDS2SNO,PGDSNO                                                  
         MVC   PGDS2BR,PGDSBRN                                                  
         MVC   PGDS2SL,PGDSSLN                                                  
         MVC   PGDS2IND,PGDIND                                                  
         LA    R6,PGRIDLQ(R6)                                                   
         BCT   R1,PGSLTSRT                                                      
         DROP  RE                                                               
         L     R8,PGNOENT                                                       
         L     R6,=A(PGRID)                                                     
         GOTO1 XSORT,DMCB,(R6),(R8),PGRIDLQ,PGSORTLQ,0                          
         EJECT                                                                  
* TABLE SORTED INTO SLOT NUMBER ORDER                                           
* NOW ASSIGN LINE NUMBERS                                                       
PGSLNO   L     R6,=A(PGRID)                                                     
         LA    R5,1                                                             
         LH    RF,NOINGRID                                                      
         ST    RF,PGWMAX                                                        
PGSLNO1  MVC   WORK(L'PGSORT),PGSORT                                            
         STC   R5,PGSUBLI          SET SUB-LINE                                 
         ZIC   RF,PGLSLOT                                                       
         BCTR  RF,0                                                             
         SR    RE,RE                                                            
         D     RE,PGWMAX           GET LINE NUMBER                              
         STC   RF,PGLINNO                                                       
         STC   RE,PGLSLOT                                                       
         LA    R6,PGRIDLQ(R6)                                                   
         CLC   PGSORT(1),WORK                                                   
         BNE   PGSLNO2                                                          
         LA    R5,1(R5)                                                         
         B     PGSLNO1                                                          
         SPACE 2                                                                
PGSLNO2  LA    R5,1                                                             
         OC    PGSORT,PGSORT                                                    
         BNZ   PGSLNO1                                                          
         SPACE 2                                                                
* SORT INTO LINE/SUBLINE/SLOT NUMBER ORDER                                      
         L     R6,=A(PGRID)                                                     
         L     R1,PGNOENT                                                       
PGSRTPR  XC    PGSORT,PGSORT                                                    
         MVC   PGSORT(3),PGLINNO                                                
         LA    R6,PGRIDLQ(R6)                                                   
         BCT   R1,PGSRTPR                                                       
         SPACE 2                                                                
         L     R8,PGNOENT                                                       
         L     R6,=A(PGRID)                                                     
         GOTO1 XSORT,DMCB,(R6),(R8),PGRIDLQ,PGSORTLQ,0                          
         EJECT                                                                  
         L     RE,ADBUY                                                         
         USING BUYREC,RE                                                        
         MVC   PGPRLNO,PGLINNO                                                  
         XC    PRPGWMAX,PRPGWMAX                                                
         BAS   R9,SETMGMS          SET UP MG/MSSD SPACING                       
         MVC   GRIDSLN,BDSEC                                                    
         DROP  RE                                                               
PTSGRDA  MVC   GRIDST,DSTAGRID                                                  
         XC    PGWKCNT,PGWKCNT                                                  
         XC    PGWMAX,PGWMAX                                                    
         OC    OPTRPT,OPTRPT       OPTIONAL REPORT ACTIVE                       
         BZ    PTSGRID1                                                         
         MVI   OPTRMODE,PROCBUY                                                 
         GOTO1 OPTRPT,DMCB,(RA)                                                 
         SPACE 2                                                                
PTSGRID1 L     R4,GRIDST           SET BEGINNING OF BLOCK                       
         ZIC   RF,PGLSLOT                                                       
         ST    RF,PGWKCNT                                                       
         ZIC   RF,GRIDLEN                                                       
         MH    RF,PGWKCNT+2         SET BEGINNING OF THIS SLOT                  
         AR    R4,RF                                                            
PTSGRID2 OC    PGDWK,PGDWK         END                                          
         BZ    PTSGRIDX                                                         
         CLC   PGLINNO(2),PGPRLNO  SAME LINE/SUBLINE                            
         BE    PTSGRID3                                                         
         MVC   PGPRLNO,PGLINNO                                                  
         BAS   R9,SETMGMS          SET UP MG/MSSD SPACING                       
         L     RE,PGWMAX            YES - SET NEXT GRID                         
         XC    PGWMAX,PGWMAX                                                    
         A     RE,PRPGWMAX                                                      
         ST    RE,PRPGWMAX                                                      
         OC    OPTRPT,OPTRPT                                                    
         BNZ   PTSGRD2                                                          
         CHI   RE,93               CAN ONLY HANDLE 100 LINES                    
         BNH   PTSGRD2A                                                         
PTSGRD2  DS    0H                                                               
         GOTO1 VMRGPL                                                           
         XC    PRPGWMAX,PRPGWMAX                                                
         L     RE,=A(PLAREA)                                                    
         LHI   RF,PLAREAX-PLAREA                                                
         XCEF                                                                   
         B     PTSGRDA                                                          
PTSGRD2A MH    RE,MGLENLIN         BUMP TO NEXT PRINT LINE                      
         A     RE,DSTAGRID                                                      
         ST    RE,GRIDST                                                        
         B     PTSGRID1                                                         
         EJECT                                                                  
* SET ALLOCATIONS IN PRINT LINE                                                 
PTSGRID3 L     R5,PGDELAD                                                       
         USING REGELEM,R5                                                       
         LTR   R5,R5               BUILD ELEMENT IF NOT PRESENT                 
         BZ    *+12                                                             
         CLI   0(R5),11            (NOTE - X'0B/0C/0D=POOL!)                    
         BL    *+14                                                             
         OC    PGDELAD,PGDELAD     HAVE ELEMENT ADDRESS                         
         BNZ   PTSG3EOK             YES - PROCESS                               
         LA    R5,PGELEM            NO - BUILD DUMMY ELEMENT                    
         XC    PGELEM,PGELEM                                                    
         MVC   RDATE,PGDWK                                                      
         MVC   RPPRD,PGDSBRN                                                    
         MVC   RPCOST,PGD2COVR                                                  
         OC    RPCOST,RPCOST                                                    
         BZ    *+8                                                              
         OI    RSTATUS,RSRATOVQ    X'20'       SET FOR COST OVERRIDE            
         MVC   PGELEM(2),=X'0B0E'                                               
         CLI   PGD2BRN,0                                                        
         BE    PTSGPBOK                                                         
         MVC   PGELEM(2),=X'0B12'                                               
         MVC   RPPRD+L'RPALLOC(1),PGD2BRN                                       
PTSGPBOK MVC   RPTIME+L'RPALLOC(1),PGD2SLN                                      
         CLI   PGDIND,PGDIMISQ     1                                            
         BNE   *+8                                                              
         MVI   RSTATUS,RSMINSDQ    X'40'                                        
         CLI   PGDIND,PGDIPREQ     4                                            
         BNE   *+8                                                              
         MVI   RSTATUS,RSMINSDQ+RSMGONLQ    X'42'                               
         CLI   PGDIND,PGDIHIAQ     X'08'                                        
         BNE   *+8                                                              
         MVI   RSTATUS,RSHIATSQ    X'04'                                        
         CLI   PGDSBRN,X'FF'                                                    
         BNE   *+8                                                              
         MVI   PGELEM+1,X'0A'                                                   
PTSG3EOK DS    0H                                                               
         CLI   PTSSPILL,C'Y'                                                    
         BNE   *+8                                                              
         NI    RSTATUS,255-RSRATOVQ SUPPRESS COST OVERRIDES IF SPILL            
         MVI   PGWNOL,0                                                         
         CLI   PGCNDSW,1           CONDENSE                                     
         BE    *+8                  YES                                         
         MVI   PGDSNO,1            NO - PGDSNO HAS SUBLINE-RESET                
         XC    DUB,DUB                                                          
         CLI   GRIDLEN,13                                                       
         BNE   *+14                                                             
         OC    PGDFNO,PGDFNO       FORCE SECOND DATE IF FILM                    
         BNZ   PTSG3DT                                                          
         CLI   SCNDDTSW,1          PRINT DATES ON SECOND LINE                   
         BE    *+12                 YES                                         
         CLI   PGSUBLI,1           FIRST SUBLINE - PRINT DATE                   
         BNE   PTSGRD3A            NO                                           
PTSG3DT  GOTO1 DATCON,DMCB,(2,RDATE),(4,DUB)                                    
PTSGRD3A CLI   GRIDLEN,6                                                        
         BE    P06GRID                                                          
         MVC   FILMNO,SPACES                                                    
         CLI   GRIDLEN,10                                                       
         BE    P10GRID                                                          
         CLI   GRIDLEN,13                                                       
         BE    P13GRID                                                          
*--- GRID WITH 4 CHARACTER ENTRIES                                              
         BAS   R9,LINEA                                                         
         CLI   DUB,0                                                            
         BE    PTSG3A1                                                          
         MVC   0(3,RE),DUB                                                      
         AH    RE,MGLENLIN                                                      
         MVC   0(2,RE),DUB+3                                                    
         BAS   R9,INCR                                                          
         BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
PTSG3A1  DS    0C                                                               
         TM    RSTATUS,RSMINSDQ    X'40'            PRE-EMPT                    
         BZ    *+10                                                             
         MVC   0(3,RE),=C'*P*'                                                  
         TM    RSTATUS,RSMGONLQ    X'02'            MAKEGOOD                    
         BZ    *+10                                                             
         MVC   0(3,RE),=C'*M*'                                                  
         CLI   MGMSDSW,1           ANY *M* OR *P* SPOTS ON LINE                 
         BNE   *+8                                                              
         BAS   R9,INCR              YES - BUMP LINE                             
         BAS   R9,LINEA                                                         
         MVC   0(3,RE),=C'*U*'                                                  
         TM    RSTATUS,RSHIATSQ    X'04'            HIATUS                      
         BZ    *+14                                                             
         MVC   0(3,RE),=C'*H*'                                                  
         BAS   R9,INCR                                                          
PTSGRD3B DS    0C                                                               
         SPACE 2                                                                
PTSGRID4 BAS   R9,LINEA                                                         
         CLI   RLEN,RLPOL1LQ       X'0E'                                        
         BL    PTSGRD4C            UNALLOCATED                                  
         ZIC   RF,RPPRD                 GET PRODUCT CODE                        
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)                                                    
         BAS   R9,INCR                                                          
         ZIC   RE,RPTIME                                                        
         LTR   RE,RE                                                            
         BZ    PTSGRID5                                                         
         TM    RSTATUS,RSHIATSQ    X'04'                                        
         BO    PTSGRID5                                                         
         CLC   GRIDSLN,RPTIME                                                   
         BE    PTSGRID5                                                         
         EDIT  (B1,RPTIME),(3,DUB),,ALIGN=LEFT                                  
         BAS   R9,LINEA                                                         
         MVC   0(3,RE),DUB                                                      
*                                                                               
         CLI   RLEN,RLPOL1LQ       TEST JUST 1 PRD ALLOCATED                    
         BNE   PTSGRD4A                                                         
         L     R9,ADBUY                                                         
         TM    BDSTAT3-BUYREC(R9),BDST3_SPODS  TEST SPODS ACTIVE                
         BZ    PTSGRD4A            NOPE                                         
         BAS   R9,INCR             SPOD!                                        
         B     PTSGRID5                                                         
*                                                                               
PTSGRD4A BAS   R9,INCR             DO PIGGYBACK                                 
         BAS   R9,LINEA                                                         
         ZIC   RF,RPPRD+L'RPALLOC                                               
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)                                                    
         BAS   R9,INCR                                                          
         ZIC   RE,RPTIME+L'RPALLOC                                              
         BAS   R9,PBEDIT                                                        
         BAS   R9,LINEA                                                         
         MVC   0(3,RE),DUB                                                      
PTSGRD4B BAS   R9,INCR                                                          
         B     PTSGRID5                                                         
         SPACE 2                                                                
PTSGRD4C TM    RSTATUS,RSHIATSQ    X'04'                                        
         BNZ   PTSGRID5                                                         
         MVC   0(3,RE),=C'*U*'                                                  
         B     PTSGRD4B                                                         
         SPACE 2                                                                
PTSGRID5 DS    0H                                                               
PTSGRID6 CLI   PGDNOSP,1                                                        
         BE    PTSGRID7                                                         
         BAS   R9,LINEA                                                         
         SR    R0,R0                                                            
         IC    R0,PGDNOSP          GET NUMBER OF SPOTS                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   0(RE),C'('          PRINT 1 DIGIT WHERE POSSIBLE                 
         UNPK  1(1,RE),DUB                                                      
         MVI   2(RE),C')'                                                       
         CHI   R0,10               TEST NEED 2 DIGITS                           
         BNH   *+14                                                             
         UNPK  1(2,RE),DUB                                                      
         MVI   3(RE),C')'          (MAY ABUTT CHRS IN NEXT SLOT)                
         BAS   R9,INCR                                                          
*                                                                               
PTSGRID7 BAS   R9,LINEA                                                         
         CLI   0(RE),C' '          ANY DATA ON NEXT PRINT LINE                  
         BNE   *+8                                                              
         MVI   0(RE),0                                                          
         BAS   R9,INCR                                                          
         C     RE,PGWMAX           MAX THIS BLOCK GT. THAN PREV BLOCK           
         BNH   *+8                                                              
         ST    RE,PGWMAX                                                        
         LA    R6,PGRIDLQ(R6)                                                   
         B     PTSGRID1                                                         
PTSGRIDX GOTO1 VMRGPL                                                           
PTSEXIT  XIT1                                                                   
         EJECT                                                                  
*--- GRID WITH 6 CHARACTER ENTRIES                                              
P06GRID  BAS   R9,LINEA                                                         
         CLI   DUB,0               DATE PRESENT                                 
         BE    P06GRID1             NO                                          
         MVC   0(5,RE),DUB                                                      
         BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
P06GRID1 DS    0C                                                               
         TM    RSTATUS,RSMINSDQ    X'40'                                        
         BZ    *+10                                                             
         MVC   0(3,RE),=C'*P*'                                                  
         TM    RSTATUS,RSMGONLQ    X'02'                                        
         BZ    *+10                                                             
         MVC   0(3,RE),=C'*M*'                                                  
         CLI   MGMSDSW,1           ANY *M* OR *P* SPOTS ON LINE                 
         BNE   *+8                                                              
         BAS   R9,INCR              YES - BUMP LINE                             
         BAS   R9,LINEA                                                         
         MVC   0(3,RE),=C'*U*'                                                  
         TM    RSTATUS,RSHIATSQ    X'04'                                        
         BZ    P06GR1                                                           
         MVC   0(3,RE),=C'*H*'                                                  
         CLI   PGCNDSW,1                                                        
         BE    *+8                                                              
         BAS   R9,INCR             BUMP LINE IF STATUS BIT ON                   
         BAS   R9,LINEA                                                         
         MVI   0(RE),0                                                          
P06GR1   DS    0C                                                               
         CLI   RLEN,RLPOL1LQ       X'0E'                                        
         BL    P06GRD1A                                                         
         CLI   RPPRD,0                                                          
         BE    P06GRD1B                                                         
         ZIC   RF,RPPRD                                                         
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)                                                    
         CLI   PGD2BRN,0                                                        
         BE    P06GRD1A                                                         
         BAS   R9,INCR                                                          
         ZIC   RE,RPTIME                                                        
         BAS   R9,PBEDIT                                                        
         BAS   R9,LINEA                                                         
         MVC   0(3,RE),DUB                                                      
         BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
         ZIC   RF,RPPRD+L'RPALLOC                                               
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)                                                    
         BAS   R9,INCR                                                          
         ZIC   RE,RPTIME+4                                                      
         BAS   R9,PBEDIT                                                        
         BAS   R9,LINEA                                                         
         MVC   0(3,RE),DUB                                                      
P06GRD1A DS    0C                                                               
         CLI   PGDNOSP,1                                                        
         BE    P06GRID3                                                         
P06GRD1B LA    R9,3(RE)            PRINT NUMBER OF SPOTS                        
         CLI   2(RE),C' '                                                       
         BNE   *+8                                                              
         LA    R9,2(RE)                                                         
         MVI   0(R9),C'-'                                                       
         LA    R9,1(R9)                                                         
         EDIT  PGDNOSP,(2,(R9)),,ALIGN=LEFT                                     
P06GRID3 DS    0H                                                               
         BAS   R9,INCR                                                          
P06GRID4 DS    0H                                                               
         B     PTSGRID7                                                         
         SPACE 2                                                                
*--- GRID WITH 10 CHARACTER ENTRIES                                             
*    NOTE: THIS OMITS ANY PIGGYBACK BRAND DETAILS! (BUG?)                       
P10GRID  BAS   R9,LINEA                                                         
P10GRID1 DS    0H                                                               
         TM    RSTATUS,RSMINSDQ    X'40'                                        
         BZ    *+10                                                             
         MVC   0(3,RE),=C'*P*'                                                  
         TM    RSTATUS,RSMGONLQ    X'02'                                        
         BZ    *+10                                                             
         MVC   0(3,RE),=C'*M*'                                                  
         CLI   MGMSDSW,1           ANY *M* OR *P* SPOTS ON LINE                 
         BNE   *+8                                                              
         BAS   R9,INCR              YES - BUMP LINE                             
         BAS   R9,LINEA                                                         
         MVC   6(3,RE),=C'*U*'                                                  
         TM    RSTATUS,RSHIATSQ    X'04'                                        
         BZ    P10GR1                                                           
         MVC   6(3,RE),=C'*H*'                                                  
         BAS   R9,INCR             BUMP LINE IF STATUS BIT ON                   
         BAS   R9,LINEA                                                         
         MVI   0(RE),0                                                          
P10GR1   DS    0C                                                               
         MVC   0(5,RE),DUB                                                      
         MVI   5(RE),C'-'                                                       
         ZIC   RF,RPPRD                                                         
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   6(3,RE),1(RF)                                                    
         BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
P10GRID2 DS    0H                                                               
         CLI   PGDNOSP,1                                                        
         BE    PTSGRID7                                                         
         BAS   R9,LINEA                                                         
         SR    R0,R0                                                            
         IC    R0,PGDNOSP          GET NUMBER OF SPOTS                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   4(RE),C'('          TIDY PRINT IF 1 DIGIT                        
         UNPK  5(1,RE),DUB                                                      
         MVI   6(RE),C')'                                                       
         CHI   R0,10               TEST NEED 2 DIGITS                           
         BNH   *+14                                                             
         UNPK  5(2,RE),DUB                                                      
         MVI   6(RE),C')'                                                       
         BAS   R9,INCR                                                          
         B     PTSGRID7                                                         
         SPACE 2                                                                
*--- GRID WITH 13 CHARACTER ENTRIES                                             
*    NOTE: THIS OMITS ANY PIGGYBACK BRAND DETAILS! (BUG?)                       
*    NOTE: THIS OMITS ANY SPOT COUNT (CONDENE=1)! (BUG?)                        
P13GRID  BAS   R9,LINEA                                                         
         CLI   DUB,0                                                            
         BE    P13GRID1                                                         
         MVC   0(5,RE),DUB                                                      
         ZIC   R9,PGWNOL                                                        
         LA    R9,1(R9)                                                         
         STC   R9,PGWNOL                                                        
P13GRID1 DS    0C                                                               
P13GRID3 BAS   R9,LINEA                                                         
         ZIC   RF,RPPRD                                                         
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)                                                    
         ZIC   RE,PGWNOL                                                        
         LA    RE,1(RE)                                                         
         STC   RE,PGWNOL                                                        
         B     PTSGRID7                                                         
         EJECT                                                                  
LINEA    LR    RE,R4                                                            
         ZIC   RF,PGWNOL                                                        
         MH    RF,MGLENLIN                                                      
         AR    RE,RF                                                            
         BR    R9                                                               
INCR     ZIC   RE,PGWNOL                                                        
         LA    RE,1(RE)                                                         
         STC   RE,PGWNOL                                                        
         BR    R9                                                               
PBEDIT   EDIT  (RE),(3,DUB),,ALIGN=LEFT                                         
         BR    R9                                                               
SETMGMS  LR    RF,R6               CHECK FOR ANY                                
         MVI   MGMSDSW,0            MAKEGOOD OR MISSED SPOTS IN                 
SETMGMS1 CLC   PGLINNO(2),PGPRLNO   CURRENT BLOCK                               
         BNE   SETMGMSX                                                         
         CLI   PGDIND,PGDIMISQ     1            MISSED                          
         BNE   *+8                                                              
         MVI   MGMSDSW,1                                                        
         CLI   PGDIND,PGDIPREQ     4            MAKEGOOD ON NEW LINE            
         BNE   *+8                                                              
         MVI   MGMSDSW,1                                                        
         LA    R6,PGRIDLQ(R6)                                                   
         B     SETMGMS1                                                         
SETMGMSX LR    R6,RF                                                            
         BR    R9                                                               
         LTORG                                                                  
*>PGELEM   DS    CL30                                                           
         EJECT                                                                  
***********************************************************************         
* ??? MERGE PRINT LINES ???                                           *         
***********************************************************************         
         SPACE                                                                  
MRGPL    NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SPN5WK,R2                                                        
         MVC   SVREPORT,REPORT     COUNT CUTIN AND COST OVR LINES               
         XC    NUMCI,NUMCI                                                      
         LA    RE,14                                                            
         LA    RF,P1                                                            
         L     R9,=A(CULIN)                                                     
CNTCU    MVC   0(132,R9),0(RF)     SAVE PL'S AND REPORT                         
         LA    R9,132(R9)                                                       
         LA    RF,132(RF)                                                       
         BCT   RE,CNTCU                                                         
         LA    RE,CNTRPT                                                        
         ST    RE,REPORT                                                        
         BRAS  RE,PRCUTIN                                                       
         LA    RE,14                                                            
         LA    RF,P1                                                            
         L     R9,=A(CULIN)                                                     
CNTCU2   MVC   0(132,RF),0(R9)     RESTORE PL'S AND REPORT                      
         LA    R9,132(R9)                                                       
         LA    RF,132(RF)                                                       
         BCT   RE,CNTCU2                                                        
         MVC   REPORT,SVREPORT                                                  
         SPACE 2                                                                
         MVC   DSTAGRID,SVSGRID    RESTORE GRID ADDRESS                         
         MVC   SVSGRID,=A(PLAREA)                                               
MRGPL1   ZIC   RE,MAXLINES                                                      
         MVI   ALLOWLIN,0                                                       
         ZIC   RF,NUMCOM           ADJUST FOR COMMENT LINES                     
         SR    RE,RF                                                            
         SH    RE,NUMCI            ADJUST FOR CUTIN LINES                       
         ZIC   RF,LINE                                                          
         L     R7,PRPGWMAX                                                      
         A     R7,PGWMAX           ADD IN FINAL BLOCK                           
         ZIC   R0,NUMCOM           ADD IN COMMENT LINES                         
         AR    R0,R7                                                            
         ST    R0,PRPGWMAX                                                      
         CLC   QPROG,=C'U5'                                                     
         BNE   MRGPL1A                                                          
         AHI   R7,4                                                             
         CLI   NUMCOM,0                                                         
         BE    MRGPL1A                                                          
         AHI   R0,4                ADD IN SPACE LINES                           
         ST    R0,PRPGWMAX                                                      
MRGPL1A  SR    RE,RF               NUMBER OF LINES LEFT ON PAGE                 
         C     RE,PRPGWMAX         WILL ENTIRE LINE FIT                         
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'        NO - FORCE TO NEW PAGE                      
MRGPL2   LA    R5,14               NUMBER OF PRINT LINES AVAILABLE              
         L     RE,DSTAGRID                                                      
         L     RF,SVSGRID                                                       
MRGPL3   LH    R6,MGLENLIN         SET LENGTH OF LINE                           
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         BCT   R7,*+8              END OF PRINT BUFFER                          
         B     MRGPL4                                                           
         LA    RE,132(RE)                                                       
         LA    RF,1(R6,RF)                                                      
         ST    RF,SVSGRID                                                       
         BCT   R5,MRGPL3                                                        
         GOTO1 REPORT                                                           
         B     MRGPL2                                                           
MRGPL4   GOTO1 REPORT                                                           
         CLC   RTYPE,=C'RS '                                                    
         BE    MRGPL6                                                           
         CLI   PKGAREA,0                                                        
         BE    MRGPL5                                                           
         MVC   P1+31(16),PKGAREA                                                
         GOTO1 REPORT                                                           
MRGPL5   DS    0H                                                               
         LA    R1,P1+4                                                          
         CLC   P1,SPACES                                                        
         BNE   *+8                                                              
         LA    R1,P1+33                                                         
         ST    R1,FULL                                                          
         GOTO1 VCOMPRNT                                                         
         CLC   P1,SPACES                                                        
         BE    MRGPL6                                                           
         GOTO1 REPORT                                                           
MRGPL6   DS    0C                                                               
         OC    OPTRPT,OPTRPT       OPTIONAL REPORTS PRINT BLOCK HERE            
         BNZ   MRGPLX                                                           
         MVC   SVP1(80),SPACES                                                  
         MVC   SVP3(80),SPACES                                                  
         OC    NUMCI,NUMCI         DONT SKIP IF NO CUTINS                       
         BZ    MRGPLX                                                           
         MVI   P1,0                                                             
         GOTO1 REPORT                                                           
MRGPLX   MVC   SVSGRID,DSTAGRID                                                 
         MVC   DSTAGRID,=A(PLAREA)                                              
         XIT1                                                                   
*                                                                               
CNTRPT   NTR1                                                                   
         LH    RE,NUMCI                                                         
         LA    RE,2(RE)                                                         
         STH   RE,NUMCI                                                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* HANDLE SORTED REPORT                                                *         
***********************************************************************         
         SPACE                                                                  
SORTC    NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         CLI   SORTPASS,SORTINPQ   INPUT PHASE                                  
         BNE   SORTOUT              NO - DO OUTPUT                              
         OC    SSCNTR,SSCNTR                                                    
         BNZ   SORTIN1                                                          
         L     RE,VSSTABLE                                                      
         L     RF,=A(SSTABLQ)                                                   
         XCEF                                                                   
         L     RE,VPNTABLE                                                      
         L     RF,=A(PNTABLQ)                                                   
         XCEF                                                                   
         MVI   SOUTFRST,1                                                       
SORTIN1  DS    0H                                                               
         XC    WORK,WORK                                                        
*        LA    RE,WORK                                                          
*        USING PNAMD,RE                                                         
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         MVC   HALF,BDTIMST        CALCULATE START/END QTR HR                   
         LH    RF,HALF                                                          
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         MH    RF,=H'4'                                                         
         LR    R0,RF                                                            
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         AR    R0,RF                                                            
         STC   R0,FULL                                                          
         MVC   HALF,BDTIMEND                                                    
         LH    RF,HALF                                                          
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         MH    RF,=H'4'                                                         
         LR    R0,RF                                                            
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         AR    R0,RF                                                            
         STC   R0,FULL+1                                                        
*                                                                               
         LA    RE,WORK                                                          
         USING PNAMD,RE                                                         
         MVC   PNDCODE,PDNCNTR+3                                                
         CLI   SORTFRMT,3                                                       
         BNE   SORTIN10                                                         
         CLI   BDPROGRM+4,C'-'   CHECK FOR SHOW(=SHOW)/LIVE SHOW(=SHW-)         
         BNE   *+14                                                             
         MVC   PNDNAME(L'PNDNAME-5),BDPROGRM+5                                  
         B     SORTIN15                                                         
         CLI   BDPROGRM,C'='       CHECK FOR LIVE PROGRAM                       
         BNE   SORTIN10                                                         
         MVC   PNDNAME(L'PNDNAME-1),BDPROGRM+1                                  
         B     SORTIN15                                                         
*                                                                               
SORTIN10 MVC   PNDNAME,BDPROG+4                                                 
SORTIN15 MVC   PNDNAME+15(1),BDSEC                                              
         L     R9,PDNCNTR                                                       
         L     R8,VPNTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,WORK),(R8),(R9),18,(1,17),255                    
         OC    DMCB(4),DMCB                                                     
         BNZ   SORTIN17                                                         
         MVC   P(25),=CL25'<<< REPORT INCOMPLETE >>>'                           
         MVC   P2(15),=CL15'PROG TABLE FULL'                                    
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         DC    H'0'                PROGRAM NAME TABLE IS FULL                   
SORTIN17 MVC   PDNCNTR,DMCB+8                                                   
         L     R1,DMCB                                                          
         MVC   CURRPNUM,0(R1)                                                   
         XC    WORK,WORK                                                        
         DROP  RE                                                               
*                                                                               
         LA    RE,WORK                                                          
         USING SQRECD,RE                                                        
         XC    WORK,WORK                                                        
         MVC   SORTKLEN,=F'4'                                                   
         MVC   SORTRLEN,=F'8'                                                   
         MVC   DADRDISP,=F'4'                                                   
         MVC   SQDADDR,KEY+14      D/A ALWAYS IN SAME POSITION IN SORTS         
         CLI   SORTFRMT,1                                                       
         BNE   SORTIN20                                                         
         MVC   SQ1DAY,BDDAY                                                     
         XI    SQ1DAY,X'FF'                                                     
         MVC   SQ1TIME,FULL                                                     
         MVC   SQ1PNUM,CURRPNUM                                                 
         B     SORTADD                                                          
         SPACE 2                                                                
SORTIN20 CLI   SORTFRMT,2                                                       
         BNE   SORTIN30                                                         
         MVC   SQ2DAY,BDDAY                                                     
         XI    SQ2DAY,X'FF'                                                     
         MVC   SQ2TIME,FULL                                                     
         MVC   SQ2PNUM,CURRPNUM                                                 
         B     SORTADD                                                          
*                                                                               
SORTIN30 CLI   SORTFRMT,3                                                       
         BNE   SORTIN40                                                         
         MVC   SQ3DAY,BDDAY                                                     
         XI    SQ3DAY,X'FF'                                                     
         MVC   SQ3TIME,FULL                                                     
         MVC   SQ3PNUM,CURRPNUM                                                 
         B     SORTADD                                                          
SORTIN40 B     SORTCX                                                           
         SPACE 2                                                                
* ADD A RECORD TO THE SORT BUFFER                                               
SORTADD  L     RF,SSCNTR                                                        
         SR    RE,RE                                                            
         M     RE,SORTRLEN                                                      
         A     RF,VSSTABLE                                                      
         MVC   0(20,RF),WORK                                                    
         L     RF,SSCNTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,SSCNTR                                                        
         MVI   SOUTFRST,1                                                       
         C     RF,=AL4(SSTABMXQ)                                                
         BNH   SORTCX                                                           
         MVC   P(25),=CL25'<<< REPORT INCOMPLETE >>>'                           
         MVC   P2(15),=CL15'SORT TABLE FULL'                                    
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
SORTOUT  CLI   SOUTFRST,1                                                       
         BNE   SRTOUT1                                                          
         MVC   SRKYSAVE,KEY                                                     
         OC    SSCNTR,SSCNTR                                                    
         BZ    SRTOUT1A                                                         
* FIRST CONVERT PROG NUMBERS INTO SORTED PROG NAME SEQ NUMBERS                  
         L     RE,VSSTABLE                                                      
         L     R5,SSCNTR                                                        
         LA    R9,SQ1PNUM                                                       
         CLI   SORTFRMT,1                                                       
         BE    SORTIT10                                                         
         LA    R9,SQ2PNUM                                                       
         CLI   SORTFRMT,2                                                       
         BE    SORTIT10                                                         
         LA    R9,SQ3PNUM                                                       
         DROP  RE                                                               
SORTIT10 L     R8,VPNTABLE        SCAN FOR PROG CODE (NOT BINSRCH KEY!)         
SORTIT12 LR    RF,R8                                                            
         L     RE,PDNCNTR                                                       
SORTIT15 CLC   0(1,RF),0(R9)                                                    
         BE    SORTIT20                                                         
         LA    RF,PNAMDLQ(RF)                                                   
         BCT   RE,SORTIT15                                                      
         MVC   P(25),=CL25'<<< REPORT INCOMPLETE >>>'                           
         MVC   P2(17),=CL17'PROG NOT IN TABLE'                                  
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         DC    H'0'                                                             
SORTIT20 SR    RF,R8               DISPLACEMENT ->  SEQ NUMBER                  
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         SR    RE,RE                                                            
         D     RE,=AL4(PNAMDLQ)                                                 
         STC   RF,0(R9)            REPLACE PROG 'CODE' WITH 'SEQ'               
         A     R9,SORTRLEN         NEXT SORT ENTRY                              
         BCT   R5,SORTIT12                                                      
* NOW SORT THE ENTRIES                                                          
         L     R4,VSSTABLE                                                      
         L     R5,SSCNTR                                                        
         L     R6,SORTRLEN                                                      
         L     R7,SORTKLEN                                                      
         GOTO1 XSORT,DMCB,(R4),(R5),(R6),(R7),0                                 
         MVI   SOUTFRST,0                                                       
         MVC   NEXTSSLT,VSSTABLE                                                
         L     RE,ADBUY                                                         
         ST    RE,AREC                                                          
         SPACE 2                                                                
SRTOUT1  L     RE,NEXTSSLT                                                      
         A     RE,DADRDISP                                                      
         MVC   KEY+14(4),0(RE)                                                  
         OC    KEY+14(4),KEY+14                                                 
         BNZ   SRTOUT2                                                          
SRTOUT1A MVI   SORTPASS,SORTENDQ                                                
         MVI   SOUTFRST,1                                                       
         XC    SSCNTR,SSCNTR                                                    
         MVC   KEY,SRKYSAVE                                                     
         B     SORTCX                                                           
*                                                                               
SRTOUT2  L     RE,ADBUY            GET A BUY RECORD                             
         ST    RE,AREC                                                          
         GOTO1 GET                                                              
         L     RE,ADBUY                                                         
         MVC   KEY(13),0(RE)                                                    
         MVC   FULL,KEY+10         SET UP ACTIVE KEY                            
         XC    KEY+10(3),KEY+10                                                 
         MVC   KEY+11(2),FULL                                                   
         L     RE,NEXTSSLT                                                      
         XC    CURRSORT,CURRSORT   SET CURRENT KEY AND NEXT KEY                 
         XC    NEXTSORT,NEXTSORT                                                
         L     R9,SORTKLEN                                                      
         BCTR  R9,0                                                             
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   CURRSORT(0),0(RE)                                                
         A     RE,SORTRLEN                                                      
         ST    RE,NEXTSSLT                                                      
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   NEXTSORT(0),0(RE)                                                
         MVI   SORTPASS,SORTGETQ                                                
SORTCX   XIT1                                                                   
         LTORG                                                                  
*>SRKYSAVE DS    XL32                                                           
*>SORTKLEN DC    F'0'                SORT KEY LENGTH                            
*>SORTRLEN DC    F'0'                SORT RECORD LENGTH                         
*>DADRDISP DC    F'0'                DISK ADDRESS DISPLACEMENT                  
*>NEXTSSLT DC    F'0'                NEXT SORT SLOT                             
*>SOUTFRST DC    X'01'               FIRST TIME SWITCH                          
         EJECT                                                                  
***********************************************************************         
* BUILD A LIST OF CUTIN SPOTS                                         *         
***********************************************************************         
         SPACE                                                                  
CUTIN    NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
*                                                                               
         MVC   SVAREC,AREC                                                      
         MVC   SVADBUY,ADBUY                                                    
         XC    SVLOCAL,SVLOCAL     CLEAR OUT LOCAL STATION TOTALS               
         MVI   NETSW,1             SET FIRST FOR NETWORK                        
*                                                                               
         L     RE,=A(LOCLTAB)      CLEAR OUT LOCAL TABLE                        
         ST    RE,LOCLADDR                                                      
         LA    RF,LOCLTBLQ                                                      
         XCEF                                                                   
         L     RE,=A(CISLIST)      CUTIN STATION LIST                           
         LHI   RF,CISLISTX-CISLIST                                              
         XCEF                                                                   
         L     RE,=A(CICLIST)                                                   
         ST    RE,ANXTCI                                                        
         LHI   RF,CICLISTX-CICLIST                                              
         XCEF                                                                   
         L     RE,=A(CINPRD)      PRODUCT LIST FOR NETWORK                      
         LHI   RF,CINPRDX-CINPRD                                                
         XCEF                                                                   
         L     RE,=A(CISPRD)      PRODUCT LIST FOR STATION                      
         LHI   RF,CISPRDX-CISPRD                                                
         XCEF                                                                   
*                                                                               
         L     RE,=A(NETCOVRD)                                                  
         LHI   RF,NETCOVRX-NETCOVRD                                             
         XCEF                                                                   
*                                                                               
         L     RE,ADBUY                                                         
         L     RF,=A(CISLIST)       BUILD POSSIBLE CUTIN LIST                   
         LA    RE,24(RE)                                                        
         USING NTWKELEM,RE                                                      
*                                                                               
CI01     CLI   0(RE),0                                                          
         BE    CI04                                                             
         CLI   0(RE),X'68'                                                      
         BE    CI03                                                             
CI02     ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     CI01                                                             
         SPACE 2                                                                
CI03     MVC   0(5,RF),NTWKMKST    PUT STATION IN LIST                          
         LA    RF,5(RF)                                                         
         B     CI02                                                             
         DROP  RE                                                               
CI04     DS    0H                                                               
         EJECT                                                                  
* BUILD LIST OF NETWORK PRODUCTS                                                
         L     RE,=A(CINPRD)      SNO/P1/P2                                     
         LHI   RF,CINPRDX-CINPRD                                                
         XCEF                                                                   
*                                                                               
         SR    R1,R1                                                            
         L     RF,=A(CINPRD)                                                    
         USING CINPRDD,RF                                                       
*                                                                               
         L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING REGELEM,RE                                                       
*                                                                               
CI10     CLI   0(RE),0             END OF RECORD                                
         BE    CI20A                                                            
         CLI   0(RE),X'0B'         FIND REGELEM                                 
         BL    CI102X                                                           
         CLI   0(RE),X'0D'                                                      
         BH    CI102X                                                           
         CLC   RDATE,STRDTE                                                     
         BL    CI102X                                                           
         CLC   RDATE,ENDDTE                                                     
         BH    CI102X                                                           
         TM    RSTATUS,RSMINSDQ    X'40'                                        
         BO    CI102X                                                           
*                                                                               
         IC    R0,ELEMNO                                                        
         CLC   RDATE,ELEMDT                                                     
         BE    *+6                                                              
         SR    R0,R0                                                            
         MVC   ELEMDT,RDATE        SAVE ELEM DATE                               
         AHI   R0,1                                                             
         STC   R0,ELEMNO                                                        
         AHI   R1,1                BUMP SPOT COUNT                              
* SET REL NETWORK SPOT/DATE SLOT                                                
         LR    R8,R1               REL SPOT                                     
         SLL   R8,2                    * 4                                      
         A     R8,=A(SPLIST)           + REL TABLE START                        
         XC    0(4,R8),0(R8)       CLEAR SLOT                                   
         MVC   0(2,R8),RDATE       SET REL DATE                                 
         CLI   RLEN,RLPOL1LQ       UNALLOCATED HAS NO PRD                       
         BL    *+10                                                             
         MVC   2(1,R8),RPPRD       SET REL PRD1                                 
         SPACE 2                                                                
* SAVE NETWORK ALOCATIONS                                                       
         STC   R1,CINPSPTN         SAVE REL SPOT NUMBER                         
         CLI   RLEN,RLPOL1LQ       UNALLOCATED HAS NO PRD                       
         BL    *+10                                                             
         MVC   CINPPRD1,RPPRD      PRD1                                         
         CLI   RLEN,RLPOL2LQ       18                                           
         BL    *+16                                                             
         MVC   CINPPRD2,RPPRD+L'RPALLOC  PRD2                                   
         MVC   3(1,R8),RPPRD+L'RPALLOC  REL PRD2                                
         AHI   RF,CINPRNLQ                                                      
         L     R0,=A(CINPRD)                                                    
         AHI   R0,CINPRDX-CINPRD                                                
         CR    RF,R0                                                            
         BL    *+6                                                              
         DC    H'0'                CINPRD IS TOO SHORT                          
*                                                                               
CI102    TM    RSTATUS,X'20'       TEST COST OVERRIDE                           
         BZ    CI102X                                                           
*&&DO                                                                           
* SINCE CANADIAN NETWORK CONVERSION, PROCESS NETWORK COST OVERRIDES             
         TM    BDSTAT3,BDST3_CNNEW  TEST NEW-STYLE RECORD                       
         BZ    CI102X                                                           
*&&                                                                             
         L     R8,=A(NETCOVRD)     POINT TO TABLE                               
*                                                                               
CI102A   OC    0(6,R8),0(R8)       TEST FREE ENTRY                              
         BZ    CI102B                                                           
         AHI   R8,6                                                             
         B     CI102A                                                           
*                                                                               
CI102B   MVC   0(2,R8),RDATE       MOVE SPOT DATE                               
         STC   R0,2(R8)            SAVE SPOT NUMBER THIS DATE                   
         MVC   3(3,R8),RPCOST      SAVE COST OVRD AMOUNT                        
*                                                                               
CI102X   SR    R0,R0                                                            
         IC    R0,RLEN                                                          
         AR    RE,R0                                                            
         B     CI10                                                             
         DROP  RF                                                               
         EJECT                                                                  
* GET STATION RECORDS AND BUILD PRODUCT LIST                                    
CI20A    MVC   CIKEY,KEY                                                        
         L     R9,=A(CISLIST)                                                   
CI20     ST    R9,SAVER9                                                        
         OC    0(5,R9),0(R9)       LIST OF CUTINS IS COMPLETE                   
         BZ    CI40                                                             
         L     RE,LOCLADDR         SET STATION IN BUFFER                        
         MVC   LOCMS-LOCDEFD(L'LOCMS,RE),0(R9)                                  
         MVC   KEY,CIKEY           RESTORE NETWORK KEY                          
         MVC   KEY+4(5),0(R9)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     MAY NOT BE THERE FOR BRAND MODE              
         BNE   CI30                                                             
         L     RE,=A(BUYRECIO)     SET NEW IO AREA                              
         ST    RE,AREC                                                          
         ST    RE,ADBUY                                                         
         GOTO1 GET                                                              
         SPACE 2                                                                
* EXTRACT LOCAL STATION DETAIL                                                  
         MVI   EXTSW,C'Y'          SET TO ADD TO BUFFALO                        
         L     RE,=A(PGRID)        CLEAR OUT TABLES                             
         LHI   RF,PGRIDX-PGRID                                                  
         XCEF                                                                   
         XC    PGNOENT,PGNOENT                                                  
         XC    HIATAB,HIATAB                                                    
         XC    PREMTAB,PREMTAB                                                  
         BRAS  RE,EXTRCT           BUILD DATA                                   
         SPACE 2                                                                
* ADD UP LOCAL DETAILS AND SAVE                                                 
         LA    R5,MEDPERD          CHEK FOR ACTIVITY                            
         L     R4,4(R5)                                                         
         L     R5,4(R5)                                                         
         OC    MEDBYD(12),MEDBYD   ANY ACTIVITY                                 
         BZ    CI30                 NO - TRY NEXT STATION                       
         L     RE,LOCLADDR         SAVE LOCAL DEMOS                             
         USING LOCDEFD,RE                                                       
         XC    LOCFLAGS,LOCFLAGS                                                
         CLI   PBDREP,C'N'                                                      
         BE    *+12                                                             
         BAS   R9,SETPBD           HANDLE POST BUY DEMOS - OR -                 
         B     *+8                                                              
         BAS   R9,SETDOVER                ESTIMATED OVERRIDES BEFORE...         
         BAS   R9,ADDLOCL          ADD UP LOCAL TOTALS                          
         L     RE,LOCLADDR         RESET POINTER                                
         MVC   LOCBYD,MEDBYD       SAVE LOCAL DEMOS                             
         MVC   LOCBY1,MEDBY1                                                    
         MVC   LOCBY2,MEDBY2                                                    
         MVC   LOCBY3,MEDBY3                                                    
         MVC   LOCBY4,MEDBY4                                                    
         MVC   LOCBY5,MEDBY5                                                    
         MVC   LOCBY6,MEDBY6                                                    
         MVC   LOCBY7,MEDBY7                                                    
         MVC   LOCBY8,MEDBY8                                                    
*                                                                               
         LA    R1,LOCACCSQ         DIVIDE LOCAL TOTALS BY SPOTS                 
         LA    R9,LOCBYD                                                        
         MVC   FULL,MEDBYSPT       CHECK FOR ZERO SPOTS                         
         OC    FULL,FULL           ON NEGATIVE BUY                              
         BNZ   *+10                                                             
         MVC   FULL,=F'1'          YES - FORCE TO 1 FOR DIVIDE                  
CI20DIV  SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,15,0(R9)                                                      
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         AHI   RF,1                                                             
         SRA   RF,1                PRESERVE SIGN, NOT SRL!!!                    
         STCM  RF,15,0(R9)                                                      
         LA    R9,4(R9)                                                         
         BCT   R1,CI20DIV                                                       
         SPACE 2                                                                
         L     RE,LOCLADDR         POINT TO NEXT SLOT                           
         LA    RE,LOCDEFLQ(RE)                                                  
         ST    RE,LOCLADDR                                                      
         DROP  RE                                                               
*                                                                               
         BAS   R9,LCLSPILL         EXTRACT SPILL IF NEEDED                      
*                                                                               
* BUILD A LIST OF STATION PRODUCTS                                              
*                                                                               
         L     RE,=A(CISPRD)                                                    
         LHI   RF,CISPRDX-CISPRD                                                
         XCEF                                                                   
         LA    R1,0                                                             
*        LA    RF,CISPRD                                                        
         L     RF,=A(CISPRD)                                                    
         USING CISPRDD,RF                                                       
         L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING REGELEM,RE                                                       
         MVI   MSREQSW,1           SET TO DO MEDIA SUMMARY                      
CI202    CLI   0(RE),0                                                          
         BE    CI206                                                            
         CLI   0(RE),X'0B'         FIND REGELEM                                 
         BL    CI204                                                            
         CLI   0(RE),X'0D'                                                      
         BH    CI204                                                            
         CLC   RDATE,STRDTE                                                     
         BNL   *+12                                                             
         MVI   MSREQSW,0           DATE LOW - ALREADY PROCESSED                 
         B     CI204                                                            
         CLC   RDATE,ENDDTE                                                     
         BH    CI204                                                            
         TM    RSTATUS,RSMINSDQ    X'40'                                        
         BO    CI204                                                            
         LA    R1,1(R1)                                                         
         XC    CISPCOST,CISPCOST   CLEAR OUT COST OVERRIDE AREA                 
*&&DO                                                                           
* REPORT DOES NOT SHOW LOCAL COST OVERRIDES MHER 14OCT05                        
         TM    RSTATUS,RSMINUSQ    COST O/R DOESNT MATTER IF MINUS SPOT         
         BNZ   CI203                                                            
         TM    RSTATUS,RSRATOVQ    X'20'                                        
         BZ    CI203                                                            
         MVC   CISPCOST,RPCOST     MOVE IT IN IF ACTIVE                         
         OC    RPCOST,RPCOST                                                    
         BNZ   CI203                                                            
         MVC   CISPCOST,=XL3'FFFFFF'    NEED TO DISTINGUISH 0$ O/R              
*&&                                                                             
CI203    STC   R1,CISPSPTN                                                      
         CLI   RLEN,RLPOL1LQ       UNALLOCATED HAS NO PRODUCT(S)                
         BL    *+10                                                             
         MVC   CISPPRD1,RPPRD                                                   
         CLI   RLEN,RLPOL2LQ       18                                           
         BL    *+10                                                             
         MVC   CISPPRD2,RPPRD+L'RPALLOC                                         
         LA    RF,CISPRNLQ(RF)                                                  
         L     R0,=A(CISPRD)                                                    
         AHI   R0,CISPRDX-CISPRD                                                
         CR    RF,R0                                                            
         BL    *+6                                                              
         DC    H'0'                CISPRD IS TOO SHORT                          
*                                                                               
CI204    ZIC   R0,RLEN                                                          
         AR    RE,R0                                                            
         B     CI202                                                            
         DROP  RF                                                               
*                                                                               
CI206    CLI   MSREQSW,1           MEDIA SUMMARY ON HIGH START DATE             
         BE    *+8                                                              
         CLI   PASS,0              MEDIA SUMMARY ON PASS 0 ONLY                 
         BNE   CI30                                                             
         CLI   NETSW,0             PROCESS NETWORK RECORD AT FIRST              
         BE    CI208                                                            
         MVC   AREC,SVAREC         RESTORE NETWORK RECORD                       
         MVC   ADBUY,SVADBUY                                                    
         XC    CIKEY,KEY           INTERCHANGE KEYS                             
         XC    KEY(20),CIKEY                                                    
         XC    CIKEY,KEY                                                        
         BRAS  RE,GOSUBC                                                        
         XC    CIKEY,KEY           RESTORE KEYS                                 
         XC    KEY(20),CIKEY                                                    
         XC    CIKEY,KEY                                                        
CI208    L     RE,=A(BUYRECIO)     SET TO LOCAL                                 
         ST    RE,AREC                                                          
         ST    RE,ADBUY                                                         
         MVI   NETSW,0                                                          
         MVC   SVGETBY,MEDGETBY    SET TO TRAP BUYS                             
         MVC   MEDGETBY,ATRAPBUY                                                
         BRAS  RE,GOSUBC                                                        
         MVC   MEDGETBY,SVGETBY                                                 
         EJECT                                                                  
*LIST OF STATION SPOTS IS BUILT                                                 
*MATCH IT TO NETWORK SPOTS                                                      
                                                                                
CI30     MVC   AREC,SVAREC         RESTORE NETWORK RECORD                       
         MVC   ADBUY,SVADBUY                                                    
         L     RE,LOCLADDR         CLEAR NEXT LOCAL SLOT                        
         XC    0(LOCDEFLQ,RE),0(RE)                                             
         L     RE,=A(CINPRD)                                                    
         L     RF,=A(CISPRD)                                                    
CI301    L     R9,SAVER9                                                        
         CLI   0(RE),0             END                                          
         BNE   *+12                                                             
         LA    R9,5(R9)            SET NEXT STATION                             
         B     CI20                                                             
*                                                                               
         CLI   BPRD,0              CHECK FOR NTWRK PRODUCT OK                   
         BE    *+8                                                              
         CLI   BPRD,X'FF'                                                       
         BE    *+14                                                             
         CLC   BPRD,1(RE)          SAME AS REQUESTED                            
         BNE   CI302                                                            
         CLC   0(3,RE),0(RF)       MATCH PRODUCTS                               
         BNE   CI304                                                            
*                                                                               
CI302    LA    RE,3(RE)                                                         
         LA    RF,6(RF)                                                         
         B     CI301                                                            
         SPACE 2                                                                
CI304    L     R1,ANXTCI           SAVE CUTIN                                   
         OC    0(6,RF),0(RF)       HIATUS SPOT HAVE NOTHING HERE                
         BZ    CI302               SO DONT PUT IN TABLE                         
         MVC   0(5,R1),0(R9)       MARKET/STATION                               
         MVC   5(3,R1),0(RF)       SNO/PRD1/PRD2                                
*                                                                               
         CLC   0(3,RE),0(RF)       SAME PRODUCTS                                
         BNE   *+8                  NO - LEAVE CUTIN                            
         MVI   6(R1),254            YES - NEGATE CUTIN                          
*                                                                               
         LA    R1,11(R1)                                                        
         ST    R1,ANXTCI                                                        
         B     CI302                                                            
                                                                                
* LIST OF CUTINS IS BUILT - SORT INTO SPOT ORDER                                
                                                                                
CI40     SR    R1,R1                                                            
         L     RE,=A(CICLIST)                                                   
*                                                                               
CI401    CLI   5(RE),0             COUNT CUTINS                                 
         BE    CI402                                                            
         LA    R1,1(R1)                                                         
         LA    RE,11(RE)                                                        
         B     CI401                                                            
*                                                                               
CI402    LTR   R4,R1                                                            
         BZ    CIEXIT                                                           
         GOTO1 XSORT,DMCB,A(CICLIST),(R4),11,8,0                                
*                                                                               
CIEXIT   MVC   AREC,SVAREC                                                      
         MVC   ADBUY,SVADBUY                                                    
         MVC   KEY,CIKEY           RESTORE NETWORK RECORD                       
         GOTO1 HIGH                                                             
         GOTO1 GET                                                              
         XIT1                                                                   
         EJECT                                                                  
* ADD LOCAL ROUTINE                                                             
ADDLOCL  LA    RE,SVLOCAL          ADD UP ALL LOCAL DATA                        
         L     RF,0(RE)            DOLLARS                                      
         C     RF,=F'-1'           PRESERVE $ OVERFLOW                          
         BE    ADDLOCLA                                                         
         CLC   MEDBYD,=F'-1'                                                    
         BE    *+12                                                             
         A     RF,MEDBYD                                                        
         BNO   *+8                 DETECT $ OVERFLOW                            
         LHI   RF,-1                                                            
         ST    RF,0(RE)                                                         
ADDLOCLA L     RF,4(RE)            EQUIVALENCED DOLLARS                         
         CHI   RF,-1               PRESERVE $ OVERFLOW                          
         BE    ADDLOCLB                                                         
         CLC   MEDBYDEQ,=F'-1'                                                  
         BE    *+12                                                             
         A     RF,MEDBYDEQ                                                      
         BNO   *+8                 DETECT $ OVERFLOW                            
         LHI   RF,-1                                                            
         ST    RF,4(RE)                                                         
ADDLOCLB L     RF,8(RE)            SPOTS                                        
         A     RF,MEDBYSPT                                                      
         ST    RF,8(RE)                                                         
*                                                                               
         LA    R0,28               ADD UP DEMOS                                 
         LA    R6,MEDBY1                                                        
         LA    RE,12(RE)                                                        
ADDLOCL1 L     RF,0(RE)                                                         
         A     RF,0(R6)                                                         
         ST    RF,0(RE)                                                         
         LA    R6,L'MEDBY1(R6)                                                  
         LA    RE,4(RE)                                                         
         BCT   R0,ADDLOCL1                                                      
         BR    R9                                                               
*                                                                               
* ACCOUNT FOR LOCAL POST BUY DEMO OVERRIDES                                     
* ENTRY: RE=LOCLADDR - I.E. A(LOCDEFD ENTRY)                                    
* EXIT : LOCOVRD SET & LOCBY1-4(8) ADJUSTED AS REQUIRED                         
*                                                                               
SETPBD   L     RF,ADBUY                                                         
         LA    RF,24(RF)                                                        
         USING PDELEM,RF                                                        
SPBD010  CLI   PDCODE,0            EOR                                          
         BE    SPBD070             NO POST BUY DEMOS                            
         CLI   PDCODE,X'22'        (SHOULD FOLLOW 02)                           
         BE    SPBD015                                                          
         ZIC   R0,PDLEN                                                         
         AR    RF,R0                                                            
         B     SPBD010                                                          
         DROP  RF                                                               
*                                                                               
SPBD015  ST    RF,FULL             SAVE A(POST BUY DEMOS ELEM)                  
         L     RF,ADBUY            LOCATE ESTIMATED DEMOS ELEM                  
         LA    RF,24(RF)                                                        
         USING NDELEM,RF                                                        
SPBD016  CLI   NDCODE,0            EOR                                          
         BE    SPBD070                                                          
         CLI   NDCODE,NDCORGQ      X'02'                                        
         BE    SPBD020                                                          
         ZIC   R0,NDLEN                                                         
         AR    RF,R0                                                            
         B     SPBD016                                                          
*                                                                               
SPBD020  SR    R6,R6               LOCATE DEMOS FOR PRODUCT                     
         IC    R6,BPRD                                                          
         CLI   BPRD,X'FF'                                                       
         BNE   *+8                                                              
         LA    R6,220                                                           
         BCTR  R6,0                                                             
         MH    R6,PRDBUFLN                                                      
         L     R1,PRDBUFF                                                       
         LA    R6,PTDEMO-PTBUFFD(R6,R1)                                         
*                                                                               
         LA    R1,NDEMNO           SET START ADDRESS                            
         SR    R0,R0                                                            
         IC    R0,NDLEN                                                         
         AR    RF,R0               SET END ADDRESS                              
         DROP  RF                                                               
* SCAN DEMOS LOOKING FOR REPORTED DEMO!                                         
         LA    R0,1                INDEX (1 BASE)                               
         XC    DUB,DUB                                                          
         ST    R6,WORD                                                          
* STORE INDEX TO APPROPRIATE DEMO...                                            
* CATERS FOR USER NOT TIDY BUYS (VIA D8) IF CHANGE DEMO CATS                    
* ALSO USER MAY MANUALLY (=BUY, ACTION PD) SET PBD FOR NOT ALL DEMOS            
SPBD030  L     R6,WORD             REPOINT TO REPORTED DEMOS                    
         SR    R8,R8               REPORTED DEMO OFFSET                         
SPBD032  CLC   0(L'NDEMNO,R1),0(R6)                                             
         BE    SPBD035                                                          
         LA    R6,L'PTDEMNO(R6)    NEXT REPORTED DEMO                           
         AHI   R8,1                                                             
         CHI   R8,MAXDEMOQ         (8 SETTINGS FIT DUB!)                        
         BNE   SPBD032                                                          
         B     *+8                 NOT REPORTING!                               
SPBD035  STC   R0,DUB(R8)          SAVE O/SET INTO BUY DEMOS                    
         LA    R1,NDEMLNQ(R1)      NEXT BUY DEMO                                
         AHI   R0,1                BUMP BUY DEMO INDEX                          
         CR    R1,RF                                                            
         BL    SPBD030                                                          
*                                  NOW CHECK PBD VALUES OF INTEREST             
         L     RF,FULL                                                          
         LA    R1,PDEMO-PDELEM(RF) SET START ADDRESS                            
         SR    R0,R0                                                            
         IC    R0,PDLEN-PDELEM(RF)                                              
         AR    RF,R0               SET END ADDRESS                              
         LHI   R0,1                INDEX (1 BASE)                               
SPBD050  TM    0(R1),X'80'                                                      
         BZ    SPBD060             NOT POST BUY O/R                             
         LA    R8,DUB              REPORTED DEMO OFFSETS                        
         LHI   RE,MAXDEMOQ         (8 SETTINGS FIT DUB!)                        
         LHI   R6,LOC1OVRQ         PRIME FLAG MASK                              
SPBD052  CLM   R0,1,0(R8)          ENSURE FLAG APPROPRIATE DEMO                 
         BE    SPBD055                                                          
         SRL   R6,1                SHIFT BIT MASK                               
         AHI   R8,1                NEXT OFFSET                                  
         BCT   RE,SPBD052                                                       
         B     SPBD060             DON'T FLAG IF NOT REPORTING!                 
SPBD055  STCM  R6,1,BYTE                                                        
         L     RE,LOCLADDR         RESTORE POINTER                              
         USING LOCDEFD,RE                                                       
         OC    LOCOVRD,BYTE        SET DEMO FLAG MASK                           
         DROP  RE                                                               
SPBD060  LA    R1,L'PDEMO(R1)      NEXT DEMO                                    
         AHI   R0,1                BUMP INDEX                                   
         CR    R1,RF                                                            
         BL    SPBD050                                                          
*                                                                               
SPBD070  CLI   PBDREP,C'O'         CLEAR NON PBD IF ONLY WANT PBD               
         BNER  R9                                                               
         L     RE,LOCLADDR         RESTORE POINTER                              
         LHI   R6,LOC1OVRQ         PRIME FLAG MASK                              
         LA    RF,MEDBY1                                                        
         LHI   R0,MAXDEMOQ                                                      
SPBD072  SR    R1,R1                                                            
         ICM   R1,1,LOCOVRD-LOCDEFD(RE)                                         
         NR    R1,R6               TEST THIS DEMO IS PBD O/R                    
         BNZ   *+10                                                             
         XC    0(L'MEDBY1,RF),0(RF)     NOPE - CLEAR                            
         LA    RF,L'MEDBY1+L'MEDBY1EQ(RF) NEXT DEMO                             
         SRL   R6,1                     BIT MASK                                
         BCT   R0,SPBD072                                                       
         BR    R9                                                               
*                                                                               
* ACCOUNT FOR LOCAL ESTIMATED DEMO OVERRIDES (MANUAL -OR- FROM DEMODEF)         
* ENTRY: RE=LOCLADDR - I.E. A(LOCDEFD ENTRY)                                    
* EXIT : LOCOVRD SET                                                            
*                                                                               
         USING LOCDEFD,RE                                                       
SETDOVER CLI   DETOFLAG,YESSETQ    FLAGGING OVERRIDES?                          
         BNE   SDOVX                                                            
         L     RF,ADBUY                                                         
         LA    RF,24(RF)                                                        
         USING NDELEM,RF                                                        
SDOV010  CLI   NDCODE,0            EOR                                          
         BE    SDOVX                                                            
         CLI   NDCODE,NDCORGQ      X'02'                                        
         BE    SDOV020                                                          
         ZIC   R0,NDLEN                                                         
         AR    RF,R0                                                            
         B     SDOV010                                                          
*                                                                               
SDOV020  SR    R6,R6                                                            
         IC    R6,BPRD                                                          
         CLI   BPRD,X'FF'          LOCATE DEMOS FOR PRODUCT                     
         BNE   *+8                                                              
         LA    R6,220                                                           
         BCTR  R6,0                                                             
         MH    R6,PRDBUFLN                                                      
         L     R1,PRDBUFF                                                       
         LA    R6,PTDEMO-PTBUFFD(R6,R1)                                         
*                                                                               
         LA    R1,NDEMNO           SET START ADDRESS                            
         SR    R0,R0                                                            
         IC    R0,NDLEN                                                         
         AR    RF,R0               SET END ADDRESS                              
* SCAN DEMOS LOOKING FOR REPORTED DEMO!                                         
         ST    R6,WORD                                                          
SDOV025  TM    NDEMRAW-NDEMNO(R1),NDEMMANQ      X'80'                           
         BZ    SDOV040             NOT OVERRIDE                                 
         L     R6,WORD             REPOINT TO REPORTED DEMOS                    
         L     R0,NODEMS                                                        
         A     R0,NORTGS                                                        
         LHI   R8,LOC1OVRQ         PRIME FLAG MASK                              
* CATER FOR USER NOT TIDY BUYS (VIA D8) IF CHANGE DEMO CATS                     
SDOV032  CLC   0(L'NDEMNO,R1),0(R6)       ENSURE FLAG APPROPRIATE DEMO          
         BE    SDOV035                                                          
         LA    R6,L'PTDEMNO(R6)    NEXT REPORTED DEMO                           
         SRL   R8,1                SHIFT BIT MASK                               
         BCT   R0,SDOV032                                                       
         B     SDOV040             DON'T FLAG IF NOT REPORTING!                 
*                                                                               
SDOV035  STCM  R8,1,BYTE                                                        
         OC    LOCOVRD,BYTE        SET DEMO FLAG MASK                           
SDOV040  LA    R1,NDEMLNQ(R1)      NEXT DEMO                                    
         CR    R1,RF                                                            
         BL    SDOV025                                                          
SDOVX    BR    R9                                                               
         DROP  RE,RF                                                            
*                                                                               
* EXTRACT ANY SPILL FOR REPORTING IN THE LOCAL STATION SECTION                  
* NOTE: ONLY NEED SPILL IF REPORTING LOCAL RTGS BECAUSE...                      
*       A) NETWORK SPILL NEVER HELD FOR AUDIENCE DEMOS                          
*       B) RATINGS ONLY SHOWN IN LOCAL STATION SECTION                          
* REQUIREMENT IS TO SEE SPILL OF EACH BUYLINE FOR VERIFICATION MEANS            
* THERE IS NO LOOK-UP/RECALCULATION/TOTALLING REQUIRED SO CURRENTLY OK          
* TO USE THIS APPROACH (GETBY DOES NOT CATER FOR NETWORK SPILL ANYWAY!)         
LCLSPILL CLI   QOPT3,C'B'                                                       
         BE    *+8                                                              
         CLI   QOPT3,C'R'                                                       
         BNER  R9                                                               
*                                                                               
         XC    DUB,DUB                                                          
         L     RF,ADBUY                                                         
         LA    RF,24(RF)                                                        
         USING NDELEM,RF                                                        
LSPL010  CLI   NDCODE,0                                                         
         BE    LSPILLX                                                          
         CLI   NDCODE,NDCSPLQ                                                   
         BE    LSPL015                                                          
         SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     LSPL010                                                          
*                                                                               
LSPL015  L     RE,LOCLADDR         POINT TO CURRENT SLOT                        
         USING LOCDEFD,RE                                                       
         MVC   LOCMS(2),4(RF)            SPILL MARKET                           
         MVI   LOCSIND,LOCSPLQ                                                  
* EXTRACT APPROPRIATE DEMOS                                                     
         OC    DUB,DUB             SHOULD ONLY NEED DO ONCE PER BUY             
         BNZ   LSPL027             - ALREADY SET, SAVE CPU                      
         SR    R6,R6                                                            
         IC    R6,BPRD                                                          
         CLI   BPRD,X'FF'          LOCATE DEMOS FOR PRODUCT                     
         BNE   *+8                                                              
         LA    R6,220                                                           
         BCTR  R6,0                                                             
         MH    R6,PRDBUFLN                                                      
         L     R1,PRDBUFF                                                       
         LA    R6,PTDEMO-PTBUFFD(R6,R1)                                         
*                                                                               
         ST    RF,FULL             SAVE A(CURRENT NDELEM)                       
         LA    R1,NDEMNO           SET START ADDRESS                            
         SR    R0,R0                                                            
         IC    R0,NDLEN                                                         
         AR    RF,R0               SET END ADDRESS                              
* SCAN DEMOS LOOKING FOR REPORTED DEMO!                                         
         LA    R0,1                INDEX (1 BASE)                               
         XC    DUB,DUB                                                          
         ST    R6,WORD                                                          
* SET INDEXES IN BUY ELEM FOR REPORTED DEMOS (NEED IF POSTOVER VALUES)          
LSPL020  L     R6,WORD             REPOINT TO REPORTED DEMOS                    
         SR    R8,R8               REPORTED DEMO OFFSET                         
LSPL022  CLC   0(L'NDEMNO,R1),0(R6)                                             
         BE    LSPL025                                                          
         LA    R6,L'PTDEMNO(R6)    NEXT REPORTED DEMO                           
         AHI   R8,1                                                             
         CHI   R8,MAXDEMOQ         (8 SETTINGS FIT DUB!)                        
         BNE   LSPL022                                                          
         B     *+8                 NOT REPORTING!                               
LSPL025  STC   R0,DUB(R8)          SAVE O/SET INTO BUY DEMOS                    
         LA    R1,NDEMLNQ(R1)      NEXT BUY DEMO                                
         AHI   R0,1                BUMP BUY DEMO INDEX                          
         CR    R1,RF                                                            
         BL    LSPL020                                                          
*                                  NOW EXTRACT DEMOS VALUES OF INTEREST         
         L     RF,FULL             READDRESS A(CURRENT NDELEM)                  
LSPL027  LA    R1,NDEMNO           SET START ADDRESS                            
         SR    R0,R0                                                            
         IC    R0,NDLEN                                                         
         AR    RF,R0               SET END ADDRESS                              
         CLI   PBDREP,C'O'                                                      
         BE    LSPL050                                                          
*                                  EXTRACT ESTIMATED DEMOS                      
         LHI   R0,1                INDEX (1 BASE)                               
LSPL030  LA    R8,DUB              REPORTED DEMO OFFSETS                        
         LHI   RE,MAXDEMOQ         (8 SETTINGS FIT DUB!)                        
LSPL032  CLM   R0,1,0(R8)          ENSURE APPROPRIATE DEMO                      
         BE    LSPL035                                                          
         AHI   R8,1                NEXT OFFSET                                  
         BCT   RE,LSPL032                                                       
         B     LSPL040             NOT REPORTING!                               
*                                                                               
LSPL035  LA    R6,DUB                                                           
         SR    R8,R6               INDEX INTO REPORTED DEMOS                    
         LR    R6,R8                                                            
         MHI   R8,L'LOCBY1         OFFSET INTO REPORTED DEMOS                   
         L     RE,LOCLADDR         RESTORE POINTER                              
         AR    R8,RE                                                            
         MVC   LOCBY1-LOCDEFD(L'LOCBY1,R8),NDEMRAW-NDEMNO(R1)                   
         NI    LOCBY1-LOCDEFD(R8),255-NDEMMANQ-NDEM2DEC                         
         TM    NDEMRAW-NDEMNO(R1),NDEMMANQ                                      
         BZ    LSPL040             NOT OVERRIDE                                 
         CLI   PBDREP,C'N'         DO WE ONLY WANT PBD O/R FLAGS                
         BNE   LSPL040             - YES                                        
         LHI   R8,LOC1OVRQ         PRIME FLAG MASK                              
         LTR   R6,R6               R6=SAVED INDEX INTO REPORTED DEOMS           
         BZ    *+12                                                             
         SRL   R8,1                SHIFT BIT MASK                               
         BCT   R6,*-4                                                           
         STCM  R8,1,BYTE                                                        
         OC    LOCOVRD,BYTE        SET DEMO FLAG MASK                           
*                                                                               
LSPL040  LA    R1,NDEMLNQ(R1)      NEXT BUY DEMO                                
         AHI   R0,1                BUMP INDEX                                   
         CR    R1,RF                                                            
         BL    LSPL030                                                          
*                                                                               
LSPL050  CLI   PBDREP,C'N'                                                      
         BE    LSPL100                                                          
*                                  EXTRACT PBD VALUES OF INTEREST               
         CLI   0(RF),X'23'                                                      
         BNE   LSPL100                                                          
* HAS MARKET - SHOULD WE CHECK IS CORRECT, I EXPECT LIKE MKT 23&03 ELS          
         LA    R1,SDEMO-SDELEM(RF) SET START ADDRESS                            
         SR    R0,R0                                                            
         IC    R0,SDLEN-SDELEM(RF)                                              
         AR    RF,R0               SET END ADDRESS                              
         LHI   R0,1                INDEX (1 BASE)                               
LSPL060  TM    0(R1),X'80'                                                      
         BZ    LSPL070             NOT POST BUY O/R                             
         LA    R8,DUB              REPORTED DEMO OFFSETS                        
         LHI   RE,MAXDEMOQ         (8 SETTINGS FIT DUB!)                        
LSPL062  CLM   R0,1,0(R8)          ENSURE APPROPRIATE DEMO                      
         BE    LSPL065                                                          
         AHI   R8,1                NEXT OFFSET                                  
         BCT   RE,LSPL062                                                       
         B     LSPL070             NOT REPORTING!                               
*                                                                               
LSPL065  LA    R6,DUB                                                           
         SR    R8,R6               INDEX INTO REPORTED DEMOS                    
         LR    R6,R8                                                            
         MHI   R8,L'LOCBY1         OFFSET INTO REPORTED DEMOS                   
         L     RE,LOCLADDR         RESTORE POINTER                              
         AR    R8,RE                                                            
         MVI   LOCBY1-LOCDEFD(R8),0            PBD DEMOS ARE 3 BYTES            
         MVC   LOCBY1-LOCDEFD+1(3,R8),0(R1)    SET DEMO VALUE                   
         NI    LOCBY1-LOCDEFD+1(R8),255-X'80'  REMOVE PBD O/R SETTING           
         LHI   R8,LOC1OVRQ         PRIME FLAG MASK                              
         LTR   R6,R6               R6=SAVED INDEX INTO REPORTED DEOMS           
         BZ    *+12                                                             
         SRL   R8,1                SHIFT BIT MASK                               
         BCT   R6,*-4                                                           
         STCM  R8,1,BYTE                                                        
         OC    LOCOVRD,BYTE        SET DEMO FLAG MASK                           
*                                                                               
LSPL070  LA    R1,L'SDEMO(R1)      NEXT DEMO                                    
         AHI   R0,1                BUMP INDEX                                   
         CR    R1,RF                                                            
         BL    LSPL060                                                          
*                                                                               
LSPL100  L     RE,LOCLADDR         RESTORE POINTER                              
         LA    RE,LOCDEFLQ(RE)     POINT TO NEXT SLOT                           
         ST    RE,LOCLADDR                                                      
         B     LSPL010             RF ALREADY A(NEXT ELEM)                      
*                                                                               
LSPILLX  BR    R9                                                               
         DROP  RE,RF                                                            
         EJECT                                                                  
PRCOSTOV NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
*                                                                               
         CLI   QOPT2,C'Y'          TEST SUPPRESS COSTS                          
         JNE   EXIT                                                             
*                                                                               
         L     R4,=A(NETCOVRD)                                                  
*                                                                               
PRCOV2   OC    0(6,R4),0(R4)                                                    
         JZ    EXIT                                                             
         CLC   0(2,R4),PASSSD2     TEST PRIOR TO PASS START                     
         BNL   PRCOV4                                                           
         AHI   R4,6                                                             
         B     PRCOV2                                                           
*                                                                               
PRCOV4   CLC   0(2,R4),PASSED2     TEST AFTER PASS END                          
         JH    EXIT                YES - DONE                                   
*                                                                               
PRCOV6   MVC   P1+4(28),=C'** NETWORK COST OVERRIDES **'                        
*                                                                               
         LA    R5,P1+40                                                         
*                                                                               
PRCOV10  ST    R5,FULL             SAVE PRINT LINE ADDR                         
         LHI   R6,4                                                             
*                                                                               
PRCOV14  GOTO1 DATCON,DMCB,(2,0(R4)),(4,0(R5))  GET MMMDD                       
*                                                                               
         LA    RF,6(R5)            POINT TO NEXT PRINT POSN                     
         CLI   2(R4),1                                                          
         BNH   PRCOV16                                                          
         BCTR  RF,0                BACK UP ONE POSN                             
         MVI   0(RF),C'-'                                                       
         SR    R0,R0                                                            
         IC    R0,2(R4)           SPOT SEQNUM                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(2,RF),DUB                                                      
         AHI   RF,4                                                             
*                                                                               
PRCOV16  SR    R0,R0                                                            
         ICM   R0,7,3(R4)          GET COST AMOUNT                              
         BNZ   PRCOV18                                                          
         MVC   0(3,RF),=C'$0 '                                                  
         B     PRCOV20                                                          
*                                                                               
PRCOV18  EDIT  (R0),(9,0(RF)),2,ALIGN=LEFT,FLOAT=$,ZERO=NOBLANK                 
         AR    RF,R0               POINT TO END                                 
         AHI   RF,-3                                                            
         CLC   0(3,RF),=C'.00'                                                  
         BNE   *+10                                                             
         MVC   0(3,RF),SPACES                                                   
*                                                                               
PRCOV20  AHI   R4,6                NEXT COST OVERRIDE                           
         OC    0(6,R4),0(R4)                                                    
         BZ    PRCOVX                                                           
         CLC   0(2,R4),PASSED2     TEST AFTER PASS END                          
         BH    PRCOVX                                                           
         AHI   R5,20               NEXT PRINT POSN                              
         BCT   R6,PRCOV14                                                       
*                                                                               
         L     R5,FULL                                                          
         AHI   R5,132              NEXT PRINT LINE                              
         B     PRCOV10                                                          
*                                                                               
PRCOVX   GOTO1 REPORT                                                           
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT CUTINS &/OR COST OVERRIDES                                    *         
***********************************************************************         
         SPACE                                                                  
PRCUTIN  NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         L     R4,=A(CICLIST)                                                   
         USING CICLISTD,R4                                                      
PRCI2    XC    LOCALWK,LOCALWK                                                  
         MVI   LOCALSW,0                                                        
         LA    R9,P1+PCUTDET-PCUTIND                                            
         ST    R9,FULL                                                          
PRCI3    LA    R5,PCUTDMXQ         MAX SLOTS ON PRINTLINE                       
         L     R9,FULL             START OF PRINTLINE                           
PRCI4    OC    CICLMS,CICLMS       END OF CUTINS                                
         BZ    PRCIXIT                                                          
         CLC   CICLMS,LOCALWK      NEW STATION                                  
         BE    PRCI6                                                            
         CLI   LOCALSW,1           PRINTLINE WAITING                            
*        BNE   PRCI5                                                            
         BNE   PRCI4A                                                           
         GOTO1 REPORT                                                           
         B     PRCI2                                                            
*                         MAY HAVE BEEN NOTHING TO PRINT FOR STATION -          
PRCI4A   MVC   P1,SPACES           CLEAR OUT ANY UNPRINTED CAPTIONS             
         MVC   P2,SPACES                                                        
         OC    LOCALWK,LOCALWK     IF NOWT TO PRINT FOR STATION                 
         BNZ   PRCI2               RESET R9/R5 ELSE BLOWS UP IF QOPT1=N         
*                                                                               
PRCI5    MVC   QOPT1(7),N5OPTS                                                  
         CLI   QOPT1,C'Y'          TEST TO PRINT CUTINS                         
         BNE   PRCI5A                                                           
         GOTO1 MSUNPK,DMCB,(X'80',(R4)),WORK,WORK+4 SET CAPTIONS                
         MVC   P1+PCUTDESC-PCUTIND(L'PCUTDESC),=C'** CUTINS ON '                
         MVC   P1+PCUTDSC2-PCUTIND(2),=C'**'                                    
         MVC   P1+PCUTSTA-PCUTIND(L'PCUTSTA),WORK+4     STATION                 
         CLI   CICLSTA+2,X'C0'                                                  
         BL    *+10                                                             
         MVC   P1+PCUTSFX-PCUTIND(3),WORK+8                                     
*                                                                               
PRCI5A   MVC   LOCALWK(L'CICLMS),CICLMS  SAVE MARKET/STATION                    
*                                                                               
PRCI6    ZIC   R8,CICLSPTN         GET DATE FROM REL SLOT                       
         SLL   R8,2                                                             
         A     R8,=A(SPLIST)                                                    
         CLI   QOPT1,C'Y'                                                       
         BNE   PRCI8                                                            
         GOTO1 DATCON,DMCB,(2,(R8)),(4,PCUTDATE-PCUTDET(R9))                    
         CLI   CICLPRD1,CICLNEGQ   NEGATED CUTIN                                
         BE    PRCI8                (PRINT ONLY COST OVERRIDE)                  
         MVI   PCUTDASH-PCUTDET(R9),C'-'                                        
         ZIC   R8,CICLPRD1         PRODUCT CODE                                 
         MVI   LOCALSW,1           SET PRINTLINE WAITING                        
         MVC   PCUTPRD1-PCUTDET(3,R9),=C'*U*'                                   
         LTR   R8,R8                                                            
         BZ    PRCI8                                                            
         BCTR  R8,0                                                             
         MH    R8,PRDBUFLN                                                      
         A     R8,PRDBUFF                                                       
         MVC   PCUTPRD1-PCUTDET(3,R9),1(R8)      SET PRODUCT CODE               
         ZIC   R8,CICLPRD2         PRODUCT CODE 2                               
         LTR   R8,R8                                                            
         BZ    PRCI8                                                            
         BCTR  R8,0                                                             
         MH    R8,PRDBUFLN                                                      
         A     R8,PRDBUFF                                                       
         MVC   PCUTPRD2-PCUTDET(3,R9),1(R8)      SET PRODUCT CODE 2             
         MVI   PCUTPB-PCUTDET(R9),C'/'                                          
*                                                                               
PRCI8    LA    R9,PCUTDNLQ(R9)                                                  
         LA    R4,CICLSNLQ(R4)                                                  
         BCT   R5,PRCI4                                                         
         DROP  R4                                                               
*                                                                               
         L     R9,FULL             POINT TO NEXT PRINT LINE                     
         LA    R9,132(R9)                                                       
         CLI   QOPT2,C'Y'                                                       
         BNE   *+8                                                              
         LA    R9,132(R9)                                                       
         ST    R9,FULL                                                          
         B     PRCI3                                                            
*                                                                               
PRCIXIT  CLI   LOCALSW,1           PRINT LINE WAITING                           
         BNE   *+10                                                             
         GOTO1 REPORT                                                           
         MVC   P1,SPACES           CLEAR OUT UNPRINTED CAPTIONS                 
         MVC   P2,SPACES                                                        
         L     R4,=A(CISLIST)                                                   
         OC    0(5,R4),0(R4)                                                    
         BZ    PRCIEXIT                                                         
         MVI   P1,X'00'                                                         
         GOTO1 REPORT                                                           
PRCIEXIT XIT1                                                                   
         EJECT                                                                  
* CHKCOV ROUTINE                                                                
CHKCOV   NTR1                                                                   
         CLI   QOPT2,C'Y'          OVERRIDES REQUESTED                          
         BNE   CHKCOVX             NO RETURN                                    
         OC    8(3,R4),8(R4)       ANY OVERRIDE VALUE                           
         BNZ   CHKCOVX             YES-EXIT                                     
         MVI   QOPT2,C'N'          YES SUPPRESS IF NOT ACTIVE                   
         LR    RE,R4                                                            
CHKCOV1  CLC   0(5,R4),0(RE)       SAME STATION                                 
         BNE   CHKCOVX                                                          
         OC    8(3,R4),8(R4)       ANY OVERRIDE VALUE                           
         BZ    *+8                                                              
         MVI   QOPT2,C'Y'          YES TURN ON OPTION                           
         LA    R4,11(R4)           TRY AGAIN                                    
         B     CHKCOV1                                                          
CHKCOVX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT LOCAL STATIONS (QOPT3=C'Y,R,B')                               *         
***********************************************************************         
         SPACE                                                                  
PRLOCL   NTR1  BASE=*,LABEL=*                                                   
         USING SPN5WK,R2                                                        
         L     R9,=A(LOCLTAB)                                                   
         USING LOCDEFD,R9                                                       
PRLOCL10 MVI   BYTE,0                                                           
         LA    R8,P1               PRINT OUT LOCAL STATIONS                     
         LR    R6,R8                                                            
         USING PLOCALD,R8                                                       
         MVC   PLOCDESC(7),=C'STATION'                                          
         MVC   PLOCDESC+L'P(4),=C'COST'                                         
         LA    R8,P3                                                            
         LA    RF,DNAME1                                                        
         L     R0,NODEMS                                                        
         CLI   QOPT3,C'B'                                                       
         BNE   *+12                                                             
         A     R0,NORTGS           INCLUDE RTGS                                 
         B     PRLOCL11                                                         
         CLI   QOPT3,C'R'                                                       
         BNE   PRLOCL11                                                         
         MHI   R0,L'DNAME1         ONLY SHOW RTGS                               
         AR    RF,R0                                                            
         L     R0,NORTGS                                                        
PRLOCL11 CLI   0(RF),0             DEMO 1-4(8) IF PRESENT!                      
         BE    PRLOCL15                                                         
         MVC   PLOCDESC(L'DNAME1),0(RF)                                         
         LA    R8,L'P(R8)                                                       
         LA    RF,L'DNAME1(RF)                                                  
         BCT   R0,PRLOCL11                                                      
*                                                                               
PRLOCL15 MVI   0(R8),X'00'         FORCE BLANK LINE AT BOTTOM (NEATER)          
         LR    R8,R6                                                            
*        MVC   SVP1,P1                                                          
*        MVC   SVP2,P2                                                          
*        MVC   SVP3,P3                                                          
*        MVC   SVP4,P4                                                          
PRLOCL20 OC    LOCMS,LOCMS                                                      
         BZ    PRLOCLX                                                          
         MVC   WORK+4(10),SPACES                                                
         GOTO1 MSUNPK,DMCB,(X'80',LOCMS),WORK,WORK+4                            
         TM    LOCSIND,LOCSPLQ                                                  
         BZ    PRLOCL25                                                         
         MVI   PLOCSPL,C'*'                                                     
         MVC   PLOCSPL+1(4),WORK                                                
         MVC   PLOCSPL+L'P(5),=C'SPILL'                                         
         B     PRLOCL30                                                         
         SPACE 2                                                                
PRLOCL25 MVC   PLOCSTA(4),WORK+4   (P2)                                         
         CLI   LOCMS+4,X'B0'     DUMMY CBL STNS BEGIN X'B0', REAL X'C0'         
         BL    *+10                                                             
         MVC   PLOCCBL(7),WORK+4                                                
         ICM   R4,15,LOCBYD                                                     
         EDIT  (R4),(10,PLOCDOL+L'P),2,FLOAT=-                                  
*                                                                               
PRLOCL30 LA    R8,2*L'P(R8)        (P3+)                                        
         LHI   R4,LOC1OVRQ         PRIME TEST BIT                               
         LA    R1,LOCBY1           DEMOS                                        
         LA    RF,DNAME1                                                        
         L     R5,NODEMS                                                        
         CLI   QOPT3,C'B'                                                       
         BNE   *+12                                                             
         A     R5,NORTGS           INCLUDE RTGS                                 
         B     PRLOCL40                                                         
         CLI   QOPT3,C'R'                                                       
         BNE   PRLOCL40                                                         
         LR    RE,R5                                                            
         MHI   R5,L'DNAME1         ONLY SHOW RTGS                               
         AR    RF,R5                                                            
         LR    R5,RE                                                            
         MHI   RE,L'LOCBY1                                                      
         AR    R1,RE                                                            
         LTR   R5,R5               TEST WERE AUD DEMOS!                         
         BZ    *+12                                                             
         SRL   R4,1                (SHIFT OVERRIDE MASK TOO!)                   
         BCT   R5,*-4                                                           
         L     R5,NORTGS                                                        
PRLOCL40 CLI   0(RF),0                                                          
         BE    PRLOCL70                                                         
         TM    LOCSIND,LOCSPLQ     DON'T PRINT VALUES FOR AUD SPILL             
         BZ    PRLOCL42                                                         
         CLI   0(RF),RTGDEMOQ                                                   
         BE    PRLOCL42            (BLANK MORE APPROPRIATE THAN .0)             
         CLI   0(RF),EXTDEMOQ                                                   
         BNE   PRLOCL45                                                         
PRLOCL42 ICM   RE,15,0(R1)                                                      
         TM    RQOPTS,RQOPTS_2DEC  REPORT SUPPORTS 2 DEC DEMOS?                 
         BZ    PRLOCL43            NO                                           
         CLI   0(RF),C'R'          RATING?                                      
         BE    *+12                YES                                          
         CLI   0(RF),C'E'          EXTENDED RATING?                             
         BNE   PRLOCL43            NO                                           
         EDIT  (RE),(8,PLOCDEMO),2 PRINT IN 2 DECIMAL                           
         B     PRLOCL44                                                         
*                                                                               
PRLOCL43 EDIT  (RE),(8,PLOCDEMO),1                                              
*                                                                               
PRLOCL44 SR    RE,RE                                                            
         ICM   RE,1,LOCOVRD                                                     
         NR    RE,R4               TEST THIS DEMO IS O/R                        
         BZ    *+8                                                              
         MVI   PLOCDOVR,OVRDINDQ                                                
*                                                                               
PRLOCL45 LA    R8,L'P(R8)          NEXT PLINE                                   
PRLOCL50 LA    R1,L'LOCBY1(R1)          DEMO                                    
         LA    RF,L'DNAME1(RF)          DEMO NAME                               
         SRL   R4,1                     BIT MAP TEST                            
         BCT   R5,PRLOCL40                                                      
*                                                                               
PRLOCL70 LA    R8,L'PLOCDET(R6)                                                 
         LR    R6,R8                                                            
         LA    R9,LOCDEFLQ(R9)                                                  
         ZIC   RE,BYTE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         CHI   RE,PLOCDMXQ                                                      
         BL    PRLOCL20                                                         
         MVI   BYTE,0                                                           
         GOTO1 REPORT                                                           
         OC    LOCMS,LOCMS       PREVENT UNNECESSARY EXTRA ROWS IF NO           
         BNZ   PRLOCL10          DATA LEFT                                      
         SPACE 2                                                                
PRLOCLX  GOTO1 REPORT                                                           
         MVC   SVP1,SPACES                                                      
         MVC   SVP2,SPACES                                                      
         MVC   SVP3,SPACES                                                      
         MVC   SVP4,SPACES                                                      
         XIT1                                                                   
         DROP  R9                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
PTSGD    DSECT                                                                  
GRIDST   DS    F                   START OF GRID                                
PRPGWMAX DS    F                                                                
PRPGDWK  DS    CL2                 PREVIOUS BLOCK DATES                         
PGPRLNO  DS    CL3                 PREVIOUS LINE/SUN LINE/SLOT                  
PGWNOL   DS    C                   NUMBER OF LINES IN THIS BLOCK                
GRIDLEN  DS    C                                                                
GRIDSW1  DS    C                                                                
GRIDSLN  DS    C                                                                
PGWKCNT  DS    F                   WEEKLY SLOT COUNTER                          
         EJECT                                                                  
***********************************************************************         
* LOCAL STORAGE CSECTS                                                          
***********************************************************************         
SPN502   CSECT                                                                  
SPN5WK   DS    0D                                                               
RTYPE    DS    CL3                 (=QPROG WITH BYTE 3 CLEARED!)                
LOCALWK  DS    CL20                ROUTINE TEMPS WORK                           
LOCALSW  DS    C                   SWITCH                                       
BUFHI    DS    C                                                                
BUFCDE   DS    C                                                                
BUFRTYP  DS    C                                                                
LEVEL    DS    C                   LEVEL CODE                                   
ESTACT   DS    CL1                                                              
CPPSW    DC    C'D'                                                             
BRDPOLSW DC    X'00'                                                            
LVCNTRL  DC    F'1'                                                             
         DC    A(2,3,4,5)                                                       
CURPGPTR DS    F                   START OF PROGRAM SAVE AREA                   
CURPH01  DS    CL12                BUFFER ADDRESS/LEN/LOADER ADDRESS            
CURPH02  DS    CL12                                                             
CURPH04  DS    CL12                                                             
DNAMES   DS    0CL98                                                            
DNAME1   DS    CL7                                                              
DNAME2   DS    CL7                                                              
DNAME3   DS    CL7                                                              
DNAME4   DS    CL7                                                              
DNAME5   DS    CL7                                                              
DNAME6   DS    CL7                                                              
DNAME7   DS    CL7                                                              
DNAME8   DS    CL7                                                              
         DS    CL42                DEMOS 9-14                                   
SUBPROG  DS    0CL8                                                             
         DC    C'SP'                                                            
SUBPROG1 DC    C'M2'                                                            
SUBPROG2 DC    C'01'                                                            
         DC    C'  '                                                            
MOPT1    DC    C' '                                                             
MOPT2    DC    C' '                                                             
MOPT3    DC    C' '                                                             
MOPT4    DC    C' '                                                             
MOPT5    DC    C' '                                                             
MOPT6    DC    C' '                                                             
MOPT7    DC    C' '                                                             
DASH     DC    80C'-'                                                           
MYBUFIO  DS    CL200                                                            
FIRST    DS    C                                                                
PASS     DS    C                                                                
MAXPASS  DS    C                   HIGHEST REQUIRED PASS                        
NUMCOM   DS    X'00'               CURRENT NUMBER OF COMMENT LINES              
SCNDDTSW DC    X'01'               PRINT DATE ON SECOND LINE                    
IDSW     DS    C                                                                
CONEND   DS    C                   END OF CONTRACT SWITCH                       
IDTOTALS DS    C                   PRINT ID TOTALS SWITCH                       
FBSTA    DS    C                                                                
PRVSTA   DS    CL7                                                              
PRTLINE  DS    CL150                                                            
VUDESC   DS    F'0'                                                             
VCALCPP  DC    F'0'                                                             
VSUMMRY  DC    F'0'                                                             
PSTASPT  DC    F'0'                                                             
PSTACOST DC    F'0'                                                             
APSTAGRD DC    F'0'                                                             
DSTAGRID DC    F'0'                A(DETAIL STATION GRID PRINT)                 
DDESC    DC    F'0'                A(DETAIL DESCRIPTION PRINT)                  
DTOTSPT  DC    F'0'                A(DETAIL TOTAL SPOTS)                        
MSBFHOOK DC    F'0'                                                             
REPCALOV DC    F'0'                                                             
SAVER9   DC    F'0'                                                             
HKPRDRD  DC    F'0'                SAVE MEDPRDRD ADDRESS                        
ATRAPBUY DC    F'0'                                                             
APTSDESC DC    F'0'                                                             
ASVFRST  DS    F                                                                
ASVLAST  DS    F                                                                
ASVLCHNK DS    F                                                                
SVAREC   DS    F                                                                
SVADBUY  DS    F                                                                
SVGETBY  DS    F                                                                
SVMSTAX  DS    CL4                                                              
SVMEDBYD DS    CL8                                                              
SVEXTAX  DS    CL1                                                              
ASVPRD   DS    C                                                                
PENNYSW  DS    C                                                                
SPACESW  DS    CL1                                                              
EXTSW    DS    C                                                                
MGMSDSW  DC    X'00'                                                            
NETSW    DS    C                   FIRST TIME FOR NETWORK BUY                   
HLDPNAM  DS    CL14                                                             
HPSNO    DS    C                                                                
LASTGSLT DS    F                                                                
HLDBOOK  DS    CL2                                                              
         DS    CL12                                                             
         DS    0F                                                               
SALSDATA DS    0CL28               SALESPERSONS WORK AREA                       
SALSWKS  DS    F                                                                
SALSSPT  DS    F                                                                
SALSDL   DS    F                                                                
SALSD1   DS    F                                                                
SALSD2   DS    F                                                                
SALSD3   DS    F                                                                
SALSD4   DS    F                                                                
*                                                                               
PRTADDR  DS    F                                                                
SVPH01   DS    F                                                                
SVPH02   DS    F                                                                
SVPH04   DS    F                                                                
SVSPECS  DS    F                                                                
SVMDTAB  DS    F                                                                
SVRDTE   DS    F                                                                
MSSTART  DS    CL12                                                             
N5START  DS    CL12                                                             
TAXAMT   DC    F'0'                                                             
VMDADDWT DC    F'0'                                                             
VSTATOT  DC    F'0'                                                             
VEDTDEMS DC    F'0'                                                             
VGETBUF  DC    F'0'                                                             
VSUBPARA DC    F'0'                                                             
VCOMPRNT DS    F                                                                
PGHILNO  DS    F                   HIGHEST LINE NUMBER                          
PGNOENT  DS    F                                                                
PGCNDSW  DS    C                   CONDENSE LIKE SPOTS                          
PGCURLNO DS    F                   CURRENT LINE #                               
PGWMAX   DS    F                   MAXIMUM SLOTS                                
VARFRMT  DS    C                   VARIABLE FORMAT                              
AHDATES  DC    F'0'                                                             
SVRCSUB  DS    C                                                                
MSRCSUB  DS    C                                                                
SVSUPMKT DS    C                                                                
MSSUPMKT DS    C                                                                
DPTSW    DS    C                   DAYPART CHANGE SWITCH                        
MRPTTYP  DS    C                                                                
BUYACT   DS    C                                                                
MKTACT   DS    C                                                                
CFDS     DS    C                                                                
CFDE     DS    C                                                                
OVRFLAG  DS    0CL1                                                             
OVRFLAGS DS    CL16                                                             
STRDTE   DS    CL2                                                              
ENDDTE   DS    CL2                                                              
ELEMDT   DS    XL2                                                              
ELEMNO   DS    XL1                                                              
SVMGC1   DS    F                                                                
SVMGC2   DS    F                                                                
SVPGC1   DS    F                                                                
SVPGC2   DS    F                                                                
MSHDHOOK DC    F'0'                                                             
SVHDHOOK DC    F'0'                                                             
VGETREP  DC    F'0'                                                             
VFOOT    DC    F'0'                                                             
VREVBUY  DC    F'0'                                                             
SVSGRID  DC    F'0'                                                             
VMRGPL   DC    F'0'                                                             
MGLENLIN DS    H                                                                
PBDREP   DC    C'N'                POST BUY DEMO OVERRIDE REPORT                
*                                                                               
* DETAIL OPTIONS - CONTROLLED BY MOPT2, SETS DETOPTS                            
*                     1=YES,0=NO                                                
*                 1    PRINT '*' NEXT TO OVERRIDEN DEMOS                        
*                 2    PRINT DEMO VALUES                                        
*                 3    PRINT COST                                               
*                 4    PRINT CPP/CPM                                            
DETOTAB  DC    AL1(1,1,1,1)        OPTION TABLE                                 
         DC    AL1(0,1,1,1)                                                     
         DC    AL1(0,0,1,1)                                                     
         DC    AL1(0,1,0,0)                                                     
         DC    AL1(1,1,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
*                                                                               
* TOTAL OPTIONS - CONTROLLED BY MOPT5                                           
*                        1=YES,0=NO                                             
*                                                                               
*               FIELD  OPTION                                                   
*               -----  ------                                                   
*                 1    PRINT TELECASTS                                          
*                 2    PRINT DOLLARS                                            
*                 3    PRINT DEMOS                                              
*                                                                               
SUMOTAB  DC    AL1(1,1,1)                                                       
         DC    AL1(1,1,0)                                                       
         DC    AL1(1,0,0)                                                       
         DC    AL1(1,0,1)                                                       
         DC    AL1(0,0,1)                                                       
         DC    AL1(0,1,0)                                                       
         DC    AL1(0,1,1)                                                       
         DC    AL1(0,0,0)                                                       
*                                                                               
* DATE OPTIONS - CONTROLLED BY PROFDCTL                                         
*                     SETS DATEOPT, VARFRMT AND SCNDDTSW                        
DATEOTAB DC    X'00',AL1(0,0)                                                   
         DC    C' ',AL1(0,0)                                                    
         DC    C'0',AL1(0,0)                                                    
         DC    C'1',AL1(0,1)                                                    
         DC    C'2',AL1(1,0)                                                    
         DC    C'3',AL1(1,1)                                                    
         DC    C'4',AL1(2,0)                                                    
         DC    C'5',AL1(2,1)                                                    
         DC    X'FF',AL1(0,0)                                                   
* SORT OPTIONS      0 = NO SORT                                                 
*                   1 = DAY/TIME/PROGRAM                                        
*                   2 = TIME/DAY/PROGRAM                                        
*                   3 = PROGRAM/DAY/TIME                                        
SORTOTAB DC    C' ',AL1(0)                                                      
         DC    C'1',AL1(1)                                                      
         DC    C'2',AL1(2)                                                      
         DC    C'3',AL1(3)                                                      
         DC    X'FF',AL1(0)                                                     
*                                                                               
* REPORT FORMAT OPTIONS            SET FRMTOPT                                  
*                   FIELD1 = LENGTH OF GRID                                     
*                   FIELD2 = NUMBER IN GRID FOR PTS                             
*                   FIELD3 = NUMBER IN GRID FOR RS                              
*                   FIELD4 = DEMOGRAPHIC FORMAT FOR PTS                         
*                                                                               
FRMTOTAB DC    X'00',AL1(4,14,14,2)                                             
         DC    C'0',AL1(4,14,14,2)                                              
         DC    C'1',AL1(6,10,11,2)                                              
         DC    C'2',AL1(10,6,6,2)                                               
*        DC    C'3',AL1(13,5,5,2)                                               
         DC    C'3',AL1(13,4,5,2)  MADE 4 REPS(*13=52 CHS) 5=65 & CRAPD         
         DC    C'4',AL1(4,19,14,1)                                              
         DC    C'5',AL1(6,13,11,1)                                              
         DC    C'6',AL1(10,8,6,1)                                               
         DC    C'7',AL1(13,6,6,1)                                               
         DC    X'FF',AL1(4,14,14,2)                                             
*                                                                               
* CONDENSE OPTIONS       SETS CNDSOPT / PGCNDSW                                 
*                                                                               
CNDSOTAB DC    X'00',AL1(0)                                                     
         DC    C'1',AL1(1)                                                      
         DC    C'2',AL1(2)                                                      
         DC    X'FF',AL1(0)                                                     
         SPACE                                                                  
* START OF CLEARED AREA!                                                        
CLRSTART DS    0C                                                               
*                                  OPTIONS                                      
DETOPTS  DS    0XL4                                                             
DETOFLAG DS    X                                                                
DETODEMO DS    X                                                                
DETOCOST DS    X                                                                
DETOCPM  DS    X                                                                
SUMOPTS  DS    0XL3                                                             
SUMOSPOT DS    X                                                                
SUMODOL  DS    X                                                                
SUMODEMO DS    X                                                                
DATEOPT  DS    CL2                                                              
SORTOPT  DS    CL1                                                              
FRMTOPT  DS    0XL4                                                             
FRMTWIDE DS    X                                                                
FRMT#PTS DS    X                                                                
FRMT#RS  DS    X                                                                
FRMTDEMO DS    X                                                                
CNDSOPT  DS    CL1                                                              
*                                                                               
SVDEMS   DS    0F                                                               
SVD1     DS    F                   DEMO 1 VALUE                                 
SVD1CP   DS    F                   DEMO 1 CPP/CPM                               
SVDEMNLQ EQU   *-SVD1              ENTRY LENGTH                                 
SVD2     DS    F                   DEMO 2 VALUE                                 
SVD2CP   DS    F                   DEMO 2 CPP/CPM                               
SVD3     DS    F                   DEMO 3 VALUE                                 
SVD3CP   DS    F                   DEMO 3 CPP/CPM                               
SVD4     DS    F                   DEMO 4 VALUE                                 
SVD4CP   DS    F                   DEMO 4 CPP/CPM                               
         DS    CL80                DEMOS 5-14 VALUE / CPP-M                     
SVDEMSLQ EQU   *-SVDEMS                                                         
SVDG     DS    F                                                                
SVDGCP   DS    F                                                                
*                                                                               
ACTMO    DS    F                                                                
UNIVERSE DS    F                                                                
WEIGHT   DS    F                                                                
MCOUNT   DS    F                                                                
NODEMS   DS    F                                                                
NORTGS   DS    F                                                                
PLDEMS   DS    0CL168                                                           
PLD1     DS    CL6                                                              
PLD1CP   DS    CL6                                                              
PLDEMNLQ EQU   *-PLD1              ENTRY LENGTH                                 
PLD2     DS    CL6                                                              
PLD2CP   DS    CL6                                                              
PLD3     DS    CL6                                                              
PLD3CP   DS    CL6                                                              
PLD4     DS    CL6                                                              
PLD4CP   DS    CL6                                                              
         DS    CL120                                                            
STAGRID  DS    56F                                                              
*                                                                               
STAACC   DS    0F                                                               
STASPOT  DS    F                   STATION TOTAL SPOTS                          
STACOST  DS    F                   STATION TOTAL DOLLARS                        
STACOSTE DS    F                   STATION TOTAL DOLLARS EQUIV                  
STADEMS  DS    (MAXDEMOQ*2)F       STATION TOTAL DEMOS (STD + EQUIV)            
STADMSLQ EQU   *-STADEMS           L'DEMO ACCUMULATORS                          
         ORG   STADEMS                                                          
STADEMO  DS    F                   DEFINE 1ST ENTRY FOR REF                     
STADEMOE DS    F                                                                
STADMNLQ EQU   *-STADEMO           L'DEMO ENTRY                                 
         ORG                                                                    
STAACCLQ EQU   *-STAACC            L'STA ACCUMULATORS                           
STAACCSQ EQU   STAACCLQ/L'STAACC   N'ACCS (SPOT/COST/COSTE+DEMOS)               
*                                                                               
STTACC   DS    0F                                                               
STTSPOT  DS    F                   OVERALL STATION SPOTS                        
STTCOST  DS    F                   OVERALL STATION DOLLARS                      
STTCOSTE DS    F                   OVERALL STATION DOLLARS EQUIV                
STTDEMS  DS    (MAXDEMOQ*2)F       OVERALL STATION DEMOS (STD + EQUIV)          
STTDMSLQ EQU   *-STTDEMS           L'ACCUMULATORS                               
         ORG   STTDEMS                                                          
STTDEMO  DS    F                   DEFINE 1ST ENTRY FOR REF                     
STTDEMOE DS    F                                                                
STTDMNLQ EQU   *-STTDEMO           L' DEMO ENTRY                                
         ORG                                                                    
STTACCLQ EQU   *-STTACC            L'STT ACCUMULATORS                           
STTACCSQ EQU   STTACCLQ/L'STTACC   N'ACCS (SPOT/COST/COSTE+DEMOS)               
*                                                                               
FILMNO   DS    CL8                                                              
STACAP   DS    CL7                                                              
MSOPT    DS    CL7                                                              
SVOPTS   DS    CL7                                                              
N5OPTS   DS    CL7                                                              
SUBPSW   DS    CL1                                                              
MSPROF   DS    CL16                                                             
SVPROF   DS    CL16                                                             
SVSPPROF DS    CL16                                                             
MSSPPROF DS    CL16                                                             
PASSSD3  DS    XL3                                                              
PASSED3  DS    XL3                                                              
PASSSD2  DS    XL2                                                              
PASSED2  DS    XL2                                                              
PKGAREA  DS    CL16                BUY TYPE CAPTIONS                            
PIGAREA  DS    CL32                PIGGYBACK AREA                               
PIGAREAL DS    CL3                 PIGGYBACK LENGTH                             
PIGPRNT  DS    CL11                PIGGYBACK PRINT                              
INVFILM  DS    C                                                                
SPLPRINT DS    C                   PRINT ORIGINATING SWITCH                     
SPBUFSTA DS    CL90                SAVE AREA FOR ORIG. STATIONS                 
SPBUFMKT DS    CL240               SAVE AREA FOR ORIG. MARKETS                  
SPSTPL   DS    CL132                                                            
PTSSPILL DS    C                                                                
         DS    0F                                                               
PREMTAB  DS    CL64                                                             
HIATAB   DS    CL64                                                             
COVRHLD  DS    396C                COST OVERRIDE HOLD AREA                      
COVRFRST DS    C                   COST OVR FIRST TIME                          
OVRCNT   DS    C                   PL OVERRIDE COUNT                            
SVMAXLIN DS    C                                                                
MGSW     DS    C                                                                
APL      DS    F'0'                A(PL) FOR COST OVERRIDES                     
SVQEND   DS    CL6                                                              
SVSPREP  DS    H                                                                
SVP1     DS    CL40                                                             
SVP2     DS    CL40                                                             
SVP3     DS    CL40                                                             
SVP4     DS    CL40                                                             
NUMWK    DS    F                                                                
NOINGRID DS    H                                                                
LENGRID  DS    C                                                                
HLDNOSP  DS    C                                                                
CURRSORT DS    CL20                                                             
NEXTSORT DS    CL20                                                             
OPTRMODE DS    C                                                                
HADOFLOW DS    C                   'Y' IF $ ACCUM OVERFLOW                      
CLREND   DS    0C                                                               
* END OF CLEARED AREA!                                                          
OPTRPT   DC    F'0'                                                             
STAUNA   DC    F'0'                                                             
UNATOT   DC    F'0'                                                             
PDNCNTR  DC    F'0'                                                             
* ADDRESS STORAGE CSECTS                                                        
VPNTABLE DC    F'0'                A(PNTABLE)                                   
VSSTABLE DC    F'0'                A(SSTABLE)                                   
* ADDRESS LOCAL ROUTINE CSECTS                                                  
*                                                                               
SSCNTR   DC    F'0'                                                             
SVREPORT DC    F'0'                                                             
LOCLADDR DC    F'0'                                                             
NUMCI    DC    H'0'                                                             
CIPRTSW  DC    X'00'                                                            
SORTPASS DS    C                                                                
SORTINPQ EQU   1                   INPUT/BUILD                                  
SORTGETQ EQU   2                   GET/EXTRACT                                  
SORTENDQ EQU   3                   END OF RECORDS                               
SORTREQ  DS    C                                                                
SORTFRMT DS    C                                                                
CURRPNUM DS    C                                                                
THISPRD  DS    CL3                                                              
PASSTAB  DS    CL144               LIST OF PASS START-END DATES                 
PASSQST  DS    CL12                THIS PASS START-END                          
REASTART DS    CL12                REQUEST START-END DATES                      
SVMID1   DS    CL132                                                            
         DS    0F                                                               
SVLOCAL  DS    CL124               SAVE LOCAL STATION TOTALS                    
*                                                                               
PSLIST   DS    100C                                                             
PGELEM   DS    CL30                                                             
SAKYSAVE DS    CL32                                                             
RPKYSAVE DS    CL32                                                             
SRKYSAVE DS    XL32                                                             
SORTKLEN DC    F'0'                SORT KEY LENGTH                              
SORTRLEN DC    F'0'                SORT RECORD LENGTH                           
DADRDISP DC    F'0'                DISK ADDRESS DISPLACEMENT                    
NEXTSSLT DC    F'0'                NEXT SORT SLOT                               
SOUTFRST DC    X'01'               FIRST TIME SWITCH (SORT)                     
MSREQSW  DS    C                                                                
CIKEY    DS    CL20                                                             
SDKEY    DS    CL15                SOFT DEMO OVERHEAD KEY                       
ANXTCI   DS    F                   A(NEXT CUTIN)                                
*<<<                                                                            
COMAREA  DS    CL400               COMMENT AREA                                 
COMAREAX EQU   *                                                                
         DS    0D                                                               
BUYRECIO DS    4000C                                                            
         DS    0D                                                               
CULIN    DS    2000C                                                            
         DS    0D                                                               
CINPRD   DS    209XL(CINPRNLQ)                                                  
CINPRDX  EQU   *                                                                
         DS    0D                                                               
CISPRD   DS    209XL(CISPRNLQ)      MAX # OF SPOTS IS 209                       
CISPRDX  EQU   *                                                                
         DS    0D                                                               
CISLIST  DS    2000C                                                            
CISLISTX EQU   *                                                                
         DS    0D                                                               
CICLIST  DS    30000C                                                           
CICLISTX EQU   *                                                                
         DS    0D                                                               
SPLIST   DS    1200C                                                            
SPLISTX  EQU   *                                                                
         DS    0D                                                               
LOCLTBLQ EQU   90*LOCDEFLQ   MAX ENTRIES(88 ON DEMOVER SCREEN)*L'ENTRY          
LOCLTAB  DS    (LOCLTBLQ)C                                                      
*                                                                               
PNTABLE  DS    0D                  *** PROG NAME TABLE ***                      
PNTABLQ  EQU   4600            OK FOR 746 ENTRIES BUT ONLY SUPPORT 255!         
PNTABMXQ EQU   255                 (1 BYTE IN SORTREC)                          
         DS    (PNTABLQ)C                                                       
*                                                                               
SSTABLE  DS    0D                  *** SORT SEQ TABLE ***                       
SSTABLQ  EQU   7100                                                             
SSTABMXQ EQU   (SSTABLQ/SQMAXLQ)-1     MAX ENTRIES THAT WILL FIT                
         DS    (SSTABLQ)C                                                       
*                                                                               
         DS    0D                                                               
PGRID    DS    15000C                                                           
PGRIDX   EQU   *                                                                
*                                                                               
         DS    0D                  NETWORK COST OVERRIDE TABLE                  
NETCOVRD DS    200XL6              DATE/SPOTNUM/OVRD AMT                        
NETCOVRX EQU   *                                                                
*                                                                               
SVMDBLK  DS    0D                                                               
         DS    1272C               SAVE MEDIA SUMMARY MEDBLOCK                  
SVMDBLKX EQU   *                                                                
*                                                                               
SVPRDBUF DS    0D                                                               
         DS    (220*PTBUFFL)C      MAX PRDS * (SPOTPAK) L'ENTRY                 
SVPRDBFX EQU   *                                                                
*                                                                               
PLAREA   DS    0D                                                               
         DS    8000C                                                            
PLAREAX  EQU   *                                                                
         DS    0D                                                               
SUBPAREA DS    90000C                                                           
         EJECT                                                                  
**********************************************************************          
* VARIOUS LOCAL EQUATES                                              *          
**********************************************************************          
         SPACE                                                                  
MAXDEMOQ EQU   MAXDAUDQ+MAXDRTGQ   1ST 4 AUD & POSSIBLY 4 RTG DEMOS             
MAXDAUDQ EQU   4                   4 AUD DEMOS (STD REPORT)                     
MAXDRTGQ EQU   4                   4 RTG DEMOS (SHOW IN LOCAL ONLY)             
OVRDINDQ EQU   C'*'                OVERRIDE INDICATOR                           
RTGDEMOQ EQU   C'R'                RATING TYPE DEMO PREFIX                      
EXTDEMOQ EQU   C'E'                EXTENDED MKT RATING TYPE DEMO PREFIX         
YESSETQ  EQU   1                   'YES' SETTING                                
NOSETQ   EQU   0                   'NO' SETTING                                 
* FOLLOWING ARE LOCAL EQUATES FOR VALUES WHICH REALLY SHOULD                    
* BE IN RESPECTIVE PANBOOKS/DSECTS BUT I'M NOT GONNA PUT THEM THERE             
* AS DON'T WANT TO UPSET ANYBODY                                                
MEDBY_MAXDEMOQ EQU 14              MAX DEMOS SUPPORTED BY SPMEDBLK              
PRDBUF_MAXPRDQ EQU 220             MAX PRODUCTS IN PRDBUFF TABLE                
BUYREC_BUYEL1DQ EQU 24             DISPLACEMENT TO FIRST ELEM IN BUYREC         
         SPACE                                                                  
**********************************************************************          
* VARIOUS LOCALLY DEFINED DSECTS                                     *          
**********************************************************************          
         SPACE                                                                  
PASSTABD DSECT                     SAVE MEDBLOCKS FOR PASSES                    
PASSSD   DS    CL6                 START DATE                                   
PASSED   DS    CL6                 END DATE                                     
* PASSP1   DS    CL28                MEDBLOCK LEADER                            
* PASSP2   DS    CL168               WEEKS                                      
* PASSP3   DS    CL48                MONTHS                                     
* PASSP4   DS    CL12                PERIOD                                     
* PASSEND  DS    0C                                                             
         SPACE                                                                  
LOCDEFD  DSECT                     *** LOCLTAB ENTRY DEFINITION ***             
LOCMS    DS    CL5                 MARKET/STATION                               
LOCFLAGS DS    0XL2                                                             
LOCSIND  DS    XL1                                                              
LOCSPLQ  EQU   X'80'               - SPILL                                      
LOCOVRD  DS    XL1                                                              
* FLAGS BELOW INDICATE - POST BUY DEMO OVERRIDES FOR PBD REPORT                 
*                      - ESTIMATED DEMO OVERRIDES FOR STD REPORT                
LOC1OVRQ EQU   B'10000000'         - LOCBY1 DEMOS ARE OVERRIDES                 
LOC2OVRQ EQU   B'01000000'         - LOCBY2 DEMOS ARE OVERRIDES                 
LOC3OVRQ EQU   B'00100000'         - LOCBY3 DEMOS ARE OVERRIDES                 
LOC4OVRQ EQU   B'00010000'         - LOCBY4 DEMOS ARE OVERRIDES                 
*                      - THESE CAN ONLY BE FOR RATINGS (IF SET)                 
LOC5OVRQ EQU   B'00001000'         - LOCBY5 DEMOS ARE OVERRIDES                 
LOC6OVRQ EQU   B'00000100'         - LOCBY6 DEMOS ARE OVERRIDES                 
LOC7OVRQ EQU   B'00000010'         - LOCBY7 DEMOS ARE OVERRIDES                 
LOC8OVRQ EQU   B'00000001'         - LOCBY8 DEMOS ARE OVERRIDES                 
*                                                                               
LOCACCSQ EQU   9                   NUMBER OF LOCAL ACCUMULATORS                 
LOCBYD   DS    XL(L'MEDBYD)        DOLLARS                                      
LOCBY1   DS    XL(L'MEDBY1)        DEMOS                                        
LOCBY2   DS    XL(L'MEDBY2)                                                     
LOCBY3   DS    XL(L'MEDBY3)                                                     
LOCBY4   DS    XL(L'MEDBY4)                                                     
LOCBY5   DS    XL(L'MEDBY5)                                                     
LOCBY6   DS    XL(L'MEDBY6)                                                     
LOCBY7   DS    XL(L'MEDBY7)                                                     
LOCBY8   DS    XL(L'MEDBY8)                                                     
LOCDEFLQ EQU   *-LOCDEFD                                                        
         SPACE 2                                                                
BPRTD    DSECT                                                                  
BPRTSPT  DS    CL10                 0                                           
BPRTD1   DS    CL7                 11                                           
BPRTDL   DS    CL9                 18                                           
BPRTD1C  DS    CL8                 27                                           
BPRTD2   DS    CL7                 35                                           
BPRTD2C  DS    CL8                 43                                           
BPRTD3   DS    CL7                 51                                           
BPRTD3C  DS    CL8                 58                                           
BPRTD4   DS    CL7                 68                                           
BPRTD4C  DS    CL8                 76                                           
         EJECT                                                                  
PGRIDD   DSECT                                                                  
PGSORT   DS    CL11                                                             
PGSORTLQ EQU   L'PGSORT                                                         
PGLINNO  DS    C                   PRINT BLOCK LINE NUMBER                      
PGSUBLI  DS    C                   SUB-LINE NUMBER                              
PGLSLOT  DS    C                   PRINT BLOCK SLOT NUMBER                      
PGDWK    DS    CL2                 WEEK OF                                      
PGDIND   DS    CL1                 REG/MISSD/MG INDICATOR                       
PGDIMISQ EQU   X'01'               - MISSED                                     
PGDIPREQ EQU   X'04'               - PRE-EMPTED / MG                            
PGDIHIAQ EQU   X'08'               - HIATUS                                     
PGDSBRN  DS    CL1                 SORT BRAND                                   
PGDSSLN  DS    CL1                 SORT SPOT LENGTH                             
PGDSNO   DS    CL1                 SORT SPOT NUMBER                             
PGDLN1Q  EQU   *-PGLINNO                                                        
PGDELAD  DS    CL4                 ELEMENT ADDRESS                              
PGDFDAY  DS    CL1                                                              
PGDFNO   DS    CL2                                                              
PGDNOSP  DS    CL1                 NUMBER OF SPOTS                              
PGD2BRN  DS    CL1                 PIGGYBACK BRAND                              
PGD2SLN  DS    CL1                 PIGGYBACK SPOT LENGTH                        
PGD2COVR DS    CL3                 COST OVERRIDES                               
PGRIDLQ  EQU   *-PGSORT                                                         
         SPACE 2                                                                
PGSRT1D  DSECT                                                                  
PGDS1WK  DS    CL2                                                              
PGDS1DY  DS    C                                                                
PGDS1SLT DS    C                                                                
PGDS1BR  DS    CL1                                                              
PGDS1SL  DS    CL1                                                              
PGDS1IND DS    CL1                                                              
         SPACE 2                                                                
PGSRT2D  DSECT                                                                  
PGDS2SLT DS    CL1                                                              
PGDS2WK  DS    CL2                                                              
PGDS2DY  DS    C                                                                
PGDS2SNO DS    C                                                                
PGDS2BR  DS    CL1                                                              
PGDS2SL  DS    CL1                                                              
PGDS2IND DS    CL1                                                              
         EJECT                                                                  
PROFDSCT DSECT                  ***PROGRAM PROFILE 1 DSECT***                   
PROFSORT DS    CL1              0  SORT CONTROL                                 
PROFFRMT DS    CL1              1  PRINT FORMAT                                 
PROFCNDS DS    CL1              2  CONDENSE CONTROL                             
PROFDCTL DS    CL1              3  DATE CONTROL                                 
PROFDPC  DS    CL1              4  DETAIL PRINT CONTROL                         
PROFCC   DS    CL1              5  COMMENT CONTROL                              
         DS    CL1              6  (CUT INS)                                    
PROFMSR  DS    CL1              7  MKT/STN RECAP ??? = COST O/RIDES             
PROFMPC  DS    CL1              8  MARKET (NWK RECAP) PRINT CONTROL             
PROFDPT  DS    CL1              9  MARKET (NWK RECAP) DPT PRINT CTL             
PROFMTR  DS    CL1             10  MARKET (NWK RECAP) TOTAL REPORT              
*        EQU   C'0'                - ??????, (CODE IMPLIES = NONE)              
*        EQU   C'1'                - SALSUM, DFLT                               
*        EQU   C'2'                - BTSSUM                      ?              
*        EQU   C'3'                - BRSSUM                      ?              
*        EQU   C'4'                - BDSSUM                      ?              
         DS    CL1             11  (LOCAL DEMOS)                                
PROFBMS  DS    CL1             12  BRAND MEDIA SUMMARY NUMBER                   
*        EQU   C'0'                - NONE                                       
*        EQU   C'2'                - BRAND PERFORMANCE, M2 PROF, DFLT           
*        EQU   C'3'                - BRAND WEEKLY SUMARY, M3 PROF               
PROFPMS  DS    CL1             13  POL MEDIA SUMMARY NUMBER                     
*        EQU   C'0'                - NONE                                       
*        EQU   C'2'                - BRAND PERFORMANCE, M2 PROF                 
*        EQU   C'4'                - MARKET PERFORMANCE, M4 PROF, DFLT          
PROFID   DS    CL1             14  PRINT IDS ON DETAILS                         
         DS    CL3             15                                               
         EJECT                                                                  
PNAMD    DSECT                     *** BINSRCH PROGRAM TABLE ENTRY ***          
PNDCODE  DS    CL1                 PROGRAM NUMBER                               
PNDNAME  DS    CL17                                                             
PNAMDLQ  EQU   *-PNAMD                                                          
         SPACE 2                                                                
SQRECD   DSECT                     *** SORT SEQ TABLE RECORD ENTRY ***          
SQKEY    DS    0CL4                                                             
SQ1DAY   DS    CL1                 DAY                                          
SQ1TIME  DS    CL2                 START-END QUARTER HOURS                      
SQ1PNUM  DS    CL1                 PROGRAM NUMBER                               
         ORG   SQKEY                                                            
SQ2TIME  DS    CL2                                                              
SQ2DAY   DS    CL1                                                              
SQ2PNUM  DS    CL1                                                              
         ORG   SQKEY                                                            
SQ3PNUM  DS    CL1                                                              
SQ3DAY   DS    CL1                                                              
SQ3TIME  DS    CL2                                                              
         ORG   SQKEY+L'SQKEY                                                    
SQDADDR  DS    CL4                 DISK ADDRESS                                 
SQRECLQ  EQU   *-SQRECD            L'ENTRY                                      
SQMAXLQ  EQU   SQRECLQ             L'LONGEST POSSIBLE ENTRY                     
         EJECT                                                                  
* APPEARS TO BE DSECT TO COVER BUFFALO (SUMMARY) RECORDS                        
SUMDSECT DSECT                                                                  
SUMKEY   DS    0CL15                                                            
SUMCODE  DS    CL1                 X'90'                                        
* SUMSPLQ  EQU   X'88'               - SPILL                                    
* SUMORIQ  EQU   X'89'               - ORIGINAL                                 
* SUMTOTQ  EQU   X'90'               - TOTAL                                    
* SUMPSPLQ EQU   X'91'               - PRODUCT GROUP SPILL                      
* SUMPORIQ EQU   X'92'               - PRODUCT GROUP ORIGINAL                   
* SUMPTOTQ EQU   X'93'               - PRODUCT GROUP TOTAL                      
SUMDPGNO DS    CL1                 DAYPART GROUP NO.                            
SUMDPGRP DS    CL3                 DAYPART GROUP CODE                           
SUMDPNO  DS    CL1                 DAYPART NO.                                  
SUMDPART DS    CL3                 DAYPART CODE                                 
SUMSLN   DS    CL1                 SPOT LENGTH                                  
* SUMSLNOQ EQU   X'FE'               - ORIGINAL DATA                            
* SUMSLNSQ EQU   X'FD'               - SPILL DATA                               
SUMRTYP  DS    CL1                 1=WEEKLY,2=MONTHLY,3=PERIOD                  
SUMDT    DS    CL4                 START-END DATES(FFFF FOR TOTAL)              
SUMRPT   DS    CL1                 REPORT CODE                                  
*                                                                               
SUMDATA  DS    0C                                                               
SUMSPOTS DS    CL4                 SPOTS                                        
SUMDL    DS    CL4                 DOLLARS                                      
SUMDLEQ  DS    CL4                 DOLLARS EQU                                  
SUMDEMS  DS    0C                                                               
SUMD1    DS    CL4                 DEMO 1                                       
SUMD1EQ  DS    CL4                 DEMO 1 EQU                                   
SUMDMNLQ EQU   *-SUMD1             - L'DEMO ENTRY                               
SUMD2    DS    CL4                 DEMO 2                                       
SUMD2EQ  DS    CL4                 DEMO 2 EQU                                   
SUMD3    DS    CL4                 DEMO 3                                       
SUMD3EQ  DS    CL4                 DEMO 3 EQU                                   
SUMD4    DS    CL4                 DEMO 4                                       
SUMD4EQ  DS    CL4                 DEMO 4 EQU                                   
SUMD5    DS    CL4                 DEMO 5                                       
SUMD5EQ  DS    CL4                 DEMO 5 EQU                                   
SUMD6    DS    CL4                 DEMO 6                                       
SUMD6EQ  DS    CL4                 DEMO 6 EQU                                   
SUMD7    DS    CL4                 DEMO 7                                       
SUMD7EQ  DS    CL4                 DEMO 7 EQU                                   
SUMD8    DS    CL4                 DEMO 8                                       
SUMD8EQ  DS    CL4                 DEMO 8 EQU                                   
SUMDEND  EQU   *                   - END OF DEMOS                               
SUMDEMLQ EQU   *-SUMD1             - TOTAL L'ALL DEMO ENTRIES                   
SUMGDL   DS    CL4                 GOAL $                                       
SUMGDLE  DS    CL4                 GOAL $ EQU                                   
SUMGD1   DS    CL4                 GOAL DEMO                                    
SUMGD1E  DS    CL4                 GOAL DEMO EQU                                
SUMDATLQ EQU   *-SUMDATA           - L'DATA FIELDS                              
SUMDLENQ EQU   *-SUMDSECT                                                       
         SPACE                                                                  
CICLISTD DSECT                     *** CUTIN LIST DSECT ***                     
CICLMS   DS    0XL5                MARKET/STATION                               
CICLMKT  DS    XL2                 MARKET                                       
CICLSTA  DS    XL3                 MARKET                                       
CICLSPTN DS    XL1                 SPOT NUMBER                                  
CICLPRD1 DS    XL1                 PRD1                                         
CICLNEGQ EQU   254                 - CUTIN NEGATED                              
CICLPRD2 DS    XL1                 PRD2                                         
CICLCOST DS    XL3                 CUTIN COST OVERRIDE                          
CICLSNLQ EQU   *-CICLISTD          L'ENTRY                                      
         SPACE                                                                  
CISTLSTD DSECT                     *** CUTIN STATION LIST DSECT ***             
CISTLMS  DS    XL5                 MARKET/STATION                               
CISTLNLQ EQU   *-CISTLSTD          L'ENTRY                                      
         SPACE                                                                  
CINPRDD  DSECT                     *** CUTIN NETWORK PRODUCT LIST DSECT         
CINPSPTN DS    XL1                 RELATIVE SPOT NUMBER                         
CINPPRD1 DS    XL1                 PRD1                                         
CINPPRD2 DS    XL1                 PRD2                                         
CINPRNLQ EQU   *-CINPRDD           L'ENTRY                                      
         SPACE                                                                  
CISPRDD  DSECT                     *** CUTIN STATION PRODUCT LIST DSECT         
CISPSPTN DS    XL1                 SPOT NUMBER?                                 
CISPPRD1 DS    XL1                 PRD                                          
CISPPRD2 DS    XL1                 PRD2                                         
CISPCOST DS    XL3                 COST OVERRIDE                                
CISPRNLQ EQU   *-CISPRDD           L'ENTRY                                      
         SPACE                                                                  
SPLISTD  DSECT                     *** CUTIN SPOT LIST DSECT ***                
SPLSDATE DS    XL2                 RELATIVE DATE                                
SPLSPRD1 DS    XL1                 RELATIVE PRD1                                
SPLSPRD2 DS    XL1                 RELATIVE PRD2                                
SPLISNLQ EQU   *-SPLISTD           L'ENTRY                                      
         SPACE                                                                  
**********************************************************************          
* PRINT LINE DSECTS (PLINED'S)                                                  
**********************************************************************          
         SPACE                                                                  
PPTSLIND DSECT                     *** MAIN POOL TIME SHEET SECTION ***         
*                                  -- BUY DETAILS PORTION (LEFT) --             
*                                  -- ROTATION GRID PORTION (CENTRE) --         
*                                  -- DEMOS PORTION (RIGHT) --                  
         ORG   PPTSLIND+93         2-UP DEMOS                                   
PPTSNAM2 DS    CL7            +93  (DNAME1)                                     
         DS    CL1                                                              
PPTSDEM2 DS    CL5            +101 (PLD1+1)                                     
PPTSFLG2 DS    CL1            +106 (OVRFLAG)                                    
PPTSCPM2 DS    CL5            +107 (PLD1CP+1)                                   
         DS    CL1                                                              
PPTSNAM1 DS    CL7            +113 1-UP DEMOS                                   
         DS    CL1                                                              
PPTSDEM1 DS    CL5            +121                                              
PPTSFLG1 DS    CL1            +126                                              
PPTSCPM1 DS    CL5            +127                                              
         ORG   PPTSLIND+(L'P-(*-PPTSLIND)) ERROR IF EXCEED L'PRINTLINE          
         SPACE                                                                  
PSTOTLND DSECT                     *** POOL TIME SHEET STATION TOTAL **         
*        LEFT SECTION                                                           
PSTOBYID DS    0CL12               BUYID ROW REDEFINITION                       
PSTOROW  DS    CL9                 ROW DESCRIPTION                              
         DS    CL1                                                              
PSTOROWX DS    CL3            +10  EXTRA ROW DESCRIPTION                        
         DS    CL1                                                              
PSTOPERS DS    CL8            +14  PERIOD START                                 
PSTOPERD DS    CL1            +22  "-"                                          
PSTOPERE DS    CL8            +23  PERIOD END                                   
         ORG   PSTOTLND+14         REDEFINITION FOR 2ND LINE                    
PSTOCOST DS    CL10           +14  SPOT TOTAL                                   
         ORG   PSTOTLND+20         REDEFINITION FOR 3RD LINE                    
PSTOSPOT DS    CL4            +20  SPOT TOTAL                                   
         DS    CL1                                                              
PSTOSPTX DS    0C             +25  SPOT TOTAL TEXT                              
*        CENTRE SECTION                                                         
         ORG   PSTOTLND+31                                                      
PSTOGRID DS    0CL4                                                             
PSTOGMXQ EQU   14                  (? MAYBE=NUMBER OF MEDMON__ FIELDS)          
         ORG   PSTOGRID+(PSTOGMXQ*L'PSTOGRID)                                   
*        RIGHT SECTION                                                          
         ORG   PSTOTLND+93                                                      
PSTONAM1 DS    CL7            +93  DEMO NAME 1                                  
PSTODEM1 DS    CL6            +100 DEMO VALUE 1                                 
         DS    CL1                                                              
PSTOCPM1 DS    CL5            +107 CPM VALUE 1                                  
PSTOEQIV DS    CL1            +112 EQUIVALENCE IND                              
PSTONAM2 DS    CL7            +113 DEMO NAME 2                                  
PSTODEM2 DS    CL6            +120 DEMO VALUE 2                                 
         DS    CL1                                                              
PSTOCPM2 DS    CL5            +127 CPM VALUE 2                                  
         ORG   PSTOTLND+(L'P-(*-PSTOTLND)) ERROR IF EXCEED L'PRINTLINE          
         SPACE                                                                  
PLOCALD  DSECT                     *** LOCAL DEMOS SECTION ***                  
* COLS WIDTHS TRIMMED TO BARE MINIMUM TO FIT AS MANY DETAIL COLS AS             
* POSSIBLE.                                                                     
* NOTE: DESC ROW 1 CAN EXCEED L'PLOCDESC & IF DOLLARS=L'10 CAN ABUTT            
         DS    CL4                                                              
*                 DESCRIPTION 'COLUMN' COVERS UP TO MAXDEMOQ+2 PLINES!          
PLOCDESC DS    CL8            +4                                                
*                 DETAIL 'COLUMN' COVERS UP TO MAXDEMOQ+2 PLINES!               
PLOCDET  DS    CL10           +16                                               
PLOCDMXQ EQU   (L'P-(PLOCDET-PLOCALD))/L'PLOCDET  MAX# DETAIL COLUMNS           
         ORG   PLOCDET+(L'PLOCDET-L'PLOCSTA)  RIGHT JUSTIFY                     
PLOCSTA  DS    CL4                            STATION (LINE 1)                  
         ORG   PLOCDET+(L'PLOCDET-L'PLOCCBL)  RIGHT JUSTIFY                     
PLOCCBL  DS    CL7                            CABLE STATION (LINE 1)            
         ORG   PLOCDET+(L'PLOCDET-L'PLOCSPL)  RIGHT JUSTIFY                     
PLOCSPL  DS    CL5                            (LINE1,2)                         
         ORG   PLOCDET                                                          
PLOCDOL  DS    CL10                           (LINE2)                           
         ORG   PLOCDET+1                                                        
PLOCDEMO DS    CL8                            (LINE3,4,5)                       
PLOCDOVR DS    CL1                                                              
         SPACE                                                                  
PCUTIND  DSECT                     *** CUTINS SECTION ***                       
         DS    CL4                                                              
PCUTDESC DS    CL13           +4                                                
PCUTSTA  DS    CL4            +17                                               
PCUTSFX  DS    CL3            +21                                               
         ORG   PCUTSFX                                                          
         DS    CL1            +21                                               
PCUTDSC2 DS    CL2            +22                                               
         DS    CL2            +24                                               
PCUTDET  DS    0C             +26  DETAIL ENTRY                                 
PCUTDATE DS    CL5            +26/0   MMMDD                                     
PCUTDASH DS    CL1               /5                                             
PCUTPRD1 DS    CL3               /8                                             
PCUTPB   DS    CL1               /11                                            
PCUTPRD2 DS    CL3               /12                                            
         DS    CL2                                                              
PCUTDNLQ EQU   *-PCUTDATE          L'DETAIL ENTRY                               
PCUTDMXQ EQU   (L'P-(PCUTDET-PCUTIND))/PCUTDNLQ MAX# DETAIL ENTRIES             
*                                                                               
         ORG   PCUTIND             REDEFINITION FOR COST OVERRIDES              
         DS    CL6                                                              
PCORDESC DS    CL16                                                             
         SPACE                                                                  
PBTSLIND DSECT                     *** NETWORK RECAP (MONTHLY) ***              
         DS    CL7                                                              
PBTSMNTH DS    CL3                                                              
         DS    CL9                                                              
PBTSSPOT DS    CL10          +19                                                
         DS    CL4                                                              
PBTSDOL  DS    CL9           +33                                                
         DS    CL2                                                              
PBTSDATA DS    0CL20         +44                                                
PBTSDMXQ EQU   4                   MAX 4 SETS OF DATA COLUMNS                   
PBTSDEMO DS    CL7                 - DEMO VALUE                                 
         DS    CL2                                                              
PBTSCPM  DS    CL7                 - CPM VALUE                                  
PBTSCPME DS    CL1                 - EQUIV $ FLAG                               
         DS    CL3                                                              
         SPACE                                                                  
*BDSLIND DSECT                     *** NETWORK RECAP (WEEKLY) ***               
         SPACE                                                                  
PSALLIND DSECT                     *** NETWORK RECAP (PERIOD) ***               
PSALROW  DS    CL8                                                              
         ORG   PSALLIND+30                                                      
PSALSPOT DS    CL8           +30                                                
         DS    CL9                                                              
PSALDATA DS    0CL15         +47                                                
PSALDMXQ EQU   4                   MAX 4 SETS OF DATA COLUMNS                   
PSALDEMO DS    CL9           +47   DEMO VALUE                                   
         DS    CL6                 (SPARE)                                      
         ORG   PSALDATA+2          REDEFINITION FOR COL HEADING                 
PSALRTG  DS    CL7           +49                                                
         ORG   PSALLIND+44                                                      
PSALAUD  DS    CL7           +44                                                
PSALAUDT DS    CL5           +51                                                
         ORG   PSALLIND+105                                                     
PSALDOL  DS    CL12          +105                                               
         ORG   PSALLIND+(L'P-(*-PSALLIND)) ERROR IF EXCEED L'PRINTLINE          
         EJECT                                                                  
**********************************************************************          
* OTHER INCLUDED DSECTS                                              *          
**********************************************************************          
         SPACE                                                                  
       ++INCLUDE TEKEY                                                          
       ++INCLUDE TEQELEM                                                        
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPPTBUF                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENADD                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPMEDBDESD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DEDBLOCK                                                       
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'095SPREPN502 05/20/15'                                      
         END                                                                    
