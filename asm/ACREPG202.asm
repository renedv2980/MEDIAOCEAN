*          DATA SET ACREPG202  AT LEVEL 062 AS OF 04/10/15                      
*PHASE ACG202A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE SORTER                                                                 
         TITLE 'GENERAL LEDGER PROFIT/LOSS'                                     
ACG202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**G202**,RR=R5                                                 
         L     RA,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RA                                                       
*                                                                               
         LA    RC,SPACEND                                                       
*                                                                               
         USING ACG202D,RC                                                       
*                                                                               
         LA    R9,2048(,RB)        R9 IS THE 2ND BASE REGISTER                  
         LA    R9,2048(,R9)                                                     
*                                                                               
         USING ACG202+4096,R9                                                   
*                                                                               
         ST    R5,RELO                                                          
         SPACE 1                                                                
CAT      EQU   X'01'                                                            
SCAT     EQU   X'02'                                                            
SSCAT    EQU   X'03'                                                            
OFFICE   EQU   X'04'                                                            
FILTER   EQU   X'05'                                                            
BLANK    EQU   X'40'                                                            
         EJECT ,                                                                
*              RUNFRST                                                          
         SPACE 2                                                                
PL01     CLI   MODE,RUNFRST                                                     
         BNE   PL02                                                             
         SPACE 1                                                                
         L     R1,=A(OFFTAB)                                                    
         A     R1,RELO                                                          
         ST    R1,AOFFTAB                                                       
         SPACE 1                                                                
         L     R1,=A(SORTAREA)                                                  
         A     R1,RELO                                                          
         ST    R1,ASORTAR                                                       
         B     PLXIT                                                            
         EJECT ,                                                                
*              REQFRST                                                          
         SPACE 2                                                                
PL02     CLI   MODE,REQFRST                                                     
         BNE   PL03                                                             
         SPACE 1                                                                
         MVC   PERIOD,SPACES                                                    
         MVC   QSTART+4(2),=C'01'                                               
         MVC   QEND+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(6,PERIOD)                                
         MVC   PERIOD+7(2),=C'TO'                                               
         GOTO1 (RF),(R1),(0,QEND),(6,PERIOD+10)                                 
         SPACE 1                                                                
         GOTO1 (RF),(R1),(0,QSTART),(1,PSTART)                                  
         GOTO1 (RF),(R1),(0,QEND),(1,PEND)                                      
         SPACE 1                                                                
         B     PLXIT                                                            
         EJECT ,                                                                
*              LEDGFRST                                                         
         SPACE 2                                                                
PL03     CLI   MODE,LEDGFRST                                                    
         BNE   PL04                                                             
         SPACE 1                                                                
         SPACE 1                                                                
         GOTO1 MYROLLER,DMCB,0,TOTTAB,4,7                                       
         BAS   RE,SETSORT          INITIALISE SORT                              
*                                                                               
         USING SORTRECD,R4                                                      
*                                                                               
         LA    R4,SORTREC1                                                      
         LA    R2,4                                                             
*                                                                               
PL03A    MVC   SORTREC,SPACES      CLEAR SORT RECORDS                           
         ZAP   SORTVAL,=P'0'                                                    
         LA    R4,L'SORTREC(,R4)                                                
         BCT   R2,PL03A                                                         
         SPACE 1                                                                
         MVI   SORTREC1+(SORTTYPE-SORTRECD),CAT                                 
         MVI   SORTREC2+(SORTTYPE-SORTRECD),SCAT                                
         MVI   SORTREC3+(SORTTYPE-SORTRECD),SSCAT                               
         MVI   SORTREC4+(SORTTYPE-SORTRECD),OFFICE                              
         SPACE 1                                                                
         MVI   CATOK,C'N'                                                       
         MVI   SCATOK,C'N'                                                      
         MVI   SSCATOK,C'N'                                                     
         MVI   OFFOK,C'N'                                                       
         SPACE 1                                                                
         BAS   RE,STRUCTUR         UPDATE LEVAGEN ETC.                          
*                                                                               
         USING LEVGEND,R4                                                       
*                                                                               
         MVI   BYTE,CAT            MUST HAVE BOTH CAT & OFFICE LEVELS           
         LA    R1,2                                                             
         SPACE 1                                                                
PL03B    LA    R4,LEVAGEN                                                       
         LA    R2,4                                                             
         SPACE 1                                                                
PL03C    CLC   LEVINT,BYTE                                                      
         BE    PL03D                                                            
         LA    R4,L'LEVAGEN(,R4)                                                
         BCT   R2,PL03C                                                         
         DC    H'0'                MISSING A VITAL LEVEL IN LEDGER              
         SPACE 1                                                                
PL03D    MVI   BYTE,OFFICE                                                      
         CLI   QOPT1,C' '          OFFICE NOT NEEDED FOR FILTER STYLE           
         BNE   PL03E               REQUEST.                                     
         BCT   R1,PL03B                                                         
         SPACE 1                                                                
PL03E    L     R4,AOFFTAB                                                       
*                                                                               
         USING OFFTAB,R4                                                        
*                                                                               
         XC    OFFP3,OFFP3         LOGICAL CLEAR FOR BINSRCH                    
         XC    HIBFDT,HIBFDT                                                    
         B     PLXIT                                                            
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
*              PROCACC                                                          
         SPACE 2                                                                
PL04     CLI   MODE,PROCACC                                                     
         BNE   PL05                                                             
         SPACE 1                                                                
         MVI   BYTE,CAT                                                         
         BAS   RE,BILDKEY                                                       
         CLC   SORTREC1+(CATKEY-SORTRECD)(L'CATKEY),MYKEY                       
         BNE   PL04A               NEW CATEGORY                                 
         SPACE 1                                                                
         MVI   BYTE,SCAT                                                        
         BAS   RE,BILDKEY                                                       
         CLC   SORTREC2+(SCATKEY-SORTRECD)(L'SCATKEY),MYKEY                     
         BNE   PL04B               NEW SUB CATEGORY                             
         SPACE 1                                                                
         MVI   BYTE,SSCAT                                                       
         BAS   RE,BILDKEY                                                       
         CLC   SORTREC3+(SSCATKEY-SORTRECD)(L'SSCATKEY),MYKEY                   
         BNE   PL04C                                                            
         SPACE 1                                                                
         MVI   BYTE,OFFICE                                                      
         CLI   QOPT1,C' '                                                       
         BE    *+8                                                              
         MVI   BYTE,FILTER                                                      
         BAS   RE,BILDKEY                                                       
         CLC   SORTREC4+(OFFKEY-SORTRECD)(L'OFFKEY),MYKEY                       
         BNE   PL04D                                                            
         B     PL04H               NOTHING HAS CHANGED                          
         SPACE 2                                                                
PL04A    BAS   RE,PUTOUT                                                        
         MVI   BYTE,SCAT                                                        
         SPACE 1                                                                
PL04B    BAS   RE,PUTOUT                                                        
         MVI   BYTE,SSCAT                                                       
         SPACE 1                                                                
PL04C    BAS   RE,PUTOUT                                                        
         MVI   BYTE,OFFICE                                                      
         CLI   QOPT1,C' '                                                       
         BE    *+8                                                              
         MVI   BYTE,FILTER                                                      
         SPACE 1                                                                
PL04D    BAS   RE,PUTOUT                                                        
         SPACE 1                                                                
         BAS   RE,BILDKEY                                                       
*                                                                               
         USING SORTRECD,R4                                                      
*                                                                               
         LA    R4,SORTREC4                                                      
         SPACE 1                                                                
         CLC   OFFKEY,MYKEY                                                     
         BE    PL04E                                                            
         SPACE 1                                                                
         MVC   BINREC,SPACES                                                    
         CLC   OFFKEY,SPACES       MUST HAVE A KEY IF WORTH WRITING             
         BE    PL04DA                                                           
         MVC   BINREC(L'OFFKEY),OFFKEY                                          
         MVC   BINREC+L'OFFKEY+1(L'SORTNAME),SORTNAME                           
         BAS   RE,PUTBIN                                                        
         SPACE 1                                                                
PL04DA   MVC   OFFKEY,MYKEY                                                     
         MVC   SORTNAME,KEYNAME                                                 
         SPACE 1                                                                
PL04E    MVI   BYTE,SSCAT                                                       
         BAS   RE,BILDKEY                                                       
         LA    R4,SORTREC3                                                      
         MVC   SORTNAME,KEYNAME                                                 
         CLC   SSCATKEY,MYKEY                                                   
         BE    PL04F                                                            
         MVC   SSCATKEY,MYKEY                                                   
         BAS   RE,REKEY                                                         
         SPACE 1                                                                
PL04F    MVI   BYTE,SCAT                                                        
         BAS   RE,BILDKEY                                                       
         LA    R4,SORTREC2                                                      
         MVC   SORTNAME,KEYNAME                                                 
         CLC   SCATKEY,MYKEY                                                    
         BE    PL04G                                                            
         MVC   SCATKEY,MYKEY                                                    
         BAS   RE,REKEY                                                         
         SPACE 1                                                                
PL04G    MVI   BYTE,CAT                                                         
         BAS   RE,BILDKEY                                                       
         LA    R4,SORTREC1                                                      
         MVC   SORTNAME,KEYNAME                                                 
         CLC   CATKEY,MYKEY                                                     
         BE    PL04H                                                            
         MVC   CATKEY,MYKEY                                                     
         BAS   RE,REKEY                                                         
         SPACE 1                                                                
PL04H    DS    0H                                                               
         L     R6,ADACCBAL                                                      
         LTR   R6,R6                                                            
         BZ    PL04I                                                            
*                                                                               
         USING ACBALD,R6                                                        
         USING SORTRECD,R4                                                      
*                                                                               
         LA    R4,SORTREC4                                                      
         CP    ACBLFRWD,=P'0'                                                   
         BE    PL04I                                                            
         AP    SORTVAL,ACBLFRWD                                                 
         MVI   CATOK,C'Y'                                                       
         MVI   SCATOK,C'Y'                                                      
         MVI   SSCATOK,C'Y'                                                     
         MVI   OFFOK,C'Y'                                                       
         SPACE 1                                                                
         L     R6,ADACCSTA                                                      
*                                                                               
         USING ACSTATD,R6                                                       
*                                                                               
         CLC   ACSTBFDT,HIBFDT                                                  
         BNH   *+10                                                             
         MVC   HIBFDT,ACSTBFDT                                                  
         SPACE 1                                                                
PL04I    B     PLXIT                                                            
*                                                                               
         DROP  R4,R6               KEEP IT CLEAN                                
         EJECT ,                                                                
*              PROCTRNS                                                         
         SPACE 2                                                                
PL05     CLI   MODE,PROCTRNS                                                    
         BNE   PL06                                                             
         SPACE 1                                                                
         LA    R4,SORTREC4                                                      
         L     R6,ADTRANS                                                       
*                                                                               
         USING SORTRECD,R4                                                      
         USING TRHISTD,R6                                                       
*                                                                               
         CLI   TRHSEL,X'45'                                                     
         BNE   PL05A                                                            
         B     PLXIT                                                            
         SPACE 1                                                                
* WE ARE NO LONGER USING BUCKETS TO PRODUCE THE FIGURES FOR THIS REPORT         
* BUT I HAVE LEFT THE CODE HERE JUST IN CASE                                    
         SPACE 1                                                                
         CLC   TRHSYEAR(2),PEND                                                 
         BH    PLXIT                                                            
         CLC   TRHSYEAR(2),PSTART                                               
         BL    PLXIT                                                            
         SPACE 1                                                                
         AP    SORTVAL,TRHSDR                                                   
         SP    SORTVAL,TRHSCR                                                   
         MVI   CATOK,C'Y'                                                       
         MVI   SCATOK,C'Y'                                                      
         MVI   SSCATOK,C'Y'                                                     
         MVI   OFFOK,C'Y'                                                       
         SPACE 1                                                                
         B     PLXIT                                                            
         SPACE 2                                                                
PL05A    L     R6,ADTRANS                                                       
*                                                                               
         USING TRANSD,R6                                                        
*                                                                               
         LR    RE,R6                                                            
         SH    RE,DATADISP                                                      
*                                                                               
         USING ACKEYD,RE                                                        
*                                                                               
         OC    ACDTPEEL,ACDTPEEL                                                
         BNZ   PLXIT                                                            
*                                                                               
         DROP  RE                                                               
*                                                                               
         CLI   TRNSEL,X'44'                                                     
         BNE   PLXIT                                                            
         SPACE 1                                                                
         MVC   WORK(2),TRNSBTCH                                                 
         TM    WORK+1,X'F0'        DEAL WITH MONTH FROM MOS                     
         BO    PL05AA                                                           
         ZIC   R1,WORK+1                                                        
         BCTR  R1,0                                                             
         STC   R1,WORK+1                                                        
         NI    WORK+1,X'0F'                                                     
         OI    WORK+1,X'10'                                                     
         B     PL05AB                                                           
         SPACE 1                                                                
PL05AA   NI    WORK+1,X'0F'                                                     
         SPACE 1                                                                
PL05AB   MVZ   WORK(1),PEND        NOW DEAL WITH YEAR AND DECADE                
         ZIC   R1,PEND                                                          
         ZIC   R2,WORK                                                          
         SR    R1,R2                                                            
         LPR   R1,R1                                                            
         CH    R1,=H'5'            IS THE ABSOLUTE DIFFERENCE OF THE            
*                                  YEARS LT 5,IF SO WE WERE RIGHT               
*                                  ABOUT THE DECADES.THINK ABOUT IT             
         BNH   PL05B                                                            
         SPACE 1                                                                
         ZIC   R1,PEND                                                          
         CR    R1,R2                                                            
         BL    PL05C               NO NEED TO CHECK THE DATES AS SUCH           
         B     PLXIT                                                            
         SPACE 1                                                                
PL05B    CLC   WORK(2),PEND                                                     
         BH    PLXIT               IGNORE TRANS WITH A HIGH DATE                
         SPACE 1                                                                
PL05C    ZAP   DUB,TRNSAMNT                                                     
         TM    TRNSSTAT,X'80'                                                   
         BO    *+10                                                             
         MP    DUB,=P'-1'                                                       
         AP    SORTVAL,DUB                                                      
         MVI   CATOK,C'Y'                                                       
         MVI   SCATOK,C'Y'                                                      
         MVI   SSCATOK,C'Y'                                                     
         MVI   OFFOK,C'Y'                                                       
         SPACE 1                                                                
         B     PLXIT                                                            
*                                                                               
         DROP  R4,R6               KEEP IT CLEAN                                
         EJECT ,                                                                
*              LEDGLAST                                                         
         SPACE 2                                                                
PL06     CLI   MODE,LEDGLAST                                                    
         BNE   PLXIT               NO POINT IN DOING REQUEST TOTALS.            
         SPACE 1                                                                
         MVI   BYTE,CAT            RELEASE ANY REMAINING RECORDS TO             
         BAS   RE,PUTOUT           SORTER                                       
         MVI   BYTE,SCAT                                                        
         BAS   RE,PUTOUT                                                        
         MVI   BYTE,SSCAT                                                       
         BAS   RE,PUTOUT                                                        
         MVI   BYTE,OFFICE                                                      
         BAS   RE,PUTOUT                                                        
         SPACE 1                                                                
         LA    R4,SORTREC4                                                      
*                                                                               
         USING SORTRECD,R4                                                      
*                                                                               
         MVC   BINREC,SPACES       WRITE OUT LAST OFFICE RECORD                 
         MVC   BINREC(L'OFFKEY),OFFKEY                                          
         MVC   BINREC+L'OFFKEY+1(L'SORTNAME),SORTNAME                           
         BAS   RE,PUTBIN                                                        
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         MVI   FIRSTPRT,C'Y'                                                    
         L     R4,ADLEDGER                                                      
         CLC   1(2,R4),=C'GP'                                                   
         BE    *+8                                                              
         MVI   RCSUBPRG,1                                                       
         SPACE 1                                                                
         MVC   MESSAGE,SPACES                                                   
         CLC   PEND(2),HIBFDT                                                   
         BNL   PL06A               OK EVERYTHING PEELED BEFORE REQUEST          
         MVC   MESSAGE(33),=C'WARNING - ACCOUNTS ON THIS LEDGER'                
         MVC   MESSAGE+34(09),=C'PEELED IN'                                     
         GOTO1 DATCON,DMCB,(1,HIBFDT),(6,MESSAGE+44)                            
*                                                                               
         USING OFFTAB,R4                                                        
*                                                                               
PL06A    L     R4,AOFFTAB                                                       
         MVI   OTHERSW,C'N'                                                     
         L     R2,OFFP3            NO OF RECORDS IN TABLE                       
         CH    R2,=H'6'                                                         
         BNH   *+8                                                              
         MVI   OTHERSW,C'Y'                                                     
         LTR   R2,R2                                                            
         BZ    PLXIT               NOTHING TO DO                                
         CH    R2,=H'1'                                                         
         BE    *+8                                                              
         LA    R2,1(,R2)           ADD ONE FOR A TOTAL COLUMN                   
         SPACE 1                                                                
*              NOW ASSIGN COLUMN NOS TO OFFICES & BUILD MIDLINES ETC.           
         SPACE 1                                                                
         MVC   MYMID1,SPACES                                                    
         MVC   MYMID2,SPACES                                                    
         MVC   MYFUT,SPACES                                                     
         MVI   ELCODE,7                                                         
         CLI   OTHERSW,C'Y'                                                     
         BE    *+8                                                              
         STC   R2,ELCODE                                                        
         LA    R6,PRINTAB                                                       
*                                                                               
         USING PRINTABD,R6                                                      
*                                                                               
PL06B    CLC   PRCOLS,ELCODE                                                    
         BNE   PL06BA                                                           
         CLI   PROPT,C'Z'          OPTION=Z IS THE DEFAULT SPEC                 
         BE    PL06C                                                            
         SPACE 1                                                                
PL06BA   LA    R6,L'PRINTAB(,R6)                                                
         B     PL06B                                                            
         SPACE 1                                                                
PL06C    DS    0H                  R6 NOW POINTING TO A PRINT SPEC RECD         
         L     R2,OFFP3                                                         
         LA    R3,OFFICES                                                       
*                                                                               
         USING OFFICED,R3                                                       
*                                                                               
         LA    R5,1                                                             
         SPACE 1                                                                
PL06D    STC   R5,OFFICOL                                                       
         CH    R5,=H'6'                                                         
         BE    *+8                                                              
         LA    R5,1(,R5)                                                        
         LA    R3,L'OFFICES(,R3)                                                
         BCT   R2,PL06D                                                         
         SPACE 1                                                                
         CLI   OTHERSW,C'Y'                                                     
         BNE   PL06I                                                            
         LA    R3,OFFICES          BUILD FUTLINE EXPLAINING OTHERS COL          
         L     R2,OFFP3                                                         
         MVI   MYWORK,C' '                                                      
         MVC   MYWORK+1(L'MYWORK-1),MYWORK                                      
         MVC   MYWORK(7),=C'OTHERS='                                            
         LA    R5,MYWORK+7                                                      
         LA    R1,5                MAY 4 EXPLANATIONS                           
         SPACE 1                                                                
PL06E    CLI   OFFICOL,6                                                        
         BNE   PL06F                                                            
         BCT   R1,*+8                                                           
         B     PL06G               4 EXPLANATIONS                               
         MVC   1(L'OFFICNAM,R5),OFFICNAM                                        
         LA    R5,L'OFFICNAM(,R5)                                               
         MVI   0(R5),C','                                                       
         SPACE 1                                                                
PL06F    LA    R3,L'OFFICES(,R3)                                                
         BCT   R2,PL06E                                                         
         B     PL06H                                                            
         SPACE 1                                                                
PL06G    MVC   1(3,R5),=C'ETC'                                                  
         LA    R5,4(,R5)                                                        
         SPACE 1                                                                
PL06H    MVI   0(R5),C'.'                                                       
         SPACE 1                                                                
         LA    R3,MYWORK                                                        
         SR    R5,R3                                                            
         LA    R5,1(,R5)                                                        
         GOTO1 =V(SQUASHER),DMCB,MYWORK,(C' ',(R5)),RR=RB                       
         MVC   MYFUT,MYWORK                                                     
         SPACE 1                                                                
PL06I    LA    R3,OFFICES          NOW BUILD HEADLINES                          
         ZIC   R2,PRCOLS                                                        
         ZIC   R5,PRFIGSRT                                                      
         LA    R5,MYMID1(R5)                                                    
         LA    R7,L'OFFICNAM                                                    
         MVI   MYWORK,C' '                                                      
         MVC   MYWORK+1(L'MYWORK-1),MYWORK                                      
         LA    R8,MYWORK                                                        
         CH    R2,=H'1'                                                         
         BE    PL06L               SPECIAL CASE,ONLY 1 COLUMN                   
         SPACE 1                                                                
PL06J    CH    R2,=H'2'                                                         
         BNE   PL06K                                                            
         CLI   OTHERSW,C'Y'                                                     
         BNE   PL06K                                                            
         MVC   5(6,R5),=C'OTHERS'                                               
         B     PL06M                                                            
         SPACE 1                                                                
PL06K    CH    R2,=H'1'                                                         
         BNE   PL06L                                                            
         MVC   6(5,R5),=C'TOTAL'                                                
         B     PL06M                                                            
         SPACE 1                                                                
PL06L    GOTO1 CHOPPER,DMCB,((R7),OFFICNAM),(11,(R8)),2                         
         ST    R5,DUB                                                           
         ST    R8,DUB+4                                                         
         XC    BYTE,BYTE                                                        
         SPACE 1                                                                
LOOP     LA    R1,11                                                            
         LA    R8,10(,R8)                                                       
         LA    R5,10(,R5)                                                       
         SPACE 1                                                                
LOOP5    TM    BYTE,X'80'                                                       
         BO    LOOP10                                                           
         CLI   0(R8),C' '                                                       
         BE    LOOP20                                                           
         SPACE 1                                                                
LOOP10   MVC   0(1,R5),0(R8)                                                    
         OI    BYTE,X'80'                                                       
         BCTR  R5,0                                                             
         SPACE 1                                                                
LOOP20   BCTR  R8,0                                                             
         BCT   R1,LOOP5                                                         
         TM    BYTE,X'01'                                                       
         BO    LOOPX                                                            
         MVI   BYTE,1                                                           
         L     R5,DUB                                                           
         L     R8,DUB+4                                                         
         LA    R5,L'MYMID1(,R5)                                                 
         LA    R8,11(,R8)                                                       
         B     LOOP                                                             
         SPACE 1                                                                
LOOPX    L     R5,DUB                                                           
         L     R8,DUB+4                                                         
         SPACE 1                                                                
PL06M    ST    R5,DUB                                                           
         CLC   L'MYMID1(11,R5),SPACES                                           
         BNE   PL06MC                                                           
         SPACE 1                                                                
         LA    R1,10                                                            
         SPACE 1                                                                
PL06MA   CLI   0(R5),C' '                                                       
         BNE   PL06MB                                                           
         LA    R5,1(,R5)                                                        
         BCT   R1,PL06MA                                                        
         SPACE 1                                                                
         PRINT GEN                                                              
PL06MB   EXMVC R1,L'MYMID1(R5),=12C'-'                                          
         PRINT NOGEN                                                            
         L     R5,DUB                                                           
         SPACE 1                                                                
PL06MC   AH    R5,PRFIGSPC                                                      
         LA    R3,L'OFFICES(,R3)                                                
         BCT   R2,PL06J                                                         
*                                                                               
         DROP  R3                  KEEP IT CLEAN                                
*                                                                               
         SPACE 1                                                                
*              WE SHOULD NOW BE READY TO PRINT THE REPORT                       
*              R2 PTS TO SORTREC1                                               
*              R4 PTS TO OFFTAB                                                 
*              R6 PTS TO PRINTAB ENTRY                                          
         SPACE 1                                                                
         LA    R2,SORTREC1                                                      
*                                                                               
         USING SORTRECD,R2                                                      
*                                                                               
PL06N    BAS   RE,GETSORT                                                       
         CLC   SORTREC1,SPACES                                                  
         BE    PL06X               E O F                                        
         SPACE 1                                                                
         CLI   SORTTYPE,OFFICE                                                  
         BE    PL06P                                                            
         CLI   SORTTYPE,FILTER                                                  
         BE    PL06P                                                            
         B     PL06Q                                                            
         SPACE 1                                                                
PL06P    BAS   RE,FINDCOL          SET P5 AND ADD IN TO  APROPRIATE COL         
         GOTO1 MYROLLER,DMCB,3,TOTTAB,SORTVAL,1                                 
         B     PL06N                                                            
         SPACE 1                                                                
PL06Q    CLC   SORTREC1,SORTREC2                                                
         BE    PL06N               DUPLICATE RECORD,QUITE POSSIBLE              
         SPACE 1                                                                
         MVC   SORTREC2,SORTREC1                                                
         CLC   OFFKEY,=12X'FF'                                                  
         BE    PL06S               END-OF-CAT TYPE RECORD                       
         SPACE 1                                                                
         BAS   RE,FINDLINE         FIRST FOR CAT,SCAT,ETC.                      
         SPACE 1                                                                
PL06QA   GOTO1 MYROLLER,DMCB,2,TOTTAB                                           
         CLC   DMCB+8(4),=F'1'                                                  
         BE    PL06QB                                                           
         L     R1,DMCB+8                                                        
         BCTR  R1,0                                                             
         ST    R1,DMCB+8                                                        
         B     PL06QA                                                           
         SPACE 1                                                                
PL06QB   BAS   RE,COUNTLEV                                                      
         CLC   SORTTYPE,LOWLEV                                                  
         BE    PL06N                                                            
         SPACE 1                                                                
         CLI   SORTTYPE,CAT        CLEAR IRRELEVANT BITS OF KEY                 
         BE    *+10                                                             
         MVC   CATKEY,SPACES                                                    
         CLI   SORTTYPE,SCAT                                                    
         BE    *+10                                                             
         MVC   SCATKEY,SPACES                                                   
         CLI   SORTTYPE,SSCAT                                                   
         BE    *+10                                                             
         MVC   SSCATKEY,SPACES                                                  
         MVC   OFFKEY,SPACES                                                    
         BAS   R7,PLREPORT                                                      
         SPACE 1                                                                
         LA    R3,4*L'CATKEY+L'SORTNAME                                         
         GOTO1 =V(SQUASHER),DMCB,SORTREC1,(R3),RR=RB                            
         SPACE 1                                                                
         ZIC   R3,DMCB+7           NEW LENGTH OF SQUASHED DATA                  
         PRINT GEN                                                              
         EXMVC R3,P+1,SORTREC1,1                                                
         CLI   SORTTYPE,CAT                                                     
         BNE   PL06R                                                            
         MVI   PSECOND+1,C'*'                                                   
         EXMVC R3,PSECOND+2,PSECOND+1,1                                         
         PRINT NOGEN                                                            
         SPACE 1                                                                
PL06R    MVI   SPACING,2                                                        
         BAS   R7,PLREPORT                                                      
         B     PL06N                                                            
         SPACE 1                                                                
PL06S    CLC   SORTTYPE,LOWLEV                                                  
         BE    PL06SA                                                           
         MVI   SPACING,1                                                        
         BAS   R7,PLREPORT                                                      
         B     PL06T                                                            
*                                  CROSS CAST & ADD DOWN                        
PL06SA   GOTO1 MYROLLER,DMCB,6,TOTTAB,1                                         
         SPACE 1                                                                
PL06T    BAS   RE,FINDLINE                                                      
         GOTO1 MYROLLER,DMCB,1,TOTTAB                                           
         L     R3,DMCB             A(LINE TO BE EDITED)                         
         BAS   RE,EDITOR                                                        
         SPACE 1                                                                
PL06U    BAS   RE,SUMLEV                                                        
         BE    PL06V                                                            
         CLC   SORTTYPE,LOWLEV                                                  
         BE    PL06V                                                            
         SPACE 1                                                                
         MVC   P,SPACES             GET RID OF EDITED FIGS & GO BACK            
         B     PL06N                                                            
         SPACE 1                                                                
PL06V    MVC   SORTVAL,SPACES                                                   
         CLC   SORTTYPE,LOWLEV                                                  
         BE    *+10                                                             
         MVC   SORTVAL,=C' TOTAL'                                               
         CLI   SORTTYPE,CAT                                                     
         BE    *+10                                                             
         MVC   CATKEY,SPACES                                                    
         CLI   SORTTYPE,SCAT                                                    
         BE    *+10                                                             
         MVC   SCATKEY,SPACES                                                   
         CLI   SORTTYPE,SSCAT                                                   
         BE    *+10                                                             
         MVC   SSCATKEY,SPACES                                                  
         MVC   OFFKEY,SPACES                                                    
         LA    R3,4*L'OFFKEY+L'SORTNAME+L'SORTVAL                               
         GOTO1 =V(SQUASHER),DMCB,SORTREC1,(R3),RR=RB                            
         SPACE 1                                                                
         ZIC   R3,PRBLURB                                                       
         MVC   BYTE,DMCB+7         NEW LENGTH OF SQUASHED DATA                  
         GOTO1 CHOPPER,DMCB,(BYTE,(R2)),((R3),P+1),(C'P',2)                     
         MVI   SPACING,1                                                        
         BAS   R7,PLREPORT                                                      
         B     PL06N                                                            
         SPACE 1                                                                
PL06X    BAS   R7,PLREPORT                                                      
         MVC   P+1(13),=C'LEDGER TOTALS'                                        
         SPACE 1                                                                
         GOTO1 MYROLLER,DMCB,1,TOTTAB,4                                         
         SPACE 1                                                                
         L     R3,DMCB                                                          
         BAS   RE,EDITOR                                                        
         MVI   SPACING,2                                                        
         MVI   FORCEFUT,C'Y'                                                    
         BAS   R7,PLREPORT                                                      
         B     PLXIT               FINISHED,AT LAST,PHEW.                       
         SPACE 1                                                                
PLXIT    XMOD1 1                                                                
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL R4,0,ELCODE                                                      
         SPACE 1                                                                
         DROP  R2,R4,R6            KEEO IT CLEAN                                
         EJECT ,                                                                
*              SUBROUTINES                                                      
         SPACE 2                                                                
*              STRUCTUR: BUILDS LEVAGEN ETC. HANDLES THE SKIPPING OF            
*                        UNWANTED LEVELS AND SO ON.                             
STRUCTUR NTR1                                                                   
         LA    R5,LEVINTAB                                                      
*                                                                               
ST10     CLC   PROGPROF(1),0(R5)                                                
         BE    ST20                                                             
         CLI   0(R5),C'Z'                                                       
         BE    ST20                                                             
         LA    R5,L'LEVINTAB(,R5)                                               
         B     ST10                                                             
*                                                                               
ST20     MVC   BYTE,1(R5)          KEEP NO OF CAT-TYPE LEVELS                   
         LA    R5,2(,R5)                                                        
         LA    R6,LEVAGEN                                                       
*                                                                               
         USING LEVGEND,R6                                                       
*                                                                               
         LA    R2,4                                                             
         MVC   LEVGEN,SPACES                                                    
         LA    R6,L'LEVGEN(,R6)                                                 
         BCT   R2,*-10                                                          
         LA    R6,LEVAGEN                                                       
         SPACE 1                                                                
         L     R4,ADLDGHIR                                                      
         LA    R4,ACHRLEVA-ACHEIRD(,R4)                                         
         LA    R3,4                                                             
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         SPACE 1                                                                
ST30     STC   R1,LEVKDISP                                                      
         IC    R2,0(,R4)                                                        
         LTR   R2,R2                                                            
         BZ    ST40                                                             
         MVC   LEVINT,0(R5)                                                     
         SR    R2,R1                                                            
         STC   R2,LEVKLEN                                                       
         AR    R1,R2                                                            
         SPACE 1                                                                
         LA    R6,L'LEVGEN(,R6)                                                 
         LA    R4,ACHRLEVB-ACHRLEVA(,R4)                                        
         LA    R5,1(,R5)                                                        
         BCT   R3,ST30                                                          
         SPACE 1                                                                
ST40     ZIC   R2,BYTE             BYTE=NO OF CAT-TYPE LEVELS                   
         TM    QOPT2,X'F0'         IS QOPT2 A NO.                               
         BNO   ST50                NO,DEFAULT                                   
         OI    BYTE,X'F0'          C.V.D.                                       
         CLC   BYTE,QOPT2                                                       
         BNH   *+10                                                             
         MVC   BYTE,QOPT2                                                       
         NI    BYTE,X'0F'          C.V.B.                                       
         SPACE 1                                                                
ST50     ZIC   R3,BYTE                                                          
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         LR    R3,R2               IF ZERO,DEFAULT                              
         SLL   R3,28                                                            
         SLDL  R2,4                                                             
         STC   R2,ELCODE                                                        
         LA    R4,LEVUPTAB                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                UP THE SWANNEE,NO PADDLE                     
*                                                                               
         USING LEVUPD,R4                                                        
*                                                                               
         CLI   LEVUPOPT,C'Z'                                                    
         BE    ST60                                                             
         SPACE 1                                                                
ST60     MVC   LOWLEV,LEVUPLOW                                                  
         LA    R5,LEVUPELS                                                      
         ZIC   R2,ELCODE                                                        
         SRL   R2,4                                                             
         SPACE 1                                                                
ST70     LA    R6,LEVAGEN                                                       
*                                                                               
         USING LEVGEND,R6                                                       
*                                                                               
         CLI   1(R5),C'N'                                                       
         BNE   ST100                                                            
         LA    R3,4                                                             
         SPACE 1                                                                
ST80     CLC   LEVINT,0(R5)                                                     
         BNE   ST90                                                             
         MVI   LEVINT,BLANK                                                     
         B     ST100                                                            
ST90     LA    R6,L'LEVAGEN(,R6)                                                
         BCT   R3,ST80                                                          
         SPACE 1                                                                
ST100    LA    R5,L'LEVUPELS(,R5)                                               
         BCT   R2,ST70                                                          
         B     XIT                                                              
*                                                                               
         DROP  R4,R6               KEEP IT CLEAN                                
         EJECT ,                                                                
         SPACE 2                                                                
LEVINTAB DS    0CL6                                                             
         DC    C'1',X'02',AL1(OFFICE),AL1(CAT),AL1(SCAT),AL1(BLANK)             
         DC    C'2',X'03',AL1(CAT),AL1(SCAT),AL1(SSCAT),AL1(OFFICE)             
         DC    C'Z',X'02',AL1(CAT),AL1(SCAT),AL1(OFFICE),AL1(BLANK)             
         SPACE 2                                                                
LEVUPTAB DS    0H                                                               
LEVU1    DC    X'11',AL1(LEVU2-LEVU1),C'Z',AL1(CAT),AL1(CAT),C'Y'               
LEVU2    DC    X'21',AL1(LEVU3-LEVU2),C'Z',AL1(CAT),AL1(CAT),C'Y'               
         DC    AL1(SCAT),C'N'                                                   
LEVU3    DC    X'22',AL1(LEVU4-LEVU3),C'Z',AL1(SCAT),AL1(CAT),C'Y'              
         DC    AL1(SCAT),C'Y'                                                   
LEVU4    DC    X'31',AL1(LEVU5-LEVU4),C'Z',AL1(CAT),AL1(CAT),C'Y'               
         DC    AL1(SCAT),C'N',AL1(SSCAT),C'N'                                   
LEVU5    DC    X'32',AL1(LEVU6-LEVU5),C'Z',AL1(SCAT),AL1(CAT),C'Y'              
         DC    AL1(SCAT),C'Y',AL1(SSCAT),C'N'                                   
LEVU6    DC    X'33',AL1(LEVU7-LEVU6),C'Z',AL1(SSCAT),AL1(CAT),C'Y'             
         DC    AL1(SCAT),C'Y',AL1(SSCAT),C'Y'                                   
LEVU7    DC    X'0000'                                                          
         EJECT ,                                                                
*              BILDKEY: WILL BUILD A KEY AND FIND ITS CORRESPONDING             
*              NAME FOR ANY OF CAT-FILTER.                                      
         SPACE 2                                                                
BILDKEY  NTR1                                                                   
         MVC   MYKEY,SPACES                                                     
         MVC   KEYNAME,SPACES                                                   
         CLI   BYTE,FILTER                                                      
         BE    BKFILT                                                           
         LA    R6,LEVAGEN                                                       
*                                                                               
         USING LEVGEND,R6                                                       
*                                                                               
         LA    R2,4                FOUR POSSIBLE LEVELS                         
         SPACE 1                                                                
BK10     CLC   LEVINT,BYTE                                                      
         BE    BK20                                                             
         LA    R6,L'LEVAGEN(,R6)                                                
         BCT   R2,BK10                                                          
         B     XIT                 NO CORRESPONDING LEVEL                       
         SPACE 1                                                                
BK20     BCTR  R2,0                                                             
         SLL   R2,2                                                             
         EX    0,LANAME(R2)                                                     
         B     BK30                                                             
         SPACE 1                                                                
LANAME   L     R5,ADLVDNAM         POINTS R5 TO THE ADDRESS OF A LEVEL          
         L     R5,ADLVCNAM         NAME                                         
         L     R5,ADLVBNAM                                                      
         L     R5,ADLVANAM                                                      
         SPACE 1                                                                
BK30     ZIC   R1,LEVKDISP                                                      
         ZIC   R2,LEVKLEN                                                       
*                                                                               
         DROP  R6                  KEEP IT CLEAN                                
*                                                                               
         L     R6,ADACC                                                         
         LA    R6,3(R1,R6)                                                      
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   MYKEY(0),0(R6)                                                   
         SPACE 1                                                                
         LR    R6,R5                                                            
*                                                                               
         USING ACNAMED,R6                                                       
*                                                                               
         ZIC   R2,ACNMLEN                                                       
         SH    R2,=H'3'                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   KEYNAME(0),ACNMNAME                                              
         B     XIT                                                              
*                                                                               
         USING ACSTATD,R6                                                       
*                                                                               
BKFILT   L     R6,ADACCSTA                                                      
         LA    R5,ACSTFILT                                                      
         SR    R2,R2                                                            
         CLI   QOPT1,C'1'                                                       
         BE    BKF10                                                            
         LA    R5,1(,R5)            FILTER 2                                    
         LA    R2,60(,R2)                                                       
         CLI   QOPT1,C'2'                                                       
         BE    BKF10                                                            
         LA    R5,ACSTANAL                                                      
         LA    R2,60(,R2)                                                       
         CLI   QOPT1,C'3'                                                       
         BE    BKF10                                                            
         LA    R5,ACSTSUB           DEFAULT TO SUBCOMP FILTER                   
         LA    R2,60(,R2)                                                       
*                                                                               
BKF10    MVC   MYKEY(1),0(R5)                                                   
         MVC   KEYNAME(7),=C'NO NAME'                                           
         CLI   MYKEY,C' '          HAS FILTER BEEN LEFT OFF ACCOUNT             
         BNE   *+12                                                             
         MVI   MYKEY,X'FF'         GIVE IT A DUMMY KEY                          
         B     XIT                                                              
         L     R6,FILTBLOC                                                      
         LA    R6,0(R2,R6)                                                      
         OC    0(15,R6),0(R6)                                                   
         BZ    *+10                                                             
         MVC   KEYNAME,16(R6)                                                   
         B     XIT                                                              
*                                                                               
         DROP  R6                  KEEP IT CLEAN                                
         EJECT ,                                                                
*              REKEY: PROPAGATES A NEW KEY THROUGH APPROPRIATE RECORDS          
         SPACE 2                                                                
REKEY    NTR1                                                                   
         LA    R5,SORTREC1+(CATKEY-SORTRECD)                                    
         CLI   BYTE,CAT                                                         
         BE    REK10                                                            
         LA    R5,SORTREC2+(SCATKEY-SORTRECD)                                   
         CLI   BYTE,SCAT                                                        
         BE    REK10                                                            
         LA    R5,SORTREC3+(SSCATKEY-SORTRECD)                                  
         CLI   BYTE,SSCAT                                                       
         BE    REK10                                                            
         B     XIT                                                              
         SPACE 1                                                                
REK10    LA    R6,L'SORTREC1                                                    
         LA    R7,SORTREC3+(SSCATKEY-SORTRECD)                                  
         SPACE 1                                                                
REK20    MVC   L'SORTREC1(L'CATKEY,R5),0(R5)                                    
         BXLE  R5,R6,REK20                                                      
         B     XIT                                                              
         EJECT ,                                                                
*              PUTOUT: IF O.K. WILL RELEASE 1ST & LAST RECORDS FOR              
*                      CAT-SSCAT. 1ST RECORD ONLY   FOR OFFICE (& FILT)         
*                      SETS CATOK-OFFOK TO 'N'                                  
         SPACE 2                                                                
         USING SORTRECD,R4                                                      
         SPACE 1                                                                
PUTOUT   NTR1                                                                   
         LA    R5,CATOK                                                         
         LA    R4,SORTREC1                                                      
         LA    R3,CATKEY                                                        
         CLI   BYTE,CAT                                                         
         BE    PUT10                                                            
         LA    R5,SCATOK                                                        
         LA    R4,SORTREC2                                                      
         LA    R3,SCATKEY                                                       
         CLI   BYTE,SCAT                                                        
         BE    PUT10                                                            
         LA    R5,SSCATOK                                                       
         LA    R4,SORTREC3                                                      
         LA    R3,SSCATKEY                                                      
         CLI   BYTE,SSCAT                                                       
         BE    PUT10                                                            
         LA    R5,OFFOK                                                         
         LA    R4,SORTREC4                                                      
         LA    R3,OFFKEY                                                        
         SPACE 1                                                                
PUT10    CLI   0(R5),C'Y'                                                       
         BNE   PUTXIT                                                           
         CLC   0(L'CATKEY,R3),SPACES                                            
         BE    PUTXIT              NO KEY FOR THIS LEVEL                        
         BAS   RE,PUTSORT                                                       
         SPACE 1                                                                
PUT20    CLI   BYTE,OFFICE                                                      
         BE    PUTXIT                                                           
         CLI   BYTE,FILTER                                                      
         BE    PUTXIT                                                           
         SPACE 1                                                                
         MVC   MYWORK(L'SORTREC1),0(R4)      SAVE                               
         MVC   OFFKEY,=12X'FF'                                                  
         CLI   BYTE,SSCAT                                                       
         BE    PUT30                                                            
         MVC   SSCATKEY,=12X'FF'             X'FF' FOR 'LAST' RECORD            
         CLI   BYTE,SCAT                                                        
         BE    PUT30                                                            
         MVC   SCATKEY,=12X'FF'                                                 
         SPACE 1                                                                
PUT30    BAS   RE,PUTSORT                    PUT OUT                            
         MVC   0(L'SORTREC1,R4),MYWORK       RESTORE                            
         SPACE 1                                                                
PUTXIT   MVI   0(R5),C'N'          SET CATOK ETC. TO 'N'                        
         ZAP   SORTVAL,=P'0'                                                    
         B     XIT                                                              
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
*              BINSRCH INTERFACE                                                
         SPACE 2                                                                
         USING OFFTAB,R2                                                        
         SPACE 1                                                                
PUTBIN   NTR1                                                                   
         L     R2,AOFFTAB                                                       
         MVC   DMCB+8(16),OFFP3                                                 
         GOTO1 BINSRCH,DMCB,(1,BINREC),OFFICES                                  
         OC    DMCB(4),DMCB                                                     
         BNZ   PB10                                                             
         DC    H'0',C'TABLE FULL'                                               
         SPACE 1                                                                
PB10     MVC   OFFP3,DMCB+8                                                     
         B     XIT                                                              
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
         EJECT ,                                                                
*              SORTER INTERFACE                                                 
         SPACE 2                                                                
SETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECDCARD,(40,ASORTAR),RR=RB             
         B     XIT                                                              
         SPACE 2                                                                
PUTSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4),RR=RB                               
         B     XIT                                                              
         SPACE 2                                                                
GETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'GET',0,RR=RB                                  
         MVC   SORTREC1,SPACES                                                  
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         MVC   SORTREC1,0(R1)                                                   
         B     XIT                                                              
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(01,44,A),FORMAT=BI,WORK=1'                     
*                                                                               
RECDCARD DC    CL80'RECORD TYPE=F,LENGTH=89'                                    
         EJECT ,                                                                
*              EDIT ROUTINE                                                     
         SPACE 2                                                                
         USING PRINTABD,R6                                                      
         SPACE 1                                                                
EDITOR   NTR1                                                                   
         ZIC   R2,PRCOLS                                                        
         ZIC   R4,PRFIGSRT                                                      
         LA    R4,P(R4)                                                         
         ST    R3,FULL             R3 IS SET TO POINT AT NOS                    
*                                                                               
ED10     CH    R2,=H'1'                                                         
         BNE   ED20                                                             
         L     R3,FULL             MUST BE TOTAL                                
         L     R5,TOTTAB+4         NO OF COLS IN TOTTAB                         
         BCTR  R5,0                                                             
         MH    R5,=H'8'            ALL NOS PL8                                  
         LA    R3,0(R5,R3)                                                      
         CP    0(8,R3),=P'0'                                                    
         BNE   ED20                                                             
         MVI   9(R4),C'0'          SHOW ZERO TOTALS                             
         B     XIT                                                              
         SPACE 1                                                                
ED20     ZAP   DUB,0(8,R3)                                                      
         AP    DUB,=P'50'                                                       
         CP    DUB,=P'0'                                                        
         BH    *+10                                                             
         SP    DUB,=P'100'                                                      
         DP    DUB,=P'100'                                                      
         MVC   MYWORK(6),DUB                                                    
         EDIT  (P6,MYWORK),(11,(R4)),0,MINUS=YES                                
         LA    R3,8(,R3)                                                        
         AH    R4,PRFIGSPC                                                      
         BCT   R2,ED10                                                          
         SPACE 1                                                                
         B     XIT                                                              
         SPACE 1                                                                
         DROP  R6                  KEEP IT CLEAN                                
         EJECT ,                                                                
*              FINDLINE & FINDCOL:SET PROLLER PARAMETERS ACCORDING TO           
*                                 THE CAT,SCAT ETC.                             
         SPACE 2                                                                
         USING SORTRECD,R2                                                      
         SPACE 1                                                                
FINDLINE DS    0H                                                               
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+11,1                                                        
         CLI   SORTTYPE,SSCAT                                                   
         BER   RE                                                               
         MVI   DMCB+11,2                                                        
         CLI   SORTTYPE,SCAT                                                    
         BER   RE                                                               
         MVI   DMCB+11,3                                                        
         CLI   SORTTYPE,CAT                                                     
         BER   RE                                                               
         DC    H'0'                DUD RECORD FROM SORTER                       
         SPACE 2                                                                
         USING OFFTAB,R4                                                        
         USING OFFICED,R3                                                       
         USING SORTRECD,R2                                                      
         SPACE 1                                                                
FINDCOL  NTR1                                                                   
         LA    R3,OFFICES                                                       
         L     R1,OFFP3                                                         
         SPACE 1                                                                
FIND10   CLC   OFFKEY,OFFICKEY                                                  
         BE    FIND20                                                           
         LA    R3,L'OFFICES(,R3)                                                
         BCT   R1,FIND10                                                        
         DC    H'0'                DUD RECORD FROM SORTER                       
         SPACE 1                                                                
FIND20   XC    DMCB+16(4),DMCB+16                                               
         MVC   DMCB+19(1),OFFICOL                                               
         B     XIT                                                              
*                                                                               
         DROP  R2,R3,R4            KEEP IT CLEAN                                
         EJECT ,                                                                
*              SUMLEV & COUNTLEV                                                
         SPACE 2                                                                
         USING SORTRECD,R2                                                      
         SPACE 1                                                                
COUNTLEV NTR1                                                                   
         LA    R1,CATOK                                                         
         CLI   SORTTYPE,CAT                                                     
         BE    COUNT10                                                          
         LA    R1,SCATOK                                                        
         CLI   SORTTYPE,SCAT                                                    
         BE    COUNT10                                                          
         LA    R1,SSCATOK                                                       
         CLI   SORTTYPE,SSCAT                                                   
         BE    COUNT10                                                          
         B     XIT                                                              
         SPACE 1                                                                
COUNT10  CLI   0(R1),C'N'                                                       
         BNE   COUNT20                                                          
         MVI   0(R1),C'A'                                                       
         B     XIT                                                              
COUNT20  CLI   0(R1),C'A'                                                       
         BNE   XIT                                                              
         MVI   0(R1),C'B'                                                       
         B     XIT                                                              
         SPACE 2                                                                
         USING SORTRECD,R2                                                      
         SPACE 1                                                                
SUMLEV   NTR1                                                                   
         LA    R1,SCATOK                                                        
         CLI   SORTTYPE,CAT                                                     
         BE    SUM10                                                            
         LA    R1,SSCATOK                                                       
         CLI   SORTTYPE,SCAT                                                    
         BE    SUM10                                                            
         B     XIT                                                              
         SPACE 1                                                                
SUM10    CLI   0(R1),C'B'                                                       
         MVI   0(R1),C'N'                                                       
         B     XIT                 CONDITION CODE SET                           
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
         EJECT ,                                                                
*              PLREPORT & PRINTAB                                               
         SPACE 2                                                                
PLREPORT MVC   HEAD4+90(L'PERIOD),PERIOD                                        
         MVC   HEAD6+56(L'MESSAGE),MESSAGE                                      
         MVC   MID1,MYMID1                                                      
         MVC   MID2,MYMID2                                                      
         CLI   FIRSTPRT,C'Y'                                                    
         MVI   FIRSTPRT,C'N'                                                    
         BE    PLREP10                                                          
         CLI   OTHERSW,C'Y'                                                     
         BNE   PLREP10                                                          
         MVC   FOOT1,MYFUT                                                      
         SPACE 1                                                                
PLREP10  GOTO1 ACREPORT                                                         
         BR    R7                                                               
         SPACE 2                                                                
*              TABLE FOR PRINT HANDLING                                         
         SPACE 1                                                                
PRINTAB  DS    0CL6                                                             
         DS    0H                                                               
         DC    X'01',C'Z',AL1(50),AL1(55),H'01'                                 
         DC    X'03',C'Z',AL1(40),AL1(45),H'17'                                 
         DC    X'04',C'Z',AL1(30),AL1(33),H'16'                                 
         DC    X'05',C'Z',AL1(25),AL1(27),H'15'                                 
         DC    X'06',C'Z',AL1(20),AL1(22),H'15'                                 
         DC    X'07',C'Z',AL1(16),AL1(18),H'13'                                 
         DS    0H                                                               
         EJECT ,                                                                
*              MYROLLER BECAUSE PROLLER CAN'T COPE WITH PL8                     
*              THEY LOOK EXACTLY THE SAME                                       
         SPACE 2                                                                
MYROLLER NTR1                                                                   
         L     R1,DMCB+4           PT TO TABLE ASSUME DMCB USED                 
         CLC   DMCB(4),=F'0'                                                    
         BE    MYR1                                                             
         CLC   DMCB(4),=F'1'                                                    
         BE    MYR2                                                             
         CLC   DMCB(4),=F'3'                                                    
         BE    MYR3                                                             
         CLC   DMCB(4),=F'6'                                                    
         BE    MYR4                                                             
         CLC   DMCB(4),=F'2'                                                    
         BE    MYR5                                                             
         B     XIT                                                              
         SPACE 2                                                                
MYR1     DS    0H                  SET TABLE                                    
         MVC   0(8,R1),DMCB+8      SET ROWS AND COLUMNS                         
         LA    R2,8(,R1)                                                        
         L     R3,DMCB+8                                                        
         MH    R3,DMCB+14          HALF WORD LIMIT ON COLUMNS                   
         SPACE 1                                                                
MYR1A    ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(,R2)                                                        
         BCT   R3,MYR1A                                                         
         B     XIT                                                              
         SPACE 2                                                                
MYR2     DS    0H                  FIND A ROW                                   
         L     R3,DMCB+8                                                        
         BAS   RE,MYRA                                                          
         ST    R2,DMCB                                                          
         B     XIT                                                              
         SPACE 2                                                                
MYR3     DS    0H                  ADD A NO INTO A TABLE ENTRY                  
         L     R3,DMCB+12                                                       
         BAS   RE,MYRA                                                          
         L     R3,DMCB+16                                                       
         C     R3,4(,R1)                                                        
         BNH   MYR3A                                                            
         DC    H'0'                NOT POSSIBLE FAULTY CALL                     
         SPACE 1                                                                
MYR3A    BCT   R3,*+8                                                           
         B     MYR3B                                                            
         LA    R2,8(,R2)                                                        
         B     MYR3A                                                            
         SPACE 1                                                                
MYR3B    L     R4,DMCB+8                                                        
         AP    0(8,R2),0(8,R4)                                                  
         B     XIT                                                              
         SPACE 2                                                                
MYR4     DS    0H                  CROSS CAST AND ADD DOWN                      
         L     R3,DMCB+8                                                        
         BAS   RE,MYRA                                                          
         L     R4,4(,R1)           NO OF COLUMNS                                
         ZAP   DUB,=P'0'                                                        
         LR    R5,R2               SAVE R2                                      
         SPACE 1                                                                
MYR4A    CH    R4,=H'1'                                                         
         BNE   MYR4B                                                            
         AP    0(8,R2),DUB                                                      
         B     MYR4C                                                            
         SPACE 1                                                                
MYR4B    AP    DUB,0(8,R2)                                                      
         LA    R2,8(,R2)                                                        
         BCT   R4,MYR4A                                                         
         SPACE 1                                                                
MYR4C    L     R3,0(,R1)           FIND LAST LINE                               
         BAS   RE,MYRA                                                          
         LR    R7,R2                                                            
         L     R6,4(,R1)                                                        
         MH    R6,=H'8'            GIVES LENGTH OF ONE LINE                     
         LA    R6,0(R6,R5)         R5 POINTS TO START LINE                      
         SPACE 1                                                                
MYR4D    CR    R7,R6               R7 POINTS TO END LINE                        
         BL    XIT                                                              
         LR    R3,R5                                                            
         L     R8,4(,R1)                                                        
MYR4E    AP    0(8,R6),0(8,R3)                                                  
         LA    R3,8(,R3)                                                        
         LA    R6,8(,R6)                                                        
         BCT   R8,MYR4E                                                         
         B     MYR4D                                                            
         SPACE 2                                                                
MYR5     DS    0H                  CLEAR A LINE                                 
         L     R3,DMCB+8                                                        
         BAS   RE,MYRA                                                          
         L     R3,4(,R1)                                                        
*                                                                               
MYR5A    ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(,R2)                                                        
         BCT   R3,MYR5A                                                         
         B     XIT                                                              
         SPACE 2                                                                
MYRA     DS    0H                  SET R2 TO POINT TO R3TH LINE                 
         LA    R2,8(,R1)           POINT TO START OF NOS                        
         C     R3,0(,R1)                                                        
         BNH   *+6                                                              
         DC    H'0'                CAN'T BE DONE                                
         L     R4,4(,R1)                                                        
         MH    R4,=H'8'            GIVES LENGTH OF ONE WHOLE LINE               
*                                                                               
MYRAA    BCT   R3,*+6                                                           
         BR    RE                                                               
         LA    R2,0(R4,R2)                                                      
         B     MYRAA                                                            
         EJECT ,                                                                
*              LITERALS                                                         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT ,                                                                
*              MAIN STORAGE & DSECTS                                            
         SPACE 2                                                                
ACG202D  DSECT                                                                  
         SPACE 1                                                                
RELO     DS    F                                                                
         SPACE 1                                                                
SORTREC1 DS    CL89                COVERED BY SORTRECD                          
SORTREC2 DS    CL89                                                             
SORTREC3 DS    CL89                                                             
SORTREC4 DS    CL89                                                             
BINREC   DS    CL47                                                             
         SPACE 1                                                                
MYMID1   DS    CL132                                                            
MYMID2   DS    CL132                                                            
MYFUT    DS    CL132                                                            
         SPACE 1                                                                
PSTART   DS    PL3                                                              
PEND     DS    PL3                                                              
HIBFDT   DS    PL3                                                              
         SPACE 1                                                                
LEVAGEN  DS    CL3                 1ST BYTE: MEANING OF THIS LEVEL              
LEVBGEN  DS    CL3                 2ND BYTE: DISPLACEMENT OF LEVELS KEY         
LEVCGEN  DS    CL3                 3RD BYTE: LENGTH OF THIS LEVELS KEY          
LEVDGEN  DS    CL3                                                              
         SPACE 1                                                                
AOFFTAB  DS    A                   A(OFFTAB)                                    
ASORTAR  DS    A                                                                
TOTTAB   DS    CL8,(4*7)PL8                                                     
         SPACE 1                                                                
CATOK    DS    CL1                                                              
SCATOK   DS    CL1                                                              
SSCATOK  DS    CL1                                                              
OFFOK    DS    CL1                                                              
         SPACE 1                                                                
OTHERSW  DS    CL1                                                              
ELCODE   DS    CL1                                                              
LOWLEV   DS    CL1                                                              
FIRSTPRT DS    CL1                                                              
PERIOD   DS    CL16                                                             
MESSAGE  DS    CL55                                                             
MYKEY    DS    CL11                                                             
KEYNAME  DS    CL36                                                             
MYWORK   DS    CL200                                                            
         EJECT ,                                                                
*              DSECT FOR SORTRECS                                               
SORTRECD DSECT                     COVERS SORTREC1 ETC                          
SORTREC  DS    0CL89                                                            
CATKEY   DS    CL11                CATEGORY KEY                                 
SCATKEY  DS    CL11                SUB CATEGORY KEY                             
SSCATKEY DS    CL11                SUB SUB CATEFORY KEY                         
OFFKEY   DS    CL11                OFFICE OR FILTER KEY                         
SORTNAME DS    CL36                NAME FOR HIGH LEVEL RECORDS                  
SORTVAL  DS    PL8                 MONEY FOR LOW LEVEL RECORDS                  
SORTTYPE DS    CL1                                                              
         SPACE 2                                                                
*              DSECT FOR LEVAGEN ETC.                                           
LEVGEND  DSECT                                                                  
LEVGEN   DS    0CL3                                                             
LEVINT   DS    CL1                 CAT,SCAT,OFFICE,OR BLANK                     
LEVKDISP DS    CL1                 DISPLACEMENT OF KEY                          
LEVKLEN  DS    CL1                 LENGTH OF KEY                                
         SPACE 2                                                                
*              DSECT FOR PRINTAB ENTRIES                                        
PRINTABD DSECT                                                                  
PRCOLS   DS    CL1                 HEX NO OF COLS                               
PROPT    DS    CL1                 OPTION LETTER                                
PRBLURB  DS    CL1                 LENGTH OF LHS NARRATIVE                      
PRFIGSRT DS    CL1                 DISPLACEMANT OF 1ST COL                      
PRFIGSPC DS    H                   DIFFERENCE 1 COL TO THE NEXT                 
         SPACE 2                                                                
OFFTAB   CSECT                                                                  
OFFP3    DC    F'0'                NO OF RECORDS SO FAR                         
OFFP4    DC    F'48'               LENGTH OF RECORD                             
OFFP5    DC    F'11'               11 BYTE KEY,STARTING BYTE 0                  
OFFP6    DC    F'250'              MAX NO IN TABLE                              
OFFICES  DS    250CL48             11 BYTE KEY,1 BYTE COL CODE,36 BYTE          
*                                  NAME                                         
OFFICED  DSECT                                                                  
OFFICKEY DS    CL11                                                             
OFFICOL  DS    CL1                                                              
OFFICNAM DS    CL36                                                             
         SPACE 1                                                                
LEVUPD   DSECT                                                                  
LEVUPKEY DS    CL1                                                              
LEVUPLEN DS    AL1                                                              
LEVUPOPT DS    CL1                                                              
LEVUPLOW DS    CL1                                                              
LEVUPELS DS    0CL2                                                             
         EJECT ,                                                                
*              STANDARD DSECTS                                                  
         SPACE 1                                                                
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         SPACE 4                                                                
SORTAREA CSECT                                                                  
         DS    CL41120             40 K BYTES OF STORAGE FOR SORTER             
         SPACE 4                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062ACREPG202 04/10/15'                                      
         END                                                                    
