*          DATA SET NEWRI45    AT LEVEL 039 AS OF 08/23/02                      
*PHASE T32045A,*                                                                
*INCLUDE NETACC                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'T32045-GF TAPE'                                                 
T32045   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEGF**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)       RA=2ND BASE REGISTER                           
         LA    RA,1(RA)                                                         
         USING T32045,RB,RA                                                     
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4          R7-ANETWS4/WORKING STORAGE                   
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         L     R1,ANETWS1                                                       
         ST    R1,ACLISTSV         ANETWS1/CLISTSV                              
         LA    R1,HEADING          ANETWS2/ANETWS3=I/O AREAS                    
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         L     R1,BOOKVAL                                                       
         A     R1,RELO                                                          
         ST    R1,ANTWKTP                                                       
*                                                                               
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR                                                               
         CLI   MODE,RUNLAST                                                     
         BE    LASTRUN                                                          
EXIT     XIT1                                                                   
*                                                                               
LASTRUN  DS    0H                                                               
         L     R2,ANTWKTP                                                       
         CLC   =X'90EC',0(R2)                                                   
         BE    LASTRX                                                           
         CLOSE ((R2),)                                                          
LASTRX   B     EXIT                                                             
         EJECT                                                                  
*****************************************                                       
LR       DS    0H                                                               
*                               * SET UP BINSRCH PARAMETERS                     
         SR    R0,R0               A OF REC TO BE ADDED                         
         LA    R1,TOTALS           A OF BINTBL                                  
         SR    R2,R2               NUM OF REC IN TBL,UPDATED BY BINSRCH         
         LA    R3,TOTLENE          LENGTH OF REC                                
         LA    R4,2                DISP OF KEY INTO REC                         
         LA    R5,11               MAX RECS IN BINTBL                           
         STM   R0,R5,BINDMCB                                                    
*                                                                               
         BAS   RE,CLEARTOT                                                      
         XC    DMCB,DMCB                                                        
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD       INIT SORTER                   
         B     LR5                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,23,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=80'                                    
*                                                                               
DDNAME   DC    CL8'NTWKTP'                                                      
DSNAME   DC    CL20'NETTAPE.NE0GGAA1'                                           
DCBOPEN  DC    C'N'                                                             
*                                                                               
LR5      DS    0H                                                               
         CLI   TAPEOPT,C'Y'                                                     
         BNE   LR7                                                              
         GOTO1 DATCON,DMCB,(5,0),(X'20',TAPEDAT)                                
         L     RE,BOOKVAL                                                       
         CLC   =X'90EC',0(RE)                                                   
         BNE   LR7                                                              
         LA    RF,NTWKTP                                                        
         MVC   0(128,RE),0(RF)                                                  
         MVC   DSNAME+13(2),NBSELAGY                                            
         L     R4,ATWA                                                          
         USING T320FFD,R4                                                       
         L     RF,TWADCONS                                                      
         L     RF,TDYNALLO-TWADCOND(RF)                                         
         GOTO1 (RF),DMCB,DDNAME,DSNAME                                          
******   GOTO1 =V(DYNALLOC),DMCB,DDNAME,DSNAME                                  
         L     R2,ANTWKTP                                                       
         OPEN  ((R2),(OUTPUT))                                                  
         DROP  R4                                                               
*                                                                               
LR7      BAS   RE,GTMONLST               GET MONTHLIST                          
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'P'                 X'94' KEY/ESTIMATE HIGH               
         MVI   NBSELUOP,0                 ACTUAL SCHEDULE/ESTIM                 
         MVI   NBSPLOPT,X'C0'             DO SPLIT 30'S                         
*                                                                               
LR10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBVALCLI                                                  
         BNE   LR12                                                             
         L     RF,ACLISTSV                                                      
         L     RE,NBAIO                                                         
         USING CKEY,RE                                                          
         LA    RE,CLIST                                                         
         LA    R1,880                                                           
         MOVE  ((RF),(R1)),(RE)                                                 
         DROP  RE                                                               
LR12     CLI   NBMODE,NBPROCUN                                                  
         BE    LR20                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    LR28                                                             
         B     LR10                                                             
*                                                                               
*  UNITS LOGIC                                                                  
*                                                                               
LR20     CLI   AGYCODE,0           FIRST TIME                                   
         BNE   LR22                                                             
         MVI   AGYCODE,C'Z'        FOR SJR TEST                                 
         CLC   NBALPHA,=C'SJ'                                                   
         BE    LR32                                                             
*                                                                               
         MVI   AGYCODE,C'M'        SET AGENCY CODE                              
         CLC   NBALPHA,=C'OM'                                                   
         BE    LR32                                                             
                                                                                
         MVI   AGYCODE,C'H'        SET AGENCY CODE                              
         CLC   NBALPHA,=C'WW'                                                   
         BE    LR32                                                             
*                                                                               
         MVI   AGYCODE,C'A'                                                     
         CLC   NBALPHA,=C'YN'                                                   
         BNE   LR20D                                                            
         CLC   =C'KRB',NBSELCLI                                                 
         BE    LR20B                                                            
         CLC   =C'KRM',NBSELCLI                                                 
         BE    LR20B                                                            
         CLC   =C'KRX',NBSELCLI                                                 
         BNE   LR32                                                             
LR20B    MVI   AGYCODE,C'5'                                                     
         B     LR32                                                             
*                                                                               
LR20D    MVI   AGYCODE,C'W'                                                     
         CLC   NBALPHA,=C'JW'                                                   
         BE    LR32                                                             
*                                                                               
         MVI   AGYCODE,C'C'                                                     
         CLC   NBALPHA,=C'FC'                                                   
         BE    LR32                                                             
         DC    H'0'                                                             
*                                                                               
LR22     CLC   CURCLT,NBACTCLI          ..IF CLIENT HAS CHANGED                 
         BNE   LR28                                                             
         CLC   CUREST,NBACTEST          ..OR EST HAS CHANGED                    
         BNE   LR28                                                             
         CLC   CURPRD,NBSPLPRN          ... IF PRD HAS CHANGED                  
         BE    LR40                                                             
         BNE   LR33                                                             
*                                                                               
LR28     BAS   RE,DOGFEST               ..ADD GFEST DATA TO TABLE               
         BAS   RE,DOBILREC              ..ADD BILL RECS TO TABLE                
         BAS   RE,PUTSORT               ..PUT TABLE TO SORTER                   
         CLI   NBMODE,NBREQLST                                                  
         BE    LR50                                                             
*                                                                               
LR32     MVC   CURCLT,NBACTCLI                                                  
         MVC   CUREST,NBACTEST                                                  
         BAS   RE,CLRCOMTB         CLEAR COMMIS TABLE                           
*                                                                               
LR33     MVC   CURPRD,NBSPLPRN          ...  GET NEW PROD                       
         BAS   RE,GETPRD                                                        
         BAS   RE,COMMIRAT                   GET COMMISSION PCT                 
*                                                                               
LR40     BAS   RE,DOUNIT                   PROCESS UNIT REC                     
         B     LR10                        GET NEXT UNIT                        
         SPACE                                                                  
*                                                                               
LR50     LA    RE,TABLE                                                         
         LA    RF,ULENE                                                         
         XCEF                                                                   
LR52     GOTO1 SORTER,DMCB,=C'GET'       TAKE RECS FROM SORTER                  
         L     R3,4(R1)                                                         
         MVC   SORTREC,0(R3)                                                    
         LTR   R3,R3                                                            
         BZ    LR70                                                             
         BAS   RE,ADDTABL          ADD REC TO TABLE                             
         B     LR52                                                             
LR70     BAS   RE,PRTMAP           PRINTS TAPE MAP                              
         BAS   RE,PROCTBL                                                       
         BAS   RE,DOTOTS           TOTALS                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* ADD UNITS DATA TO TABLE                                                       
*                                                                               
DOUNIT   NTR1                                                                   
         LA    R2,WORK                                                          
         USING UNITSD,R2                                                        
         MVI   UXTRA,0                                                          
         MVC   UCLT,NBACTCLI                                                    
         MVC   UPRD,CURPROD        3 CHAR PROD                                  
         MVC   UEST,NBACTEST                                                    
         DROP  R2                                                               
         LA    R4,TABLE                                                         
         USING UNITSD,R4                                                        
DUN3     CLC   0(7,R4),WORK        IS REC ALREADY THERE                         
         BE    DUN5                                                             
         CLI   1(R4),0                                                          
         BE    DUN3B                                                            
         LA    R4,ULENE(R4)                                                     
         CLI   1(R4),0                                                          
         BNE   DUN3                                                             
DUN3B    MVC   0(7,R4),WORK        NO/ADD NEW REC                               
         LA    R2,UACCMON                                                       
         LA    R3,24                                                            
DUN4     ZAP   0(8,R2),=P'0'      PREPARE ITS DOLLAR FIELDS                     
         ZAP   8(8,R2),=P'0'                                                    
         ZAP   16(8,R2),=P'0'                                                   
         ZAP   24(8,R2),=P'0'                                                   
         LA    R2,UMONLENE(R2)                                                  
         BCT   R3,DUN4                                                          
DUN5     LA    R4,UACCMON                                                       
         LA    R2,MONLIST          GET PROPER MONTH SLOT                        
         L     R5,NUMPER                                                        
DUN6     CLC   NBACTDAT,2(R2)                                                   
         BNH   DUN10                                                            
         LA    R2,4(R2)                                                         
         LA    R4,UMONLENE(R4)                                                  
         BCT   R5,DUN6                                                          
*                                  IF NO MATCH,PUT IN LAST MONTH                
DUN10    XC    DMCB(12),DMCB                                                    
         GOTO1 =V(NETACC),DMCB,(14,NEACCWRK),NETBLOCK     NETACT+INT            
         AP    0(8,R4),PL8WRK                                                   
         NI    COMTYPE,X'01'       ..IS COMMISSION BASED ON NET                 
         CLI   COMTYPE,1                                                        
         BE    DUN12                                   ..YES                    
***      GOTO1 =V(NETACC),DMCB,(9,NEACCWRK),NETBLOCK   ..NO   ACT+INT           
         L     RE,NBACTUAL                                                      
         L     RF,NBINTEG                                                       
         AR    RE,RF                                                            
         CVD   RE,DUB                                                           
         MVC   PL8WRK,DUB                                                       
DUN12    ZAP   PL16WRK,PL8WRK                      COMMISSION                   
         MP    PL16WRK,COMPCT                                                   
         AP    PL16WRK,=P'500000'                                               
         DP    PL16WRK,=P'1000000'                                              
         AP    8(8,R4),PL16WRK(12)                                              
*                                                                               
         GOTO1 =V(NETACC),DMCB,(152,NEACCWRK),NETBLOCK     NETASS+INTG          
         AP    16(8,R4),PL8WRK                                                  
         NI    COMTYPE,X'01'     ..IS COMMISSION BASED ON NET                   
         CLI   COMTYPE,1                                                        
         BE    DUN14             ..YES                                          
****     GOTO1 =V(NETACC),DMCB,(2,NEACCWRK),NETBLOCK  ..NO  ASS+ INTG           
         L     RE,NBASSIGN                                                      
         L     RF,NBINTEG                                                       
         AR    RE,RF                                                            
         CVD   RE,DUB                                                           
         MVC   PL8WRK,DUB                                                       
DUN14    ZAP   PL16WRK,PL8WRK                      COMMISSION                   
         MP    PL16WRK,COMPCT                                                   
         AP    PL16WRK,=P'500000'                                               
         DP    PL16WRK,=P'1000000'                                              
         AP    24(8,R4),PL16WRK(12)                                             
*                                                                               
DOUX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* READ GFEST RECORDS/ADD TO UNITS DATA                                          
*                                                                               
DOGFEST  NTR1                                                                   
         LA    R6,KEY              SET UP TO GET GFEST RECORDS                  
         USING PGKEY,R6                                                         
         XC    KEY,KEY                                                          
         MVC   PGKTYP(2),=X'0D5D'                                               
         MVC   PGKAM,NBACTAM                                                    
         MVC   PGKCLT,CURCLT                                                    
         MVC   PGKNETE,CUREST                                                   
         MVC   PGKNETP,=C'POL'        *** READ POL GFEST REC ***                
         NETGO NVSETSPT,DMCB                                                    
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(9),KEY       ID/AM/CLT/EST/PRD                           
         BNE   GF30                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         MVC   AIO,ANETWS2                                                      
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         LA    R4,TABLE                                                         
         USING UNITSD,R4                                                        
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         BNE   GF30                                                             
         USING GFNETELM,R6                                                      
GF25     MVC   UNAT,GFNETNAT                                                    
         MVC   USUB,GFNETSUB                                                    
         MVC   UASS,GFNETASS                                                    
         LA    R4,ULENE(R4)       BUMP TABLE                                    
         CLI   1(R4),0                                                          
         BNE   GF25                                                             
*                                                                               
GF30     DS    0H              *** READ ZERO EST/SPECIFIC PRD REC ***           
         LA    R4,TABLE                                                         
GF31     LA    R6,KEY                                                           
         USING PGKEY,R6                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D5D'                                                  
         MVC   PGKAM,NBACTAM                                                    
         MVC   PGKCLT,CURCLT                                                    
         MVI   PGKNETE,0                                                        
         MVC   PGKNETP,UPRD                 PROD FROM REC IN TABLE              
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BNE   GF40                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         MVC   AIO,ANETWS2                                                      
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING GFNETELM,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   GF40                                                             
         MVC   UDIV,GFNETDIV                                                    
         MVC   UGFPRD,GFNETPRD                                                  
         CLI   UASS,C'Y'                                                        
         BE    GF40                                                             
         MVC   UASS,GFNETASS                                                    
GF40     LA    R4,ULENE(R4)        BUMP TABLE                                   
         CLI   1(R4),0                                                          
         BNE   GF31                                                             
GFX      NETGO NVSETUNT,DMCB       RESET TO UNIT FILE                           
         XC    FILENAME,FILENAME   CLEAR FILENAME                               
         MVI   NBFUNCT,NBFRDHI                                                  
         B     EXIT                                                             
         DROP  R6,R4                                                            
         EJECT                                                                  
*                                                                               
*  DO BILL RECORDS *                                                            
*  FILL IN INVOICE CODE AND DOLLAR BUCKETS                                      
*  IN CASE OF MORE THAN ONE INVOICE PER MONTH/YEAR                              
*  ADD DUPLICATE RECORD AND SORTER WILL PUT IT IN RIGHT SLOT                    
DOBILREC NTR1                                                                   
         NETGO NVSETSPT,DMCB                                                    
         LA    R4,TABLE                                                         
         ST    R4,ATABLE                                                        
         USING UNITSD,R4                                                        
DB5      LA    R3,MONYEAR                                                       
         LA    R5,24               BCT FOR MAX MONTH BUCKETS                    
DB10     XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),UCLT                                                    
         MVC   KEY+4(3),UPRD                                                    
         MVC   KEY+7(1),UEST                                                    
         MVC   KEY+8(2),0(R3)      YM OF SERVICE FROM TABLE                     
* - CONVERT REQUESTED YYMM TO 1 BYTE Y/M BILLING REC FORMAT                     
         MVC   KEY+10(1),BILLMON+1      SET X'0N' BILL MONTH                    
         PACK  WORK(2),BDAT(2)          SWITCHES LAST DIGIT OF YY               
         MVZ   KEY+10(1),WORK+1                                                 
         LA    R4,UACCMON                                                       
         DROP  R4                                                               
         B     DB14                                                             
*                                                                               
DB12     MVC   KEY,KEYSAVE                                                      
         MVC   KEY+8(2),0(R3)                                                   
*                                                                               
DB14     MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
****                                                                            
DB14B    B     DB14C               DEBUGGING PATCH                              
****                                                                            
DB14C    CLC   KEY(11),KEYSAVE    ID/AM/CLT/PRD/EST/YM SERVICE/YMBILL           
         BNE   DB50                                                             
         B     DB15                YES                                          
DB14D    MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                 NO                                           
         B     DB14B                                                            
*                                                                               
DB15     MVC   FILENAME,=C'SPTFIL  '                                            
         MVC   AIO,ANETWS2                                                      
         GOTO1 GETREC                                                           
         L     R6,AIO                  ADD INVOICE NO/AMT TO REC                
         USING BILLREC,R6                                                       
         TM    BILSTAT,X'20'       CHK TRUE AOR BILL                            
         BNO   DB17                NO                                           
         CLC   NBSELAGY,=C'YN'     YES/SKIP UNLESS YNR                          
         BNE   DB14D                                                            
         CLC   =C'KRM',NBSELCLI         AND CLIENT=KRM                          
         BE    DB16                                                             
         CLC   =C'KRX',NBSELCLI         OR  CLIENT=KRX                          
         BNE   DB14D                                                            
DB16     XC    BNET,BNET           THEN CLEAR NET AND PROCESS                   
DB17           EQU                 *                                            
                                                                                
*  Y2K CHANGE BDATE = INTERNAL DDS FORMAT  FAF0F0F1 - JAN/2000                  
*             BDAT  =                      F0F0F0F1 - JAN/2000                  
*  BDAT IS IN THAT FORMAT SINCE IT IS INTERESTED IN YEAR WITHIN DECADE          
*  IN ORDER TO CONSTRUCT BILLING KEY ABOVE                                      
         MVC   WORK(4),BDATE                                                    
         MVC   WORK+4(2),=X'F0F1'                                               
         GOTO1 DATCON,DMCB,WORK,(X'20',WORK+6)                                  
         CLC   WORK+6(4),BDAT                                                   
*                                                                               
*        CLC   BDATE(4),BDAT       CHK BILLING DATE                             
         BNE   DB14D                                                            
         LR    R2,R4                                                            
         USING UACCMON,R2                                                       
         MVC   UINV,BINVNO         INVOICE NUMBER                               
         ICM   RE,15,BNET          NET BILL AMOUNT                              
         CVD   RE,DUB                                                           
         ZAP   UACCMON,DUB                                                      
         ICM   RF,15,BACTUAL        COMMISSION                                  
         SR    RF,RE                                                            
         CVD   RF,DUB                                                           
         ZAP   UACCMON+8(8),DUB                                                 
DB20     MVC   FILENAME,=C'SPTDIR  '    ...CHECK MULTIPLE INV FOR YR/MO         
         GOTO1 SEQ                                                              
         CLC   KEY(11),KEYSAVE                                                  
         BNE   DB50                                                             
*                                                                               
*                             MULTIPLE INVOICES FOR SAME YR/MON                 
*                                                                               
         LA    RE,TABLE            ...YES/FIND END OF TABLE                     
DB25     CLI   1(RE),0                                                          
         BE    DB27                                                             
         LA    RE,ULENE(RE)                                                     
         B     DB25                                                             
DB27     LR    RF,RE               ... ADD REC TO END OF TABLE                  
         ST    RF,ADUPLIC                                                       
         L     RE,ATABLE                                                        
         LA    R1,ULENE                                                         
         MOVE  ((RF),(R1)),(RE)                                                 
         L     RE,ADUPLIC           ...  RESET RE TO TABLE ADDRESS              
         MVI   0(RE),C'Y'           ... SET DUPLICATE REC FLAG                  
         DROP  R2                                                               
         USING UNITSD,RE                                                        
         LA    RF,24                ...  CLEAR ITS MONTH BUCKETS                
         LA    R1,UACCMON                                                       
DB30     ZAP   0(8,R1),=P'0'                                                    
         ZAP   8(8,R1),=P'0'                                                    
         ZAP   16(8,R1),=P'0'                                                   
         ZAP   24(8,R1),=P'0'                                                   
         XC    32(6,R1),32(R1)                                                  
         LA    R1,UMONLENE(R1)                                                  
         BCT   RF,DB30                                                          
         MVC   FILENAME,=C'SPTFIL  '     ... GO ADD INVNO/AMT                   
         LR    R1,RE                                                            
         GOTO1 GETREC                                                           
*                                                                               
         LR    RE,R1                                                            
         LA    R1,MONYEAR          FIND PROPER MONTH BUCKET                     
         LA    RE,UACCMON                                                       
DB32     CLC   0(8,R1),0(R3)                                                    
         BE    DB35                                                             
         LA    R1,8(R1)                                                         
         LA    RE,UMONLENE(RE)                                                  
         B     DB32                                                             
DB35     MVC   32(6,RE),BINVNO                                                  
         TM    BILSTAT,X'20'       CHK TRUE AOR BILL                            
         BNO   DB37                                                             
         CLC   NBSELAGY,=C'YN'     YES/SKIP UNLESS YNR                          
         BNE   DB20                                                             
         CLC   =C'KRM',NBSELCLI         AND CLIENT=KRM                          
         BE    DB36                                                             
         CLC   =C'KRX',NBSELCLI         OR  CLIENT=KRX                          
         BNE   DB20                                                             
DB36     XC    BNET,BNET           THEN CLEAR NET AND PROCESS                   
DB37     ICM   R1,15,BNET                                                       
         CVD   R1,DUB                                                           
         ZAP   0(8,RE),DUB                                                      
         ICM   RF,15,BACTUAL        COMMISSION                                  
         SR    RF,R1                                                            
         CVD   RF,DUB                                                           
         ZAP   8(8,RE),DUB                                                      
         B     DB20                CHECK ANY MORE DUPLICATE YR/MON              
         DROP  RE                                                               
*                                                                               
*                                                                               
DB50     LA    R3,8(R3)                 BUMP MONTH TABLE                        
         CLI   0(R3),0                                                          
         BE    DB52                                                             
         LA    R4,UMONLENE(R4)          BUMP MONTH BUCKETS                      
         BCT   R5,DB12                                                          
*                                                                               
DB52     L     R4,ATABLE                                                        
         LA    R4,ULENE(R4)                                                     
         ST    R4,ATABLE                                                        
         CLI   0(R4),C'Y'          IS IT EXTRA INVOICE REC                      
         BE    DBX                                                              
         CLI   1(R4),0              IS IT END OF TABLE                          
         BNE   DB5                                                              
*                                                                               
DBX      NETGO NVSETUNT,DMCB                                                    
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI                                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************                         
*   PUTS RECORD TO SORTER                                                       
PUTSORT  NTR1                                                                   
         LA    R2,TABLE                                                         
         USING UNITSD,R2                                                        
         CLI   UCLT,0                                                           
         BE    PSX                                                              
PS5      LA    R3,RECWORK                                                       
         USING TLINED,R3                                                        
         MVI   0(R3),X'40'                                                      
         MVC   1(L'RECWORK-1,R3),0(R3)                                          
         GOTO1 =V(CLUNPK),DMCB,UCLT,TCPE             CLIENT                     
         MVC   TCPE+3(3),UPRD                       PRODUCT                     
         EDIT  (B1,UEST),(3,TCPE+6)                 ESTIMATE                    
         MVC   WORK(1),UEST                   ***    FUDGE SAVE                 
         MVC   TCOMP,UDIV                           COMPANY                     
         MVC   TDIV,UDIV+1                          DIVISION                    
         MVC   TGFPRD,UGFPRD                        GFPRODUCT                   
         MVC   TAGY,AGYCODE                         AGENCY                      
         MVC   TNAT,UNAT                            NATURAL                     
         MVC   TSUB,USUB                            SUBNATURAL                  
*                                                                               
         LA    R4,MONYEAR                                                       
         LA    R5,24               BCT MAX MONTHS                               
         LA    R6,UACCMON                                                       
         LR    RE,R6                                                            
PS15     MVI   TTYPE,C'X'                           TYPE                        
         ZAP   TNET,=P'0'                                                       
         ZAP   TCOM,=P'0'                                                       
         MVC   TINV,=10X'40'                                                    
         CLI   WORK,33          FUDGE EST ***                                   
         BE    PS17                                                             
         CLI   32(RE),X'40'        IS IT INVOICE                                
         BH    PS17                                                             
         CLI   UASS,C'Y'           IS IT ASSIGNED DOLLARS                       
         BE    PS20                                                             
         CLC   0(8,RE),=PL8'0'                                                  
         BNE   PS17                                                             
         CLC   8(8,RE),=PL8'0'                                                  
         BE    PS40                NO ACTUAL DOLLARS/GET NEXT MONTH             
*                                                                               
PS17     AP    TNET,0(8,RE)        ACTUAL OR INVOICE DOLLARS                    
         AP    TCOM,8(8,RE)                                                     
         MVC   TMONTH(2),0(R4)     SET 2BYTE Y/M FOR SORT                       
         CLI   32(RE),X'40'            IS IT INVOICE                            
***      BNH   PS30                                                             
         BH    PS18                    YES                                      
         CLC   =C'KRX',NBSELCLI    IF KRX?                                      
         BE    *+14                                                             
         CLC   =C'KRM',NBSELCLI    IF KRM?                                      
         BNE   PS30                                                             
         CLC   =C'YN',NBALPHA      AND YNR                                      
         BE    PS40                SKIP THIS REC                                
         B     PS30                ELSE NORMAL PROCESS                          
PS18     MVI   TINV,C'N'                                                        
         MVI   TINV+1,C'/'                                                      
         MVC   TINV+2(2),32(RE)         INV=N/NN/NNNN                           
         MVI   TINV+4,C'/'                                                      
         MVC   TINV+5(4),34(RE)                                                 
         MVI   TTYPE,C'P'          TYPE =P                                      
         B     PS32                                                             
PS20     LA    RE,16(RE)           BUMP TO ASSIGNED DOLLARS                     
         CLC   0(8,RE),=PL8'0'                                                  
         BNE   PS22                                                             
         CLC   8(8,RE),=PL8'0'                                                  
         BE    PS40                                                             
PS22     AP    TNET,0(8,RE)        ASSIGNED DOLLARS                             
         AP    TCOM,8(8,RE)                                                     
         MVI   TMARK,C'Y'          SET ASS DOLLARS                              
         MVC   TMONTH(2),0(R4)     SET 2BYTE Y/M FOR SORT                       
PS30     CLC   TMONTH(2),BILLMON   FOR X RECORDS MOS > BILLMON                  
         BNH   PS40                                                             
*                                                                               
PS32     GOTO1 SORTER,DMCB,=C'PUT',RECWORK                                      
*                                                                               
PS40     LA    R4,8(R4)            BUMP MONYEAR                                 
         LA    R6,UMONLENE(R6)     BUMP UNIT MONTH BUCKETS                      
         LR    RE,R6                                                            
         BCT   R5,PS15                                                          
         LA    R2,ULENE(R2)        BUMP TABLE                                   
         CLI   UCLT,0              ANY MORE RECORDS                             
         BNE   PS5                                                              
PSX      LA    R4,TABLE            CLEAR TABLE                                  
         LR    RE,R4                                                            
PSX1     LA    RF,ULENE                                                         
         XCEF                                                                   
         LA    R4,ULENE(R4)                                                     
         CLI   1(R4),0                                                          
         BE    PSXX                                                             
         LR    RE,R4                                                            
         B     PSX1                                                             
PSXX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* PROCESS TABLE/ WRITING REPORT / TAPE                                          
*        R3 POINTS TO REC FROM SORTER                                           
*                                                                               
PROCTBL  NTR1                                                                   
         LA    R3,TABLE                                                         
         USING TLINED,R3                                                        
         CLI   TAGY,0                                                           
         BE    TOTX                                                             
PT00     LA    R2,P                                                             
         USING PLINED,R2                                                        
         CLC   COMPREV,TCOMP                                                    
         BNE   PT1                                                              
         CLC   DIVPREV,TDIV                                                     
         BNE   PT2                                                              
         CLC   PRDPREV,TGFPRD                                                   
         BNE   PT3                                                              
         CLC   CPEPREV,TCPE                                                     
         BNE   PT3A                                                             
         B     PT3B                                                             
PT1      MVC   PCOMP(1),TCOMP                           COMPANY                 
         MVC   COMPREV,TCOMP                                                    
PT2      MVC   PDIVI(1),TDIV                           DIVISION                 
         MVC   DIVPREV,TDIV                                                     
PT3      MVC   PGFPRD(4),TGFPRD                        GFPRODUCT                
         MVC   PRDPREV,TGFPRD                                                   
PT3A     MVC   PCPE(3),TCPE                         CLIENT                      
         MVC   PCPE+4(3),TCPE+3                     PRODUCT                     
         MVC   PCPE+8(3),TCPE+6                     ESTIMATE                    
         MVC   CPEPREV,TCPE                                                     
PT3B     MVC   PTYPE(1),TTYPE                          TYPE                     
         MVC   PNAT(3),TNAT                            NATURAL                  
         MVC   PSUB(3),TSUB                            SUB                      
         MVC   PINV,TINV                                                        
         CLI   TMARK,C'Y'           IS IT ASSIGNED DOLLARS                      
         BNE   PT4                                                              
         MVI   TMARK,0                                                          
         MVI   PASS+2,C'Y'                                                      
PT4      PACK  TNETSV,TNET                                                      
         EDIT  (P10,TNETSV),(13,PNET),2,MINUS=YES                               
         PACK  TCOMSV,TCOM                                                      
         EDIT  (P10,TCOMSV),(13,PCOM),2,MINUS=YES                               
         ZAP   MYPAK,TNETSV                                                     
         AP    MYPAK,TCOMSV                                                     
         EDIT  (P10,MYPAK),(13,PTOT),2,MINUS=YES                                
         MVC   WORK+50(4),TMONTH                      YYMM                      
         MVC   WORK+54(2),=C'01'                                                
         GOTO1 DATCON,DMCB,(0,WORK+50),(6,PMONTH)   PRINT NEEDS MMM/YY          
         BAS   RE,SPOOLIT                                                       
         L     R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         ST    R1,COUNTER                                                       
         CLI   TAPEOPT,C'Y'                                                     
         BNE   TOT0                                                             
         MVC   TDATE,TAPEDAT                                                    
         BAS   RE,WRITAPE                                                       
*                                                                               
*                                                                               
TOT0     AP    REPTOT(8),TNETSV            ADD TO ALL REPORT TOTS               
         AP    REPTOT+8(8),TCOMSV                                               
*                                                                               
TOT1     DS    0H                          ADD TO COMP TOTS                     
         XC    WORK(2),WORK                                                     
         LA    R4,WORK+2                                                        
         LA    R5,6                                                             
TOT2     ZAP   0(8,R4),=P'0'       PREPARE PACKED FIELDS                        
         LA    R4,8(R4)                                                         
         BCT   R5,TOT2                                                          
         MVC   WORK(1),TCOMP                                                    
         MVC   WORK+1(1),TDIV                                                   
TOT4     GOTO1 =V(BINSRCH),BINDMCB,(1,WORK),RR=RELO                             
         L     R4,0(R1)                                                         
         USING TOTALS,R4                                                        
         LTR   R1,R1               IS TABLE FULL                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AP    2(8,R4),TNETSV    ADD TO REPORT TOTS                             
         AP    10(8,R4),TCOMSV                                                  
         CLI   TTYPE,C'X'                                                       
         BNE   TOT6                                                             
         AP    TOLNETX,TNETSV                                                   
         AP    TOLCOMX,TCOMSV                                                   
         B     TOTX                                                             
TOT6     AP    TOLNETP,TNETSV                                                   
         AP    TOLCOMP,TCOMSV                                                   
*                                                                               
*                                                                               
TOTX     DS    0H                                                               
         LA    R3,TLENE(R3)                                                     
         CLI   TAGY,0                                                           
         BNE   PT00                                                             
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
WRITAPE  NTR1                                                                   
         L     R1,ANTWKTP                                                       
         MVC   RECWORK,SPACES                                                   
         MVC   RECWORK(TLENE),0(R3)                                             
         PUT   (R1),RECWORK                   WRITE TAPE                        
*****    GOTO1 HEXOUT,DMCB,RECWORK,P,67                                         
******   GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
***************************************                                         
* ROUTINE CLEARS AND ZAPS TOTAL FIELDS                                          
*                                                                               
CLEARTOT NTR1                                                                   
         LA    R3,REPTOT                                                        
         LA    R2,6                                                             
CLR3     ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R2,CLR3                                                          
         LA    R2,10                                                            
         LA    R3,TOTALS                                                        
CLR5     ZAP   2(8,R3),=P'0'                                                    
         ZAP   10(8,R3),=P'0'                                                   
         LA    R3,TOTLENE(R3)                                                   
         BCT   R2,CLR5                                                          
         XC    COUNTER,COUNTER                                                  
         B     EXIT                                                             
*                                                                               
         SPACE                                                                  
*******************************                                                 
*                                                                               
SPOOLIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
         SPACE                                                                  
**********************************                                              
*  TO GET PRD CODE FROM C LIST                                                  
*                                                                               
GETPRD   NTR1                                                                   
         L     R2,ACLISTSV                                                      
GP10     CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GP12                                                             
         MVC   CURPROD,=C'***'    SET TO UNDEFINED                              
         B     GPX                                                              
GP12     CLC   NBSPLPRN,3(R2)                                                   
         BE    GP14                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GP10                RETURN TO LOOP                               
GP14     MVC   CURPROD,0(R2)      SET 3 CHAR PRINTABLE PRD CODE                 
*                                                                               
GPX      B     EXIT                                                             
         EJECT                                                                  
****************************************                                        
* GET WEEKLIST INTO PERLIST                                                     
*                                                                               
GTMONLST NTR1                                                                   
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'                                                       
         MVI   NBRESUME,NBPROCPK                                                
GTWK5    NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBVALDAT                                                  
         BNE   GTWK5                                                            
         MVI   PERTYPE,C'M'        MONTHS ONLY                                  
         LA    R3,24               FOR 24 MONTHS MAX                            
         ST    R3,NUMPER                                                        
         LA    R3,MONLIST                                                       
         ST    R3,AMONLIST                                                      
         NETGO NVWKLST,DMCB,NUMPER,(R3),PERTYPE                                 
*                                                                               
         L     R3,AMONLIST         CREAT MONTHYR LIST                           
         LA    R4,MONYEAR          2 BYTE Y/M + 6 BYTE MMM/YY                   
         L     R5,NUMPER                                                        
GTWK20   GOTO1 DATCON,DMCB,(2,0(R3)),(0,RECWORK)                                
         DS    0H                                                               
         GOTO1 ADDAY,DMCB,RECWORK,RECWORK+10,F'+10'                             
         DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,RECWORK+10),(3,RECWORK)                           
         MVC   0(2,R4),RECWORK                                                  
         GOTO1 DATCON,DMCB,(0,RECWORK+10),(6,RECWORK)                           
         MVC   2(6,R4),RECWORK                                                  
         LA    R3,4(R3)                                                         
         CLI   0(R3),0                                                          
         BE    GTWKX                                                            
         LA    R4,8(R4)                                                         
         BCT   R5,GTWK20                                                        
GTWKX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
DOTOTS   NTR1                                                                   
         CLI   TAPEOPT,C'Y'        IF WRITING TAPE                              
         BNE   DOT1                                                             
*                                                                               
         LA    R3,P                PUT TOTAL LINE TO TAPE                       
         USING TLINED,R3                                                        
         MVI   P,C'9'                                                           
         MVC   P+1(TLENE-1),P                                                   
         MVI   TTYPE,C'T'                                                       
         MVC   TAGY,AGYCODE                                                     
         MVC   TDATE,TAPEDAT                                                    
         MVC   BYTE,REPTOT+7                                                    
         OI    BYTE,X'F0'                                                       
         CLI   BYTE,X'FC'                                                       
         BNE   DOTB                                                             
         OI    REPTOT+7,X'0F'                                                   
DOTB     UNPK  TNET(10),REPTOT(8)                                               
         MVC   BYTE,REPTOT+15                                                   
         OI    BYTE,X'F0'                                                       
         CLI   BYTE,X'FC'                                                       
         BNE   DOTD                                                             
         OI    REPTOT+15,X'0F'                                                  
DOTD     UNPK  TCOM(10),REPTOT+8(8)                                             
         BAS   RE,WRITAPE                                                       
         MVC   P,SPACES                                                         
         DROP  R3                                                               
*                                                                               
DOT1     BAS   RE,SPOOLIT                                                       
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         LA    R3,TOTALS                                                        
         USING TOTALS,R3                                                        
DOT2     CLC   0(2,R3),=2X'00'                                                  
         BE    DOT14                                                            
         CLC   PREVCOMP,0(R3)      IF SAME COMP                                 
         BE    DOT2D               SUPRESS PRINTING OFCOMP CODE                 
         MVI   PCOMP,C'-'          ELSE PRINT DOTTED LINE FOR BREAK             
         MVC   PCOMP+1(PLENE-3),PCOMP                                           
         BAS   RE,SPOOLIT                                                       
         MVC   PREVCOMP,0(R3)                                                   
         MVC   PCOMP(1),0(R3)                                                   
DOT2D    MVC   PDIVI(1),1(R3)                                                   
         ZAP   MYPAK,2(8,R3)                                                    
         AP    MYPAK,10(8,R3)                                                   
         EDIT  (P8,2(R3)),(13,PNET),2,MINUS=YES                                 
         EDIT  (P8,10(R3)),(13,PCOM),2,MINUS=YES                                
         EDIT  (P10,MYPAK),(13,PTOT),2,MINUS=YES                                
         BAS   RE,SPOOLIT                                                       
         ZAP   MYPAK,TOLNETX                 P/X TOTALS                         
         AP    MYPAK,TOLCOMX                                                    
         CLC   MYPAK,=PL10'0'                                                   
         BE    DOT12                                                            
DOT10    DS    0H                                                               
         MVI   PNET,C'X'                                                        
         EDIT  (P8,TOLNETX),(12,PNET+1),2,MINUS=YES                             
         MVI   PCOM,C'X'                                                        
         EDIT  (P8,TOLCOMX),(12,PCOM+1),2,MINUS=YES                             
         MVI   PTOT,C'X'                                                        
         EDIT  (P10,MYPAK),(12,PTOT+1),2,MINUS=YES                              
         BAS   RE,SPOOLIT                                                       
DOT12    ZAP   MYPAK,TOLNETP                                                    
         AP    MYPAK,TOLCOMP                                                    
         CLC   MYPAK,=PL10'0'                                                   
         BE    DOT13                                                            
         MVI   PNET,C'P'                                                        
         EDIT  (P8,TOLNETP),(12,PNET+1),2,MINUS=YES                             
         MVI   PCOM,C'P'                                                        
         EDIT  (P8,TOLCOMP),(12,PCOM+1),2,MINUS=YES                             
         MVI   PTOT,C'P'                                                        
         EDIT  (P10,MYPAK),(12,PTOT+1),2,MINUS=YES                              
         BAS   RE,SPOOLIT                                                       
DOT13    LA    R3,TOTLENE(R3)                                                   
         B     DOT2                                                             
         DROP  R3                                                               
*                                                                               
DOT14    MVI   FORCEHED,C'Y'                 RECAP TOTALS                       
         MVI   HEADRTN,3                                                        
         USING TOTLD,R2                                                         
         LA    R3,TOTALS                                                        
DOT15    CLC   0(2,R3),=2X'00'                                                  
         BE    DOT20                                                            
         MVC   TOTCOMP(1),0(R3)                                                 
***      MVC   TOTDIV(1),1(R3)   ** SUPPRESS DIV PRINTING FOR NOW               
DOT16    LR    R4,R3                                                            
         LA    R4,TOTLENE(R4)      SET R4 TO NEXT TOTLINE                       
         CLC   0(1,R4),0(R3)       IS NEXT TOTLINE SAME COMPANY                 
         BNE   DOT17                                                            
         AP    2(8,R4),2(8,R3)     YES/ROLL OVER TOTALS                         
         AP    10(8,R4),10(8,R3)                                                
         LA    R3,TOTLENE(R3)                                                   
         B     DOT16                                                            
DOT17    EDIT  (P8,2(R3)),(13,TOTNET),2,MINUS=YES                               
         EDIT  (P8,10(R3)),(13,TOTCOM),2,MINUS=YES                              
         BAS   RE,SPOOLIT                                                       
DOT18    LA    R3,TOTLENE(R3)                                                   
         B     DOT15                                                            
DOT20    BAS   RE,SPOOLIT                                                       
         MVC   TOTCOMP(13),=C'**ALL TOTAL**'                                    
         EDIT  (P8,REPTOT),(13,TOTNET),2,MINUS=YES                              
         EDIT  (P8,REPTOT+8),(13,TOTCOM),2,MINUS=YES                            
         BAS   RE,SPOOLIT                                                       
         EDIT  (B4,COUNTER),(6,P+11),ALIGN=LEFT                                 
         MVC   P+3(8),=C'RECORDS='                                              
         BAS   RE,SPOOLIT                                                       
DOTX     B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                 PRINT TAPE MAP                                
PRTMAP   NTR1                                                                   
         MVI   HEADRTN,2                                                        
         MVI   FORCEHED,C'Y'                                                    
         LA    R5,TABLE                                                         
         USING TLINED,R5                                                        
         LA    R2,P                                                             
         USING TPMAPD,R2                                                        
PM13     CLI   TAGY,0                                                           
         BE    PMX                                                              
         MVI   ERROR,0                                                          
         MVC   TPMKEY,0(R5)                                                     
         MVI   TPMKT,0             CLEAR MKT CODE ON TAPE                       
         OC    TPMCD(6),TPMCD      COMP/DIV/GFPRD                               
         BNZ   *+8                                                              
         OI    ERROR,X'01'                                                      
         OC    TPMNTSB,TPMNTSB     NAT/SUB/MAT                                  
         BNZ   *+8                                                              
         OI    ERROR,X'02'                                                      
         MVC   FULL(2),TMONTH      SORT HAS YM                                  
         EDIT  (B1,FULL),(2,TMONTH)                                             
         EDIT  (B1,FULL+1),(2,TMONTH+2)                                         
         CLI   TMONTH+2,X'40'                                                   
         BH    *+8                                                              
         MVI   TMONTH+2,C'0'                                                    
         MVC   TPMMON,TMONTH                                                    
         MVC   WORK+50(10),TNET      **  SEND EDITED NUMBER TO TAPE             
         MVC   BYTE,WORK+59          **  MAKE SURE IT UNPKS AS NUMBER           
         OI    BYTE,X'F0'                                                       
         CLI   BYTE,X'FC'                                                       
         BNE   PM13C                                                            
         OI    WORK+59,X'0F'                                                    
PM13C    UNPK  TNET(10),WORK+50(10)                                             
         MVC   TPMNET+1(10),TNET                                                
         MVC   WORK+50(10),TCOM      **  SEND EDITED NUMBER TO TAPE             
         MVC   BYTE,WORK+59          **  MAKE SURE IT UNPKS AS NUMBER           
         OI    BYTE,X'F0'                                                       
         CLI   BYTE,X'FC'                                                       
         BNE   PM13E                                                            
         OI    WORK+59,X'0F'                                                    
PM13E    UNPK  TCOM(10),WORK+50(10)                                             
         MVC   TPMCOM+1(10),TCOM                                                
         CLI   ERROR,X'03'                                                      
         BNE   PM14                                                             
         MVC   TPMERR(9),=C'*EST/PRD*'                                          
         B     PM15                                                             
PM14     TM    ERROR,X'01'                                                      
         BNO   PM14B                                                            
         MVC   TPMERR(9),=C' *PRD*  '                                           
         B     PM15                                                             
PM14B    TM    ERROR,X'02'                                                      
         BNO   PM15                                                             
         MVC   TPMERR(9),=C'  *EST*  '                                          
PM15     BAS   RE,SPOOLIT                                                       
         LA    R5,TLENE(R5)                                                     
         B     PM13                                                             
PMX      LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVI   HEADRTN,0                                                        
         B     EXIT                                                             
         DROP  R5,R2                                                            
         EJECT                                                                  
*                                                                               
* FIND COMMISION RATE/ RETURN IN COMPCT                                         
*                                                                               
COMMIRAT NTR1                                                                   
         LA    R5,COMPCTBL                                                      
         XC    KEY,KEY                                                          
         LA    R2,KEY                     ESTIMATE HEADER                       
         USING ESTHDR,R2                                                        
         MVC   EKEYAM,NBACTAM                                                   
         MVC   EKEYCLT,CURCLT                                                   
         MVC   EKEYPRD,CURPROD                                                  
         MVC   EKEYEST,CUREST                                                   
CR2      CLI   0(R5),0                                                          
         BE    CR5                                                              
         CLC   0(6,R5),KEY+1       IS IT IN TABLE                               
         BE    CRXX                                                             
         LA    R5,11(R5)                                                        
         B     CR2                                                              
CR5      MVC   0(6,R5),KEY+1       NO,ADD IT                                    
         ZAP   COMPCT,=P'150000'          DEFAULT=15.0000                       
         MVI   COMTYPE,0                                                        
         MVC   AIO,ANETWS2                                                      
         NETGO NVSETSPT,DMCB                                                    
         BAS   RE,DOHIGH                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DOREC                                                         
         L     R2,AIO                                                           
         MVC   BYTE,EBILLBAS                                                    
         MVC   FULL,EBILLCOM                                                    
         L     R1,FULL                                                          
         LTR   R1,R1                                                            
         BNZ   CR50                                                             
*                                                                               
         MVC   AIO,ANETWS2                                                      
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY              PROD RATE                                   
         USING PRDHDR,R2                                                        
         LA    R2,KEY                                                           
         MVC   PKEYAM,NBACTAM                                                   
         MVC   PKEYCLT,CURCLT                                                   
         MVC   PKEYPRD,CURPROD                                                  
         BAS   RE,DOHIGH                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DOREC                                                         
         L     R2,AIO                                                           
         MVC   BYTE,PBILLBAS                                                    
         MVC   FULL,PBILLCOM                                                    
         L     R1,FULL                                                          
         LTR   R1,R1                                                            
         BNZ   CR50                                                             
CR20     MVC   KEY,KEYSAVE                PRD=AAA                               
         MVC   PKEYPRD,=C'AAA'                                                  
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CRX                                                              
         BAS   RE,DOREC                                                         
         L     R2,AIO                                                           
         MVC   BYTE,PBILLBAS                                                    
         MVC   FULL,PBILLCOM                                                    
         L     R1,FULL                                                          
         LTR   R1,R1                                                            
         BZ    CR52                                                             
*                                                                               
CR50     LTR   R1,R1                                                            
         BP    CR51                                                             
         L     R2,=F'150000'                                                    
         AR    R1,R2                                                            
CR51     CVD   R1,DUB                                                           
         MVC   COMPCT,DUB+4                                                     
         MVC   COMTYPE,BYTE                                                     
CR52     MVC   6(4,R5),COMPCT                                                   
         MVC   10(1,R5),COMTYPE                                                 
*                                                                               
CRX      NETGO NVSETUNT                                                         
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI                                                  
         B     EXIT                                                             
*                                                                               
CRXX     MVC   COMPCT,6(R5)                                                     
         MVC   COMTYPE,10(R5)                                                   
         B     EXIT                                                             
*                                                                               
*                                                                               
CLRCOMTB NTR1                      CLEAR COMPCTBL                               
         LA    R2,COMPCTBL                                                      
CL2      CLI   0(R2),0                                                          
         BE    CLX                                                              
         XC    0(11,R2),0(R2)                                                   
         LA    R2,11(R2)                                                        
         B     CL2                                                              
CLX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
DOHIGH   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
DOREC    NTR1                                                                   
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         B     EXIT                                                             
*                                                                               
*                                                                               
ADDTABL  NTR1              ADD TAPE REC FROM SORTER TO TABLE                    
*                          PRINTS OUT LATER AS MAP OF TAPE                      
         LA    R2,TABLE                                                         
AD2      CLI   7(R2),0           CHECK TCPE(CLT/PRD ETC FIELD)                  
         BE    AD5                                                              
         LA    R2,TLENE(R2)                                                     
         B     AD2                                                              
AD5      MVC   0(TLENE,R2),0(R3)                                                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         L     R6,ATWA                                                          
         USING T320FFD,R6                                                       
         MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+15(20),SPLCLIN                                                
         MVC   H4(7),=C'PRODUCT'                                                
         MVC   H4+10(3),SPLPRO                                                  
         MVC   H4+15(20),SPLPRON                                                
         MVC   H5(8),=C'ESTIMATE'                                               
         MVC   H5+10(7),SPLEST                                                  
         MVC   H5+18(20),SPLESTN                                                
         CLI   HEADRTN,2                                                        
         BE    HD2                                                              
         CLI   HEADRTN,3                                                        
         BE    HD3                                                              
         SPACE                                                                  
         LA    R2,H10                                                           
         USING PLINED,R2                                                        
         MVC   PCOMP(7),=C'COMPANY'                                             
         MVC   PDIVI(8),=C'DIVISION'                                            
         MVC   PGFPRD(7),=C'PRODUCT'                                            
         MVC   PCPE(11),=C'CLT/PRD/EST'                                         
         MVC   PMONTH(5),=C'MONTH'                                              
         MVC   PINV(7),=C'INVOICE'                                              
         MVC   PTYPE(4),=C'TYPE'                                                
         MVC   PNAT(7),=C'NATURAL'                                              
         MVC   PSUB(7),=C'  SUB  '                                              
         MVC   PNET(10),=C'   NET    '                                          
         MVC   PASS,=C'ASS '                                                    
         MVC   PCOM(10),=C'COMMISSION'                                          
         MVC   PTOT+3(5),=C'TOTAL'                                              
         LA    R2,132(R2)                                                       
         MVC   PGFPRD(7),=C' CODE  '                                            
         MVC   PSUB(7),=C'NATURAL'                                              
         DROP  R6                                                               
*                                                                               
         DS    0H                                                               
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    HDX                 YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R4,BOXCOLS                                                       
         USING PLINED,R4                                                        
         MVI   0(R4),C'L'                                                       
         MVI   PDIVI-1,C'C'                                                     
         MVI   PGFPRD-1,C'C'                                                    
         MVI   PCPE-1,C'C'                                                      
         MVI   PMONTH-1,C'C'                                                    
         MVI   PINV-1,C'C'                                                      
         MVI   PTYPE-1,C'C'                                                     
         MVI   PNAT-1,C'C'                                                      
         MVI   PSUB-1,C'C'                                                      
         MVI   PASS-1,C'C'                                                      
         MVI   PNET-1,C'C'                                                      
         MVI   PCOM-1,C'C'                                                      
         MVI   PTOT-1,C'C'                                                      
         MVI   PEND,C'R'                                                        
         SPACE                                                                  
         LA    R4,BOXROWS                                                       
         LA    R4,8(R4)                                                         
         MVI   0(R4),C'T'                                                       
         LA    R4,3(R4)                                                         
         MVI   0(R4),C'M'                                                       
         LA    R4,49(R4)                                                        
         MVI   0(R4),C'B'                                                       
         SPACE                                                                  
HDX      B     EXIT                 (XIT1)                                      
         EJECT                                                                  
*                                                                               
HD2      DS    0H                                                               
         MVC   H7+10(23),=C'*** TAPE RECORD MAP ***'                            
         MVC   H7+49(3),=C'NET'                                                 
         MVC   H7+63(3),=C'COM'                                                 
         MVC   H7+74(7),=C'*ERROR*'                                             
*                                                                               
         MVI   BOXSET,C'Y'                                                      
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    HD2X                YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R4,BOXCOLS                                                       
         USING TPMAPD,R4                                                        
         MVI   TPMSTRT,C'L'                                                     
         MVI   TPMNET-1,C'C'                                                    
         MVI   TPMCOM-1,C'C'                                                    
         MVI   TPMERR-1,C'C'                                                    
         MVI   TPMEND,C'R'                                                      
         SPACE                                                                  
         LA    R4,BOXROWS                                                       
         LA    R4,5(R4)                                                         
         MVI   0(R4),C'T'                                                       
         LA    R4,2(R4)                                                         
         MVI   0(R4),C'M'                                                       
         LA    R4,54(R4)                                                        
         MVI   0(R4),C'B'                                                       
         SPACE                                                                  
HD2X     B     EXIT                 (XIT1)                                      
*                                                                               
HD3      DS    0H                                                               
         MVC   H7+1(8),=C'COMPANY '                                             
         MVC   H7+20(3),=C'NET'                                                 
         MVC   H7+30(10),=C'COMMISSION'                                         
*                                                                               
         MVI   BOXSET,C'Y'                                                      
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    HD3X                YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R4,BOXCOLS                                                       
         USING TOTSTR,R4                                                        
         MVI   TOTSTR,C'L'                                                      
         MVI   TOTNET-1,C'C'                                                    
         MVI   TOTCOM-1,C'C'                                                    
         MVI   TOTEND,C'R'                                                      
         SPACE                                                                  
         LA    R4,BOXROWS                                                       
         LA    R4,5(R4)                                                         
         MVI   0(R4),C'T'                                                       
         LA    R4,2(R4)                                                         
         MVI   0(R4),C'M'                                                       
         LA    R4,54(R4)                                                        
         MVI   0(R4),C'B'                                                       
         SPACE                                                                  
HD3X     B     EXIT                 (XIT1)                                      
         EJECT                                                                  
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C' GENERAL FOODS TAPE'                                     
         SSPEC H2,52,C' ------------------'                                     
         SSPEC H3,52,PERIOD                                                     
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
NTWKTP   DCB   DDNAME=NTWKTP,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00080,                                            X        
               BLKSIZE=00800,                                          X        
               MACRF=PM                                                         
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
         DC    C'**TABLE**'                                                     
TABLE    DS    CL20000                                                          
         DS    CL20000                                                          
         DS    CL20000                                                          
         DS    CL20000                                                          
         DS    CL20000                                                          
TABLEN   EQU   *-TABLE                                                          
       EJECT                                                                    
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS2                         
*                                                                               
*                                                                               
RELO     DS    F                *  FROM EDIT MODULE                             
ACLISTSV DS    F                *                                               
TAPEOPT  DS    CL1              *                                               
BILLMON  DS    CL2              *  REQUESTED BILLING YEAR/MONTH                 
BDAT     DS    CL4              *  REQUESTED BILLING YYMM                       
*                                                                               
NUMPER   DS    F                                                                
AMONLIST DS    F                                                                
ANTWKTP  DS    F                                                                
ATABLE   DS    F                                                                
ADUPLIC  DS    F                                                                
COUNTER  DS    F                                                                
CDTOT    DS    F                   NUMBER OF COMP TOTALS                        
DOLLARS  DS    CL1                                                              
AGYCODE  DS    CL1                                                              
PREVCOMP DS    CL1                                                              
*                                                                               
CUREST   DS    CL1                                                              
CURPRD   DS    CL1                                                              
CURCLT   DS    CL2                                                              
CURPROD  DS    CL3                                                              
COMPREV  DS    CL1                                                              
DIVPREV  DS    CL1                                                              
PRDPREV  DS    CL4                                                              
CPEPREV  DS    CL9                                                              
COMTYPE  DS    CL1                 COMMISSION BASE=B'NNNN0001'=NET              
COMPCT   DS    PL4                 COMMISSION PCT                               
NEACCWRK DS    CL1       FOR NETACC RETURN                                      
PL8WRK   DS    CL8       FOR NETACC RETURN                                      
PL16WRK  DS    CL16                WORK AREA                                    
TAPEDAT  DS    CL6                 TAPE CREATION DATE YYMMDD                    
HEADRTN  DS    CL1                                                              
TCOMSV   DS    PL10                                                             
TNETSV   DS    PL10                                                             
MYPAK    DS    PL10                                                             
BINDMCB  DS    6F                                                               
*                                                                               
TOTALS   DS    CL2       COMPANY ID                                             
TOLNET   DS    PL8                 NET                                          
TOLCOM   DS    PL8                 COM                                          
TOLNETX  DS    PL8                 NETX                                         
TOLCOMX  DS    PL8                 COMX                                         
TOLNETP  DS    PL8                 NETP                                         
TOLCOMP  DS    PL8                 COMP                                         
TOTLENE  EQU   *-TOTALS                                                         
         DS    CL500      10 MORE TOTALS                                        
*                                                                               
REPTOT   DS    PL8        ALL REPORT TOTALS                                     
         DS    PL8                                                              
REPPNET  DS    PL8        NET-P                                                 
REPPCOM  DS    PL8        COM-P                                                 
REPXNET  DS    PL8        NET-X                                                 
REPXCOM  DS    PL8        COM-X                                                 
*                                                                               
*                                                                               
MONLIST  DS    CL96                4 BYTE COMPRESSED START-END                  
*                                                                               
MONYEAR  DS   0CL8                 2 BYTE Y/M + 6 BYTE MMM/YY                   
YRMOSTRT DS    CL8                                                              
         ORG   YRMOSTRT                                                         
         DS    CL192               8 X 24(MAX MON)                              
         ORG   *-8                                                              
YRMOEND  DS    CL8                                                              
*                                                                               
PERTYPE  DS    CL1                                                              
BOXSET   DS    CL1                                                              
MYBYTE   DS    CL1                                                              
*                                                                               
RECWORK  DS    CL132               WORK AREA FOR TAPE RECORDS                   
SORTREC  DS    CL200               SO I CAN SEE SORTREC IN DUMPS                
*                                                                               
COMPCTBL DS    CL2000              200*10CL                                     
*                                  CLT2/PROD3/EST1/COMPCT4                      
         EJECT                                                                  
*                                                                               
PLINED   DSECT                    DSECT FOR PRINTED REPORT                      
PSTR     DS    CL1                                                              
PCOMP    DS    CL7                                                              
         DS    CL1                                                              
PDIVI    DS    CL8                                                              
         DS    CL1                                                              
PGFPRD   DS    CL7                 GF PRODUCT                                   
         DS    CL1                                                              
PCPE     DS    CL13                CLIENT/PROD/EST (UNITS)                      
         DS    CL1                                                              
PMONTH   DS    CL6                 (MMM/YY)                                     
         DS    CL1                                                              
PINV     DS    CL9                 INVOICE (N/NN/NNNN)                          
         DS    CL1                                                              
PTYPE    DS    CL4                                                              
         DS    CL1                                                              
PNAT     DS    CL7                 NATURAL                                      
         DS    CL1                                                              
PSUB     DS    CL7                 SUBNATURAL                                   
         DS    CL1                                                              
PASS     DS    CL4                                                              
         DS    CL1                                                              
PNET     DS    CL13                 NET INVOICE AMT                             
         DS    CL1                                                              
PCOM     DS    CL13                 AGENCY COMMISION                            
         DS    CL1                                                              
PTOT     DS    CL13                                                             
PEND     DS    CL1                                                              
PLENE    EQU   *-PLINED                                                         
*                                                                               
TOTLD    DSECT                    DSECT FOR TOTALS                              
TOTSTR   DS    CL1                                                              
TOTCOMP  DS    CL1                                                              
         DS    CL4                                                              
TOTDIV   DS    CL1                                                              
         DS    CL3                                                              
TOTCOUNT DS    CL3                                                              
         DS    CL1                                                              
TOTNET   DS    CL13                 NET INVOICE AMT                             
         DS    CL1                                                              
TOTCOM   DS    CL13                 AGENCY COMMISION                            
TOTEND   DS    CL1                                                              
*                                                                               
*                                                                               
*                                                                               
*                                                                               
TLINED   DSECT                    DSECT FOR TAPE AND SORT                       
TCOMP    DS    CL1                                                              
TDIV     DS    CL1                                                              
TGFPRD   DS    CL4                 GF PRODUCT                                   
TAGY     DS    CL1                                                              
TCPE     DS    CL12              3-CLT,3-PRD,3-EST,3-=BLANKS                    
TMONTH   DS    CL4                 (9006)                                       
TINV     DS    CL9                 INVOICE (N/NN/NNNN)                          
TTYPE    DS    CL1                                                              
TNAT     DS    CL3                 NATURAL                                      
TSUB     DS    CL3                 SUBNATURAL                                   
TMARK    DS    CL4                 MARKET CODE NOT USED                         
TNET     DS    CL10                 NET INVOICE AMT                             
TCOM     DS    CL10                 AGENCY COMMISION                            
TDATE    DS    CL4                 TAPE CREATION DATE                           
TLENE    EQU   *-TLINED                                                         
         EJECT                                                                  
*                                                                               
UNITSD   DSECT             DESECT FOR STORING UNIT DOLLS                        
UXTRA    DS    CL1                                                              
UCLT     DS    CL2                                                              
UPRD     DS    CL3                                                              
UEST     DS    CL1                                                              
UDIV     DS    CL2                                                              
UGFPRD   DS    CL4                                                              
UNAT     DS    CL3                                                              
USUB     DS    CL3                                                              
UASS     DS    CL1                                                              
UACCMON  DS    PL8                 ACTUAL NET DOLLARS                           
         DS    PL8                 COMMISSION                                   
UASSMON  DS    PL8                 ASSIGNED NET DOLLARS                         
         DS    PL8                 COMMISSION                                   
UINV     DS    CL6                 BILL INVOICE NUMBER                          
UMONLENE EQU   *-UACCMON                                                        
         DS    CL874              ((ONE MONTH)=CL38*23MONTHS)                   
ULENE    EQU   *-UCLT                                                           
*                                                                               
*                                                                               
TPMAPD   DSECT        DSECT FOR TAPE MAP                                        
TPMSTRT  DS    CL1                 BOX LINE                                     
TPMKEY   DS    0CL43                                                            
TPMCD    DS    CL2                 COMP/DIV                                     
TPMGFP   DS    CL4                 GF PRD                                       
         DS    CL13                                                             
TPMMON   DS    CL4                                                              
         DS    CL10                                                             
TPMNTSB  DS    CL6                 NAT/SUB-NAT                                  
TPMKT    DS    CL4                 (NOT USED MARKET CODE)                       
         DS    CL1                 BOX LINE                                     
TPMNET   DS    CL13                                                             
         DS    CL1                 BOX LINE                                     
TPMCOM   DS    CL13                                                             
         DS    CL1                 BOX LINE                                     
TPMERR   DS    CL10                                                             
TPMEND   DS    CL1                 BOX LINE                                     
         EJECT                                                                  
         SPACE                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRID2D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NEGENUNIT                                                      
         PRINT ON                                                               
BILHD    DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPGENPGEST                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039NEWRI45   08/23/02'                                      
         END                                                                    
